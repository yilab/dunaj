;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; Additional copyright for parts of documentation and/or
;; underlying implementation:
;; Copyright (C) 2008, 2015, Rich Hickey and Clojure contributors.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns dunaj.coll.hamt-map
  "Hash Array Mapped Trie map, a persistent collection.

  Based on Phil Bagwell's
  http://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf[Hash Array Mapped Tries],
  this collection type stores items in a trie (pronounced /ˈtriː/),
  which offers both persistency and efficient item lookup. For small
  maps however, <<dunaj.coll.array-map.api.ad#,array maps>> can be
  more efficient.

  HAMT maps have support for transients, folding and efficient
  unpacked reduce.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  map literal or functions defined in
  `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn U I Va Maybe]]
   [dunaj.boolean :refer [Boolean boolean and or not]]
   [dunaj.host :refer [AnyArray ArrayManager set!]]
   [dunaj.host.int :refer [Int iint iinc i< iadd i2 i0]]
   [dunaj.math :refer
    [nneg? < integer? == <= quot dec >= zero? mod > subtract add neg?
     one? multiply inc dec npos? /]]
   [dunaj.compare :refer
    [IHash IEquiv nil? hash IComparable sentinel identical?]]
   [dunaj.flow :refer [when-let cond loop recur if let do when delay
                       when-not if-let doto if-some]]
   [dunaj.threading :refer [->]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta]]
   [dunaj.poly :refer [deftype defrecord extend-protocol!]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed ISeq IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable ILookup IIndexed ISectionable
     IReversible IUnpackedRed IPersistentCollection IStacked
     IPersistentVector IAssociative IPersistentMap IEditable
     IMutableStacked IMutableMap IMutableAssociative ISettleable
     IMutableCollection IConvolutionFactory
     reduce empty? count section counted? seq empty single?
     peek conj assoc conj! settle! edit assoc! -reduce-unpacked
     reduced? postponed? postponed unsafe-advance!]]
   [dunaj.function :refer [IInvocable fn defn apply]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable fork join invoke -fold]]
   [dunaj.coll.helper :refer
    [fold-sectionable fold-every reduce* fold* reduce-unpacked*
     advance-fn]]
   [dunaj.host.array :refer
    [array-manager array to-array aget acount adapt]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.tuple :refer [pair]]))


;;;; Implementation details

(defn ^:private unpacked-fn
  [f]
  (fn [val a b] (f val (pair a b))))

;;; PersistentHashMap

(def ^:private root-field :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.PersistentHashMap "root")
    (.setAccessible true)))

(defn get-root :- Any
  [x :- clojure.lang.PersistentHashMap]
  (.get root-field x))

(def ^:private has-null-field :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.PersistentHashMap "hasNull")
    (.setAccessible true)))

(defn has-null? :- Boolean
  [x :- clojure.lang.PersistentHashMap]
  (boolean (.get has-null-field x)))

(def ^:private null-value-field :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.PersistentHashMap "nullValue")
    (.setAccessible true)))

(defn get-null-value :- Any
  [x :- clojure.lang.PersistentHashMap]
  (.get null-value-field x))

;;; TransientHashMap

(def ^:private mutable-root-field :- java.lang.reflect.Field
  (doto (.getDeclaredField
         clojure.lang.PersistentHashMap$TransientHashMap "root")
    (.setAccessible true)))

(defn mutable-get-root :- Any
  [x :- clojure.lang.PersistentHashMap$TransientHashMap]
  (.get mutable-root-field x))

(def ^:private mutable-has-null-field :- java.lang.reflect.Field
  (doto (.getDeclaredField
         clojure.lang.PersistentHashMap$TransientHashMap
         "hasNull")
    (.setAccessible true)))

(defn mutable-has-null? :- Boolean
  [x :- clojure.lang.PersistentHashMap$TransientHashMap]
  (boolean (.get mutable-has-null-field x)))

(def ^:private mutable-null-value-field :- java.lang.reflect.Field
  (doto (.getDeclaredField
         clojure.lang.PersistentHashMap$TransientHashMap
         "nullValue")
    (.setAccessible true)))

(defn mutable-get-null-value :- Any
  [x :- clojure.lang.PersistentHashMap$TransientHashMap]
  (.get mutable-null-value-field x))

;;; ArrayNode

(def ^:private an-array-field :- java.lang.reflect.Field
  (doto (.getDeclaredField
         clojure.lang.PersistentHashMap$ArrayNode "array")
    (.setAccessible true)))

(defn get-an-array :- AnyArray
  [x :- clojure.lang.PersistentHashMap$ArrayNode]
  (.get an-array-field x))

(def ^:private oam :- ArrayManager
  (array-manager java.lang.Object))

(deftype ArrayNode
  clojure.lang.PersistentHashMap$ArrayNode
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (reduce* (adapt oam (get-an-array this))
             #(reduce-unpacked* %2 reducef %)
             init))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (let [tasks (adapt oam (get-an-array this))]
      (fold-every tasks reduce-fn pool n combinef reducef))))

;;; BitmapIndexedNode

(def ^:private bi-array-field :- java.lang.reflect.Field
  (doto (.getDeclaredField
         clojure.lang.PersistentHashMap$BitmapIndexedNode
         "array")
    (.setAccessible true)))

(defn get-bi-array :- AnyArray
  [x :- clojure.lang.PersistentHashMap$BitmapIndexedNode]
  (.get bi-array-field x))

(deftype BitmapIndexedNode
  clojure.lang.PersistentHashMap$BitmapIndexedNode
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [arr :- AnyArray (get-bi-array this)
          al (acount arr)
          af (advance-fn [ret i :- Int]
               (i< i al)
               (if-some [k (aget arr i)]
                 (recur (reducef ret k (aget arr (iinc i)))
                        (iadd (i2) i))
                 (recur (reduce-unpacked*
                         (aget arr (iinc i)) reducef ret)
                        (iadd (i2) i)))
               :else ret)]
      (af init (i0))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (reduce-fn this reducef (combinef))))

;;; HashCollisionNode

(def ^:private ^java.lang.reflect.Field hc-array-field
  (doto (.getDeclaredField
         clojure.lang.PersistentHashMap$HashCollisionNode "array")
    (.setAccessible true)))

(defn get-hc-array [x]
  (.get hc-array-field x))

(deftype HashCollisionNode
  clojure.lang.PersistentHashMap$HashCollisionNode
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [arr :- AnyArray (get-hc-array this)
          al (acount arr)
          af (advance-fn [ret i :- Int]
               (i< i al)
               (if-some [k (aget arr i)]
                 (recur (reducef ret k (aget arr (iinc i)))
                        (iadd (i2) i))
                 (recur (reduce-unpacked*
                         (aget arr (iinc i)) reducef ret)
                        (iadd (i2) i)))
               :else ret)]
      (af init (i0))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (reduce-fn this reducef (combinef))))

(extend-protocol! IRed
  clojure.lang.IMapEntry
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (reducef ret (.val this)))]
      (af (reducef init (.key this))))))


;;;; Public API

(deftype HamtMap
  "A type for Hash Array Mapped Trie maps."
  clojure.lang.PersistentHashMap
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Map
  ;; j.l.Runnable, j.u.c.Callable
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [r (get-root this)
          af (advance-fn [ret] (reduce-unpacked* r reducef ret))
          ret (if (has-null? this)
                (reducef init nil (get-null-value this))
                init)]
      (af ret)))
  ISeqable
  ICounted
  IEmptyable
  ILookup
  (-contains? [this key] (.containsKey this key))
  (-get [this key not-found] (.valAt this key not-found))
  IEditable
  IPersistentCollection
  IAssociative
  IInvocable
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (let [dofn
          #(let [ret (if-let [r (get-root this)]
                       (-fold r reduce-fn pool n combinef reducef)
                       (combinef))]
             (if (has-null? this)
               (combinef
                ret
                (reduce-fn
                 (assoc (empty this) nil (get-null-value this))
                 reducef (combinef)))
               ret))]
      (invoke pool dofn)))
  ;; Abstract types
  IPersistentMap)

(deftype MutableHamtMap
  "A type for mutable HAMT maps."
  clojure.lang.PersistentHashMap$TransientHashMap
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [r (mutable-get-root this)
          af (advance-fn [ret] (reduce-unpacked* r reducef ret))
          ret (if (mutable-has-null? this)
                (reducef init nil (mutable-get-null-value this))
                init)]
      (af ret)))
  ICounted
  ILookup
  (-contains? [coll key]
    (let [s (sentinel)]
      (if (identical? s (.valAt coll key s)) false true)))
  (-get [coll key not-found] (.valAt coll key not-found))
  ;; following protocols are already implemented
  ISettleable
  IMutableCollection
  IMutableAssociative
  IMutableMap
  IInvocable)

(def empty-hamt-map :- HamtMap
  "An empty HAMT map."
  {:added v1
   :see '[hamt-map-factory dunaj.coll.array-map/empty-array-map
          dunaj.coll.default/empty-map dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/assoc dunaj.coll/edit]}
  clojure.lang.PersistentHashMap/EMPTY)

;;; Factory

(defrecord HamtMapFactory
  "A factory record for hamt map."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (settle! (reduce conj! (edit empty-hamt-map) coll)))
  (-from-items [factory] empty-hamt-map)
  (-from-items [factory a] (conj empty-hamt-map a))
  (-from-items [factory a b] (conj empty-hamt-map a b))
  (-from-items [factory a b c] (conj empty-hamt-map a b c))
  (-from-items [factory a b c d] (conj empty-hamt-map a b c d))
  (-from-items [factory a b c d more]
    (let [t (edit empty-hamt-map)
          t (-> t (conj! a) (conj! b) (conj! c) (conj! d))]
      (settle! (reduce conj! t more))))
  IConvolutionFactory
  (-convolute [factory c1 c2]
    (loop [ret (edit empty-hamt-map)
           a1 (reduce #(postponed %2) nil c1)
           a2 (reduce #(postponed %2) nil c2)]
      (if (and (postponed? a1) (postponed? a2))
        (recur (assoc! ret @a1 @a2)
               (unsafe-advance! a1) (unsafe-advance! a2))
        (settle! ret))))
  (-from-interleaved [factory] empty-hamt-map)
  (-from-interleaved [factory a b]
    (assoc empty-hamt-map a b))
  (-from-interleaved [factory a b c d]
    (assoc empty-hamt-map a b c d))
  (-from-interleaved [factory a b c d e f]
    (assoc empty-hamt-map a b c d e f))
  (-from-interleaved [factory a b c d e f g h]
    (assoc empty-hamt-map a b c d e f g h))
  (-from-interleaved [factory a b c d e f g h more]
    (apply assoc empty-hamt-map a b c d e f g h more)))

(def hamt-map-factory :- (U ICollectionFactory IConvolutionFactory)
  "A HAMT map factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#IConvolutionFactory,IConvolutionFactory>>`
  and
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocols.

  New instances of HAMT map can be created with
  `<<dunaj.coll.api.ad#collection,collection>>`,
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`,
  `<<dunaj.coll.api.ad#convolution,convolution>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}convolution,-{gt}convolution>>`."
  {:added v1
   :see '[dunaj.coll.array-map/array-map-factory
          dunaj.coll.default/map-factory dunaj.coll.default/->map
          dunaj.coll.default/zipmap dunaj.coll/collection
          dunaj.coll/->collection dunaj.coll/convolution
          dunaj.coll/->convolution]}
  (->HamtMapFactory))
