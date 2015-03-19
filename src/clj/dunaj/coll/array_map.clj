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

(ns dunaj.coll.array-map
  "Array map, a persistent collection for small maps.

  Array map is an implementation of
  `<<dunaj.coll.spi.ad#IPersistentMap,IPersistentMap>>` where items
  are stored internally in a host array. Whole array is copied
  on each update operation (`<<dunaj.coll.api.ad#conj,conj>>`,
  `<<dunaj.coll.api.ad#assoc,assoc>>`, etc). This absence of
  structural sharing is efficient only for small maps. Use
  `<<dunaj.coll.hamt-map.api.ad#,hamt-map>>` for
  larger maps. Array maps have support for transients, folding and
  efficient unpacked reduce.

  NOTE: Current implementation switches to the `hamt-map` when number
  of items (`[key value]` pairs) exceeds *8*.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  map literal or functions defined in
  `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn U I Va Maybe AnyFn]]
   [dunaj.boolean :refer [Boolean boolean and or not]]
   [dunaj.host :refer [AnyArray ArrayManager set!]]
   [dunaj.host.int :refer
    [Int iint iinc i< iadd i2 i0 imul isub idiv]]
   [dunaj.math :refer [nneg? < integer? == <= quot dec >= zero? mod >
                       subtract add neg? one? multiply inc dec /]]
   [dunaj.compare :refer
    [IHash IEquiv nil? hash IComparable sentinel identical?]]
   [dunaj.flow :refer [when-let cond loop recur if let do when delay
                       when-not if-let doto if-some if-not]]
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
   [dunaj.coll.helper :refer [fold-sectionable fold-every reduce*
                              fold* reduce-unpacked* advance-fn]]
   [dunaj.host.array :refer
    [array-manager array to-array aget acount adapt]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.tuple :refer [pair]]
   [dunaj.coll.hamt-map]))


;;;; Implementation details

(def ^:private array-field :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.PersistentArrayMap "array")
    (.setAccessible true)))

(defn get-array :- AnyArray
  [x :- clojure.lang.PersistentArrayMap]
  (.get array-field x))

(defn ^:private reduce-unpacked-array-map :- Any
  "Reduce section of Array map."
  [arr :- AnyArray, reducef :- AnyFn, init :- Any,
   begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (let [ib (imul begin (i2))
          ie (imul end (i2))
          af (advance-fn [ret i]
               (i< i ie)
               (recur (reducef ret (aget arr i) (aget arr (iinc i)))
                      (iadd i (i2)))
               :else ret)]
      (af init ib))))

(declare array-map-section-section)

(defn ^:private unpacked-fn
  [f]
  (fn [val a b] (f val (pair a b))))

(deftype ^:private ArrayMapSection
  [arr :- AnyArray, begin :- Int, end :- Int]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (reduce-unpacked-array-map arr reducef init begin end))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef
                      array-map-section-section))
  ICounted
  (-count [this] (isub end begin)))

(defn ^:private array-map-section :- ArrayMapSection
  [m :- clojure.lang.PersistentArrayMap, begin :- Int, end :- Int]
  (let [arr (get-array m)
        end (or end (idiv (acount arr) (i2)))]
    (->ArrayMapSection arr begin end)))

(defn ^:private array-map-section-section :- ArrayMapSection
  [ms :- ArrayMapSection, begin :- Int, end :- Int]
  (let [offset (.-begin ms)
        end (or end (count ms))]
    (->ArrayMapSection
     (.-arr ms) (iadd begin offset) (iadd end offset))))


;;;; Public API

;; NOTE: Array map is capable of ICatenable and IInvertible, but
;;       as this type often transforms into hashmap, we do not
;;       implement it.

(deftype ArrayMap
  "A type for Array map persistent collections.
  Use `<<array_map_factory,array-map-factory>>` to construct
  instances of this type."
  clojure.lang.PersistentArrayMap
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
    (let [arr (get-array this)]
      (reduce-unpacked-array-map arr reducef init (i0)
                                 (idiv (acount arr) (i2)))))
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
    (fold-sectionable
     this reduce-fn pool n combinef reducef array-map-section))
  ;; Abstract types
  IPersistentMap)

(deftype TransientArrayMap
  "A type for Transient Array Maps.

  Instances of this type are created by calling
  `<<dunaj.coll.api.ad#edit,edit>>` on existing persistent
  array maps."
  clojure.lang.PersistentArrayMap$TransientArrayMap
  ICounted
  ILookup
  (-contains? [this key]
    (let [s (sentinel)]
      (if (identical? s (.valAt this key s)) false true)))
  (-get [this key not-found] (.valAt this key not-found))
  ISettleable
  IMutableCollection
  IMutableAssociative
  IMutableMap
  IInvocable)

(def empty-array-map :- ArrayMap
  "An empty array map."
  {:added v1
   :see '[array-map-factory dunaj.coll.hamt-map/empty-hamt-map
          dunaj.coll.default/empty-map dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/assoc dunaj.coll/edit]}
  clojure.lang.PersistentArrayMap/EMPTY)

;;; Factory

(defrecord ArrayMapFactory
  "A factory for array maps. Use
  `<<array_map_factory,array-map-factory>>` in preference to
  ad-hoc instances of this type."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (settle! (reduce conj! (edit empty-array-map) coll)))
  (-from-items [factory] empty-array-map)
  (-from-items [factory a] (conj empty-array-map a))
  (-from-items [factory a b] (conj empty-array-map a b))
  (-from-items [factory a b c] (conj empty-array-map a b c))
  (-from-items [factory a b c d] (conj empty-array-map a b c d))
  (-from-items [factory a b c d more]
    (let [t (edit empty-array-map)
          t (-> t (conj! a) (conj! b) (conj! c) (conj! d))]
      (settle! (reduce conj! t more))))
  IConvolutionFactory
  (-convolute [factory c1 c2]
    (loop [ret (edit empty-array-map)
           a1 (reduce #(postponed %2) nil c1)
           a2 (reduce #(postponed %2) nil c2)]
      (if (and (postponed? a1) (postponed? a2))
        (recur (assoc! ret @a1 @a2)
               (unsafe-advance! a1)
               (unsafe-advance! a2))
        (settle! ret))))
  (-from-interleaved [factory] empty-array-map)
  (-from-interleaved [factory a b]
    (assoc empty-array-map a b))
  (-from-interleaved [factory a b c d]
    (assoc empty-array-map a b c d))
  (-from-interleaved [factory a b c d e f]
    (assoc empty-array-map a b c d e f))
  (-from-interleaved [factory a b c d e f g h]
    (assoc empty-array-map a b c d e f g h))
  (-from-interleaved [factory a b c d e f g h more]
    (apply assoc empty-array-map a b c d e f g h more)))

(def array-map-factory :- (U ICollectionFactory IConvolutionFactory)
  "An Array Map factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#IConvolutionFactory,IConvolutionFactory>>` and
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocols.

  New instances of Array Map can be created with
  `<<dunaj.coll.api.ad#collection,collection>>`,
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`,
  `<<dunaj.coll.api.ad#convolution,convolution>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}convolution,-{gt}convolution>>`."
  {:added v1
   :see '[dunaj.coll.hamt-map/hamt-map-factory
          dunaj.coll.default/map-factory
          dunaj.coll.default/->map
          dunaj.coll.default/zipmap
          dunaj.coll/collection
          dunaj.coll/->collection
          dunaj.coll/convolution
          dunaj.coll/->convolution]}
  (->ArrayMapFactory))
