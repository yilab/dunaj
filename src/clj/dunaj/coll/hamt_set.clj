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

(ns dunaj.coll.hamt-set
  "Hash Array Mapped Trie set, a persistent collection.

  Based on Phil Bagwell's http://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf[Hash Array Mapped Tries], this collection type
  stores items in a trie (pronounced /ˈtriː/), which offers
  both persistency and efficient item lookup.

  HAMT sets have support for transients and folding.

  IMPORTANT: Except for very special cases,
  it is idiomatic to use set literal or functions defined in
  `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn I Va Maybe]]
   [dunaj.boolean :refer [Boolean boolean and or not]]
   [dunaj.host :refer [AnyArray ArrayManager set!]]
   [dunaj.host.int :refer [Int iint iinc i< iadd i2 i0]]
   [dunaj.math :refer
    [nneg? < integer? == <= quot dec >= zero? mod > subtract add neg?
     one? multiply inc dec npos? /]]
   [dunaj.compare :refer
    [IHash IEquiv nil? hash IComparable identical?]]
   [dunaj.flow :refer
    [when-let cond loop recur if let do when delay when-not if-let
     doto if-some]]
   [dunaj.threading :refer [->]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta]]
   [dunaj.poly :refer [deftype defrecord extend-protocol!]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed ISeq IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable ILookup IIndexed ISectionable
     IReversible IUnpackedRed IPersistentCollection IStacked
     IPersistentVector IAssociative IPersistentSet IEditable
     IMutableStacked IMutableSet IMutableAssociative ISettleable
     IMutableCollection IConvolutionFactory IInvertible
     reduce empty? count section counted? seq empty single?
     peek conj assoc conj! settle! edit assoc! -reduce-unpacked
     reduced? postponed? postponed advance]]
   [dunaj.function :refer [IInvocable fn defn apply]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable fork join invoke -fold]]
   [dunaj.coll.helper :refer
    [fold-sectionable fold-every reduce* fold* reduce-unpacked*]]
   [dunaj.host.array :refer
    [array-manager array to-array aget acount adapt]]
   [dunaj.state.var :refer [def]]
   [dunaj.set :refer [U set-complement]]
   [dunaj.coll.tuple :refer [pair key]]
   [dunaj.coll.hamt-map]))


;;;; Implementation details

(defn ^:private wrap-fn
  [f]
  (fn [val a b] (f val a)))

(defn ^:private unwrap-fn
  [f]
  (fn [val p :- java.util.Map$Entry] (f val (key p))))

(def ^:private impl-field :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.APersistentSet "impl")
    (.setAccessible true)))

(defn get-impl :- clojure.lang.IPersistentMap
  [x :- clojure.lang.APersistentSet]
  (.get impl-field x))

(def ^:private mutable-impl-field :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.ATransientSet "impl")
    (.setAccessible true)))

(defn mutable-get-impl :- clojure.lang.ITransientMap
  [x :- clojure.lang.ATransientSet]
  (.get mutable-impl-field x))


;;;; Public API

(deftype HamtSet
  "A type for Hash Array Mapped Trie sets."
  clojure.lang.PersistentHashSet
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.Set
  ;; j.l.Runnable, j.u.c.Callable
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (reduce-unpacked* (get-impl this) (wrap-fn reducef) init))
  ISeqable
  ICounted
  IEmptyable
  ILookup
  (-contains? [this key] (.contains this key))
  (-get [this key not-found] (if (.contains this key) key not-found))
  IInvertible
  (-invert [this] (if (empty? this) U (set-complement this)))
  IEditable
  IPersistentCollection
  IInvocable
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold* (get-impl this) reduce-fn
           pool n combinef (unwrap-fn reducef)))
  ;; Abstract types
  IPersistentSet)

(deftype TransientHamtSet
  "A type for transient hamt maps."
  clojure.lang.PersistentHashSet$TransientHashSet
  IRed
  (-reduce [this reducef init]
    (reduce-unpacked* (mutable-get-impl this) (wrap-fn reducef) init))
  ILookup
  (-contains? [this key] (.contains this key))
  (-get [this key not-found] (if (.contains this key) key not-found))
  ICounted
  ISettleable
  IMutableCollection
  IMutableSet
  IInvocable)

(def empty-hamt-set :- HamtSet
  "An empty HAMT set."
  {:added v1
   :see '[dunaj.coll.default/empty-set dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  clojure.lang.PersistentHashSet/EMPTY)

;;; Factory

(defrecord HamtSetFactory
  "A factory record for hamt set."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (settle! (reduce conj! (edit empty-hamt-set) coll)))
  (-from-items [factory] empty-hamt-set)
  (-from-items [factory a] (conj empty-hamt-set a))
  (-from-items [factory a b] (conj empty-hamt-set a b))
  (-from-items [factory a b c] (conj empty-hamt-set a b c))
  (-from-items [factory a b c d] (conj empty-hamt-set a b c d))
  (-from-items [factory a b c d more]
    (let [t (edit empty-hamt-set)
          t (-> t (conj! a) (conj! b) (conj! c) (conj! d))]
      (settle! (reduce conj! t more)))))

(def hamt-set-factory :- ICollectionFactory
  "A HAMT set factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of HAMT set can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[dunaj.coll.default/set-factory
          dunaj.coll.default/->set
          dunaj.coll.default/set
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->HamtSetFactory))
