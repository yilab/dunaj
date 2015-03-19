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

(ns dunaj.coll.rbt-sorted-set
  "Red black tree based set, a sorted persistent collection.

  RBT sorted sets are persistent sets represented by a
  self-balancing binary search tree. Besides being sorted,
  they are counted, invertible and have support for efficient
  sectioning, flipping and reversions.

  The comparator used for sorting the set is stored in the
  set's configuration and is accessible with
  `<<dunaj.feature.api.ad#config,config>>` function.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  functions defined in `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [take-while]]
   [clojure.bootstrap :refer [v1 not-implemented]]
   [clojure.bridge]
   [dunaj.type :refer [Any Fn I Va Maybe AnyFn]]
   [dunaj.boolean :refer [Boolean boolean and or not]]
   [dunaj.host :refer
    [AnyArray ArrayManager set! class-instance? proxy]]
   [dunaj.host.int :refer
    [Int iint iinc i< iadd i2 i0 inpos? ineg? izero? ixor]]
   [dunaj.math :refer
    [nneg? < integer? == <= quot dec >= zero? mod > subtract add neg?
     one? multiply inc dec npos? /]]
   [dunaj.compare :refer
    [IHash IEquiv nil? hash IComparable identical? =
     next-basis unordered-hash-factory basis-seed hash-from-basis]]
   [dunaj.flow :refer
    [when-let cond loop recur if let do when delay when-not if-let
     doto if-some]]
   [dunaj.threading :refer [->]]
   [dunaj.feature :refer
    [IMeta IPersistentMeta IConfig meta assoc-meta -config]]
   [dunaj.poly :refer [deftype defrecord extend-protocol!]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed ISeq IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable ILookup IIndexed ISortedSectionable
     IReversible IUnpackedRed IPersistentCollection IStacked
     IPersistentVector IAssociative IPersistentMap IEditable
     IMutableStacked IMutableMap IMutableAssociative ISettleable
     IMutableCollection IConvolutionFactory IFlippable ISorted
     IPersistentSet IInvertible
     reduce empty? count section counted? seq empty single? get -get
     peek conj assoc conj! settle! edit assoc! -reduce-unpacked
     reduced? postponed? postponed advance first -empty second
     -conj -key -dissoc contains? reduced -get -flip -disj
     -reduce -contains?]]
   [dunaj.function :refer
    [Function IInvocable fn defn apply constantly]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable fork join invoke -fold]]
   [dunaj.coll.helper :refer
    [fold-sectionable fold-every reduce* fold* reduce-unpacked*
     coll->iterator]]
   [dunaj.host.array :refer
    [array-manager array to-array aget acount adapt]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.tuple :refer [pair]]
   [dunaj.coll.hamt-map]
   [dunaj.coll.hamt-set]
   [dunaj.coll.rbt-sorted-map :refer
    [->FlippedRbtSortedMap ->RbtSortedMapSection
     rbt-sorted-map-section reversed-reduce-rbt reduce-rbt-section]]
   [dunaj.set :refer [finite? set-complement U]]))


;;;; Implementation details

(defn ^:private wrap-fn
  [f]
  (fn [val a b] (f val a)))

(def ^:private ^java.lang.reflect.Field impl-field
  (doto (.getDeclaredField clojure.lang.APersistentSet "impl")
    (.setAccessible true)))

(defn get-impl [x]
  (.get impl-field x))

(declare flipped-rbt-sorted-set rbt-sorted-set-section)

(defn ^:private range-check :- Boolean
  [comparator :- java.util.Comparator key ascending? begin end]
  (if ascending?
    (and (or (nil? begin) (npos? (.compare comparator begin key)))
         (or (nil? end) (neg? (.compare comparator key end))))
    (and (or (nil? begin) (npos? (.compare comparator key begin)))
         (or (nil? end) (neg? (.compare comparator end key))))))

(declare ->FlippedRbtSortedSet)

(deftype RbtSortedSetSection
  "Red black tree sorted set section type. Not persistent!"
  [coll :- clojure.lang.PersistentTreeSet, ascending? :- Boolean,
   begin :- Any, end :- Any, ^:volatile-mutable hash :- Int,
   ^:volatile-mutable hash-code :- Int,
   ^:volatile-mutable count :- Int]
  IHash
  (-hash [this]
    (when (izero? hash)
      (let [hb (reduce #(next-basis unordered-hash-factory % %2)
                       (basis-seed unordered-hash-factory)
                       this)]
        (set! hash (iint (hash-from-basis hb
                                          (dunaj.coll/count this))))))
    hash)
  IEquiv
  (-equiv [this other]
    (cond (identical? this other) true
          (not (class-instance? java.util.Set other)) false
          :else (let [m :- java.util.Set other]
                  (reduce
                   (fn [r k] (if (.contains m k) r (reduced false)))
                   true this))))
  IRed
  (-reduce [this reducef init]
    (if-let [tree (.-tree ^clojure.lang.PersistentTreeMap
                          (get-impl coll))]
      (reduce-rbt-section tree (wrap-fn reducef) init ascending?
                          (.comparator coll) begin end)
      init))
  ILookup
  (-contains? [this key]
    (if (range-check (.comparator coll) key ascending? begin end)
      (.contains coll key)
      false))
  (-get [this key not-found]
    (if (contains? this key) key not-found))
  ISorted
  (-key [this item] (-key coll item))
  (-ascending? [this] ascending?)
  ISortedSectionable
  (-sorted-section [this nb ne]
    (let [comparator (.comparator coll)]
      (if ascending?
        (let [nb (if (or (nil? nb)
                         (and (not (nil? begin))
                              (neg? (.compare comparator nb begin))))
                   begin nb)
              ne (if (or (nil? ne)
                         (and (not (nil? end))
                              (neg? (.compare comparator end ne))))
                   end ne)]
          (if (or (nil? ne) (nil? nb)
                  (neg? (.compare comparator nb ne)))
            (rbt-sorted-set-section coll ascending? nb ne)
            (-empty this)))
        (let [nb (if (or (nil? nb)
                         (and (not (nil? begin))
                              (neg? (.compare comparator begin nb))))
                   begin nb)
              ne (if (or (nil? ne)
                         (and (not (nil? end))
                              (neg? (.compare comparator ne end))))
                   end ne)]
          (if (or (nil? ne) (nil? nb)
                  (neg? (.compare comparator ne nb)))
            (rbt-sorted-set-section coll ascending? nb ne)
            (-empty this))))))
  ICounted
  (-count [this]
    (when (izero? count)
      (set! count (iint (reduce (fn [r v] (iinc r)) (i0) this))))
    count)
  IConfig
  (-config [this] {:comparator (.comparator coll)})
  IEmptyable
  (-empty [this]
    (if ascending?
      (-empty coll)
      (->FlippedRbtSortedSet (-empty coll))))
  IMeta
  (-meta [this] (meta coll))
  IPersistentMeta
  (-assoc-meta [this m]
    (rbt-sorted-set-section (assoc-meta coll m) ascending? begin end))
  IInvocable
  (-invoke [this key] (-get this key nil))
  (-invoke [this key not-found] (-get this key not-found))

  ;; Clojure compatibility
  clojure.lang.ISeq
  (seq [this] (clojure.bridge/red-to-seq this))
  clojure.lang.ILookup
  (valAt [this key] (-get this key nil))
  (valAt [this key not-found] (-get this key not-found))
  clojure.lang.Sorted
  (comparator [this] (.comparator coll))
  (entryKey [this entry] (.entryKey coll entry))
  (seq [this ascending?*]
    ;; TODO
    (not-implemented))
  (seqFrom [this key ascending*]
    ;; TODO
    (not-implemented))

  ;; JVM integration
  java.lang.Object
  (hashCode [this]
    (when (izero? hash-code)
      (set! hash-code
            (iint
             (-reduce
              this
              #(iadd % (if (nil? %2) (i0)
                           (.hashCode ^java.lang.Object %2)))
              (i0)))))
    hash-code)
  (equals [this other]
    (cond (identical? this other) true
          (not (class-instance? java.util.Set other)) false
          :else (let [m :- java.util.Set other]
                  (reduce*
                   (fn [r k] (if (.contains m k) r (reduced false)))
                   true this))))
  java.util.Set
  java.util.Collection
  (contains [this key] (contains? this key))
  (containsAll [this c]
    (clojure.core/every? #(-contains? this %) (seq c)))
  (isEmpty [this] (zero? (dunaj.coll/count this)))
  (size [this] (dunaj.coll/count this))
  (toArray [this] (clojure.lang.RT/seqToArray (seq this)))
  (toArray [this a] (clojure.lang.RT/seqToPassedArray (seq this) a))
  java.lang.Iterable
  (iterator [this] (coll->iterator this)))

(deftype FlippedRbtSortedSet
  "Red black tree flipped sorted set type."
  [oset :- clojure.lang.PersistentTreeSet]
  IHash
  (-hash [this] (hash oset))
  IEquiv
  (-equiv [this x] (= oset x)) ;; order does not affect equality
  IMeta
  (-meta [this] (.meta oset))
  IPersistentMeta
  (-assoc-meta [this m] (flipped-rbt-sorted-set (assoc-meta oset m)))
  IConfig
  (-config [this] (-config oset))
  IRed
  (-reduce [this reducef init]
    (if-let [tree (.-tree ^clojure.lang.PersistentTreeMap
                          (get-impl oset))]
      (reversed-reduce-rbt tree (wrap-fn reducef) init)
      init))
  ISeqable
  (-seq [this] (.seq oset false))
  ICounted
  (-count [this] (count oset))
  IEmptyable
  (-empty [this] (flipped-rbt-sorted-set (-empty oset)))
  ILookup
  (-contains? [this key] (.contains oset key))
  (-get [this key not-found] (if (.contains oset key) key not-found))
  IFlippable
  (-flip [this] oset)
  IReversible
  (-reverse [this] oset)
  ISorted
  (-key [this item] item)
  (-ascending? [this] false)
  ISortedSectionable
  (-sorted-section [this begin end]
    (rbt-sorted-set-section oset false begin end))
  IPersistentCollection
  (-conj [this x] (flipped-rbt-sorted-set (-conj oset x)))
  IInvocable
  (-invoke [this key] (oset key))
  (-invoke [this key not-found] (oset key not-found))

  ;; Abstract types
  IPersistentSet
  (-disj [this key] (flipped-rbt-sorted-set (-disj oset key)))

  ;; Clojure compatibility
  clojure.lang.ILookup
  (valAt [this key] (-get this key nil))
  (valAt [this key not-found] (-get this key not-found))
  clojure.lang.Reversible
  (rseq [this] (seq oset))
  clojure.lang.Sorted
  (comparator [this] (.comparator oset))
  (entryKey [this entry] (.entryKey oset entry))
  (seq [this ascending] (.seq oset ascending))
  (seqFrom [this key ascending] (.seqFrom oset key ascending))
  clojure.lang.IPersistentSet
  (get [this key] (get this key nil))

  ;; JVM integration
  java.lang.Object
  ;; order does not matter
  (hashCode [this] (.hashCode oset))
  (equals [this other] (.equals oset other))
  java.util.Set
  java.util.Collection
  (contains [this key] (.contains oset key))
  (containsAll [this c] (.containsAll oset c))
  (isEmpty [this] (.isEmpty oset))
  (size [this] (.size oset))
  (toArray [this] (.toArray oset))
  (toArray [this a] (.toArray oset a))
  java.lang.Iterable
  (iterator [this] (coll->iterator this)))

(defn ^:private flipped-rbt-sorted-set
  [coll]
  (when-not (empty? coll)
    (->FlippedRbtSortedSet coll)))

(defn rbt-sorted-set-section
  [coll ascending? begin end]
  (->RbtSortedSetSection coll ascending? begin end 0 0 0))


;;;; Public API

(deftype RbtSortedSet
  "A type for red black tree based sorted sets."
  clojure.lang.PersistentTreeSet
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.Set
  ;; j.l.Runnable, j.u.c.Callable
  ;; c.l.Reversible, c.l.Sorted
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
  IFlippable
  (-flip [this] (flipped-rbt-sorted-set this))
  IReversible
  (-reverse [this] (flipped-rbt-sorted-set this))
  ISorted
  (-key [this item] item)
  (-ascending? [this] true)
  ISortedSectionable
  (-sorted-section [this nb ne]
    (rbt-sorted-set-section this true nb ne))
  IConfig
  (-config [this] {:comparator (.comparator this)})
  IPersistentCollection
  IInvocable

  ;; Abstract types
  IPersistentSet)

(defn empty-rbt-sorted-set :- RbtSortedSet
  "Returns an empty RBT set with `_comparator_`. Will compare using
  natural total ordering (see
  `<<dunaj.compare.spi.ad#IComparable,IComparable>>`) if no
  comparator is given."
  {:added v1
   :see '[rbt-sorted-set-factory
          dunaj.coll.default/empty-sorted-set dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit dunaj.feature/config
          dunaj.coll/invert]}
  ([]
     clojure.lang.PersistentTreeSet/EMPTY)
  ([comparator :- Function]
     (if (nil? comparator)
       clojure.lang.PersistentTreeSet/EMPTY
       (clojure.lang.PersistentTreeSet/create comparator nil))))

;;; Factory

(defrecord RbtSortedSetFactory
  "A factory record for rbt sorted set."
  [comparator :- (Maybe Function)]
  ICollectionFactory
  (-from-coll [factory coll]
    (reduce conj (empty-rbt-sorted-set comparator) coll))
  (-from-items [factory] (empty-rbt-sorted-set comparator))
  (-from-items [factory a]
    (conj (empty-rbt-sorted-set comparator) a))
  (-from-items [factory a b]
    (conj (empty-rbt-sorted-set comparator) a b))
  (-from-items [factory a b c]
    (conj (empty-rbt-sorted-set comparator) a b c))
  (-from-items [factory a b c d]
    (conj (empty-rbt-sorted-set comparator) a b c d))
  (-from-items [factory a b c d more]
    (let [t (-> (empty-rbt-sorted-set comparator)
                (conj a) (conj b) (conj c) (conj d))]
      (reduce conj t more))))

(def rbt-sorted-set-factory :- ICollectionFactory
  "A RBT sorted set factory instance.
  Factory has following configuration options:

  * `comparator` - a comparator used for sorting items. `nil`
    represents a natural ordering, implemented with
    `<<dunaj.compare.spi.ad#IComparable,IComparable>>` protocol.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocols.

  New instances of RBT set can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[dunaj.coll.default/sorted-set-factory
          dunaj.coll.default/->sorted-set
          dunaj.coll.default/sorted-set
          dunaj.coll.default/->sorted-set-by
          dunaj.coll.default/sorted-set-by
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->RbtSortedSetFactory nil))
