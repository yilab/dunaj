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

(ns dunaj.coll.rbt-sorted-map
  "Red black tree based map, a sorted persistent collection.

  RBT sorted maps are persistent maps represented by a
  self-balancing binary search tree. Besides being sorted,
  they are counted and have support for efficient unpacked reduce,
  efficient sectioning, flipping and reversions.

  The comparator used for sorting the map is stored in the
  map's configuration and is accessible with
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
   [dunaj.type :refer [Any Fn U I Va Maybe AnyFn]]
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
     reduce empty? count section counted? seq empty single? get
     peek conj assoc conj! settle! edit assoc! -reduce-unpacked
     reduced? postponed? postponed advance first -empty second
     -conj -key -dissoc contains? reduced -get -flip
     unsafe-advance!]]
   [dunaj.function :refer
    [Function IInvocable fn defn apply constantly]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable fork join invoke -fold]]
   [dunaj.coll.helper :refer
    [fold-every reduce* fold* reduce-unpacked*
     coll->iterator strip-reduced]]
   [dunaj.host.array :refer
    [array-manager array to-array aget acount adapt]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.tuple :refer [pair]]
   [dunaj.coll.hamt-map]))


;;;; Implementation details

(defn ^:private unpacked-fn
  [f]
  (fn [val a b] (f val (pair a b))))

(def ^:private left-node-method :- java.lang.reflect.Method
  (doto (.getDeclaredMethod
         clojure.lang.PersistentTreeMap$Node
         "left" nil)
    (.setAccessible true)))

(defn left-node :- (Maybe clojure.lang.PersistentTreeMap$Node)
  [x :- clojure.lang.PersistentTreeMap$Node]
  (.invoke left-node-method x nil))

(def ^:private right-node-method :- java.lang.reflect.Method
  (doto (.getDeclaredMethod
         clojure.lang.PersistentTreeMap$Node
         "right" nil)
    (.setAccessible true)))

(defn right-node :- (Maybe clojure.lang.PersistentTreeMap$Node)
  [x :- clojure.lang.PersistentTreeMap$Node]
  (.invoke right-node-method x nil))

(defn ^:private range-check :- Boolean
  [comparator :- java.util.Comparator, key :- Any,
   ascending? :- Boolean, begin :- Any, end :- Any]
  (if ascending?
    (and (or (nil? begin)
             (inpos? (.compare comparator begin key)))
         (or (nil? end)
             (ineg? (.compare comparator key end))))
    (and (or (nil? begin)
             (inpos? (.compare comparator key begin)))
         (or (nil? end)
             (ineg? (.compare comparator end key))))))

(defn reduce-rbt-section
  [tree :- clojure.lang.PersistentTreeMap$Node, reducef :- AnyFn,
   init :- Any, ascending? :- Boolean,
   comparator :- java.util.Comparator, begin :- Any, end :- Any]
  (let [endp :- Function
        (cond (nil? end) (constantly true)
              ascending? #(ineg? (.compare comparator % end))
              :else #(ineg? (.compare comparator end %)))
        beginp :- Function
        (cond (nil? begin) (constantly true)
              ascending? #(not (ineg? (.compare comparator % begin)))
              :else #(not (ineg? (.compare comparator begin %))))
        af :- Function
        (fn af [ret act buf :- java.util.ArrayDeque]
          (cond
           (reduced? ret) ret
           (postponed? ret)
           (postponed @ret
                      #(af (advance ret) act (.clone buf))
                      #(af (unsafe-advance! ret) act buf))
           :else
           (if-let [n (if ascending?
                        (right-node act)
                        (left-node act))]
             ;; has next subtree,
             ;; push to buffer while it has other subtree
             (let [nact :- clojure.lang.PersistentTreeMap$Node
                   (loop [node
                          :- clojure.lang.PersistentTreeMap$Node n]
                     (if-let [l (if ascending?
                                  (left-node node)
                                  (right-node node))]
                       (do (.addLast buf node) (recur l))
                       (when (endp (.key node)) node)))]
               (if nact
                 (recur (reducef ret (.key nact) (.val nact))
                        nact buf)
                 ret))
             ;; no next tree, pop from buffer
             (let [nact :- clojure.lang.PersistentTreeMap$Node
                   (.pollLast buf)]
               (if (or (nil? nact) (not (endp (.key nact))))
                 ret
                 (recur (reducef ret (.key nact) (.val nact))
                        nact buf))))))
        buf :- java.util.ArrayDeque
        (java.util.ArrayDeque.)
        node :- clojure.lang.PersistentTreeMap$Node
        (if ascending?
          (loop [node :- clojure.lang.PersistentTreeMap$Node tree]
            (if (nil? node)
              (.pollLast buf)
              (if (beginp (.key node))
                (do (.addLast buf node) (recur (left-node node)))
                (recur (right-node node)))))
          (loop [node :- clojure.lang.PersistentTreeMap$Node tree]
            (if (nil? node)
              (.pollLast buf)
              (if (beginp (.key node))
                (do (.addLast buf node) (recur (right-node node)))
                (recur (left-node node))))))]
    (if (or (nil? node)
            (not (range-check comparator (.key node)
                              ascending? begin end)))
      init
      (af (reducef init (.key node) (.val node)) node buf))))

(defn ^:private reduce-rbt [tree reducef init]
  (let [af (fn af [ret act buf :- java.util.ArrayDeque]
             (cond
              (reduced? ret) ret
              (postponed? ret)
              (postponed @ret
                         #(af (advance ret) act (.clone buf))
                         #(af (unsafe-advance! ret) act buf))
              :else
              (if-let [r (right-node act)]
                ;; has right subtree,
                ;; push to buffer while it has left tree
                (let [nact :- clojure.lang.PersistentTreeMap$Node
                      (loop [node r]
                        (if-let [l (left-node node)]
                          (do (.addLast buf node) (recur l))
                          node))]
                  (recur (reducef ret (.key nact) (.val nact))
                         nact buf))
                ;; no right tree, pop from buffer
                (if-let [nact :- clojure.lang.PersistentTreeMap$Node
                         (.pollLast buf)]
                  (recur (reducef ret (.key nact) (.val nact))
                         nact buf)
                  ret))))
        buf (java.util.ArrayDeque.)
        node :- clojure.lang.PersistentTreeMap$Node
        (loop [node tree]
          (if-let [l (left-node node)]
            (do (.addLast buf node) (recur l))
            node))]
    (af (reducef init (.key node) (.val node)) node buf)))

(defn reversed-reduce-rbt [tree reducef init]
  (let [af (fn af [ret act buf :- java.util.ArrayDeque]
             (cond
              (reduced? ret) ret
              (postponed? ret)
              (postponed @ret
                         #(af (advance ret) act (.clone buf))
                         #(af (unsafe-advance! ret) act buf))
              :else
              (if-let [l (left-node act)]
                ;; has left subtree,
                ;; push to buffer while it has right tree
                (let [nact :- clojure.lang.PersistentTreeMap$Node
                      (loop [node l]
                        (if-let [r (right-node node)]
                          (do (.addLast buf node) (recur r))
                          node))]
                  (recur (reducef ret (.key nact) (.val nact))
                         nact buf))
                ;; no left tree, pop from buffer
                (if-let [nact :- clojure.lang.PersistentTreeMap$Node
                         (.pollLast buf)]
                  (recur (reducef ret (.key nact) (.val nact))
                         nact buf)
                  ret))))
        buf (java.util.ArrayDeque.)
        node :- clojure.lang.PersistentTreeMap$Node
        (loop [node tree]
          (if-let [r (right-node node)]
            (do (.addLast buf node) (recur r))
            node))]
    (af (reducef init (.key node) (.val node)) node buf)))

(declare ->FlippedRbtSortedMap)

(declare rbt-sorted-map-section)

(deftype RbtSortedMapSection
  "Red blact tree sorted map section type. Not persistent!"
  [coll :- clojure.lang.PersistentTreeMap, ascending? :- Boolean,
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
          (not (class-instance? java.util.Map other)) false
          :else (let [m :- java.util.Map other]
                  (strip-reduced
                   (reduce-unpacked*
                    this
                    (fn [r k v]
                      (if (and (.containsKey m k)
                               (clojure.lang.Util/equiv v (.get m k)))
                        r
                        (reduced false)))
                    true)))))
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (if-let [tree (.-tree coll)]
      (reduce-rbt-section tree reducef init ascending?
                          (.comparator coll) begin end)
      init))
  ILookup
  (-contains? [this key]
    (if (range-check (.comparator coll) key ascending? begin end)
      (.containsKey coll key)
      false))
  (-get [this key not-found]
    (if (range-check (.comparator coll) key ascending? begin end)
      (.valAt coll key not-found)
      not-found))
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
            (rbt-sorted-map-section coll ascending? nb ne)
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
            (rbt-sorted-map-section coll ascending? nb ne)
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
      (->FlippedRbtSortedMap (-empty coll))))
  IMeta
  (-meta [this] (meta coll))
  IPersistentMeta
  (-assoc-meta [this m]
    (rbt-sorted-map-section (assoc-meta coll m) ascending? begin end))
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
             (-reduce-unpacked
              this
              #(iadd % (ixor (if (nil? %2) (i0)
                                 (.hashCode ^java.lang.Object %2))
                             (if (nil? %3) (i0)
                                 (.hashCode ^java.lang.Object %3))))
              (i0)))))
    hash-code)
  (equals [this other]
    (cond (identical? this other) true
          (not (class-instance? java.util.Map other)) false
          :else (let [m :- java.util.Map other]
                  (strip-reduced
                   (reduce-unpacked*
                    this
                    (fn [r k v]
                      (if (and
                           (.containsKey m k)
                           (clojure.lang.Util/equals v (.get m k)))
                        r
                        (reduced false)))
                    true)))))
  java.util.Map
  (containsValue [this value] (.contains (.values this) value))
  (containsKey [this key] (contains? this key))
  (entrySet [this*]
    (proxy [java.util.AbstractSet] []
      (iterator [] (coll->iterator this*))
      (size [] (dunaj.coll/count this*))
      (hashCode [] (.hashCode ^java.lang.Object this*))
      (contains [e]
        (if (class-instance? java.util.Map$Entry e)
          (and
           (contains? this* (.getKey ^java.util.Map$Entry e))
           (clojure.lang.Util/equals
            (.getValue ^java.util.Map$Entry e)
            (get this* (.getKey ^java.util.Map$Entry e))))
          false))))
  (get [this key] (get this key))
  (isEmpty [this] (zero? (dunaj.coll/count this)))
  (keySet [this*]
    (proxy [java.util.AbstractSet] []
      (iterator []
        (.iterator ^java.lang.Iterable
                   (clojure.core/map first (seq this*))))
      (size [] (dunaj.coll/count this*))
      (contains [k] (contains? this* k))))
  (size [this] (dunaj.coll/count this))
  (values [this*]
    (proxy [java.util.AbstractCollection] []
      (size [] (dunaj.coll/count this*))
      (iterator []
        (.iterator ^java.lang.Iterable
                   (clojure.core/map second (seq this*))))))
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  clojure.lang.MapEquivalence)

(defn rbt-sorted-map-section
  [coll ascending? begin end]
  (->RbtSortedMapSection coll ascending? begin end (i0) (i0) (i0)))

(deftype FlippedRbtSortedMap
  "Red black tree flipped sorted map type."
  [coll :- clojure.lang.PersistentTreeMap]
  IHash
  (-hash [this] (hash coll))
  IEquiv
  (-equiv [this x] (= coll x)) ;; order does not affect equality
  IMeta
  (-meta [this] (.meta coll))
  IPersistentMeta
  (-assoc-meta [this m] (->FlippedRbtSortedMap (assoc-meta coll m)))
  IConfig
  (-config [this] (-config coll))
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (if-let [tree (.-tree coll)]
      (reversed-reduce-rbt tree reducef init)
      init))
  ISeqable
  (-seq [this] (.seq this false))
  ICounted
  (-count [this] (.count coll))
  IEmptyable
  (-empty [this] (->FlippedRbtSortedMap (-empty coll)))
  ILookup
  (-contains? [this key] (.containsKey coll key))
  (-get [this key not-found] (.valAt coll key not-found))
  IFlippable
  (-flip [this] coll)
  IReversible
  (-reverse [this] coll)
  ISorted
  (-key [this item] (-key coll item))
  (-ascending? [this] false)
  ISortedSectionable
  (-sorted-section [this begin end]
    (rbt-sorted-map-section coll false begin end))
  IPersistentCollection
  (-conj [this x] (->FlippedRbtSortedMap (-conj coll x)))
  IAssociative
  (-assoc [this k v] (->FlippedRbtSortedMap (.assoc coll k v)))
  (-entry [this key] (.entryAt coll key))
  IInvocable
  (-invoke [this key] (coll key))
  (-invoke [this key not-found] (coll key not-found))

  ;; Abstract types
  IPersistentMap
  (-dissoc [this key] (-dissoc coll key))

  ;; Clojure compatibility
  clojure.lang.ILookup
  (valAt [this key] (.valAt coll key))
  (valAt [this key not-found] (.valAt coll key not-found))
  clojure.lang.Reversible
  (rseq [this] (seq coll))
  clojure.lang.Sorted
  (comparator [this] (.comparator coll))
  (entryKey [this entry] (.entryKey coll entry))
  (seq [this ascending] (.seq coll ascending))
  (seqFrom [this key ascending] (.seqFrom coll key ascending))
  clojure.lang.Associative
  (containsKey [this key] (.containsKey coll key))

  ;; JVM integration
  java.lang.Object
  (hashCode [this] (.hashCode coll))
  (equals [this other] (.equals coll other))
  java.util.Map
  (containsValue [this value] (.containsValue coll value))
  (entrySet [this] (.entrySet coll))
  (get [this key] (.get coll key))
  (isEmpty [this] (.isEmpty coll))
  (keySet [this] (.keySet coll))
  (size [this] (.size coll))
  (values [this] (.values coll))
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  clojure.lang.MapEquivalence)

(deftype RbtSortedMap
  "Red black tree sorted map type."
  clojure.lang.PersistentTreeMap
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Map
  ;; j.l.Runnable, j.u.c.Callable
  ;; c.l.Sorted, c.l.Reversible
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (if-let [tree (.-tree this)] (reduce-rbt tree reducef init) init))
  ISeqable
  ICounted
  IEmptyable
  ILookup
  (-contains? [this key] (.containsKey this key))
  (-get [this key not-found] (.valAt this key not-found))
  IFlippable
  (-flip [this] (->FlippedRbtSortedMap this))
  IReversible
  (-reverse [this] (->FlippedRbtSortedMap this))
  ISorted
  (-key [this item]
    (if (class-instance? clojure.lang.IMapEntry item)
      (.key ^clojure.lang.IMapEntry item)
      (first item)))
  (-ascending? [this] true)
  ISortedSectionable
  (-sorted-section [this nb ne]
    (rbt-sorted-map-section this true nb ne))
  IConfig
  (-config [this] {:comparator (.comparator this)})
  ;; following protocols are already implemented
  IPersistentCollection
  IAssociative
  IInvocable
  ;; TODO: fold support?

  ;; Abstract types
  IPersistentMap)


;;;; Public API

(defn empty-rbt-sorted-map :- RbtSortedMap
  "Returns an empty RBT map with `_comparator_`. Will compare using
  natural total ordering (see
  `<<dunaj.compare.spi.ad#IComparable,IComparable>>`) if no
  comparator is given."
  {:added v1
   :see '[rbt-sorted-map-factory
          dunaj.coll.default/empty-sorted-map dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/assoc dunaj.coll/edit
          dunaj.feature/config]}
  ([]
     clojure.lang.PersistentTreeMap/EMPTY)
  ([comparator :- Function]
     (if (nil? comparator)
       clojure.lang.PersistentTreeMap/EMPTY
       (clojure.lang.PersistentTreeMap. nil comparator))))

;;; Factory

(defrecord RbtSortedMapFactory
  "A factory record for rbt sorted map."
  [comparator :- Function]
  ICollectionFactory
  (-from-coll [factory coll]
    (reduce conj (empty-rbt-sorted-map comparator) coll))
  (-from-items [factory]
    (empty-rbt-sorted-map comparator))
  (-from-items [factory a]
    (conj (empty-rbt-sorted-map comparator) a))
  (-from-items [factory a b]
    (conj (empty-rbt-sorted-map comparator) a b))
  (-from-items [factory a b c]
    (conj (empty-rbt-sorted-map comparator) a b c))
  (-from-items [factory a b c d]
    (conj (empty-rbt-sorted-map comparator) a b c d))
  (-from-items [factory a b c d more]
    (let [m (-> (empty-rbt-sorted-map comparator)
                (conj a) (conj b) (conj c) (conj d))]
      (reduce conj m more)))
  IConvolutionFactory
  (-convolute [factory c1 c2]
    (loop [ret (empty-rbt-sorted-map comparator)
           a1 (reduce #(postponed %2) nil c1)
           a2 (reduce #(postponed %2) nil c2)]
      (if (and (postponed? a1) (postponed? a2))
        (recur (assoc ret @a1 @a2)
               (unsafe-advance! a1) (unsafe-advance! a2))
        ret)))
  (-from-interleaved [factory] (empty-rbt-sorted-map comparator))
  (-from-interleaved [factory a b]
    (assoc (empty-rbt-sorted-map comparator) a b))
  (-from-interleaved [factory a b c d]
    (assoc (empty-rbt-sorted-map comparator) a b c d))
  (-from-interleaved [factory a b c d e f]
    (assoc (empty-rbt-sorted-map comparator) a b c d e f))
  (-from-interleaved [factory a b c d e f g h]
    (assoc (empty-rbt-sorted-map comparator) a b c d e f g h))
  (-from-interleaved [factory a b c d e f g h more]
    (apply assoc (empty-rbt-sorted-map comparator)
           a b c d e f g h more)))

(def rbt-sorted-map-factory
  :- (U ICollectionFactory IConvolutionFactory)
  "A RBT sorted map factory instance.
  Factory has following configuration options:

  * `comparator` - a comparator used for sorting items. `nil`
    represents a natural ordering, implemented with
    `<<dunaj.compare.spi.ad#IComparable,IComparable>>` protocol.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>` and
  `<<dunaj.coll.spi.ad#IConvolutionFactory,IConvolutionFactory>>`
  factory protocols.

  New instances of RBT map can be created with
  `<<dunaj.coll.api.ad#collection,collection>>`,
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`,
  `<<dunaj.coll.api.ad#convolution,convolution>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}convolution,-{gt}convolution>>`."
  {:added v1
   :see '[dunaj.coll.default/sorted-map-factory
          dunaj.coll.default/->sorted-map
          dunaj.coll.default/sorted-zipmap
          dunaj.coll.default/->sorted-map-by
          dunaj.coll.default/sorted-zipmap-by
          dunaj.coll/collection
          dunaj.coll/->collection
          dunaj.coll/convolution
          dunaj.coll/->convolution]}
  (->RbtSortedMapFactory nil))
