;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; Additional copyright for parts of documentation and/or
;; underlying implementation:
;; Copyright (C) 2012, 2015, Michał Marczyk, Rich Hickey
;; and Clojure contributors
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

(ns dunaj.coll.primitive-vector
  "A vector holding primitive values. A sequential persistent
  collection.

  This collection type stores unboxed primitive items, while
  preserving all characteristics of persistent vectors.

  NOTE: Current implementation is based on RRB-Tree vectors.

  NOTE: Current API is tied to the JVM host and its primitive data
  types.

  Primitive vectors have support for transients, folding, batched
  reduce, efficient reversion, sectioning and slicing.
  Besides primitive vector type, Dunaj offers following persistent
  vector types:

  * <<dunaj.coll.bvt-vector.api.ad#,BVT vectors>> for efficient
    representation of medium to large vectors

  * <<dunaj.coll.tuple.api.ad#,tuples>> for efficient representation
    of small vectors

  * <<dunaj.coll.rrbt-vector.api.ad#,RRB-Tree vectors>> for efficient
    catenation or insertion/removal of items in the middle of a
    vector.

  Just like all vector types, primitive vectors add/remove items
  to/from the rear of the collection.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  functions defined in `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :categories ["Primary"
                ["Factories"
                 "Note that there is no primitive vector factory for
                 booleans, as there is no batch support for them."]]
   :additional-copyright
   "2012, 2015, Michał Marczyk, Rich Hickey and Clojure contributors"}
  (:api bare)
  (:require
   [clojure.core.rrb-vector :refer [vector-of]]
   [clojure.core.rrb-vector.rrbt :refer [as-rrbt]]
   [clojure.core.rrb-vector.nodes :refer [ranges]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Va U I Maybe]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer
    [Class AnyArray class-instance? class provide-class]]
   [dunaj.host.int :refer
    [Int i< iinc i0 i<< i5 iint i<= i>= i> i31 i32 idiv imul idec
     iand inneg? ineg? isub iloop izero? i>> imin iadd imax i==]]
   [dunaj.math :refer
    [nneg? < integer? dec >= > add neg? inc dec zero? ==]]
   [dunaj.compare :refer
    [IHash IEquiv IComparable nil? hash = identical?]]
   [dunaj.flow :refer
    [cond loop recur if let do when if-not when-not case condp]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta assoc-meta]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [Type deftype extend-type! defrecord]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable ILookup IIndexed ISectionable
     IReversible IEditable ISettleable IMutableStacked IStacked
     IMutableAssociative IMutableCollection IPersistentCollection
     IPersistentVector IAssociative ICatenable ISliceable
     IHomogeneous IBatchedRed -empty -slice -edit
     first next postponed? postponed reduce empty? count -nth
     section counted? seq reduced? conj! settle! edit conj -count
     -section -nth -get item-type -contains? -reverse get nth -peek]]
   [dunaj.function :refer [IInvocable fn defn -invoke]]
   [dunaj.concurrent.forkjoin :refer [IFoldable -fold]]
   [dunaj.host.batch :refer
    [batch-manager batch-on item-types-match?]]
   [dunaj.host.array :refer [aget]]
   [dunaj.coll.helper :refer
    [fold-sectionable prepare-ordered-section equals-ordered
     equiv-ordered reduce* advance-fn]]
   [dunaj.string :refer [->str]]
   [dunaj.error :refer [throw index-out-of-bounds illegal-argument]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.vector-section :refer
    [IReversedVectorSectionHelper IVectorSectionHelper
     reversed-vector-section vector-section]]
   [dunaj.coll.bvt-vector]
   [dunaj.coll.rrbt-vector :refer [offset offset-transient]]))


;;;; Implementation details

(defn ^:private reduce-batched-section
  [vec :- clojure.core.rrb_vector.rrbt.Vector, reducef :- AnyFn,
   init :- Any, begin :- Int, end :- Int]
  (let [am ^clojure.core.ArrayManager (.-am vec)]
    (if-not (i< begin end)
      init
      (let [bm (batch-manager
                (.getComponentType (class (.array am 0))))
            af (advance-fn [ret :- Any, i :- Int, oi :- Int]
                 (let [arr (.arrayFor vec i)
                       al (.alength am arr)
                       left (isub end i)]
                   (if (i<= left al)
                     (reducef ret (batch-on bm arr oi left))
                     (recur (reducef ret (batch-on arr oi al))
                            (iadd i al) (i0)))))
            oi (offset vec begin)]
        (af init (isub begin oi) oi)))))

(defn ^:private reduce-batched-mutable-section
  [vec :- clojure.core.rrb_vector.rrbt.Transient, reducef :- AnyFn,
   init :- Any, begin :- Int, end :- Int]
  (let [am ^clojure.core.ArrayManager (.-am vec)]
    (if-not (i< begin end)
      init
      (let [bm (batch-manager
                (.getComponentType (class (.array am 0))))
            af (advance-fn [ret :- Any, i :- Int, oi :- Int]
                 (let [arr (.arrayFor vec i)
                       al (.alength am arr)
                       left (isub end i)]
                   (if (i<= left al)
                     (reducef ret (batch-on bm arr oi left))
                     (recur (reducef ret (batch-on arr oi al))
                            (iadd i al) (i0)))))
            oi (offset-transient vec begin)]
        (af init (isub begin oi) oi)))))

(declare cat-helper ->MutablePrimitiveVector
         ->PrimitiveVectorSection ->PrimitiveVector)


;;;; Public API

(deftype PrimitiveReversedVectorSection
  "Primitive reversed vector section. Just because of IHomogeneous."
  [vec :- dunaj.coll.vector_section.ReversedVectorSection]
  IComparable
  (-compare-to [this other] (.compareTo vec other))
  IHash
  (-hash [this] (hash vec))
  IEquiv
  (-equiv [this other] (= vec other))
  IMeta
  (-meta [this] (.meta vec))
  IPersistentMeta
  (-assoc-meta [this m]
    (->PrimitiveReversedVectorSection (.withMeta vec m)))
  IRed
  (-reduce [this reducef init] (reduce* vec reducef init))
  ISeqable
  (-seq [this] (seq vec))
  ICounted
  (-count [this] (-count vec))
  IEmptyable
  (-empty [this] (->PrimitiveVector (-empty vec)))
  IPeekable
  (-peek [this] (-peek vec))
  ILookup
  (-contains? [this key] (-contains? vec key))
  (-get [this key not-found] (-get vec key not-found))
  IIndexed
  (-nth [this index not-found] (-nth vec index not-found))
  IReversible
  (-reverse [this]
    (let [r (-reverse vec)]
      (if (class-instance? clojure.core.rrb_vector.rrbt.Vector r)
        (->PrimitiveVector r)
        (->PrimitiveVectorSection r))))
  ISectionable
  (-section [this nb ne]
    (->PrimitiveReversedVectorSection (-section vec nb ne)))
  ISliceable
  (-slice [this nb ne]
    (->PrimitiveReversedVectorSection (-slice vec nb ne)))
  IHomogeneous
  (-item-type [this]
    (let [rrbt-coll :- clojure.core.rrb_vector.rrbt.Vector
          (.-vec vec)]
      (.getComponentType (class (.array ^clojure.core.ArrayManager
                                        (.-am rrbt-coll) 0)))))
  ISequential
  IInvocable
  (-invoke [this arg] (-invoke vec arg))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (-fold vec reduce-fn pool n combinef reducef))

  ;; Abstract types
  IPersistentVector

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this index] (.nth vec index))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] (.hashCode vec))
  (equals [this other] (equals-ordered this other))
  java.lang.Iterable
  (iterator [this] (.iterator vec))
  java.util.Collection
  (contains [this x] (.contains vec x))
  (containsAll [this c] (.containsAll vec c))
  (isEmpty [this] (.isEmpty vec))
  (size [this] (.size vec))
  (toArray [this] (.toArray vec))
  (toArray [this a] (.toArray vec a))
  java.util.List
  (get [this index] (.get vec index))
  (indexOf [this x] (.indexOf vec x))
  (lastIndexOf [this x] (.lastIndexOf vec x))
  (listIterator [this] (.listIterator vec))
  (listIterator [this i] (.listIterator vec i))
  (subList [this begin end] (.subList vec begin end))
  java.util.RandomAccess)

(deftype PrimitiveVectorSection
  "Primitive vector section. Wrapper with support for batch and
  items type."
  [vec :- clojure.lang.APersistentVector$SubVector]
  IComparable
  (-compare-to [this other] (.compareTo vec other))
  IHash
  (-hash [this] (hash vec))
  IEquiv
  (-equiv [this other] (= vec other))
  IMeta
  (-meta [this] (.meta vec))
  IPersistentMeta
  (-assoc-meta [this m] (->PrimitiveVectorSection (.withMeta vec m)))
  IRed
  (-reduce [this reducef init] (reduce* vec reducef init))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match? requested-type (item-type this))
      (throw (illegal-argument
              (->str "requested type is not supported"
                     requested-type))))
    (reduce-batched-section
     (.-v vec) reducef init (.-start vec) (.-end vec)))
  ISeqable
  (-seq [this] (.seq vec))
  ICounted
  (-count [this] (.count vec))
  IEmptyable
  (-empty [this] (->PrimitiveVector (.empty vec)))
  IPeekable
  (-peek [this] (.peek vec))
  ILookup
  (-contains? [this key] (-contains? vec key))
  (-get [this key not-found] (-get vec key not-found))
  IIndexed
  (-nth [this index not-found] (-nth vec index not-found))
  IReversible
  (-reverse [this] (->PrimitiveReversedVectorSection (-reverse vec)))
  ISliceable
  (-slice [this nb ne]
    (->PrimitiveVector (-slice vec nb ne)))
  ISectionable
  (-section [this nb ne]
    (->PrimitiveVectorSection (-section vec nb ne)))
  IHomogeneous
  (-item-type [this]
    (let [rrbt-coll :- clojure.core.rrb_vector.rrbt.Vector (.-v vec)]
      (.getComponentType (class (.array ^clojure.core.ArrayManager
                                        (.-am rrbt-coll) 0)))))
  ISequential
  IPersistentCollection
  (-conj [this x] (->PrimitiveVectorSection (.cons vec x)))
  IAssociative
  (-entry [this key] (.entryAt vec key))
  (-assoc [this key val]
    (->PrimitiveVectorSection (.assoc vec key val)))
  IStacked
  (-pop [this] (->PrimitiveVectorSection (.pop vec)))
  IInvocable
  (-invoke [this arg] (.invoke vec arg))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  IPersistentVector

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this index] (.nth vec index))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] (.hashCode vec))
  (equals [this other] (equals-ordered this other))
  java.lang.Iterable
  (iterator [this] (.iterator vec))
  java.util.Collection
  (contains [this x] (.contains vec x))
  (containsAll [this c] (.containsAll vec c))
  (isEmpty [this] (.isEmpty vec))
  (size [this] (.size vec))
  (toArray [this] (.toArray vec))
  (toArray [this a] (.toArray vec a))
  java.util.List
  (get [this index] (.get vec index))
  (indexOf [this x] (.indexOf vec x))
  (lastIndexOf [this x] (.lastIndexOf vec x))
  (listIterator [this] (.listIterator vec))
  (listIterator [this i] (.listIterator vec i))
  (subList [this begin end] (.subList vec begin end))
  java.util.RandomAccess)

(deftype PrimitiveVector
  "A type for primitive vectors. Wrapper around rrbt-coll so that
  support for batch and item type can be added cleanly."
  [vec :- clojure.core.rrb_vector.rrbt.Vector]
  IComparable
  (-compare-to [this other] (.compareTo vec other))
  IHash
  (-hash [this] (hash vec))
  IEquiv
  (-equiv [this other] (= vec other))
  IMeta
  (-meta [this] (.meta vec))
  IPersistentMeta
  (-assoc-meta [this m] (->PrimitiveVector (.withMeta vec m)))
  IRed
  (-reduce [this reducef init] (reduce* vec reducef init))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match? requested-type (item-type this))
      (throw (illegal-argument
              (->str "requested type is not supported"
                     requested-type))))
    ;; size-hint is ignored
    (reduce-batched-section vec reducef init (i0) (.-cnt vec)))
  ISeqable
  (-seq [this]
    (when-not (izero? (.-cnt vec))
      (assoc-meta (clojure.lang.APersistentVector$Seq. vec (i0))
        (meta vec))))
  ICounted
  (-count [this] (.-cnt vec))
  IEmptyable
  (-empty [this] (->PrimitiveVector (.empty vec)))
  IPeekable
  (-peek [this] (.peek vec))
  ILookup
  (-contains? [this key] (-contains? vec key))
  (-get [this key not-found] (-get vec key not-found))
  IIndexed
  (-nth [this index not-found] (-nth vec index not-found))
  IReversible
  (-reverse [this] (->PrimitiveReversedVectorSection (-reverse vec)))
  ISliceable
  (-slice [this nb ne]
    (let [ne (prepare-ordered-section nb ne (.-cnt vec))]
      (->PrimitiveVector (.slicev vec nb ne))))
  ISectionable
  (-section [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (.-cnt vec)))]
      (if (and (izero? nb) (i== ne (.-cnt vec)))
        this
        (->PrimitiveVectorSection (-section vec nb ne)))))
  IHomogeneous
  (-item-type [this]
    (.getComponentType (class (.array ^clojure.core.ArrayManager
                                      (.-am vec) 0))))
  IEditable
  (-edit [this capacity-hint] (->MutablePrimitiveVector (-edit vec)))
  ISequential
  IPersistentCollection
  (-conj [this x] (->PrimitiveVector (.cons vec x)))
  IAssociative
  (-entry [this key] (.entryAt vec key))
  (-assoc [this key val] (->PrimitiveVector (.assoc vec key val)))
  IStacked
  (-pop [this] (->PrimitiveVector (.pop vec)))
  ICatenable
  (-cat [this other]
    (->PrimitiveVector
     (if (class-instance? clojure.core.rrb_vector.rrbt.Vector other)
       (.splicev vec other)
       (cat-helper this other))))
  IInvocable
  (-invoke [this arg] (.invoke vec arg))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  IPersistentVector

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this index] (.nth vec index))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))
  clojure.lang.IEditableCollection
  (asTransient [this] (edit this nil))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] (.hashCode vec))
  (equals [this other] (equals-ordered this other))
  java.lang.Iterable
  (iterator [this] (.iterator vec))
  java.util.Collection
  (contains [this x] (.contains vec x))
  (containsAll [this c] (.containsAll vec c))
  (isEmpty [this] (.isEmpty vec))
  (size [this] (.size vec))
  (toArray [this] (.toArray vec))
  (toArray [this a] (.toArray vec a))
  java.util.List
  (get [this index] (.get vec index))
  (indexOf [this x] (.indexOf vec x))
  (lastIndexOf [this x] (.lastIndexOf vec x))
  (listIterator [this] (.listIterator vec))
  (listIterator [this i] (.listIterator vec i))
  (subList [this begin end] (.subList vec begin end))
  java.util.RandomAccess)

(defn ^:private cat-helper :- clojure.core.rrb_vector.rrbt.Vector
  [vec :- clojure.core.rrb_vector.rrbt.Vector
   other :- PrimitiveVector]
  (.splicev vec (.-vec other)))

(deftype MutablePrimitiveVector
  "A type for mutable primitive vectors based on RRBT."
  [mvec :- clojure.core.rrb_vector.rrbt.Transient]
  IRed
  (-reduce [this reducef init] (reduce* mvec reducef init))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match? requested-type (item-type this))
      (throw (illegal-argument
              (->str "requested type is not supported"
                     requested-type))))
    ;; size-hint is ignored
    (reduce-batched-mutable-section
     mvec reducef init (i0) (.count mvec)))
  ICounted
  (-count [this] (.count mvec))
  IPeekable
  (-peek [this] (-nth mvec (idec (.count mvec)) nil))
  ILookup
  (-contains? [this key] (-contains? mvec key))
  (-get [this key not-found]
    (if (integer? key) (.nth mvec key not-found) not-found))
  IIndexed
  (-nth [this index not-found] (.nth mvec index not-found))
  ISettleable
  (-settle! [this] (->PrimitiveVector (.persistent mvec)))
  IMutableCollection
  (-conj! [this x] (->MutablePrimitiveVector (.conj mvec x)))
  IMutableAssociative
  (-assoc! [this k v] (->MutablePrimitiveVector (.assoc mvec k v)))
  IMutableStacked
  (-pop! [this] (->MutablePrimitiveVector (.pop mvec)))
  IInvocable
  (-invoke [this x] (.invoke mvec x)))

(defn ^:private class->keyword
  [c]
  (condp identical? c
    java.lang.Byte/TYPE :byte
    java.lang.Integer/TYPE :int
    java.lang.Long/TYPE :long
    java.lang.Float/TYPE :float
    java.lang.Double/TYPE :double
    java.lang.Short/TYPE :short
    java.lang.Character/TYPE :char
    java.lang.Boolean/TYPE :boolean
    :object))

(defn empty-primitive-vector-of :- IPersistentVector
  "Returns an empty primitive vector of given type `_t_`, which can be
  a host class, a type or a keyword. See
  `<<dunaj.host.api.ad#keyword__GT_class,keyword->class>>` for the
  list of valid keywords."
  {:added v1
   :category "Primary"
   :see '[empty-int-vec
          empty-long-vec
          empty-float-vec
          empty-double-vec
          empty-byte-vec
          empty-short-vec
          empty-char-vec
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  [t :- (U nil Class Type)]
  (->PrimitiveVector (vector-of (class->keyword (provide-class t)))))

(def empty-int-vec :- PrimitiveVector
  "An empty primitive int vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :int))

(def empty-long-vec :- PrimitiveVector
  "An empty primitive long vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :long))

(def empty-float-vec :- PrimitiveVector
  "An empty primitive float vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :float))

(def empty-double-vec :- PrimitiveVector
  "An empty primitive double vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :double))

(def empty-byte-vec :- PrimitiveVector
  "An empty primitive byte vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :byte))

(def empty-short-vec :- PrimitiveVector
  "An empty primitive short vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :short))

(def empty-char-vec :- PrimitiveVector
  "An empty primitive char vector.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Primary"
   :see '[empty-primitive-vector-of
          primitive-vector-factory-of
          dunaj.coll.bvt-vector/empty-bvt-vector
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  (empty-primitive-vector-of :char))

;; no boolean vector as it does not support batching

(defrecord PrimitiveVectorFactory
  "A factory record for primitive vectors."
  [t :- (U nil Class Type)]
  ICollectionFactory
  (-from-coll [factory coll]
    (settle!
     (reduce conj! (edit (empty-primitive-vector-of t)) coll)))
  (-from-items [factory] (empty-primitive-vector-of t))
  (-from-items [factory a] (conj (empty-primitive-vector-of t) a))
  (-from-items [factory a b]
    (conj (empty-primitive-vector-of t) a b))
  (-from-items [factory a b c]
    (conj (empty-primitive-vector-of t) a b c))
  (-from-items [factory a b c d]
    (conj (empty-primitive-vector-of t) a b c d))
  (-from-items [factory a b c d more]
    (let [t (edit (empty-primitive-vector-of t))
          t (-> t (conj! a) (conj! b) (conj! c) (conj! d))]
      (settle! (reduce conj! t more)))))

(defn primitive-vector-factory-of :- ICollectionFactory
  "Returns a primitive vector factory for type `_t_`, which can be
  a host class, a type or a keyword. See
  `<<dunaj.host.api.ad#keyword__GT_class,keyword->class>>` for the
  list of valid keywords.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :category "Factories"
   :see '[int-vec-factory
          long-vec-factory
          float-vec-factory
          double-vec-factory
          byte-vec-factory
          short-vec-factory
          char-vec-factory
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  [t :- (U nil Class Type)]
  (->PrimitiveVectorFactory t))

(def int-vec-factory :- ICollectionFactory
  "A primitive int vector factory instance.
  Currently there are no options.

  NOTE: More `Int` related functions can be found in
  `<<dunaj.host.int.api.ad#,dunaj.host.int>>` namespace.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :int))

(def long-vec-factory :- ICollectionFactory
  "A primitive long vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :long))

(def float-vec-factory :- ICollectionFactory
  "A primitive float vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :float))

(def double-vec-factory :- ICollectionFactory
  "A primitive double vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :double))

(def byte-vec-factory :- ICollectionFactory
  "A primitive byte vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :byte))

(def short-vec-factory :- ICollectionFactory
  "A primitive short vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :short))

(def char-vec-factory :- ICollectionFactory
  "A primitive char vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of primitive vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`.

  NOTE: This var is specific to JVM host."
  {:added v1
   :category "Factories"
   :see '[primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.bvt-vector/bvt-vector-factory
          dunaj.coll.default/vec-factory-of
          dunaj.coll.default/->vec-of
          dunaj.coll.default/vec-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (primitive-vector-factory-of :char))
