;; Copyright (C) 2013, 2015, Jozef Wagner, Zachary Tellman.
;; All rights reserved.
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

(ns dunaj.coll.tuple
  "A set of tuple types, a sequential persistent collection efficient
  for small vectors.

  NOTE: Taken and adapted from
  https://github.com/ztellman/clj-tuple[clj-tuple]
  by http://ideolalia.com/[Zachary Tellman], MIT license.

  This namespace provides number of tuple types for small
  vectors. Tuples have a constant time access to their items and
  efficient memory use. There is no structural sharing between
  tuples, which if efficient only for small vectors.

  NOTE: Current implementation switches to the `bvt-vector`
  when number of items exceeds *6*.

  Tuples have support for transients, folding,
  efficient reversion and flipping, sectioning, slicing and
  catenation. Tuples with 2 items also implement a host MapEntry
  interface, which makes them an ideal type for map `[key value]`
  entries.

  Besides tuples, Dunaj offers following persistent vector
  types:

  * <<dunaj.coll.bvt-vector.api.ad#,BVT vectors>> for efficient
    representation of medium to large vectors

  * <<dunaj.coll.rrbt-vector.api.ad#,RRB-Tree vectors>> for efficient
    catenation or insertion/removal of items in the middle of a
    vector.

  * <<dunaj.coll.primitive-vector.api.ad#,primitive vectors>>
    for efficient storage of host primitive data types.

  Just like all vector types, tuples add/remove items to/from the
  rear of the collection."
  {:categories ["Primary" "Pairs"]
   :authors ["Jozef Wagner" "Zachary Tellman"]}
  (:api bare)
  (:require
   [clojure.core :refer
    [map pr-str interpose re-find list vec doall range mapcat last
     butlast into reduce first second]]
   [clojure.bootstrap :refer [v1]]
   [clojure.core.rrb-vector.rrbt :refer [as-rrbt]]
   [dunaj.type :refer [Any]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [. set! class-instance?]]
   [dunaj.host.int :refer [Int iint iadd imul i31 i0 i1 isub i< iinc]]
   [dunaj.math :refer
    [nneg? < integer? == <= quot dec >= zero? mod > subtract
     add neg? multiply inc dec npos? one?]]
   [dunaj.compare :refer
    [IHash IEquiv nil? hash IComparable identical? =
     IHashBasis hash-from-basis compare basis-seed hash-basis
     next-basis ordered-hash-factory -hash-basis]]
   [dunaj.flow :refer
    [when-let cond loop recur if let do when delay condp case]]
   [dunaj.feature :refer
    [IMeta IPersistentMeta -assoc-meta assoc-meta meta]]
   [dunaj.threading :refer [-> ->>]]
   [dunaj.poly :refer [identical-type? defrecord deftype]]
   [dunaj.coll :refer
    [next ISequential IEmptyable IRed
     ISeq IEmptyAware IPeekable ICounted ICollectionFactory
     ISeqable ILookup IIndexed ISectionable IReversible -nth
     empty? count section counted? seq ->collection
     IUnpackedRed collection
     IFlippable -reverse -slice -section -contains? -rest
     empty ISliceable reduced? conj! settle! edit
     postponed postponed? seq? list? coll?
     IEditable ISettleable assoc!
     IMutableStacked IMutableMap IMutableAssociative
     IMutableCollection -edit IAssociative -assoc -cat
     assoc ICatenable IStacked
     IPersistentCollection IPersistentVector]]
   [dunaj.function :refer [IInvocable fn defn apply partial]]
   [dunaj.host.array :refer [aget aset!]]
   [dunaj.coll.helper :refer
    [equiv-ordered coll->iterator index-of equals-ordered
     compare-ordered coll->list-iterator advance-fn]]
   [dunaj.macro :refer [defmacro gensym]]
   [dunaj.string :refer [->str]]
   [dunaj.identifier :refer [symbol? symbol]]
   [dunaj.error :refer [throw index-out-of-bounds illegal-argument]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.empty-list]
   [dunaj.coll.cons-seq]
   [dunaj.coll.bvt-vector :refer
    [bvt-vector-factory empty-bvt-vector]]))


;;;; Implementation details

;;; Taken and adapted from clj-tuple by Zachary Tellman, MIT license.

(def revlist clojure.core/reverse)

(defn ^:private walk
  "Like `clojure.walk/walk`, but preserves metadata."
  [inner outer form]
  (let [x (cond
           (list? form) (outer (apply list (map inner form)))
           (class-instance? clojure.lang.IMapEntry form)
           (outer (vec (map inner form)))
           (seq? form) (outer (doall (map inner form)))
           (coll? form) (outer (into (empty form) (map inner form)))
           :else (outer form))]
    (if (class-instance? clojure.lang.IObj x)
      (assoc-meta x (meta form))
      x)))

(defn ^:private postwalk
  "Like `clojure.walk/postwalk`, but preserves metadata."
  [f form]
  (walk (partial postwalk f) f form))

(def ^:private gensym-regex
  #"(_|[a-zA-Z0-9\-\'\*]+)#?_+(\d+_*#?)+(auto__)?$")

(def ^:private unified-gensym-regex
  #"([a-zA-Z0-9\-\'\*]+)#__\d+__auto__$")

(defn ^:private unified-gensym? [s]
  (and (symbol? s) (re-find unified-gensym-regex (->str s))))

(defn ^:private un-gensym [s]
  (second (re-find gensym-regex (->str s))))

(defn ^:private unify-gensyms
  "All gensyms defined using two hash symbols are unified to the same
  value, even if they were defined within different syntax-quote
  scopes."
  [body]
  (let [gensym* (clojure.core/memoize gensym)]
    (postwalk
     #(if (unified-gensym? %)
        (symbol (->str (gensym* (->str (un-gensym %) "__"))
                       "__unified__"))
        %)
     body)))

(declare conj-tuple empty-tuple ->Tuple2 ->Tuple2M)

(defn ^:private throw-arity [actual]
  (throw (illegal-argument
          (->str "Wrong number of args (" actual ")"))))

(defmacro ^:private def-tuple [cardinality mc meta?]
  (let [fields (map #(symbol (->str "e" %)) (range cardinality))
        M (when meta? "M")
        Mfield (when meta? [`meta##])
        name (symbol (->str "Tuple" cardinality M))
        dec-name (symbol (->str "Tuple" (dec cardinality) M))
        inc-name (symbol (->str "Tuple" (inc cardinality) M))
        ->name (symbol (->str "->Tuple" cardinality M))
        ->mname (symbol (->str "->Tuple" cardinality "M"))
        ->nname (symbol (->str "->Tuple" cardinality))
        ->dec-name (symbol (->str "->Tuple" (dec cardinality) M))
        ->inc-name (symbol (->str "->Tuple" (inc cardinality) M))
        first? (zero? cardinality)
        last? (== mc cardinality)
        other (assoc-meta `x## {:tag (->str name)})
        to-vec (if meta?
                 `(assoc-meta
                    (->collection bvt-vector-factory ~@fields)
                    meta##)
                 `(->collection bvt-vector-factory ~@fields))
        lookup (fn this
                 ([idx] (this idx `(throw (index-out-of-bounds
                                           (->str ~idx)))))
                 ([idx not-found] `(let [idx# ~idx]
                                     (case idx#
                                       ~@(mapcat (fn [n field]
                                                   `(~n ~field))
                                                 (range) fields)
                                       ~not-found))))
        conj-tuple (if last?
                     `(-> empty-bvt-vector
                          edit
                          ~@(map (fn [x] `(conj! ~x)) fields)
                          (conj! val##)
                          settle!
                          ~@(when meta? [`(assoc-meta meta##)]))
                     `(~->inc-name ~@fields val## ~@Mfield))]
    (unify-gensyms
     `(do
        (declare ~->inc-name ~->nname)
        (deftype ~name [~@fields ~@Mfield]
          IComparable
          (-compare-to [this## x##]
            (if (identical-type? this## x##)
              ~(condp == cardinality
                 0 0
                 1 `(compare ~(first fields)
                             (. ~other ~(first fields)))
                 (reduce
                  (fn [form field]
                    `(let [cmp# (compare ~field (. ~other ~field))]
                       (if (zero? cmp#) ~form cmp#)))
                  0 (revlist fields)))
              (let [cnt# (count x##)]
                (if (== ~cardinality cnt#)
                  (compare-ordered this## x##)
                  (isub ~cardinality cnt#)))))
          IHash
          (-hash [this##]
            (hash-from-basis (-hash-basis this##) ~cardinality))
          IHashBasis
          (-hash-basis [this##]
            ~(if first?
               `(basis-seed ordered-hash-factory)
               (reduce (fn [form x] `(next-basis ordered-hash-factory
                                                 ~form ~x))
                       `(basis-seed ordered-hash-factory)
                       fields)))
          IEquiv
          (-equiv [this## x##]
            (if (identical-type? this## x##)
              ~(if first?
                 true
                 `(and ~@(map (fn [f] `(= ~f (. ~other ~f)))
                              fields)))
              (equiv-ordered this## x##)))
          IRed
          (-reduce [this## f## init##]
            ~(cond first? `init##
                   (one? cardinality) `(f## init## ~(first fields))
                   (== 2 cardinality)
                   `(let [af# (advance-fn [ret#]
                                (f## ret# ~(second fields)))]
                      (af# (f## init## ~(first fields))))
                   :else
                   `(let [af# (advance-fn [ret# x# :- Int]
                                (i< x# (iint ~cardinality))
                                (recur (f## ret# (-nth this## x#))
                                       (iinc x#))
                                :else ret#)]
                      (af# (f## init## ~(first fields)) (i1)))))
          ISeqable
          (-seq [this##]
            ~(cond first? nil
                   (one? cardinality)
                   `(.cons () ~(first fields))
                   (== 2 cardinality)
                   `(.cons (.cons () ~(second fields))
                           ~(first fields))
                   :else
                   ;; TODO: more efficient seqs
                   (reduce (fn [ret val]
                             (list '.cons ret val))
                           () (clojure.core/reverse fields))))
          ICounted
          (-count [_] ~cardinality)
          IEmptyAware
          (-empty? [_] ~first?)
          IEmptyable
          (-empty [_] (assoc-meta empty-tuple ~(when meta? `meta##)))
          IPeekable
          (-peek [_] ~(last fields))
          ILookup
          (-contains? [this## key##]
            (and (integer? key##) (< -1 key## ~mc)))
          (-get [this## k## not-found##]
            (if (integer? k##)
              (-nth this## k## not-found##)
              not-found##))
          IIndexed
          (-nth [this## index## not-found##]
            ~(lookup `(iint index##) `not-found##))
          IFlippable
          (-flip [_] (~->name ~@(revlist fields) ~@Mfield))
          IReversible
          (-reverse [_] (~->name ~@(revlist fields) ~@Mfield))
          ISliceable
          (-slice [_ begin## end##] (-slice ~to-vec begin## end##))
          ISectionable
          (-section [_ begin## end##]
            (-section ~to-vec begin## end##))
          ISequential
          IEditable
          (-edit [this## capacity-hint##]
            ;; TODO: more efficient transients
            (-> empty-bvt-vector
                (edit capacity-hint##)
                ~@(map (fn [x] `(conj! ~x)) fields)))
          IPersistentCollection
          (-conj [this## val##] ~conj-tuple)
          IAssociative
          (-entry [this## key##]
            (when (-contains? this## key##)
              (->Tuple2 key## ~(lookup `(iint key##)))))
          (-assoc [this## key## val##]
            (case (iint key##)
              ~@(mapcat
                 (fn [idx field]
                   `(~idx (~->name
                           ~@(-> fields vec (assoc idx `val##))
                           ~@Mfield)))
                 (range)
                 fields)
              ~cardinality ~conj-tuple
              (throw (index-out-of-bounds (->str key##)))))
          IStacked
          (-pop [this##]
            ~(if first?
               `(throw (illegal-argument
                        "Cannot pop from an empty vector."))
               `(~->dec-name ~@(butlast fields) ~@Mfield)))
          ICatenable
          (-cat [this## x##] (-cat ~to-vec x##))
          IMeta
          (-meta [_] ~(when meta? `meta##))
          IPersistentMeta
          (-assoc-meta [this## m##]
            (if (nil? m##)
              (~->nname ~@fields)
              (~->mname ~@fields m##)))
          IInvocable
          (-invoke [_ index##] ~(lookup `(iint index##)))
          (-apply [this## args##]
            (let [cnt# (count args##)]
              (if (== 1 cnt#)
                ~(lookup `(iint (first args##)))
                (throw-arity cnt#))))
          IPersistentVector

          ;; Clojure interop
          clojure.lang.ILookup
          (valAt [this## k##] ~(lookup `(iint k##)))
          (valAt [this## k## not-found##]
            (-nth this## k## not-found##))
          clojure.lang.Indexed
          (nth [this## i##] ~(lookup `(iint i##)))
          clojure.lang.Reversible
          (rseq [this##] (seq (-reverse this##)))
          clojure.lang.IEditableCollection
          (asTransient [this##] (-edit this## nil))
          clojure.lang.Associative
          (containsKey [this## key##] (-contains? this## key##))
          clojure.lang.IPersistentVector
          (length [this##] ~cardinality)
          (assocN [this## key## val##] (-assoc this## key## val##))
          clojure.core.rrb_vector.rrbt.AsRRBT
          (as-rrbt [this] (as-rrbt ~to-vec))
          ;; JVM interop
          java.lang.Object
          (hashCode [_]
            ~(if (zero? cardinality)
               1
               `(iint
                 ~(reduce (fn [form x]
                            `(iadd
                              (imul (i31) ~form)
                              (if (nil? ~x)
                                (i0)
                                (.hashCode ^java.lang.Object ~x))))
                          (i1) fields))))
          (equals [this## x##]
            (if (identical-type? this## x##)
              ~(if first?
                 true
                 `(and ~@(map (fn [f] `(clojure.lang.Util/equals
                                        ~f (. ~other ~f))) fields)))
              (equals-ordered this## x##)))
          (toString [_]
            (->str "[" ~@(->> fields
                            (map (fn [f] `(pr-str ~f)))
                            (interpose " ")) "]"))
          java.lang.Iterable
          (iterator [this##] (coll->iterator this##))
          java.util.Collection
          (contains [this## x##] (-contains? this## x##))
          (containsAll [this## c##]
            (clojure.core/every? #(-contains? this## %) (seq c##)))
          (isEmpty [this##] ~first?)
          (size [this##] ~cardinality)
          (toArray [this##] (clojure.lang.RT/seqToArray (seq this##)))
          (toArray [this## a##]
            (clojure.lang.RT/seqToPassedArray (seq this##) a##))
          java.util.List
          (get [this## index##] (.nth this## index##))
          (indexOf [this## x##] (index-of this## x##))
          (lastIndexOf [this## x##]
            (isub ~(dec cardinality)
                  (index-of (-reverse this##) x##)))
          (listIterator [this##] (.listIterator this## 0))
          (listIterator [this## i##] (coll->list-iterator this## i##))
          (subList [this## begin## end##]
            (-section this## begin## end##))
          java.util.RandomAccess

          ;; other
          ~@(when (== 2 cardinality)
              `(clojure.lang.IMapEntry
                (key [_] ~(first fields))
                (val [_] ~(second fields))

                java.util.Map$Entry
                (getKey [_] ~(first fields))
                (getValue [_] ~(second fields)))))))))

(defmacro ^:private def-tuples
  [max]
  `(do ~@(map #(list `def-tuple % 6 true) (range (inc max)))
       ~@(map #(list `def-tuple % 6 false) (range (inc max)))))

(def-tuples 6)


;;;; Public API

(def empty-tuple :- Tuple0
  "An empty tuple."
  {:added v1
   :category "Primary"
   :see '[tuple-factory,
          dunaj.coll.primitive-vector/empty-primitive-vector-of,
          dunaj.coll.bvt-vector/empty-bvt-vector,
          dunaj.coll.default/empty-vec, dunaj.coll.util/into,
          dunaj.coll/conj, dunaj.coll/edit]}
  (->Tuple0))

(defn tuple :- IPersistentVector
  "Returns a tuple which behaves like a vector, but is highly
  efficient for index lookups, hash calculations, equality checks,
  and reduction. If there are more than six items, returns a
  normal vector."
  {:added v1
   :category "Primary"
   :see '[pair dunaj.coll.default/->vec]}
  ([] empty-tuple)
  ([x] (->Tuple1 x))
  ([x y] (->Tuple2 x y))
  ([x y z] (->Tuple3 x y z))
  ([x y z w] (->Tuple4 x y z w))
  ([x y z w u] (->Tuple5 x y z w u))
  ([x y z w u v] (->Tuple6 x y z w u v))
  ([x y z w u v & rst]
     (apply ->collection bvt-vector-factory x y z w u v rst)))

(defn pair :- Tuple2
  "Returns a pair tuple containing `_x_` and `_y_`."
  {:added v1
   :category "Pairs"
   :see '[tuple key val dunaj.coll.default/->vec]}
  [x y]
  (->Tuple2 x y))

(defn key :- Any
  "Returns key of a given `_pair_` tuple. More efficient than `first`.
  Also works with host pairs, e.g. map entry in JVM."
  {:added v1
   :category "Pairs"
   :see '[tuple pair val dunaj.coll/first]}
  [pair :- java.util.Map$Entry]
  (.getKey pair))

(defn val :- Any
  "Returns value of a given `_pair_` tuple. More efficient than
  `second`. Also works with host pairs, e.g. map entry in JVM."
  {:added v1
   :category "Pairs"
   :see '[tuple pair key dunaj.coll/second]}
  [pair :- java.util.Map$Entry]
  (.getValue pair))

;;; Factory

(defrecord TupleFactory
  "A factory record for tuples."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (let [vec (collection bvt-vector-factory coll)]
      (if (< (count vec) 7) (apply tuple vec) vec)))
  (-from-items [factory] empty-tuple)
  (-from-items [factory a] (->Tuple1 a))
  (-from-items [factory a b] (->Tuple2 a b))
  (-from-items [factory a b c] (->Tuple3 a b c))
  (-from-items [factory a b c d] (->Tuple4 a b c d))
  (-from-items [factory a b c d more] (apply tuple a b c d more)))

(def tuple-factory :- ICollectionFactory
  "A tuple factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of tuple can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[tuple]
   :category "Primary"}
  (->TupleFactory))
