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

(ns dunaj.coll.util
  "Various collection utilities, mainly those returning either a
  non-collection result or a non-recipe collection."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Iterations" "Reducers" "Maps" "Walk"]}
  (:api bare)
  (:require
   [clojure.core :refer [lazy-seq cons]]
   [clojure.bootstrap :refer [v1 not-implemented]]
   [clojure.set]
   [dunaj.type :refer [U Maybe Any AnyFn Fn Va Predicate Macro]]
   [dunaj.boolean :refer [Boolean and or not]]
   [dunaj.host :refer [Class class-instance?]]
   [dunaj.host.int :refer [Int iint iinc izero? ipos? idec iloop]]
   [dunaj.math :refer [Integer == dec]]
   [dunaj.compare :refer [nil? natural-comparator defsentinel]]
   [dunaj.flow :refer
    [cond let if when if-let when-not loop recur if-not]]
   [dunaj.feature :refer [assoc-meta meta]]
   [dunaj.poly :refer [Type identical-type? satisfies? deftype]]
   [dunaj.coll :refer
    [ISeq IRed IPersistentCollection IBatchedRed IUnpackedRed
     IPersistentMap IPersistentList Transducer indexed? nth
     seq? -reduce postponed transduce catenable? cat editable? count
     edit settle! reduce conj! conj invertible? invert item-type next
     reduced seq first sectionable? section reversible? reverse coll?
     homogeneous? empty? pop collection empty rest emptyable? assoc
     contains? vector? map? set? reducing]]
   [dunaj.function :refer [Function defn identity fn apply comp nop]]
   [dunaj.string :refer [string?]]
   [dunaj.identifier :refer [keyword keyword? name]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable default-fold-pool default-fold-size]]
   [dunaj.coll.helper :refer
    [reduce-batched* red->seq* reduce-unpacked* reduce* strip-reduced
     fold* fold-batched* fold-unpacked* adaptb]]
   [dunaj.host.batch :refer [item-types-match? batch]]
   [dunaj.host.array :refer
    [array-manager to-array array array-manager-from object-array]]
   [dunaj.macro :refer [defmacro macroexpand]]
   [dunaj.state.var :refer [def defalias replace-var!]]
   [dunaj.coll.bvt-vector :refer [bvt-vector-factory]]
   [dunaj.coll.tuple :refer [key val pair]]))


;;;; Implementation details

(defsentinel nothing)

(defn ^:private unpacked-fn [f] (fn [val xs] (apply f val xs)))

;; injected in dunaj.coll.recipe
(defn ^:private zip
  ([a b] (not-implemented))
  ([a b c] (not-implemented))
  ([a b c & colls] (not-implemented)))

(defn ^:private seq*
  [coll]
  (lazy-seq (red->seq* (-reduce coll #(postponed %2) nil))))

;;;; Public API

(defn sequence :- ISeq
  "Transforms `_coll_` to a possibly empty lazy seq, or returns
  `_coll_` if it already is a seq. Will not force lazy seq, nor
  will reduce any items from `_coll_`. Returns empty list if the
  `_coll_` is `nil`. May supply transducer `_xform_` or multiple
  colls, in which case a multireducible will be created."
  {:added v1
   :category "Primary"
   :see '[recipe dunaj.coll/transduce]}
  ([coll :- []]
   (cond (seq? coll) coll
         (class-instance? clojure.lang.Seqable coll)
         (.seq ^clojure.lang.Seqable coll)
         (nil? coll) clojure.lang.PersistentList/EMPTY
         :else (seq* coll)))
  ([xform :- Transducer, coll :- []]
   (lazy-seq (red->seq* (transduce xform #(postponed %2) nil coll))))
  ([xform :- Transducer, coll, :- [] & colls :- []]
   (sequence xform (apply zip coll colls))))

(defn into :- IPersistentCollection
  "Returns a new coll consisting of `_coll_` with all of the items of
  `_from_` conjoined. Prefer `<<dunaj.coll.api.ad#cat,cat>>` instead
  if both collections are of same type and are catenable."
  {:added v1
   :category "Primary"
   :see '[dunaj.coll/assoc dunaj.coll/conj merge]}
  ([coll :- IPersistentCollection, from :- []]
   (cond (and (identical-type? coll from) (catenable? coll))
         (cat coll from)
         (editable? coll)
         (assoc-meta (settle! (reduce conj! (edit coll) from))
                     (meta coll))
         :else (reduce conj coll from)))
  ([coll :- IPersistentCollection, xform :- Transducer, from :- []]
   (if (editable? coll)
     (assoc-meta (settle! (transduce xform conj! (edit coll) from))
                 (meta coll))
     (transduce xform conj coll from))))

(defalias group-by
  "Returns a map of the items of `_coll_` keyed by the result of `_f_`
  on each item. The value at each key will be a vector of the
  corresponding items, in the order they appeared in coll."
  {:added v1
   :category "Primary"
   :see '[frequencies]
   :tsig (Fn [{} AnyFn []])})

(defalias frequencies
  "Returns a map from distinct items in `_coll_` to the number
  of times they appear."
  {:added v1
   :tsig (Fn [{} []])
   :see '[group-by]
   :category "Primary"})

(defn merge :- {}
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first. If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  {:added v1
   :see '[merge-with dunaj.coll/conj]
   :category "Maps"}
  ([] nil)
  ([m :- {}] m)
  ([m :- {}, m1 :- {}] (if (nil? m) m1 (conj m m1)))
  ([m :- {}, m1 :- {}, m2 :- {}]
   (if (nil? m) (merge m1 m2) (conj m m1 m2)))
  ([m :- {}, m1 :- {}, m2 :- {} & maps :- {}]
   (let [maps (cons m (cons m1 (cons m2 maps)))]
     (when (clojure.core/some identity maps)
       (reduce #(conj (or %1 {}) %2) maps)))))

(defalias merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first. If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping
  in the result by calling `(_f_ val-in-result val-in-latter)`."
  {:added v1
   :category "Maps"
   :see '[merge dunaj.coll/conj]
   :tsig (Fn [{} (Fn [Any Any Any]) (Va {})])})

(defn select-keys :- {}
  "Returns a map containing only those entries in `_map_` whose
  key is in `_keys_`."
  {:added v1
   :see '[rename-keys]
   :category "Maps"}
  [map :- {}, keys :- []]
  (let [select-fn (fn [m k]
                    ;; TODO: adapt if IAssociative is changed
                    (if-let [entry (clojure.lang.RT/find map k)]
                      (conj m entry)
                      m))
        ret (reduce select-fn {} keys)]
    (assoc-meta ret (meta map))))

(defn map-invert :- {}
  "Returns the map `_m_` with the vals mapped to the keys."
  {:added v1
   :category "Maps"
   :see '[dunaj.coll/invert]}
  [m :- {}]
  (when-not (nil? m)
    (if (invertible? m) (invert m) (clojure.set/map-invert m))))

(defalias rename-keys
  "Returns the `_map_` with the keys in `_kmap_` renamed to the vals
  in `_kmap_`"
  {:added v1
   :see '[select-keys]
   :category "Maps"
   :tsig (Fn [{} {} {}])}
  clojure.set/rename-keys)

(defalias revlist
  "Returns a seq of the items in `_coll_` in reverse order. Not lazy.

  NOTE: Use more performant functions
  `<<dunaj.coll.api.ad#reverse,reverse>>` or
  `<<dunaj.coll.api.ad#flip,flip>>` if the input collection
  supports them."
  {:added v1
   :category "Primary"
   :tsig (Fn [IPersistentList []])
   :see '[dunaj.coll/reverse dunaj.coll/flip]}
  clojure.core/reverse)

(deftype Batched
  [requested-type :- (U nil Class Type),
   size-hint :- (Maybe Integer),
   coll :- IRed]
  IRed
  (-reduce [this reducef init]
    (if (and (satisfies? IBatchedRed coll)
             (item-types-match? requested-type (item-type coll)))
      (reduce-batched* requested-type size-hint coll reducef init)
      (reduce* (batch requested-type size-hint coll) reducef init))))

(defn batched :- IRed
  "Returns a collection that reduces on batched values."
  {:added v1
   :see '[not-batchable reduce-batched]
   :category "Reducers"}
  ([coll :- IRed]
   (batched nil nil coll))
  ([requested-type :- (U nil Class Type), coll :- IRed]
   (batched requested-type nil coll))
  ([requested-type :- (U nil Class Type),
    size-hint :- (Maybe Integer),
    coll :- IRed]
   (->Batched requested-type size-hint coll)))

(defn not-batchable :- IRed
  "Returns a collection that is not batchable."
  {:added v1
   :see '[batched reduce-batched]
   :category "Reducers"}
  [coll :- IRed]
  (adaptb coll coll))

(defn reduce-batched :- Any
  "Reduces `_coll_` by batches, creating temporary batches if
  `_coll_` is not batchable."
  {:added v1
   :see '[batched not-batchable dunaj.coll/reduce fold-batched]
   :category "Reducers"}
  ([reducef :- AnyFn, coll :- IRed]
   (reduce-batched nil nil reducef (reducef) coll))
  ([reducef :- AnyFn, init :- Any, coll :- IRed]
   (reduce-batched nil nil reducef init coll))
  ([requested-type :- (U nil Class Type),
    reducef :- AnyFn, init :- Any, coll :- IRed]
   (reduce-batched requested-type nil reducef init coll))
  ([requested-type :- (U nil Class Type),
    size-hint :- (Maybe Integer),
    reducef :- AnyFn, init :- Any, coll :- IRed]
   (strip-reduced
    (if (and (satisfies? IBatchedRed coll)
             (item-types-match? requested-type (item-type coll)))
      (reduce-batched* requested-type size-hint coll reducef init)
      (reduce* (batch requested-type size-hint coll) reducef init)))))

(defn fold-batched :- Any
  "Folds batched collection."
  {:added v1
   :see '[reduce-batched dunaj.concurrent.forkjoin/fold]
   :category "Reducers"}
  ([reducef :- AnyFn, coll :- IFoldable]
   (fold-batched* coll nil nil @default-fold-pool
                  @default-fold-size reducef reducef))
  ([combinef :- AnyFn, reducef :- AnyFn, coll :- IFoldable]
   (fold-batched* coll nil nil @default-fold-pool
                  @default-fold-size combinef reducef))
  ([requested-type :- (U nil Class Type),
    size-hint :- (Maybe Integer),
    combinef :- AnyFn, reducef :- AnyFn, coll :- IFoldable]
   (fold-batched* coll requested-type size-hint @default-fold-pool
                  @default-fold-size combinef reducef))
  ([requested-type :- (U nil Class Type),
    size-hint :- (Maybe Integer), n :- Integer,
    combinef :- AnyFn, reducef :- AnyFn, coll :- IFoldable]
   (fold-batched* coll requested-type size-hint @default-fold-pool
                  n combinef reducef)))

(deftype Unpacked
  [coll :- IRed]
  IRed
  (-reduce [this reducef init]
    (if (satisfies? IUnpackedRed coll)
      (reduce-unpacked* coll reducef init)
      (reduce* coll (unpacked-fn reducef) init))))

(defn unpacked :- IRed
  "Returns a collection that reduces on unpacked values. Works on any
  collection, not just on unpackable one."
  {:added v1
   :see '[reduce-unpacked fold-unpacked]
   :category "Reducers"}
  [coll :- IRed]
  (->Unpacked coll))

(defn reduce-unpacked :- Any
  "Reduces `_coll_` with unpacked fn `_reducef_`. Works on any
  collection, not just unpackable one."
  {:added v1
   :see '[unpacked fold-unpacked dunaj.coll/reduce
          dunaj.coll.helper/reduce-unpacked*]
   :category "Reducers"}
  ([reducef :- AnyFn, coll :- []]
   (reduce-unpacked reducef (reducef) coll))
  ([reducef :- AnyFn, init :- Any, coll :- []]
   (strip-reduced
    (if (satisfies? IUnpackedRed coll)
      (reduce-unpacked* coll reducef init)
      (reduce* coll (unpacked-fn reducef) init)))))

(defn fold-unpacked :- Any
  "Folds unpacked collection."
  {:added v1
   :see '[unpacked reduce-unpacked dunaj.concurrent.forkjoin/fold]
   :category "Reducers"}
  ([reducef :- AnyFn, coll :- IFoldable]
   (fold-unpacked* coll @default-fold-pool @default-fold-size
                   reducef reducef))
  ([combinef :- AnyFn, reducef :- AnyFn, coll :- IFoldable]
   (fold-unpacked* coll @default-fold-pool @default-fold-size
                   combinef reducef))
  ([n :- Integer,
    combinef :- AnyFn, reducef :- AnyFn, coll :- IFoldable]
   (fold-unpacked* coll @default-fold-pool n combinef reducef)))

(defn every? :- Boolean
  "Returns `true` if every item of `_coll_` satisfies `_pred_`,
  otherwise returns `false`."
  {:added v1
   :category "Primary"
   :see '[some not-any? not-every?]}
  [pred :- AnyFn, coll :- []]
  (reduce #(if (pred %2) % (reduced false)) true coll))

(defn some :- Any
  "Returns first logical true value of calling `_pred_` on items from
  `_coll_`, otherwise returns `nil`."
  {:added v1
   :category "Primary"
   :see '[every? not-any? not-every?]}
  [pred :- AnyFn, coll :- []]
  (reduce #(if-let [x (pred %2)] (reduced x) %) nil coll))

(def not-any?
  "Returns `true` if no item from `_coll_` satisfies `_pred_`,
  otherwise returns `false`."
  {:added v1
   :tsig (Fn [Boolean AnyFn []])
   :arglists '([pred coll])
   :category "Primary"
   :see '[some every? not-every?]}
  (comp not some))

(def not-every?
  "Returns `true` if at least one item from `_coll_` does not satisfy
  `_pred_`, otherwise returns `false`."
  {:added v1
   :tsig (Fn [Boolean AnyFn []])
   :arglists '([pred coll])
   :category "Primary"
   :see '[some not-any? every?]}
  (comp not every?))

(defn dorun :- nil
  "Realizes all or `_n_` items from `_coll_`, returning `nil`.
  Does not hold onto the head of the intermediate sequence."
  {:added v1
   :category "Iterations"
   :see '[doall doseq dored for]}
  ([coll :- []]
   (dorun nil coll))
  ([n :- (Maybe Integer), coll :- []]
   (if n
     (reduce #(if (izero? %) (reduced %2) (idec %)) (iint n) coll)
     (reduce nop coll))
   nil))

(defn doall :- (Maybe ISeq)
  "Realizes all or `_n_` items in the `_coll_` and returns seq to the
  realized sequence."
  {:added v1
   :category "Iterations"
   :see '[dorun doseq dored for]}
  ([coll :- []]
   (doall nil coll))
  ([n :- (Maybe Integer), coll :- []]
   (let [s (seq coll)]
     (if n
       (iloop [s s, n (iint n)]
         (when (and s (ipos? n)) (recur (next s) (idec n))))
       (loop [s s]
         (when s (recur (next s)))))
     s)))

(defalias for
  "List comprehension. Takes a vector of one or more
  binding-form/collection-expr pairs, each followed by zero or more
  modifiers, and yields a lazy sequence of evaluations of
  `_body-expr_`. Collections are iterated in a nested fashion,
  rightmost fastest, and nested coll-exprs can refer to bindings
  created in prior binding-forms.

  Supported modifiers are: `:let [binding-form expr ...]`,
  `:while test` and `:when test`."
  {:added v1
   :category "Iterations"
   :see '[doall doseq dored dorun]
   :tsig Macro})

(defmacro dored
  "For every value in `_coll_`, execute the transducer (optional),
  and bind the resulting value to the given binding.
  Prefer this macro over `doseq` when transducers are in use."
  {:added v1
   :category "Iterations"
   :highlight :flow
   :indent 1
   :see '[doall doseq for dorun]
   :let-bindings true}
  [[binding-name xform coll :as bs] & body]
  (if (== 3 (count bs))
    `(transduce
      ~xform (fn [ret# ~binding-name] ~@body nil) nil ~coll)
    `(reduce (fn [ret# ~binding-name] ~@body nil) nil ~xform)))

(defmacro doseq
  "Repeatedly executes `_body_` (presumably for side-effects) with
  bindings and filtering as provided by `for`. Does not retain
  the head of the sequence. Returns `nil`."
  {:added v1
   :category "Iterations"
   :highlight :flow
   :indent 1
   :see '[doall for dored dorun]
   :let-bindings true}
  [seq-exprs & body]
  (if (and (== 2 (count seq-exprs))
           (not (keyword? (first seq-exprs))))
    `(dored ~seq-exprs ~@body)
    `(clojure.core/doseq ~seq-exprs ~@body)))

(defn last :- Any
  "Returns the last item from `_coll_`, returning `nil` if `_coll_`
  is empty. Is linear time at worst."
  {:added v1
   :category "Primary"
   :see '[butlast dunaj.coll/first dunaj.coll/peek dunaj.coll/second
          dunaj.coll/nth dunaj.coll/count]}
  [coll :- []]
  (cond (empty? coll) nil
        (indexed? coll) (nth coll (dec (count coll)))
        (sectionable? coll)
        (first (section coll (dec (count coll)) (count coll)))
        (reversible? coll) (first (reverse coll))
        :else (reduce (fn [r v] v) nil coll)))

(defn shuffle :- []
  "Returns a random permutation of `_coll_`."
  {:added v1
   :category "Primary"
   :see '[sort sort-by]}
  [coll :- []]
  (when coll
    (let [coll (if (class-instance? java.util.Collection coll)
                 coll
                 (seq coll))
          al (java.util.ArrayList. ^java.util.Collection coll)]
      (java.util.Collections/shuffle al)
      (dunaj.host.array/adapt (.toArray al)))))

(defn sort :- []
  "Returns a sorted collection of the items in `_coll_`.
  If no comparator is supplied, uses natural comparator."
  {:added v1
   :category "Primary"
   :see '[sort-by shuffle]}
  ([coll :- []]
   (let [am (when (homogeneous? coll)
              (array-manager (item-type coll)))]
     (cond (empty? coll) []
           am
           (let [a (array (.itemType am) coll)]
             (.sort am a)
             (dunaj.host.array/adapt am a))
           :else
           (let [a (object-array coll)
                 am (array-manager-from a)]
             (java.util.Arrays/sort a natural-comparator)
             (dunaj.host.array/adapt am a)))))
  ([comp :- Function, coll :- []]
   (let [am (when (homogeneous? coll)
              (array-manager (item-type coll)))]
     (if (empty? coll)
       []
       (let [a (object-array coll)
             am (array-manager-from a)]
         (java.util.Arrays/sort a comp)
         (dunaj.host.array/adapt am a))))))

(defn sort-by :- []
  "Returns a sorted collection of the items in `_coll_`, where the
  sort order is determined by comparing (keyfn item).
  If no comparator is supplied, uses natural comparator."
  {:added v1
   :see '[sort shuffle]
   :category "Primary"}
  ([keyfn :- AnyFn, coll :- []]
   (sort-by keyfn natural-comparator coll))
  ([keyfn :- AnyFn, comp :- Function, coll :- []]
   (sort (fn [x y] (.compare ^java.util.Comparator
                             comp (keyfn x) (keyfn y))) coll)))

(defn butlast :- (Maybe ISeq)
  "Returns a non-lazy realized sequence of all but last item from
  `_coll_` in linear time."
  {:added v1
   :category "Primary"
   :see '[dunaj.coll.recipe/drop-last last]}
  [coll :- []]
  (seq (pop (collection bvt-vector-factory coll))))

(defn walk :- Any
  "Traverses `_form_`, an arbitrary data structure. Applies `_inner_`
  to each item of form, building up a data structure of the same
  type, then applies `_outer_` to the result. Uses `_branch-fn_` to
  determine whether walk should nest or not."
  {:added v1
   :see '[postwalk prewalk]
   :category "Walk"}
  ([inner :- AnyFn, outer :- AnyFn, form :- Any]
   (walk #(or (coll? %) (seq? %)) inner outer form))
  ([branch-fn :- Predicate,
    inner :- AnyFn, outer :- AnyFn, form :- Any]
   (loop [cur (inner form), result (), s nil, backlog ()]
     (cond
       ;; current level is done
       (nothing? cur)
       (if-let [ncur (first backlog)]
         (let [newresult (key ncur)
               newseq (val ncur)
               ;; reverse list-like collections
               result
               (if (or (vector? result) (map? result) (set? result))
                 result
                 (into (empty result) result))]
           (recur (if newseq (inner (first newseq)) nothing)
                  (conj newresult (outer result))
                  (next newseq) (rest backlog)))
         (first result))
       ;; nest into current item
       (and (emptyable? cur) (branch-fn cur))
       ;; seq* instead of seq as map entries cause troubles
       (let [newseq (seq (seq* cur)), prev (pair result s)]
         (if-not newseq
           ;; cannot nest, do as if not nestable
           (recur (if s (inner (first s)) nothing)
                  (conj result (outer cur)) (next s) backlog)
           ;; nest
           (recur (inner (first newseq)) (empty cur)
                  (next newseq) (conj backlog prev))))
       ;; push item into result and get another item
       :else (recur (if s (inner (first s)) nothing)
                    (conj result (outer cur)) (next s) backlog)))))

(defn postwalk :- Any
  "Performs a depth-first, post-order traversal of `_form_`.
  Calls `_f_` on each sub-form, uses f's return value in place
  of the original."
  {:added v1
   :see '[walk prewalk postwalk-replace]
   :category "Walk"}
  ([f :- AnyFn, form :- Any]
   (walk identity f form))
  ([branch-fn :- Predicate, f :- AnyFn, form :- Any]
   (walk branch-fn identity f form)))

(defn prewalk :- Any
  "Like postwalk, but does pre-order traversal."
  {:added v1
   :see '[walk postwalk prewalk-replace]
   :category "Walk"}
  ([f :- AnyFn, form :- Any]
   (walk f identity form))
  ([branch-fn :- Predicate, f :- AnyFn, form :- Any]
   (walk branch-fn f identity form)))

(defn prewalk-replace :- Any
  "Recursively transforms `_form_` by replacing keys in `_smap_`
  with their values.
  Like `<<dunaj.coll.recipe.api.ad#replace,replace>>` but works on
  any data structure. Does replacement at the root of the tree first."
  {:added v1
   :see '[postwalk-replace prewalk]
   :category "Walk"}
  ([smap :- {}, form :- Any]
   (prewalk #(if (contains? smap %) (smap %) %) form))
  ([branch-fn :- Predicate, smap :- {}, form :- Any]
   (prewalk branch-fn #(if (contains? smap %) (smap %) %) form)))

(defn postwalk-replace :- Any
  "Recursively transforms `_form_` by replacing keys in `_smap_`
  with their values.
  Like `<<dunaj.coll.recipe.api.ad#replace,replace>>` but works on
  any data structure.
  Does replacement at the leaves of the tree first."
  {:added v1
   :see '[prewalk-replace postwalk]
   :category "Walk"}
  ([smap :- {}, form :- Any]
   (postwalk #(if (contains? smap %) (smap %) %) form))
  ([branch-fn :- Predicate, smap :- {}, form :- Any]
   (postwalk branch-fn #(if (contains? smap %) (smap %) %) form)))

(defn stringify-keys :- Any
  "Recursively transforms all map keys from keywords to strings."
  {:added v1
   :see '[keywordize-keys]
   :category "Maps"}
  [m :- Any]
  (let [rf (fn [m k v] (assoc m (if (keyword? k) (name k) k) v))]
    (postwalk (fn [x] (if (map? x) (reduce-unpacked rf {} x) x))
              m)))

(defn keywordize-keys :- Any
  "Recursively transforms all map keys from strings to keywords."
  {:added v1
   :see '[stringify-keys]
   :category "Maps"}
  [m :- Any]
  (let [rf (fn [m k v] (assoc m (if (string? k) (keyword k) k) v))]
    (postwalk (fn [x] (if (map? x) (reduce-unpacked rf {} x) x))
              m)))

(defalias recipe
  {:added v1
   :category "Primary"
   :tsig (Fn [[] AnyFn []])
   :see '[sequence dunaj.coll/transduce]}
  dunaj.coll.helper/recipe)

(defn ^:private macroexpand-all :- Any
  "Recursively performs all possible macroexpansions in form."
  {:added v1}
  [form :- Any]
  (prewalk (fn [x] (if (seq? x) (macroexpand x) x)) form))

(replace-var! dunaj.macro/macroexpand-all)
