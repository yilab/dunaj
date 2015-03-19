;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
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

(ns dunaj.coll.default
  "Default implementations for basic collection types.

  This namespace assigns default implementations for commonly used
  collection types."
  {:categories ["Primary" "Empty collections" "Factories"]
   :authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Maybe Fn I U]]
   [dunaj.host :refer [Class]]
   [dunaj.math :refer [Integer]]
   [dunaj.poly :refer [Type defrecord]]
   [dunaj.coll :refer
    [ICollectionFactory ISorted IConvolutionFactory IRed
     IPersistentList IPersistentMap IPersistentVector IHomogeneous
     IPersistentSet -from-coll -from-items -convolute
     -from-interleaved assoc]]
   [dunaj.function :refer [Function defn]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.state.var :refer [replace-var! defalias def]]
   [dunaj.coll.empty-list :as el]
   [dunaj.coll.cons-seq :as cs :refer [cons]]
   [dunaj.coll.linked-list :as ll]
   [dunaj.coll.bvt-vector :as bv]
   [dunaj.coll.primitive-vector :as pv]
   [dunaj.coll.array-map :as am]
   [dunaj.coll.tuple :as t]
   [dunaj.coll.batched-queue :as bq]
   [dunaj.coll.hamt-set :as hs]
   [dunaj.coll.hamt-map :as hm]
   [dunaj.coll.rbt-sorted-set :as rss]
   [dunaj.coll.rbt-sorted-map :as rsm]
   [dunaj.coll.lazy-seq]
   [dunaj.coll.lazy-seq-map]
   [dunaj.coll.lazy-seq-set]
   [dunaj.coll.rrbt-vector]
   [dunaj.coll.vector-section]
   [dunaj.coll.util :refer [sequence]]))


;;;; Implementation details

(defrecord SeqFactory
  "Factory for seqs."
  []
  ICollectionFactory
  (-from-coll [factory coll] (sequence coll))
  (-from-items [factory] ())
  (-from-items [factory a] (cons a nil))
  (-from-items [factory a b] (cons a (cons b nil)))
  (-from-items [factory a b c] (cons a (cons b (cons c nil))))
  (-from-items [factory a b c d]
    (cons a (cons b (cons c (cons d nil)))))
  (-from-items [factory a b c d more]
    (cons a (cons b (cons c (cons d (sequence more)))))))


;;;; Public API

(def seq-factory :- ICollectionFactory
  "A seq factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of seq can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :category "Factories"
   :see '[lst-factory ->lst lst
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->SeqFactory))

(defalias lst-factory
  {:added v1
   :category "Factories"
   :see '[->lst lst que-factory
          dunaj.coll/collection
          dunaj.coll/->collection]}
  ll/linked-list-factory)

(defalias que-factory
  {:added v1
   :category "Factories"
   :see '[->que que lst-factory
          dunaj.coll/collection
          dunaj.coll/->collection]}
  bq/batched-queue-factory)

(defalias vec-factory
  {:added v1
   :category "Factories"
   :see '[->vec vec vec-factory-of
          dunaj.coll/collection
          dunaj.coll/->collection]}
  t/tuple-factory)

(defalias vec-factory-of
  {:added v1
   :category "Factories"
   :see '[->vec-of vec-of vec-factory
          dunaj.coll/collection
          dunaj.coll/->collection]}
  pv/primitive-vector-factory-of)

(defalias set-factory
  {:added v1
   :category "Factories"
   :see '[->set set sorted-set-factory
          dunaj.coll/collection
          dunaj.coll/->collection]}
  hs/hamt-set-factory)

(defalias sorted-set-factory
  {:added v1
   :category "Factories"
   :see '[->sorted-set sorted-set set-factory
          dunaj.coll/collection
          dunaj.coll/->collection]}
  rss/rbt-sorted-set-factory)

(defalias map-factory
  {:added v1
   :category "Factories"
   :see '[->map zipmap sorted-map-factory
          dunaj.coll/collection
          dunaj.coll/->collection
          dunaj.coll/convolution
          dunaj.coll/->convolution]}
  am/array-map-factory)

(defalias sorted-map-factory
  {:added v1
   :category "Factories"
   :see '[->sorted-map sorted-zipmap map-factory
          dunaj.coll/collection
          dunaj.coll/->collection
          dunaj.coll/convolution
          dunaj.coll/->convolution]}
  rsm/rbt-sorted-map-factory)

;;; Default empty collections

(defalias empty-seq
  {:added v1
   :category "Empty collections"
   :see '[empty-lst empty-que seq-factory
          dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/peek dunaj.coll/pop]}
  el/empty-list)

(defalias empty-lst
  {:added v1
   :category "Empty collections"
   :see '[empty-seq empty-que lst-factory
          dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/peek dunaj.coll/pop]}
  el/empty-list)

(defalias empty-que
  {:added v1
   :category "Empty collections"
   :see '[empty-lst empty-seq que-factory
          dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/peek dunaj.coll/pop]}
  bq/empty-batched-queue)

(defalias empty-vec
  {:added v1
   :category "Empty collections"
   :see '[empty-vec-of vec-factory
          dunaj.coll.util/into dunaj.coll/conj dunaj.coll/edit]}
  t/empty-tuple)

(defalias empty-vec-of
  {:added v1
   :category "Empty collections"
   :see '[empty-vec vec-factory-of
          dunaj.coll.util/into dunaj.coll/conj dunaj.coll/edit]}
  pv/empty-primitive-vector-of)

(defalias empty-set
  {:added v1
   :category "Empty collections"
   :see '[empty-sorted-set set-factory
          dunaj.coll.util/into dunaj.coll/conj dunaj.coll/disj]}
  hs/empty-hamt-set)

(defalias empty-sorted-set
  {:added v1
   :category "Empty collections"
   :see '[empty-set sorted-set-factory
          dunaj.coll.util/into dunaj.coll/conj dunaj.coll/disj]}
  rss/empty-rbt-sorted-set)

(defalias empty-map
  {:added v1
   :category "Empty collections"
   :see '[empty-sorted-map map-factory
          dunaj.coll.util/into dunaj.coll/assoc dunaj.coll/dissoc]}
  am/empty-array-map)

(defalias empty-sorted-map
  {:added v1
   :category "Empty collections"
   :see '[empty-map sorted-map-factory
          dunaj.coll.util/into dunaj.coll/assoc dunaj.coll/dissoc]}
  rsm/empty-rbt-sorted-map)

;;; Default constructors for abstract types

(defn lst :- IPersistentList
  "Returns a list with contents of `_coll_`."
  {:added v1
   :category "Primary"
   :see '[->lst empty-lst lst-factory]}
  [coll :- []]
  (-from-coll lst-factory coll))

(defn ->lst :- IPersistentList
  "Returns a list containing given items, if any."
  {:added v1
   :category "Primary"
   :see '[lst empty-lst lst-factory]}
  ([] (-from-items lst-factory))
  ([a :- Any] (-from-items lst-factory a))
  ([a :- Any, b :- Any] (-from-items lst-factory a b))
  ([a :- Any, b :- Any, c :- Any] (-from-items lst-factory a b c))
  ([a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items lst-factory a b c d))
  ([a :- Any, b :- Any, c :- Any, d :- Any & more :- Any]
   (-from-items lst-factory a b c d more)))

(defn que :- IPersistentList
  "Returns a queue with contents of `_coll_`."
  {:added v1
   :category "Primary"
   :see '[->que empty-que que-factory]}
  [coll :- []]
  (-from-coll que-factory coll))

(defn ->que :- IPersistentList
  "Returns a queue containing given items, if any."
  {:added v1
   :category "Primary"
   :see '[que empty-que que-factory]}
  ([] (-from-items que-factory))
  ([a :- Any] (-from-items que-factory a))
  ([a :- Any, b :- Any] (-from-items que-factory a b))
  ([a :- Any, b :- Any, c :- Any] (-from-items que-factory a b c))
  ([a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items que-factory a b c d))
  ([a :- Any, b :- Any, c :- Any, d :- Any & more :- Any]
   (-from-items que-factory a b c d more)))

(defn vec :- IPersistentVector
  "Returns a vector with contents of `_coll_`."
  {:added v1
   :category "Primary"
   :see '[->vec empty-vec vec-factory]}
  [coll :- []]
  (-from-coll vec-factory coll))

(defn ->vec :- IPersistentVector
  "Returns a vector containing given items, if any."
  {:added v1
   :category "Primary"
   :see '[vec empty-vec vec-factory]}
  ([] (-from-items vec-factory))
  ([a :- Any] (-from-items vec-factory a))
  ([a :- Any, b :- Any] (-from-items vec-factory a b))
  ([a :- Any, b :- Any, c :- Any] (-from-items vec-factory a b c))
  ([a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items vec-factory a b c d))
  ([a :- Any, b :- Any, c :- Any, d :- Any & more :- Any]
   (-from-items vec-factory a b c d more)))

(defn vec-of :- (I IHomogeneous IPersistentVector)
  "Returns a primitive vector of type `_t_` with contents `_coll_`."
  {:added v1
   :category "Primary"
   :see '[->vec-of empty-vec-of vec-factory-of]}
  [t :- (U nil Class Type Keyword), coll :- []]
  (-from-coll (vec-factory-of t) coll))

(defn ->vec-of :- (I IHomogeneous IPersistentVector)
  "Returns a primitive vector of type `_t_` containing given items,
  if any."
  {:added v1
   :category "Primary"
   :see '[vec-of empty-vec-of vec-factory-of]}
  ([t :- (U nil Class Type Keyword)]
   (-from-items (vec-factory-of t)))
  ([t :- (U nil Class Type Keyword), a :- Any]
   (-from-items (vec-factory-of t) a))
  ([t :- (U nil Class Type Keyword), a :- Any, b :- Any]
   (-from-items (vec-factory-of t) a b))
  ([t :- (U nil Class Type Keyword), a :- Any, b :- Any, c :- Any]
   (-from-items (vec-factory-of t) a b c))
  ([t :- (U nil Class Type Keyword), a :- Any, b :- Any, c :- Any,
    d :- Any]
   (-from-items (vec-factory-of t) a b c d))
  ([t :- (U nil Class Type Keyword), a :- Any, b :- Any, c :- Any,
    d :- Any & more :- Any]
   (-from-items (vec-factory-of t) a b c d more)))

(defn set :- IPersistentSet
  "Returns a set with contents of `_coll_`."
  {:added v1
   :category "Primary"
   :see '[->set empty-set set-factory]}
  [coll :- []]
  (-from-coll set-factory coll))

(defn ->set :- IPersistentSet
  "Returns a set containing given items, if any."
  {:added v1
   :category "Primary"
   :see '[set empty-set set-factory]}
  ([] (-from-items set-factory))
  ([a :- Any] (-from-items set-factory a))
  ([a :- Any, b :- Any] (-from-items set-factory a b))
  ([a :- Any, b :- Any, c :- Any] (-from-items set-factory a b c))
  ([a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items set-factory a b c d))
  ([a :- Any, b :- Any, c :- Any, d :- Any & more :- Any]
   (-from-items set-factory a b c d more)))

(defn sorted-set :- (I ISorted IPersistentSet)
  "Returns a sorted set with contents of `_coll_`,
  using default item ordering."
  {:added v1
   :category "Primary"
   :see '[->sorted-set empty-sorted-set sorted-set-factory]}
  [coll :- []]
  (-from-coll sorted-set-factory coll))

(defn ->sorted-set :- (I ISorted IPersistentSet)
  "Returns a sorted set containing given items, if any,
  using default item ordering."
  {:added v1
   :category "Primary"
   :see '[sorted-set empty-sorted-set sorted-set-factory]}
  ([] (-from-items sorted-set-factory))
  ([a :- Any] (-from-items sorted-set-factory a))
  ([a :- Any, b :- Any] (-from-items sorted-set-factory a b))
  ([a :- Any, b :- Any, c :- Any]
   (-from-items sorted-set-factory a b c))
  ([a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items sorted-set-factory a b c d))
  ([a :- Any, b :- Any, c :- Any, d :- Any & more :- Any]
   (-from-items sorted-set-factory a b c d more)))

(defn sorted-set-by :- (I ISorted IPersistentSet)
  "Returns a sorted set with contents of `_coll_` and a custom
  `_comparator_`."
  {:added v1
   :category "Primary"
   :see '[->sorted-set-by empty-sorted-set sorted-set-factory]}
  [comparator :- Function, coll :- []]
  (-from-coll (assoc sorted-set-factory :comparator comparator) coll))

(defn ->sorted-set-by :- (I ISorted IPersistentSet)
  "Returns a sorted set with custom `_comparator_` containing given
  items, if any."
  {:added v1
   :category "Primary"
   :see '[sorted-set-by empty-sorted-set sorted-set-factory]}
  ([comparator :- Function]
   (-from-items (assoc sorted-set-factory :comparator comparator)))
  ([comparator :- Function, a :- Any]
   (-from-items (assoc sorted-set-factory :comparator comparator) a))
  ([comparator :- Function, a :- Any, b :- Any]
   (-from-items
    (assoc sorted-set-factory :comparator comparator) a b))
  ([comparator :- Function, a :- Any, b :- Any,
    c :- Any]
   (-from-items
    (assoc sorted-set-factory :comparator comparator) a b c))
  ([comparator :- Function, a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items
    (assoc sorted-set-factory :comparator comparator) a b c d))
  ([comparator :- Function, a :- Any, b :- Any, c :- Any, d :- Any
    & more :- Any]
   (-from-items
    (assoc sorted-set-factory :comparator comparator) a b c d more)))

(defn zipmap :- IPersistentMap
  "Returns a map with convoluted contents of `_keys_` and `_vals_`."
  {:added v1
   :see '[->map]
   :category "Primary"}
  [keys :- [], vals :- []]
  (-convolute map-factory keys vals))

(defn ->map :- IPersistentMap
  "Returns a map containing given keyvals, if any."
  {:added v1
   :see '[zipmap]
   :category "Primary"}
  ([] (-from-interleaved map-factory))
  ([key :- Any, val :- Any] (-from-interleaved map-factory key val))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any]
   (-from-interleaved map-factory key1 val1 key2 val2))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any,]
   (-from-interleaved map-factory key1 val1 key2 val2 key3 val3))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any, key4 :- Any, val4 :- Any]
   (-from-interleaved map-factory key1 val1 key2 val2 key3 val3
                      key4 val4))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any, key4 :- Any, val4 :- Any & more :- Any]
   (-from-interleaved map-factory key1 val1 key2 val2 key3 val3
                      key4 val4 more)))

(defn sorted-zipmap :- (I ISorted IPersistentMap)
  "Returns a sorted map with convoluted contents of
  `_keys_` and `_vals_`, using default item ordering."
  {:added v1
   :see '[->sorted-map sorted-zipmap-by]
   :category "Primary"}
  [keys :- [], vals :- []]
  (-convolute sorted-map-factory keys vals))

(defn ->sorted-map :- (I ISorted IPersistentMap)
  "Returns a sorted map containing given keyvals, if any,
  using default item ordering."
  {:added v1
   :see '[sorted-zipmap ->sorted-map-by]
   :category "Primary"}
  ([] (-from-interleaved sorted-map-factory))
  ([key :- Any, val :- Any]
   (-from-interleaved sorted-map-factory key val))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any]
   (-from-interleaved sorted-map-factory key1 val1 key2 val2))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any,]
   (-from-interleaved sorted-map-factory key1 val1 key2 val2
                      key3 val3))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any, key4 :- Any, val4 :- Any,]
   (-from-interleaved sorted-map-factory key1 val1 key2 val2
                      key3 val3 key4 val4))
  ([key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any, key4 :- Any, val4 :- Any & more :- Any]
   (-from-interleaved sorted-map-factory key1 val1 key2 val2
                      key3 val3 key4 val4 more)))

(defn sorted-zipmap-by :- (I ISorted IPersistentMap)
  "Returns a sorted map with convoluted contents of
  `_keys_` and `_vals_`, sorted with `_comparator_`."
  {:added v1
   :see '[->sorted-map-by sorted-zipmap]
   :category "Primary"}
  [comparator :- Function, keys :- [], vals :- []]
  (-convolute (assoc sorted-map-factory :comparator comparator)
              keys vals))

(defn ->sorted-map-by :- (I ISorted IPersistentMap)
  "Returns the sorted map containing given keyvals, if any,
  sorted with `_comparator_`."
  {:added v1
   :see '[sorted-zipmap-by ->sorted-map]
   :category "Primary"}
  ([comparator :- Function]
   (-from-interleaved
    (assoc sorted-map-factory :comparator comparator)))
  ([comparator :- Function, key :- Any, val :- Any]
   (-from-interleaved
    (assoc sorted-map-factory :comparator comparator) key val))
  ([comparator :- Function, key1 :- Any, val1 :- Any,
    key2 :- Any, val2 :- Any]
   (-from-interleaved
    (assoc sorted-map-factory :comparator comparator)
    key1 val1 key2 val2))
  ([comparator :- Function, key1 :- Any, val1 :- Any,
    key2 :- Any, val2 :- Any, key3 :- Any, val3 :- Any,]
   (-from-interleaved
    (assoc sorted-map-factory :comparator comparator)
    key1 val1 key2 val2 key3 val3))
  ([comparator :- Function,
    key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any, key4 :- Any, val4 :- Any,]
   (-from-interleaved
    (assoc sorted-map-factory :comparator comparator)
    key1 val1 key2 val2 key3 val3 key4 val4))
  ([comparator :- Function,
    key1 :- Any, val1 :- Any, key2 :- Any, val2 :- Any,
    key3 :- Any, val3 :- Any, key4 :- Any, val4 :- Any & more :- Any]
   (-from-interleaved
    (assoc sorted-map-factory :comparator comparator)
    key1 val1 key2 val2 key3 val3 key4 val4 more)))

(replace-var! clojure.core/empty dunaj.coll/empty)
