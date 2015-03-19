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

(ns dunaj.set
  "Universal and complement sets, set operations.

  Universal sets are useful as identity elements for reductions
  and folding, for example:

  [source,clojure]
  --
  (reduce intersection [#{0 1 2} #{1 2 3} #{2 3 4}])
  ;;=> #{2}
  --

  NOTE: Prefer aliasing this namespace (as shown in the usage
  example) to refering its functions."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Operations"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [defmethod get-method cons some print-method]]
   [clojure.set :as cs]
   [clojure.bootstrap :refer [v1]]
   [clojure.pprint]
   [dunaj.type :refer [Any Fn I Maybe AnyFn Va KeywordMap Predicate]]
   [dunaj.boolean :refer [Boolean not or and]]
   [dunaj.host :refer [class-instance?]]
   [dunaj.host.int :refer [Int iadd iint]]
   [dunaj.compare :refer [IHash IEquiv identical? hash -hash =]]
   [dunaj.flow :refer [if let cond when-not if-let when]]
   [dunaj.feature :refer [IPersistentMeta IMeta meta assoc-meta]]
   [dunaj.poly :refer [instance? identical-type? satisfies? deftype]]
   [dunaj.coll :refer
    [IRed IEmptyAware IEmptyable empty ILookup contains? empty? conj
     IInvertible ISeqable reduce IPersistentSet disj get
     IPersistentCollection ICatenable cat]]
   [dunaj.function :refer [IInvocable apply defn]]
   [dunaj.string :refer [->str]]
   [dunaj.error :refer
    [throw unsupported-operation illegal-argument]]
   [dunaj.state.var :refer [declare def]]))


;;;; Public API

(declare UniversalSet ccat cequiv cequals U)

(deftype ComplementSet
  "A type for absolute complement set."
  {:added v1
   :see '[set-complement U finite? dunaj.coll/invert]
   :category "Primary"
   :predicate 'complement?}
  [excluded :- #{}, meta :- KeywordMap]
  IHash
  (-hash [this] (iadd (iint 42) (hash excluded)))
  IEquiv
  (-equiv [this other] (cequiv this other))
  IMeta
  (-meta [this] meta)
  IPersistentMeta
  (-assoc-meta [this m] (->ComplementSet excluded m))
  IRed
  (-reduce [this reducef init]
    (throw (unsupported-operation
            "reduce is not supported on complement set")))
  ISeqable
  (-seq [this]
    (throw (unsupported-operation
            "seq is not supported on complement set")))
  IEmptyAware
  (-empty? [this] false)
  IEmptyable
  (-empty [this] (assoc-meta #{} meta))
  ILookup
  (-contains? [this key] (not (contains? excluded key)))
  (-get [this key not-found] (if (contains? this key) key not-found))
  IInvertible
  (-invert [this] (assoc-meta excluded meta))
  IPersistentCollection
  (-conj [this x]
    (let [e (disj excluded x)]
      (if (empty? e) (assoc-meta U meta) (->ComplementSet e meta))))
  ICatenable
  (-cat [this other] (ccat this other))
  IInvocable
  (-invoke [this key]
    (if (contains? this key) key nil))
  (-invoke [this key not-found]
    (if (contains? this key) key not-found))

  ;; Abstract types
  IPersistentSet
  (-disj [this key] (->ComplementSet (conj excluded key) meta))

  ;; Clojure compatibility
  clojure.lang.ILookup
  (valAt [this key] (if (contains? this key) key nil))
  (valAt [this key not-found] (if (contains? this key) key not-found))
  clojure.lang.IPersistentSet
  (get [this key] (get this key nil))
  (contains [this key] (contains? this key))

  ;; JVM compatibility
  java.lang.Object
  (hashCode [this] (-hash this))
  (equals [this other] (cequals this other))
  (toString [this] (->str excluded "ᶜ")))

(deftype UniversalSet
  "A type for universal set."
  {:predicate 'universal?
   :see '[U set-complement finite? dunaj.coll/invert]
   :category "Primary"
   :added v1}
  [meta :- KeywordMap]
  IHash
  (-hash [this] 42)
  IEquiv
  (-equiv [this other] (if (identical-type? this other) true false))
  IMeta
  (-meta [this] meta)
  IPersistentMeta
  (-assoc-meta [this m] (->UniversalSet m))
  IRed
  (-reduce [this reducef init]
    (throw (unsupported-operation
            "reduce is not supported on universal set")))
  ISeqable
  (-seq [this]
    (throw (unsupported-operation
            "seq is not supported on universal set")))
  IEmptyAware
  (-empty? [this] false)
  IEmptyable
  (-empty [this] (assoc-meta #{} meta))
  ILookup
  (-contains? [this key] true)
  (-get [this key not-found] key)
  IInvertible
  (-invert [this] (empty this))
  IPersistentCollection
  (-conj [this x] this)
  ICatenable
  (-cat [this other]
    (when-not (instance? clojure.lang.IPersistentSet other)
      (throw (illegal-argument "Not of similar type.")))
    this)
  IInvocable
  (-invoke [this key] key)
  (-invoke [this key not-found] key)

  ;; Abstract types
  IPersistentSet
  (-disj [this key] (->ComplementSet #{key} meta))

  ;; Clojure compatibility
  clojure.lang.ILookup
  (valAt [this key] key)
  (valAt [this key not-found] key)
  clojure.lang.IPersistentSet
  (get [this key] key)
  (contains [this key] true)

  ;; JVM compatibility
  java.lang.Object
  (hashCode [this] 42)
  (equals [this other] (if (identical-type? this other) true false))
  (toString [this] "𝕌"))

(defn ^:private ccat :- #{}
  [x :- ComplementSet, other :- Any]
  (cond
   (class-instance? dunaj.set.ComplementSet other)
   (let [r (cs/intersection
            (.-excluded x)
            (.-excluded ^dunaj.set.ComplementSet other))]
     (if (empty? r)
       (assoc-meta U (meta x))
       (->ComplementSet r (meta x))))
   (class-instance? dunaj.set.UniversalSet other)
   (assoc-meta U (meta x))
   (satisfies? IPersistentSet other)
   (let [r (cs/difference (.-excluded x) other)]
     (if (empty? r)
       (assoc-meta U (meta x))
       (->ComplementSet r (meta x))))
   :else (throw (illegal-argument "Not of similar type."))))

(defn ^:private cequiv :- Boolean
  [x :- ComplementSet, other :- Any]
  (if (identical-type? x other)
    (= (.-excluded x)
       (.-excluded ^dunaj.set.ComplementSet other))
    false))

(defn ^:private cequals :- Boolean
  [x :- ComplementSet, other :- Any]
  (if (identical-type? x other)
    (clojure.lang.Util/equals
     (.-excluded x)
     (.-excluded ^dunaj.set.ComplementSet other))
    false))

(defmethod print-method dunaj.set.ComplementSet
  [x ^java.io.Writer w]
  (.write w (.toString ^java.lang.Object x)))

(defmethod print-method dunaj.set.UniversalSet
  [x ^java.io.Writer w]
  (.write w (.toString ^java.lang.Object x)))

(defmethod clojure.pprint/simple-dispatch dunaj.set.UniversalSet
  [x]
  ((get-method clojure.pprint/simple-dispatch
               clojure.lang.IPersistentVector) x))

(def U :- UniversalSet
  "Universal set 𝕌."
  {:added v1
   :see '[set-complement finite?]
   :category "Primary"}
  (->UniversalSet nil))

(defn set-complement :- #{}
  "Returns `_setᶜ_`, a complement of `_set_`."
  {:added v1
   :see '[U finite?]
   :category "Operations"}
  [set :- #{}]
  (cond (empty? set) U
        (universal? set) #{}
        (complement? set) (.-excluded ^dunaj.set.ComplementSet set)
        :else (->ComplementSet set nil)))

(defn finite? :- Boolean
  "Returns `true` is `_set_` is finite, `false` otherwise."
  {:added v1
   :see '[set-complement U]
   :category "Primary"}
  [set :- #{}]
  (not (or (instance? ComplementSet set)
           (instance? UniversalSet set))))

(defn union :- #{}
  "Return a set that is the union of the input sets.
  `(union)` returns `#{}`."
  {:added v1
   :see '[intersection difference]
   :category "Operations"}
  ([] #{})
  ([s1 :- #{}] s1)
  ([s1 :- #{}, s2 :- #{}]
     (cond (or (universal? s1) (universal? s2)) U
           (complement? s1) (ccat s1 s2)
           (complement? s2) (ccat s2 s1)
           :else (cs/union s1 s2)))
  ([s1 :- #{}, s2 :- #{} & sets :- #{}]
     (let [s (cons s1 (cons s2 sets))]
       (if-let [what (some #(cond (universal? %) :universal
                                  (complement? %) %
                                  :else false) s)]
         (if (identical? what :universal) U (reduce cat what s))
         (apply cs/union s)))))

(defn intersection :- #{}
  "Return a set that is the intersection of the input sets.
  `(intersection)` returns `𝕌`."
  {:added v1
   :see '[union difference]
   :category "Operations"}
  ([] U)
  ([s1 :- #{}] s1)
  ([s1 :- #{}, s2 :- #{}]
     (cond (universal? s1) s2
           (universal? s2) s1
           (and (complement? s1) (complement? s2))
           (->ComplementSet
            (cs/union (.-excluded ^dunaj.set.ComplementSet s1)
                      (.-excluded ^dunaj.set.ComplementSet s2))
            (meta s1))
           (complement? s1)
           (cs/difference s2 (.-excluded ^dunaj.set.ComplementSet s1))
           (complement? s2)
           (cs/difference s1 (.-excluded ^dunaj.set.ComplementSet s2))
           :else (cs/intersection s1 s2)))
  ([s1 :- #{}, s2 :- #{} & sets :- #{}]
     (let [s (cons s1 (cons s2 sets))]
       (if (some #(not (finite? %)) s)
         (reduce intersection (intersection s1 s2) sets)
         (apply cs/intersection s)))))

(defn select :- #{}
  "Returns a set of the items from `_xset_` for which `_pred_`
  is logically true."
  {:added v1
   :see '[project]
   :category "Operations"}
  [pred :- Predicate, xset :- #{}]
  (when-not (finite? xset)
    (throw (unsupported-operation
            "select is not supported on infinite sets")))
  (cs/select pred xset))

(defn project :- #{}
  "Returns a rel of the items of `_xrel_` with only the keys in
  `_ks_`."
  {:added v1
   :see '[select rename]
   :category "Operations"}
  [xrel :- #{}, ks :- [Any]]
  (when-not (finite? xrel)
    (throw (unsupported-operation
            "project is not supported on infinite sets")))
  (cs/project xrel ks))

(defn rename :- #{}
  "Returns a rel of the maps in `_xrel_` with the keys in `_kmap`_
  renamed to the vals in `_kmap_`."
  {:added v1
   :category "Operations"}
  [xrel :- #{}, kmap :- {}]
  (when-not (finite? xrel)
    (throw (unsupported-operation
            "rename is not supported on infinite sets")))
  (cs/rename xrel kmap))

(defn index :- {}
  "Returns a map of the distinct values of `_ks_` in the `_xrel_`
  mapped to a set of the maps in `_xrel_` with the corresponding
  values of `_ks_`."
  {:added v1
   :category "Operations"}
  [xrel :- #{}, ks :- [Any]]
  (when-not (finite? xrel)
    (throw (unsupported-operation
            "index is not supported on infinite sets")))
  (cs/index xrel ks))

(defn join :- #{}
  "When passed 2 rels, returns the rel corresponding to the natural
  join. When passed an additional keymap `_km_`,
  joins on the corresponding keys."
  {:added v1
   :category "Operations"}
  ([xrel :- #{}, yrel :- #{}] ;natural join
     (when-not (finite? xrel)
       (throw (unsupported-operation
               "join is not supported on infinite sets")))
     (cs/join xrel yrel))
  ([xrel :- #{}, yrel :- #{}, km :- {}] ;arbitrary key mapping
     (when-not (finite? xrel)
       (throw (unsupported-operation
               "join is not supported on infinite sets")))
     (cs/join xrel yrel km)))

(defn difference :- #{}
  "Returns a set that is the first set without items of the
  remaining sets."
  {:added v1
   :see '[union intersection]
   :category "Operations"}
  ([s1 :- #{}] s1)
  ([s1 :- #{}, s2 :- #{}]
     (cond (universal? s2) #{}
           (universal? s1) (set-complement s2)
           (and (complement? s1) (complement? s2))
           (cs/difference (.-excluded ^dunaj.set.ComplementSet s2)
                          (.-excluded ^dunaj.set.ComplementSet s1))
           (complement? s2)
           (intersection s1 (.-excluded ^dunaj.set.ComplementSet s2))
           (complement? s1)
           (set-complement
            (union (.-excluded ^dunaj.set.ComplementSet s1) s2))
           :else (cs/difference s1 s2)))
  ([s1 :- #{}, s2 :- #{} & sets :- #{}]
     (reduce difference (difference s1 s2) sets)))

(defn subset? :- Boolean
  "Returns `true` if `_s1_` is a subset of `_s2_`, otherwise
  returns `false`."
  {:added v1
   :see '[superset? dunaj.coll/contains?]
   :category "Operations"}
  [s1 :- #{}, s2 :- #{}]
  (cond (universal? s2) true
        (universal? s1) false
        (and (complement? s1) (complement? s2))
        (subset? (.-excluded ^dunaj.set.ComplementSet s2)
                 (.-excluded ^dunaj.set.ComplementSet s1))
        (complement? s2)
        (empty?
         (intersection s1 (.-excluded ^dunaj.set.ComplementSet s2)))
        (complement? s1) false
        :else (cs/subset? s1 s2)))

(defn superset? :- Boolean
  "Returns `true` if `_s1_` is a superset of `_s2_`, otherwise
  returns `false`."
  {:added v1
   :see '[subset? dunaj.coll/contains?]
   :category "Operations"}
  [s1 :- #{}, s2 :- #{}]
  (subset? s2 s1))
