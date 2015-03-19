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

(ns dunaj.compare
  "Comparison, hashing and equality.

  Identity based comparison is represented by `identical?`, while
  `=` uses equivalence based on a value-based comparison.

  .Design notes
  * Any object can be compared for equality with `identical?` or
    with `=`, regardless of whether it implements custom equivalence
    through `IEquiv`.
  * Every object has a hash, regardless of whether it implements
    custom hash through `IHash`.
  * Dunaj does not provide protocol predicate for custom equivalence,
    nor for a custom hash."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Comparison" "Hashing"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [if when-not do let]]
   [clojure.bootstrap :refer
    [defprotocol defalias defn defonce v1 def defrecord fn defmacro]]
   [dunaj.type :refer [Predicate Fn Any Va Unknown R]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.host.int :refer [Int iint iadd imul i31 i0 i1]]
   [dunaj.math :refer [Integer]]))


;;;; Public API

(defalias identical?
  {:doc "Returns `true` if both arguments are the same object
        (have the same identity), otherwise returns `false`."
   :added v1
   :tsig (Fn [Boolean Any Any])
   :category "Primary"
   :see '[= distinct? dunaj.math/== dunaj.poly/identical-type?]})

(defn sentinel :- Unknown
  "Returns a unique object, of an unspecified type, for each call.
  The returned object is not equal (not `=` nor `identical?`) to
  any object other than itself. Is safe to put into channels or
  into functions that don't accept `nil`."
  {:added v1
   :category "Primary"
   :see '[identical? = nil-sentinel encode-nil decode-nil
          defsentinel]}
  []
  (java.lang.Object.))

(defmacro defsentinel
  "Defines a private sentinel object named `_sname_` and
  a corresponding private predicate function."
  {:added v1
   :category "Primary"
   :see '[identical? sentinel nil-sentinel]}
  [sname]
  (let [pname (clojure.core/symbol
               (clojure.core/str (clojure.core/name sname) \?))]
    `(do (def ~sname {:private true} (sentinel))
         (defn ~pname :- Boolean {:private true} [x#]
           (identical? ~sname x#)))))

;;; nil

(defalias nil?
  {:doc "Returns `true` if `_x_` is `nil`, otherwise returns `false`."
   :added v1
   :tsig Predicate
   :see '[some? nil-sentinel encode-nil decode-nil
          dunaj.boolean/false? dunaj.flow/if]
   :category "Primary"})

(defalias some?
  {:doc "Returns `true` if `_x_` is not `nil`,
        otherwise returns `false`."
   :added v1
   :tsig Predicate
   :see '[nil? nil-sentinel encode-nil decode-nil
          dunaj.boolean/true? dunaj.flow/if]
   :category "Primary"})

(defonce nil-sentinel :- Unknown
  "A sentinel value used for `nil` encoding/decoding."
  {:added v1
   :see '[nil? some? sentinel encode-nil decode-nil defsentinel]
   :category "Primary"}
  ;; TODO: make sure it IS NOT serializable
  ;;       and IS nicely human printable
  (sentinel))

(defn encode-nil :- Any
  "Returns `nil-sentinel` if `_x_` is `nil`, otherwise returns `_x_`."
  {:added v1
   :see '[nil? some? sentinel nil-sentinel decode-nil]
   :category "Primary"}
  [x :- Any]
  (if (nil? x) nil-sentinel x))

(defn decode-nil :- Any
  "Returns `nil` if `_x_` is the `nil-sentinel`,
  otherwise returns `_x_`."
  {:added v1
   :see '[nil? some? sentinel encode-nil nil-sentinel]
   :category "Primary"}
  [x :- Any]
  (when-not (identical? nil-sentinel x) x))

;;; Comparator

(defprotocol IComparable
  "A value protocol for natural total ordering."
  {:added v1
   :predicate 'comparable?
   :category "Comparison"
   :on-interface java.lang.Comparable
   :see '[compare natural-comparator]
   :forbid-extensions true}
  (-compare-to :- Integer
    "Returns a negative integer, zero, or a positive integer if
    `_this_` is less than, equal to, or greater than the `_other_`.
    When implementing `-compare-to`, mind not to mix ordered and
    unordered collections."
    {:on 'compareTo}
    [this other :- Any]))

(defalias compare
  {:doc "Returns a negative integer, zero, or a positive integer if
        `_this_` is less than, equal to, or greater than the
        `_other_`."
   :added v1
   :see '[IComparable comparable? natural-comparator]
   :tsig (Fn [Integer IComparable Any])
   :category "Comparison"})

(def natural-comparator :- (Fn [Integer Any Any])
  "A comparator which uses object's natural ordering
  (implemented with `IComparable`) for comparison."
  {:added v1
   :see '[compare IComparable comparable?]
   :category "Comparison"}
  clojure.lang.RT/DEFAULT_COMPARATOR)

;;; Hash

(defprotocol IHash
  "A value protocol for a custom hash. Types that don't extend this
  protocol get their hash value computed from their identity.

  NOTE: JVM host specific: For compatibility with host,
  implementations should also implement
  `https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#hashCode--[j.l.Object/hashCode]`."
  {:added v1
   :on-interface clojure.lang.IHashEq
   :see '[hash =]
   :category "Hashing"
   :forbid-extensions true}
  (-hash :- Int
    "Returns a hash value for `_this_`.

    IMPORTANT: Two objects which are equivalent with `-equiv` must
    return same value for `-hash`."
    {:on 'hasheq}
    [this]))

(defalias hash
  {:doc "Returns a hash code of `_x_` that is consistent with `=`."
   :added v1
   :see '[IHash = hash-from-basis]
   :tsig (Fn [Int Any])
   :category "Hashing"})

;;; Incremental hash

(defprotocol IIncrementalHashFactory
  "A factory protocol for incremental hashes."
  {:added v1
   :see '[basis-seed next-basis hash-basis hash-from-basis IHashBasis]
   :category "Hashing"}
  (-basis-seed :- Int
    "Returns the hash basis seed.
    Throws if `_this_` factory does not provide a seed."
    [this])
  (-next-basis :- Int
    "Returns a new hash basis based on `_old_` basis and `_val_`."
    [this old :- Int, val :- Any]))

(defn basis-seed :- Int
  "Returns the hash basis seed of a given `_factory_`."
  {:added v1
   :see '[next-basis hash-basis hash-from-basis
          IIncrementalHashFactory IHashBasis]
   :category "Hashing"}
  [factory :- IIncrementalHashFactory]
  (-basis-seed factory))

(defn next-basis :- Int
  "Returns a new hash basis computed with a given `_factory_`,
  based on `_old_` basis and `_val_`."
  {:added v1
   :see '[basis-seed hash-basis hash-from-basis]
   :category "Hashing"}
  [factory :- IIncrementalHashFactory, old :- Int, val :- Any]
  (-next-basis factory old val))

(defrecord OrderedHashFactory
  "An incremental hash factory type for ordered collections."
  []
  IIncrementalHashFactory
  (-basis-seed [factory]
    (i1))
  (-next-basis [factory old val]
    (iadd (imul (i31) (iint old)) (hash val))))

(defrecord UnorderedHashFactory
  "An incremental hash factory type for unordered collections."
  []
  IIncrementalHashFactory
  (-basis-seed [factory]
    (i0))
  (-next-basis [factory old val]
    (iadd (iint old) (hash val))))

(def ordered-hash-factory :- (R IIncrementalHashFactory)
  "An incremental hash factory for ordered collections."
  {:added v1
   :see '[unordered-hash-factory basis-seed next-basis
          IIncrementalHashFactory]
   :category "Hashing"}
  (->OrderedHashFactory))

(def unordered-hash-factory :- (R IIncrementalHashFactory)
  "An incremental hash factory for unordered collections."
  {:added v1
   :see '[ordered-hash-factory basis-seed next-basis
          IIncrementalHashFactory]
   :category "Hashing"}
  (->UnorderedHashFactory))

(defprotocol IHashBasis
  "A value protocol for collections with incremental hash basis."
  {:added v1
   :see '[hash-basis hash-from-basis ordered-hash-factory
          unordered-hash-factory]
   :category "Hashing"
   :predicate 'hash-basis?}
  (-hash-basis :- Int
    "Returns the hash basis for `_this_`."
    [this]))

(defn hash-basis :- Int
  "Returns the hash basis of `_x_`."
  {:added v1
   :see '[hash-from-basis hash-basis? ordered-hash-factory
          unordered-hash-factory hash IHashBasis]
   :category "Hashing"}
  [x :- IHashBasis]
  (-hash-basis x))

(defn hash-from-basis :- Int
  "Returns the hash value from a given `_hash-basis_` and `_count_`."
  {:added v1
   :see '[hash hash-basis ordered-hash-factory unordered-hash-factory]
   :category "Hashing"
   :inline (fn [x y] `(clojure.core/mix-collection-hash ~x ~y))}
  [hash-basis :- Int, count :- Int]
  (clojure.core/mix-collection-hash hash-basis count))

;;; Equivalence

(defprotocol IEquiv
  "A value protocol for custom equivalence. Types which do not extend
  this protocol will use their identity for comparison.

  NOTE: JVM host specific: For compatibility reasons, implementations
  should also implement
  `https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#equals-java.lang.Object-[j.l.Object/equals]`."
  {:added v1
   :see '[= not= identical? hash dunaj.math/==]
   :category "Primary"
   :on-interface clojure.lang.IPersistentCollection
   :forbid-extensions true}
  (-equiv :- Boolean
    "Returns `true` if `_this_` is equal to `_other_`, otherwise
    returns `false`. Note that two objects that are equivalent with
    `-equiv` must also return same value for `-hash`.

    When implementing `-equiv`, mind not to mix ordered and
    unordered collections. Sequential collections are usually not
    equal to non-sequential ones, but may be equal to other
    sequential collections even if are not of a same type."
    {:on 'equiv}
    [this other :- Any]))

(defalias =
  {:doc "Returns `true` if `_x_` equals `_y_`, otherwise returns
        `false`. Contrary to the `identical?`, `=` is an equivalence
        comparison based on values, not identities."
   :added v1
   :see '[not= identical? dunaj.math/== hash IEquiv]
   :category "Primary"
   :tsig (Fn [true Any]
             [Boolean Any Any]
             [Boolean Any Any (Va Any)])})

(defalias not=
  {:doc "Returns `true` if `_x_` does not equal `_y_`, otherwise
        returns `false`."
   :added v1
   :see '[= identical? hash dunaj.boolean/not dunaj.math/== IEquiv]
   :category "Primary"
   :tsig (Fn [false Any]
             [Boolean Any Any]
             [Boolean Any Any (Va Any)])})

(defalias distinct?
  {:doc "Returns `true` if no two of given arguments are `=`."
   :added v1
   :see '[= not= dunaj.coll.recipe/distinct dunaj.coll.recipe/dedupe]
   :category "Primary"
   :tsig (Fn [true Any]
             [Boolean Any Any]
             [Boolean Any Any (Va Any)])})


;;;; Testing

(clojure.core/require
 '[clojure.bootstrap :refer [assert-primitive assert-boolean]])

(assert-boolean
 (nil? nil)
 (nil? 'd)
 (identical? 'f 'f)
 (identical? 3 5)
 ;; (identical? 3 'd)
 (comparable? :x)
 (= 1 1)
 (= :x 'x)
 (not= :x 'x)
 (distinct? 1 2))

(assert-primitive
 (hash 3)
 (hash [:foo])
 (compare 1 2)
 (compare :x :y))
