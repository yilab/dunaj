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

(ns dunaj.math.unchecked
  "Unchecked math where operations are fast but prone to overflow.

  Depending on the needs, there are several other namespaces with
  similar functionalities:

  * `<<dunaj.math.api.ad#,dunaj.math>>` which is slightly faster
    but subject to rounding and truncation
  * `<<dunaj.math.precise.api.ad#,dunaj.math.precise>>`,
    slower but with arbitrary precision, auto-promoting when needed
  * `<<dunaj.host.int.api.ad#,dunaj.host.int>>`, host specific
    unchecked operations with fastest performance."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require [clojure.core :refer [reduce . let]]
            [clojure.bootstrap :refer [defalias defn v1]]
            [dunaj.type :refer [Fn Va]]
            [dunaj.math :refer [Number]]))


;;;; Public API

(defalias add
  {:doc "Returns the unchecked sum of `_x_` and `_y_`.
        Subject to overflow."
   :added v1
   :see '[+ dunaj.math/add dunaj.math.precise/add
          dunaj.host.int/iadd]
   :tsig (Fn [Number Number Number])}
  clojure.core/unchecked-add)

(defn +
  "Returns the unchecked sum of nums. `(+)` returns 0.
  Subject to overflow."
  ;; TODO: support inlining
  {:added v1
   :see '[add inc dunaj.math/+ dunaj.math.precise/+
          dunaj.host.int/iadd]
   :tsig (Fn [0]
             [java.lang.Long java.lang.Long]
             [java.lang.Long java.lang.Long java.lang.Long]
             [java.lang.Long java.lang.Long java.lang.Long
              (Va java.lang.Long)])}
  ([] 0)
  ([x] x)
  ([x y] (. clojure.lang.Numbers (unchecked_add x y)))
  ([x y & more] (reduce + (+ x y) more)))

(defalias negate
  {:doc "Returns the unchecked negation of `_x_`.
        Subject to overflow."
   :added v1
   :see '[subtract - dunaj.math/negate dunaj.math.precise/negate
          dunaj.host.int/ineg]
   :tsig (Fn [Number Number])}
  clojure.core/unchecked-negate)

(defalias subtract
  {:doc "Returns the result of unchecked subtraction of `_y_` from
        `_x_`. Subject to overflow."
   :added v1
   :see '[negate - dec dunaj.math/subtract
          dunaj.math.precise/subtract dunaj.host.int/isub]
   :tsig (Fn [Number Number Number])}
  clojure.core/unchecked-subtract)

(defn - :- java.lang.Long
  "If no ys are supplied, returns the unchecked negation of `_x_`,
  else subtracts the ys from `_x_` and returns the unchecked result.
  Subject to overflow."
  ;; TODO: support inlining
  {:added v1
   :see '[negate subtract dec dunaj.math/- dunaj.math.precise/-
          dunaj.host.int/isub]}
  ([x :- java.lang.Long]
   (. clojure.lang.Numbers (unchecked_minus x)))
  ([x :- java.lang.Long, y :- java.lang.Long]
   (. clojure.lang.Numbers (unchecked_minus x y)))
  ([x :- java.lang.Long, y :- java.lang.Long & more :- java.lang.Long]
   (reduce - (- x y) more)))

(defalias multiply
  {:doc "Returns the result of unchecked multiplication of `_x_` and
        `_y_`. Subject to overflow."
   :added v1
   :see '[* dunaj.math/multiply dunaj.math.precise/multiply
          dunaj.host.int/imul]
   :tsig (Fn [Number Number Number])}
  clojure.core/unchecked-multiply)

(defn *
  "Returns the unchecked product of nums. `(*)` returns 1.
  Subject to overflow."
  ;; TODO: support inlining
  {:added v1
   :see '[multiply dunaj.math/* dunaj.math.precise/*
          dunaj.host.int/imul]
   :tsig (Fn [1]
             [java.lang.Long java.lang.Long]
             [java.lang.Long java.lang.Long java.lang.Long]
             [java.lang.Long java.lang.Long java.lang.Long
              (Va java.lang.Long)])}
  ([] 1)
  ([x] x)
  ([x y] (. clojure.lang.Numbers (unchecked_multiply x y)))
  ([x y & more]
   (reduce * (* x y) more)))

(defalias inc
  {:doc "Returns a number one greater than `_x_`.
        Subject to overflow."
   :added v1
   :see '[add + dunaj.math/inc dunaj.math.precise/inc
          dunaj.host.int/iinc]
   :tsig (Fn [Number Number])}
  clojure.core/unchecked-inc)

(defalias dec
  {:doc "Returns a number one less than `_x_`. Subject to overflow."
   :added v1
   :see '[subtract - dunaj.math/dec dunaj.math.precise/dec
          dunaj.host.int/idec]
   :tsig (Fn [Number Number])}
  clojure.core/unchecked-dec)


;;;; Testing

(clojure.core/require '[clojure.bootstrap :refer [assert-primitive]])

(assert-primitive
 (add 1 2)
 (negate 3)
 (subtract 1 2)
 (multiply 1 2)
 (inc 1)
 (dec 1)
 (*)
 ;;(* 1)
 (+)
 ;;(+ 5)
)
