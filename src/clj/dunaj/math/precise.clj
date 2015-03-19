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

(ns dunaj.math.precise
  "Precise math with an arbitrary precision.

  Slower than normal math operations, but auto-promotes when needed.

  Depending on the needs, there are several other namespaces with
  similar functionalities:

  * `<<dunaj.math.api.ad#,dunaj.math>>` which is slightly faster
    but subject to rounding and truncation
  * `<<dunaj.math.unchecked.api.ad#,dunaj.math.unchecked>>`,
    much faster but subject to overflow
  * `<<dunaj.host.int.api.ad#,dunaj.host.int>>`, host specific
    unchecked operations with fastest performance."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require [clojure.core :refer [. let assert]]
            [clojure.bootstrap :refer [defalias defn v1]]
            [dunaj.type :refer [Fn Va]]
            [dunaj.math :refer [Number Integer]]))


;;;; Public API

(defn add :- Number
  "Returns the sum of `_x_` and `_y_`. Supports arbitrary precision."
  {:added v1
   :see '[+ dunaj.math/add dunaj.math.unchecked/add
          dunaj.host.int/iadd]}
  [x :- Number, y :- Number]
  (. clojure.lang.Numbers (addP x y)))

(defalias +
  {:doc "Returns the sum of nums. `(+)` returns 0.
        Supports arbitrary precision."
   :added v1
   :see '[add inc dunaj.math/+ dunaj.math.unchecked/+
          dunaj.host.int/iadd]
   :tsig (Fn [Number]
             [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])}
  clojure.core/+')

(defn negate :- Number
  "Returns the negation of `_x_`. Supports arbitrary precision."
  {:added v1
   :see '[subtract - dunaj.math/negate dunaj.math.unchecked/negate
          dunaj.host.int/ineg]}
  [x :- Number]
  (. clojure.lang.Numbers (minusP x)))

(defn subtract :- Number
  "Returns the result of subtraction of `_y_` from `_x_`.
  Supports arbitrary precision."
  {:added v1
   :see '[negate - dec dunaj.math/subtract
          dunaj.math.unchecked/subtract dunaj.host.int/isub]}
  [x :- Number, y :- Number]
  (. clojure.lang.Numbers (minusP x y)))

(defalias -
  {:doc "If no ys are supplied, returns the negation of `_x_`,
        else subtracts the ys from `_x_` and returns the result.
        Supports arbitrary precision."
   :added v1
   :see '[negate subtract dec dunaj.math/- dunaj.math.unchecked/-
          dunaj.host.int/isub]
   :tsig (Fn [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])}
  clojure.core/-')

(defn multiply :- Number
  "Returns the result of multiplication of `_x_` and `_y_`.
  Supports arbitrary precision."
  {:added v1
   :see '[* dunaj.math/multiply dunaj.math.unchecked/multiply
          dunaj.host.int/imul]}
  [x :- Number, y :- Number]
  (. clojure.lang.Numbers (multiplyP x y)))

(defalias *
  {:doc "Returns the product of nums. `(*)` returns 1.
        Supports arbitrary precision."
   :added v1
   :see '[multiply dunaj.math/* dunaj.math.unchecked/*
          dunaj.host.int/imul]
   :tsig (Fn [Number]
             [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])}
  clojure.core/*')

(defalias inc
  {:doc "Returns a number one greater than `_x_`.
        Supports arbitrary precision."
   :added v1
   :see '[add + dunaj.math/inc dunaj.math.unchecked/inc
          dunaj.host.int/iinc]
   :tsig (Fn [Number Number])}
  clojure.core/inc')

(defalias dec
  {:doc "Returns a number one less than `_x_`.
        Supports arbitrary precision."
   :added v1
   :see '[subtract - dunaj.math/dec dunaj.math.unchecked/dec
          dunaj.host.int/idec]
   :tsig (Fn [Number Number])}
  clojure.core/dec')
