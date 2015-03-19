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

(ns dunaj.math.angle
  "Math functions of an angle, with floating point numbers.

  Precision as implemented by host."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Hyperbolic"]}
  (:api bare)
  (:require [clojure.core :refer [.]]
            [clojure.bootstrap :refer [defn v1 fn]]
            [dunaj.math :refer [Float]])
  (:import [java.lang Math]))


;;;; Public API

;;; Circular

(defn deg :- Float
  "Returns an angle in degrees which is approximately equivalent to
  given `_rad_` angle in radians. Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (toDegrees ~x)))}
  [rad :- Float]
  (Math/toDegrees rad))

(defn rad :- Float
  "Returns an angle in radians which is approximately equivalent to
  given `_deg_` angle in degrees. Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (toRadians ~x)))}
  [deg :- Float]
  (Math/toRadians deg))

(defn sin :- Float
  "Returns the circular sine of an angle `_rad_`.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (sin ~x)))}
  [rad :- Float]
  (Math/sin rad))

(defn cos :- Float
  "Returns the circular cosine of an angle `_rad_`.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (cos ~x)))}
  [rad :- Float]
  (Math/cos rad))

(defn tan :- Float
  "Returns the circular tangent of an angle `_rad_`.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (tan ~x)))}
  [rad :- Float]
  (Math/tan rad))

(defn asin :- Float
  "Returns the arc sine of a value `_x_`, in radians.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (asin ~x)))}
  [x :- Float]
  (Math/asin x))

(defn acos :- Float
  "Returns the arc cosine of a value `_x_`, in radians.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (acos ~x)))}
  [x :- Float]
  (Math/acos x))

(defn atan :- Float
  "Returns the arc tangent of a value `_x_`, in radians.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(. Math (atan ~x)))}
  [x :- Float]
  (Math/atan x))

(defn atan2 :- Float
  "Returns the angle theta from the conversion of rectangular
  coordinates `(_x_, _y_)` to polar coordinates `(r, theta)`.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x y] `(. Math (atan2 ~x ~y)))}
  [x :- Float, y :- Float]
  (Math/atan2 x y))

(defn hypot :- Float
  "Returns hypotenuse without intermediate overflow or underflow.
  Precision as implemented by host."
  {:added v1
   :category "Primary"
   :inline (fn [x y] `(. Math (hypot ~x ~y)))}
  [x :- Float, y :- Float]
  (Math/hypot x y))

;;; Hyperbolic

(defn sinh :- Float
  "Returns the hyperbolic sine of a hyperbolic angle `_x_`.
  Precision as implemented by host."
  {:added v1
   :category "Hyperbolic"
   :inline (fn [x] `(. Math (sinh ~x)))}
  [x :- Float]
  (Math/sinh x))

(defn cosh :- Float
  "Returns the hyperbolic cosine of a hyperbolic angle `_x_`.
  Precision as implemented by host."
  {:added v1
   :category "Hyperbolic"
   :inline (fn [x] `(. Math (cosh ~x)))}
  [x :- Float]
  (Math/cosh x))

(defn tanh :- Float
  "Returns the hyperbolic tangent of a hyperbolic angle `_x_`."
  {:added v1
   :category "Hyperbolic"
   :inline (fn [x] `(. Math (tanh ~x)))}
  [x :- Float]
  (Math/tanh x))


;;;; Testing

(clojure.core/require '[clojure.bootstrap :refer [assert-primitive]])

(assert-primitive
 (deg 3.14)
 (rad 180)
 (sin 2.07)
 (cos 2.07)
 (tan 2.07)
 (asin 0.07)
 (acos 0.07)
 (atan 0.07)
 (atan2 0.3 0.4)
 (hypot 3 4)
 (sinh 2.07)
 (cosh 2.07)
 (tanh 2.07)
)
