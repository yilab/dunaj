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

(ns dunaj.host.number
  "Coercions to host number types."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require [clojure.bootstrap :refer [defalias v1]]
            [dunaj.type :refer [Fn Any]]
            [dunaj.math :refer [Number]]))


;;;; Public API

(defalias number
  {:added v1
   :see '[dunaj.math/num]
   :tsig (Fn [Number Any])}
  clojure.core/num)

(defalias bigdec
  {:added v1
   :see '[double float dunaj.math/Decimal]
   :tsig (Fn [java.math.BigDecimal Any])})

(defalias bigint
  {:added v1
   :see '[biginteger long dunaj.math/Integer]
   :tsig (Fn [clojure.lang.BigInt Any])})

(defalias biginteger
  {:added v1
   :see '[bigint long dunaj.math/Integer]
   :tsig (Fn [java.math.BigInteger Any])})

(defalias byte
  {:added v1
   :see '[unchecked-byte dunaj.math/Integer]
   :tsig (Fn [java.lang.Byte Any])})

(defalias unchecked-byte
  {:added v1
   :see '[byte dunaj.math/Integer]
   :tsig (Fn [java.lang.Byte Any])})

(defalias double
  {:added v1
   :see '[unchecked-double dunaj.math/Float]
   :tsig (Fn [java.lang.Double Any])})

(defalias unchecked-double
  {:added v1
   :see '[double dunaj.math/Float]
   :tsig (Fn [java.lang.Double Any])})

(defalias float
  {:added v1
   :see '[unchecked-float dunaj.math/Float]
   :tsig (Fn [java.lang.Float Any])})

(defalias unchecked-float
  {:added v1
   :see '[float dunaj.math/Float]
   :tsig (Fn [java.lang.Float Any])})

(defalias int
  {:added v1
   :see '[unchecked-int dunaj.math/Integer dunaj.host.int/Int
          dunaj.host.int/iint]
   :tsig (Fn [java.lang.Integer Any])})

(defalias unchecked-int
  {:added v1
   :see '[int dunaj.math/Integer dunaj.host.int/Int
          dunaj.host.int/iint]
   :tsig (Fn [java.lang.Integer Any])})

(defalias long
  {:added v1
   :see '[unchecked-long dunaj.math/Integer]
   :tsig (Fn [java.lang.Long Any])})

(defalias unchecked-long
  {:added v1
   :see '[long dunaj.math/Integer]
   :tsig (Fn [java.lang.Long Any])})

(defalias short
  {:added v1
   :see '[unchecked-short dunaj.math/Integer]
   :tsig (Fn [java.lang.Short Any])})

(defalias unchecked-short
  {:added v1
   :see '[short dunaj.math/Integer]
   :tsig (Fn [java.lang.Short Any])})


;;;; Testing

(clojure.core/require '[clojure.bootstrap :refer [assert-primitive]])

(assert-primitive
 (byte 3)
 (byte \a)
 (unchecked-byte 400)
 (double 3)
 (unchecked-double 3)
 (float 4)
 (unchecked-float 5)
 (int 4)
 (unchecked-int 0)
 (long 3)
 (unchecked-long 9)
 (short 5)
 (unchecked-short 9))
