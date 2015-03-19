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

(ns dunaj.bit
  "Functions for bitwise manipulation.

  Functions defined in this namespace work with any integer type.
  More performant functions that work with
  <<dunaj.host.int.api.ad#Int,Int>> type can be found in
  <<dunaj.host.int.api.ad#Bitwise,dunaj.host.int>>. Boolean logic
  functions can be found in
  <<dunaj.boolean.api.ad#Logic,dunaj.boolean>>.

  WARNING: JVM host specific: bitwise operations are not supported
  for integers of http://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html[BigInteger]
  or http://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/BigInt.java[BigInt] type.

  TIP: JVM host specific: It is usually more performant to use
  <<dunaj.host.int.api.ad#Bitwise,dunaj.host.int>> when handling
  numeric data from host (e.g. array indexes, enums, lenghts).
  
  [TIP]
  --
  When using numeric constants, remember that `CLJ` format supports
  hexadecimal and binary notations for integer literals.
  It will make your code more readable.
  
  [source,clojure,linenums]
  ----
  (binary (bit/and 2r10110 2r11011))
  ;;=> \"00010010\"
  
  (hexa (bit/and 0x0F0 0x1BC))
  ;;=> \"0x00B0\"
  ----
  --
  
  NOTE: Prefer aliasing this namespace (as shown in the usage
  example) to refering its functions."
  {:categories
   ["Primary"
    ["Logic"
     "See also
     <<dunaj.host.int.api.ad#Bitwise, bitwise operators on Ints>>
     and <<dunaj.boolean.api.ad#Logic, boolean logic operators>>."]
    ["Shifts"
     "See also
     <<dunaj.host.int.api.ad#Shifts, bitwise operators on Ints>>."]]
   :authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [defalias v1]]
   [dunaj.type :refer [Fn Va]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.math :refer [Integer]]))


;;;; Public API

(defalias clear
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :doc "Returns the number `_x_` with cleared (set to `0`) bit at
        zero-based index `_n_`."
   :category "Primary"}
  clojure.core/bit-clear)

(defalias flip
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :doc "Returns the number `_x_` with flipped bit at zero-based
        index `_n_`."
   :category "Primary"}
  clojure.core/bit-flip)

(defalias set
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :doc "Returns the number `_x_` with set (to `1`) bit at zero-based
        index `_n_`."
   :category "Primary"}
  clojure.core/bit-set)

(defalias test
  {:added v1
   :tsig (Fn [Boolean Integer Integer])
   :doc "Returns `true` if number `_x_` has bit at zero-based index
        `_n_` set to `1`, otherwise returns `false`."
   :category "Primary"}
  clojure.core/bit-test)

;;; Shifts

(defalias shift-left
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :category "Shifts"
   :doc "Returns the result of shifting `_x_` to the left by `_n_`
        bits."
   :see '[<< dunaj.host.int/i<<]}
  clojure.core/bit-shift-left)

(defalias shift-right
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :category "Shifts"
   :doc "Returns the result of shifting `_x_` to the right by `_n_`
        bits. Performs an arithmetic shift, preserving the sign of
        the input number."
   :see '[>> dunaj.host.int/i>> unsigned-shift-right]}
  clojure.core/bit-shift-right)

(defalias unsigned-shift-right
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :category "Shifts"
   :doc "Returns the result of shifting `_x_` to the right by `_n_`
        bits. Performs an unsigned (logical) shift, inserting binary
        value `0`."
   :see '[>>> dunaj.host.int/i>>> shift-right]}
  clojure.core/unsigned-bit-shift-right)

(defalias <<
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :category "Shifts"
   :doc "Alias of `<<shift_left,shift-left>>`. Returns the result of
        shifting `_x_` to the left by `_n_` bits."
   :see '[shift-left dunaj.host.int/i<<]}
  shift-left)

(defalias >>
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :category "Shifts"
   :doc "Alias of `<<shift_right,shift-right>>`. Returns the result of
        shifting `_x_` to the left by `_n_` bits."
   :see '[shift-right dunaj.host.int/i>> >>>]}
  shift-right)

(defalias >>>
  {:added v1
   :tsig (Fn [Integer Integer Integer])
   :category "Shifts"
   :doc "Alias of `<<unsigned_shift_right,unsigned-shift-right>>`.
        Returns the result of shifting `_x_` to the left by `_n_`
        bits."
   :see '[unsigned-shift-right dunaj.host.int/i>>> >>]}
  unsigned-shift-right)

;;; Logic

(defalias and
  {:added v1
   :tsig (Fn [Integer Integer Integer]
             [Integer Integer Integer (Va Integer)])
   :category "Logic"
   :doc "Returns the result of performing logical AND on each group
        of corresponding bits."
   :see '[dunaj.host.int/iand dunaj.boolean/and and-not]}
  clojure.core/bit-and)

(defalias or
  {:added v1
   :tsig (Fn [Integer Integer Integer]
             [Integer Integer Integer (Va Integer)])
   :category "Logic"
   :doc "Returns the result of performing logical OR on each group
        of corresponding bits."
   :see '[dunaj.host.int/ior dunaj.boolean/or xor]}
  clojure.core/bit-or)

(defalias xor
  {:added v1
   :tsig (Fn [Integer Integer Integer]
             [Integer Integer Integer (Va Integer)])
   :category "Logic"
   :doc "Returns the result of performing logical XOR on each group
        of corresponding bits."
   :see '[dunaj.host.int/ixor dunaj.boolean/xor or]}
  clojure.core/bit-xor)

(defalias and-not
  {:added v1
   :tsig (Fn [Integer Integer Integer]
             [Integer Integer Integer (Va Integer)])
   :category "Logic"
   :see '[and not dunaj.boolean/implication]
   :doc "Returns bitwise `and` with complement
        (a material non-implication) of `_x_` and `_y_`."}
  clojure.core/bit-and-not)

(defalias not
  {:added v1
   :tsig (Fn [Integer Integer])
   :category "Logic"
   :doc "Returns the result of performing logical NOT on each bit
        of `_x_`."
   :see '[dunaj.host.int/inot dunaj.boolean/not]}
  clojure.core/bit-not)
