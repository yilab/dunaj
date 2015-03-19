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

(ns dunaj.math
  "Basic math facilities.

  Operations throw on overflow and don't auto-promote.
  Concrete number types are host specific and are specified in
  <<dunaj.host.number.api.ad#,dunaj.host.number>>.

  Depending on the needs, there are several other namespaces with
  similar functionalities:

  * `<<dunaj.math.precise.api.ad#,dunaj.math.precise>>`
    slower but with arbitrary precision, auto-promoting when needed
  * `<<dunaj.math.unchecked.api.ad#,dunaj.math.unchecked>>`
    much faster but subject to overflow
  * `<<dunaj.host.int.api.ad#,dunaj.host.int>>` host specific
    unchecked operations with fastest performance.

  Design notes:

  * Number types are not extensible (there are no number protocols),
    due to host optimizations.
  * No built-in number types. Utilizes host number types.
  * `clojure.core/\\*unchecked-math*` is omited by design."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Comparison" "Operations" "Rounding"]}
  (:api bare)
  (:require
   [clojure.core :as cc :refer
    [. if ratio? assert apply hash-map cond -> = binding when-not
     if-not recur seq first rest and declare nil? throw double long]]
   [clojure.bootstrap :refer [defalias defn def fn loop let v1
                              defprotocol not-implemented defmacro]]
   [dunaj.type :refer [Fn Maybe U Any Va Signature]]
   [dunaj.boolean :refer [Boolean not or]]
   [dunaj.host :refer [class-instance?]]
   [dunaj.host.int :refer [iint]])
  (:import
   [java.lang Math]
   [java.math BigDecimal MathContext RoundingMode BigInteger]
   [clojure.lang BigInt Numbers]))


;;;; Public API

(def Number :- Signature
  "A type signature for numbers."
  {:added v1
   :see '[number? Integer Float Decimal Rational INumerical num]
   :category "Primary"}
  java.lang.Number)

(defn number? :- Boolean
  "Returns `true` if `_x_` is a number, otherwise returns `false`."
  ;; TODO: inline for primitive support
  {:added v1
   :see '[Number integer? float? decimal? rational? numerical? num
          dunaj.host.number/number]
   :category "Primary"}
  [x :- Any]
  (class-instance? java.lang.Number x))

(def Integer :- Signature
  "A type signature for integer numbers.

  IMPORTANT: This is not a host Integer type, but rather a type
  signature for any integer type (e.g. JVM Integer, Byte, Short)."
  {:added v1
   :see '[integer? Number Float Decimal Rational dunaj.host.int/Int
          dunaj.host.number/long]
   :category "Primary"}
  (U java.lang.Integer java.lang.Long clojure.lang.BigInt
     java.math.BigInteger java.lang.Short java.lang.Byte))

(defn integer? :- Boolean
  "Returns `true` if `_x_` is an integer number,
  otherwise returns `false`."
  {:added v1
   :see '[Integer number? float? decimal? rational?
          dunaj.host.int/iint]
   :category "Primary"}
  ;; TODO: inline for primitive support
  [x :- Any]
  (or (class-instance? java.lang.Integer x)
      (class-instance? java.lang.Long x)
      (class-instance? java.lang.Byte x)
      (class-instance? clojure.lang.BigInt x)
      (class-instance? java.math.BigInteger x)
      (class-instance? java.lang.Short x)))

(def Float :- Signature
  "A type signature for floating point numbers.

  IMPORTANT: This is not a host Float type, but rather a type
  signature for any floating point type (e.g. JVM Float, Double)."
  {:added v1
   :see '[integer? Number Float Decimal Rational
          dunaj.host.number/double]
   :category "Primary"}
  (U java.lang.Double java.lang.Float))

(defn float? :- Boolean
  "Returns `true` if `_x_` is a floating point number,
  otherwise returns `false`."
  {:added v1
   :see '[Float number? integer? decimal? rational?]
   :category "Primary"}
  ;; TODO: inline for primitive support
  [x :- Any]
  (or (class-instance? java.lang.Double x)
      (class-instance? java.lang.Float x)))

(def Decimal :- Signature
  "A type signature for decimal numbers."
  {:added v1
   :see '[decimal? Number Integer Float Rational
          dunaj.host.number/bigdec]
   :category "Primary"}
  java.math.BigDecimal)

(defn decimal? :- Boolean
  "Returns `true` if `_x_` is a decimal number,
  otherwise returns `false`."
  {:added v1
   :see '[Decimal number? integer? float? rational?]
   :category "Primary"}
  [x :- Any]
  (class-instance? java.math.BigDecimal x))

(def Rational :- Signature
  "A type signature for rational numbers (not just Clojure ratios)."
  {:added v1
   :see '[rational? Number Integer Float Decimal]
   :category "Primary"}
  (U java.lang.Integer java.lang.Long clojure.lang.BigInt
     java.math.BigInteger java.lang.Short java.lang.Byte
     java.math.BigDecimal clojure.lang.Ratio))

(defn rational? :- Boolean
  "Returns `true` if `_x_` is a rational number,
  otherwise returns `false`."
  {:added v1
   :see '[Rational number? integer? float? decimal?]
   :category "Primary"}
  [x :- Any]
  (or (class-instance? java.lang.Integer x)
      (class-instance? java.lang.Long x)
      (class-instance? clojure.lang.BigInt x)
      (class-instance? java.math.BigInteger x)
      (class-instance? java.lang.Short x)
      (class-instance? java.lang.Byte x)
      (class-instance? java.math.BigDecimal x)
      (class-instance? clojure.lang.Ratio x)))

;;; Numerical protocol

(defprotocol INumerical
  "A value protocol for objects other than numbers which however
  have a canonical numerical representation. Types satisfying
  `INumerical` protocol must accept their canonical number form in
  their constructors.
  Examples of such type is character, instant or UUID."
  {:added v1
   :category "Primary"
   :see '[num Number number? dunaj.host.number/number]
   :predicate 'numerical?}
  (-numerical :- Number
    "Returns a numerical value representing `_this_` object."
    [this]))

(defn num
  "Returns a numerical value for `_x_`.
  Returns `_x_` if it is already a number.
  Returns `nil` if `_x_` is `nil`."
  {:added v1
   :see '[INumerical Number number? dunaj.host.number/number]
   :category "Primary"
   :tsig (Fn [nil nil] [Number Number] [Number INumerical])}
  [x]
  (cond (nil? x) nil
        (class-instance? java.lang.Number x) x
        :else (-numerical x)))

;;; Value predicates

(defalias zero?
  {:doc "Returns `true` if `_x_` is zero, otherwise returns `false`."
   :added v1
   :see '[one? pos? neg? dunaj.host.int/izero?]
   :category "Comparison"
   :tsig (Fn [Boolean Number])})

(defn one? :- Boolean
  "Returns `true` if `_x_` is equal to 1, `false` otherwise."
  {:added v1
   :see '[zero? pos? neg? dunaj.host.int/ione?]
   :category "Comparison"}
  [x :- Number]
  (cc/== x 1))

(defalias pos?
  {:doc "Returns `true` if `_x_` is greater than zero,
        otherwise returns `false`."
   :added v1
   :see '[npos? zero? one? neg? dunaj.host.int/ipos?]
   :category "Comparison"
   :tsig (Fn [Boolean Number])})

(defalias neg?
  {:doc "Returns `true` if `_x_` is less than zero,
        otherwise returns `false`."
   :added v1
   :see '[nneg? zero? pos? neg? dunaj.host.int/ineg?]
   :category "Comparison"
   :tsig (Fn [Boolean Number])})

(defn npos? :- Boolean
  "Returns `true` if `_x_` is not positive, `false` otherwise."
  {:added v1
   :see '[pos? zero? neg? dunaj.host.int/inpos?]
   :category "Comparison"}
  [x :- Number]
  (not (pos? x)))

(defn nneg? :- Boolean
  "Returns `true` if `_x_` is not negative, `false` otherwise."
  {:added v1
   :see '[neg? zero? pos? dunaj.host.int/inneg?]
   :category "Comparison"}
  [x :- Number]
  (not (neg? x)))

(defalias even?
  {:doc "Returns `true` if `_n_` is even, otherwise returns `false`."
   :added v1
   :see '[odd? dunaj.host.int/ieven?]
   :category "Comparison"
   :tsig (Fn [Boolean Integer])})

(defalias odd?
  {:doc "Returns `true` if `_n_` is odd, otherwise returns `false`."
   :added v1
   :see '[odd? dunaj.host.int/iodd?]
   :category "Comparison"
   :tsig (Fn [Boolean Integer])})

(defalias <
  {:doc "Returns `true` if nums are in monotonically increasing order,
        otherwise returns `false`."
   :added v1
   :see '[<= == > dunaj.host.int/i<]
   :category "Comparison"
   :tsig (Fn [Boolean Number]
             [Boolean Number Number]
             [Boolean Number Number (Va Number)])})

(defalias <=
  {:doc "Returns `true` if nums are in monotonically non-decreasing
        order, otherwise returns `false`."
   :added v1
   :see '[< == >= dunaj.host.int/i<=]
   :category "Comparison"
   :tsig (Fn [Boolean Number]
             [Boolean Number Number]
             [Boolean Number Number (Va Number)])})

(defalias >
  {:doc "Returns `true` if nums are in monotonically decreasing
        order, otherwise returns `false`."
   :added v1
   :see '[< == >= dunaj.host.int/i>]
   :category "Comparison"
   :tsig (Fn [Boolean Number]
             [Boolean Number Number]
             [Boolean Number Number (Va Number)])})

(defalias >=
  {:doc "Returns `true` if nums are in monotonically non-increasing
        order, otherwise returns `false`."
   :added v1
   :see '[<= == > dunaj.host.int/i>=]
   :category "Comparison"
   :tsig (Fn [Boolean Number]
             [Boolean Number Number]
             [Boolean Number Number (Va Number)])})

(defalias ==
  {:doc "Returns `true` if nums all have the equivalent value
        (type-independent), otherwise returns `false`."
   :added v1
   :see '[< > dunaj.host.int/i==]
   :category "Comparison"
   :tsig (Fn [Boolean Number]
             [Boolean Number Number]
             [Boolean Number Number (Va Number)])})

;;; Transformation

(defalias rationalize
  {:doc "Returns the rational value of `_num_`."
   :added v1
   :see '[numerator denominator rational? trunc]
   :category "Primary"
   :tsig (Fn [Rational Number])})

(defn numerator :- Integer
  "Returns the numerator part of a given rational number `_x_`."
  {:added v1
   :see '[rationalize denominator rational? trunc]
   :category "Primary"}
  [x :- Rational]
  (cond (ratio? x) (clojure.core/numerator x)
        (rational? x) x
        :else (throw (java.lang.IllegalArgumentException.
                      "Not a rational number."))))

(defn denominator :- Integer
  "Returns the denominator part of a given rational number `_x_`."
  {:added v1
   :see '[rationalize numerator rational? trunc]
   :category "Primary"}
  [x :- Rational]
  (cond (ratio? x) (clojure.core/denominator x)
        (rational? x) 1
        :else (throw (java.lang.IllegalArgumentException.
                      "Not a rational number."))))

(declare quot)

(defn trunc :- Integer
  "Returns an integer number created from given floating or decimal
  number `_x_` by truncating its decimal part. Returns unchanged if
  `_x_` is integer or rational."
  {:added v1
   :see '[round rationalize ceil floor]
   :category "Rounding"}
  [x :- Number]
  (if-not (integer? x)
    ;; TODO: optimize performance
    (Numbers/reduceBigInt (quot (rationalize x) 1))
    x))

;;; Computation

(defn add :- Number
  "Returns the sum of `_x_` and `_y_`.
  Does not auto-promote, will throw on overflow."
  {:added v1
   :see '[+ dunaj.math.precise/add dunaj.math.unchecked/add
          dunaj.host.int/iadd]
   :category "Operations"
   :inline (fn [x y] `(. Numbers (add ~x ~y)))}
  [x :- Number, y :- Number]
  (. Numbers (add x y)))

(defalias +
  {:doc "Returns the sum of nums. `(+)` returns 0.
        Does not auto-promote, will throw on overflow."
   :added v1
   :see '[add dunaj.math.precise/+ dunaj.math.unchecked/+
          dunaj.host.int/iadd]
   :category "Operations"
   :tsig (Fn [Number]
             [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])})

(defn negate :- Number
  "Returns the negation of `_x_`.
  Does not auto-promote, will throw on overflow."
  {:added v1
   :see '[subtract - dunaj.math.precise/negate
          dunaj.math.unchecked/negate
          dunaj.host.int/ineg]
   :category "Operations"
   :inline (fn [x] `(. Numbers (minus ~x)))}
  [x :- Number]
  (. Numbers (minus x)))

(defn subtract :- Number
  "Returns the result of subtraction of `_y_` from `_x_`.
  Does not auto-promote, will throw on overflow."
  {:added v1
   :see '[negate - dunaj.math.precise/subtract
          dunaj.math.unchecked/subtract
          dunaj.host.int/isub]
   :category "Operations"
   :inline (fn [x y] `(. Numbers (minus ~x ~y)))}
  [x :- Number, y :- Number]
  (. Numbers (minus x y)))

(defalias -
  {:doc "If no ys are supplied, returns the negation of `_x_`,
        else subtracts the ys from `_x_` and returns the result.
        Does not auto-promote, will throw on overflow."
   :added v1
   :see '[negate subtract dunaj.math.precise/-
          dunaj.math.unchecked/- dunaj.host.int/isub]
   :category "Operations"
   :tsig (Fn [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])})

(defn multiply :- Number
  "Returns the result of multiplication of `_x_` and `_y_`.
  Does not auto-promote, will throw on overflow."
  {:added v1
   :see '[* dunaj.math.precise/multiply
          dunaj.math.unchecked/multiply dunaj.host.int/imul]
   :category "Operations"
   :inline (fn [x y] `(. Numbers (multiply ~x ~y)))}
  [x :- Number, y :- Number]
  (. Numbers (multiply x y)))

(defalias *
  {:doc "Returns the product of nums. `(*)` returns 1.
        Does not auto-promote, will throw on overflow."
   :added v1
   :see '[multiply dunaj.math.precise/*
          dunaj.math.unchecked/* dunaj.host.int/imul]
   :category "Operations"
   :tsig (Fn [Number]
             [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])})

(defn divide :- Number
  "Returns the result of division of `_x_` by `_y_`."
  {:added v1
   :see '[/ dunaj.host.int/idiv]
   :category "Operations"
   :inline (fn [x y] `(. Numbers (divide ~x ~y)))}
  [x :- Number, y :- Number]
  (. Numbers (divide x y)))

(defalias /
  {:doc "If no denominators are supplied, returns 1/`_x_`,
        else returns `_x_` divided by all of the denominators."
   :added v1
   :see '[divide dunaj.host.int/idiv]
   :category "Operations"
   :tsig (Fn [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])})

(defalias inc
  {:doc "Returns a number one greater than `_x_`.
        Does not auto-promote, will throw on overflow."
   :added v1
   :see '[+ dec dunaj.math.precise/inc
          dunaj.math.unchecked/inc dunaj.host.int/iinc]
   :category "Operations"
   :tsig (Fn [Number Number])})

(defalias dec
  {:doc "Returns a number one less than `_x_`.
        Does not auto-promote, will throw on overflow."
   :added v1
   :see '[- inc dunaj.math.precise/dec
          dunaj.math.unchecked/dec dunaj.host.int/idec]
   :category "Operations"
   :tsig (Fn [Number Number])})

(defalias min
  {:doc "Returns the least of the nums."
   :added v1
   :see '[max < dunaj.host.int/imin]
   :category "Comparison"
   :tsig (Fn [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])})

(defalias max
  {:doc "Returns the greatest of the nums."
   :added v1
   :see '[min > dunaj.host.int/imax dunaj.host.int/imax0]
   :category "Comparison"
   :tsig (Fn [Number Number]
             [Number Number Number]
             [Number Number Number (Va Number)])})

(defalias quot
  {:doc "Returns the quotient of dividing `_num_` by `_div_`."
   :added v1
   :see '[rem mod dunaj.host.int/idiv]
   :category "Operations"
   :tsig (Fn [Number Number Number])})

(defalias rem
  {:doc "Returns the remainder of dividing `_num_` by `_div_`."
   :added v1
   :see '[mod quot dunaj.host.int/irem]
   :category "Operations"
   :tsig (Fn [Number Number Number])})

(defalias mod
  {:doc "Returns the modulus of `_num_` and `_div_`.
        Truncates toward negative infinity."
   :added v1
   :see '[rem quot dunaj.host.int/irem]
   :category "Operations"
   :tsig (Fn [Number Number Number])})

(defn abs :- Number
  "Returns the absolute value of a number `x`."
  ;; TODO: add inline in order to support primitives
  {:added v1
   :see '[* dunaj.host.int/iabs]
   :category "Operations"}
  [x :- Number]
  ;; TODO: support bignum
  (if (class-instance? clojure.lang.Ratio x)
    (if (neg? x) (* -1 x) x)
    ;; WARNING: Reflection, still faster than * ???
    ;; TODO: use cond with dispatch on type?
    (Math/abs x)))

(defn divisible? :- Boolean
  "Returns `true` if `_num_` is divisible by `_div_`,
  otherwise returns `false`."
  {:added v1
   :see '[indivisible? gcd lcm mod rem]
   :category "Comparison"}
  [num :- Number div :- Number]
  (zero? (mod num div)))

(defn gcd :- Integer
  "Returns the greatest common divisor of given integer numbers."
  {:added v1
   :see '[lcm divisible? indivisible?]
   :category "Operations"}
  ;; TODO: make it faster, mainly varargs version
  ([x :- Integer, y :- Integer]
     (let [to-big-integer #(cond (class-instance? BigInt %)
                                 (.toBigInteger ^BigInt %)
                                 (class-instance? BigInteger %) %
                                 :else (BigInteger/valueOf %))
           bx :- BigInteger (to-big-integer x)
           by :- BigInteger (to-big-integer y)]
       (-> (.gcd bx by)
           (BigInt/fromBigInteger)
           (Numbers/reduceBigInt))))
  ([x :- Integer, y :- Integer, & more :- Integer]
     (loop [g (gcd x y), xs more]
       (if (seq xs)
         (recur (gcd g (first xs)) (rest xs))
         g))))

(defn lcm :- Integer
  "Returns the least common multiple of given integer numbers."
  ;; TODO: make it faster, mainly varargs version
  {:added v1
   :see '[gcd divisible? indivisible?]
   :category "Operations"}
  ([x :- Integer, y :- Integer]
     (let [to-big-integer #(cond (class-instance? BigInt %)
                                 (.toBigInteger ^BigInt %)
                                 (class-instance? BigInteger %) %
                                 :else (BigInteger/valueOf %))
           bx :- BigInteger (to-big-integer x)
           by :- BigInteger (to-big-integer y)]
       (-> (.multiply (.divide (.abs bx) (.gcd bx by)) (.abs by))
           (BigInt/fromBigInteger)
           (Numbers/reduceBigInt))))
  ([x :- Integer, y :- Integer, & more :- Integer]
     (loop [g (lcm x y), xs more]
       (if (seq xs)
         (recur (lcm g (first xs)) (rest xs))
         g))))

;;; Rounding

(def ^:private rounding-mode :- {clojure.lang.Keyword RoundingMode}
  "A translation map for rounding modes."
  {:ceiling RoundingMode/CEILING
   :floor RoundingMode/FLOOR
   :half-up RoundingMode/HALF_UP
   :half-down RoundingMode/HALF_DOWN
   :half-even RoundingMode/HALF_EVEN
   :up RoundingMode/UP
   :down RoundingMode/DOWN
   :unnecessary RoundingMode/UNNECESSARY})

(def PrecisionConfig :- Signature
  "A type signature for precision config."
  {:added v1
   :see '[round with-precision]
   :category "Rounding"}
  {:precision Integer
   :type (U :decimal :significant)
   :mode (U :ceiling :floor
            :half-up :half-down :half-even
            :up :down :unnecessary)})

(defn math-context :- (Maybe MathContext)
  "Returns host math context based on config."
  [config :- PrecisionConfig]
  (when-not (= :decimal (:type config))
    (MathContext. (or (:precision config) 0)
                  (rounding-mode (or (:mode config) :half-up)))))

(defn ^:private set-scale :- BigDecimal
  "Returns bigdecimal number `x` rounded to `p` decimal digits,
  using `rm` RoundingMode configuration."
  [x :- BigDecimal, p :- Integer, rm :- RoundingMode]
  (.setScale x (iint p) rm))

(defn ^:private round-bigdec :- BigDecimal
  "Returns rounded BigDecimal based on given config."
  [x :- BigDecimal, config :- PrecisionConfig]
  (if (= :decimal (:type config))
    (set-scale x (or (:precision config) 0)
               (rounding-mode (or (:mode config) :half-up)))
    (.round x (math-context config))))

(defn round :- Number
  "Returns rounded number of same type according to given precision
  config.

  Precision `_config_` is a map with following keys:

  * `:precision` - Any integer. Nonnegative in case of `:significant`
    type. Defaults to 0 (0 precision with significant type means
    that any number of significant digits is OK).
  * `:type` - `:significant` or `:decimal`.
    Defaults to `:significant`.
  * `:mode` - `:ceiling`, `:floor`, `:half-up`, `:half-down`,
    `:half-even`, `:up`, `:down` and `:unnecessary`.
    Defaults to `:half-up`.

  `(round _x_)` is equal to `(round _x_ {:type :decimal})`. Note
  that this is different from `(round _x_ {})`, which equals to
  `(round _x_ {:precision 0 :type :significant :mode :half-up})`"
  ;; TODO: add inline in order to support primitives?
  {:added v1
   :see '[PrecisionConfig floor ceil trunc with-precision]
   :category "Rounding"}
  ([x :- Number]
   (round x {:type :decimal}))
  ([x :- Number, config :- PrecisionConfig]
   ;; TODO: Support for primitives and performance optimizations
   (let [prepared
         (cond
           (class-instance? BigInt x) (.toBigDecimal ^BigInt x)
           (class-instance? BigDecimal x) x
           (or (class-instance? clojure.lang.Ratio x) (float? x))
           (BigDecimal/valueOf (double x))
           :else (BigDecimal/valueOf (long x)))
         rounded (round-bigdec prepared config)]
     (cond
       (float? x) (.doubleValue rounded)
       (class-instance? BigInteger x) (.toBigInteger rounded)
       (class-instance? BigInt x) (BigInt/fromBigInteger rounded)
       (integer? x) (.longValue rounded)
       :else rounded)))
  ([x :- Number,
    key :- (U :precision :type :mode), val :- Any & keyvals :- Any]
   (round x (apply hash-map key val keyvals))))

(defn floor :- Number
  "Returns the floor of a given number `_x_`."
  ;; TODO: add inline in order to support primitives
  {:added v1
   :see '[round ceil trunc]
   :category "Rounding"}
  [x :- Number]
  (round x {:type :decimal :mode :floor}))

(defn ceil :- Number
  "Returns the ceiling of a given number `_x_`."
   ;; TODO: add inline in order to support primitives
  {:added v1
   :see '[round floor trunc]
   :category "Rounding"}
  [x :- Number]
  (round x {:type :decimal :mode :ceiling}))

(defmacro with-precision
  "Sets the precision config to be used for BigDecimal operations.

  Precision `_config_` is a map with following keys:

  * `:precision` - A nonnegative integer. Defaults to 0 (no changes)
  * `:mode` - One of `:ceiling`, `:floor`, `:half-up`,
    `:half-down`, `:half-even`, `:up`, `:down` and `:unnecessary`.
    Defaults to `:half-up`."
  {:added v1
   :see '[round PrecisionConfig]
   :category "Rounding"}
  [config & body]
  (if (= :decimal (:type config))
    ;; TODO: implement :decimal precision type for bigdec ops
    (not-implemented)
    `(binding [clojure.core/*math-context* (math-context ~config)]
       ~@body)))

;;; Exponentiation

(defn pow :- Number
  "Returns `_x^y^_`, the `_x_` raised to power of `_y_`."
  ;; TODO: add inline in order to support primitives
  {:added v1
   :see '[sqrt cbrt exp]
   :category "Operations"}
  ([x :- Number, y :- Number]
     (if (and (integer? x) (integer? y) (nneg? y))
       (let [x (cond (class-instance? BigInt x)
                     (.toBigInteger ^BigInt x)
                     (class-instance? BigInteger x) x
                     :else (BigInteger/valueOf x))
             bx :- BigInteger x]
         (.pow bx y))
       (Math/pow x y))))

(defn sqrt :- Float
  "Returns the square root of a floating point number `_x_`."
  {:added v1
   :see '[cbrt pow]
   :category "Operations"
   :inline (fn [x] `(. Math (sqrt ~x)))}
  [x :- Float]
  (Math/sqrt x))

(defn cbrt :- Float
  "Returns the cube root of a floating point number `_x_`."
  {:added v1
   :see '[sqrt pow]
   :category "Operations"
   :inline (fn [x] `(. Math (cbrt ~x)))}
  [x :- Float]
  (Math/cbrt x))

(defn exp :- Float
  "Returns `_e^x^_`, the `e` raised to the power of a floating point
  number `_x_`."
  {:added v1
   :see '[expm1 log]
   :category "Operations"
   :inline (fn [x] `(. Math (exp ~x)))}
  [x :- Float]
  (Math/exp x))

(defn expm1 :- Float
  "Returns `(dec (exp _x_))`, which yields more precise result."
  {:added v1
   :see '[exp log1p]
   :category "Operations"
   :inline (fn [x] `(. Math (expm1 ~x)))}
  [x :- Float]
  (Math/expm1 x))

(defn log :- Float
  "Returns the natural logarithm of a floating point number `_x_`."
  {:added v1
   :see '[log1p log10 exp]
   :category "Operations"
   :inline (fn [x] `(. Math (log ~x)))}
  [x :- Float]
  (Math/log x))

(defn log1p :- Float
  "Returns `(inc (exp _x_))`, which yields more precise result."
  {:added v1
   :see '[log log10 expm1]
   :category "Operations"
   :inline (fn [x] `(. Math (log1p ~x)))}
  [x :- Float]
  (Math/log1p x))

(defn log10 :- Float
  "Returns the base 10 logarithm of a floating point number `_x_`."
  {:added v1
   :see '[log log1p]
   :category "Operations"
   :inline (fn [x] `(. Math (log10 ~x)))}
  [x :- Float]
  (Math/log10 x))

;;; Constants

(def ^:const ^java.lang.Double pi :- Float
  "The `PI` constant as defined by the host."
  {:added v1
   :see '[e dunaj.math.angle/deg]}
  Math/PI)

(def ^:const ^java.lang.Double e :- Float
  "The `e` constant as defined by the host."
  {:added v1
   :see '[pi exp expm1 log log1p log10]}
  Math/E)


;;;; Testing

(clojure.core/require
 '[clojure.bootstrap :refer [assert-boolean assert-primitive]])

(assert-boolean
 (number? 5)
 (integer? 5)
 (float? 5)
 (decimal? 5)
 (rational? 5)
 (zero? 5)
 (one? 5)
 (pos? 5)
 (neg? 5)
 (npos? 5)
 (nneg? 5)
 (even? 5)
 (odd? 5)
 (< 5 6)
 (<= 5 6)
 (== 5 6)
 (> 5 6)
 (>= 5 6))

(assert-primitive
 ;; (trunc 2)
 (+ 2 1)
 (add 1 2)
 (negate 1)
 (subtract 1 2)
 (- 1 2)
 (multiply 1 2)
 (* 1 2)
 (inc 1)
 (dec 1)
 (min 1 2)
 (max 1 2)
 (rem 1 2)
 (quot 1 2)
 ;; (round 2.1)
 ;; (floor 2.1)
 ;; (ceil 2.1)
 ;; (pow 2.0 3.0)
 (sqrt 2.0)
 (cbrt 2.0)
 (exp 2.0)
 (expm1 2.0)
 (log 2.0)
 (log1p 2.0)
 (log10 2.0))
