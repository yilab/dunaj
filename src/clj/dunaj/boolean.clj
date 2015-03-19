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

(ns dunaj.boolean
  "Boolean type and related logic operations.

  Dunaj (and Clojure) performs an automatic coercion to `Boolean`
  when e.g. calling a function which expects a boolean argument.
  By definition, `nil` and `false` coerce to `false`, any other
  value coerces to `true`.
  
  TIP: JVM host specific: Dunaj supports primitive booleans as
  function arguments and return values (subject to the number of
  arguments and combination of argument types).
  
  [WARNING]
  --
  JVM host specific: `false` is identical to http://docs.oracle.com/javase/8/docs/api/java/lang/Boolean.html#FALSE[`Boolean/FALSE`], so
  using other instances of http://docs.oracle.com/javase/8/docs/api/java/lang/Boolean.html[`java.lang.Boolean`] may lead to surprising
  results. In such cases, you may want to use `boolean` function
  to coerce into correct boolean value.
  
  [source,clojure,linesnum]
  ----
  (if (java.lang.Boolean. false) :is-true :is-false)
  ;;=> :is-true
  
  (if (boolean (java.lang.Boolean. false)) :is-true :is-false)
  ;;=> :is-false
  ----
  --"
  {:categories
   ["Primary"
    ["Logic"
     "See also <<dunaj.bit.api.ad#Logic, bitwise logic operators>>."]]
   :authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [if seq recur first inc rest odd? cons]]
   [clojure.bootstrap :refer
    [defalias deftype defmacro v1 defn let fn]]
   [dunaj.type :refer [Fn Any Predicate Macro Va]]))


;;;; Public API

(deftype Boolean
  "A boolean type, which has two values, `true` and `false`.

  [NOTE]
  --
  JVM host specific: `false` is identical to http://docs.oracle.com/javase/8/docs/api/java/lang/Boolean.html#FALSE[`Boolean/FALSE`]
  and `true` is identical to http://docs.oracle.com/javase/8/docs/api/java/lang/Boolean.html#TRUE[`Boolean/TRUE`]
  --"
  {:added v1
   :predicate 'boolean?
   :category "Primary"
   :see '[boolean false? true?]}
  java.lang.Boolean)

(defalias boolean
  "Returns `_x_` coerced to `Boolean` type, by returning `false` if
  `_x_` is `nil` or `false` value, and returning `true` for all
  other values. As a special case, `boolean` returns `false` also if
  given instance of host boolean type which represents a
  false value."
  {:added v1
   :tsig (Fn [Boolean Any])
   :category "Primary"
   :see '[Boolean boolean? true? false?]})

;;; Value predicates

(defalias false?
  {:added v1
   :tsig Predicate
   :category "Primary"
   :see '[boolean? true? boolean dunaj.compare/nil?]
   :doc "Returns `true` if `_x_` is `false`, otherwise returns
        `false`.

        IMPORTANT: `false?` returns `false` if `_x_` is `nil`."})

(defalias true?
  {:added v1
   :tsig Predicate
   :category "Primary"
   :see '[boolean? false? boolean dunaj.compare/nil?]
   :doc "Returns `true` if `_x_` is `true`, otherwise returns
        `false`."})

;;; Operations

(defalias not
  {:added v1
   :tsig (Fn [Boolean Any])
   :category "Logic"
   :see '[dunaj.bit/not dunaj.host.int/inot]
   :doc "Returns `true` if `_x_` is logical false (`nil` or `false`),
        otherwise returns `false`.

        .Truth table for not
        [format=\"csv\", options=\"header,autowidth\",cols=\"2*^\"]
        |===
        `*x*`,`*(not x)*`
        `false`,`false`
        `*true*`,`*true*`
        |==="})

(defalias and
  {:added v1
   :tsig Macro
   :category "Logic"
   :see '[nand dunaj.bit/and dunaj.host.int/iand]
   :doc "Evaluates given expressions one at a time, from left to
        right. If a form returns logical false (`nil` or `false`),
        `and` returns that value and doesn’t evaluate any of the
        other expressions, otherwise it returns the value of the
        last expression. `(and)` returns `true`.

        .Truth table for and
        [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
        |===
        `*x*`,`*y*`,`*(and x y)*`
        `false`,`false`,`false`
        `false`,`*true*`,`false`
        `*true*`,`false`,`false`
        `*true*`,`*true*`,`*true*`
        |==="})

(defalias or
  {:added v1
   :tsig Macro
   :category "Logic"
   :see '[nor xor dunaj.bit/or dunaj.host.int/ior]
   :doc "Evaluates given expressions one at a time, from left to
        right. If a form returns a logical true value, `or` returns
        that value and doesn’t evaluate any of the other expressions,
        otherwise it returns the value of the last expression.
        `(or)` returns `nil`.

        .Truth table for or
        [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
        |===
        `*x*`,`*y*`,`*(or x y)*`
        `false`,`false`,`false`
        `false`,`*true*`,`*true*`
        `*true*`,`false`,`*true*`
        `*true*`,`*true*`,`*true*`
        |==="})

(defmacro nand
  "The logical nand operator. Returns the negation of `and`.
  Stops the evaluation of given expressions on first logical false
  value.

  .Truth table for nand
  [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
  |===
  `*x*`,`*y*`,`*(nand x y)*`
  `false`,`false`,`*true*`
  `false`,`*true*`,`*true*`
  `*true*`,`false`,`*true*`
  `*true*`,`*true*`,`false`
  |==="
  {:added v1
   :category "Logic"
   :see '[and nor]}
  [& body]
  `(not (and ~@body)))

(defmacro nor
  "The logical nor operator. Returns the negation of `or`.
  Stops the evaluation of given expressions on first logical true
  value.
  
  .Truth table for nor
  [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
  |===
  `*x*`,`*y*`,`*(nor x y)*`
  `false`,`false`,`*true*`
  `false`,`*true*`,`false`
  `*true*`,`false`,`false`
  `*true*`,`*true*`,`false`
  |==="
  {:added v1
   :category "Logic"
   :see '[or xor xnor]}
  [& body]
  `(not (or ~@body)))

(defn xor
  "Returns the last logical true value if odd number of inputs is
  logical true, otherwise returns a logical false value.
  `(xor)` returns `nil`, `(xor _x_)` returns `_x_`.
  
  .Truth table for xor
  [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
  |===
  `*x*`,`*y*`,`*(xor x y)*`
  `false`,`false`,`false`
  `false`,`*true*`,`*true*`
  `*true*`,`false`,`*true*`
  `*true*`,`*true*`,`false`
  |==="
  {:added v1
   ;; TODO: add inline in order to support primitives
   :tsig (Fn [Any] [Any Any] [Any Any Any] [Any Any Any (Va Any)])
   :category "Logic"
   :see '[or nor xnor]}
  ([] nil)
  ([x] x)
  ([x y] (or (and x (not y)) (and y (not x))))
  ([x y & more]
     (let [hfn
           (fn [count last more]
             (if (seq more)
               (recur (if (first more) (inc count) count)
                      (if (first more) (first more) last)
                      (rest more))
               (and (odd? count) last)))]
       (hfn 0 nil (cons x (cons y more))))))

(defn xnor
  "Returns the negation of `xor`. Note that `xnor` is not supported
  for more than 2 arguments, as logical biconditional is ambiguous
  in such cases. `(xnor)` returns `true`, `(xnor _x_)` is same as
  `(not _x_)`.
  
  .Truth table for xnor
  [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
  |===
  `*x*`,`*y*`,`*(xnor x y)*`
  `false`,`false`,`*true*`
  `false`,`*true*`,`false`
  `*true*`,`false`,`false`
  `*true*`,`*true*`,`*true*`
  |==="
  {:added v1
   ;; TODO: inline for better primitive support
   :tsig (Fn [Boolean] [Boolean Any] [Boolean Any Any])
   :category "Logic"
   :see '[nor or]}
  ([] true)
  ([x] (not x))
  ([x y] (boolean (or (and x y) (and (not y) (not x))))))

(defmacro implication
  "Returns the result of material implication. Note that implication
  is not supported for more than 2 arguments, as implication is
  not associative.
  
  .Truth table for implication
  [format=\"csv\", options=\"header,autowidth\",cols=\"3*^\"]
  |===
  `*x*`,`*y*`,`*(implication x y)*`
  `false`,`false`,`*true*`
  `false`,`*true*`,`*true*`
  `*true*`,`false`,`false`
  `*true*`,`*true*`,`*true*`
  |==="
  {:added v1
   :see '[dunaj.bit/and-not]
   :category "Logic"}
  [x y]
  `(or ~y (not ~x)))


;;;; Testing

(clojure.core/require
 '[clojure.bootstrap :refer [assert-boolean assert-primitive]])

(assert-boolean
 (boolean? "foo")
 (boolean? true)
 (boolean? 3)
 (boolean "foo")
 (boolean 5)
 (boolean true)
 (boolean nil)
 (false? nil)
 (false? true)
 (true? nil)
 (true? false)
 (not nil)
 (not false)
 (not true)
 (not 'f)
 (not 5)
 (and)
 (and true false true)
 (or true false true)
 (nand 3 false false nil)
 (nor nil false nil))

(assert-primitive
 (boolean? "foo")
 (boolean? true)
 (boolean 5)
 (boolean true)
 (boolean nil)
 (false? nil)
 (false? true)
 (true? nil)
 (true? false)
 (not nil)
 (not false)
 (not true)
 (not 5)
 (and)
 ;; (and true false true) ;; issues with AOT
 ;; (or true false true) ;; issues with AOT
 (nand 3 false 'foo nil)
 (nor nil false 'foo nil))
