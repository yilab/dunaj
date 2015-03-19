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

(ns dunaj.flow
  "Control Flow. Conditionals, loops, evaluation."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Conditionals" "Iteration" "Evaluation"]}
  (:api bare)
  (:require
   [clojure.core :refer [locking list list* var throw satisfies?]]
   [clojure.bootstrap :refer
    [defalias defmacro defprotocol deftype defn def v1 replace-var!]]
   [dunaj.type :refer [Macro Fn Va Any]]
   [dunaj.host :refer [set!]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [IReference IPending]]))


;;;; Public API

;;; Lexical scope

(defalias let
  "Evaluates the `_exprs_` in a lexical context in which the symbols
  in the `_binding-forms_` are bound to their respective init-exprs
  or parts therein."
  {:added v1
   :tsig Macro
   :highlight :flow
   :indent 1
   :see '[letfn do]
   :category "Primary"
   :let-bindings true}
  clojure.bootstrap/let)

(defalias letfn
  "fnspec => `(fname [params*] exprs)` or `(fname ([params*] exprs)+)`

  Takes a vector of function specs and a `_body_`, and generates a
  set of bindings of functions to their names. All of the names are
  available in all of the definitions of the functions, as well as
  the `_body_`."
  {:added v1
   :tsig Macro
   :highlight :flow
   :see '[let dunaj.function/fn]
   :indent 1
   :category "Primary"
   :let-bindings true})

;;; Conditionals

(defmacro if
  "Evaluates `_test_`. If not the singular values `nil` or `false`,
  evaluates and yields `_then_`, otherwise, evaluates and yields
  `_else_`. If `_else_` is not supplied it defaults to `nil`.
  All of the other conditionals in are based upon the same logic,
  that is, `nil` and `false` constitute logical falsity,
  and everything else constitutes logical truth, and those
  meanings apply throughout. If tests for primitive boolean values,
  not the boxed ones. If your host supports boxed booleans, do not
  use them in `if`."
  {:added v1
   :indent 1
   :see '[when cond if-let if-not if-some]
   :category "Conditionals"
   :highlight :flow}
  [& args]
  `(clojure.core/if ~@args))

(defalias if-not
  "Evaluates `_test_`. If logical false, evaluates and returns
  `_then_` expr, otherwise `_else_` expr, if supplied, else `nil`."
  {:added v1
   :tsig Macro
   :highlight :flow
   :see '[when-not cond if-let if if-some]
   :category "Conditionals"
   :indent 1})

(defmacro if-let
  "bindings => binding-form test

  If test is logical true, evaluates `_then_` with binding-form bound
  to the value of test, if not, yields `_else_`"
  {:added v1
   :highlight :flow
   :indent 1
   :see '[when-let cond if if-not if-some]
   :category "Conditionals"
   :let-bindings true}
  ([bindings then]
   `(dunaj.flow/if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (let [form (bindings 0) tst (clojure.core/rest bindings)]
     `(dunaj.flow/let [temp# ~@tst]
        (if temp#
          (dunaj.flow/let [~form temp#] ~then)
          ~else)))))

(defalias if-some
  "bindings => binding-form test

  If `_test_` is not `nil`, evaluates `_then_` with binding-form
  bound to the value of test, if not, yields `_else_`."
  {:added v1
   :tsig Macro
   :category "Conditionals"
   :highlight :flow
   :see '[when-some when if cond case dunaj.compare/some?]
   :indent 1
   :let-bindings true})

(defalias when
  "Evaluates `_test_`. If logical true,
  evaluates `_body_` in an implicit `do`."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :see '[if cond when-let when-not when-some]
   :highlight :flow
   :indent 1})

(defalias when-not
  "Evaluates `_test_`. If logical false,
  evaluates `_body_` in an implicit `do`."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :see '[if-not when if cond]
   :highlight :flow
   :indent 1})

(defalias when-let
  "bindings => binding-form test

  When test is `true`, evaluates `_body_` with binding-form bound to
  the value of test."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :highlight :flow
   :see '[if-let when if cond case]
   :indent 1
   :let-bindings true})

(defalias when-some
  "bindings => binding-form test

  When test is not `nil`, evaluates `_body_` with binding-form bound
  to the value of test."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :highlight :flow
   :see '[if-some when if cond case dunaj.compare/some?]
   :indent 1
   :let-bindings true})

(defalias cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time. If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn’t evaluate any of
  the other tests or exprs. `(cond)` returns `nil`."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :highlight :flow
   :see '[condp case if when]
   :indent 0
   :indent-group 2})

(defalias condp
  "Takes a binary predicate `_pred_`, an expression `_expr_`,
  and a set of clauses. Each clause can take the form of either:

  * `test-expr result-expr`
  * `test-expr :>> result-fn` (Note :>> is an ordinary keyword.)

  For each clause, `(_pred_ test-expr expr)` is evaluated.
  If it returns logical true, the clause is a match.
  If a binary clause matches, the result-expr is returned,
  if a ternary clause matches, its result-fn, which must be a unary
  function, is called with the result of the predicate as its
  argument, the result of that call being the return value of
  condp.

  A single default expression can follow the clauses, and its
  value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an
  `IllegalArgumentException` is thrown."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :indent 2
   :see '[cond if when case]
   :indent-group 2
   :highlight :flow})

(defalias case
  "Takes an expression `_e_`, and a set of `_clauses_`.

  Each clause can take the form of either:

  * `test-constant result-expr`
  * `(test-constant1 ...​ test-constantN) result-expr`

  The test-constants are not evaluated. They must be compile-time
  literals, and need not be quoted. If the expression is equal to a
  test-constant, the corresponding result-expr is returned.
  A single default expression can follow the clauses, and its
  value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an
  `IllegalArgumentException` is thrown.

  Unlike `cond` and `condp`, `case` does a constant-time dispatch,
  the clauses are not considered sequentially. All manner of constant
  expressions are acceptable in case, including numbers, strings,
  symbols, keywords, and composites thereof. Note that
  since lists are used to group multiple constants that map to the
  same expression, a vector can be used to match a list if needed.
  The test-constants need not be all of the same type."
  {:added v1
   :category "Conditionals"
   :tsig Macro
   :see '[cond condp if when]
   :highlight :flow})

(defalias comment
  "Ignores body, yields `nil`."
  {:added v1
   :tsig Macro
   :see '[dunaj.dev/scratch]
   :category "Primary"
   :highlight :def
   :indent 0})

;;; Iteration

(defmacro recur
  "Evaluates the `_args_` in order, then, in parallel, rebinds the
  bindings of the recursion point to the values of the `_args_`.
  If the recursion point was a `fn` method, then it rebinds the
  params. If the recursion point was a `loop`, then it rebinds the
  loop bindings. Execution then jumps back to the recursion point.
  The recur expression must match the arity of the recursion point
  exactly. In particular, if the recursion point was the top of a
  variadic fn method, there is no gathering of rest args - a single
  seq (or null) should be passed. recur in other than a tail
  position is an error.

  NOTE: `recur` is the only non-stack-consuming looping construct
  in Clojure. There is no tail-call optimization and the use of
  self-calls for looping of unknown bounds is discouraged. `recur` is
  functional and its use in tail-position is verified by the
  compiler."
  {:added v1
   :see '[loop dunaj.function/fn dunaj.function/trampoline
          dotimes while]
   :category "Iteration"
   :highlight :flow}
  [& args]
  `(clojure.core/recur ~@args))

(defmacro loop
  "Evaluates the `_body_` in a lexical context in which the symbols in
  the `_bindings_` are bound to their respective init-exprs or parts
  therein. Acts as a `recur` target.
  `_bindings_` vector supports type signatures."
  {:added v1
   :category "Iteration"
   :see '[recur while dotimes]
   :indent 1
   :highlight :flow
   :let-bindings true}
  [bindings & body]
  `(clojure.bootstrap/loop ~bindings ~@body))

(defalias dotimes
  "`_bindings_` => name n

  Repeatedly executes `_body_` (presumably for side-effects) with
  `_name_` bound to integers from 0 through `_n_`-1."
  {:added v1
   :category "Iteration"
   :tsig Macro
   :see '[doto while loop]
   :highlight :flow
   :indent 1
   :let-bindings true})

(defalias doto
  "Evaluates `_x_` then calls all of the methods and functions from
  `_forms_` with the value of `_x_` supplied at the front of the
  given arguments (as the second item in the form).
  The `_forms_` are evaluated in order. Returns `_x_`."
  {:added v1
   :category "Primary"
   :see '[let do while dotimes loop]
   :tsig Macro
   :highlight :flow
   :indent 1})

(defalias while
  "Repeatedly executes `_body_` while `_test_` expression is `true`.
  Presumes some side-effect will cause `_test_` to become
  `false`/`nil`. Returns `nil`."
  {:added v1
   :category "Iteration"
   :see '[loop doto dotimes]
   :tsig Macro
   :highlight :flow
   :indent 1})

;;; Evaluation

(defalias eval
  "Evaluates the `_form_` data structure (not text!) and returns
  the result."
  {:added v1
   :see '[quote]
   :category "Evaluation"
   :tsig (Fn [Any Any])})

(defmacro quote
  "Returns the unevaluated `_form_`."
  {:added v1
   :see '[eval]
   :category "Evaluation"
   :highlight :flow}
  [form]
  `(clojure.core/quote ~form))

(defmacro do
  "Evaluates the `_exprs_` in order and returns the value of the
  last. If no expressions are supplied, returns `nil`."
  {:added v1
   :see '[doto let]
   :category "Primary"
   :highlight :flow
   :indent 0}
  [& exprs]
  `(clojure.core/do ~@exprs))

;;; Delayed evaluation

(defprotocol IDelay
  "An abstract type protocol for Delay reference objects."
  {:added v1
   :see '[delay retrying-delay force dunaj.state/realized?
          dunaj.state/deref]
   :category "Evaluation"})

(deftype Delay
  "A delay type. Is used to delay the evaluation of an expression."
  clojure.lang.Delay
  IDelay
  ;; JVM: following protocols are already implemented
  IReference
  IPending)

(deftype RetryingDelay
  "A delay type which retries body on subsequent call if it throwed
  previously."
  [^:volatile-mutable val ^:volatile-mutable f]
  IDelay
  IReference
  (-deref [this]
    ;; double-checked locking
    (if (nil? f)
      val
      (locking this
        (if (nil? f)
          val
          (let [new-val (f)]
            (set! val new-val)
            (set! f nil)
            new-val)))))
  IPending
  (-realized? [this] (nil? f)))

(defalias delay
  "Takes a `_body_` of expressions and yields an `IDelay` object that
  will invoke the `_body_` only the first time it is forced
  (with `force` or `deref`/`@`), and will cache the result and
  return it on all subsequent `force` calls."
  {:added v1
   :category "Evaluation"
   :see '[force retrying-delay dunaj.state/deref IDelay
          dunaj.state/realized?]
   :tsig Macro})

(defmacro retrying-delay
  "Takes a `_body_` of expressions and yields a `IDelay` object that
  will invoke the `_body_` only the first time it is forced
  (with `force` or `deref`/`@`), and will cache the result and
  return it on all subsequent force calls.
  Retries `_body_` on subsequent call if it throwed previously.
  Does not clear locals like normal `delay` does."
  {:added v1
   :see '[force delay dunaj.state/deref IDelay dunaj.state/realized?]
   :category "Evaluation"}
  [& body]
  (list `->RetryingDelay nil (list* `clojure.core/fn* [] body)))

(defn force :- Any
  "If `_x_` is a `IDelay`, returns the (possibly cached) value of its
  expression, else returns `_x_`."
  {:added v1
   :see '[delay retrying-delay dunaj.state/deref IDelay
          dunaj.state/realize?]
   :category "Evaluation"}
  [x :- Any]
  (if (satisfies? IDelay x) @x x))

;; so that clojure code will support retrying delay
(replace-var! clojure.core/force)


;;;; Testing

(clojure.core/require
 '[clojure.core :refer [keyword]]
 '[clojure.bootstrap :refer
   [assert-boolean assert-primitive assert-int]]
 '[dunaj.host.int :refer [iint]]
 '[dunaj.math :refer [==]])

(assert-boolean
 (let [] false)
 ;; (letfn [] false) ;; letfn inside let binding autoboxes, bug?
 (if nil false false)
 (if-not nil false false)
 (if-let [x 3] false false)
 (case true
  false false
  true true)
 (case 3
   1 true
   2 true
   3 false)
 (case (keyword "foo")
   1 false
   2 false
   :foo (== 1 2)
   3 true)
 (do :foo true))

(loop [x 2] (assert-primitive x))

;; use iloop for ints
;; (loop [x (iint 2)] (assert-int x))

;; Issues with AOT
#_(assert-primitive
 ;; (let [] 4) ;; issue with AOT
 ;; (letfn [] false) ;; issue with AOT
 (letfn [] 3)
 (if :foo 1 2)
 (if-not nil 3 4)
 (if-let [x 3] 3 4)
 ;; aaand here:
 (case true
  false 2
  true 3)
 (case 3
   1 1
   2 2
   3 3)
 (case :foo
   1 1
   2 2
   :foo 8
   3 3))

;; Issues with AOT
#_(assert-int
 (let [] (iint 4))
 ;; (letfn [] (iint 4)) ;; boxes to long
 (if :foo (iint 1) (iint 2))
 (if-not nil (iint 3) (iint 4))
 (if-let [x 3] (iint 3) (iint 4))
 (case true
   false (iint 2)
   true (iint 3))
 (case 3
   1 (iint 1)
   2 (iint 2)
   3 (iint 3))
 (case :foo
   1 (iint 1)
   2 (iint 2)
   :foo (iint 8)
   3 (iint 3)))
