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

(ns dunaj.function
  "Function definition, transformation and memoization facilities.

  While types other than `Function` may extend
  `<<dunaj.function.spi.ad#IInvocable,IInvocable>>` protocol and be
  invoked in a same way as function is, the `Function` type provides
  additional functionalities besides ability to be invoked:
  
  * any Function can be used as a comparator for sorting
  * support for `IMeta`
  * only Functions can be passed into `IExecutor` and `ITaskExecutor`
  
  IMPORTANT: Type signatures `AnyFn` and `Fn`, found in
  <<dunaj.type.api.ad#,dunaj.type>> namespace, represent any
  invocable object. Use `Function` type signature to require
  Functions created with `defn` or `fn`.
  
  NOTE: When documentation uses the name 'function', an invocable
  object is meant. In those rare cases when only objects of type
  `Function` are required, the 'fn' or 'Function' (with capital F)
  is used.
  
  NOTE: Documentation needs more work."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Transformations" "Memoization"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [throw var satisfies? str]]
   [clojure.bootstrap :as cb :refer [defalias defprotocol def v1]]
   [dunaj.type :refer [Fn Any AnyFn I U Va Predicate Signature Macro]]
   [dunaj.boolean :refer [Boolean and boolean]]
   [dunaj.flow :refer [if cond let if-let]]
   [dunaj.coll :refer
    [ICollectionFactory IConvolutionFactory assoc conj]]
   [dunaj.feature :refer [meta update-meta]]))


;;;; Implementation details

(def ^:private ^:dynamic *default-memoization-factory* nil)


;;;; Public API

(defprotocol IInvocable
  "A feature protocol for function invocation.

  NOTE: JVM host specific: Due to the limit of 20
  arguments, not every method from IFn is defined in IInvocable."
  {:added v1
   :see '[invoke apply]
   :category "Primary"
   :predicate 'invocable?
   :on-interface clojure.lang.IFn
   :forbid-extensions true}
  (-invoke
    "Function invocation."
    {:tsig
     (Fn
      [Any IInvocable]
      [Any IInvocable Any]
      [Any IInvocable Any Any]
      [Any IInvocable Any Any Any]
      [Any IInvocable Any Any Any Any]
      [Any IInvocable Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any Any Any Any Any Any]
      [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
       Any Any Any Any Any Any Any])
     :on 'invoke}
    [o] [o a1] [o a1 a2] [o a1 a2 a3] [o a1 a2 a3 a4]
    [o a1 a2 a3 a4 a5] [o a1 a2 a3 a4 a5 a6] [o a1 a2 a3 a4 a5 a6 a7]
    [o a1 a2 a3 a4 a5 a6 a7 a8] [o a1 a2 a3 a4 a5 a6 a7 a8 a9]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]
    [o a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18
     a19])
  (-apply :- Any
    "Function application."
    {:on 'applyTo}
    [this args :- [Any]]))

(cb/defn invoke
  "Returns the result of invocation of `f` with supplied arguments.
  The use of CLJ syntax `(f a1 a2 ...)` is preferred to using this
  function."
  {:added v1
   :see '[invocable apply]
   :category "Primary"
   :tsig
   (Fn
    [Any IInvocable]
    [Any IInvocable Any]
    [Any IInvocable Any Any]
    [Any IInvocable Any Any Any]
    [Any IInvocable Any Any Any Any]
    [Any IInvocable Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any Any Any Any Any Any]
    [Any IInvocable Any Any Any Any Any Any Any Any Any Any Any Any
     Any Any Any Any Any Any Any])}
  ([f] (f))
  ([f a1] (f a1))
  ([f a1 a2] (f a1 a2))
  ([f a1 a2 a3] (f a1 a2 a3))
  ([f a1 a2 a3 a4] (f a1 a2 a3 a4))
  ([f a1 a2 a3 a4 a5] (f a1 a2 a3 a4 a5))
  ([f a1 a2 a3 a4 a5 a6] (f a1 a2 a3 a4 a5 a6))
  ([f a1 a2 a3 a4 a5 a6 a7] (f a1 a2 a3 a4 a5 a6 a7))
  ([f a1 a2 a3 a4 a5 a6 a7 a8] (f a1 a2 a3 a4 a5 a6 a7 a8))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9] (f a1 a2 a3 a4 a5 a6 a7 a8 a9))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18]
     (f
      a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18))
  ([f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
    a11 a12 a13 a14 a15 a16 a17 a18 a19]
     (f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
        a11 a12 a13 a14 a15 a16 a17 a18 a19)))

(defalias apply
  "Returns the result of applying function `_f_` to the argument list
  formed by prepending intervening arguments to `_args_`."
  {:added v1
   :see '[invocable? invoke fn partial]
   :category "Primary"
   :tsig (Fn [Any AnyFn [Any]]
             [Any AnyFn Any [Any]]
             [Any AnyFn Any Any [Any]]
             [Any AnyFn Any Any Any [Any]]
             [Any AnyFn Any Any Any Any (Va Any)])})

;;; Definition

(def Function :- Signature
  "A type signature for Function.

  Functions provide following functionality:

  * can be invoked
  * can be used as a comparator for sorting
  * can have metadata
  * can be passed into `IExecutor` and `ITaskExecutor`"
  {:added v1
   :see '[dunaj.type/AnyFn dunaj.type/Fn fn defn]
   :category "Primary"}
  clojure.lang.Fn)

(defalias defn
  "Defines a var with newly created fn set as a root binding
  and returns that var."
  {:added v1
   :arglists
   '([name def-arguments params body]
     [name def-arguments params-w-body & more-params-w-body])
   :see '[fn fn?]
   :category "Primary"
   :indent 1
   :highlight :def
   :fn-params true
   :named true
   :tsig Macro}
  clojure.bootstrap/defn)

(defalias fn
  "Returns new fn created from given args."
  {:added v1
   :arglists
   '([optional-name def-arguments params body]
     [optional-name def-arguments params-w-body & more-params-w-body])
   :see '[defn bound-fn fn?]
   :category "Primary"
   :indent 1
   :fn-params true
   :named :optional
   :highlight :flow
   :tsig Macro}
  clojure.bootstrap/fn)

(defalias bound-fn
  "Returns a fn defined by the given `_fntail_`, which will install
  the same bindings in effect as in the thread at the time bound-fn
  was called. This may be used to define a helper function which runs
  on a different thread, but needs the same bindings in place."
  {:added v1
   :see '[bounded defn fn fn?]
   :category "Primary"
   :tsig Macro})

(defalias fn?
  "Returns `true` if `_x_` is a `Function` (not just invocable),
  otherwise returns `false`."
  {:added v1
   :see '[invocable? defn fn bound-fn]
   :category "Primary"
   :tsig Predicate})

;;; Transformation

(defalias trampoline
  "Calls `_f_` with supplied `_args_`, if any. If `_f_` returns a fn,
  calls that fn with no arguments, and continues to repeat, until
  the return value is not a fn, then returns that non-fn value.

  trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption.

  NOTE: If you want to return a fn as a final value, you must wrap
  it in some data structure and unpack it after trampoline returns."
  {:added v1
   :see '[dunaj.flow/loop dunaj.flow/recur]
   :category "Transformations"
   :tsig (Fn [Any AnyFn] [Any AnyFn (Va Any)])})

(defalias partial
  "Takes a function `_f_` and fewer than the normal arguments to
  `_f_`, and returns a fn that takes a variable number of additional
  args. When called, the returned function calls `_f_` with args
  + additional args."
  {:added v1
   :see '[fn]
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn]
             [AnyFn AnyFn Any]
             [AnyFn AnyFn Any Any]
             [AnyFn AnyFn Any Any Any]
             [AnyFn AnyFn Any Any Any (Va Any)])})

(defalias comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns. The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next fn
  (right-to-left) to the result, etc."
  {:added v1
   :see '[juxt]
   :category "Transformations"
   :tsig (Fn [AnyFn]
             [AnyFn AnyFn]
             [AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn (Va AnyFn)])})

(defalias complement
  "Takes a fn `_f_` and returns a fn that takes the same arguments
  as `_f_`, has the same effects, if any, and returns the opposite
  truth value."
  {:added v1
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn])})

(defalias fnil
  "Takes a function `_f_`, and returns a fn that calls `_f_`,
  replacing a `nil` first argument to `_f_` with the supplied value
  `_x_`. Higher arity versions can replace arguments in the second
  and third positions (`_y_`, `_z_`).

  NOTE: The function `_f_` can take any number of arguments,
  not just the one(s) being nil-patched."
  {:added v1
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn Any]
             [AnyFn AnyFn Any Any]
             [AnyFn AnyFn Any Any Any])})

(defalias bounded
  "Returns a fn, which will install the same bindings in
  effect as in the thread at the time bounded was called and
  then call `_f_` with any given arguments. This may be used to
  define a helper function which runs on a different thread,
  but needs the same bindings in place."
  {:added v1
   :see '[bound-fn]
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn])}
  clojure.core/bound-fn*)

(defalias every-pred
  "Takes a set of predicates and returns a fn that
  returns `true` if all of its composing predicates return a logical
  true value against all of its arguments, else it returns `false`.

  NOTE: fn is short-circuiting in that it will stop execution on
  the first argument that triggers a logical false result against
  the original predicates."
  {:added v1
   :see '[some-fn]
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn]
             [AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn (Va AnyFn)])})

(defalias some-fn
  "Takes a set of predicates and returns a fn that
  returns the first logical true value returned by one of its
  composing predicates against any of its arguments, else it returns
  logical false.

  NOTE: fn is short-circuiting in that it will stop execution on
  the first argument that triggers a logical true result against
  the original predicates."
  {:added v1
   :see '[every-pred]
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn]
             [AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn (Va AnyFn)])})

;; TODO: change to return reducible instead of vec?
(defalias juxt
  "Takes a set of functions and returns a fn that is the
  juxtaposition of those fns.

  The returned fn takes a variable number of args, and returns a
  vector containing the result of applying each fn to the args
  (left-to-right).

  `((juxt a b c) x)` = `[(a x) (b x) (c x)]`"
  {:added v1
   :category "Transformations"
   :tsig (Fn [AnyFn AnyFn]
             [AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn]
             [AnyFn AnyFn AnyFn AnyFn (Va AnyFn)])})

(defn monoid :- AnyFn
  "Returns a fn that returns `_init_` when no args are given and calls
  `_f_` with provided arguments if more than one argument is given."
  {:added v1
   :category "Transformations"}
  [f :- AnyFn, init :- Any]
  (fn ([] init)
     ([x y] (f x y))
     ;; Support for multireducibles
     ([x y & more] (apply f x y more))))

;;; General purpose functions

(defalias identity
  "Returns `_x_`."
  {:added v1
   :see '[constantly nop]
   :tsig (Fn [Any Any])
   :category "Primary"})

(defalias constantly
  "Returns a fn that takes any number of arguments and
  returns `_x_`."
  {:added v1
   :see '[identity nop]
   :tsig (Fn [AnyFn Any])
   :category "Primary"})

(defn nop :- nil
  "Takes any number of arguments and returns `nil`."
  {:added v1
   :see '[identity constantly]
   :category "Primary"}
  ([])
  ([x :- Any])
  ([x :- Any, y :- Any])
  ([x :- Any, y :- Any, z :- Any])
  ([x :- Any, y :- Any, z :- Any & more :- Any]))

;;; Memoization

(defprotocol IMemoizationFactory
  "A factory protocol for memoization factories."
  {:added v1
   :see '[memoize]
   :category "Memoization"}
  (-memoize :- AnyFn
    "Returns a memoized version of a function `_f_`."
    [this f :- AnyFn]))

(def default-memoization-factory :- clojure.lang.Var
  "A dynamic var holding default memoization factory."
  {:added v1
   :see '[memoize IMemoizationFactory]
   :category "Memoization"}
  (var *default-memoization-factory*))

(defn memoize :- AnyFn
  "Returns a memoized version of a referentially transparent function
  `_f_`.

  The memoized version of the function keeps a cache of the mapping
  from arguments to results and, when calls with the same arguments
  are repeated often, has higher performance at the expense of
  higher memory use.

  Takes optional `_factory_` parameter. Factory may be
  an implementation of a memoization factory or a collection &
  convolution factory. In later case the factory is assumed to be a
  cache factory and it will be used for a value of cache-factory
  field in the default memoization factory.

  Additional factory optional parameter can be specified at the end
  of the argument list. Default memoization factory is stored in
  `default-memoization-factory` var. Default implementaton of
  memoization factory uses `=` for comparison of args and it
  recognizes optional paramer `:seed` which takes a collection
  of initial cached args-value pairs (a map).

  NOTE: Custom memoization factories are needed if more control
  is needed (e.g. cache snapshots, clearing, swaping, ...)."
  {:added v1
   :see '[default-memoization-factory memoized? unmemoize]
   :category "Memoization"}
  ([f :- AnyFn]
   (memoize *default-memoization-factory* f))
  ([factory :- (U IMemoizationFactory
                  (I IConvolutionFactory ICollectionFactory)),
    f :- AnyFn]
   (let [mf (cond
              (satisfies? IMemoizationFactory factory)
              (-memoize factory f)
              (and (satisfies? IConvolutionFactory factory)
                   (satisfies? ICollectionFactory factory))
              (-memoize (assoc *default-memoization-factory*
                               :cache-factory factory)
                        f)
              :else
              (throw (java.lang.UnsupportedOperationException.
                      (str "Factory must be either a memoization"
                           " or convolution factory."))))]
     (update-meta mf assoc ::original f)))
  ([memoization-factory f & {:as opts}]
   (memoize (conj memoization-factory opts) f)))

(defn memoized? :- Boolean
  "Returns `true` if `_f_` is memoized, otherwise returns `false`."
  {:added v1
   :see '[memoize unmemoize]
   :category "Memoization"}
  [f :- AnyFn]
  (boolean (::original (meta f))))

(defn unmemoize :- AnyFn
  "Returns original unmemoized function from `_f_`.
  Returns `_f_` if it is not memoized."
  {:added v1
   :see '[memoize memoized?]
   :category "Memoization"}
  [f :- AnyFn]
  (if-let [o (::original (meta f))] o f))
