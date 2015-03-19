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

(ns dunaj.macro
  "Macros. &nbsp; icon:magic[]"
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [defalias v1 not-implemented]]
   [dunaj.type :refer [Fn Any Macro]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.function :refer [defn]]
   [dunaj.string :refer [String]]
   [dunaj.identifier :refer [Symbol]]))


;;;; Public API

(defalias defmacro
  "Defines a var with newly created macro set as a root binding and
  returns that var. Similar to `defn`, but will be used as a macro
  by the compiler when it is called."
  {:added v1
   :arglists
   '([name def-arguments params body]
     [name def-arguments params-w-body & more-params-w-body])
   :inline :all
   :highlight :def
   :see '[macro? dunaj.function/defn macroexpand]
   :tsig Macro}
  clojure.bootstrap/defmacro)

(defn macro? :- Boolean
  "Returns `true` if var `_v_` holds a macro,
  otherwise returns `false`."
  {:added v1
   :see '[defmacro]}
  [v :- clojure.lang.Var]
  (.isMacro v))

(defalias gensym
  "Returns a new symbol with a unique name. If a `_prefix-string_`
  is supplied, the name is `prefix-string#` where `#` is some unique
  number. If prefix is not supplied, the prefix is `G__`."
  {:added v1
   :see '[defmacro]
   :tsig (Fn [Symbol] [Symbol String])})

(defalias macroexpand
  "Repeatedly calls `macroexpand-1` on `_form_` until it no longer
  represents a macro form, then returns it. Note neither
  `macroexpand-1` nor `macroexpand` expand macros in subforms."
  {:added v1
   :see '[macroexpand-all macroexpand-1]
   :tsig (Fn [Any Any])})

(defalias macroexpand-1
  "If `_form_` represents a macro form, returns its expansion,
  else returns `_form_`."
  {:added v1
   :see '[macroexpand macroexpand-all]
   :tsig (Fn [Any Any])})

;; injected in dunaj.coll.util
(defn macroexpand-all :- Any
  "Recursively performs all possible macroexpansions in `_form_`,
  including subforms."
  {:added v1
   :see '[macroexpand macroexpand-1]}
  [form :- Any]
  (not-implemented))
