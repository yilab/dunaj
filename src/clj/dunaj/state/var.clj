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

(ns dunaj.state.var
  "Vars icon:inbox[], references with mutable root binding and thread
  local bindings that can be rebound dynamically.

  Derefing a var gets its thread binding, or if it's not bound in
  the current thread, gets its root binding. Var can have its
  root binding unbound."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [var-set constantly throw]]
   [clojure.bootstrap :as cb :refer [defmacro deftype defn v1]]
   [dunaj.type :refer [Fn Any Va AnyFn Maybe Macro]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.state :refer [IReference IMutable ICloneable]]
   [dunaj.flow :refer [when when-let]]
   [dunaj.feature :refer [IMeta]]
   [dunaj.function :refer [IInvocable]]
   [dunaj.string :refer [String]]
   [dunaj.identifier :refer
    [Symbol INamed INamespaced name namespace]]))


;;;; Implementation details

(defn ^:private mname :- (Maybe String)
  [x :- Any]
  (when x (name x)))


;;;; Public API

(deftype Var
  "A type for Vars."
  {:added v1
   :see '[def declare]
   :predicate 'var?}
  clojure.lang.Var
  INamed
  (-name [this] (name (.sym this))) ;; throws on letvar
  INamespaced
  (-namespace [this]
    (when-let [ns (.ns this)]
      (mname (.name ^clojure.lang.Namespace ns))))
  IMutable
  (-reset! [this val] (var-set this val))
  ;; JVM: following protocols are already implemented
  ;; IValidator ;; through extend on c.l.IRef in dunaj.feature
  ;; IMult ;; through extend on c.l.IRef in dunaj.port
  IMeta
  ;; IMutableMeta ;; extend on c.l.IReference in dunaj.feature
  ICloneable
  (-clone [this] (throw (java.lang.UnsupportedOperationException.)))
  IReference
  IInvocable)

;;; Value manipulation

(cb/defalias bound?
  "Returns `true` if all of the vars provided as arguments have any
  bound value, root or thread-local. Implies that deref’ing the
  provided vars will succeed. Returns `true` if no vars are provided."
  {:added v1
   :see '[thread-bound alter-root! reset-root!]
   :tsig (Fn [Boolean (Va Var)])})

(cb/defalias thread-bound?
  "Returns `true` if all of the vars provided as arguments have
  thread-local bindings. Implies that set!'ing the provided vars
  will succeed. Returns `true` if no vars are provided."
  {:added v1
   :see '[bound? dunaj.state/reset!]
   :tsig (Fn [Boolean (Va Var)])})

(cb/defalias alter-root!
  "Atomically alters the root binding of var `_v_` by applying `_f_`
  to its current value plus any `_args_`. Mutates `_var_`."
  {:added v1
   :see '[reset-root! bound?]
   :tsig (Fn [Any Var AnyFn (Va Any)])}
  clojure.core/alter-var-root)

(defn reset-root! :- Any
  "Resets root value of `_var_` to `_val_`.
  Returns new value. Mutates `_var_`."
  {:added v1
   :see '[alter-root! bound? dunaj.state/reset!]}
  [var :- Var, val :- Any]
  (alter-root! var (constantly val)))

;;; Global vars definitions

(cb/defalias def
  "Defines a var and returns it. The var definition process creates
  and interns or locates a global var with given `_name_` and a
  namespace of the value of the current namespace.

  If initial value (last argument besides `_name_`) is supplied,
  it is evaluated, and the root binding of the var is set to the
  resulting value. If initial value is not supplied, the root
  binding of the var is unaffected.

  `def` always applies to the root binding, even if the var is
  thread-bound at the point where def is called. `_name_` is an
  unqualified `Symbol`. Metadata from `_name_` is used as a metadata
  of the var. Rest of arguments (args) are parsed for docstring and
  attribute map, which are merged with vars metadata.

  Throws an exception if `_name_` symbol is already in the namespace
  and not mapped to an interned var."
  {:added v1
   :indent :all
   :highlight :def
   :named true
   :tsig Macro
   :arglists '([name] [name def-arguments init])
   :see '[defonce declare defalias]}
  clojure.bootstrap/def)

(cb/defalias defalias
  "Defines a var with the same root binding and metadata as the
  aliased var.

  Aliased var name is optional and if not provided, it will be
  constructed as `clojure.core/<_name_>`."
  {:added v1
   :indent :all
   :highlight :def
   :named true
   :tsig Macro
   :arglists '([name] [name def-arguments aliased])
   :see '[defonce declare def]}
  clojure.bootstrap/defalias)

(defalias defonce
  "Defines a var, sets root binding if not yet set and returns
  the var."
  {:added v1
   :indent :all
   :highlight :def
   :named true
   :tsig Macro
   :see '[def declare defalias]
   :arglists '([name] [name doc-string? attr-map? init])}
  clojure.bootstrap/defonce)

(defalias declare
  "defs the supplied var names with no bindings,
  useful for making forward declarations."
  {:added v1
   :see '[defonce def defalias]
   :tsig Macro})

;;; Global vars manipulation

(defmacro var
  "The symbol must resolve to a global var, and the Var object itself
  (not its value) is returned.
  The reader macro #'x expands to (var x)."
  {:added v1
   :see '[find-var def]
   :highlight :flow}
  [symbol]
  `(clojure.core/var ~symbol))

(defalias find-var
  "Returns the global var named by the namespace-qualified symbol
  `_sym_`, or `nil` if there is no var with that name."
  {:added v1
   :see '[var def]
   :tsig (Fn [(Maybe Var) Symbol])})

(defalias replace-var!
  "Replaces `_dest_` global var with `_source_` one, assuming source
  var does not contain a macro."
  {:added v1
   :see '[replace-macro!]
   :tsig Macro}
  clojure.bootstrap/replace-var!)

(defmacro replace-macro!
  "Replaces `_dest_` global var with `_source_` one, assuming source
  var contains a macro."
  {:added v1
   :see '[replace-var]}
  ([dest source-var]
   `(clojure.core/alter-var-root
     (clojure.core/var ~dest)
     (clojure.core/constantly @~source-var))))

(defalias bindings
  "Gets a map with the Var/value pairs which is currently in effect
  for the current thread."
  {:added v1
   :see '[with-bindings with-roots letvar]
   :tsig (Fn [{Var Any}])}
  clojure.core/get-thread-bindings)

(defalias with-bindings
  "Takes a map of Var/value pairs. Installs for the given Vars the
  associated values as thread-local bindings. Then executes `_body_`.
  Pops the installed bindings after `_body_` was evaluated.
  Returns the value of `_body_`."
  {:added v1
   :see '[bindings with-roots letvar]
   :tsig Macro})

(defmacro with-roots
  "Temporarily redefines Vars during the execution of `_body_`.
  Each val of binding-map will replaces the root value of its
  key which must be a Var. After the `_body_` is executed, the root
  values of all the Vars will be set back to their old values.
  These temporary changes will be visible in all threads.
  Useful for mocking out functions during testing."
  {:added v1
   :see '[with-bindings bindings letvar]}
  [binding-map & body]
  `(clojure.core/with-redefs-fn ~binding-map
     (clojure.core/fn [] ~@body)))

;;; Local vars

(defalias letvar
  "Executes the `_body_` in a context in which the symbols in
  `_bindings_` vec are bound to vars with per-thread bindings to the
  respective init-exprs. The symbols refer to the var objects
  themselves, and can be accessed with `deref` and `reset!`."
  {:added v1
   :arglists '([bindings & body])
   :see '[with-bindings with-roots bindings def]
   :tsig Macro}
  clojure.core/with-local-vars)
