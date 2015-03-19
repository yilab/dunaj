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

(ns dunaj.state
  "Stateful objects. References, mutation, cloning, etc."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Mutation" "Auxiliary"]}
  (:api bare)
  (:require
   [clojure.core :refer
    [recur if apply count when when-not throw reduce partition
     extend-protocol]]
   [clojure.bootstrap :refer [defmacro defprotocol deftype defn def v1
                              replace-var! milliseconds loop let fn]]
   [dunaj.type :refer [Any Fn Va]]
   [dunaj.boolean :refer [Boolean boolean]]
   [dunaj.math :refer [Integer odd?]]
   [dunaj.compare :refer [identical?]]))


;;;; Implementation details

(def ^:private ^:dynamic *io-thread* nil)


;;;; Public API

(defmacro io!
  "If an `io!` block occurs in a transaction, throws an
  `IllegalStateException`, else runs `_body_` in an implicit `do`.
  If the first expression in `_body_` is a literal string,
  will use that as the exception message.

  NOTE: Some mutable functions, (e.g. reducing collections connected
  to live resource) must be run inside `io!` block.
  `io!` block is valid only in the current thread."
  {:added v1
   :see '[ensure-io dunaj.state.ref/dosync dunaj.resource/read!]
   :category "Primary"}
  [& body]
  ;; JVM HOST
  `(clojure.core/with-bindings
     {(clojure.core/var *io-thread*) (java.lang.Thread/currentThread)}
     (clojure.core/io! ~@body)))

(defn ensure-io :- nil
  "Throws if not inside `io!` block. Returns `nil`.
  Is used to assert that the currently executed code is evaluated
  under the `io!` block."
  {:added v1
   :see '[io!]
   :category "Primary"}
  []
  ;; JVM HOST
  (when-not (identical? (java.lang.Thread/currentThread) *io-thread*)
    (throw (java.lang.UnsupportedOperationException.
            "Not inside io! block."))))

;;; Reference

(defprotocol IReference
  "A state protocol for reference objects (references)."
  {:added v1
   :category "Primary"
   :see '[deref blocking-reference?]
   :predicate 'reference?
   :on-interface clojure.lang.IDeref}
  (-deref :- Any
    "Returns the referenced object.
    Blocks if `_this_` also satisfies `IBlockingReference`."
    {:on 'deref}
    [this]))

(defprotocol IBlockingReference
  "A state protocol for blocking references."
  {:added v1
   :category "Primary"
   :see '[deref reference?]
   :predicate 'blocking-reference?
   :on-interface clojure.lang.IBlockingDeref}
  (-deref-for :- Any
    "Returns referenced object or `_timeout-val_`, if `_timeout-ms_`
    is reached. Blocks until value is produced or until timeout is
    reached."
    {:on 'deref}
    [this timeout-ms :- Integer, timeout-val :- Any]))

(defn deref :- Any
  "Returns the current state of `_ref_`. Concrete semantics differ
  based on object's type. Types for which this function block return
  `true` for `blocking-reference?` predicate, and variant with
  `_timeout_` may be used for them."
  {:added v1
   :see '[reference? blocking-reference?]
   :category "Primary"}
  ([ref :- IReference]
     (-deref ref))
  ([ref :- IBlockingReference, timeout :- Any, timeout-val :- Any]
     (-deref-for ref (milliseconds timeout) timeout-val)))

;; Support IReference for Clojure code
(replace-var! clojure.core/deref)

;;; Mutation

(defprotocol IMutable
  "A state protocol for mutable references."
  {:added v1
   :see '[reset!]
   :category "Mutation"
   :predicate 'mutable?}
  (-reset! :- Any
    "Resets the referenced value to `_val_`.
    Returns new value. Mutates `_this_`."
    [this val :- Any]))

(defn reset! :- Any
  "Sets the referenced value to newval without regard
  for the current value. Returns the new value."
  {:added v1
   :see '[mutable? adjustable? atomic? cas!]
   :category "Mutation"}
  [ref :- IMutable, val :- Any]
  (-reset! ref val))

(defprotocol IAdjustable
  "A state protocol for references which can mutate parts of their
  state."
  {:added v1
   :see '[adjust! reset!]
   :category "Mutation"
   :predicate 'adjustable?}
  (-adjust! :- IAdjustable
    "Adjusts part of the state identified by a `_key_` to a given
    `_val_` and returns `_this_`. Throws if `_key_` does not
    corresponds to any part of the state, or the particular part
    cannot be adjusted."
    [this key :- Any, val :- Any]))

(defn adjust! :- IAdjustable
  "Adjusts part of the `_ref_` state identified by a `_key_`
  to a given `_val_` and returns `_ref_`. Throws if `_key_` does not
  corresponds to any part of the state, or the particular part
  cannot be adjusted."
  {:added v1
   :see '[adjustable? reset!]
   :category "Mutation"}
  ([ref :- IAdjustable, key :- Any, val :- Any]
     (-adjust! ref key val))
  ([ref :- IAdjustable, key :- Any, val :- Any & keyvals :- Any]
     (when (odd? (count keyvals))
       (throw (java.lang.IllegalArgumentException.
               "odd number of arguments to adjust!")))
     (reduce (fn [r [k v]] (-adjust! r k v))
             (adjust! ref key val)
             (partition 2 keyvals))))

(defprotocol IAtomic
  "A state protocol for atomic value references."
  {:added v1
   :see '[mutable? cas!]
   :category "Mutation"
   :predicate 'atomic?}
  (-cas! :- Boolean
    "Sets the value to `_newval_` if and only if the current value
    is identical to `_oldval_`. Returns `true` if set happened,
    otherwse returns `false`."
    [this oldval :- Any, newval :- Any]))

(defn cas! :- Boolean
  "Atomically sets the value of the reference `_ref_` to `_newval_` if
  and only if the current value is identical to `_oldval_`.
  Returns `true` if set happened, otherwse returns `false`."
  {:added v1
   :see '[atomic? switch! alter! trade! mutable? reset!]
   :category "Mutation"}
  [ref :- IAtomic, oldval :- Any, newval :- Any]
  (boolean (-cas! ref oldval newval)))

(defn switch! :- Any
  "Sets the value of atomic `_ref_` to newval and `_returns_`
  its previous value."
  {:added v1
   :see '[atomic? cas! alter! trade! mutable? reset!]
   :category "Mutation"}
  [ref :- IAtomic, newval :- Any]
  (loop [oldval @ref]
    (if (-cas! ref oldval newval) oldval (recur @ref))))

(defn alter!
  "Atomically changes the referenced value to be:
  `(apply _f_ current-value _args_)`. `_f_` may be called multiple
  times, and thus should be free of side effects.
  Returns the new value."
  {:added v1
   :see '[atomic? switch! cas! trade! mutable? reset!]
   :category "Mutation"
   :tsig (Fn [Any IAtomic Any]
             [Any IAtomic Any Any]
             [Any IAtomic Any Any Any]
             [Any IAtomic Any Any Any Any]
             [Any IAtomic Any Any Any Any (Va Any)])}
  ([ref f]
   (loop [oldval @ref]
     (let [newval (f oldval)]
       (if (-cas! ref oldval newval) newval (recur @ref)))))
  ([ref f x]
   (loop [oldval @ref]
     (let [newval (f oldval x)]
       (if (-cas! ref oldval newval) newval (recur @ref)))))
  ([ref f x y]
   (loop [oldval @ref]
     (let [newval (f oldval x y)]
       (if (-cas! ref oldval newval) newval (recur @ref)))))
  ([ref f x y z]
   (loop [oldval @ref]
     (let [newval (f oldval x y z)]
       (if (-cas! ref oldval newval) newval (recur @ref)))))
  ([ref f x y z & args]
   (loop [oldval @ref]
     (let [newval (apply f oldval x y z args)]
       (if (-cas! ref oldval newval) newval (recur @ref))))))

(defn trade!
  "Atomically changes the referenced value to be:
  `(apply _f_ current-value _args_)`. `_f_` may be called multiple
  times, and thus should be free of side effects.
  Returns the old value."
  {:added v1
   :see '[atomic? switch! alter! cas! mutable? reset!]
   :category "Mutation"
   :tsig (Fn [Any IAtomic Any]
             [Any IAtomic Any Any]
             [Any IAtomic Any Any Any]
             [Any IAtomic Any Any Any Any]
             [Any IAtomic Any Any Any Any (Va Any)])}
  ([ref f]
   (loop [oldval @ref]
     (let [newval (f oldval)]
       (if (-cas! ref oldval newval) oldval (recur @ref)))))
  ([ref f x]
   (loop [oldval @ref]
     (let [newval (f oldval x)]
       (if (-cas! ref oldval newval) oldval (recur @ref)))))
  ([ref f x y]
   (loop [oldval @ref]
     (let [newval (f oldval x y)]
       (if (-cas! ref oldval newval) oldval (recur @ref)))))
  ([ref f x y z]
   (loop [oldval @ref]
     (let [newval (f oldval x y z)]
       (if (-cas! ref oldval newval) oldval (recur @ref)))))
  ([ref f x y z & args]
   (loop [oldval @ref]
     (let [newval (apply f oldval x y z args)]
       (if (-cas! ref oldval newval) oldval (recur @ref))))))

;;; Auxiliary state

(defprotocol IPending
  "An auxiliary state protocol for objects with pending value."
  {:added v1
   :see '[realized?]
   :category "Auxiliary"
   :predicate 'pending?
   :on-interface clojure.lang.IPending}
  (-realized? :- Boolean
    "Returns `true` when object's value has already been realized."
    {:on 'isRealized}
    [this]))

(defn realized? :- Boolean
  "Returns `true` if the referenced value of `_x_` has been produced,
  otherwise returns `false`."
  {:added v1
   :see '[pending?]
   :category "Auxiliary"}
  [x :- IPending]
  (-realized? x))

;; Support IPending for Clojure code
(replace-var! clojure.core/realized?)

(defprotocol IOpenAware
  "An auxiliary state protocol for objects which can be 'open'.
  Note that there is no IOpenable protocol, see
  <<dunaj.resource.api.ad#,dunaj.resource>>
  on how 'opening' of references is handled in Dunaj."
  {:added v1
   :see '[open? ensure-open]
   :category "Auxiliary"
   :predicate 'open-aware?}
  (-open? :- Boolean
    "Returns `true` if `_this_` is open, otherwise returns `false`."
    [this]))

(defn open? :- Boolean
  "Returns `true` if `_x_` is open, otherwise returns `false`.
  Open is taken in broader sense; if `_x_` is not open, it won't
  perform most operations and is not 'operational'."
  {:added v1
   :see '[open-aware? ensure-open]
   :category "Auxiliary"}
  [x :- IOpenAware]
  (-open? x))

(defn ensure-open :- IOpenAware
  "Throws if `_x_` is not open. Returns `_x_`."
  {:added v1
   :see '[open-aware? open?]
   :category "Auxiliary"}
  [x :- IOpenAware]
  (when-not (open? x)
    (throw (java.lang.IllegalStateException. "Not opened."))))

(defprotocol ICancelledAware
  "An auxiliary state protocol for objects which can be cancelled."
  {:added v1
   :see '[cancelled? cancellable? cancel!]
   :category "Auxiliary"
   :predicate 'cancelled-aware?}
  (-cancelled? :- Boolean
    "Returns `true` if `_this_` is already cancelled, otherwise
    returns `false`."
    [this]))

(defn cancelled? :- Boolean
  "Returns `true` if `_x_` is already cancelled, otherwise returns
  `false`."
  {:added v1
   :see '[cancelled-aware? cancellable? cancel!]
   :category "Auxiliary"}
  [x :- ICancelledAware]
  (-cancelled? x))

(defprotocol ICancellable
  "An auxiliary state protocol for cancellable objects."
  {:added v1
   :see '[cancelled-aware? cancelled? cancel!]
   :category "Auxiliary"
   :predicate 'cancellable?}
  (-cancel! :- Boolean
    "Attempts to cancel `_this_`, returning `true` if cancellation
    was successfull, otherwise returns `false`. Must not block."
    [this]))

(defn cancel! :- Boolean
  "Attempts to cancel the object `_x_`, returning `true` if
  cancellation was successfull, otherwise returns `false`.
  Does not block."
  {:added v1
   :see '[cancelled-aware? cancelled? cancellable?]
   :category "Auxiliary"}
  [x :- ICancellable]
  (-cancel! x))

(defprotocol ICloneable
  "An auxiliaty state protocol for cloneable objects."
  {:added v1
   :see '[clone]
   :category "Primary"}
  (-clone :- ICloneable
    "Returns a cloned `_this_`."
    [this]))

(defn clone :- Any
  "Returns a cloned object, or `_x_`, if it is not cloneable.
  May throw if objects forbids cloning."
  {:added v1
   :category "Primary"}
  [x :- Any]
  (when x (-clone x)))

;; much faster than using 'satisfies?' in clone
(extend-protocol ICloneable
  java.lang.Object
  (-clone [this] this))


;;;; Testing

(clojure.core/require '[clojure.bootstrap :refer [assert-boolean]])

(assert-boolean
 (reference? 3)
 (blocking-reference? 'f)
 (pending? nil))
