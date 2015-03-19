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

(ns dunaj.state.ref
  "Refs, a transactional references that use software transactional
  memory (STM) for safe shared use of mutable references.

  Refs allow mutation to occur only within a transaction.
  Transactions ensure that all actions on Refs are atomic,
  consistent, and isolated.

  IMPORTANT: Transactions are retried in case a conflict arises, so
  the code inside transactions should be I/O and side-effect free.
  Note that agents play nicely with transactions, as sends from
  within a transaction are dispatched only when the transaction
  is successful.

  NOTE: Refs are abstracted into `IRef` protocol,
  as there can be multiple implementations of Refs (e.g. cgrand's
  https://github.com/cgrand/megaref[Megarefs])"
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [doseq keys ref-set ref-min-history
                         ref-max-history ref-history-count]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn AnyFn Va Macro]]
   [dunaj.math :refer [Integer]]
   [dunaj.compare :refer [= IComparable identical?]]
   [dunaj.state :refer [IReference IMutable IAdjustable ICloneable]]
   [dunaj.flow :refer [let do if when-not if-let cond]]
   [dunaj.feature :refer [IMeta IMutableMeta IConfig IMutableConfig]]
   [dunaj.poly :refer [deftype defprotocol]]
   [dunaj.coll :refer [empty? dissoc]]
   [dunaj.function :refer [IInvocable defn]]
   [dunaj.error :refer [throw illegal-argument]]
   [dunaj.state.var :refer [defalias declare]]))


;;;; Public API

(defprotocol IRef
  "An abstract type protocol for Refs."
  {:added v1
   :see '[ref dosync]
   :predicate 'ref?}
  (-ensure :- Any
    "Must be called in a transaction. Protects `_this_` from
    modification by other transactions. Returns the
    in-transaction-value of `_this_`."
    [this])
  (-commute :- Any
    "Must be called in a transaction. Sets the in-transaction-value
    of `_this_` to: `(apply _f_ in-transaction-value-of-ref _args_)`
    and returns the in-transaction-value of `_this_`.
    At the commit point of the transaction, sets the value of
    `_this_` to be:
    `(apply _f_ most-recently-committed-value-of-ref _args_)`"
    [this f :- AnyFn, args :- [Any]])
  (-alter :- Any
    "Must be called in a transaction. Sets the in-transaction-value of
    `_this_` to: `(apply _f_ in-transaction-value-of-ref _args_)`
    and returns the in-transaction-value of `_this_`."
    [this f :- AnyFn, args :- [Any]])
  (-reset :- Any
    "Must be called in a transaction. Sets the value of `_this_`.
    Returns `_val_`."
    [this val :- Any])
  (-history-count :- Integer
    "Returns the history count of `_this_` ref."
    [this]))

(defn ensure :- Any
  "Must be called in a transaction. Protects the `_ref_` from
  modification by other transactions. Returns the
  in-transaction-value of `_ref_`. Allows for more concurrency
  than `(reset _ref_ @_ref_)`."
  {:added v1
   :see '[reset ref dosync]}
  [ref :- IRef]
  (-ensure ref))

(defn commute :- Any
  "Must be called in a transaction. Sets the in-transaction-value
  of `_ref_` to: `(apply _f_ in-transaction-value-of-ref _args_)`
  and returns the in-transaction-value of `_ref_`.
  At the commit point of the transaction, sets the value of `_ref_`
  to be: `(apply _f_ most-recently-committed-value-of-ref _args_)`

  Thus `_f_` should be commutative, or, failing that, you must accept
  last-one-in-wins behavior. `commute` allows for more concurrency
  than `reset`."
  {:added v1
   :see '[alter reset ref dosync]}
  [ref :- IRef, f :- AnyFn & args :- Any]
  (-commute ref f args))

(defn alter :- Any
  "Must be called in a transaction. Sets the in-transaction-value of
  `_ref_` to: `(apply _f_ in-transaction-value-of-ref _args_)`
  and returns the in-transaction-value of `_ref_`."
  {:added v1
   :see '[commute ensure reset ref dosync]}
  [ref :- IRef, f :- AnyFn & args :- Any]
  (-alter ref f args))

(defn reset :- Any
  "Must be called in a transaction. Sets the value of `_ref_`.
  Returns `_val_`."
  {:added v1
   :see '[alter commute ensure ref dosync]}
  [ref :- IRef, val :- Any]
  (-reset ref val))

(defn history-count :- Integer
  "Returns the history count of a `_ref_`. Min and max history
  parameters are stored and can be changed in ref's mutable config."
  {:added v1
   :see '[ref dunaj.feature/config]}
  [ref :- IRef]
  (-history-count ref))

;; Basic Ref type
;; Yes, there can be more than one ref type, e.g. cgrands megaref

(deftype RefConfigRef [r]
  IReference
  (-deref [this]
    {:min-history (ref-min-history r)
     :max-history (ref-max-history r)})
  IMutable
  (-reset! [this conf]
    (when-not (empty? (dissoc conf :min-history :max-history))
      (throw
       (illegal-argument
        "Config must contain :min-history and :max-history only.")))
    (if-let [mh (:min-history conf)]
      (ref-min-history r mh)
      (throw
       (illegal-argument
        "Config must contain :min-history with an integer value.")))
    (if-let [mh (:max-history conf)]
      (ref-max-history r mh)
      (throw
       (illegal-argument
        "Config must contain :max-history with an integer value."))))
  IAdjustable
  (-adjust! [this key val]
    (cond (identical? key :min-history) (ref-min-history r val)
          (identical? key :max-history) (ref-max-history r val)
          :else (throw
                 (illegal-argument
                  "Key must be :min-history or :max-history")))))

(declare ref)

(deftype Ref
  "Ref type."
  clojure.lang.Ref
  IRef
  (-ensure [this] (clojure.core/ensure this))
  (-commute [this f args] (.commute ^clojure.lang.Ref this f args))
  (-alter [this f args] (.alter ^clojure.lang.Ref this f args))
  (-reset [this val] (ref-set this val))
  (-history-count [this] (ref-history-count this))
  IConfig
  (-config [this] {:min-history (ref-min-history this)
                   :max-history (ref-max-history this)})
  IMutableConfig
  (-config-ref [this] (->RefConfigRef this))
  ;; JVM: following protocols are already implemented
  ;; IValidator ;; through extend on c.l.IRef in dunaj.feature
  ;; IMult ;; through extend-protocol on c.l.IRef in dunaj.port
  IMeta
  ;; IMutableMeta ;; through extend on c.l.IReference in dunaj.feature
  ICloneable
  (-clone [this] (ref @this))
  IReference
  IInvocable
  IComparable)

(defalias ref
  "Creates and returns a Ref with an initial value of `_x_` and zero
  or more options:

  * `:meta metadata-map` - If supplied, it will become the metadata
    on the ref.
  * `:validator validate-fn` - must be `nil` or a side-effect-free fn
    of one argument, which will be passed the intended new state on
    any state change. If the new state is unacceptable, the
    validate-fn should return `false` or throw an exception.
    validate-fn will be called on transaction commit, when all
    refs have their final values.
  * `:min-history (default 0) :max-history (default 10)` - Normally
    refs accumulate history dynamically as needed to deal with read
    demands. If you know in advance you will need history you can
    set `:min-history` to ensure it will be available when first
    needed (instead of after a read fault). History is limited,
    and the limit can be set with `:max-history`.

  History parameters can be later changed through ref's mutable
  configuration. Validator may be also set and managed through its
  functions in <<dunaj.feature.api.ad#,dunaj.feature>> namespace."
  {:added v1
   :see '[dosync ref? reset alter commit ensure]
   :tsig (Fn [IRef Any] [IRef Any (Va Any)])})

(defalias dosync
  "Runs the `_exprs_` (in an implicit `do`) in a transaction that
  encompasses `_exprs_` and any nested calls. Starts a transaction
  if none is already running on this thread. Any uncaught exception
  will abort the transaction and flow out of dosync.

  The `_exprs_` may be run more than once, but any effects on Refs
  will be atomic."
  {:added v1
   :see '[ref reset alter commit ensure dunaj.state/io!
          dunaj.concurrent.agent/agent]
   :tsig Macro})
