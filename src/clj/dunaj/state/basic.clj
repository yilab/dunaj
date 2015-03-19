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

(ns dunaj.state.basic
  "Atom, Box, Local and other basic reference types."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [reset! compare-and-set!]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn Va Maybe]]
   [dunaj.host :refer [set!]]
   [dunaj.state :refer [IReference IAtomic IMutable ICloneable]]
   [dunaj.feature :refer [IMeta]]
   [dunaj.poly :refer [deftype]]
   [dunaj.function :refer [defn]]
   [dunaj.concurrent.thread :refer
    [IThreadLocal IPassableThreadLocal
     Thread current-thread ensure-thread-local]]
   [dunaj.state.var :refer [def replace-var! defalias declare]]))


;;;; Public API

(declare atom)

(deftype Atom
  "An atom reference type."
  clojure.lang.Atom
  IMutable
  (-reset! [this val] (reset! this val))
  IAtomic
  (-cas! [this oldval newval] (compare-and-set! this oldval newval))
  ICloneable
  (-clone [this] (atom @this))
  ;; JVM: following protocols are already implemented
  ;; IValidator ;; through extend on c.l.IRef in dunaj.feature
  ;; IMult ;; through extend on c.l.IRef in dunaj.concurrent.port
  ;; IMutableMeta ;; through extend on c.l.IReference in dunaj.feature
  IMeta
  IReference)

(defalias atom
  "Returns a new atom with an initial value of `_x_` and
  zero or more options:

  * `:meta metadata-map` - If supplied, it will become the metadata
    on the atom.
  * `:validator validate-fn` - Must be `nil` or a side-effect-free fn
    of one argument, which will be passed the intended new state on
    any state change. If the new state is unacceptable, the
    validate-fn should return `false` or throw an exception.

  Validator may be also set and managed through its respective
  function in <<dunaj.feature.api.ad#,dunaj.feature>> namespace."
  {:added v1
   :see '[dunaj.state/deref
          dunaj.state/reset! dunaj.state/alter! dunaj.state/cas!]
   :tsig (Fn [Atom Any] [Atom Any (Va Any)])})

(deftype Box
  "An immutable reference type holding given value for situations
  where reference is needed and you have just a value and no
  need to change the referenced value."
  [val :- Any]
  IReference
  (-deref [this] val))

(defn box :- Box
  "Returns a boxed `_x_`, an object which implements IReference.
  Dereferencing such object returns back the original value."
  {:added v1
   :see '[dunaj.state/deref local]}
  [x :- Any]
  (->Box x))

(deftype Raw
  "An unsynchronized mutable reference type for advanced uses.
  You are discouraged to use this type!"
  [^:unsynchronized-mutable val :- Any]
  IReference
  (-deref [this] val)
  ICloneable
  (-clone [this] (->Raw val))
  IMutable
  (-reset! [this newval] (set! val newval) newval))

(defn unsynchronized-reference :- Raw
  "Returns new unsynchronized reference to `_val_`.

  CAUTION: Low level. You are discouraged to use this function."
  {:added v1
   :see '[volatile-reference local]}
  [val :- Any] (->Raw val))

(deftype Volatile
  "Volatile mutable reference type for advanced uses.
  You are discouraged to use this type!"
  [^:volatile-mutable val :- Any]
  IReference
  (-deref [this] val)
  ICloneable
  (-clone [this] (->Volatile val))
  IMutable
  (-reset! [this newval] (set! val newval) newval))

(defn volatile-reference :- Volatile
  "Returns new volatile reference to `_val_`.

  CAUTION: Low level. You are discouraged to use this function."
  {:added v1
   :see '[unsynchronized-reference local]}
  [val :- Any] (->Volatile val))

(deftype Local
  "Thread local mutable reference type."
  [^:volatile-mutable val :- Any,
   ^:volatile-mutable thread :- Thread]
  IMutable
  (-reset! [this newval]
    (ensure-thread-local thread)
    (set! val newval)
    newval)
  IReference
  (-deref [this] val)
  ICloneable
  (-clone [this]
    (->Local val (current-thread)))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this))

(defn local :- Local
  "Returns new reference to `_val_`, local to the given `_thread_`,
  or to the current one, if `_thread_` is not explicitly given.
  `_thread_` can be `nil`, in that case returned reference can be
  mutated from any thread and is effectively a volatile reference.
  The returned reference can be read or cloned from any thread.
  Cloning returns new reference thread local to the caller."
  {:added v1
   :see '[volatile-reference unsynchronized-reference
          dunaj.concurrent.thread/pass!]}
  ([val :- Any]
     (local val (current-thread)))
  ([val :- Any, thread :- (Maybe Thread)]
     (->Local val thread)))
