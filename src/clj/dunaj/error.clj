;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
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

(ns dunaj.error
  "Error handling, exceptions."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Exceptions"]}
  (:api bare)
  (:require
   [clojure.core :refer [extend-protocol map into]]
   [clojure.stacktrace]
   [clojure.bootstrap :refer
    [defprotocol defn defalias def defmacro v1 not-implemented]]
   [dunaj.type :refer [Fn Maybe KeywordMap]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.host :refer [bean->map class]]
   [dunaj.flow :refer [let when]]
   [dunaj.state :refer [IReference]]
   [dunaj.function :refer [fn comp]]
   [dunaj.host.array :refer [adapt]]
   [dunaj.string :refer [String hyphen-case]]
   [dunaj.identifier :refer [INamed keyword namespace name]]))


;;;; Public API

;;; Exception type

(defprotocol IException
  "An abstract type protocol for exceptions."
  {:added v1
   :category "Primary"
   :see '[throw try catch]
   :predicate 'exception?
   :on-interface java.lang.Throwable ;; TODO: or just j.l.Exception?
   :forbid-extensions true})

;; TODO: too clever?
(extend-protocol INamed
  java.lang.Throwable
  (-name [o] (.getName ^java.lang.Class (class o))))

;; TODO: make exception look like a record?

;;; Custom error handling

(defprotocol IErrorHandleable
  "A state protocol for custom error handling of references,
  resources, etc. Corresponding keyword options are
  `:error-handler` and `:error-mode`."
  {:added v1
   :category "Primary"
   :see '[error-handler error-mode]
   :predicate 'error-handleable?}
  (-error-handler :- IReference
    "Returns reference to the current error handler.
    Referenced value is `nil` if no error handler is set."
    [this])
  (-error-mode :- IReference
    "Returns reference to the current error mode.
    Referenced value is `nil` if no error mode is set."
    [this]))

(defn error-handler :- IReference
  "Returns reference to the error handler fn for `_x_`."
  {:added v1
   :category "Primary"
   :see '[error-handleable? error-mode]}
  [x :- IErrorHandleable]
  (-error-handler x))

(defn error-mode :- IReference
  "Returns reference to the error mode for `_x_`."
  {:added v1
   :category "Primary"
   :see '[error-handleable error-handler]}
  [x :- IErrorHandleable]
  (-error-mode x))

;;; Failable protocol

(defprotocol IFailAware
  "An auxiliary state protocol for objects that can be in
  a failed state."
  {:added v1
   :category "Primary"
   :see '[error]
   :predicate 'fail-aware?}
  (-error :- (Maybe IException)
    "Returns exception which has caused `_this_` to fail.
    Returns `nil` if object is not failed."
    [this]))

(defn error :- (Maybe IException)
  "Returns first exception which has caused object `_x_` to fail.
  Returns `nil` if object is not in a failed state."
  {:added v1
   :category "Primary"
   :see '[fail-aware? fail!]}
  [x :- IFailAware]
  (-error x))

(defprotocol IFailable
  "An auxiliary state protocol for object's failed state that can
  be reported from outside."
  {:added v1
   :category "Primary"
   :see '[fail!]}
  (-fail! :- nil
    "Reports a failed state to the failable object. Returns `nil`."
    [this exception :- IException]))

(defn fail! :- nil
  "Reports a failed state to the failable object. Returns `nil`.
  Throws if `_throw?_` is `true`."
  {:added v1
   :category "Primary"
   :see '[error fragile]}
  ([x :- IFailable, exception :- IException]
   (-fail! x exception))
  ([x :- IFailable, exception :- IException, throw? :- Boolean]
   (-fail! x exception)
   (when throw? (clojure.core/throw exception))))

(defmacro fragile
  "Wraps `_body_` in a try-catch which makes `_x_` failed if exception
  occurs."
  {:added v1
   :category "Primary"
   :see '[opened-fragile fail!]}
  [x & body]
  `(clojure.core/try
    ~@body
    (clojure.core/catch java.lang.Exception e# (fail! ~x e# true))))

(defmacro opened-fragile
  "Like fragile, but ensures stateful object `x` is also opened."
  {:added v1
   :category "Primary"
   :see '[fragile dunaj.state/ensure-open]}
  [x & body]
  `(clojure.core/do (dunaj.state/ensure-open ~x) (fragile ~x ~@body)))

;;; Operations

(defn suppressed :- []
  "Returns collection of suppressed exceptions."
  {:added v1
   :category "Primary"
   :highlight :warn}
  [ex :- IException]
  (adapt (.getSuppressed ex)))

(defn cause :- (Maybe IException)
  "Returns cause for given exception, or `nil` if exception does not
  contain a cause."
  {:added v1
   :category "Primary"
   :see '[message root-cause]
   :highlight :warn}
  [ex :- IException]
  (.getCause ex))

(defn message :- (Maybe String)
  "Returns message for given exception, or `nil` if exception does not
  contain a message."
  {:added v1
   :category "Primary"
   :see '[ex-data cause]
   :highlight :warn}
  [ex :- IException]
  (.getMessage ex))

(defalias root-cause
  "Returns the last 'cause' `Throwable` in a chain of `Throwables`."
  {:added v1
   :category "Primary"
   :see '[cause stack-trace]
   :highlight :warn
   :tsig (Fn [(Maybe IException) IException])}
  clojure.stacktrace/root-cause)

(defn stack-trace :- [KeywordMap]
  "Returns collection of frame maps representing given stack trace."
  {:added v1
   :category "Primary"
   :see '[cause root-cause message suppressed]
   :highlight :warn}
  [ex :- IException]
  (let [hyphenate-key
        #(keyword (hyphen-case (namespace %)) (hyphen-case (name %)))
        hyphenate-map
        (fn [m] (into {} (map (fn [[k v]] [(hyphenate-key k) v]) m)))]
    (map (comp hyphenate-map bean->map) (adapt (.getStackTrace ex)))))

;; TODO: formatter for exception, frame map, etc.

;;; Predefined exceptions

(defmacro illegal-argument
  "Returns illegal argument exception optionally with given `_msg_`
  string and `_cause_` exception."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.lang.IllegalArgumentException.))
  ([msg] `(java.lang.IllegalArgumentException. ~msg))
  ([msg cause] `(java.lang.IllegalArgumentException. ~msg ~cause)))

(defmacro illegal-state
  "Returns illegal state exception optionally with given `_msg_`
  string and `_cause_` exception."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.lang.IllegalStateException.))
  ([msg] `(java.lang.IllegalStateException. ~msg))
  ([msg cause] `(java.lang.IllegalStateException. ~msg ~cause)))

(defmacro io
  "Returns IO exception optionally with given `_msg_`
  string and `_cause_` exception."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.io.IOException.))
  ([msg] `(java.io.IOException. ~msg))
  ([msg cause] `(java.io.IOException. ~msg ~cause)))

(defmacro index-out-of-bounds
  "Returns index out of bounds exception optionally with given
  `_msg_` string."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.lang.IndexOutOfBoundsException.))
  ([msg] `(java.lang.IndexOutOfBoundsException. ~msg)))

(defmacro no-such-element
  "Returns no such element exception optionally with given `_msg_`
  string."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.util.NoSuchElementException.))
  ([msg] `(java.util.NoSuchElementException. ~msg)))

(defmacro npe
  "Returns null pointer exception optionally with given `_msg_`
  string."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.lang.NullPointerException.))
  ([msg] `(java.lang.NullPointerException. ~msg)))

(defmacro unsupported-operation
  "Returns unsupported operation exception optionally with given
  `_msg_` string."
  {:added v1
   :see '[throw]
   :category "Exceptions"
   :highlight :warn}
  ([] `(java.lang.UnsupportedOperationException.))
  ([msg] `(java.lang.UnsupportedOperationException. ~msg)))

(defmacro ex-info
  "Create an instance of `ExceptionInfo`, a `RuntimeException`
  subclass that carries a map of additional data."
  {:added v1
   :see '[ex-data]
   :category "Exceptions"}
  ([msg]
   `(clojure.lang.ExceptionInfo. ~msg {}))
  ([msg map]
   `(clojure.lang.ExceptionInfo. ~msg ~map))
  ([msg map cause]
   `(clojure.lang.ExceptionInfo. ~msg ~map ~cause)))

(defalias ex-data
  "Returns exception data (a map) if `_ex_` is an `IExceptionInfo`.
  Otherwise returns `nil`."
  {:added v1
   :category "Exceptions"
   :see '[ex-info]
   :highlight :warn
   :tsig (Fn [{} (Maybe IException)])})

;;; try-catch

(defmacro try
  "The `_exprs_` are evaluated and, if no exceptions occur, the
  value of the last is returned. If an exception occurs and catch
  clauses are provided, each is examined in turn and the first for
  which the thrown exception is an instance of the named class is
  considered a matching catch clause. If there is a matching
  catch clause, its exprs are evaluated in a context in which
  name is bound to the thrown exception, and the value of the
  last is the return value of the function. If there is no matching
  catch clause, the exception propagates out of the function.
  Before returning, normally or abnormally, any finally exprs
  will be evaluated for their side effects."
  {:added v1
   :category "Primary"
   :see '[catch finally throw]
   :indent 0
   :highlight :flow}
  [& exprs]
  `(clojure.core/try ~@exprs))

(defmacro catch
  "A catch clause."
  {:added v1
   :category "Primary"
   :see '[try finally throw]
   :highlight :flow
   :indent 2}
  [ex-type ex & body]
  (not-implemented "catch outside try block."))

(defmacro finally
  "A finally clause."
  {:added v1
   :category "Primary"
   :see '[try catch throw]
   :highlight :flow
   :indent 0}
  [& body]
  (not-implemented "finally outside try block."))

(defmacro throw
  "The `_expr_` is evaluated and thrown, therefore it should yield an
  instance of `IException`."
  {:added v1
   :category "Primary"
   :see '[try catch finally]
   :highlight :flow}
  [expr]
  `(clojure.core/throw ~expr))
