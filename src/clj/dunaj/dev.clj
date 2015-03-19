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

(ns dunaj.dev
  "Helpers for developers."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require [clojure.bootstrap :refer [v1]]
            [dunaj.type :refer [Macro]]
            [dunaj.flow :refer [when]]
            [dunaj.function :refer [defn]]
            [dunaj.macro :refer [defmacro]]
            [dunaj.state.var :refer [defalias def reset-root! var]]
            [dunaj.env :refer [pr! color print!]]))


;;;; Implementation details

(def ^:private trace false)


;;;; Public API

(defalias scratch
  "Like `comment`, but parses first form as an input to
  `dunaj.lib/require!`."
  {:added v1
   :see '[dunaj.flow/comment dunaj.lib/require!]
   :tsig Macro
   :highlight :def
   :indent 1}
  clojure.bootstrap/scratch)

(defmacro warn-on-reflection!
  "Starts emiting warnings when reflection is
  needed to resolve host method calls or field accesses."
  {:added v1}
  []
  `(clojure.core/set! clojure.core/*warn-on-reflection* true))

(defmacro not-implemented
  "Macro for cases where some part of code is not implemented yet."
  {:added v1
   :highlight :warning}
  ([] `(throw (java.lang.UnsupportedOperationException.)))
  ([m] `(throw (java.lang.UnsupportedOperationException. ~m))))

(defalias assert
  "Evaluates expr `_x_` and throws an exception with optional
  `_message_` if it does not evaluate to logical true."
  {:added v1
   :see '[toggle-assert assert-primitive]
   :tsig Macro
   :highlight :flow})

(defalias assert-primitive
  "Evaluates given forms and throws if some of them do not yield
  result of primitive type."
  {:added v1
   :see '[toggle-assert assert assert-boolean assert-int pt]
   :tsig Macro
   :highlight :flow}
  clojure.bootstrap/assert-primitive)

(defalias assert-boolean
  "Evaluates given forms and throws if some of them do not yield
  result of primitive boolean type."
  {:added v1
   :see '[toggle-assert assert assert-primitive assert-int]
   :tsig Macro
   :highlight :flow}
  clojure.bootstrap/assert-boolean)

(defalias assert-int
  "Evaluates given forms and throws if some of them do not yield
  result of primitive int type."
  {:added v1
   :see '[toggle-assert assert assert-boolean assert-primitive]
   :tsig Macro
   :highlight :flow}
  clojure.bootstrap/assert-int)

(def toggle-assert :- clojure.lang.Var
  "A dynamic var which toggles assertions."
  {:added v1
   :see '[assert]}
  (var clojure.core/*assert*))

(defalias time
  "Evaluates `_expr_` and prints the time it took.
  Returns the value of `_expr_`."
  {:added v1
   :see '[trace]
   :tsig Macro})

(defn set-trace! :- nil
  "Enables/toggles tracing.
  Used for debugging and development purposes."
  {:added v1
   :see '[trace]}
  ([] (set-trace! true))
  ([val] (reset-root! (var trace) val) nil))

(defmacro trace
  "Prints `_xs_` to the output, if `set-trace!` was called before.
  Used for debugging and development purposes."
  [& xs]
  {:added v1
   :see '[set-trace! time]}
  (when trace `(print! (dunaj.function/apply) ~@xs)))

(defn set-color! :- nil
  "Globally sets a color var so that any print! will use color mode
  by default. Used for debugging and development purposes."
  {:added v1
   :see '[dunaj.env/color dunaj.env/with-color]}
  ([] (set-color! true))
  ([val] (reset-root! color val) nil))

(defalias pt
  "Returns keyword based on type of `_x_`. Used to determine whether
  value is of primitive type or not."
  {:added v1
   :see '[assert-primitive]
   :tsig Macro}
  clojure.bootstrap/pt)
