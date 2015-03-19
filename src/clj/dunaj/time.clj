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

(ns dunaj.time
  "Instants (RFC 3339) and duration protocol."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [var format]]
   [clojure.instant :refer [validated parse-timestamp]]
   [clojure.bootstrap :refer
    [replace-var! def defprotocol deftype defrecord v1 fn defn]]
   [dunaj.type :refer [U Any AnyFn Fn]]
   [dunaj.boolean :refer [Boolean and]]
   [dunaj.math :refer [subtract dec quot neg? INumerical rem num trunc
                       Integer integer? decimal? multiply Decimal]]
   [dunaj.math.precise :as dmp]
   [dunaj.host.number :refer [bigdec long]]
   [dunaj.compare :refer [IHash IEquiv = hash nil?]]
   [dunaj.flow :refer [let if doto cond]]
   [dunaj.threading :refer [->]]
   [dunaj.coll :refer [slice count]]
   [dunaj.string :refer
    [String ->str canonical ICanonical -canonical]]))


;;;; Implementation details

(defn ^:private print-formatted :- String
  [val :- Any, digits :- Integer]
  (let [s (->str "000000000" val)]
    (slice s (subtract (count s) digits))))

(def ^:private construct-calendar :- AnyFn
  @#'clojure.instant/construct-calendar)

(def ^:private thread-local-utc-date-format :- AnyFn
  @#'clojure.instant/thread-local-utc-date-format)

(def ^:private thread-local-utc-timestamp-format :- AnyFn
  @#'clojure.instant/thread-local-utc-timestamp-format)


;;;; Public API

(defprotocol IInstant
  "An abstract type protocol for instants.
  Note that calling `dunaj.math/num` on instant should return number
  of milliseconds from epoch, without time zone information."
  {:added v1
   :see '[dunaj.math/num]
   :predicate 'instant?})

(defprotocol IInstantFactory
  "A factory protocol for instants."
  {:added v1
   :see '[IInstant instant]}
  (-instant
    "Returns a new instant object with given arguments."
    {:tsig (Fn [IInstant IInstantFactory
                Integer Integer Integer Integer Integer
                Integer Integer Integer Integer Integer])}
    [this years months days hours minutes seconds nanoseconds
     offset-sign offset-hours offset-minutes]))

(defn ^:private wrap-instant-factory :- AnyFn
  "Returns a function which can be passed to clojure.instant
  constructors."
  [factory :- IInstantFactory]
  (fn [years months days hours minutes seconds
      nanoseconds offset-sign offset-hours offset-minutes]
    (-instant factory years months days hours minutes seconds
              nanoseconds offset-sign offset-hours offset-minutes)))

(defn ^:private instant*
  "Returns a new instant based on `val` with the help of `factory`."
  [factory :- IInstantFactory, val :- Any]
  (let [val (cond
             (decimal? val)
             (doto (java.sql.Timestamp.
                    (multiply (quot (long val) 1000) 1000))
               (.setNanos
                (long
                 (.movePointRight
                  (.remainder ^java.math.BigDecimal val 1000M) 6))))
             (integer? val) (java.util.Date. (long val))
             :else val)
        val (if (instant? val) (canonical val) val)]
    (-> factory
        wrap-instant-factory
        validated
        (parse-timestamp val))))

;;; Basic instant

(deftype BasicInstant
  "A basic instant type.

  Numerical value returns the number of milliseconds
  from epoch, without time zone information."
  ;; TODO: maybe cache canonical rfc3339 string
  [years months days hours minutes seconds nanoseconds
   offset-sign offset-hours offset-minutes]
  INumerical
  (-numerical [this]
    (let [m (.getTimeInMillis
             ^java.util.Calendar
             (construct-calendar years months days hours minutes
                                 seconds nanoseconds offset-sign
                                 offset-hours offset-minutes))
          bd1 (bigdec m)
          bd2 (.movePointLeft (bigdec (rem nanoseconds 1000000)) 6)]
      (.add bd1 bd2)))
  IInstant
  ICanonical
  (-canonical [this]
    (->str (print-formatted years 4) "-"
           (print-formatted months 2) "-"
           (print-formatted days 2) "T"
           (print-formatted hours 2) ":"
           (print-formatted minutes 2) ":"
           (print-formatted seconds 2) "."
           (print-formatted nanoseconds 9)
           (if (neg? offset-sign) "-" "+")
           (print-formatted offset-hours 2) ":"
           (print-formatted offset-minutes 2)))
  java.lang.Object
  (toString [this] (-canonical this))
  IHash
  (-hash [this] (hash (-canonical this)))
  IEquiv
  (-equiv [this other]
    (and (instant? other) (= (-canonical this) (-canonical other)))))

(defrecord BasicInstantFactory
  "A factory type for basic instant."
  []
  IInstantFactory
  (-instant
    [this years months days hours minutes seconds nanoseconds
     offset-sign offset-hours offset-minutes]
    (->BasicInstant years months days hours minutes seconds
                    nanoseconds offset-sign offset-hours
                    offset-minutes)))

(def basic-instant-factory :- IInstantFactory
  "A factory for basic instants."
  {:added v1
   :see '[instant IIntantFactory]}
  (->BasicInstantFactory))

;;; java.util.Date instant

(deftype DateInstant
  "A `java.util.Date` instant type with millisecond precision and no
  time zone information. Numerical value returns number of
  milliseconds from epoch, without time zone information."
  java.util.Date
  IInstant
  ICanonical
  (-canonical [this]
    (.format ^java.text.SimpleDateFormat
             (.get ^java.lang.ThreadLocal
                   thread-local-utc-date-format) this))
  INumerical
  ;; NB: returns only in millisecond precision
  (-numerical [this] (.getTime this)))

(defrecord DateInstantFactory
  "A `java.util.Date` instant factory type."
  []
  IInstantFactory
  (-instant
    [this years months days hours minutes seconds nanoseconds
     offset-sign offset-hours offset-minutes]
    (.getTime ^java.util.Calendar
              (construct-calendar
               years months days hours minutes seconds nanoseconds
               offset-sign offset-hours offset-minutes))))

(def date-instant-factory :- IInstantFactory
  "A `java.util.Date` instant factory."
  {:added v1
   :see '[instant IIntantFactory]}
  (->DateInstantFactory))

;;; java.util.Calendar instant

(deftype CalendarInstant
  "A `java.util.Calendar` instant type with milisecond precition.
  Numerical value returns number of milliseconds from epoch,
  without time zone information."
  java.util.Calendar
  IInstant
  ICanonical
  (-canonical [this]
    ;; taken from clojure.instant
    (let [calstr (format "%1$tFT%1$tT.%1$tL%1$tz" this)
          offset-minutes (subtract (.length calstr) 2)]
      ;; calstr is almost right,
      ;; but is missing the colon in the offset
      (->str (slice calstr 0 offset-minutes)
             ":" (slice calstr offset-minutes))))
  INumerical
  (-numerical [this] (.getTimeInMillis this)))

(defrecord CalendarInstantFactory
  "A `java.util.Calendar` instant factory type."
  []
  IInstantFactory
  (-instant
    [this years months days hours minutes seconds nanoseconds
     offset-sign offset-hours offset-minutes]
    (construct-calendar
     years months days hours minutes seconds nanoseconds
     offset-sign offset-hours offset-minutes)))

(def calendar-instant-factory :- IInstantFactory
  "A `java.util.Calendar` instant factory."
  {:added v1
   :see '[instant IIntantFactory]}
  (->CalendarInstantFactory))

;;; java.sql.Timestamp instant

(deftype TimestampInstant
  "A `java.sql.Timestamp` instant type without time zone information.
  Numerical value returns number of milliseconds from epoch,
  without time zone information."
  java.sql.Timestamp
  IInstant
  ICanonical
  (-canonical [this]
    (->str
     (.format ^java.text.SimpleDateFormat
              (.get ^java.lang.ThreadLocal
                    thread-local-utc-timestamp-format) this)
     (format ".%09d-00:00" (.getNanos this))))
  INumerical
  (-numerical [this]
    (let [bd1 (bigdec (multiply (quot (.getTime this) 1000) 1000))
          bd2 (.movePointLeft (bigdec (.getNanos this)) 6)]
      (.add bd1 bd2))))

(defrecord TimestampInstantFactory
  "A `java.sql.Timestamp` instant factory type."
  []
  IInstantFactory
  (-instant
    [this years months days hours minutes seconds nanoseconds
     offset-sign offset-hours offset-minutes]
    (doto (java.sql.Timestamp.
           (.getTimeInMillis
            ^java.util.Calendar
            (construct-calendar
             years months days hours minutes seconds 0
             offset-sign offset-hours offset-minutes)))
      ;; nanos must be set separately, passed 0 above
      (.setNanos nanoseconds))))

(def timestamp-instant-factory :- IInstantFactory
  "A `java.sql.Timestamp` instant factory."
  {:added v1
   :see '[instant IIntantFactory]}
  (->TimestampInstantFactory))

;;; Constructor

(def ^:dynamic ^:private *default-instant-factory* :- IInstantFactory
  basic-instant-factory)

(def default-instant-factory :- clojure.lang.Var ;; JVM Host
  "A dynamic var holding default instant factory."
  {:added v1
   :see '[instant IIntantFactory]}
  (var *default-instant-factory*))

(defn instant :- IInstant
  "Returns an instant from `_val_`, which is an instant or a string
  representing instant timestamp.
  Instant type is determined by `_factory_`. Default
  instant factory is stored in `default-instant-factory` var."
  {:added v1
   :see '[instant? now]}
  ([val :- Any] (instant *default-instant-factory* val))
  ([factory :- IInstantFactory, val :- Any] (instant* factory val)))

(defn now :- IInstant
  "Returns an instant representing current time."
  {:added v1
   :see '[instant instant?]}
  [] (java.util.Calendar/getInstance))

;;; Duration

(defprotocol IDuration
  "An abstract type protocol for durations."
  {:added v1
   :see '[milliseconds nanoseconds]
   :predicate 'duration?}
  (-milliseconds :- (U Integer Decimal)
    "Returns number of milliseconds relative to given start
    `_instant_`. If `_before?_` is `true`, uses `_instant_` as an
    end instant. May return decimal."
    [this instant :- IInstant, before? :- Boolean]))

(defn milliseconds :- Integer
  "Returns number of milliseconds from `_duration_` relative to given
  start `_instant_`, which defaults to `(now)`. If `_before?_` is
  `true` (defaults to `false`), uses `_instant_` as an end instant.
  Accepts integer in `_duration_`, and treats it as a number of
  milliseconds."
  {:added v1
   :see '[nanoseconds duration?]}
  ([duration :- (U IDuration Integer)]
   (milliseconds duration (now)))
  ([duration :- (U IDuration Integer), instant :- IInstant]
   (milliseconds duration instant false))
  ([duration :- (U IDuration Integer), instant :- IInstant,
    before? :- Boolean]
   (cond
     (integer? duration) duration
     (nil? duration) 0
     :else (trunc (-milliseconds duration instant before?)))))

(defn nanoseconds :- Integer
  "Returns number of nanoseconds from `_duration_` relative to given
  start `_instant_`, which defaults to `(now)`. If `_before?_` is
  `true` (defaults to `false`), uses `_instant_` as an end instant.
  Accepts integer in `_duration_`, and treats it as a number of
  milliseconds."
  {:added v1
   :see '[milliseconds duration?]}
  ([duration :- (U IDuration Integer)] (nanoseconds duration (now)))
  ([duration :- (U IDuration Integer), instant :- IInstant]
   (nanoseconds duration instant false))
  ([duration :- (U IDuration Integer), instant :- IInstant,
    before? :- Boolean]
   (cond
     (integer? duration) (dmp/multiply 1000000 duration)
     (nil? duration) 0
     :else
     (trunc
      (dmp/multiply 1000000
                    (-milliseconds duration instant before?))))))

(replace-var! clojure.bootstrap/milliseconds)
