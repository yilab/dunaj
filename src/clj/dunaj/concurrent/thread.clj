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

(ns dunaj.concurrent.thread
  "Concurrency primitives for creating and managing threads.

  Besides normal (user) threads, this namespace provides facilities
  for creating daemon threads, which are threads on which JVM doesn't
  wait when exiting."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [defmacro def v1]]
   [dunaj.type :refer [Any KeywordMap Maybe U AnyFn]]
   [dunaj.boolean :refer [Boolean and not xor]]
   [dunaj.math :refer [Integer zero? rem]]
   [dunaj.host.number :refer [int long]]
   [dunaj.compare :refer [not= identical? nil?]]
   [dunaj.state :refer
    [IReference IOpenAware IMutable IAdjustable open?]]
   [dunaj.flow :refer [when-not let when if doto when-let cond]]
   [dunaj.feature :refer [IConfig IMutableConfig config]]
   [dunaj.poly :refer [defprotocol deftype]]
   [dunaj.coll :refer
    [empty? several? first rest contains? dissoc map? assoc]]
   [dunaj.function :refer [fn defn]]
   [dunaj.string :refer [string? String]]
   [dunaj.time :as dt :refer [IDuration milliseconds nanoseconds]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.error :refer [throw illegal-argument]]))


;;;; Implementation details

(def ^:private state->keyword :- {Any Keyword}
  {java.lang.Thread$State/NEW :new
   java.lang.Thread$State/BLOCKED :blocked
   java.lang.Thread$State/RUNNABLE :runnable
   java.lang.Thread$State/TERMINATED :terminated
   java.lang.Thread$State/TIMED_WAITING :timed-waiting
   java.lang.Thread$State/WAITING :waiting})

(defn ^:private opts-body :- [{} []]
  [body :- []]
  (let [opts (when (several? body)
               (let [f (first body)]
                 (cond (string? f) {:name f}
                       (map? f) f
                       :else nil)))
        body (if (nil? opts) body (rest body))]
    [opts body]))


;;;; Public API

(def ^:const max-priority :- Integer
  "A thread maximum priority constant."
  {:added v1
   :see '[min-priority default-priority thread Thread]}
  java.lang.Thread/MAX_PRIORITY)

(def ^:const min-priority :- Integer
  "A thread minimum priority constant."
  {:added v1
   :see '[max-priority default-priority thread Thread]}
  java.lang.Thread/MIN_PRIORITY)

(def ^:const default-priority :- Integer
  "A thread default priority constant."
  {:added v1
   :see '[min-priority max-priority thread Thread]}
  java.lang.Thread/NORM_PRIORITY)

(deftype ThreadConfigRef
  [thread :- java.lang.Thread]
  IReference
  (-deref [this] (config thread))
  IMutable
  (-reset! [this val]
    (when-not (empty? (dissoc val :name :priority :daemon))
      (throw
       (illegal-argument
        "Thread config must have :name, :priority and :daemon keys")))
    (.setPriority thread (:priority val))
    (.setName thread (:name val))
    (when (xor (:daemon val) (.isDaemon thread))
      (throw (illegal-argument ":daemon property cannot be changed")))
    val)
  IAdjustable
  (-adjust! [this key val]
    (cond (identical? key :name) (.setName thread val)
          (identical? key :priority) (.setPriority thread val)
          (identical? key :daemon)
          (when (xor val (.isDaemon thread))
            (throw (illegal-argument
                    ":daemon property cannot be changed")))
          :else (throw (illegal-argument
                        "Key must be :name or :priority")))
    this))

(deftype Thread
  "A type for thread.

  Thread priority and name can be controlled with mutable and
  ajustable configuration, which is attached to the thread object."
  {:added v1
   :see '[thread current-thread daemon dunaj.concurrent.port/thread
          dunaj.concurrent/future dunaj.feature/config-ref
          dunaj.concurrent.agent/agent dunaj.state/adjust!]
   :predicate 'thread?}
  java.lang.Thread
  IConfig
  (-config [this]
    {:name (.getName this)
     :priority (.getPriority this)
     :daemon (.isDaemon this)})
  IMutableConfig
  (-config-ref [this] (->ThreadConfigRef this))
  IOpenAware
  (-open? [this] (.isAlive this)))

(defprotocol IThreadLocal
  "A marker protocol for thread local objects. A thread local object
  can be accessed only from its owning thread, which is usually
  a thread that created the object."
  {:added v1
   :see '[thread IPassableThreadLocal pass! ensure-thread-local]
   :predicate 'thread-local?})

(defprotocol IPassableThreadLocal
  "A protocol for thread local objects that support changing their
  owning thread."
  {:added v1
   :see '[IThreadLocal pass! ensure-thread-local]
   :predicate 'passable-thread-local?}
  (-pass! :- IPassableThreadLocal
    "Returns a new object similar to `_this_` that has changed
    its thread ownership to the `_new-thread_`.
    Must be called in the thread that owns the `_this_` object.

    NOTE: Per convention, `_this_` object will not be used again in
    any way after call to this method,
    users will use returned one instead.

    NOTE: `_new-thread_` may be `nil` which indicates that
    object will not be owned and can be accessed from any thread."
    [this new-thread :- (Maybe Thread)]))

(defn pass! :- IPassableThreadLocal
  "Returns a new object similar to `_x_` that has changed
  its thread ownership to the `_new-thread_`. Must be called in the
  thread that owns the `_x_` object. Throws if not called in the
  owning thread. If the `_new-thread_` is `nil`, object won't be
  owned and can be accessed from any thread.

  CAUTION: `_x_` object must not be used again in any way after
  this method, returned one should be used instead."
  {:added v1
   :see '[ensure-thread-local current-thread thread]}
  [x :- IPassableThreadLocal, new-thread :- (Maybe Thread)]
  (-pass! x new-thread))

(defn current-thread :- Thread
  "Returns current thread."
  {:added v1
   :see '[ensure-thread-local thread]}
  []
  (java.lang.Thread/currentThread))

(defn ensure-thread-local :- nil
  "Throws if `_thread_` is not current thread. Returns `nil`.
  Is used to assert that operation is run only in a given `_thread_`."
  {:added v1
   :see '[current-thread thread pass!]}
  [thread :- Thread]
  (when thread
    (when-not (identical? thread (java.lang.Thread/currentThread))
      (throw (java.lang.UnsupportedOperationException.
              "Operation is thread local.")))))

(defn interrupt! :- nil
  "Interrupts `_thread_`. Returns `nil`.

  IMPORTANT: Code running in `_thread_` must support the interuption
  logic, which is that `interrupted?` predicate returns `true` inside
  interrupted threads and all blocking operations on interrupted
  thread throw interruption related exception."
  {:added v1
   :see '[interrupted? thread]}
  [thread :- Thread]
  (.interrupt thread))

(defn interrupted? :- Boolean
  "Returns `true` if current thread was interrupted, otherwise returns
  `false`. Thread can be interrupted e.g. by cancelling a future.

  NOTE: JVM host specific: Interruption may also manifest itself as
  `InterruptedException` when e.g. thread is waiting or sleeping."
  {:added v1
   :see '[interrupt! current-thread sleep join status]}
  []
  (.isInterrupted (current-thread)))

(defn status :- Keyword
  "Returns the thread status. Possible values are `:new`, `:blocked`,
  `:runnable`, `:terminated`, `:timed-waiting` and `:waiting`."
  {:added v1
   :see '[interrupt! thread daemon]}
  [thread :- Thread]
  (state->keyword (.getState thread)))

(defn join :- Boolean
  "Waits for a `_thread_` to die. Optional `_timeout_` can be
  specified. Returns `false` if returning due to `_timeout_`,
  `true` otherwise. Throws if interrupted."
  {:added v1
   :see '[thread sleep daemon interrupt!]}
  ([thread :- Thread]
     (join thread 0))
  ([thread :- Thread, timeout :- (U IDuration Integer)]
     (let [ms (long (milliseconds timeout))
           ns (int (rem (nanoseconds timeout) 1000000))]
       (if (and (zero? ms) (zero? ns))
         (.join thread)
         (.join thread ms ns))
       (not (open? thread)))))

(defn sleep :- nil
  "Waits for a specified `_duration_`. Returns `nil`.
  Throws if interrupted."
  {:added v1
   :see '[interrupt! join thread daemon]}
  [duration :- (U IDuration Integer)]
  (let [ms (long (milliseconds duration))
        ns (int (rem (nanoseconds duration) 1000000))]
    (java.lang.Thread/sleep ms ns)))

(defn thread-call :- Thread
  "Executes `_f_` in another thread, returning immediately to the
  calling thread. Returns thread. May supply thread name or options
  map with `:name`, `:priority` and `:daemon` as allowed keys."
  {:added v1
   :see '[thread daemon-call dunaj.concurrent.port/thread-call]}
  ([f :- AnyFn] (thread-call f nil))
  ([f :- AnyFn, name-or-opts :- (U String KeywordMap)]
     ;; this is faster than dunaj.function/bounded
     (let [opts (if (string? name-or-opts)
                  {:name name-or-opts} name-or-opts)
           binds (clojure.lang.Var/getThreadBindingFrame)
           tf (fn []
                (clojure.lang.Var/resetThreadBindingFrame binds)
                (f))
           t (java.lang.Thread. ^java.lang.Runnable tf)]
       (when-let [n (:name opts)] (.setName t n))
       (when-let [p (:priority opts)] (.setPriority t p))
       (cond (:daemon opts) (.setDaemon t true)
             (contains? opts :daemon) (.setDaemon t false))
       (.start t)
       t)))

(defmacro thread
  "Executes `_body_` in another thread, returning immediately to the
  calling thread. Returns thread. First form in `_body_` may be a
  thread name or options map with `:name`, `:priority` and `:daemon`
  as allowed keys."
  {:added v1
   :see '[thread-call daemon dunaj.concurrent.port/thread]}
  [& body]
  (let [[opts body] (opts-body body)]
    `(thread-call (^{:once true} clojure.core/fn* [] ~@body) ~opts)))

(defn daemon-call :- Thread
  "Executes `_f_` in new daemon thread returning immediately to the
  calling thread. May supply thread name or options map with `:name`
  and `:priority` as allowed keys. Returns deamon thread."
  {:added v1
   :see '[daemon thread-call]}
  ([f :- AnyFn] (thread-call f {:daemon true}))
  ([f :- AnyFn, name-or-opts :- (U String KeywordMap)]
     (let [opts (if (string? name-or-opts)
                  {:name name-or-opts :daemon true}
                  (assoc name-or-opts :daemon true))]
       (thread-call f opts))))

(defmacro daemon
  "Executes `_body_` in a new daemon thread, returning
  immediately to the calling thread. Returns daemon thread.
  First form in `_body_` may be a thread name or options map with
  `:name` and `:priority` as allowed keys."
  {:added v1
   :see '[daemon-call thread]}
  [& body]
  (let [[opts body] (opts-body body)
        opts (assoc opts :daemon true)]
    `(thread-call (^{:once true} clojure.core/fn* [] ~@body) ~opts)))
