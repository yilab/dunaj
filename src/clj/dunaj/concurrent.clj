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

(ns dunaj.concurrent
  "General concurrency facilities, futures, promises."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [var try throw]]
   [clojure.bootstrap :refer [defmacro defalias def v1]]
   [dunaj.type :refer [Fn Any I Macro]]
   [dunaj.boolean :refer [and]]
   [dunaj.host.number :refer [long]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [IReference IBlockingReference ICancellable
                        ICancelledAware IPending ICloneable]]
   [dunaj.flow :refer [let when if]]
   [dunaj.poly :refer [defprotocol extend-type!]]
   [dunaj.coll :refer [empty? first rest several? map?]]
   [dunaj.function :refer [Function apply fn defn]]))


;;;; Implementation details

;; taken from Clojure
(defn ^:private binding-conveyor-fn
  [f]
  (let [frame (clojure.lang.Var/cloneThreadBindingFrame)]
    (fn
      ([]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f))
      ([x]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x))
      ([x y]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y))
      ([x y z]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (f x y z))
      ([x y z & args]
         (clojure.lang.Var/resetThreadBindingFrame frame)
         (apply f x y z args)))))


;;;; Public API

(defalias locking
  {:doc "Executes `_body_` in an implicit `do`, while holding the
        monitor of `_x_`. Will release the monitor of `_x_` in all
        circumstances."
   :added v1
   :tsig Macro})

(defprotocol IFuture
  "An abstract type protocol for futures."
  {:added v1
   :see '[future future-call]
   :predicate 'future?
   :on-interface java.util.concurrent.Future})

(defprotocol IExecutor
  "An abstract type protocol for executors."
  {:added v1
   :see '[execute ITaskExecutor submit]
   :on-interface java.util.concurrent.Executor}
  (-execute :- nil
    "Executes given function `_f_` and returns `nil`."
    {:on 'execute}
    [this f :- Function]))

(defn execute :- nil
  "Executes function `_f_` with `_executor_`. Returns `nil`."
  {:added v1
   :see '[submit IExecutor]}
  [executor :- IExecutor, f :- Function]
  (-execute executor f))

(defprotocol ITaskExecutor
  "An abstract type protocol for task executors."
  {:added v1
   :see '[submit IExecutor execute IFuture]
   :on-interface java.util.concurrent.ExecutorService}
  (-submit :- IFuture
    "Submits a given function `_f_` for execution and returns an
    `IFuture` representing that task."
    {:on 'submit
     :hints '[java.util.concurrent.Callable]}
    [this t :- Function]))

(defn submit :- IFuture
  "Submits a function `_f_` for execution and returns an
  `IFuture` representing that task."
  {:added v1
   :see '[execute ITaskExecutor]}
  [executor :- ITaskExecutor, executable :- Function]
  (-submit executor executable))

;; extending on the interface!
(extend-type! java.util.concurrent.Future
  ICancelledAware
  (-cancelled? [this] (.isCancelled this))
  ICancellable
  (-cancel! [this] (.cancel this true))
  IReference
  (-deref [this] (.get this))
  IPending
  (-realized? [this] (.isDone this))
  ICloneable
  (-clone [this] (throw (java.lang.UnsupportedOperationException.)))
  IBlockingReference
  (-deref-limited [this timeout-ms timeout-val]
    (try (.get ^java.util.concurrent.Future this
               ^long (long timeout-ms)
               java.util.concurrent.TimeUnit/MILLISECONDS)
         (catch java.util.concurrent.TimeoutException e
           timeout-val))))

(def ^:dynamic ^:private *default-future-executor* :- ITaskExecutor
  clojure.lang.Agent/soloExecutor)

(def default-future-executor :- clojure.lang.Var
  "A dynamic var holding default future executor."
  {:added v1
   :see '[future future-call dunaj.concurrent.parallel/pmap
          dunaj.concurrent.parallel/pcalls]}
  (var *default-future-executor*))

(defn future-call :- IFuture
  "Takes a function of no args and yields an `IFuture` object that
  will invoke the function in a given `_executor_` (in another
  thread), and will cache the result and return it on all subsequent
  calls to `deref`/`@`. Uses `default-future-executor` if
  `_executor_` is not specified.
  If the computation has not yet finished, calls to `deref`/`@`
  will block, unless the variant of deref with timeout is used."
  {:added v1
   :see '[future dunaj.state/realized? dunaj.state/deref
          dunaj.concurrent.thread/thread-call
          dunaj.concurrent.thread/daemon-call]}
  ([f :- Function]
     (future-call *default-future-executor* f))
  ([executor :- ITaskExecutor, f :- Function]
     (-submit executor (binding-conveyor-fn f))))

(defmacro future
  "Takes a `_body_` of expressions and yields an `IFuture` object
  that will invoke the body in another thread, and will cache the
  result and return it on all subsequent calls to `deref`/`@`.
  If the computation has not yet finished, calls to `deref`/`@`
  will block, unless the variant of `deref` with timeout is used.

  If first form in a `_body_` is a map, uses it as a config map.
  Current config keys are:

  * `:executor` - A `ITaskExecutor` to be used to execute future
    tasks."
  {:added v1
   :see '[future-call]}
  [& body]
  (let [m (when (and (several? body) (map? (first body)))
            (first body))
        body (if (nil? m) body (rest body))]
    `(future-call (clojure.core/or
                   ~(:executor m)
                   (clojure.core/var-get default-future-executor))
                  (^{:once true} clojure.core/fn* [] ~@body))))

;;; Promise

;; TODO: reimplement promise in terms of channels
;; TODO: what behavior when cloning promise?
(defalias promise
  {:doc "Returns a promise object that can be read with `deref`/`@`,
        and set, once only, with `deliver`. Calls to `deref`/`@`
        prior to delivery will block, unless the variant of deref
        with timeout is used. All subsequent derefs will return the
        same delivered value without blocking."
   :added v1
   :see '[deliver! dunaj.state/realize? dunaj.state/deref
          dunaj.concurrent.port/promise-chan]
   :tsig (Fn [(I IReference IBlockingReference IPending)])})

(defn deliver! :- nil
  "Delivers the supplied `_val_` to the `_promise_`, releasing
  any pending derefs. A subsequent call to deliver on a promise
  will have no effect."
  {:added v1
   :see '[promise dunaj.state/deref dunaj.state/realized?]}
  [promise :- (I IReference IBlockingReference IPending), val :- Any]
  (promise val)
  nil)
