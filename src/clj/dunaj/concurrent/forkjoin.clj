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

(ns dunaj.concurrent.forkjoin
  "Parallel computations based on a Fork-join model.

  See <<dunaj.concurrent.parallel.api.ad#,dunaj.concurrent.parallel>>
  for parallel computations based on thread pools.
  See <<dunaj.concurrent.port.api.ad#pipeline_BANG_,pipeline!>>
  if you want to work with channels."
  {:authors ["Jozef Wagner"]
   :categories
   ["Primary"
    ["Folds" "Additional helper functions for folds can be found at
             <<dunaj.coll.helper.api.ad#Folds,dunaj.coll.helper>>."]]}
  (:api bare)
  (:require [clojure.core :refer [var throw]]
            [clojure.core.reducers :refer [coll-fold]]
            [clojure.bootstrap :refer [def v1 replace-var!]]
            [dunaj.type :refer [Fn Any AnyFn U Maybe]]
            [dunaj.boolean :refer [and or not Boolean true?]]
            [dunaj.host :refer [class-instance?]]
            [dunaj.math :refer [Integer]]
            [dunaj.compare :refer [nil? identical?]]
            [dunaj.flow :refer [if do let cond if-not when]]
            [dunaj.threading :refer [->>]]
            [dunaj.poly :refer
             [defprotocol deftype extend-protocol! satisfies?]]
            [dunaj.coll :refer [IRed IReducing]]
            [dunaj.function :refer [Function defn apply fn]]
            [dunaj.concurrent :refer
             [IFuture IExecutor ITaskExecutor submit]]))


;;;; Implementation details

(def ^:dynamic ^:private *default-fold-size* :- Integer
  512)

(def ^:dynamic ^:private *default-fold-pool*
  :- java.util.concurrent.ForkJoinPool
  ;;(java.util.concurrent.ForkJoinPool.)
  @clojure.core.reducers/pool)

(def ^:private reducing-function :- AnyFn
  @#'dunaj.coll/reducing-function)

(def ^:private reduce* :- AnyFn
  @#'dunaj.coll/reduce*)


;;;; Public API

(deftype ForkJoinTask
  "A forkjoin task type."
  {:added v1
   :see '[ForkJoinPool inside-forkjoin? adapt fork join]
   :category "Primary"}
  java.util.concurrent.ForkJoinTask
  ;; JVM: extended on the java.util.concurrent.Future interface!
  ;; IReference
  ;; IBlockingReference
  ;; IPending
  ;; ICancellable
  ;; ICancelledAware
  ;; JVM: following protocols are already implemented
  IFuture)

(deftype ForkJoinPool
  "A forkjoin pool type."
  {:added v1
   :see '[ForkJoinTask invoke]
   :category "Primary"}
  java.util.concurrent.ForkJoinPool
  ;; JVM: following protocols are already implemented
  IExecutor
  ITaskExecutor)

(defn inside-forkjoin? :- Boolean
  "Returns `true` if the current thread is executing as a
  `ForkJoinPool` computation, otherwise returns `false`."
  {:added v1
   :see '[ForkJoinPool ForkJoinTask invoke]
   :category "Primary"}
  []
  (java.util.concurrent.ForkJoinTask/inForkJoinPool))

;;; Forkjoin task

(defn adapt :- ForkJoinTask
  "Returns forkjoin task adapted from a function `_f_`."
  {:added v1
   :see '[fork join invoke]
   :category "Primary"}
  [f :- Function]
  (java.util.concurrent.ForkJoinTask/adapt
   ^java.util.concurrent.Callable f))

(defn fork :- ForkJoinTask
  "Arranges to asynchronously execute `_t_`, it being a function
  or a forkjoin task. Returns forked task.

  IMPORTANT: While not enforced or checked, the idiomatic use is
  to always call this function from inside the forkjoin pool (i.e.
  when running in another forkjoin task)."
  {:added v1
   :see '[adapt join invoke]
   :category "Primary"}
  [t :- (U ForkJoinTask Function)]
  (let [t (if (class-instance? java.util.concurrent.ForkJoinTask t)
            t
            (adapt t))]
    (.fork ^java.util.concurrent.ForkJoinTask t)))

(defn join :- Any
  "Returns the result of the computation task `_t_` when it is done.

  NOTE: JVM host specific: Throws `RuntimeException` or `Error`, not
  `ExecutionException`, if task has finished abnormally. Moreover,
  interrupts of the calling thread do not cause the method to
  abruptly return by throwing `InterruptedException`."
  {:added v1
   :see '[fork adapt invoke]
   :category "Primary"}
  [t :- ForkJoinTask]
  (.join t))

;;; Convenience

(defn invoke :- Any
  "Invokes `_f_` on the given `_pool_`, returning the result of the
  computation. Blocks until computation is finished.
  If the current thread is already in a fork join pool, invokes
  computation in the current thread."
  {:added v1
   :see '[fork join adapt inside-forkjoin?]
   :category "Primary"}
  [pool :- ForkJoinPool, ^java.util.concurrent.Callable f :- Function]
  (if (inside-forkjoin?) (f) (join (submit pool f))))

;;; Foldable collection

(defprotocol IFoldable
  "A value protocol for foldable collections."
  {:added v1
   :see '[fold transfold dunaj.coll/reduce]
   :category "Folds"}
  (-fold
    "Returns the reduction of `_this_` with `_reducef_` and with
    `(combinef)` as a starting value. Uses a potentially parallel
    reduce-combine strategy (dividing while length is greater than
    `_n_`, reducing divided chunks with `_reducef_` and combining
    partial results with `_combinef_`), with `_reduce-fn_` specifying
    the reduction function (e.g. `reduce*`). Uses forkjoin `_pool_`
    for the execution of tasks."
    [this reduce-fn :- AnyFn, pool :- ForkJoinPool, n :- Integer,
     combinef :- AnyFn, reducef :- AnyFn]))

(def default-fold-size :- clojure.lang.Var
  "A dynamic var holding a default fold size."
  {:added v1
   :see '[fold transfold]
   :category "Folds"}
  (var *default-fold-size*))

(def default-fold-pool :- clojure.lang.Var
  "A dynamic var holding a default forkjoin pool."
  {:added v1
   :see '[dunaj.coll.helper/fold* dunaj.coll.helper/transfold*]
   :category "Folds"}
  (var *default-fold-pool*))

(deftype Folding
  "An augmented reducing type which uses `_reducef_` for reducing
  steps and `_combinef_` for an initial value and for combining."
  [combinef :- AnyFn, reducef :- AnyFn]
  IReducing
  (-init [this] (combinef))
  (-finish [this wrap] wrap)
  (-wrap [this ret] ret)
  (-unwrap [this wrap] wrap)
  (-step [this wrap val] (reducef wrap val))
  (-step [this wrap val val2] (reducef wrap val val2))
  (-step [this wrap val val2 val3] (reducef wrap val val2 val3))
  (-step [this wrap val val2 val3 val4]
    (reducef wrap val val2 val3 val4))
  (-step [this wrap val val2 val3 val4 more]
    (apply reducef wrap val val2 val3 val4 more))
  (-combine [this wrap other] (combinef wrap other)))

(defn folding :- IReducing
  "Returns an augmented reducing function which uses `_reducef_` for
  reducing steps and `_combinef_` for initial value and for
  combining. Uses `_reducef_` for everything if `_combinef_`
  is not given."
  {:added v1
   :see '[fold-augmented transfold dunaj.coll/reducing]
   :category "Folds"}
  ([reducef :- AnyFn]
   (->Folding reducef reducef))
  ([combinef :- AnyFn, reducef :- AnyFn]
   (->Folding combinef reducef)))

(defn ^:private fold* :- Any
  "Like fold, but with customizable `_pool_` and `_reduce-fn_`."
  ([coll :- [], reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn]
   (let [reduce-fn (or reduce-fn reduce*)]
     (if-not (satisfies? IFoldable coll)
       ;; legacy collections accept classic reduce only
       (cond (nil? coll) (combinef)
             (identical? reduce* reduce-fn)
             (coll-fold coll n combinef reducef)
             :else (throw (java.lang.UnsupportedOperationException.)))
       (-fold coll reduce-fn (or pool *default-fold-pool*)
              n combinef reducef)))))

(defn ^:private fold-augmented* :- Any
  "Returns a result of the folding of `_coll_` with `_reduce-fn_`
  as a reduction function taking `_coll_` `reducef` `init` in that
  order, and with the augmented reducing function `_r_`.
  Does not return reduced nor postponed object."
  [coll :- [], reduce-fn :- AnyFn, pool :- ForkJoinPool,
   n :- Integer, r :- IReducing]
  (let [reducef (reducing-function r)
        combinef (fn
                   ([] (._wrap r (._init r)))
                   ([wrap other] (._combine r wrap other)))]
    (->> (fold* coll reduce-fn pool n combinef reducef)
         (._finish r))))

(defn ^:private transfold* :- Any
  ([coll :- IRed, reduce-fn, :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, xform :- AnyFn, reducef :- AnyFn]
   (fold-augmented*
    coll reduce-fn pool n (xform (folding reducef))))
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, xform :- AnyFn, combinef :- AnyFn, reducef :- AnyFn]
   (fold-augmented*
    coll reduce-fn pool n (xform (folding combinef reducef)))))

(defn fold :- Any
  "Returns the reduction of `_coll_` with `_reducef_` and with
  `(combinef)` as a starting value. Uses a potentially parallel
  reduce-combine strategy. The collection is partitioned into groups
  of approximately `_n_` (default to `default-fold-size`),
  each of which is reduced with `_reducef_`
  (with a seed value obtained by calling `(combinef)` with
  no arguments). The results of these reductions are then reduced
  with `_combinef_` (default `_reducef_`).
  Uses `default-fold-pool` as a forkjoin pool.

  IMPORTANT: `_combinef_` must be associative,
  and, when called with no arguments, `(combinef)` must produce its
  identity element. These operations may be performed in parallel,
  but the results will preserve order."
  {:added v1
   :see '[fold-augmented folding transfold dunaj.coll/reduce
          dunaj.coll.helper/fold*]
   :category "Folds"}
  ([reducef :- AnyFn, coll :- []]
   (fold reducef reducef coll))
  ([combinef :- AnyFn, reducef :- AnyFn, coll :- []]
   (fold *default-fold-size* combinef reducef coll))
  ([n :- Integer, combinef :- AnyFn, reducef :- AnyFn, coll :- []]
   (fold* coll nil nil n combinef reducef)))

(defn fold-augmented :- Any
  "Returns the result of folding of `_coll_` with augmented reducing
  function `_r_`. Uses a potentially parallel reduce-combine
  strategy. The collection is partitioned into groups of
  approximately `_n_` (default to `default-fold-size`).
  Uses `default-fold-pool` as a forkjoin pool."
  {:added v1
   :see '[fold folding transfold dunaj.coll/reduce-augmented
          dunaj.coll.helper/fold-augmented*]
   :category "Folds"}
  ([r :- IReducing, coll :- []]
   (fold-augmented *default-fold-size* r coll))
  ([n :- Integer, r :- IReducing, coll :- []]
   (fold-augmented* coll nil nil n r)))

(defn transfold :- Any
  "Returns the result of the folding of `_coll_`
  with the classic reducing function `_reducef_`, combining function
  `_combinef_` (uses `_reducef_` if not given), and transducer
  `_xform_`. Uses `default-fold-pool` as a forkjoin pool."
  {:added v1
   :see '[fold fold-augmented dunaj.coll/transduce
          dunaj.coll.helper/transfold*]
   :category "Folds"}
  ([xform :- AnyFn, reducef :- AnyFn, coll :- []]
   (fold-augmented (xform (folding reducef)) coll))
  ([xform :- AnyFn, combinef :- AnyFn, reducef :- AnyFn, coll :- []]
   (fold-augmented (xform (folding combinef reducef)) coll)))

;; Replace original fold so that the old code will work
;; with new collections
(replace-var! clojure.core.reducers/fold)

;; Fall back to reduce for non foldable objects
(extend-protocol! IFoldable
  java.lang.Object
  (-fold [coll reduce-fn pool n combinef reducef]
    (reduce-fn coll reducef (combinef))))
