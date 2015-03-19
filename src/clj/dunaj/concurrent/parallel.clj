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

(ns dunaj.concurrent.parallel
  "Parallel computations based on thread pools.

  See <<dunaj.concurrent.forkjoin.api.ad#,dunaj.concurrent.forkjoin>>
  for parallel computations based on a Fork–join model.
  See <<dunaj.concurrent.port.api.ad#pipeline_BANG_,pipeline!>>
  if you want to work with channels."
  {:authors ["Jozef Wagner"]
   :categories
   [["Primary"
     "See <<Unordered,unordered section>> for faster
     but unordered version of functions for parallel computation."],
    ["Unordered"
     "See <<Primary,ordered section>> for slower
     but ordered version of functions for parallel computation."]]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [clojure.core.async]
   [dunaj.type :refer [Any Fn AnyFn Maybe U I]]
   [dunaj.boolean :refer [or]]
   [dunaj.host :refer [..]]
   [dunaj.math :refer [Integer < + zero? dec]]
   [dunaj.compare :refer [decode-nil encode-nil]]
   [dunaj.state :refer [deref]]
   [dunaj.flow :refer [cond if let loop recur when when-let do]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [deftype]]
   [dunaj.coll :refer
    [IRed IReducing conj count peek pop unsafe-postponed
     reduced reduced? postponed? postponed advance unsafe-advance!]]
   [dunaj.function :refer [fn defn apply]]
   [dunaj.coll.helper :refer
    [reduce* defxform finish-advance reduced-advance strip-reduced]]
   [dunaj.error :refer [throw unsupported-operation]]
   [dunaj.concurrent :refer
    [ITaskExecutor default-future-executor submit]]
   [dunaj.concurrent.port :refer
    [ICloseablePort ISourcePort ITargetPort <!! >!! close! chan]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.coll.default :refer [empty-que]]
   [dunaj.coll.tuple :refer [tuple]]
   [dunaj.coll.recipe :refer [concat]]))


;;;; Implementation details

(deftype ^:private PMapWrap [ret :- Any, q :- []])

(defn ^:private pmap-advance :- Any
  [ret :- Any, q :- []]
  (cond (reduced? ret)
        (reduced (->PMapWrap @ret nil))
        (postponed? ret)
        (postponed (->PMapWrap @ret q)
                   #(pmap-advance (advance ret) q)
                   #(pmap-advance (unsafe-advance! ret) q))
        :else (->PMapWrap ret q)))

(deftype PMapReducing
  [r :- IReducing, pool :- ITaskExecutor, n :- Integer, f :- AnyFn]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [w (strip-reduced wrap)
          ret (.-ret ^dunaj.concurrent.parallel.PMapWrap w)
          q (.-q ^dunaj.concurrent.parallel.PMapWrap w)]
      (-> (reduce* q #(._step r % (deref %2)) ret)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret] (->PMapWrap (._wrap r ret) empty-que))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.concurrent.parallel.PMapWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.concurrent.parallel.PMapWrap wrap)
          q (.-q ^dunaj.concurrent.parallel.PMapWrap wrap)
          nq (conj q (submit pool #(f val)))]
      (if (< n (count nq))
        (pmap-advance (._step r ret @(peek nq)) (pop nq))
        (->PMapWrap ret nq))))
  (-step [this wrap val1 val2]
    (let [ret (.-ret ^dunaj.concurrent.parallel.PMapWrap wrap)
          q (.-q ^dunaj.concurrent.parallel.PMapWrap wrap)
          nq (conj q (submit pool #(f val1 val2)))]
      (if (< n (count nq))
        (pmap-advance (._step r ret @(peek nq)) (pop nq))
        (->PMapWrap ret nq))))
  (-step [this wrap val1 val2 val3]
    (let [ret (.-ret ^dunaj.concurrent.parallel.PMapWrap wrap)
          q (.-q ^dunaj.concurrent.parallel.PMapWrap wrap)
          nq (conj q (submit pool #(f val1 val2 val3)))]
      (if (< n (count nq))
        (pmap-advance (._step r ret @(peek nq)) (pop nq))
        (->PMapWrap ret nq))))
  (-step [this wrap val1 val2 val3 val4]
    (let [ret (.-ret ^dunaj.concurrent.parallel.PMapWrap wrap)
          q (.-q ^dunaj.concurrent.parallel.PMapWrap wrap)
          nq (conj q (submit pool #(f val1 val2 val3 val4)))]
      (if (< n (count nq))
        (pmap-advance (._step r ret @(peek nq)) (pop nq))
        (->PMapWrap ret nq))))
  (-step [this wrap val1 val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.concurrent.parallel.PMapWrap wrap)
          q (.-q ^dunaj.concurrent.parallel.PMapWrap wrap)
          nq (conj q (submit pool
                             #(apply f val1 val2 val3 val4 more)))]
      (if (< n (count nq))
        (pmap-advance (._step r ret @(peek nq)) (pop nq))
        (->PMapWrap ret nq)))))

(deftype ^:private UPMapWrap
  [ret :- Any, ich :- Any, och :- (Maybe ICloseablePort), tl :- Any])

(defn ^:private upmap-advance :- Any
  [ret :- Any, ich :- Any, och :- ICloseablePort, tl :- Any]
  (cond (reduced? ret)
        (do (close! och)
            (reduced (->UPMapWrap @ret ich nil tl)))
        (postponed? ret)
        (unsafe-postponed
         (->UPMapWrap @ret ich och tl)
         #(upmap-advance (unsafe-advance! ret) ich och tl))
        :else (->UPMapWrap ret ich och tl)))

(defn ^:private pf :- nil
  [f :- AnyFn, ich :- ISourcePort, och :- ITargetPort]
  (loop [val (<!! ich)]
    (when val
      (>!! och (encode-nil (apply f val)))
      (recur (<!! ich)))))

(deftype UnorderedPMapReducing
  [r :- IReducing, pool :- ITaskExecutor, n :- Integer, f :- AnyFn]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [w (strip-reduced wrap)
          ret (.-ret ^dunaj.concurrent.parallel.UPMapWrap w)
          ich (.-ich ^dunaj.concurrent.parallel.UPMapWrap w)
          och (.-och ^dunaj.concurrent.parallel.UPMapWrap w)
          tl (.-tl ^dunaj.concurrent.parallel.UPMapWrap w)
          af (fn af [ret i]
               (cond
                (reduced? ret) (do (close! och) ret)
                (postponed? ret)
                (postponed @ret
                           #(af (advance ret) i)
                           #(af (unsafe-advance! ret) i))
                (zero? i) (do (close! och) ret)
                :else (recur (._step r ret (decode-nil (<!! och)))
                             (dec i))))]
      (close! ich)
      (-> (if och (af ret (count tl)) ret)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret] (->UPMapWrap (._wrap r ret) (chan) (chan n) ()))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.concurrent.parallel.UPMapWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ich (.-ich ^dunaj.concurrent.parallel.UPMapWrap wrap)
          och (.-och ^dunaj.concurrent.parallel.UPMapWrap wrap)
          tl (.-tl ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ntl (if (< (count tl) n)
                (conj tl (submit pool #(pf f ich och)))
                tl)]
      (>!! ich (tuple val))
      (if (< (count tl) n)
        (->UPMapWrap ret ich och ntl)
        (upmap-advance (._step r ret (decode-nil (<!! och)))
                       ich och ntl))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ich (.-ich ^dunaj.concurrent.parallel.UPMapWrap wrap)
          och (.-och ^dunaj.concurrent.parallel.UPMapWrap wrap)
          tl (.-tl ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ntl (if (< (count tl) n)
                (conj tl (submit pool #(pf f ich och)))
                tl)]
      (>!! ich (tuple val val2))
      (if (< (count tl) n)
        (->UPMapWrap ret ich och ntl)
        (upmap-advance (._step r ret (decode-nil (<!! och)))
                       ich och ntl))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ich (.-ich ^dunaj.concurrent.parallel.UPMapWrap wrap)
          och (.-och ^dunaj.concurrent.parallel.UPMapWrap wrap)
          tl (.-tl ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ntl (if (< (count tl) n)
                (conj tl (submit pool #(pf f ich och)))
                tl)]
      (>!! ich (tuple val val2 val3))
      (if (< (count tl) n)
        (->UPMapWrap ret ich och ntl)
        (upmap-advance (._step r ret (decode-nil (<!! och)))
                       ich och ntl))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ich (.-ich ^dunaj.concurrent.parallel.UPMapWrap wrap)
          och (.-och ^dunaj.concurrent.parallel.UPMapWrap wrap)
          tl (.-tl ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ntl (if (< (count tl) n)
                (conj tl (submit pool #(pf f ich och)))
                tl)]
      (>!! ich (tuple val val2 val3 val4))
      (if (< (count tl) n)
        (->UPMapWrap ret ich och ntl)
        (upmap-advance (._step r ret (decode-nil (<!! och)))
                       ich och ntl))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ich (.-ich ^dunaj.concurrent.parallel.UPMapWrap wrap)
          och (.-och ^dunaj.concurrent.parallel.UPMapWrap wrap)
          tl (.-tl ^dunaj.concurrent.parallel.UPMapWrap wrap)
          ntl (if (< (count tl) n)
                (conj tl (submit pool #(pf f ich och)))
                tl)]
      (>!! ich (concat (tuple val val2 val3 val4) more))
      (if (< (count tl) n)
        (->UPMapWrap ret ich och ntl)
        (upmap-advance (._step r ret (decode-nil (<!! och)))
                       ich och ntl)))))


;;;; Public API

(defxform pmap*
  "Returns a transducer that applies `_f_` at step items at parallel,
  `_n_` threads at most, using `_pool_` for executing tasks.
  Order of items is preserved.

  Uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  if `_pool_` is `nil`.

  Uses twice the available processors if `_n_` is `nil`."
  {:added v1
   :see '[pmap dunaj.concurrent.forkjoin/transfold]
   :category "Primary"}
  [pool :- (Maybe ITaskExecutor), n :- (Maybe Integer), f :- AnyFn]
  ([r] (let [pool (or pool @default-future-executor)
             n (or n (+ 2 (.. java.lang.Runtime
                              getRuntime availableProcessors)))]
         (->PMapReducing r pool n f)))
  :unpack false)

(defxform pmap-unordered*
  "Returns a transducer that applies `_f_` at step items at parallel,
  `_n_` threads at most, using `_pool_` for executing tasks.
  Order of items is not preserved.

  Uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  if `_pool_` is `nil`.

  Uses twice the available processors if `_n_` is `nil`."
  {:added v1
   :see '[pmap-unordered dunaj.concurrent.forkjoin/transfold]
   :category "Unordered"}
  [pool :- (Maybe ITaskExecutor), n :- (Maybe Integer), f :- AnyFn]
  ([r] (let [pool (or pool @default-future-executor)
             n (or n (+ 2 (.. java.lang.Runtime
                              getRuntime availableProcessors)))]
         (->UnorderedPMapReducing r pool n f)))
  :fold false
  :unpack false)

(defn pmap :- Any
  "Returns a transducer that applies `_f_` at step items at parallel,
  `_n_` threads at most, using `_pool_` for executing tasks.
  Order of items is preserved.

  Uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  if `_pool_` is not specified.

  Uses twice the available processors if `_n_` is not specified."
  {:added v1
   :transducer true
   :see '[pmap* pmap-unordered dunaj.concurrent.forkjoin/fold
          dunaj.concurrent.port/pipeline!]
   :category "Primary"}
  ([f :- AnyFn]
   (pmap* nil nil f))
  ([f :- AnyFn, coll :- []]
   (pmap* nil nil f coll))
  ([n :- (Maybe Integer), f :- AnyFn, coll :- []]
   (pmap* nil n f coll))
  ([pool :- (Maybe ITaskExecutor), n :- (Maybe Integer), f :- AnyFn,
    coll :- []]
   (pmap* pool n f coll)))

(defn pmap-unordered :- Any
  "Returns a transducer that applies `_f_` at step items at parallel,
  `_n_` threads at most, using `_pool_` for executing tasks.
  Order of items is not preserved.

  Uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  if `_pool_` is not specified.

  Uses twice the available processors if `_n_` is not specified."
  {:added v1
   :transducer true
   :see '[pmap-unordered* pmap]
   :category "Unordered"}
  ([f :- AnyFn]
   (pmap-unordered* nil nil f))
  ([f :- AnyFn, coll :- []]
   (pmap-unordered* nil nil f coll))
  ([n :- (Maybe Integer), f :- AnyFn, coll :- []]
   (pmap-unordered* nil n f coll))
  ([pool :- (Maybe ITaskExecutor), n :- (Maybe Integer), f :- AnyFn,
    coll :- []]
   (pmap-unordered* pool n f coll)))

(defn pcalls :- IRed
  "Executes the no-arg `_fns_` in parallel,
  returning collection recipe of their values, in the same order.

  NOTE: uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  for the execution of `_fns_` and twice the available processors."
  {:added v1
   :see '[pvalues pcalls-unordered]
   :category "Primary"}
  [& fns :- AnyFn] (pmap #(%) fns))

(defn pcalls-unordered :- IRed
  "Executes the no-arg `_fns_` in parallel,
  returning collection recipe of their values, in any order.

  NOTE: uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  for the execution of `_fns_` and twice the available processors."
  {:added v1
   :see '[pvalues-unordered pcalls]
   :category "Unordered"}
  [& fns :- AnyFn] (pmap-unordered #(%) fns))

(defmacro pvalues
  "Returns a collection recipe of the values of the `_exprs_`,
  which are evaluated in parallel. The values are in the same order
  as their `_exprs_`.

  NOTE: uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  for the execution of `_exprs_` and twice the available processors."
  {:added v1
   :see '[pcalls pvalues-unordered]
   :category "Primary"}
  [& exprs]
  `(pcalls ~@(clojure.core/map #(clojure.core/list `fn [] %) exprs)))

(defmacro pvalues-unordered
  "Returns a collection recipe of the values of the exprs, which are
  evaluated in parallel. The values may not be in the same order
  as their `_exprs_`.

  NOTE: uses `<<dunaj.concurrent.api.ad#default_future_executor,default-future-executor>>`
  for the execution of `_exprs_` and twice the available processors."
  {:added v1
   :see '[pcalls-unordered pvalues]
   :category "Unordered"}
  [& exprs]
  `(pcalls-unordered
    ~@(clojure.core/map #(clojure.core/list `fn [] %) exprs)))
