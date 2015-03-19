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

(ns dunaj.coll.batched-queue
  "Batched queue, a sequential FIFO persistent collection.

  Batched queue is an implementation of FIFO (conj onto rear,
  peek/pop from front)
  `<<dunaj.coll.spi.ad#IPersistentList,IPersistentList>>`
  based on http://okasaki.blogspot.sk/[Okasaki's] Batched Queues.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  functions defined in `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.host :refer [set! class-instance?]]
   [dunaj.compare :refer [IHash IEquiv nil?]]
   [dunaj.flow :refer [when-let cond loop recur if let do]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [deftype defrecord]]
   [dunaj.coll :refer
    [IEmptyable IRed ISeq ISequential IPersistentCollection IStacked
     IPersistentList IEmptyAware IPeekable ICounted ICollectionFactory
     ISeqable collection first next reverse reversible? reduce conj
     seq postponed postponed? reduced?]]
   [dunaj.function :refer [apply fn]]
   [dunaj.coll.helper :refer [reduce* advance-fn]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.empty-list]
   [dunaj.coll.bvt-vector]))


;;;; Public API

(deftype BatchedQueue
  "Batched Queue."
  clojure.lang.PersistentQueue
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  ;; (CLJ BUG: only Collection for now)
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (reduce* (.-r this) reducef ret))]
      (af (reduce* (.-f this) reducef init))))
  ISeqable
  ICounted
  IEmptyable
  IPeekable
  ISequential
  IPersistentCollection
  IStacked

  ;; Abstract types
  IPersistentList)

(def empty-batched-queue :- BatchedQueue
  "An empty batched queue."
  {:added v1
   :see '[batched-queue-factory dunaj.coll.empty-list/empty-list
          dunaj.coll.default/empty-que dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/peek dunaj.coll/pop]}
  clojure.lang.PersistentQueue/EMPTY)

;;; Factory

(defrecord BatchedQueueFactory
  "Factory for batched queue."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    ;; use coll as a rear seq
    (let [s (if (class-instance? clojure.lang.Seqable coll)
              coll
              (seq coll))]
      (clojure.lang.PersistentQueue/createFromColl nil s)))
  (-from-items [factory] empty-batched-queue)
  (-from-items [factory a] (conj empty-batched-queue a))
  (-from-items [factory a b] (conj empty-batched-queue a b))
  (-from-items [factory a b c] (conj empty-batched-queue a b c))
  (-from-items [factory a b c d]
    (conj empty-batched-queue a b c d))
  (-from-items [factory a b c d more]
    (apply conj empty-batched-queue a b c d more)))

(def batched-queue-factory :- ICollectionFactory
  "A Batched Queue factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of Batched Queue can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[dunaj.coll.linked-list/linked-list-factory
          dunaj.coll.default/que-factory
          dunaj.coll.default/->que
          dunaj.coll.default/que
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->BatchedQueueFactory))
