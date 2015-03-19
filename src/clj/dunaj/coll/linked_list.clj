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

(ns dunaj.coll.linked-list
  "Linked list, a sequential LIFO seq type.

  This namespace defines a type for object which extends
  `<<dunaj.coll.spi.ad#IPersistentList,IPersistentList>>`
  that has at least one item.
  CLJ format defines syntax for empty list literal, `(x y z)`,
  but as this syntax is used for function invocation, idiomatic way
  is to use `<<dunaj.coll.default.api.ad#__GT_lst,->lst>>` and
  `<<dunaj.coll.default.api.ad#lst,lst>>` constructor functions.

  Note that persistent lists are a type of LIFO collections.
  They are seqs, are counted and support `pop`. Empty list is
  represented by the special type defined in
  `<<dunaj.coll.empty-list.api.ad#,dunaj.coll.empty-list>>`
  namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [I]]
   [dunaj.host :refer [set! class-instance?]]
   [dunaj.flow :refer [cond recur if let do]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [reify deftype defrecord]]
   [dunaj.coll :refer
    [IEmptyable IRed ISeq IEmptyAware IPeekable ICounted IEditable
     ICollectionFactory ISeqable IPersistentCollection IStacked
     IPersistentList ISequential ISettleable IMutableCollection
     first next reverse reversible? conj collection count -from-coll
     reduced? postponed? postponed -conj! reduce -settle! edit
     counted? settle! conj!]]
   [dunaj.function :refer [fn]]
   [dunaj.coll.helper :refer [advance-fn]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.bvt-vector :refer [empty-bvt-vector]]
   [dunaj.coll.empty-list :refer [empty-list]]))


;;;; Public API

(deftype LinkedList
  "A type for kinked lists."
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  ;; c.l.IReduce
  clojure.lang.PersistentList
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret coll :- clojure.lang.ISeq]
               coll (recur (reducef ret (.first coll))
                                  (.next coll))
               :else ret)]
      (af init this)))
  ISeqable
  ICounted
  IEmptyAware
  (-empty? [this] false)
  IEmptyable
  IPeekable
  ISequential
  IPersistentCollection
  IStacked

  ;; Abstract types
  IPersistentList
  ISeq)

(deftype ReversedListBuilder
  [^:unsynchronized-mutable tv :- (I IMutableCollection ISettleable)]
  ICounted
  (-count [this] (count tv))
  ISettleable
  (-settle! [this] (reduce conj empty-list (reverse (-settle! tv))))
  IMutableCollection
  (-conj! [this x] (set! tv (-conj! tv x)) this))

(def reversed-list-builder :- IEditable
  "An editable type for efficiently building list from non-reversible
  collections."
  (reify
    IEditable
    (-edit [this capacity-hint]
      (->ReversedListBuilder
       (edit empty-bvt-vector capacity-hint)))))

;;; Factory

(defrecord LinkedListFactory
  "A factory record for linked lists."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (cond (class-instance? clojure.lang.PersistentList coll) coll
          (reversible? coll) (reduce conj empty-list (reverse coll))
          :else (let [tv (if (counted? coll)
                           (edit empty-bvt-vector (count coll))
                           (edit empty-bvt-vector))
                      vec (settle! (reduce conj! tv coll))]
                  (reduce conj empty-list (reverse vec)))))
  (-from-items [factory] empty-list)
  (-from-items [factory a] (conj empty-list a))
  (-from-items [factory a b] (conj empty-list b a))
  (-from-items [factory a b c] (conj empty-list c b a))
  (-from-items [factory a b c d] (conj empty-list d c b a))
  (-from-items [factory a b c d more]
    (conj (-from-coll factory more) d c b a)))

(def linked-list-factory :- ICollectionFactory
  "A Linked List factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of Linked List can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[dunaj.coll.batched-queue/batched-queue-factory
          dunaj.coll.default/lst-factory
          dunaj.coll.default/->lst
          dunaj.coll.default/lst
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->LinkedListFactory))
