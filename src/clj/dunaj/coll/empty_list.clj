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

(ns dunaj.coll.empty-list
  "Empty list, a seq type.

  This namespace defines a type for object which extends
  `<<dunaj.coll.spi.ad#IPersistentList,IPersistentList>>`
  and has no items. CLJ format defines syntax for empty list
  literal, `()`.

  Note that persistent lists are a type of LIFO collections.
  They are seqs, are counted and support `pop`.
  Calling `pop` on empty list will however result in an exception
  being thrown.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  `nil` rather than `empty-list`."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [deftype]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed ISeq IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable IPersistentCollection IPersistentList
     first]]
   [dunaj.state.var :refer [def]]))


;;;; Public API

(deftype EmptyList
  "A type for persistent empty lists."
  clojure.lang.PersistentList$EmptyList
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  IMeta
  IPersistentMeta
  IRed (-reduce [this reducef init] init)
  ISeqable
  ICounted
  IEmptyAware (-empty? [this] true)
  IEmptyable
  IPeekable
  ISequential
  IPersistentCollection
  ;; Abstract types
  ISeq
  IPersistentList)

(def empty-list :- EmptyList
  "An empty list. Same as `()`.
  Note that if is often more idiomatic to use `nil` instead of
  (any form of) empty list or collection."
  {:added v1
   :see '[dunaj.coll.linked-list/linked-list-factory
          dunaj.coll.batched-queue/empty-batched-queue
          dunaj.coll.default/empty-lst dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/peek dunaj.coll/pop]}
  clojure.lang.PersistentList/EMPTY)
