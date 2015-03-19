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

(ns dunaj.coll.lazy-seq
  "A Lazy seq type."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Macro]]
   [dunaj.state :refer [IPending]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [deftype]]
   [dunaj.coll :refer
    [IEmptyable IRed ISequential ISeqable ISeq IEmptyAware IPeekable
     IPersistentCollection ICollectionFactory]]
   [dunaj.coll.helper :refer [reduce*]]
   [dunaj.state.var :refer [defalias]]))


;;;; Public API

(deftype Lazy
  "Lazy seq type.

  NOTE: In Dunaj, lazy seqs have less use and collection recipes
  are a better abstraction for most cases."
  clojure.lang.LazySeq
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  IPending
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init] (reduce* (.seq this) reducef init))
  ISeqable
  IEmptyable
  IPeekable (-peek [this] (.first this))
  ISequential
  IPersistentCollection
  ;; Abstract types
  ISeq)

(defalias lazy-seq
  "Takes a `_body_` of expressions that returns an `ISeq` or
  `nil`, and yields a seq collection that will invoke the body
  only the first time `seq` is called, and will cache the
  result and return it on all subsequent `seq` calls."
  {:added v1
   :tsig Macro
   :see '[dunaj.coll/realized? lazy-cat
          dunaj.state/delay
          dunaj.coll.lazy-seq-map/lazy-seq->map
          dunaj.coll.lazy-seq-set/lazy-seq->set]})

(defalias lazy-cat
  "Expands to code which yields a lazy sequence of the
  concatenation of the supplied `_colls_`. Each coll expr is
  not evaluated until it is needed.

  `(lazy-cat xs ys zs)` is the same as
  `(seq (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs)))`"
  {:added v1
   :tsig Macro
   :see '[dunaj.coll/realized? lazy-seq
          dunaj.state/delay
          dunaj.coll.lazy-seq-map/lazy-seq->map
          dunaj.coll.lazy-seq-set/lazy-seq->set]})
