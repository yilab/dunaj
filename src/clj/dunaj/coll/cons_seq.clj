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

(ns dunaj.coll.cons-seq
  "Cons seq, a basic seq type.

  A cons object consists of head (first) item and of a tail
  (rest of the collection). Cons does not care what the tail is as
  long as it is `<<dunaj.coll.spi.ad#ISeq,ISeq>>`.
  Moreover, cons objects are not counted (calling count on them is
  not efficient), never empty and are not a persistent stack, as pop
  cannot guarantee to return
  `<<dunaj.coll.spi.ad#IStacked,IStacked>>` seq."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.host :refer [class-instance?]]
   [dunaj.host.int :refer [Int iint isub i== i0 iinc]]
   [dunaj.flow :refer [if loop recur let cond]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [deftype extend-protocol! identical-type?]]
   [dunaj.coll :refer
    [IEmptyable IRed ISequential ISeqable ISeq IEmptyAware IPeekable
     IPersistentCollection ICollectionFactory
     reduced? postponed? postponed]]
   [dunaj.function :refer [defn fn]]
   [dunaj.coll.helper :refer [reduce* advance-fn]]
   [dunaj.state.var :refer [defalias]]))


;;;; Implementation details

(defn ^:private cons? :- Boolean
  [x :- Any]
  (class-instance? clojure.lang.Cons x))

(extend-protocol! IRed
  clojure.lang.IChunk
  (-reduce [this reducef init]
    (let [c (iint (.count this))
          af (advance-fn [ret i :- Int]
               (i== i c) ret
               :else (recur (reducef ret (.nth this i)) (iinc i)))]
      (af init (i0))))
  clojure.lang.ChunkedCons
  (-reduce [this reducef init]
    (let [af (advance-fn [ret s :- clojure.lang.ChunkedCons]
               (let [m (clojure.core/seq (.chunkedMore s))]
                 (if (identical-type? s m)
                   (recur
                    (reduce*
                     (.chunkedFirst ^clojure.lang.ChunkedCons m)
                     reducef ret)
                    m)
                   (reduce* m reducef ret))))]
      (af (reduce* (.chunkedFirst this) reducef init) this))))


;;;; Public API

(deftype Cons
  "A type for Cons seq."
  clojure.lang.Cons
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret :- Any, cons :- Cons]
               (let [x (.next cons)]
                 (if (cons? x)
                   (recur (reducef ret (.first x)) x)
                   (reduce* x reducef ret))))]
      (af (reducef init (.first this)) this)))
  ISeqable
  IEmptyAware (-empty? [this] false)
  IEmptyable
  IPeekable (-peek [this] (.first this))
  ISequential
  IPersistentCollection
  ;; Abstract types
  ISeq)

(defalias cons
  {:added v1
   :tsig (Fn [Cons Any []])
   :arglists '([x coll])
   :see '[dunaj.coll/conj dunaj.coll.default/->lst
          dunaj.coll.default/empty-seq]
   :doc "Returns a new seq where `_x_` is the first item and
        `_coll_` is the rest. Calls `seq` on `_coll_` if it is not
        already a seq.

        IMPORTANT: `cons` is not polymorphic and will always return
        a seq. Use `<<dunaj.coll.api.ad#conj,conj>>` if you want to
        preserve the characteristics of `_coll_`."})
