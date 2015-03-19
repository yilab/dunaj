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

(ns dunaj.state.weak
  "Weak and soft references.

  Weak references don't protect the referenced object from getting
  collected by a garbage collector. Soft references are like weak
  ones, but they allow the referenced object to be cleared only
  when memory is running low."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core.async]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn U I Maybe]]
   [dunaj.boolean :refer [and not]]
   [dunaj.state :refer [IReference IAtomic cas! ICloneable]]
   [dunaj.compare :refer [not= nil?]]
   [dunaj.flow :refer [let when if doto if-let recur loop]]
   [dunaj.poly :refer [extend-type! deftype]]
   [dunaj.function :refer [fn defn]]
   [dunaj.error :refer [IException throw no-such-element try catch]]
   [dunaj.state.basic :refer [atom]]
   [dunaj.state.var :refer [defonce defalias declare]]
   [dunaj.concurrent.port :refer
    [ITargetPort chan put! thread go-loop]]))


;;;; Implementation details

(defonce ^:private reference-queue :- java.lang.ref.ReferenceQueue
  (java.lang.ref.ReferenceQueue.))

(defonce ^:private reference-map :- java.util.Map
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(defonce ^:private reference-handler? :- (I IReference IAtomic)
  (atom nil))

(defn ^:private spin-up! :- nil
  []
  (when (cas! reference-handler? nil true)
    (thread
     ;; TODO: Does this even work?
     (try (loop [r (.remove reference-queue)]
            (if-let [ch (.remove reference-map r)]
              (put! ch r)
              (throw (no-such-element "Unknown reference enqueued.")))
            (recur (.remove reference-queue)))
          (catch java.lang.Exception e
            (clojure.core/println "caught " e))))
    nil))

(extend-type! java.lang.ref.Reference
  IReference
  (-deref [ref]
    (.get ref)))


;;;; Public API

(declare weak soft)

(deftype Weak
  "A weak reference type."
  java.lang.ref.WeakReference
  ICloneable
  (-clone [this] (weak @this)))

(defn weak :- IReference
  "Returns a weak reference to `_x_`. If `_ch_` is specified,
  puts reference into `_ch_` when enqueued by host."
  {:added v1
   :see '[soft dunaj.state/deref dunaj.state.basic/box
          dunaj.state.basic/local dunaj.state.basic/atom]}
  ([x :- Any]
   (java.lang.ref.WeakReference. x))
  ([x :- Any, ch :- ITargetPort]
   (if (nil? ch)
     (weak x)
     (let [r (java.lang.ref.WeakReference. x reference-queue)]
       (spin-up!)
       (.put reference-map r ch)
       r))))

(deftype Soft
  "A soft reference type."
  java.lang.ref.SoftReference
  ICloneable
  (-clone [this] (soft @this)))

(defn soft :- IReference
  "Returns a soft reference to `_x_`.
  If `_ch_` is specified, puts reference into `_ch_` when enqueued
  by host."
  {:added v1
   :see '[weak dunaj.state/deref dunaj.state.basic/box
          dunaj.state.basic/local dunaj.state.basic/atom]}
  ([x :- Any]
   (java.lang.ref.SoftReference. x))
  ([x :- Any, ch :- ITargetPort]
   (if (nil? ch)
     (soft x)
     (let [r (java.lang.ref.SoftReference. x reference-queue)]
       (spin-up!)
       (.put reference-map r ch)
       r))))
