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

(ns dunaj.resource.collreader
  "Class for dunaj.resource.host/coll-reader.

  IMPORTANT: This is a helper namespace. It does not contain any
  public vars and there is *no need to require this namespace*
  directly."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require [dunaj.boolean :refer [Boolean and or not]]
            [dunaj.host.int :refer [iint iinc i0 i-1 i< iadd]]
            [dunaj.compare :refer [nil? =]]
            [dunaj.state :refer [reset! trade! alter!]]
            [dunaj.flow :refer [let recur if do cond loop if-let]]
            [dunaj.feature :refer [assoc-meta]]
            [dunaj.poly :refer [reify defprotocol deftype defrecord]]
            [dunaj.coll :refer [first item-type seq next empty? conj]]
            [dunaj.function :refer [fn defn]]
            [dunaj.host.array :refer [aset-char! array-manager]]
            [dunaj.macro :refer [defmacro]]
            [dunaj.state.basic :refer [local]])
  (:gen-class
   :extends java.io.Reader
   :state state
   :name dunaj.resource.CollReader
   :init init
   :constructors {[java.lang.Object] []}
   :main false))

(deftype CollWrapState [csr])

(defn -init [coll] [[] (->CollWrapState (local (seq coll)))])

(defn -close [this])

(defmacro get-state
  "All this just to eliminate reflection."
  [x]
  (let [x (if clojure.core/*compile-files*
            (assoc-meta x {:tag 'dunaj.resource.CollReader})
            x)]
    `(.state ~x)))

(defn -read-char<>-int-int [this carr off len]
  (let [s :- CollWrapState (get-state this)
        csr (.-csr ^dunaj.resource.collreader.CollWrapState s)]
    (if (nil? @csr)
      (i-1)
      ;; TODO: make it faster for char batchable colls
      (loop [s @csr, i (i0)]
        (if (and (not (nil? s)) (i< i len))
          (do (aset-char! carr (iadd i off) (first s))
              (recur (next s) (iinc i)))
          (do (reset! csr s) i))))))
