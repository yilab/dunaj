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

(ns dunaj.resource.collwriter
  "Class for dunaj.resource.host/coll-writer.

  IMPORTANT: This is a helper namespace. It does not contain any
  public vars and there is *no need to require this namespace*
  directly."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require [clojure.core.async]
            [dunaj.boolean :refer [Boolean and or not]]
            [dunaj.host :refer [keyword->class Array ArrayManager]]
            [dunaj.host.int :refer [iint iinc i0 i-1 i< iadd]]
            [dunaj.compare :refer [nil? =]]
            [dunaj.state :refer [reset! trade! alter!]]
            [dunaj.feature :refer [assoc-meta]]
            [dunaj.flow :refer [let recur if do cond loop when]]
            [dunaj.poly :refer [reify defprotocol deftype defrecord]]
            [dunaj.coll :refer [first item-type seq next empty? conj]]
            [dunaj.function :refer [fn defn]]
            [dunaj.host.array :refer [aset-char! array-manager]]
            [dunaj.state.var :refer [def]]
            [dunaj.macro :refer [defmacro]]
            [dunaj.concurrent.port :refer [close!]])
  (:gen-class
   :extends java.io.Writer
   :state state
   :name dunaj.resource.CollWriter
   :init init
   :constructors
   {[java.lang.Object java.lang.Object java.lang.Object] []}
   :main false))

(deftype CollWrapState [dor dar wc])

(defmacro get-state
  "All this just to eliminate reflection."
  [x]
  (let [x (if clojure.core/*compile-files*
            (assoc-meta x {:tag 'dunaj.resource.CollWriter})
            x)]
    `(.state ~x)))

(defn -init [done?-ref data-ref watcher-ch]
  [[] (->CollWrapState done?-ref data-ref watcher-ch)])

(defn -close [this]
  (let [s :- CollWrapState (get-state this)
        watcher-ch (.-wc ^dunaj.resource.collwriter.CollWrapState s)
        done?-ref (.-dor ^dunaj.resource.collwriter.CollWrapState s)]
    (when watcher-ch (close! watcher-ch))
    (reset! done?-ref true)))

(defn -flush [this])

(def ^:private cam :- ArrayManager
  (array-manager (keyword->class :char)))

(defn -write-char<>-int-int [this carr off len]
  (let [s :- CollWrapState (get-state this)
        data-ref (.-dar ^dunaj.resource.collwriter.CollWrapState s)]
    (let [arr (.duplicate cam carr off (iadd off len))]
      (alter! data-ref conj arr))))
