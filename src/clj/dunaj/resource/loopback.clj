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

(ns dunaj.resource.loopback
  "Basic loopback resource."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [clojure.core.async]
   [dunaj.type :refer [Fn AnyFn Any Maybe U I]]
   [dunaj.boolean :refer [Boolean or and not]]
   [dunaj.host :refer [keyword->class class-instance? set!]]
   [dunaj.host.int :refer [i0 iinc]]
   [dunaj.math :refer [Integer integer? pos? odd? max neg? == min]]
   [dunaj.state :refer
    [IAtomic cancel! alter! cas! reference? IOpenAware adjust! io!
     atomic? switch! IAdjustable IReference IMutable ensure-io
     ensure-open open? ICloneable]]
   [dunaj.compare :refer [identical? = nil?]]
   [dunaj.flow :refer [if when let when-not when-let loop cond do
                       condp if-let if-not recur when if-some]]
   [dunaj.threading :refer [->>]]
   [dunaj.buffer :refer [buffer]]
   [dunaj.poly :refer
    [instance? reify satisfies? defprotocol deftype defrecord]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.coll :refer
    [count contains? next first IRed IBatchedRed sequential? empty?
     -reduce-batched reduced? get reduce unsafe-postponed full?
     map? assoc update-in conj postponed? unsafe-advance! postponed]]
   [dunaj.function :refer [apply defn invocable? fn partial]]
   [dunaj.coll.helper :refer [recipe]]
   [dunaj.host.batch :refer [select-item-type batch-manager]]
   [dunaj.concurrent :refer [future]]
   [dunaj.concurrent.port :as dp :refer
    [ISourcePort chan <!! thread >!! timeout alts!! close!]]
   [dunaj.string :refer [String string?]]
   [dunaj.time :refer [IDuration milliseconds]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.state.weak :refer [weak]]
   [dunaj.state.basic :refer [atom]]
   [dunaj.state.var :refer [Var var var? def declare alter-root!]]
   [dunaj.error :refer [IException ex-info illegal-argument throw npe
                        try unsupported-operation fail-aware? error]]
   [dunaj.uri :refer [Uri uri uri?]]
   [dunaj.coll.tuple :refer [tuple]]
   [dunaj.coll.util :refer [into revlist doseq merge]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.default]
   [dunaj.format :refer [IParserFactory IPrinterFactory print parse]]
   [dunaj.resource :refer
    [IWritable IReadable IAcquirableFactory IReleasable]]
   [dunaj.resource.helper :refer [defreleasable]]))


;;;; Implementation details

(def ^:private default-loopback-size :- Integer
  "Default size for loopback buffer."
  8192)

(deftype LoopbackResourceReader
  "Reads from the loopack."
  [resource :- IOpenAware, buf :- Any,
   ch :- ISourcePort, non-blocking? :- Boolean]
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IRed
  (-reduce [this reducef init]
    (ensure-io)
    (ensure-open resource)
    (let [af (fn af [ret wait?]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed
                 @ret #(af (unsafe-advance! ret) false))
                wait? (unsafe-postponed ret #(af ret false))
                (and non-blocking? (empty? buf)) (recur ret true)
                :else
                (if-some [v (<!! ch)]
                  (recur (reducef ret v) false)
                  ret)))]
      (af init false))))

(defreleasable LoopbackResource
  [config :- {}, buf :- Any, ch :- Any,
   ^:volatile-mutable opened? :- Boolean, non-blocking? :- Boolean]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] opened?)
  IReleasable
  (-release! [this] (set! opened? false) (close! ch))
  IReadable
  (-read! [this] (->LoopbackResourceReader this buf ch non-blocking?))
  IWritable
  (-write! [this coll]
    (ensure-open this)
    (reduce
     (fn af [ret :- Any, val :- Any]
       (if (and non-blocking? (full? buf))
         (unsafe-postponed ret #(af ret val))
         (do (>!! ch val) (iinc ret))))
     (i0) coll)))

(defrecord LoopbackResourceFactory
  [size-hint non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [size (max (or size-hint 0) default-loopback-size)
          buf (buffer size)]
      (->LoopbackResource
       {:size size} buf (chan buf) true non-blocking?))))


;;;; Public API

(def loopback-factory :- IAcquirableFactory
  "Loopback resource factory. Thread safe."
  {:added v1
   :see '[loopback]}
  (->LoopbackResourceFactory nil false))

(defn loopback :- LoopbackResource
  "Returns a thread safe loopback resource factory,
  which will return back the written data."
  {:added v1
   :see '[loopback-factory]}
  ([] loopback-factory)
  ([size-hint :- (Maybe Integer) & {:as opts}]
     (merge loopback-factory (assoc opts :size-hint size-hint))))
