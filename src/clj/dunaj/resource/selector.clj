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

(ns dunaj.resource.selector
  "Selector resources."
  {:authors ["Jozef Wagner"]
   :categories ["Primary"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer [Boolean and or not]]
   [dunaj.host :refer [keyword->class set!]]
   [dunaj.host.int :refer [Int iint iadd izero? iand ipos? ior i0]]
   [dunaj.math :refer [Integer max neg?]]
   [dunaj.bit :as bit]
   [dunaj.compare :refer [nil? identical?]]
   [dunaj.state :refer [IOpenAware IReference IMutable ICloneable
                        ensure-io reset! ensure-open open?]]
   [dunaj.flow :refer [let loop recur if do cond when condp when-let]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.poly :refer [reify defprotocol deftype defrecord]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous postponed? postponed
     unsafe-advance! reduce unsafe-postponed
     reduced? -reduce-batched provide-collection assoc conj]]
   [dunaj.function :refer [fn defn]]
   [dunaj.coll.helper :refer []]
   [dunaj.host.array :refer [array]]
   [dunaj.host.batch :refer [select-item-type]]
   [dunaj.time :refer [IDuration milliseconds]]
   [dunaj.concurrent.thread :refer
    [IThreadLocal IPassableThreadLocal Thread
     current-thread ensure-thread-local]]
   [dunaj.string :refer [string?]]
   [dunaj.error :refer
    [IException IFailAware IFailable try catch throw fail! error
     fragile opened-fragile unsupported-operation]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.recipe :refer [keep]]
   [dunaj.coll.util :refer [merge]]
   [dunaj.coll.default]
   [dunaj.coll.tuple :refer [pair tuple]]
   [dunaj.resource :refer
    [IImmutableReadable IReleasable IFlushable IReadable
     IAcquirableFactory IWritable ISeekable acquire!]]
   [dunaj.resource.helper :refer
    [readable-resource-recipe basic-write! defreleasable]]))


;;;; Implementation details

(defn ^:private selector-key->map :- {}
  [sk :- java.nio.channels.SelectionKey]
  (let [ready-flag (.readyOps sk)
        ready ()
        ready (if (izero?
                   (iand ready-flag
                         java.nio.channels.SelectionKey/OP_ACCEPT))
                ready (conj ready :accept))
        ready (if (izero?
                   (iand ready-flag
                         java.nio.channels.SelectionKey/OP_CONNECT))
                ready (conj ready :connect))
        ready (if (izero?
                   (iand ready-flag
                         java.nio.channels.SelectionKey/OP_READ))
                ready (conj ready :read))
        ready (if (izero?
                   (iand ready-flag
                         java.nio.channels.SelectionKey/OP_WRITE))
                ready (conj ready :write))]
    (assoc (or (.attachment sk) {}) :ready ready)))

(deftype ReadableSelectorRecipe
  "Reads from the selector."
  [sel :- java.nio.channels.Selector,
   resource :- (I IFailable IOpenAware),
   ^:volatile-mutable thread :- (Maybe Thread)]
  IRed
  (-reduce [this reducef init]
    (ensure-io)
    (ensure-thread-local thread)
    (ensure-open resource)
    (let [sf #(fragile resource
                       (when (.isOpen sel)
                         (.iterator (.selectedKeys sel))))
          af (fn af [ret iter :- (Maybe java.util.Iterator),
                     wait? :- Boolean]
               (cond
                 (reduced? ret) ret
                 (postponed? ret)
                 (unsafe-postponed
                  @ret #(af (unsafe-advance! ret) iter false))
                 wait? (unsafe-postponed ret #(af ret (sf) false))
                (nil? iter) ret
                (.hasNext iter)
                (let [sk :- java.nio.channels.SelectionKey
                      (fragile resource (.next iter))
                      m (selector-key->map sk)]
                  (fragile resource (.remove iter))
                  (recur (reducef ret m) iter false))
                (.isOpen sel) (recur ret iter true)
                :else ret))]
      (af init (sf) false)))
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this))

(defprotocol ISelector
  (-select :- Integer [this timeout-ms :- Integer])
  (-select-now :- Integer [this]))

(defreleasable ^:private SelectorResource
  "Selector resource type."
  [sel :- java.nio.channels.Selector,
   config :- {},
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen sel)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IReleasable
  (-release! [this] (fragile this (.close sel)) true)
  IReadable
  (-read! [this]
    (->ReadableSelectorRecipe sel this (current-thread)))
  ISelector
  (-select [this timeout-ms]
    (let [i (iint timeout-ms)]
      (if (ipos? i)
        (opened-fragile this (.select sel i))
        (opened-fragile this (.select sel)))))
  (-select-now [this]
    (opened-fragile this (.selectNow sel))))

(defrecord SelectorResourceFactory
  "Factory type for selector resources."
  [selector-provider]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          sel
          (.openSelector ^java.nio.channels.spi.SelectorProvider sp)
          r (assoc this :selector-provider sp)]
      (->SelectorResource sel r nil))))

(def ^:const ^:private ALL_OPTS
  (iint
   (bit/or java.nio.channels.SelectionKey/OP_READ
           java.nio.channels.SelectionKey/OP_WRITE
           java.nio.channels.SelectionKey/OP_ACCEPT
           java.nio.channels.SelectionKey/OP_CONNECT)))

(defn ^:private interests->selector-opts :- Int
  "Returns host specific selector opts based on interest collection."
  [ch :- java.nio.channels.SelectableChannel, interests :- IRed]
  (let [rf (fn [ret val]
             (condp identical? val
               :all
               (iand (ior ret ALL_OPTS) (.validOps ch))
               :read
               (ior ret java.nio.channels.SelectionKey/OP_READ)
               :write
               (ior ret java.nio.channels.SelectionKey/OP_WRITE)
               :accept
               (ior ret java.nio.channels.SelectionKey/OP_ACCEPT)
               :connect
               (ior ret java.nio.channels.SelectionKey/OP_CONNECT)
               ret))]
    (reduce rf (i0) interests)))


;;;; Public API

(defprotocol ISelectable
  "A protocol for resources that can be registered in a selector."
  {:added v1
   :category "Primary"
   :see '[register! deregister!]}
  (-register! :- nil
    "Registers `_this_` resource with `_selector_`, using
    `_interests_` collection of keywords as an interest set.
    `_user-map_` must be returned by the selector when the
    resource will be ready for one of given interest operations.
    Returns `nil`.
    Must throws when resource is not in non-blocking mode."
    [this selector :- SelectorResource,
     interests :- IRed, user-map :- {}])
  (-deregister! :- nil
    "Deregisters `_this_` resource from `_selector_`. Returns `nil`."
    [this selector :- SelectorResource]))

(defn register* :- nil
  "A helper function that registers given host selectable channel
  `_ch_` within `_selector_` with given `_interests_`
  and `_user-map_`."
  {:added v1
   :see '[register!]}
  [selector :- SelectorResource, resource :- ISelectable
   interests :- IRed, user-map :- {},
   ch :- java.nio.channels.SelectableChannel]
  (fragile resource
           (.register ch ^java.nio.channels.Selector (.-sel selector)
                      (interests->selector-opts ch interests)
                      (assoc (or user-map {}) :resource resource))))

(defn deregister* :- nil
  "A helper function that deregisters given channel `_ch_` from
  `_selector_`."
  {:added v1
   :see '[deregister!]}
  [selector :- SelectorResource, resource :- ISelectable,
   ch :- java.nio.channels.SelectableChannel]
  (fragile resource
           (when-let [k (.keyFor ch (.-sel selector))]
             (.cancel ^java.nio.channels.SelectionKey k))))

(def selector-factory :- IAcquirableFactory
  "Selector resource factory."
  {:added v1
   :category "Primary"}
  (->SelectorResourceFactory nil))

(defn selector :- IAcquirableFactory
  "Returns a selector factory with given `_opts_` set."
  {:added v1
   :see '[select register!]
   :category "Primary"}
  [& {:as opts}]
  (merge selector-factory opts))

(defn select :- Integer
  "Returns number of ready resources among ones registered within
  `_selector_`. Blocks until some resources are ready or until
  `_timeout_` is reached."
  {:added v1
   :see '[select-now selector register!]
   :category "Primary"}
  ([selector :- SelectorResource]
   (select selector nil))
  ([selector :- SelectorResource,
    timeout :- (U nil Integer IDuration)]
   (-select selector (milliseconds timeout))))

(defn select-now :- Integer
  "Returns number of ready resources among ones registered within
  `_selector_`. Returns immediatelly."
  {:added v1
   :see '[select selector register]
   :category "Primary"}
  [selector :- SelectorResource]
  (-select-now selector))

(defn register! :- nil
  "Registers a `_resource_` with `_selector_`, using `_interests_`
  collection of keywords as an interest set. May supply `_user-map_`
  which will be returned by the selector when the resource will be
  ready for one of given interest operations. Returns `nil`.
  Throws when `_resource_` is not in non-blocking mode.

  Supported interest operations are host specific.
  JVM defines following interest operations:

  * `:any` - watch for any operation
  * `:read` - resource is ready for reading
  * `:write` - resource is ready for writing
  * `:accept` - resource is ready to accept a connection
  * `:connect` - resource has been successfully connected."
  {:added v1
   :see '[deregister! select selector]
   :category "Primary"}
  ([selector :- SelectorResource, resource :- ISelectable]
   (-register! resource selector (tuple :any) nil))
  ([selector :- SelectorResource, resource :- ISelectable
    interests :- IRed]
   (-register!
    resource selector (provide-collection interests) nil))
  ([selector :- SelectorResource, resource :- ISelectable
    interests :- IRed, user-map :- {}]
   (-register!
    resource selector (provide-collection interests) user-map)))

(defn deregister! :- nil
  "Deregisters a `_resource_` from `_selector_`. Returns `nil`."
  {:added v1
   :see '[register! select selector]
   :category "Primary"}
  [selector :- SelectorResource, resource :- ISelectable]
  (-deregister! resource selector))
