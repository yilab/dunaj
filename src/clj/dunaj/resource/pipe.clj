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

(ns dunaj.resource.pipe
  "Pipe resources. Works across threads, not processes nor over the
  network.

  Pipes are used to support selectors in custom resources."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [keyword->class set!]]
   [dunaj.host.int :refer [iint iloop iadd]]
   [dunaj.math :refer [Integer max neg?]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [IOpenAware IReference IMutable
                        ensure-io reset! ensure-open open?]]
   [dunaj.flow :refer [let loop recur if do cond when when-let]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.poly :refer [reify defprotocol deftype defrecord]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous
     reduced? -reduce-batched provide-collection assoc]]
   [dunaj.function :refer [fn defn]]
   [dunaj.coll.helper :refer []]
   [dunaj.host.array :refer [array]]
   [dunaj.host.batch :refer [select-item-type]]
   [dunaj.concurrent.thread :refer
    [Thread current-thread ensure-thread-local]]
   [dunaj.string :refer [string?]]
   [dunaj.error :refer [IException IFailAware IFailable try catch
                        throw fail! error fragile opened-fragile]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.recipe :refer [keep]]
   [dunaj.coll.util :refer [merge]]
   [dunaj.coll.default]
   [dunaj.coll.tuple :refer [pair]]
   [dunaj.resource :refer
    [IImmutableReadable IReleasable IFlushable IReadable
     IAcquirableFactory IWritable ISeekable acquire!]]
   [dunaj.resource.helper :refer
    [readable-resource-recipe basic-write! defreleasable]]
   [dunaj.resource.selector :refer
    [ISelectable register* deregister*]]))


;;;; Implementation details

(def ^:private default-pipe-batch-size :- Integer
  "Default size for pipe batch."
  8192)

(defn ^:private provide-pipe-batch-size :- Integer
  "Returns pipe batch size taking into account given batch size hint."
  [size-hint :- (Maybe Integer)]
  (max (or size-hint 0) default-pipe-batch-size))

(defreleasable ^:private SourceResource
  "Source pipe resource type."
  [ch :- java.nio.channels.ReadableByteChannel, batch-size :- Integer,
   config :- {}, ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen ch)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IReleasable
  (-release! [this] (fragile this (.close ch)) true)
  ISelectable
  (-register! [this selector interests user-map]
    (register* selector this interests user-map ch))
  (-deregister! [this selector]
    (deregister* selector this ch))
  IReadable
  (-read! [this]
    (readable-resource-recipe this ch batch-size (current-thread))))

(defreleasable ^:private SinkResource
  "Sink pipe resource type."
  [ch :- java.nio.channels.WritableByteChannel, batch-size :- Integer,
   config :- {}, ^:volatile-mutable, error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen ch)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IReleasable
  (-release! [this] (fragile this (.close ch)) true)
  ISelectable
  (-register! [this selector interests user-map]
    (register* selector this interests user-map ch))
  (-deregister! [this selector]
    (deregister* selector this ch))
  IWritable
  (-write! [this coll]
    (basic-write! this ch batch-size (current-thread) coll)))

(defrecord SinkResourceFactory
  "Factory type for sink pipe resources."
  [batch-size selector-provider pipe non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-pipe-batch-size batch-size)
          config (assoc this :batch-size batch-size)
          ch (.sink ^java.nio.channels.Pipe pipe)]
      (when non-blocking? (.configureBlocking ch false))
      (->SinkResource ch batch-size config nil))))

(defrecord SourceResourceFactory
  "Factory type for source pipe resources."
  [batch-size selector-provider pipe non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-pipe-batch-size batch-size)
          config (assoc this :batch-size batch-size)
          ch (.source ^java.nio.channels.Pipe pipe)]
      (when non-blocking? (.configureBlocking ch false))
      (->SourceResource ch batch-size config nil))))

(defrecord PipeResourceFactory
  "Factory type for pipe resources."
  [batch-size selector-provider non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          pipe (.openPipe ^java.nio.channels.spi.SelectorProvider sp)
          sink (acquire! (->SinkResourceFactory
                          batch-size sp pipe non-blocking?))
          source (acquire! (->SourceResourceFactory
                            batch-size sp pipe non-blocking?))]
      (pair sink source))))


;;;; Public API

(def pipe-factory :- IAcquirableFactory
  "Pipe resource factory.
  Current options are:

  * `:batch-size` - batch size, low level
  * `:selector-provider` - `nil` or host specific selector provider
  * `:non-blocking?` - `nil` (default) or `true` if non-blocking mode
    is requested."
  {:added v1
   :see '[pipe]}
  (->PipeResourceFactory nil nil false))

(defn pipe :- IAcquirableFactory
  "Returns pipe factory with given `_opts_` set. `acquire!` will
  return pair of sink and source resources."
  {:added v1
   :see '[pipe-factory]}
  [& {:as opts}]
  (merge pipe-factory opts))
