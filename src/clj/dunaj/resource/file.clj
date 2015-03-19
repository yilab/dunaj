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

(ns dunaj.resource.file
  "Host filesystem resources."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [AnyArray keyword->class set! class-instance?]]
   [dunaj.host.int :refer [iint iloop iadd i0]]
   [dunaj.math :refer [Integer max neg?]]
   [dunaj.host.number :refer [long]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [IOpenAware IReference IMutable ICloneable
                        ensure-io reset! open? ensure-open]]
   [dunaj.flow :refer [let loop recur if do cond when]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.poly :refer [reify defprotocol deftype defrecord]]
   [dunaj.coll :refer [IRed ICounted IBatchedRed IHomogeneous reduced?
                       -reduce-batched provide-collection assoc]]
   [dunaj.function :refer [fn defn]]
   [dunaj.coll.helper :refer [reduce-with-batched* advance-fn]]
   [dunaj.host.array :refer [array]]
   [dunaj.host.batch :refer [select-item-type provide-batch-size]]
   [dunaj.concurrent.thread :refer
    [Thread IThreadLocal IPassableThreadLocal
     current-thread ensure-thread-local]]
   [dunaj.string :refer [String string?]]
   [dunaj.uri :refer [Uri uri uri? absolute? resolve]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.error :refer [IException IFailAware IFailable opened-fragile
                        try catch throw fail! error fragile]]
   [dunaj.coll.recipe :refer [keep]]
   [dunaj.coll.util :refer [merge]]
   [dunaj.resource :refer
    [IImmutableReadable IReleasable IFlushable IReadable
     IAcquirableFactory IWritable ISeekable acquire!]]
   [dunaj.resource.helper :refer
    [register-factory! basic-write!
     readable-resource-recipe defreleasable]]))


;;;; Implementation details

(def ^:private default-file-batch-size :- Integer
  "Default size for file batch."
  8192)

(defn ^:private provide-file-batch-size :- Integer
  "Returns file batch size taking into account given batch size hint."
  [size-hint :- (Maybe Integer)]
  (provide-batch-size (max (or size-hint 0) default-file-batch-size)))

(defn ^:private get-fs :- java.nio.file.FileSystem
  "Returns filesystem based on `x`."
  [x :- Any]
  (cond (class-instance? java.nio.file.FileSystem x) x
        (nil? x) (java.nio.file.FileSystems/getDefault)
        :else (java.nio.file.FileSystems/getFileSystem (uri x))))

(defn ^:private to-path :- java.nio.file.Path
  "Returns NIO Path object based on given string or uri `x`.
  Relative paths are resolved in the current working directory."
  [fs :- java.nio.file.FileSystem, x :- (U String Uri),
   wd :- (U nil String Uri)]
  (let [x (uri x)
        wduri (uri (or wd "."))]
    (cond (absolute? x) (java.nio.file.Paths/get x)
          (absolute? wduri)
          (.resolve (java.nio.file.Paths/get wduri) (.toString x))
          :else (let [ea (array java.lang.String [])
                      rp (.getPath fs (.toString wduri) ea)]
                  (.resolve rp (.toString x))))))

(def ^:private oom :- KeywordMap
  "A Keyword -> OpenOption translation map."
  {:append java.nio.file.StandardOpenOption/APPEND
   :create java.nio.file.StandardOpenOption/CREATE
   :new java.nio.file.StandardOpenOption/CREATE_NEW
   :delete java.nio.file.StandardOpenOption/DELETE_ON_CLOSE
   :dsync java.nio.file.StandardOpenOption/DSYNC
   :read java.nio.file.StandardOpenOption/READ
   :sparse java.nio.file.StandardOpenOption/SPARSE
   :sync java.nio.file.StandardOpenOption/SYNC
   :truncate java.nio.file.StandardOpenOption/TRUNCATE_EXISTING
   :write java.nio.file.StandardOpenOption/WRITE})

(defn ^:private translate-oos :- AnyArray
  "Returns array of OpenOption objects based on given collection of
  keyword modes in `coll`."
  [coll :- IRed]
  (array java.nio.file.OpenOption (keep #(or (oom %) %) coll)))

(defn ^:private file-channel :- java.nio.channels.FileChannel
  "Returns NIO FileChannel based on given `path` and open `mode`s."
  [path :- java.nio.file.Path, mode :- IRed]
  (java.nio.channels.FileChannel/open
   path (translate-oos (provide-collection mode))))

(declare ->FileResourceImmutableReader)

(defreleasable FileResourceImmutableReader
  "Reads always from the begining of the file. Passable thread local."
  [fch :- java.nio.channels.FileChannel,
   batch-size :- (Maybe Integer),
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)]
  IAcquirableFactory
  (-acquire! [this] this)
  IReleasable
  (-release! [this] (fragile this (.close fch)) nil)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen fch)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  ICounted
  (-count [this]
    (ensure-thread-local thread)
    (ensure-open this)
    (fragile this (.size fch)))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (ensure-thread-local thread)
    (ensure-open this)
    (fragile this (.position fch (i0)))
    (let [st (select-item-type requested-type (keyword->class :byte))
          batch-size (provide-file-batch-size
                      (max (or size-hint 0) (or batch-size 0)))
          batch ^java.nio.ByteBuffer
          (java.nio.ByteBuffer/allocateDirect batch-size)
          af (advance-fn [ret]
              (do (.clear batch)
                  (if (neg? (fragile this (.read fch batch)))
                    ret
                    (recur (reducef ret (.flip batch))))))]
      (af init))))

(defprotocol ITruncatable
  "A protocol for truncatable resources."
  {:added v1
   :see '[truncate!]}
  (-truncate! :- nil
    "Truncates `_this_` file resource to a given `_size_`,
    returning `nil`."
    [this size :- Integer]))

(defreleasable ^:private FileResource
  "File resource type. Passable thread local."
  [fch :- java.nio.channels.FileChannel,
   batch-size :- (Maybe Integer), config :- {},
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen fch)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  IReleasable
  (-release! [this]
    (when (open? this)
      (try
        (.force fch true)
        (catch java.lang.Exception e
          (fail! this e) (.close fch) (throw e))))
    (fragile this (.close fch))
    nil)
  IFlushable
  (-flush! [this]
    (ensure-thread-local thread)
    (ensure-open this)
    (fragile this (.force fch true))
    nil)
  IReadable
  (-read! [this]
    (readable-resource-recipe this fch batch-size thread))
  IWritable
  (-write! [this coll]
    (basic-write! this fch batch-size thread coll))
  ITruncatable
  (-truncate! [this size]
    (ensure-thread-local thread)
    (ensure-open this)
    (fragile this (.truncate fch (long size)))
    nil)
  ISeekable
  (-size [this]
    (ensure-thread-local thread)
    (ensure-open this)
    (fragile this (.size fch)))
  (-position [this]
    (reify
      IReference
      (-deref [_]
        (ensure-thread-local thread)
        (ensure-open this)
        (fragile this (.position fch)))
      IMutable
      (-reset! [_ nval]
        (ensure-thread-local thread)
        (ensure-open this)
        (fragile this (.position fch (long nval))) nval))))

(defrecord FileResourceFactory
  "Factory type for file resources supporting immutable reads."
  [uri mode batch-size file-system working-directory]
  IImmutableReadable
  (-read [this]
    (let [fs (get-fs file-system)]
      (acquire! (->FileResourceImmutableReader
                 (file-channel (to-path fs uri working-directory) nil)
                 (provide-file-batch-size batch-size)
                 (current-thread) nil))))
  IAcquirableFactory
  (-acquire! [this]
    (let [fs (get-fs file-system)
          path (to-path fs uri working-directory)
          batch-size (provide-file-batch-size batch-size)]
      (->FileResource
       (file-channel path mode) batch-size
       (assoc this
         :uri (.toUri path)
         :batch-size batch-size
         :file-system fs)
       (current-thread) nil))))


;;;; Public API

(def file-factory :- (I IImmutableReadable IAcquirableFactory)
  "A file resource factory. Passable thread local.
  Current options are:

  * `:uri` - file uri
  * `:mode` - host specific, usually a combination of `:append`,
    `:create`, `:new`, `:delete`, `:dsync`, `:read`, `:sparse`,
    `:sync`, `:truncate` and `:write`
  * `:batch-size` - default batch size, low level
  * `:file-system` - `nil` or host specifis filesystem object
  * `:working-directory` - `nil` or URI of a working directory

  Use `config` to get configuration parameters from an acquired file
  resource."
  {:added v1
   :see '[file]}
  (->FileResourceFactory nil [:read :write :create] nil nil nil))

(defn file :- (I IImmutableReadable IAcquirableFactory)
  "Returns factory for passable thread local files, with given
  `_uri_` and `_opts_` set.

  In addition to readable and writable, files are flushable,
  seekable and truncatable."
  {:added v1
   :see '[file-factory dunaj.resource/resource]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge file-factory (assoc opts :uri uri)))

(defn truncate! :- nil
  "Truncates given resource `_res_` to `_size_`. Returns `nil`."
  {:added v1}
  [res :- ITruncatable, size :- Integer]
  (-truncate! res size))

(register-factory! nil file-factory)
(register-factory! "file" file-factory)
