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

(ns dunaj.resource.host
  "Classpath and stream backed host resources."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [clojure.core.async]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer [Boolean and or not]]
   [dunaj.host :refer [Batch AnyArray ArrayManager Array
                       keyword->class set! class-instance? proxy]]
   [dunaj.host.int :refer [iint iinc i0 i-1 i< iadd]]
   [dunaj.host.array :refer [char-array]]
   [dunaj.math :refer [Integer max neg?]]
   [dunaj.compare :refer [nil? =]]
   [dunaj.state :refer
    [IOpenAware ICloneable io!
     ensure-io open? ensure-open reset! trade! alter!]]
   [dunaj.flow :refer
    [let recur if do cond when when-let when-not loop if-let]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.poly :refer [reify defprotocol deftype defrecord]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous assoc slice first reduce
     item-type reduced? postponed? reduced postponed unsafe-advance!
     advance seq next empty? peek pop conj unsafe-postponed]]
   [dunaj.function :refer [fn defn]]
   [dunaj.coll.helper :refer
    [reduce-with-batched* reduce* advance-fn]]
   [dunaj.host.array :refer [aset-char! array-manager]]
   [dunaj.host.batch :refer [select-item-type provide-batch-size]]
   [dunaj.concurrent.thread :refer
    [Thread IThreadLocal IPassableThreadLocal
     current-thread ensure-thread-local]]
   [dunaj.concurrent.port :refer [<!! chan close! tap!]]
   [dunaj.string :refer [String string?]]
   [dunaj.uri :refer [Uri uri uri? absolute? resolve]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.state.basic :refer [local atom]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.error :refer
    [IException IFailAware IFailable
     throw error fragile illegal-argument unsupported-operation]]
   [dunaj.coll.tuple :refer [pair]]
   [dunaj.coll.util :refer [merge]]
   [dunaj.coll.default :refer [empty-que]]
   [dunaj.buffer :refer [dropping-buffer]]
   [dunaj.resource :refer
    [IImmutableReadable IReleasable IReadable IAcquirableFactory
     IFlushable IWritable acquire!]]
   [dunaj.resource.helper :refer
    [register-factory! readable-resource-recipe defreleasable
     basic-write!]]
   [dunaj.resource.collwriter]
   [dunaj.resource.collreader]))


;;;; Implementation details

(def ^:private default-host-batch-size :- Integer
  "Default size for host batch."
  8192)

(defn ^:private provide-host-batch-size :- Integer
  "Returns host batch size taking into account given batch
  size hint."
  [size-hint :- (Maybe Integer)]
  (provide-batch-size (max (or size-hint 0) default-host-batch-size)))

(defn ^:private classpath-channel
  :- java.nio.channels.ReadableByteChannel
  "Returns NIO ReadableByteChannel based on given `class-loader`
  and `x`."
  [class-loader :- (Maybe java.lang.ClassLoader), x :- (U String Uri)]
  (let [x (uri x)
        strip-slash #(if (= \/ (first %)) (slice % 1) %)
        p (strip-slash (.getSchemeSpecificPart x))]
    (when-let [scheme (.getScheme x)]
      (when-not (= "cp" scheme)
        (throw (illegal-argument "scheme is not supported"))))
    (java.nio.channels.Channels/newChannel
     (if (nil? class-loader)
       (java.lang.ClassLoader/getSystemResourceAsStream p)
       (.getResourceAsStream class-loader p)))))

(defn ^:private classpath-uri :- Uri
  "Returns URI based on given `class-loader` and `x`."
  [class-loader :- (Maybe java.lang.ClassLoader), x :- (U String Uri)]
  (let [strip-slash #(if (= \/ (first %)) (slice % 1) %)
        x (strip-slash (.getSchemeSpecificPart (uri x)))]
    (when-let [url (if (nil? class-loader)
                     (java.lang.ClassLoader/getSystemResource x)
                     (.getResource class-loader x))]
      (.toURI url))))

(declare ->ClasspathResourceImmutableReader)

(defreleasable ClasspathResourceImmutableReader
  "Reads always from the begining of the classpath resource.
  Passable thread local."
  [class-loader :- (Maybe java.lang.ClassLoader), x :- (U String Uri),
   batch-size :- (Maybe Integer),
   ^:volatile-mutable rch
   :- (Maybe java.nio.channels.ReadableByteChannel),
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)]
  IAcquirableFactory
  (-acquire! [this] this)
  IReleasable
  (-release! [this] (fragile this (when rch (.close rch))) nil)
  IOpenAware
  (-open? [this] (nil? error))
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
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (ensure-thread-local thread)
    (ensure-open this)
    (fragile this (when rch (.close rch)))
    (set! rch (classpath-channel class-loader x))
    (let [st (select-item-type requested-type (keyword->class :byte))
          batch-size (provide-host-batch-size
                      (max (or size-hint 0) (or batch-size 0)))
          batch ^java.nio.ByteBuffer
          (java.nio.ByteBuffer/allocateDirect batch-size)
          af (advance-fn [ret]
              (do (.clear batch)
                  (if (neg? (fragile this (.read rch batch)))
                    ret
                    (recur (reducef ret (.flip batch))))))]
      (af init))))

(defreleasable ^:private ClasspathResource
  "Classpath resource type. Passable thread local."
  [^:volatile-mutable rch
   :- (Maybe java.nio.channels.ReadableByteChannel),
   batch-size :- (Maybe Integer), config :- {},
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen rch)))
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
  (-release! [this] (fragile this (.close rch)) nil)
  IReadable
  (-read! [this]
    (readable-resource-recipe this rch batch-size thread)))

(defrecord ClasspathResourceFactory
  "Factory type for classpath resources supporting immutable reads."
  [uri batch-size class-loader]
  IImmutableReadable
  (-read [this]
    (acquire! (->ClasspathResourceImmutableReader
               (or class-loader (clojure.lang.RT/baseLoader))
               uri (provide-host-batch-size batch-size)
               nil (current-thread) nil)))
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-host-batch-size batch-size)
          class-loader (or class-loader (clojure.lang.RT/baseLoader))
          rch (classpath-channel class-loader uri)]
      (->ClasspathResource
       rch batch-size
       (assoc this
         :uri (classpath-uri class-loader uri)
         :batch-size batch-size
         :class-loader class-loader)
       (current-thread) nil))))

(defreleasable ^:private OStreamResource
  "OutputStream backed resource type. Passable thread local."
  [wch :- (Maybe java.nio.channels.WritableByteChannel),
   stream :- java.io.OutputStream, batch-size :- (Maybe Integer),
   config :- {}, keep-open? :- Boolean,
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen wch)))
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
    (when-not keep-open? (fragile this (.close wch))) nil)
  IFlushable
  (-flush! [this] (.flush stream))
  IWritable
  (-write! [this coll]
    (basic-write! this wch batch-size thread coll)))

(defrecord OStreamResourceFactory
  "Factory type for OutputStream backed resources."
  [stream :- java.io.OutputStream, batch-size :- (Maybe Integer),
   keep-open? :- Boolean]
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-host-batch-size batch-size)
          wch (java.nio.channels.Channels/newChannel stream)]
      (->OStreamResource
       wch stream batch-size (assoc this :batch-size batch-size)
       keep-open? (current-thread) nil))))

(defreleasable ^:private IStreamResource
  "InputStream backed resource type. Passable thread local."
  [rch :- (Maybe java.nio.channels.ReadableByteChannel),
   batch-size :- (Maybe Integer), config :- {}, keep-open? :- Boolean,
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (.isOpen rch)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread))
  IReleasable
  (-release! [this]
    (when-not keep-open? (fragile this (.close rch))) nil)
  IReadable
  (-read! [this]
    (readable-resource-recipe this rch batch-size thread)))

(defrecord IStreamResourceFactory
  "Factory type for InputStream backed resources."
  [stream :- java.io.InputStream, batch-size :- (Maybe Integer),
   keep-open? :- Boolean]
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-host-batch-size batch-size)
          rch (java.nio.channels.Channels/newChannel stream)]
      (->IStreamResource
       rch batch-size (assoc this :batch-size batch-size)
       keep-open? (current-thread) nil))))

(defreleasable ^:private WriterResource
  "Writer backed resource type. Passable thread local."
  [writer :- java.io.Writer, batch-size :- (Maybe Integer),
   config :- {}, keep-open? :- Boolean,
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable opened? :- Boolean,
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) opened?))
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
    (set! opened? false)
    (when-not keep-open? (fragile this (.close writer)))
    nil)
  IFlushable
  (-flush! [this] (.flush writer))
  IWritable
  (-write! [this coll]
    (ensure-thread-local thread)
    (ensure-open this)
    ;; HACK around emacs flushing printing newline
    (.write writer (char-array coll))
    ;; TODO: make it faster
    #_(reduce #(do (.write writer (iint %2)) (iinc %)) (i0) coll)))

(defrecord WriterResourceFactory
  "Factory type for Writer backed resources."
  [writer :- java.io.Writer, batch-size :- (Maybe Integer),
   keep-open? :- Boolean]
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-host-batch-size batch-size)]
      (->WriterResource
       writer batch-size (assoc this :batch-size batch-size)
       keep-open? (current-thread) true nil))))

(deftype ReaderResourceReader
  "Reads from the reader resource. Passable thread local."
  [reader :- java.io.Reader,
   batch-size :- (Maybe Integer),
   resource :- (I IFailable IOpenAware),
   ^:volatile-mutable thread :- (Maybe Thread)]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  IHomogeneous
  (-item-type [this] (keyword->class :char))
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (ensure-io)
    (ensure-thread-local thread)
    (ensure-open resource)
    (let [st (select-item-type requested-type (item-type this))
          batch-size (provide-batch-size
                      (max (or size-hint 0) (or batch-size 0)))
          batch ^java.nio.CharBuffer
          (java.nio.CharBuffer/allocate batch-size)
          af (fn af [ret]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed @ret #(af (unsafe-advance! ret)))
                :else
                (let [b :- (Batch java.lang.Character) (.clear batch)
                      x (fragile resource (.read reader b))]
                  (if (neg? x)
                    ret
                    (recur (reducef ret (.flip batch)))))))]
      (af init))))

(defreleasable ^:private ReaderResource
  "Reader backed resource type. Passable thread local."
  [reader :- java.io.Reader, batch-size :- (Maybe Integer),
   config :- {}, keep-open? :- Boolean,
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable opened? :- Boolean,
   ^:volatile-mutable error :- (Maybe IException)]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) opened?))
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
    (set! opened? false)
    (when-not keep-open? (fragile this (.close reader)))
    nil)
  IReadable
  (-read! [this]
    (->ReaderResourceReader reader batch-size this thread)))

(defrecord ReaderResourceFactory
  "Factory type for Reader backed resources."
  [reader :- java.io.Reader, batch-size :- (Maybe Integer),
   keep-open? :- Boolean]
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-host-batch-size batch-size)]
      (->ReaderResource
       reader batch-size (assoc this :batch-size batch-size)
       keep-open? (current-thread) true nil))))

(deftype CollWriter
  [done?-ref data-ref watcher-ch]
  IOpenAware
  (-open? [this] (or (not @done?-ref) (not (empty? @data-ref))))
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IHomogeneous
  (-item-type [this] (keyword->class :char))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [st (select-item-type requested-type (keyword->class :char))
          af (fn af [ret]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed @ret #(af (unsafe-advance! ret)))
                (empty? @data-ref)
                (cond
                 @done?-ref ret
                 watcher-ch (do (<!! watcher-ch) (recur ret))
                 :else (unsafe-postponed ret #(af ret)))
                :else
                (recur
                 (if-let [arr :- (Array java.lang.Character)
                          (peek (trade! data-ref
                                        #(if (empty? %) % (pop %))))]
                   (reducef ret (java.nio.CharBuffer/wrap arr))
                   ret))))]
      (io! (af init)))))

(defmacro gen-coll-writer
  [done?-ref data-ref watcher-ch]
  (if clojure.core/*compile-files*
    `(dunaj.resource.CollWriter. ~done?-ref ~data-ref ~watcher-ch)
    `(throw (unsupported-operation "supported only in AOT"))))

(defmacro gen-coll-reader
  [coll]
  (if clojure.core/*compile-files*
    `(dunaj.resource.CollReader. ~coll)
    `(throw (unsupported-operation "supported only in AOT"))))


;;;; Public API

(def classpath-factory :- (I IImmutableReadable IAcquirableFactory)
  "Classpath resource factory. Passable thread local.
  Current options are:

  * `uri` - uri
  * `batch-size` - batch size, low level
  * `class-loader` - `nil` or host specific class loader"
  {:added v1
   :see '[classpath]}
  (->ClasspathResourceFactory nil nil nil))

(defn classpath :- (I IImmutableReadable IAcquirableFactory)
  "Returns factory for passable thread local classpath resource,
  with given `_uri_` and `_opts_` set."
  {:added v1
   :see '[classpath-factory]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge classpath-factory (assoc opts :uri uri)))

(def output-stream-factory :- IAcquirableFactory
  "OutputStream backed resource factory. Passable thread local.
  Current options are:

  * `stream` - output stream
  * `batch-size` - batch-size, low level
  * `keep-open?` - boolean, default `false` -
    keep stream open after resource is released?"
  {:added v1
   :see '[output-stream input-stream-factory]}
  (->OStreamResourceFactory nil nil false))

(defn output-stream :- IAcquirableFactory
  "Returns factory for passable thread local OutputStream backed
  resource, with given `_stream_` and `_opts_` set."
  {:added v1
   :see '[output-stream-factory input-stream]}
  [stream :- java.io.OutputStream & {:as opts}]
  (merge output-stream-factory (assoc opts :stream stream)))

(def input-stream-factory :- IAcquirableFactory
  "InputStream backed resource factory. Passable thread local.
  Current options are:

  * `stream` - input stream
  * `batch-size` - batch-size, low level
  * `keep-open?` - boolean, default `false` -
    keep stream open after resource is released?"
  {:added v1
   :see '[input-stream output-stream-factory]}
  (->IStreamResourceFactory nil nil false))

(defn input-stream :- IAcquirableFactory
  "Returns factory for passable thread local InputStream backed
  resource, with given `_stream_` and `opts` set."
  {:added v1
   :see '[input-stream-factory output-stream]}
  [stream :- java.io.InputStream & {:as opts}]
  (merge input-stream-factory (assoc opts :stream stream)))

(def writer-factory :- IAcquirableFactory
  "Writer backed resource factory. Passable thread local.
  Current options are:

  * `writer` - writer
  * `batch-size` - batch-size, low level
  * `keep-open?` - boolean, default `false` -
    keep stream open after resource is released?"
  {:added v1
   :see '[writer reader-factory]}
  (->WriterResourceFactory nil nil false))

(defn writer :- IAcquirableFactory
  "Returns factory for passable thread local Writer backed
  resource, with given `_wr_` and `_opts_` set."
  {:added v1
   :see '[reader writer-factory]}
  [wr :- java.io.Writer & {:as opts}]
  (merge writer-factory (assoc opts :writer wr)))

(def reader-factory :- IAcquirableFactory
  "Reader backed resource factory. Passable thread local.
  Current options are:

  * `reader` - reader
  * `batch-size` - batch-size, low level
  * `keep-open?` - boolean, default `false` -
    keep stream open after resource is released?"
  {:added v1
   :see '[reader writer-factory]}
  (->ReaderResourceFactory nil nil false))

(defn reader :- IAcquirableFactory
  "Returns factory for passable thread local Reader backed
  resource, with given `_rd_` and `_opts_` set."
  {:added v1
   :see '[reader-factory writer]}
  [rd :- java.io.Reader & {:as opts}]
  (merge reader-factory (assoc opts :reader rd)))

(defn coll-reader :- java.io.Reader
  "Returns `java.io.Reader` object from a given `_coll_`."
  {:added v1
   :see '[coll-writer]}
  [coll :- IRed]
  (gen-coll-reader coll))

(defn coll-writer :- []
  "Returns pair of writer object and a mutable collection recipe
  containing data from that writer.

  Returned collection may return postponed object if
  `_non-blocking?_` is `true` (defaults to `false`), otherwise it
  will block until more data is sent to the writer or writer is
  closed."
  {:added v1
   :see '[coll-reader]}
  ([] (coll-writer false))
  ([non-blocking? :- Boolean]
     (let [done?-ref (atom false)
           data-ref (atom empty-que)
           watcher-ch (when-not non-blocking?
                        (let [ch (chan (dropping-buffer 1))]
                          (tap! data-ref ch)
                          ch))
           wr (gen-coll-writer done?-ref data-ref watcher-ch)
           coll (->CollWriter done?-ref data-ref watcher-ch)]
       (pair wr coll))))

(register-factory! "cp" classpath-factory)
