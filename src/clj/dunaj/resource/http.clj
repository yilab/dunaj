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

(ns dunaj.resource.http
  "Resource for fetching data through http with very basic
  functionalities."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [clojure.core.async]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer
    [Boolean and or not boolean boolean? true? false?]]
   [dunaj.host :refer [Class BatchManager Batch AnyBatch
                       keyword->class set! class-instance?]]
   [dunaj.host.int :refer [iint iloop iadd ixFF i0 iinc i1]]
   [dunaj.host.number :refer [long]]
   [dunaj.math :refer [Integer integer? max neg? == < zero? nneg?]]
   [dunaj.compare :refer [nil? = identical?]]
   [dunaj.state :refer
    [IOpenAware IReference IMutable IAdjustable ICloneable
     ensure-io reset! adjust! ensure-open io!]]
   [dunaj.flow :refer
    [let loop recur if do cond when-not when condp if-let when-let]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.poly :refer
    [reify defrecord deftype defprotocol satisfies?]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous IUnpackedRed seq single?
     -reduce-unpacked second nth reduced? -reduce-batched rest empty?
     item-type reduce contains? assoc conj postponed? postponed
     unsafe-advance! provide-sequential counted? count first
     -reduce-batched unsafe-postponed]]
   [dunaj.function :refer [fn defn identity apply]]
   [dunaj.coll.helper :refer [reduce-with-batched* reduce*]]
   [dunaj.concurrent.thread :refer
    [Thread IThreadLocal IPassableThreadLocal
     current-thread ensure-thread-local]]
   [dunaj.concurrent.port :refer
    [IMult -tap! -untap! -untap-all! chan put! <!! close!]]
   [dunaj.time :refer [milliseconds]]
   [dunaj.uri :refer [Uri uri? uri]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword keyword name symbol named?]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.default :refer [vec]]
   [dunaj.coll.recipe :refer
    [keep map take-nth partition-by interpose concat]]
   [dunaj.coll.util :refer
    [every? merge merge-with unpacked batched doseq]]
   [dunaj.host.array :refer
    [array array-manager aget byte-array adapt]]
   [dunaj.host.batch :refer
    [provide-batch-size select-item-type batch-manager
     item-types-match?]]
   [dunaj.string :refer [String string? ->str str]]
   [dunaj.error :refer
    [IFailAware IFailable IException
     throw illegal-argument illegal-state fragile io
     opened-fragile fail! try catch unsupported-operation]]
   [dunaj.buffer :refer [dropping-buffer]]
   [dunaj.format :refer [parse]]
   [dunaj.regex]
   [dunaj.resource :refer
    [IImmutableReadable IControllable IFlushable IReadable IWritable
     ISeekable IStatusable IReleasable IAcquirableFactory
     acquire! read!]]
   [dunaj.resource.helper :refer
    [register-factory! defreleasable readable-resource-recipe
     basic-write!]]
   [dunaj.resource.selector :refer
    [ISelectable register* deregister*]]))


;;;; Implementation details

(def ^:private default-http-batch-size :- Integer
  "Default size for http batch."
  8192)

(defn ^:private provide-http-batch-size :- Integer
  "Returns http batch size taking into account given batch size hint."
  [size-hint :- (Maybe Integer)]
  (provide-batch-size (max (or size-hint 0) default-http-batch-size)))

(defn ^:private socket-address :- java.net.InetSocketAddress
  "Returns an instance of a socket address."
  [address :- (Maybe String), port :- (Maybe Integer)]
  (let [port (or port 0)
        address (when address
                  (java.net.InetAddress/getByName address))]
    (java.net.InetSocketAddress.
     ^java.net.InetAddress address (iint port))))

(defprotocol IHttpResource
  (-set-realized! [this])
  (-update-status! [this]))

(deftype ImmutableHttpReader
  [factory]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (io! (-reduce-batched (read! (acquire! factory)) requested-type
                          size-hint reducef init))))

(deftype HttpReader
  "Reads from the http resource. Passable thread local."
  [c :- java.net.URLConnection,
   batch-size :- (Maybe Integer),
   resource :- (I IFailable IOpenAware),
   ^:volatile-mutable thread :- (Maybe Thread)]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (ensure-io)
    (ensure-thread-local thread)
    (ensure-open resource)
    (-set-realized! resource)
    (let [s :- java.io.InputStream
          (fragile resource (.getInputStream c))
          rch (java.nio.channels.Channels/newChannel s)
          st (select-item-type requested-type (item-type this))
          batch-size (provide-batch-size
                      (max (or size-hint 0) (or batch-size 0)))
          batch ^java.nio.ByteBuffer
          (java.nio.ByteBuffer/allocateDirect batch-size)
          af (fn af [ret]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed @ret #(af (unsafe-advance! ret)))
                (.isOpen rch)
                (let [x (fragile resource (.read rch (.clear batch)))]
                  (cond
                   (neg? x) (do (fragile resource (.close s)) ret)
                   (zero? x) (recur ret)
                   :else (recur (reducef ret (.flip batch)))))
                :else ret))]
      (af init))))

(defreleasable ^:private HttpResource
  "Connected HTTP resource type."
  [c :- java.net.URLConnection, batch-size :- Integer,
   config :- {}, ^:volatile-mutable thread :- (Maybe Thread),
   ^:volatile-mutable error :- (Maybe IException)
   ^:volatile-mutable open? :- Boolean,
   ^:volatile-mutable realized? :- Boolean
   ^:volatile-mutable status :- {}]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and (nil? error) (true? open?)))
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
    (set! open? false)
    (when realized? (fragile this (.close (.getInputStream c)))))
  IHttpResource
  (-set-realized! [this] (set! realized? true))
  (-update-status! [this]
    (ensure-thread-local thread)
    (ensure-open this)
    (when (and realized? (nil? status))
      (let [rf (fn [m [k v]] (let [v (seq v)
                                   v (if (single? v) (first v) v)]
                               (assoc m k v)))
            us {:headers (reduce rf {} (seq (.getHeaderFields c)))}
            hs (when (class-instance? java.net.HttpURLConnection c)
                 (let [hc :- java.net.HttpURLConnection c]
                   {:request-method (.getRequestMethod hc)
                    :response-code (.getResponseCode hc)
                    :response-message (.getResponseMessage hc)
                    :proxy? (.usingProxy hc)}))
            ss (when (class-instance?
                      javax.net.ssl.HttpsURLConnection c)
                 (let [sc :- javax.net.ssl.HttpsURLConnection c]
                   {:cipher-suite (.getCipherSuite sc)
                    :local-certificates (.getLocalCertificates sc)
                    :local-principal (.getLocalPrincipal sc)
                    :peer-principal (.getPeerPrincipal sc)}))]
        (set! status (merge us hs ss)))))
  IStatusable
  (-status [this]
    (reify
      IReference
      (-deref [_]
        (ensure-open this)
        (-update-status! this)
        (.-status this))))
  IReadable
  (-read! [this]
    (->HttpReader c batch-size this thread))
  IWritable
  (-write! [this coll]
    (when realized?
      (throw (illegal-state
              "Connection has already been established.")))
    (set! realized? true)
    (fragile this (.setDoOutput c true))
    (when (counted? coll)
      (fragile this
               (.setFixedLengthStreamingMode
                ^java.net.HttpURLConnection c (long (count coll)))))
    (let [s :- java.io.OutputStream
          (fragile this (.getOutputStream c))
          ch (java.nio.channels.Channels/newChannel s)]
      (let [res (basic-write! this ch batch-size thread coll)]
        (fragile this (.close s))
        ;; ensure request has been sent
        (fragile this (.getInputStream c))
        res))))

(def key->proxy-type
  {:direct java.net.Proxy$Type/DIRECT
   :http java.net.Proxy$Type/HTTP
   :socks java.net.Proxy$Type/SOCKS})

(defrecord HttpResourceFactory
  "Factory type for HTTP resources."
  [uri proxy secure? allow-ui? timeout use-caches? follow?
   request-properties request-method chunked-streaming
   hostname-verifier ssl-context batch-size]
  IImmutableReadable
  (-read [this] (->ImmutableHttpReader this))
  IAcquirableFactory
  (-acquire! [this]
    (let [batch-size (provide-http-batch-size batch-size)
          uri (dunaj.uri/uri uri)
          proxy (cond
                  (nil? proxy) nil
                  (class-instance? java.net.Proxy proxy) proxy
                  (class-instance? java.net.ProxySelector proxy)
                  (.get (.select ^java.net.ProxySelector proxy uri) 0)
                  :else (java.net.Proxy.
                         (key->proxy-type (:type proxy))
                         (socket-address (:address proxy)
                                         (:port proxy))))
          scheme (.getScheme uri)
          _ (when-not (or (empty? scheme)
                          (= "http" scheme) (= "https" scheme))
              (throw (illegal-argument "Invalid scheme")))
          secure? (cond (= "http" scheme) false
                        (= "https" scheme) true
                        :else secure?)
          uri (dunaj.uri/uri
               (apply ->str (if secure? "https" "http")
                      (if (empty? scheme) "://" ":")
                      (.getRawSchemeSpecificPart uri)
                      (when-not (empty? (.getRawFragment uri))
                        ["#" (.getRawFragment uri)])))
          config (assoc this :uri uri :secure? secure? :proxy proxy)
          c ^java.net.URLConnection
          (if proxy
            (.openConnection (.toURL uri) proxy)
            (.openConnection (.toURL uri)))]
      (when allow-ui? (.setAllowUserInteraction c allow-ui?))
      (when timeout (.setConnectTimeout c (milliseconds timeout)))
      (when use-caches? (.setUseCaches c use-caches?))
      (when follow? (.setInstanceFollowRedirects
                     ^java.net.HttpURLConnection c follow?))
      (when-not (empty? request-properties)
        (reduce (fn [_ [k vs]]
                  (doseq [v (provide-sequential vs)]
                    (.addRequestProperty
                     c
                     (if (named? k) (name k) (->str k))
                     (if (named? v) (name v) (->str v)))))
                request-properties))
      (when request-method
        (.setRequestMethod
         ^java.net.HttpURLConnection c
         (.toUpperCase ^java.lang.String (name request-method))))
      (when chunked-streaming
        (.setChunkedStreamingMode
         ^java.net.HttpURLConnection c
         (if (integer? chunked-streaming) chunked-streaming 0)))
      (when hostname-verifier
        (.setHostnameVerifier
         ^javax.net.ssl.HttpsURLConnection c hostname-verifier))
      (when ssl-context
        (.setSSLSocketFactory
         ^javax.net.ssl.HttpsURLConnection c
         (.getSocketFactory ^javax.net.ssl.SSLContext ssl-context)))
      (->HttpResource
       c batch-size config (current-thread) nil true false nil))))


;;;; Public API

(def http-factory :- HttpResourceFactory
  "Http(s) resource factory.
  Current options are:

  * `:uri` - resource uri
  * `:proxy` - `nil`, host proxy instance or map with `:type`,
    `:address` and `:port`, where `:type` is one of `:direct`,
    `:http` or `:socks`.
  * `:secure?` - `nil` or `true` if https is requested
  * `:allow-ui?` - `nil` or boolean. Can request input from user?
  * `:timeout` - `nil` or integer or `IDuration`. Connection timeout.
  * `:use-caches?` - `nil` or boolean. Use resource cache?
  * `:follow?` - `nil` or boolean. Follow redirects?
  * `:request-properties` - `nil` or map with request properties.
  * `:request-method` - `nil` or request method (e.g. 'GET')
  * `:chunked-streaming` - `nil` or boolean. Host specific
  * `:hostname-verifier` - `nil` or host specific hostname verifier
  * `:ssl-context` - `nil` or host specific SSL context
  * `:batch-size` - batch size, low level"
  {:added v1
   :see '[http https]}
  (->HttpResourceFactory
   nil nil false nil nil nil nil nil nil nil nil nil nil))

(defn http :- (I IImmutableReadable IAcquirableFactory)
  "Returns HTTP resource factory with given `_uri_` and `_opts_` set.
  Note that URI scheme has precedence over any other protocol
  settings set in the factory."
  {:added v1
   :see '[https http-factory]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge http-factory (assoc opts :uri uri)))

(defn https :- (I IImmutableReadable IAcquirableFactory)
  "Returns HTTPS resource factory with given `_uri_` and `_opts_` set.
  Note that URI scheme has precedence over any other protocol
  related settings set in the factory."
  {:added v1
   :see '[http http-factory]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge http-factory (assoc opts :uri uri :secure? true)))

(register-factory! "http" http-factory)
(register-factory! "https" http-factory)
