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

(ns dunaj.resource.tcp
  "TCP sockets."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [clojure.core.async]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer [Boolean and or not true? false?]]
   [dunaj.host :refer [Class BatchManager Batch AnyBatch set!]]
   [dunaj.host.int :refer [iint iloop iadd ixFF i0 iinc i1]]
   [dunaj.math :refer [Integer max neg? == < zero? nneg?]]
   [dunaj.compare :refer [nil? = identical?]]
   [dunaj.state :refer [IOpenAware IReference IMutable io!]]
   [dunaj.flow :refer [let loop recur if do when-not when when-let]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.poly :refer [defrecord deftype defprotocol satisfies?]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous IUnpackedRed seq contains?
     -reduce-unpacked second nth reduced? rest empty? unsafe-advance!
     item-type reduce assoc conj postponed? postponed]]
   [dunaj.function :refer [fn defn identity]]
   [dunaj.coll.helper :refer []]
   [dunaj.concurrent.thread :refer [current-thread]]
   [dunaj.concurrent.port :refer [chan put! <!! close!]]
   [dunaj.time :refer [milliseconds]]
   [dunaj.uri :refer [Uri uri? uri]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword keyword name symbol]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.default :refer [vec]]
   [dunaj.coll.recipe :refer [keep map interpose concat]]
   [dunaj.coll.util :refer [every? merge merge-with unpacked batched]]
   [dunaj.host.array :refer [array aget byte-array adapt]]
   [dunaj.host.batch :refer [provide-batch-size]]
   [dunaj.string :refer [String string? ->str str split]]
   [dunaj.error :refer
    [IFailAware IFailable IException
     throw illegal-argument illegal-state fragile io
     opened-fragile fail! try catch unsupported-operation]]
   [dunaj.buffer :refer [dropping-buffer]]
   [dunaj.format :refer [parse]]
   [dunaj.regex]
   [dunaj.resource :refer
    [IImmutableReadable IReleasable IFlushable IReadable ISeekable
     IAcquirableFactory IWritable IControllable IStatusable acquire!]]
   [dunaj.resource.helper :refer
    [register-factory! defreleasable readable-resource-recipe
     basic-write!]]
   [dunaj.resource.selector :refer
    [ISelectable register* deregister*]]))


;;;; Implementation details

(def ^:private default-tcp-batch-size :- Integer
  "Default size for tcp batch."
  8192)

(defn ^:private provide-tcp-batch-size :- Integer
  "Returns tcp batch size taking into account given batch size hint."
  [size-hint :- (Maybe Integer)]
  (provide-batch-size (max (or size-hint 0) default-tcp-batch-size)))

(defn ^:private get-query-map :- {Keyword String}
  "Returns map of parsed query params from a given uri `x`."
  [x :- (U Uri String)]
  (let [x (uri x)
        s (.getQuery x)
        params (when-not (empty? s) (parse #"([^=&]+)(=([^&]*))?" s))]
    (reduce #(assoc % (keyword (second %2)) (or (nth %2 3) "true"))
            {} params)))

(def ^:private boolean-map :- {String Boolean}
  {"0" false "1" true "F" false "T" true "false" false "true" true})

(defn ^:private tcp-server-uri->map :- KeywordMap
  "Returns server socket settings map based on a given uri `x`."
  [x :- (U String Uri)]
  (let [x (uri x)
        scheme (.getScheme ^java.net.URI x)
        qm (get-query-map x)
        host (.getHost x)
        port (.getPort x)
        port (when (nneg? port) port)
        toi #(when-let [x (qm %)]
               (java.lang.Integer/valueOf ^java.lang.String x))]
    (when-not (or (empty? scheme) (= scheme "tcp"))
      (throw (illegal-argument "scheme")))
    {:local-address host
     :local-port port
     :batch-size (toi :bs)
     :non-blocking? (boolean-map (:nb qm))
     :in-buffer-size (toi :ibs)
     :reuse? (boolean-map (:reuse qm))}))

(defn ^:private tcp-uri->map :- KeywordMap
  "Returns socket settings map based on a given uri `x`."
  [x :- (U String Uri)]
  (let [x (uri x)
        scheme (.getScheme ^java.net.URI x)
        qm (get-query-map x)
        [lh lp] (vec (split #(= \: %) (or (.getUserInfo x) "")))
        lp (when lp (java.lang.Integer/valueOf ^java.lang.String lp))
        host (.getHost x)
        port (.getPort x)
        port (when (nneg? port) port)
        toi #(when-let [x (qm %)]
               (java.lang.Integer/valueOf ^java.lang.String x))]
    (when-not (or (empty? scheme) (= scheme "tcp"))
      (throw (illegal-argument "scheme")))
    {:remote-address host
     :remote-port port
     :local-address lh
     :local-port lp
     :batch-size (toi :bs)
     :non-blocking? (boolean-map (:nb qm))
     :keep-alive? (boolean-map (:ka qm))
     :in-buffer-size (toi :ibs)
     :out-buffer-size (toi :obs)
     :linger (toi :linger)
     :no-delay? (boolean-map (:nd qm))
     :reuse? (boolean-map (:reuse qm))}))

(defn ^:private socket-address :- java.net.InetSocketAddress
  "Returns an instance of a socket address."
  [address :- (Maybe String), port :- (Maybe Integer)]
  (let [port (or port 0)
        address (when address
                  (java.net.InetAddress/getByName address))]
    (java.net.InetSocketAddress.
     ^java.net.InetAddress address (iint port))))

(defn ^:private map->tcp-server-uri :- Uri
  "Returns canonical TCP server uri based on given `map`."
  [map :- KeywordMap]
  (let [{:keys [local-address local-port reuse? batch-size
                in-buffer-size non-blocking?]} map
        params [(when batch-size (->str "bs=" batch-size))
                (when (true? non-blocking?) "nb=true")
                (when in-buffer-size (->str "ibs=" in-buffer-size))
                (when-not (nil? reuse?) (->str "reuse=" reuse?))]
        params (str (interpose \& (keep identity params)))
        local-address (or local-address
                          (.getHostString (socket-address nil nil)))]
    (java.net.URI. "tcp" nil local-address (or local-port -1)
                   nil (when-not (empty? params) params) nil)))

(defn ^:private map->tcp-uri :- Uri
  "Returns canonical TCP uri based on given `map`."
  [map :- KeywordMap]
  (let [{:keys [local-address local-port remote-address remote-port
                batch-size non-blocking? keep-alive? reuse?
                in-buffer-size out-buffer-size linger no-delay?]} map
        params [(when batch-size (->str "bs=" batch-size))
                (when in-buffer-size (->str "ibs=" in-buffer-size))
                (when out-buffer-size (->str "obs=" out-buffer-size))
                (when (true? non-blocking?) "nb=true")
                (when (true? keep-alive?) "ka=true")
                (when linger (->str "linger=" linger))
                (when (true? no-delay?) "nd=true")
                (when-not (nil? reuse?) (->str "reuse=" reuse?))]
        params (str (interpose \& (keep identity params)))
        local? (or local-address local-port)
        local-address (or local-address
                          (.getHostString (socket-address nil nil)))]
    (java.net.URI.
     "tcp"
     (when local?
       (->str local-address
              (if (nil? local-port) "" (->str ":" local-port))))
     remote-address remote-port nil
     (when-not (empty? params) params) nil)))

(defreleasable ^:private TcpResource
  "Connected TCP resource type."
  [ch :- java.nio.channels.SocketChannel, batch-size :- Integer,
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
    (readable-resource-recipe this ch batch-size (current-thread)))
  IWritable
  (-write! [this coll]
    (basic-write! this ch batch-size (current-thread) coll)))

(defprotocol ^:private IServer
  (-accept! :- (Maybe TcpResource) [this]))

(defreleasable ^:private TcpServerResource
  "TCP Server resource type."
  [ch :- java.nio.channels.ServerSocketChannel, batch-size :- Integer,
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
  IServer
  (-accept! [this]
    (when-let [ch (opened-fragile this (.accept ch))]
      (->TcpResource ch batch-size (assoc config :server this) nil))))

(defmacro set-when-some
  [ch opt val]
  (let [opt (symbol "java.net.StandardSocketOptions" (name opt))]
    `(when-not (nil? ~val) (.setOption ~ch ~opt ~val))))

(defmacro set-when
  [pred ch opt val]
  (let [opt (symbol "java.net.StandardSocketOptions" (name opt))]
    `(when (~pred ~val) (.setOption ~ch ~opt ~val))))

(defrecord TcpServerResourceFactory
  "Factory type for TCP server sockets."
  [uri local-address local-port reuse? in-buffer-size batch-size
   selector-provider non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          um (tcp-server-uri->map (or uri ""))
          config (merge-with #(if (nil? %2) % %2) this um)
          config (assoc config
                   :uri (map->tcp-server-uri config)
                   :selector-provider sp)
          {:keys [local-address local-port reuse? in-buffer-size
                  batch-size non-blocking?]} config
          ch (.openServerSocketChannel
              ^java.nio.channels.spi.SelectorProvider sp)]
      (try
        (set-when-some ch SO_REUSEADDR reuse?)
        (set-when-some ch SO_RCVBUF in-buffer-size)
        (let [batch-size (provide-tcp-batch-size batch-size)
              ch (.bind ch (socket-address local-address local-port))]
          (when non-blocking? (.configureBlocking ch false))
          (->TcpServerResource ch batch-size config nil))
        (catch java.lang.Exception e (.close ch) (throw e))))))

(defrecord TcpResourceFactory
  "Factory type for connected TCP sockets."
  [uri remote-address remote-port local-address local-port
   batch-size non-blocking? keep-alive? in-buffer-size
   out-buffer-size linger no-delay? reuse? selector-provider]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          um (tcp-uri->map (or uri ""))
          config (merge-with #(if (nil? %2) % %2) this um)
          config (assoc config
                   :uri (map->tcp-uri config)
                   :selector-provider sp)
          {:keys [remote-address remote-port local-address local-port
                  batch-size non-blocking? keep-alive? in-buffer-size
                  out-buffer-size linger no-delay? reuse?]} config
          ch (.openSocketChannel
              ^java.nio.channels.spi.SelectorProvider sp)]
      (try
        (set-when-some ch SO_REUSEADDR reuse?)
        (set-when-some ch SO_RCVBUF in-buffer-size)
        (set-when-some ch SO_SNDBUF out-buffer-size)
        (let [batch-size (provide-tcp-batch-size batch-size)
              ch (.bind ch (socket-address local-address local-port))
              _ (when non-blocking? (.configureBlocking ch false))
              cr (.connect
                  ch (socket-address remote-address remote-port))]
          (set-when true? ch SO_KEEPALIVE keep-alive?)
          (set-when-some ch SO_LINGER linger)
          (set-when true? ch TCP_NODELAY no-delay?)
          (->TcpResource ch batch-size config nil))
        (catch java.lang.Exception e (.close ch) (throw e))))))


;;;; Public API

(def tcp-server-factory :- IAcquirableFactory
  "TCP server resource factory.
  Current options are:

  * `:uri` - resource uri
  * `:local-address` - string, local address
  * `:local-port` - integer, local port
  * `:reuse?` - boolean, reuse port?
  * `:in-buffer-size` - integer, input buffer size
  * `:batch-size` - integer, batch size, low level
  * `:selector-provider` - `nil` (default) or host specific selector
    provider
  * `:non-blocking?` - boolean, default false - open resource
    in non-blocking mode?"
  {:added v1
   :see '[tcp-server accept! tcp-factory]}
  (->TcpServerResourceFactory nil nil nil nil nil nil nil nil))

(def tcp-factory :- IAcquirableFactory
  "TCP resource factory.
  Current options are:

  * `:uri` - resource uri
  * `:remote-address` - string, remote address
  * `:remote-port` - integer, remote port
  * `:local-address` - string, local address
  * `:local-port` - integer, local port
  * `:batch-size` - integer, batch size, low level
  * `:non-blocking?` - boolean, default false - open resource
    in non-blocking mode?
  * `:keep-alive?` - boolean, default false, TCP keepalive
  * `:in-buffer-size` - integer, input buffer size
  * `:out-buffer-size` - integer, output buffer size
  * `:linger` - integer, linger
  * `:no-delay?` - boolean, default false, TCP no delay
  * `:reuse?` - boolean, reuse port?
  * `:selector-provider` - `nil` (default) or host specific selector
    provider"
  {:added v1
   :see '[tcp tcp-server-factory]}
  (->TcpResourceFactory
   nil nil nil nil nil nil nil false nil nil nil false nil nil))

(defn tcp-server :- IAcquirableFactory
  "Returns TCP server resource factory with
  given `_uri_` and `_opts_` set."
  {:added v1
   :see '[tcp-server-factory tcp accept!]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge tcp-server-factory (assoc opts :uri uri)))

(defn tcp :- IAcquirableFactory
  "Returns TCP resource factory with given `_uri_` and `_opts_` set."
  {:added v1
   :see '[tcp-factory tcp-server]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge tcp-factory (assoc opts :uri uri)))

(defn accept! :- nil
  "Returns incoming connection as TCP resource. Blocks or returns
  `nil` if there are no pending connections.
  Supports non-blockimg mode."
  {:added v1
   :see '[tcp-server tcp-server-factory]}
  [server :- IServer]
  (-accept! server))

(defn finish-connect! :- nil
  "Returns `true` if connection has been established, otherwise
  returns `false`. Must be called on client resource in non-blocking
  mode that has signalled that it is ready to finish the connection
  process."
  {:added v1
   :see '[tcp tcp-factory]}
  [res :- TcpResource]
  (.finishConnect ^java.nio.channels.SocketChannel (.-ch res)))

(register-factory! "tcp" tcp-factory)
