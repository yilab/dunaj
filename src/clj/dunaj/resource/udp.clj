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

(ns dunaj.resource.udp
  "UDP sockets."
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
   [dunaj.math :refer [Integer max neg? == < zero? nneg?]]
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
    [IRed ICounted IBatchedRed IHomogeneous IUnpackedRed seq
     -reduce-unpacked second nth reduced? -reduce-batched rest empty?
     item-type reduce contains? assoc conj postponed? postponed
     unsafe-advance! unsafe-postponed]]
   [dunaj.function :refer [fn defn identity]]
   [dunaj.coll.helper :refer []]
   [dunaj.concurrent.thread :refer
    [Thread IThreadLocal IPassableThreadLocal current-thread
     ensure-thread-local]]
   [dunaj.concurrent.port :refer
    [IMult -tap! -untap! -untap-all! chan put! <!! close!]]
   [dunaj.time :refer [milliseconds]]
   [dunaj.uri :refer [Uri uri? uri]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword keyword name symbol]]
   [dunaj.state.basic :refer [atom]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.default :refer [vec]]
   [dunaj.coll.recipe :refer
    [keep map take-nth partition-by interpose concat]]
   [dunaj.coll.util :refer [every? merge merge-with unpacked batched]]
   [dunaj.host.array :refer
    [array array-manager aget byte-array adapt]]
   [dunaj.host.batch :refer [provide-batch-size select-item-type
                             batch-manager item-types-match?]]
   [dunaj.string :refer [String string? ->str str split]]
   [dunaj.error :refer [IFailAware IFailable IException io catch try
                        throw illegal-argument illegal-state fragile
                        opened-fragile fail! unsupported-operation]]
   [dunaj.buffer :refer [dropping-buffer]]
   [dunaj.format :refer [parse]]
   [dunaj.regex]
   [dunaj.resource :refer [IImmutableReadable IControllable IFlushable
                           IReleasable IStatusable IReadable ISeekable
                           IAcquirableFactory IWritable acquire!]]
   [dunaj.resource.helper :refer [register-factory! defreleasable]]
   [dunaj.resource.selector :refer
    [ISelectable register* deregister*]]))


;;;; Implementation details

(def ^:private default-datagram-batch-size :- Integer
  "Default size for datagram batch."
  2048)

(defn ^:private provide-datagram-batch-size :- Integer
  "Returns datagram batch size taking into account given
  batch size hint."
  [size-hint :- (Maybe Integer)]
  (provide-batch-size
   (max (or size-hint 0) default-datagram-batch-size)))

(defrecord UdpDatagram [address port payload])

(defn datagram :- UdpDatagram
  "Returns a new datagram containing given `_address_`, `_port_` and
  `_payload_`."
  {:added v1
   :see '[bare-udp]}
  [address :- (Maybe String), port :- (Maybe Integer),
   payload :- (U AnyBatch IRed)]
  (->UdpDatagram address port payload))

(deftype BareUdpResourceReader
  "Reads from an opened connectionless UDP socket.
  Passable thread local. Supports non blocking mode."
  [ch :- java.nio.channels.DatagramChannel,
   resource :- (U IFailable IOpenAware), batch-size :- Integer,
   ^:volatile-mutable thread :- (Maybe Thread), payload-fn :- AnyFn]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked
     this #(reducef % (->UdpDatagram %2 %3 %4)) init))
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (ensure-io)
    (ensure-thread-local thread)
    (ensure-open resource)
    (let [batch (java.nio.ByteBuffer/allocateDirect batch-size)
          af (fn af [ret wait?]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed
                 @ret #(af (unsafe-advance! ret) false))
                wait? (unsafe-postponed ret #(af ret false))
                (.isOpen ch)
                (let [address
                      (fragile resource (.receive ch (.clear batch)))]
                  (.flip batch)
                  (if (nil? address)
                    (recur ret true)
                    (recur (reducef
                            ret
                            (.getHostAddress
                             (.getAddress
                              ^java.net.InetSocketAddress address))
                            (.getPort
                             ^java.net.InetSocketAddress address)
                            (payload-fn batch))
                           false)))
                :else ret))]
      (af init false))))

(deftype ^:private UdpResourceReader
  "Reads from an opened connected UDP socket.
  Passable thread local. Supports non blocking mode."
  [ch :- java.nio.channels.DatagramChannel,
   resource :- (U IOpenAware IFailable), batch-size :- Integer,
   ^:volatile-mutable thread :- (Maybe Thread), payload-fn :- AnyFn]
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  IRed
  (-reduce [this reducef init]
    (ensure-io)
    (ensure-thread-local thread)
    (ensure-open resource)
    (let [batch (java.nio.ByteBuffer/allocateDirect batch-size)
          non-blocking?
          (and
           (class-instance? java.nio.channels.SelectableChannel ch)
           (not
            (.isBlocking ^java.nio.channels.SelectableChannel ch)))
          af (fn af [ret wait?]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed
                 @ret #(af (unsafe-advance! ret) false))
                wait? (unsafe-postponed ret #(af ret false))
                (.isOpen ch)
                (let [x (fragile resource
                                 (.read ch ^java.nio.ByteBuffer
                                        (.clear batch)))]
                  (cond
                   (neg? x) ret
                   (and non-blocking? (zero? x)) (recur ret true)
                   (zero? x) (recur ret false)
                   :else
                   (recur
                    (reducef ret (payload-fn (.flip batch))) false)))
                :else ret))]
      (af init false))))

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

(defn ^:private uri->map* :- KeywordMap
  "Common config uri for UDP resources."
  [qm :- {Keyword String}]
  (let [toi #(when-let [x (qm %)]
               (java.lang.Integer/valueOf ^java.lang.String x))]
    {:protocol-family (when-let [x (:pf qm)] (keyword x))
     :batch-size (toi :bs)
     :non-blocking? (boolean-map (:nb qm))
     :tos (toi :tos)
     :in-buffer-size (toi :ibs)
     :out-buffer-size (toi :obs)
     :multicast-if
     (when-let [x (:if qm)]
       (or (java.net.NetworkInterface/getByName x)
           (throw (illegal-argument "unknown interface name"))))
     :multicast-ttl (toi :ttl)
     :multicast-loop  (boolean-map (:loop qm))}))

(defn ^:private bare-uri->map :- KeywordMap
  "Returns settings map based on a given uri `x`."
  [x :- (U String Uri)]
  (let [x (uri x)
        scheme (.getScheme ^java.net.URI x)
        qm (get-query-map x)
        host (.getHost x)
        port (.getPort x)
        port (when (nneg? port) port)
        toi #(when-let [x (qm %)]
               (java.lang.Integer/valueOf ^java.lang.String x))]
    (when-not (or (empty? scheme) (= scheme "bare-udp"))
      (throw (illegal-argument "scheme")))
    (merge (uri->map* qm)
           {:local-address host
            :local-port port
            :reuse? (boolean-map (:reuse qm))
            :broadcast? (boolean-map (:broadcast qm))})))

(defn ^:private uri->map :- KeywordMap
  "Returns settings map based on a given uri `x`."
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
    (when-not (or (empty? scheme) (= scheme "udp"))
      (throw (illegal-argument "scheme")))
    (merge (uri->map* qm)
           {:remote-address host
            :remote-port port
            :local-address lh
            :local-port lp
            :reuse? (boolean-map (:reuse qm))
            :broadcast? (boolean-map (:broadcast qm))})))

(defn ^:private multicast-uri->map :- KeywordMap
  "Returns settings map based on a given multicast uri `x`."
  [x :- (U String Uri)]
  (let [x (uri x)
        scheme (.getScheme ^java.net.URI x)
        qm (get-query-map x)
        [ni lp] (vec (split #(= \: %) (or (.getUserInfo x) "")))
        lp (when lp (java.lang.Integer/valueOf ^java.lang.String lp))
        host (.getHost x)
        port (.getPort x)
        port (when (nneg? port)
               (throw (illegal-argument
                       "multicast group cannot have port")))
        toi #(when-let [x (qm %)]
               (java.lang.Integer/valueOf ^java.lang.String x))]
    (when-not (or (empty? scheme) (= scheme "multicast"))
      (throw (illegal-argument "scheme")))
    (merge (uri->map* qm)
           {:multicast-group host
            :network-interface
            (when-let [x ni]
              (or (java.net.NetworkInterface/getByName x)
                  (throw (illegal-argument
                          "unknown interface name"))))
            :sources (vec (split #(= \, %) (or (qm :sources) "")))
            :local-port lp})))

(declare bare-udp-factory)
(declare multicast-udp-factory)
(declare udp-factory)

(defn ^:private socket-address :- java.net.InetSocketAddress
  "Returns instance of socket address."
  [address :- (Maybe String), port :- (Maybe Integer)]
  (let [port (or port 0)
        address (when address
                  (java.net.InetAddress/getByName address))]
    (java.net.InetSocketAddress.
     ^java.net.InetAddress address (iint port))))

(defn ^:private map->uri* :- IRed
  [map :- KeywordMap]
  (let [{:keys [protocol-family batch-size non-blocking?
                tos in-buffer-size out-buffer-size
                multicast-if multicast-ttl multicast-loop]} map]
    [(when-not (identical?
                protocol-family
                (:protocol-family bare-udp-factory))
       (->str "pf=" (name protocol-family)))
     (when batch-size (->str "bs=" batch-size))
     (when tos (->str "tos=" tos))
     (when (true? non-blocking?) (->str "nb=true"))
     (when in-buffer-size (->str "ibs=" in-buffer-size))
     (when out-buffer-size (->str "obs=" out-buffer-size))
     (when multicast-if
       (->str "if=" (.getName ^java.net.NetworkInterface
                              multicast-if)))
     (when multicast-ttl (->str "ttl=" multicast-ttl))
     (when (false? multicast-loop) (->str "loop=false"))]))

(defn ^:private map->bare-uri :- Uri
  "Returns canonical UDP uri based on given `map`."
  [map :- KeywordMap]
  (let [{:keys [local-address local-port reuse? broadcast?]} map
        params [(when-not (nil? reuse?) (->str "reuse=" reuse?))
                (when (true? broadcast?) "broadcast=true")]
        params (concat (map->uri* map) params)
        params (str (interpose \& (keep identity params)))
        local-address (or local-address
                          (.getHostString (socket-address nil nil)))]
    (java.net.URI. "bare-udp" nil local-address (or local-port -1)
                   nil (when-not (empty? params) params) nil)))

(defn ^:private map->uri :- Uri
  "Returns canonical UDP uri based on given `map`."
  [map :- KeywordMap]
  (let [{:keys [local-address local-port remote-address remote-port
                reuse? broadcast?]} map
        params [(when-not (nil? reuse?) (->str "reuse=" reuse?))
                (when (true? broadcast?) "broadcast=true")]
        params (concat (map->uri* map) params)
        params (str (interpose \& (keep identity params)))
        local? (or local-address local-port)
        local-address (or local-address
                          (.getHostString (socket-address nil nil)))]
    (java.net.URI.
     "udp"
     (when local?
       (->str local-address
              (if (nil? local-port) "" (->str ":" local-port))))
     remote-address remote-port nil
     (when-not (empty? params) params) nil)))

(defn ^:private map->multicast-uri :- Uri
  "Returns canonical multicast UDP uri based on given `map`."
  [map :- KeywordMap]
  (let [{:keys [local-port multicast-group sources network-interface]}
        map
        params [(when-not (empty? sources)
                  (->str "sources=" (str (interpose \, sources))))]
        params (concat (map->uri* map) params)
        params (str (interpose \& (keep identity params)))]
    (java.net.URI.
     "multicast"
     (->str (.getName ^java.net.NetworkInterface network-interface)
            ":" local-port)
     multicast-group -1 nil
     (when-not (empty? params) params) nil)))

(def ^:private bbm :- BatchManager
  (batch-manager (keyword->class :byte)))

(defn ^:private provide-batch :- (Batch java.lang.Byte)
  "Provides input data in `x` as one host batch."
  [x :- (U AnyBatch IRed), bm :- BatchManager, batch :- AnyBatch]
  ;; x is batch, batchable coll or just coll
  (cond (class-instance? java.nio.ByteBuffer x)
        x
        (and (satisfies? IBatchedRed x)
             (item-types-match? (.itemType bm) (item-type x)))
        (let [rf (fn [^java.nio.Buffer to ^java.nio.Buffer from]
                   (.copy bm from to))]
          (.flip ^java.nio.Buffer
                 (-reduce-batched x (.itemType bm) (.capacity batch)
                                  rf (.clear batch))))
        :else
        (let [rf (fn cf [^java.nio.Buffer to val] (.put bm to val))]
          (.flip ^java.nio.Buffer (reduce rf (.clear batch) x)))))

(defreleasable ^:private BareUdpResource
  "Connectionless UDP resource type."
  [ch :- java.nio.channels.DatagramChannel, batch-size :- Integer,
   config :- {}, ^:volatile-mutable error :- (Maybe IException),
   payload-fn :- AnyFn]
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
   (->BareUdpResourceReader
    ch this batch-size (current-thread) payload-fn))
  IWritable
  (-write! [this coll]
    (ensure-open this)
    (let [batch (java.nio.ByteBuffer/allocateDirect batch-size)
          af (fn af [ret dgram]
               (let [dest
                     (socket-address (:address dgram) (:port dgram))
                     payload
                     (provide-batch (:payload dgram) bbm batch)]
                 (if (zero? (fragile this (.send ch payload dest)))
                   (unsafe-postponed ret #(af ret dgram))
                   (iinc ret))))]
      (reduce af (i0) coll))))

(defreleasable ^:private UdpResource
  "Connected UDP resource type."
  [ch :- java.nio.channels.DatagramChannel, batch-size :- Integer,
   config :- {}, ^:volatile-mutable error :- (Maybe IException),
   payload-fn :- AnyFn]
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
    (->UdpResourceReader
     ch this batch-size (current-thread) payload-fn))
  IWritable
  (-write! [this coll]
    (ensure-open this)
    (let [batch (java.nio.ByteBuffer/allocateDirect batch-size)
          af (fn af [ret payload]
               (let [pb (provide-batch payload bbm batch)]
                 (if (zero? (fragile this (.write ch pb)))
                   (unsafe-postponed ret #(af ret payload))
                   (iinc ret))))]
      (reduce af (i0) coll))))

(defprotocol ^:private IMulticast
  (-block! :- nil [this address :- String])
  (-unblock! :- nil [this address :- String]))

(defreleasable ^:private MulticastResource
  "Multicast UDP resource type."
  [ch :- java.nio.channels.DatagramChannel, batch-size :- Integer,
   config :- {}, ^:volatile-mutable error :- (Maybe IException),
   payload-fn :- AnyFn,
   membership-key :- java.nio.channels.MembershipKey,
   group-socket-address]
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
    (->BareUdpResourceReader
     ch this batch-size (current-thread) payload-fn))
  IWritable
  (-write! [this coll]
    (ensure-open this)
    (let [batch (java.nio.ByteBuffer/allocateDirect batch-size)
          af (fn af [ret dgram]
               (let [dg? (class-instance?
                          dunaj.resource.udp.UdpDatagram dgram)
                     dest
                     (if dg?
                       (socket-address (:address dgram) (:port dgram))
                       group-socket-address)
                     payload
                     (if dg?
                       (provide-batch (:payload dgram) bbm batch)
                       (provide-batch dgram bbm batch))]
                 (if (zero? (fragile this (.send ch payload dest)))
                   (unsafe-postponed ret #(af ret dgram))
                   (iinc ret))))]
      (reduce af (i0) coll)))
  IMulticast
  (-block! [this address]
    (when membership-key
      (opened-fragile
       this
       (.block
        membership-key (java.net.InetAddress/getByName address)))
      nil))
  (-unblock! [this address]
    (when membership-key
      (opened-fragile
       this
       (.unblock
        membership-key (java.net.InetAddress/getByName address)))
      nil)))

(defn ^:private pf->dc :- java.net.StandardProtocolFamily
  "Returns protocol family."
  [x :- Keyword]
  (cond (identical? :inet x) java.net.StandardProtocolFamily/INET
        (identical? :inet6 x) java.net.StandardProtocolFamily/INET6
        :else (throw (illegal-argument "unknown protocol family"))))

(defmacro set-when-some
  [ch opt val]
  (let [opt (symbol "java.net.StandardSocketOptions" (name opt))]
    `(when-not (nil? ~val) (.setOption ~ch ~opt ~val))))

(defmacro set-when
  [pred ch opt val]
  (let [opt (symbol "java.net.StandardSocketOptions" (name opt))]
    `(when (~pred ~val) (.setOption ~ch ~opt ~val))))

(defrecord BareUdpResourceFactory
  "Factory type for connectionless UDP sockets."
  [uri protocol-family local-address local-port batch-size
   reuse? broadcast? tos in-buffer-size out-buffer-size
   multicast-if multicast-ttl multicast-loop payload-fn
   selector-provider non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          um (bare-uri->map (or uri ""))
          config (merge-with #(if (nil? %2) % %2) this um)
          config (assoc config
                   :uri (map->bare-uri config)
                   :selector-provider sp)
          {:keys [protocol-family local-address local-port batch-size
                  reuse? broadcast? tos in-buffer-size out-buffer-size
                  multicast-if multicast-ttl multicast-loop
                  non-blocking?]} config
          ch (.openDatagramChannel
              ^java.nio.channels.spi.SelectorProvider sp
              (pf->dc protocol-family))]
      (try
        (set-when-some ch SO_REUSEADDR reuse?)
        (set-when true? ch SO_BROADCAST broadcast?)
        (set-when-some ch SO_RCVBUF in-buffer-size)
        (set-when-some ch SO_SNDBUF out-buffer-size)
        (let [batch-size (provide-datagram-batch-size batch-size)
              ch (.bind ch (socket-address local-address local-port))]
          (set-when-some ch IP_TOS tos)
          (set-when-some ch IP_MULTICAST_IF multicast-if)
          (set-when-some ch IP_MULTICAST_TTL multicast-ttl)
          (set-when false? ch IP_MULTICAST_LOOP multicast-loop)
          (when non-blocking? (.configureBlocking ch false))
          (->BareUdpResource ch batch-size config nil payload-fn))
        (catch java.lang.Exception e (.close ch) (throw e))))))

(defrecord UdpResourceFactory
  "Factory type for connected UDP sockets."
  [uri protocol-family remote-address remote-port local-address
   local-port batch-size reuse? broadcast? tos in-buffer-size
   out-buffer-size multicast-if multicast-ttl multicast-loop
   payload-fn selector-provider non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          um (uri->map (or uri ""))
          config (merge-with #(if (nil? %2) % %2) this um)
          config (assoc config
                   :uri (map->uri config)
                   :selector-provider sp)
          {:keys [protocol-family local-address local-port batch-size
                  reuse? broadcast? tos in-buffer-size out-buffer-size
                  multicast-if multicast-ttl multicast-loop
                  remote-address remote-port non-blocking?]} config
          ch (.openDatagramChannel
              ^java.nio.channels.spi.SelectorProvider sp
              (pf->dc protocol-family))]
      (try
        (set-when-some ch SO_REUSEADDR reuse?)
        (set-when true? ch SO_BROADCAST broadcast?)
        (set-when-some ch SO_RCVBUF in-buffer-size)
        (set-when-some ch SO_SNDBUF out-buffer-size)
        (let [batch-size (provide-datagram-batch-size batch-size)
              ch (.bind ch (socket-address local-address local-port))
              ch (.connect
                  ch (socket-address remote-address remote-port))]
          (set-when-some ch IP_TOS tos)
          (set-when-some ch IP_MULTICAST_IF multicast-if)
          (set-when-some ch IP_MULTICAST_TTL multicast-ttl)
          (set-when false? ch IP_MULTICAST_LOOP multicast-loop)
          (when non-blocking? (.configureBlocking ch false))
          (->UdpResource ch batch-size config nil payload-fn))
        (catch java.lang.Exception e (.close ch) (throw e))))))

(defrecord MulticastResourceFactory
  "Factory type for multicast UDP sockets."
  [uri protocol-family multicast-group sources network-interface
   local-port batch-size tos in-buffer-size
   out-buffer-size multicast-if multicast-ttl multicast-loop
   payload-fn selector-provider non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [sp (or selector-provider
                 (java.nio.channels.spi.SelectorProvider/provider))
          um (multicast-uri->map (or uri ""))
          config (merge-with #(if (nil? %2) % %2) this um)
          config (assoc config
                   :uri (map->multicast-uri config)
                   :selector-provider sp)
          {:keys [protocol-family local-port batch-size
                  tos in-buffer-size out-buffer-size non-blocking?
                  multicast-if multicast-ttl multicast-loop
                  multicast-group sources network-interface]} config
          ch (.openDatagramChannel
              ^java.nio.channels.spi.SelectorProvider sp
              (pf->dc protocol-family))
          mg (java.net.InetAddress/getByName multicast-group)]
      (try
        (set-when-some ch SO_REUSEADDR java.lang.Boolean/TRUE)
        (set-when-some ch SO_RCVBUF in-buffer-size)
        (set-when-some ch SO_SNDBUF out-buffer-size)
        (let [batch-size (provide-datagram-batch-size batch-size)
              ch (.bind ch (socket-address nil local-port))
              membership-key
              (when (empty? sources)
                (.join ^java.nio.channels.MulticastChannel ch
                       mg network-interface))]
          (when-not (empty? sources)
            (reduce
             (fn [^java.nio.channels.MulticastChannel ch source]
                (.join ch mg network-interface
                       (java.net.InetAddress/getByName source)))
             ch sources))
          (set-when-some ch IP_TOS tos)
          (set-when-some ch IP_MULTICAST_IF multicast-if)
          (set-when-some ch IP_MULTICAST_TTL multicast-ttl)
          (set-when false? ch IP_MULTICAST_LOOP multicast-loop)
          (when non-blocking? (.configureBlocking ch false))
          (->MulticastResource
           ch batch-size config nil payload-fn membership-key
           (socket-address multicast-group local-port)))
        (catch java.lang.Exception e (.close ch) (throw e))))))

(defn ^:private payload->array-coll :- IRed
  "Returns reducible coll from a given payload batch."
  [batch :- java.nio.ByteBuffer]
  (let [arr (byte-array (.remaining batch))]
    (.get batch arr)
    (adapt arr)))


;;;; Public API

(def bare-udp-factory :- IAcquirableFactory
  "A connectionless UDP resource factory.
  Current options are:

  * `:uri` - resource URI
  * `:protocol-family` - `:inet` (default) or `:inet6`
  * `:local-address` - `nil` or string
  * `:local-port` - `nil` or integer
  * `:batch-size` - batch size, low level
  * `:reuse?` - reuse address?. boolean
  * `:broadcast?` - udp broadcast?, boolean, default false
  * `:tos` - tos
  * `:in-buffer-size` - `nil` or integer
  * `:out-buffer-size` - `nil` or integer
  * `:multicast-if` - multicast interface
  * `:multicast-ttl` - multicast ttl
  * `:multicast-loop` - multicast loopback, boolean, default true
  * `:payload-fn` - takes received payload as a batch and produces
    reducible collection that has no connection to given batch.
  * `:selector-provider` - host selector provider
  * `:non-blocking?` - use non-blocking mode?, boolean"
  {:added v1
   :see '[bare-udp udp-factory multicast-factory]}
  (->BareUdpResourceFactory
   nil :inet nil nil nil nil false
   nil nil nil nil nil true payload->array-coll nil false))

(def udp-factory :- IAcquirableFactory
  "An UDP resource factory.
  Current options are:

  * `:uri` - resource URI
  * `:protocol-family` - `:inet` (default) or `:inet6`
  * `:remote-address` - `nil` or string
  * `:remote-port` - `nil` or integer
  * `:local-address` - `nil` or string
  * `:local-port` - `nil` or integer
  * `:batch-size` - batch size, low level
  * `:reuse?` - reuse address?. boolean
  * `:broadcast?` - udp broadcast?, boolean, default false
  * `:tos` - tos
  * `:in-buffer-size` - `nil` or integer
  * `:out-buffer-size` - `nil` or integer
  * `:multicast-if` - multicast interface
  * `:multicast-ttl` - multicast ttl
  * `:multicast-loop` - multicast loopback, boolean, default true
  * `:payload-fn` - takes received payload as a batch and produces
    reducible collection that has no connection to given batch.
  * `:selector-provider` - host selector provider
  * `:non-blocking?` - use non-blocking mode?, boolean"
  {:added v1
   :see '[udp bare-udp-factory multicast-factory]}
  (->UdpResourceFactory
   nil :inet nil nil nil nil nil nil false
   nil nil nil nil nil true payload->array-coll nil false))

(def multicast-factory :- IAcquirableFactory
  "A resource factory for multicast.
  Current options are:

  * `:uri` - resource URI
  * `:protocol-family` - `:inet` (default) or `:inet6`
  * `:multicast-group` - string
  * `:sources` - nil or collection of sources
  * `:network-interface` - host network interface
  * `:local-port` - `nil` or integer
  * `:batch-size` - batch size, low level
  * `:tos` - tos
  * `:in-buffer-size` - `nil` or integer
  * `:out-buffer-size` - `nil` or integer
  * `:multicast-if` - multicast interface
  * `:multicast-ttl` - multicast ttl
  * `:multicast-loop` - multicast loopback, boolean, default true
  * `:payload-fn` - takes received payload as a batch and produces
    reducible collection that has no connection to given batch.
  * `:selector-provider` - host selector provider
  * `:non-blocking?` - use non-blocking mode?, boolean"
  {:added v1
   :see '[multicast bare-udp-factory udp-factory]}
  (->MulticastResourceFactory
   nil :inet nil nil nil nil nil
   nil nil nil nil nil true payload->array-coll nil false))

(defn bare-udp :- IAcquirableFactory
  "Returns connectionless UDP resource factory with
  given `_uri_` and `_opts_` set."
  {:added v1
   :see '[bare-udp-factory udp multicast]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge bare-udp-factory (assoc opts :uri uri)))

(defn udp :- IAcquirableFactory
  "Returns UDP resource factory with given `_uri_` and `_opts_` set."
  {:added v1
   :see '[udp-factory bare-udp multicast]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge udp-factory (assoc opts :uri uri)))

(defn multicast :- IAcquirableFactory
  "Returns multicast UDP resource factory
  with given `_uri_` and `_opts_` set."
  {:added v1
   :see '[multicast-factory bare-udp udp block!]}
  [uri :- (U nil String Uri) & {:as opts}]
  (merge multicast-factory (assoc opts :uri uri)))

(defn block! :- nil
  "Blocks given `_address_` for `_multicast_` resource. Resource must
  not be source specific."
  {:added v1
   :see '[unblock! multicast]}
  [multicast :- IMulticast, address :- String]
  (-block! multicast address))

(defn unblock! :- nil
  "Unblocks given `_address_` from `_multicast_` resource.
  Throws if `_address_` is not blocked."
  {:added v1
   :see '[block! multicast]}
  [multicast :- IMulticast, address :- String]
  (-unblock! multicast address))

(register-factory! "bare-udp" bare-udp-factory)
(register-factory! "udp" udp-factory)
(register-factory! "multicast" multicast-factory)
