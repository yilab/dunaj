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

(ns dunaj.resource.secure
  "A resource for communication using TLS protocol.

  WARNING: Needs review from someone who knows TLS protocol and
  SSLEngine better than me."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core.async]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Maybe U I KeywordMap]]
   [dunaj.boolean :refer [Boolean and or not true? false?]]
   [dunaj.host :refer [Class Batch keyword->class set! proxy]]
   [dunaj.host.int :refer [Int iint iadd i0 i1 imin i== ipos? i< i<<]]
   [dunaj.math :refer [Integer max neg? == < zero? nneg?]]
   [dunaj.compare :refer [nil? = identical?]]
   [dunaj.state :refer [IOpenAware ICancellable ICloneable
                        ensure-open io! open? realized?]]
   [dunaj.flow :refer
    [let loop recur if do cond when-not when condp when-let]]
   [dunaj.feature :refer [IConfig config]]
   [dunaj.poly :refer [reify defrecord deftype defprotocol]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous seq postponed
     second nth reduced? rest empty? unsafe-advance! unsafe-postponed
     item-type reduce contains? assoc conj postponed?]]
   [dunaj.function :refer [Function fn defn identity]]
   [dunaj.coll.helper :refer [reduce-with-batched*]]
   [dunaj.buffer :refer [dropping-buffer]]
   [dunaj.concurrent.thread :refer
    [Thread IThreadLocal IPassableThreadLocal current-thread
     ensure-thread-local]]
   [dunaj.concurrent.port :refer [chan <!! >!!]]
   [dunaj.concurrent :refer [IFuture ITaskExecutor submit locking]]
   [dunaj.string :refer [String string? ->str]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.uri :refer [Uri uri? uri]]
   [dunaj.state.var :refer [def declare]]
   [dunaj.coll.util :refer [every? merge batched reduce-batched]]
   [dunaj.host.array :refer [array aget]]
   [dunaj.host.batch :refer [provide-batch-size item-types-match?]]
   [dunaj.error :refer
    [IFailAware IFailable IException throw illegal-argument
     illegal-state fragile io fail! try catch unsupported-operation]]
   [dunaj.resource :refer
    [IReleasable IReadable IAcquirableFactory IWritable
     acquire! -write! -read! with-scope]]
   [dunaj.resource.helper :refer [defreleasable register-factory!]]
   [dunaj.resource.tcp :refer [tcp finish-connect!]]
   [dunaj.resource.selector :refer
    [ISelectable selector register! select-now select deregister!
     -register! -deregister!]]))


;;;; Implementation details

(def ^:private batch-overhead :- Int
  "Default overhead size for TLS batches."
  (iint 100))

(defn ^:private round-size :- Int
  "Returns buffer size rounded to nearest power of 2."
  [size :- Int]
  (loop [i 8192] (if (i< i size) (recur (i<< i (i1))) i)))

(defn ^:private app-batch-size :- Int
  "Returns minimum size for app batch processed with SSL `eng`ine."
  [eng :- javax.net.ssl.SSLEngine]
  (.getApplicationBufferSize (.getSession eng)))

(defn ^:private net-batch-size :- Int
  "Returns minimum size for net batch processed with SSL `eng`ine."
  [eng :- javax.net.ssl.SSLEngine]
  (.getPacketBufferSize (.getSession eng)))

(defmacro trace
  [& xs]
  nil)

;; uncomment for verbose debugging info
#_(defmacro trace
  [& xs]
  `(clojure.core/println ~@xs))

(defn ^:private provide-byte-batch :- (Batch java.lang.Byte)
  "Returns byte batch with given `size`, which may be a direct batch,
  based on value of `direct?`. Returns `batch` if it is not nil and
  if it's capacity is at least `size`, otherwise copies contents
  from `batch`, if any, to the returned one."
  ([size :- Int, direct? :- Boolean]
     (provide-byte-batch nil size direct?))
  ([batch :- (Maybe (Batch java.lang.Byte)),
    size :- Int, direct? :- Boolean]
     (let [size (round-size (iadd size batch-overhead))]
       (if (or (nil? batch) (< (.capacity batch) size))
         (let [b (if direct?
                   (java.nio.ByteBuffer/allocateDirect size)
                   (java.nio.ByteBuffer/allocate size))]
           (trace "creating byte batch" size
                  "from" (when batch (.capacity batch)))
           (if (nil? batch)
             b
             (.put b ^java.nio.ByteBuffer (.flip batch))))
         batch))))

(defprotocol ^:private ITls
  (-tls-process! :- nil [this])
  (-reduce-batched* :- Any [this size-hint :- (Maybe Integer),
                            reducef :- AnyFn, init :- Any])
  (-set-net-recv! :- nil [this val :- Any])
  (-set-app-recv! :- nil [this val :- Any])
  (-set-other-app-recv! :- nil [this val :- Any]))

(deftype SecureReader
  [resource :- ITls]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match? requested-type (item-type this))
      (throw (illegal-argument "item types do not match")))
    (-reduce-batched* resource size-hint reducef init)))

(defn ^:private execute-delegated-tasks :- nil
  "Executes all delegated tasks from ssl `eng`ine in a current thread,
  returning nil."
  [eng :- javax.net.ssl.SSLEngine]
  (loop [] (when-let [t (.getDelegatedTask eng)] (.run t) (recur))))

(def ^:private nh
  javax.net.ssl.SSLEngineResult$HandshakeStatus/NOT_HANDSHAKING)

(defreleasable ^:private SecureResource
  "A TLS resource type."
  [transport :- (I IOpenAware IReadable IWritable),
   eng :- javax.net.ssl.SSLEngine,
   config :- {}, ^:volatile-mutable error :- (Maybe IException)
   ^:unsynchronized-mutable net-send :- ;; kept in write mode
   (Maybe (Batch java.lang.Byte)),
   ^:unsynchronized-mutable net-recv :- ;; kept in write mode
   (Maybe (Batch java.lang.Byte)),
   ^:unsynchronized-mutable app-send :- ;; kept in write mode
   (Maybe (Batch java.lang.Byte)),
   ^:unsynchronized-mutable app-recv :- ;; kept in write mode
   (Maybe (Batch java.lang.Byte)),
   ^:volatile-mutable thread :- (Maybe Thread),
   ^:unsynchronized-mutable other-app-recv
   :- (Maybe (Batch java.lang.Byte)),
   ^:unsynchronized-mutable other-net-send
   :- (Maybe (Batch java.lang.Byte)), direct-buffers? :- Boolean,
   ^:volatile-mutable opened? :- Boolean,
   ^:unsynchronized-mutable input :- Any,
   ^:unsynchronized-mutable output :- Any,
   ^:unsynchronized-mutable pending :- (Maybe IFuture),
   ^:unsynchronized-mutable retry? :- Boolean,
   executor :- (Maybe ITaskExecutor), executor-fn :- (Maybe Function)
   non-blocking? :- Boolean]
  IConfig
  (-config [this] config)
  IOpenAware
  (-open? [this] (and opened? (nil? error) (open? transport)))
  IFailAware
  (-error [this] error)
  IFailable
  (-fail! [this ex] (when (nil? error) (set! error ex)) nil)
  IReleasable
  (-release! [this]
    (set! opened? false)
    (locking this
      (when (nil? error)
        ;; unblock buffer overflows
        (.clear ^java.nio.Buffer (.-app-recv this))
        (with-scope
          (let [sel (acquire! (selector))]
            (register! sel transport :write)
            (loop []
              (-tls-process! this)
              (when (and (open? transport) (nil? (.-error this))
                         (ipos? (.position ^java.nio.Buffer
                                           (.-app-send this))))
                (select sel 1000)
                ;; there may be wait for incoming handshake,
                ;; thus explicit sleep
                (java.lang.Thread/sleep 50)
                (trace "retrying app-send")
                (recur)))
            (trace "closing outbound")
            (fragile this (.closeOutbound eng))
            (when (open? transport)
              (deregister! sel transport)
              (select-now sel) ;; to propagate deregister
              (register! sel transport :read))
            (loop []
              (-tls-process! this)
              (when (and (open? transport) (nil? (.-error this))
                         (or (postponed? (.-input this))
                             (ipos? (.position net-recv))
                             (ipos? (.position net-send))))
                (.clear ^java.nio.Buffer (.-app-recv this))
                (select sel 1000)
                ;; following sleep is probably unnecessary
                (java.lang.Thread/sleep 50)
                (trace "retrying net-recv"
                       (postponed? input)
                       (ipos? (.position ^java.nio.Buffer
                                         (.-net-recv this)))
                       (ipos? (.position ^java.nio.Buffer
                                         (.-net-send this))))
                (recur))))))))
  ICancellable
  (-cancel! [this]
    (fail! this (java.util.concurrent.CancellationException.)))
  ;; TODO: selector not reporting available app data
  ISelectable
  (-register! [this selector interests user-map]
    (-register! transport selector interests user-map))
  (-deregister! [this selector]
    (-deregister! transport selector))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  ITls
  ;; workarounds for closing over mutable fields
  (-set-net-recv! [this val] (set! net-recv val))
  (-set-app-recv! [this val] (set! app-recv val))
  (-set-other-app-recv! [this val] (set! other-app-recv val))
  (-tls-process! [this]
    ;; should finish remaining stuff also if resource is closed
    (when error (throw error))
    (set! retry? true)
    (when (and pending (realized? pending))
      @pending ;; throw any pending exceptions
      (set! pending nil))
    (when-not pending
      (when (nil? app-recv)
        (set! app-recv
              (provide-byte-batch (app-batch-size eng) false)))
      (when (nil? app-send)
        (set! app-send
              (provide-byte-batch (app-batch-size eng) false)))
      (when (nil? net-recv)
        (set! net-recv
              (provide-byte-batch
               (net-batch-size eng) direct-buffers?)))
      (when (nil? net-send)
        (set! net-send
              (provide-byte-batch
               (net-batch-size eng) direct-buffers?)))
      (when (nil? other-net-send)
        (set! other-net-send
              (provide-byte-batch
               (net-batch-size eng) direct-buffers?)))
      (trace "T ->" (.position net-recv)
             "-" (.position app-recv)
             "-x-" (.position app-send)
             "-" (.position net-send) "-> T "
             (open? this) (open? transport))
      ;; net-send -> transport
      (if (nil? output)
        (when (ipos? (.position net-send))
          (let [batch (.flip net-send)
                res (fragile this (-write! transport batch))]
            (when (postponed? res)
              (set! net-send other-net-send)
              (set! other-net-send batch)
              (set! output res))
            (.clear net-send)))
        (let [res (unsafe-advance! output)]
          (set! output (if (postponed? res) res nil))))
      ;; transport -> net-recv
      (when (nil? input)
        (let [rf (fn rf [ret batch :- (Batch java.lang.Byte)]
                   (if (.hasRemaining
                        ^java.nio.Buffer (.-net-recv this))
                     (let [oldlim (.limit batch)
                           minrem (imin
                                   (.remaining batch)
                                   (.remaining ^java.nio.Buffer
                                               (.-net-recv this)))]
                       (.limit batch (iadd (.position batch) minrem))
                       (.put ^java.nio.ByteBuffer (.-net-recv this)
                             batch)
                       (.limit batch oldlim)
                       (let [nret (iadd (iint ret) minrem)]
                         (if (.hasRemaining batch)
                           (recur nret batch)
                           nret)))
                     (unsafe-postponed ret #(rf ret batch))))
              result (io! (reduce-batched
                           (keyword->class :byte) (net-batch-size eng)
                           rf (i0) (-read! transport)))]
          (set! input result)))
      (when (postponed? input)
        (let [ni (unsafe-advance! input)]
          (when-not (postponed? ni)
            (trace "EOF from transport"))
          (set! input ni)))
      (when (and (not (postponed? input))
                 (zero? (.position net-recv))
                 (not (.isInboundDone eng))
                 (not (:ignore-missing-close? config)))
        ;; 'not postponed?' is more correct than 'not open? transport'
        (trace "inbound closed!" input)
        (fragile this (.closeInbound eng)))
      ;; net-recv -> app-recv
      (when (ipos? (.position net-recv))
        (.flip net-recv)
        (let [result :- javax.net.ssl.SSLEngineResult
              (fragile this (.unwrap eng net-recv app-recv))]
          (.compact net-recv)
          (condp = (.getStatus result)
            javax.net.ssl.SSLEngineResult$Status/BUFFER_OVERFLOW
            (do (trace "net-recv buff overflow")
                (let [ar (provide-byte-batch
                          app-recv (app-batch-size eng) false)]
                  (set! app-recv ar))
                (set! retry? false))
            javax.net.ssl.SSLEngineResult$Status/BUFFER_UNDERFLOW
            (do (trace "net-recv buff underflow")
                (let [nr (provide-byte-batch
                          net-recv (net-batch-size eng)
                          direct-buffers?)]
                  (set! net-recv nr))
                (set! retry? false))
            nil)))
      ;; app-send -> net-send
      (.flip app-send)
      (let [result :- javax.net.ssl.SSLEngineResult
            (fragile this (.wrap eng app-send net-send))]
        (.compact app-send)
        (condp = (.getStatus result)
          javax.net.ssl.SSLEngineResult$Status/BUFFER_OVERFLOW
          (do (trace "app-send buff overflow")
              (let [ns (provide-byte-batch
                        net-send (net-batch-size eng)
                        direct-buffers?)]
                (set! net-send ns))
              (set! retry? false))
          javax.net.ssl.SSLEngineResult$Status/BUFFER_UNDERFLOW
          (do (trace "app-send buff underflow")
              (let [as (provide-byte-batch
                        app-send (app-batch-size eng) false)]
                (set! app-send as))
              (set! retry? false))
          nil))
      ;; run tasks and retry on OKed NEED_(UN)WRAP
      (condp = (.getHandshakeStatus eng)
        javax.net.ssl.SSLEngineResult$HandshakeStatus/NOT_HANDSHAKING
        nil
        javax.net.ssl.SSLEngineResult$HandshakeStatus/NEED_TASK
        (let [ef (or executor-fn execute-delegated-tasks)]
          (if executor
            (set! pending (submit executor #(ef eng)))
            (ef eng))
          (trace "retrying pending tasks")
            (recur))
        javax.net.ssl.SSLEngineResult$HandshakeStatus/NEED_UNWRAP
        (when (and retry? (ipos? (.position net-recv)))
          (trace "retrying" (.getHandshakeStatus eng))
          (recur))
        javax.net.ssl.SSLEngineResult$HandshakeStatus/NEED_WRAP
        (when retry?
          (trace "retrying" (.getHandshakeStatus eng))
          (recur)))))
  (-reduce-batched* [this size-hint reducef init]
    ;; app-recv -> app
    ;; does work with closed transport because of net-recv cache
    (when non-blocking? (ensure-thread-local thread))
    (when (nil? other-app-recv)
      (set! other-app-recv
            (provide-byte-batch (app-batch-size eng) false)))
    (let [af (fn af [ret]
               (-tls-process! this)
               (cond
                 (reduced? ret) ret
                 (postponed? ret)
                 (unsafe-postponed @ret #(af (unsafe-advance! ret)))
                 (or (not opened?) error) ret
                 (ipos? (.position
                         ^java.nio.Buffer (.-app-recv this)))
                 (let [batch ^java.nio.Buffer (.-app-recv this)]
                   (-set-app-recv! this (.-other-app-recv this))
                   (-set-other-app-recv! this batch)
                   (.clear ^java.nio.Buffer (.-app-recv this))
                   (recur (reducef ret (.flip batch))))
                 (or (postponed? (.-input this))
                     (ipos? (.position
                             ^java.nio.Buffer (.-net-recv this))))
                 (unsafe-postponed ret #(af ret))
                 :else (do (-tls-process! this) ret)))]
      (if non-blocking?
        (af init)
        (with-scope
          (let [sel (acquire! (selector))]
            (register! sel this :read)
            (loop [ret (locking this (af init))]
              (trace "read loop")
              (if (postponed? ret)
                (do (select sel 1000)
                    (recur (locking this (unsafe-advance! ret))))
                ret)))))))
  IReadable
  (-read! [this] (->SecureReader this))
  IWritable
  (-write! [this coll]
    ;; app -> app-send
    (when non-blocking? (ensure-thread-local thread))
    (ensure-open this)
    (-tls-process! this) ;; initialize batches
    (when (and non-blocking? (ipos? (.position app-send)))
      (throw (illegal-state "Write operation pending.")))
    (let [rf (fn rf [ret :- Any, batch :- (Batch java.lang.Byte)]
               (cond
                (not (open? this))
                (throw (illegal-state "resource is closed"))
                (.hasRemaining ^java.nio.Buffer (.-app-send this))
                (let [oldlim (.limit batch)
                      minrem (imin (.remaining batch)
                                   (.remaining ^java.nio.Buffer
                                               (.-app-send this)))]
                  (.limit batch (iadd (.position batch) minrem))
                  (.put ^java.nio.ByteBuffer (.-app-send this) batch)
                  (.limit batch oldlim)
                  (let [nret (iadd (iint ret) minrem)]
                    (if (.hasRemaining batch)
                      (recur nret batch)
                      nret)))
                :else
                (do (-tls-process! this)
                    (if (.hasRemaining
                         ^java.nio.Buffer (.-app-send this))
                      (recur ret batch)
                      (unsafe-postponed ret #(rf ret batch))))))
          batch-size (provide-batch-size (app-batch-size eng))
          ret (reduce-batched
               (keyword->class :byte) batch-size rf (i0) coll)
          af (fn af [ret]
               (cond
                 (postponed? ret)
                 (unsafe-postponed @ret #(af (unsafe-advance! ret)))
                 (not (open? this)) ret
                 ;; postpone until app-send is empty
                 (ipos? (.position
                         ^java.nio.Buffer (.-app-send this)))
                 (let [opos (.position
                             ^java.nio.Buffer (.-app-send this))]
                   (-tls-process! this)
                   (if (i== opos (.position ^java.nio.Buffer
                                            (.-app-send this)))
                     (unsafe-postponed ret #(af ret))
                     (recur ret)))
                 ;; postpone until net-send is empty
                 (or (ipos? (.position
                             ^java.nio.Buffer (.-net-send this)))
                     output)
                 (let [opos (.position
                             ^java.nio.Buffer (.-net-send this))]
                   (-tls-process! this)
                   (if (i== opos (.position ^java.nio.Buffer
                                            (.-net-send this)))
                     (unsafe-postponed ret #(af ret))
                     (recur ret)))
                 :else ret))]
      (if non-blocking?
        (af ret)
        (with-scope
          (let [sel (acquire! (selector))]
            (register! sel this :write)
            (loop [ret (locking this (af ret))]
              (trace "write loop")
              (if (postponed? ret)
                (do (select sel 1000)
                    ;; wait if handshaking
                    (when-not (= nh (.getHandshakeStatus eng))
                      (trace "slowing down...")
                      (java.lang.Thread/sleep 100))
                    (recur (locking this (unsafe-advance! ret))))
                ret))))))))

(defrecord SecureResourceFactory
  "Factory type for TLS resources."
  [transport ssl-context remote-address remote-port client?
   session-creation? client-auth cipher-suites protocols
   session-cache-size session-timeout ignore-missing-close?
   executor executor-fn direct-buffers? non-blocking?]
  IAcquirableFactory
  (-acquire! [this]
    (let [u (when (:uri this) (uri (:uri this)))
          _ (when u
              (when-let [s (.getScheme u)]
                (when-not (empty? s)
                  (when-not (= "tcps" s)
                    (throw (illegal-argument "illegal scheme"))))))
          u (when u (uri (.getRawSchemeSpecificPart u)))
          transport (if u
                      (let [sel (acquire! (selector))
                            t (acquire! (tcp u :non-blocking? true))]
                        (register! sel t [:connect])
                        (select sel 1000)
                        (deregister! sel t)
                        (finish-connect! t)
                        t)
                      transport)]
      (when-not (:non-blocking? (config transport))
        (throw (illegal-state
                (->str "Transport resource must be open "
                       "and in a non-blocking mode"))))
      (let [sc :- javax.net.ssl.SSLContext
            (or ssl-context (javax.net.ssl.SSLContext/getDefault))
            ssc (if client?
                  (.getClientSessionContext sc)
                  (.getServerSessionContext sc))
            config (assoc this
                     :ssl-context sc
                     :transport transport)
            config (if u (assoc config :uri u) config)
            eng (if (or (empty? remote-address)
                        (nil? remote-port) (neg? remote-port))
                  (.createSSLEngine sc)
                  (.createSSLEngine
                   sc remote-address (iint remote-port)))]
        (.setUseClientMode eng client?)
        (when-not (nil? ssc)
          (when session-cache-size
            (.setSessionCacheSize ssc (iint session-cache-size)))
          (when session-timeout
            (.setSessionTimeout ssc (iint session-timeout))))
        (when session-creation?
          (.setEnableSessionCreation eng session-creation?))
        (when (identical? :want client-auth)
          (.setWantClientAuth eng true))
        (when (identical? :need client-auth)
          (.setNeedClientAuth eng true))
        (when protocols
          (.setEnabledProtocols
           eng (array java.lang.String protocols)))
        (when cipher-suites
          (.setEnabledCipherSuites
           eng (array java.lang.String cipher-suites)))
        (->SecureResource
         transport eng config nil nil nil nil nil (current-thread) nil
         nil direct-buffers? true nil nil nil false executor
         executor-fn non-blocking?)))))

(def trust-all :- javax.net.ssl.X509TrustManager
  "A trust manager that accepts any certificate."
  (proxy [javax.net.ssl.X509TrustManager] []
    (getAcceptedIssuers [this]
      (array java.security.cert.X509Certificate nil))
    (checkClientTrusted [this certs auth-type])
    (checkServerTrusted [this certs auth-type])))


;;;; Public API

(def secure-factory :- IAcquirableFactory
  "TLS resource factory. Passable thread local in non-blocking mode,
  otherwise thread safe except for multiple concurrent reads (or
  writes). Current options are:

  * `:transport` - transport resource, must be set with open
    non-blocking selectable resource.
  * `:ssl-context` - `nil` or host specific SSL context
  * `:remote-address` - string, remote address
  * `:remote-port` - integer, remote port
  * `:client?` - boolean, default true, client mode?
  * `:session-creation?` - boolean, default true,
     enable new session creation?
  * `:client-auth` - `nil`, `:want`, `:need` - client auth
  * `:cipher-suites` - collection of strings - allowed cipher suites
  * `:protocols` - collection of strings - allowed protocols
  * `:session-cache-size` - `nil` or integer, session cache size
  * `:session-timeout` - `nil` or integer, session timeout in seconds
  * `:ignore-missing-close?` - boolean, default false - ignore
    missing close handshake?
  * `:executor` - executor for running delegated tasks
  * `:executor-fn` - onbe arg fn that takes SSLEngine and runs
    all delegated tasks
  * `:direct-buffers?` - boolean, default true,
    use direct internal buffers?
  * `:non-blocking?` - boolean, default true,
    open in non-blocking mode?"
  {:added v1
   :see '[secure]}
  (->SecureResourceFactory nil nil nil nil true true nil nil nil nil
                           nil false nil nil true true))

(defn secure :- IAcquirableFactory
  "Returns TLS resource factory with given open non-blocking
  selectable `transport` resource `_x_`, or `uri` of one,
  and `_opts_` set.

  Passable thread local in non-blocking mode, otherwise thread safe
  except for multiple concurrent reads (or writes)."
  {:added v1
   :see '[secure-factory]}
  [x :- (U String Uri (I IOpenAware IReadable IWritable))
   & {:as opts}]
  (let [k (if (or (string? x) (uri? x)) :uri :transport)]
    (merge secure-factory (assoc opts k x))))

(register-factory! "tcps" secure-factory)
