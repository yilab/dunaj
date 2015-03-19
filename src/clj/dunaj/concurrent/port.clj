;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; Additional copyright for parts of documentation and/or
;; underlying implementation:
;; Copyright (C) 2008, 2015, Rich Hickey and Clojure contributors.
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

(ns dunaj.concurrent.port
  "Asynchronous programming through ports and channels.

  NOTE: Documentation needs more work."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Ports" "Operations" "Mult" "Mix" "Pub"]}
  (:api bare)
  (:require
   [clojure.core :refer
    [add-watch constantly assert atom remove-watch disj empty?]]
   [clojure.core.async.impl.dispatch]
   [clojure.core.async.impl.protocols :as cap]
   [clojure.bootstrap :refer [v1 not-implemented]]
   [dunaj.type :refer
    [Fn Any U I Va Maybe AnyFn Predicate Signature KeywordMap]]
   [dunaj.boolean :refer [Boolean and]]
   [dunaj.math :refer [Integer]]
   [dunaj.state :refer [IOpenAware -open? IReference]]
   [dunaj.flow :refer [let when if if-let]]
   [dunaj.feature :refer [assoc-meta meta]]
   [dunaj.poly :refer [extend-protocol! reify deftype defprotocol]]
   [dunaj.coll :refer
    [IMutableCollection IRed conj assoc editable? edit settle! conj!]]
   [dunaj.function :refer [apply nop fn complement defn partial]]
   [dunaj.concurrent :refer [IExecutor locking execute]]
   [dunaj.buffer :refer [promise-buffer]]
   [dunaj.string :refer [String]]
   [dunaj.time :refer [IDuration]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.error :refer [IException]]
   [dunaj.state.var :refer [Var var def]]))


;;;; Implementation details

;; TODO: should we put all default executors in Dunaj as delays
;;       in order to be more lazy with these pools?
(def ^:dynamic ^:private *default-port-executor* :- IExecutor
  @clojure.core.async.impl.dispatch/executor)


;;;; Public API

(def PortVal :- Signature
  "A type signature for values that can be processed by a port."
  {:added v1
   :see '[<!! <! >!! >! put! take!]
   :category "Ports"}
  Any) ;; TODO: make it Any but nil

;; Implementation for most vars is provided in clojure.core.async

(def default-port-executor :- Var
  "Dynamic var holding default port executor.

  NOTE: Not yet fully integrated."
  ;; TODO: do we need customization on function call level
  ;;       (e.g. as a parameter in put!, take!, <!! etc.)?
  {:added v1}
  (var *default-port-executor*))

(defn fn-handler :- Any
  "Returns a fn handler created from a given function `_f_`.
  Returned handler can be used as an arg in source/target ports'
  protocol methods.

  NOTE: Low level function."
  {:added v1}
  ([f :- AnyFn]
   (not-implemented))
  ([f :- AnyFn, blockable? :- Boolean]
   (not-implemented)))

(defprotocol ISourcePort
  "A state protocol for source ports."
  {:added v1
   :category "Ports"
   :see '[take! <!! <! source ITargetPort ICloseablePort]
   :predicate 'source-port?}
  (-take! :- (Maybe IReference)
    "Returns derefable val if taken, `nil` if take was enqueued.
    Returns derefable `nil` if closed."
    [this fn1-handler :- Any]))

(defn take! :- nil
  "Asynchronously takes a value from source `_port_`, passing it to
  `_fn1_`. Will pass `nil` if closed. If `_always-dispatch?_`
  (default `false`) is `false`, and value is immediately available,
  will call `_fn1_` on calling thread. Returns `nil`."
  {:added v1
   :see '[<!! <! ISourcePort put! close!]
   :category "Primary"}
  ([port :- ISourcePort, fn1 :- (Fn [Any (Maybe PortVal)])]
   (take! port fn1 false))
  ([port :- ISourcePort, fn1 :- (Fn [Any (Maybe PortVal)]),
    always-dispatch? :- Boolean]
   (let [ret (-take! port (fn-handler fn1))]
     (when ret
       (let [val @ret]
         (if always-dispatch?
           (execute *default-port-executor* #(fn1 val))
           (fn1 val))))
     nil)))

(defn <!! :- (Maybe PortVal)
  "Takes a value from source `_port_` and returns it.
  Will return `nil` if `_port_` is closed.
  Will block current thread if nothing is available."
  {:added v1
   :see '[take! <! ISourcePort put! close! thread]
   :category "Primary"}
  [port :- ISourcePort]
  (not-implemented))

(defn <! :- (Maybe PortVal)
  "Takes a value from source `_port_` and returns it.
  Will return `nil` if `_port_` is closed.
  Will park if nothing is available.
  Must be called inside a `(go ...)` block."
  {:added v1
   :see '[take! <!! go ISourcePort put! close!]
   :category "Primary"}
  [port :- ISourcePort]
  (assert nil "<! used not in (go ...) block"))

(defprotocol ITargetPort
  "A state protocol for target ports."
  {:added v1
   :category "Ports"
   :see '[put! >!! >! target ISourcePort ICloseablePort]
   :predicate 'target-port?}
  (-put! :- (Maybe IReference)
    "Returns derefable boolean (`false` iff already closed) if
    handled, `nil` if put was enqueued. Must throw on `nil` val."
    [this val :- PortVal, fn1-handler :- Any]))

(defn put!
  "Asynchronously puts a non-nil value into target `_port_`, calling
  `_fn1_` (if supplied) when complete, passing `false` iff port is
  already closed. If `_always-dispatch?_` (default `false`) is
  `false`, and the put is immediately accepted, will call `_fn1_` on
  calling thread. Returns `true` unless port is already closed.

  IMPORTANT: `nil` values are not allowed. Use `encode-nil` to
  pass nil values."
  {:added v1
   :see '[>!! >! ITargetPort take! close! dunaj.compare/encode-nil]
   :category "Primary"}
  ([port :- ITargetPort, val :- PortVal]
   (if-let [ret (-put! port val (fn-handler nop))] @ret true))
  ([port :- ITargetPort, val :- PortVal, fn1 :- (Fn [Any Boolean])]
   (put! port val fn1 false))
  ([port :- ITargetPort, val :- PortVal, fn1 :- (Fn [Any Boolean]),
    always-dispatch? :- Boolean]
   (if-let [ret (-put! port val (fn-handler fn1))]
     (let [retb @ret]
       (if always-dispatch?
         (execute *default-port-executor* (fn1 retb))
         (fn1 retb)))
     true)))

(defn >!! :- Boolean
  "Puts a non-nil `_val_` into target `_port_`.
  Will block if no buffer space is available.
  Returns `true` unless port is already closed.

  IMPORTANT: `nil` values are not allowed. Use `encode-nil` to
  pass nil values."
  {:added v1
   :see '[put! >! ITargetPort take! close! dunaj.compare/encode-nil]
   :category "Primary"}
  [port :- ITargetPort, val :- PortVal]
  (not-implemented))

(defn >! :- Boolean
  "Puts a non-nil `_val_` into target `_port_`.
  Will park if no buffer space is available.
  Must be called inside a `(go ...)` block.
  Returns `true` unless port is already closed.

  IMPORTANT: `nil` values are not allowed. Use `encode-nil` to
  pass nil values."
  {:added v1
   :see '[>!! put! ITargetPort take! close! dunaj.compare/encode-nil]
   :category "Primary"}
  [port :- ITargetPort, val :- PortVal]
  (assert nil ">! used not in (go ...) block"))

(defn offer!
  "Puts a `_val_` into `_port_` if it's possible to do so immediately.
  Never blocks. Returns `true` if offer succeeds.

  IMPORTANT: `nil` values are not allowed. Use `encode-nil` to
  pass nil values."
  [port :- ITargetPort, val :- PortVal]
  (let [ret (-put! port val (fn-handler nop false))]
    (when ret @ret)))

(defn poll!
  "Takes a val from `_port_` if it's possible to do so immediately.
  Never blocks. Returns value if successful, `nil` otherwise."
  [port :- ISourcePort]
  (let [ret (-take! port (fn-handler nop false))]
    (when ret @ret)))

(defprotocol ICloseablePort
  "A state protocol for ports that can be closed.

  NOTE: Resources in <<dunaj.resource.api.ad#,dunaj.resource>> use
  concept of blocking releasable instead of closeable."
  {:added v1
   :category "Ports"
   :see '[close! close-only ISourcePort ITargetPort]
   :predicate 'closeable-port?}
  (-close! :- nil
    "Closes `_this_` and returns `nil`. Must not block."
    [this]))

(defn close! :- nil
  "Closes `_port_`. The `_port_` will no longer accept any puts (they
  will be ignored). Data in the port remains available for taking,
  until exhausted, after which takes will return `nil`.
  If there are any pending takes, they will be dispatched with `nil`.
  Closing a closed port is a no-op. Returns `nil`."
  {:added v1
   :see '[closeable-port? close-only take! put!]
   :category "Primary"}
  [port :- ICloseablePort]
  (-close! port))

(defn close-only :- ICloseablePort
  "Returns a close-only port from given closeable `_port_`."
  {:added v1
   :see '[closeable-port? close!]
   :category "Ports"}
  [port :- ICloseablePort]
  (reify
    ICloseablePort
    (-close! [this] (-close! port))))

;;; Execution strategies

(defmacro go
  "Asynchronously executes the `_body_`, returning immediately to the
  calling thread. Additionally, any visible calls to `<!`,`>!` and
  `alt!`/`alts!` channel operations within the `_body_` will block
  (if necessary) by 'parking' the calling thread rather than tying
  up an OS thread,
  Upon completion of the operation, the `_body_` will be resumed.

  Returns a source port which will receive the result of the body
  when completed."
  {:added v1
   :see '[go-loop thread thread-call]
   :category "Primary"
   :highlight :flow}
  [& body]
  (not-implemented))

;; TODO: add support for iloop (also in tools.analyzer)
(defmacro go-loop
  "Like `(go (loop ...))`"
  {:added v1
   :see '[go thread thread-call]
   :category "Primary"}
  [bindings & body]
  `(go (clojure.core/loop ~bindings ~@body)))

(defn thread-call :- ISourcePort
  "Executes `_f_` in another thread, returning immediately to the
  calling thread. Returns a source port which will receive the
  result of calling `_f_` when completed."
  {:added v1
   :see '[go go-loop thread dunaj.concurrent.thread/thread-call]
   :category "Primary"}
  [f :- (Fn [Any])]
  (not-implemented))

(defmacro thread
  "Executes the `_body_` in another thread, returning immediately to
  the calling thread. Returns a source port which will receive the
  result of the `_body_` when completed."
  {:added v1
   :see '[go go-loop thread dunaj.concurrent.thread/thread]
   :category "Primary"
   :highlight :flow}
  [& body]
  `(thread-call (fn [] ~@body)))

;;; Basic ports

(defn chan :- (I ISourcePort ITargetPort ICloseablePort)
  "Creates a channel with an optional buffer, an optional transducer,
  and an optional `exception-handler`. If `buf-or-n` is a number,
  will create and use a fixed buffer of that size.

  `ex-handler` must be a function of one argument. If an exception
  occurs during transformation it will be called with the
  `IException` as an argument, and any non-nil return value will be
  placed in the channel.

  IMPORTANT: If a transducer is supplied a buffer must be specified."
  {:added v1
   :see '[timeout master source target close-only take! put!
          <!! <! >!! >! close! dunaj.buffer/buffer dunaj.state/open?
          dunaj.buffer/dropping-buffer dunaj.buffer/sliding-buffer]
   :category "Ports"}
  ([]
   (chan nil))
  ([buf-or-n :- (U Integer IMutableCollection)]
   (chan buf-or-n nil))
  ([buf-or-n :- (U Integer IMutableCollection), xform :- Any]
   (chan buf-or-n xform nil))
  ([buf-or-n :- (U Integer IMutableCollection), xform :- Any,
    exception-handler :- (Fn [Any IException])]
   (not-implemented)))

(defn timeout :- ISourcePort
  "Returns a source port that will close after `_duration_`."
  {:added v1
   :ses '[chan dunaj.state/open? take!]
   :category "Ports"}
  [duration :- (U Integer IDuration)]
  (not-implemented))

(defn promise-chan :- (I ISourcePort ITargetPort ICloseablePort)
  "Creates a promise channel with an optional transducer `_xform_`,
  and an optional `_exception-handler_`. A promise channel can take
  exactly one value that consumers will receive. Once set, puts
  complete but val is dropped (no transfer). Consumers will block
  until either a value is placed in the channel or the channel is
  closed."
  {:added v1
   :see '[chan dunaj.concurrent/promise]
   :category "Ports"}
  ([] (promise-chan nil))
  ([xform :- Any] (promise-chan xform nil))
  ([xform :- Any, exception-handler :- (Fn [Any IException])]
   (chan (promise-buffer) xform exception-handler)))

(deftype Master
  "A master port type. Will close automatically when garbage
  collected."
  {:added v1
   :see '[master close! put! dunaj.state/open?]
   :category "Ports"
   :predicate 'master?}
  [port :- (I IOpenAware ICloseablePort ITargetPort)]
  IOpenAware
  (-open? [this] (-open? port))
  ITargetPort
  (-put! [this val fn1-handler] (-put! port val fn1-handler))
  ICloseablePort
  (-close! [this] (-close! port))
  java.lang.Object
  (finalize [this] (-close! port)))

(defn master :- (I IOpenAware ICloseablePort ITargetPort)
  "Returns a closeable target port from a given `_channel_`.
  Port automatically closes when finalized."
  {:added v1
   :see '[master? Master close! put! source target chan timeout
          close-only]
   :category "Ports"}
  [channel :- (I IOpenAware ICloseablePort ITargetPort)]
  (->Master channel))

(defn source :- (I IOpenAware ISourcePort)
  "Returns a source port from a given `_channel_`."
  {:added v1
   :see '[chan target master close-only dunaj.state/open?]
   :category "Ports"}
  [channel :- (I IOpenAware ISourcePort)]
  (reify
    IOpenAware
    (-open? [this] (-open? channel))
    ISourcePort
    (-take! [this fn1-handler] (-take! channel fn1-handler))))

(defn target :- (I IOpenAware ITargetPort)
  "Returns a target port from a given `_channel_` (or master port)."
  {:added v1
   :see '[chan source master close-only dunaj.state/open?]
   :category "Ports"}
  [channel :- (I IOpenAware ITargetPort)]
  (reify
    IOpenAware
    (-open? [this] (-open? channel))
    ITargetPort
    (-put! [this val fn1-handler] (-put! channel val fn1-handler))))

;;; Port alterations

(defn alts! :- [Any Any]
  "Completes at most one of several channel operations. Must be called
  inside a `(go ...)` block. `_ports_` is a vector of channel
  endpoints, which can be either a channel to take from or a vector
  of `[channel-to-put-to val-to-put]`, in any combination.
  Takes will be made as if by `<!`, and puts will be made as if by
  `>!`.

  Unless the `:priority` option is `true`, if more than one
  port operation is ready a non-deterministic choice will be made.

  If no operation is ready and a `:default` value is supplied,
  `[default-val :default]` will be returned, otherwise `alts!` will
  park until the first operation to become ready completes.

  Returns `[val port]` of the completed operation, where `val` is
  the value taken for takes, and `nil` for puts.

  `_opts_` are passed as `:key` `val` ... Supported options:

  * `:default` `val` - the value to use if none of the operations are
    immediately ready
  * `:priority` `true` - (default `nil`) when `true`, the operations
    will be tried in order.

  NOTE: there is no guarantee that the port exps or val exprs will be
  used, nor in what order should they be, so they should not be
  depended upon for side effects."
  {:added v1
   :see '[alts!! alt!! alt!]
   :category "Primary"}
  [ports :- [(U ISourcePort [ITargetPort PortVal])] & {:as opts}]
  (assert nil "alts! used not in (go ...) block"))

(defn alts!! :- [Any Any]
  "Like `alts!`, except takes will be made as if by `<!!`, and puts
  will be made as if by `>!!`, will block until completed, and not
  intended for use in `(go ...)` blocks."
  {:added v1
   :see '[alts! alt!! alt!]
   :category "Primary"}
  [ports :- [(U ISourcePort [ITargetPort PortVal])] & {:as opts}]
  (not-implemented))

(defmacro alt!!
  "Like `alt!`, except as if by `alts!!`, will block until completed,
  and not intended for use in `(go ...)` blocks."
  {:added v1
   :see '[alts!! alts! alt!]
   :category "Primary"}
  [& clauses]
  (not-implemented))

(defmacro alt!
  "Makes a single choice between one of several channel operations,
  as if by `alts!`, returning the value of the result expr
  corresponding to the operation completed.
  Must be called inside a `(go ...)` block.

  Each clause takes the form of:

  * `channel-op[s]` `result-expr`

  where channel-ops is one of:

  * `take-port` - a single port to take
  * `[take-port | [put-port put-val] ...]` - a vector of ports
    as per `alts!`
  * `:default` | `:priority` - an option for `alts!`

  and `result-expr` is either a list beginning with a vector,
  whereupon that vector will be treated as a binding for the
  `[val port]` return of the operation, else any other expression.

  .Usage example
  [source,clojure]
  --
  (alt!
    [c t] ([val ch] (foo ch val))
    x ([v] v)
    [[out val]] :wrote
    :default 42)
  --

  Each option may appear at most once. The choice and parking
  characteristics are those of `alts!`."
  {:added v1
   :see '[alts!! alt!! alts!]
   :category "Primary"}
  [& clauses]
  (not-implemented))

;;; Operations

(defn map! :- ISourcePort
  "Takes a function `_mapf_` and a collection of source `_ports_`,
  and returns a source port which contains the values produced by
  applying `_mapf_` to the set of first items taken from each
  source port, followed by applying `_mapf_` to the set of second
  items from each port, until any one of the `_ports_` is closed,
  at which point the output port will be closed.

  The returned port will be unbuffered by default, or a
  `_buf-or-n_` can be supplied"
  {:added v1
   :category "Operations"}
  ([mapf :- AnyFn, ports :- [ISourcePort]]
   (map! mapf ports nil))
  ([mapf :- AnyFn, ports :- [ISourcePort],
    buf-or-n :- (U Integer IMutableCollection)]
   (not-implemented)))

(defn pipe! :- ISourcePort
  "Takes items from the `_from_` source port and supplies them to
  the `_to_` target port. By default, the `_to_` port will be closed
  when the `_from_` port closes, but can be determined by the
  `_keep-open?_` parameter."
  {:added v1
   :see '[pipeline!]
   :category "Operations"}
  ([from :- ISourcePort, to :- ITargetPort] (pipe! from to false))
  ([from :- ISourcePort, to :- ITargetPort, keep-open? :- Boolean]
   (not-implemented)))

(defn pipeline! :- ISourcePort
  "Takes items from the `_from_` source port and supplies them to
  the `_to_` target port, subject to the transducer `_xform_`, with
  parallelism `_n_`. Because it is parallel, the transducer will be
  applied independently to each item, not across items, and
  may produce zero or more outputs per input.  Outputs will be
  returned in order relative to the inputs. By default, the `_to_`
  port will be closed when the `_from_` port closes, but can be
  determined by the `_keep-open?_` parameter. Will stop consuming
  the `_from_` port if the `_to_` port closes.

  NOTE: This function should be used for computational parallelism.
  If you have multiple blocking operations to put in flight, use
  `pipeline-blocking!` instead. If you have multiple asynchronous
  operations to put in flight, use `pipeline-async!` instead."
  {:added v1
   :see '[pipe! pipeline-blocking! pipeline-async!]
   :category "Operations"}
  ([n :- Integer, to :- ITargetPort, xform :- Any,
    from :- ISourcePort]
   (pipeline! n to xform from false))
  ([n :- Integer, to :- ITargetPort, xform :- Any,
    from :- ISourcePort, keep-open? :- Boolean]
   (pipeline! n to xform from keep-open? nil))
  ([n :- Integer, to :- ITargetPort, xform :- Any,
    from :- ISourcePort, keep-open? :- Boolean, ex-handler :- AnyFn]
   (not-implemented)))

(defn pipeline-blocking! :- ISourcePort
  "Like `pipeline!`, but for blocking operations."
  {:added v1
   :see '[pipe! pipeline! pipeline-async!]
   :category "Operations"}
  ([n :- Integer, to :- ITargetPort, xform :- Any,
    from :- ISourcePort]
   (pipeline-blocking! n to xform from false))
  ([n :- Integer, to :- ITargetPort, xform :- Any,
    from :- ISourcePort, keep-open? :- Boolean]
   (pipeline-blocking! n to xform from keep-open? nil))
  ([n :- Integer, to :- ITargetPort, xform :- Any,
    from :- ISourcePort, keep-open? :- Boolean, ex-handler :- AnyFn]
   (not-implemented)))

(defn pipeline-async! :- ISourcePort
  "Takes items from the `_from_` source port and supplies them to
  the `_to_` target port, subject to the async function `_af_`,
  with parallelism `_n_`. `_af_` must be a function of two arguments,
  the first an input value and the second a target port on which to
  place the result(s). `_af_` must close the port before
  returning.

  The presumption is that `_af_` will return immediately, having
  launched some asynchronous operation (i.e. in another thread)
  whose completion/callback will manipulate the result port.
  Outputs will be returned in order relative to the inputs.
  By default, the `_to_` port will be closed when the `_from_` port
  closes, but can be determined by the `_keep-open?_` parameter.
  Will stop consuming the `_from_` port if the `_to_` port closes."
  {:added v1
   :see '[pipe! pipeline-blocking! pipeline!]
   :category "Operations"}
  ([n :- Integer, to :- ITargetPort, af :- AnyFn, from :- ISourcePort]
   (pipeline-async! n to af from true))
  ([n :- Integer, to :- ITargetPort, af :- AnyFn, from :- ISourcePort,
    keep-open? :- Boolean]
   (not-implemented)))

(defn split! :- [ISourcePort ISourcePort]
  "Takes a `_pred_` and a source `_port_` and returns a vector of two
  source ports, the first of which will contain the values for
  which the predicate `_pred_` returned `true`, the second those
  for which it returned `false`.

  The out ports will be unbuffered by default, or two buf-or-ns
  can be supplied. The ports will close after the `_port_`
  has closed."
  {:added v1
   :category "Operations"}
  ([pred :- Predicate, port :- ISourcePort]
   (split! pred port nil nil))
  ([pred :- Predicate, port :- ISourcePort,
    t-buf-or-n :- (U Integer IMutableCollection),
    f-buf-or-n :- (U Integer IMutableCollection)]
   (not-implemented)))

(defn reduce! :- ISourcePort
  "Returns a source port containing the single result of applying
  `_reducef_` to `_init_` and the first item from the
  `_port_`, then applying `_reducef_` to that result and the 2nd
  item, etc. `_reducef_` should be a function of 2 arguments.
  If the `_port_` closes without yielding items, returns `_init_`
  and `_reducef_` is not called.

  IMPORTANT: `_port_` must close before reduce produces a result."
  {:added v1
   :see '[into!]
   :category "Operations"}
  [reducef :- (Fn [Any Any Any]), init :- Any, port :- ISourcePort]
  (not-implemented))

(defn onto-chan! :- ISourcePort
  "Puts the contents of `_coll_` into the supplied target `_port_`.

  Returns a source port which will close after the items are copied.

  By default the `_port_` will be closed after the items are copied,
  but can be determined by the `_keep-open?_` parameter."
  {:added v1
   :see '[to-chan into!]
   :category "Operations"}
  ([port :- ITargetPort, coll :- IRed] (onto-chan! port coll true))
  ([port :- ITargetPort, coll :- IRed, keep-open? :- Boolean]
   (not-implemented)))

(defn to-chan :- ISourcePort
  "Creates and returns a source port which contains the contents of
  `_coll_`, closing when exhausted."
  {:added v1
   :see '[onto-chan into!]
   :category "Operations"}
  [coll :- IRed]
  (not-implemented))

(defn merge! :- ISourcePort
  "Takes a collection of source `_ports_` and returns a source port
  which contains all values taken from them. The returned channel
  will be unbuffered by default, or a `_buf-or-n_` can be supplied.
  The port will close after all the source `_ports_` have closed."
  {:added v1
   :see '[pipe!]
   :category "Operations"}
  ([ports :- [ISourcePort]] (merge! ports nil))
  ([ports :- [ISourcePort],
    buf-or-n :- (U Integer IMutableCollection)]
   (not-implemented)))

(defn into! :- ISourcePort
  "Returns a source port containing the single (collection) result
  of the items taken from the `_port_` conjoined to the supplied
  `_coll_`. `_port_` must close before into! produces a result."
  {:added v1
   :see '[to-chan onto-chan! reduce!]
   :category "Operations"}
  [coll :- IRed, port :- ISourcePort]
  (not-implemented))

(defn take-n! :- ISourcePort
  "Returns a source port that will return, at most, `_n_` items from
  `_port_`. After `_n_` items have been returned, or `_port_` has
  been closed, the returned port will close.

  The output port is unbuffered by default, unless `_buf-or-n_` is
  given."
  {:added v1
   :see '[pipe!]
   :category "Operations"}
  ([n :- Integer, port :- ISourcePort] (take-n! n port nil))
  ([n :- Integer, port :- ISourcePort,
    buf-or-n :- (U Integer IMutableCollection)]
   (not-implemented)))

;;; Mults

(defprotocol IMult
  "An abstract type protocol for Mults."
  {:added v1
   :category "Mult"
   :see '[mult! tap! untap!]
   :predicate 'mult?}
  (-tap! :- nil
    "Attaches `_port_` to `_this_` mult, using `_close?_` to determine
    whether `_port_` should be closed if `_this_` Mult's source
    is closed."
    [this port :- ITargetPort, close? :- Boolean])
  (-untap! :- nil
    "Detached `_port_` from `_this_` mult."
    [this port :- ITargetPort])
  (-untap-all! :- nil
    "Detaches all taps from `_this_` mult."
    [this]))

(defn mult! :- IMult
  "Creates and returns a mult(iple) of the supplied `_port_`. Ports
  containing copies of the `_port_` can be created with `tap!`, and
  detached with `untap!`.

  Each item is distributed to all taps in parallel and synchronously,
  i.e. each tap must accept before the next item is distributed. Use
  buffering/windowing to prevent slow taps from holding up the mult.

  Items received when there are no taps get dropped.

  If a tap put throws an exception, it will be removed from the mult."
  {:added v1
   :see '[mult? tap! untap! untap-all!]
   :category "Mult"}
  [port :- ISourcePort]
  (not-implemented))

(defn tap! :- nil
  "Copies the `_mult_` source onto the supplied target `_port_`.

  By default the `_port_` will be closed when the source closes,
  but can be determined by the `_keep-open?_` parameter."
  {:added v1
   :see '[mult? mult! untap! untap-all!]
   :category "Mult"}
  ([mult :- IMult, port :- ITargetPort] (tap! mult port false))
  ([mult :- IMult, port :- ITargetPort, keep-open? :- Boolean]
   (not-implemented)))

(defn untap! :- nil
  "Disconnects a target `_port_` from a `_mult_`."
  {:added v1
   :see '[mult? mult! tap! untap-all!]
   :category "Mult"}
  [mult :- IMult, port :- ITargetPort]
  (not-implemented))

(defn untap-all! :- nil
  "Disconnects all target ports from a `_mult_`."
  {:added v1
   :see '[mult? mult! untap! tap!]
   :category "Mult"}
  [mult :- IMult]
  (not-implemented))

;; Support taps for all c.l.IRef objects

(defn ^:private wfn
  [master key ref old new]
  ;; BUG in core.async: mult processing loop holds onto last value
  ;; (put! master [ref old new])
  (put! master [nil old new]))

(def ^:private reference-mults :- java.util.Map
  (java.util.Collections/synchronizedMap (java.util.WeakHashMap.)))

(defn ^:private provide-mult!
  [r]
  (if-let [[m s :as ms] (.get reference-mults r)]
    ms
    (let [ch (chan)
          m (mult! ch)
          s #{}
          ms [m s]]
      ;; chan must close when r gets finalized
      (add-watch r ::mult (partial wfn (master ch)))
      (.put reference-mults r ms)
      ms)))

(defn ^:private tap!*
  [r ch close?]
  (locking reference-mults
    (let [[mult s] (provide-mult! r)]
      (-tap! mult ch close?)
      (.put reference-mults r [mult (conj s ch)]))))

(defn ^:private untap!*
  [r ch]
  (locking reference-mults
    (let [[mult s] (provide-mult! r)
          s (disj s ch)]
      (-untap! mult ch)
      (when (empty? s)
        (remove-watch r ::mult)
        (.remove reference-mults r)))))

(defn ^:private untap-all!*
  [r]
  (locking reference-mults
    (let [[mult _] (provide-mult! r)]
      (-untap-all! mult)
      (remove-watch r ::mult)
      (.remove reference-mults r))))

(extend-protocol! IMult
  clojure.lang.IRef
  (-tap! [this ch close?] (tap!* this ch close?))
  (-untap! [this ch] (untap!* this ch))
  (-untap-all! [this] (untap-all!* this)))

;;; Mix

(def Mix :- Signature
  "A type signature for Mix object."
  {:added v1
   :see '[mix]
   :category "Mix"}
  cap/Mix)

(defn mix :- Mix
  "Creates and returns a mix of one or more input ports which will
  be put on the supplied `_out_` target port. Input sources can be
  added to the mix with `admix!`, and removed with `unmix!`.
  A mix supports soloing, muting and pausing multiple inputs
  atomically using `toggle!`, and can solo using either muting or
  pausing as determined by `:solo-mode` value in the configuration.

  Each port can have zero or more boolean modes set via `toggle!`:

  * `:solo`  - when `true`, only this (ond other soloed) port(s) will
    appear in the mix output port. `:mute` and `:pause` states
    of soloed ports are ignored. If `:solo-mode` is `:mute`,
    non-soloed ports are muted, if `:pause`, non-soloed
    ports are paused.

  * `:mute` - muted ports will have their contents consumed but not
    included in the mix

  * `:pause` - paused ports will not have their contents consumed
    (and thus also not included in the mix)"
  {:added v1
   :see '[Mix admix! unmix! unmix-all! toggle!]
   :category "Mix"}
  [out :- ITargetPort]
  (not-implemented))

(defn admix! :- nil
  "Adds source `_port_` as an input to the `_mix_`."
  {:added v1
   :see '[Mix mix! unmix! unmix-all! toggle!]
   :category "Mix"}
  [mix :- Mix, port :- ISourcePort]
  (not-implemented))

(defn unmix! :- nil
  "Removes `_port_` as an input to the `_mix_`."
  {:added v1
   :see '[Mix admix! mix! unmix-all! toggle!]
   :category "Mix"}
  [mix :- Mix, port :- ISourcePort]
  (not-implemented))

(defn unmix-all! :- nil
  "Removes all inputs from the `_mix_`"
  {:added v1
   :see '[Mix admix! unmix! mix! toggle!]
   :category "Mix"}
  [mix :- Mix]
  (not-implemented))

(defn toggle! :- nil
  "Atomically sets the state(s) of one or more ports in a `_mix_`.
  The `_state-map_` is a map of `channels -> channel-state-map`.

  A `channel-state-map` is a map of `attrs -> boolean`, where `attr`
  is one or more of `:mute`, `:pause` or `:solo`.
  Any states supplied are merged with the current state.

  NOTE: Ports can be added to a `_mix_` via `toggle!`, which can be
  used to add ports in a particular (e.g. paused) state."
  {:added v1
   :see '[Mix admix! unmix! unmix-all! mix!]
   :category "Mix"}
  [mix :- Mix, state-map :- {ISourcePort KeywordMap}]
  (not-implemented))

;;; Pub Sub

(defprotocol IPub
  "An abstract type protocol for Pub."
  {:added v1
   :category "Pub"
   :see '[pub! sub!]
   :predicate 'pub?}
  (-sub! :- nil
    "Subscribes target `_port_` in `_this_` Pub under given `_topic_`.
    `_close?_` determines whether `_port_` will be closed when
    `_this_` Pub's source closes."
    [this topic :- Any, port :- ITargetPort, close? :- Boolean])
  (-unsub! :- nil
    "Unsubscribes `_port_` from `_this_` Pub and from given
    `_topic_`."
    [this topic :- Any, port :- ITargetPort])
  (-unsub-all! :- nil
    "Unsubscribes all ports subscribed to given `_topic_`, or
    unsunscribes all ports, is no topic is given."
    [this] [this topic :- Any]))

(defn pub! :- IPub
  "Creates and returns a pub(lication) of the supplied source
  `_port_`, partitioned into topics by the `_topic-fn_`.
  `_topic-fn_` will be applied to each value on the port and the
  result will determine the 'topic' on which that value will be put.

  Ports can be subscribed to receive copies of topics using `sub!`,
  and unsubscribed using `unsub!`. Each topic will be handled by an
  internal mult on a dedicated channel. By default these internal
  channels are unbuffered, but a `_buf-fn_` can be supplied which,
  given a topic, creates a buffer with desired properties.

  Each item is distributed to all subs in parallel and synchronously,
  i.e. each sub must accept before the next item is distributed. Use
  buffering/windowing to prevent slow subs from holding up the pub.

  Items received when there are no matching subs get dropped.

  NOTE: If buf-fns are used then each topic is handled
  asynchronously, i.e. if a port is subscribed to more than one
  topic it should not expect them to be interleaved identically with
  the source."
  {:added v1
   :see '[pub? sub! unsub! unsub-all!]
   :category "Pub"}
  ([port :- ISourcePort, topic-fn :- (Fn [Any PortVal])]
   (pub! port topic-fn (constantly nil)))
  ([port :- ISourcePort, topic-fn :- (Fn [Any PortVal]),
    buf-fn :- (Fn [(U Integer IMutableCollection) Any])]
   (not-implemented)))

(defn sub! :- nil
  "Subscribes a `_port_` to a topic of a `_pub_`.

  By default the `_port_` will be closed when the source closes,
  but can be determined by the `_keep-open?_` parameter."
  {:added v1
   :see '[pub? pub! unsub! unsub-all!]
   :category "Pub"}
  ([pub :- IPub, topic :- Any, port :- ITargetPort]
   (sub! pub topic port false))
  ([pub :- IPub, topic :- Any, port :- ITargetPort,
    keep-open? :- Boolean]
   (not-implemented)))

(defn unsub! :- nil
  "Unsubscribes a `_port_` from a `_topic_` of a `_pub_`."
  {:added v1
   :see '[pub? sub! pub! unsub-all!]
   :category "Pub"}
  [pub :- IPub, topic :- Any, port :- ITargetPort]
  (not-implemented))

(defn unsub-all! :- nil
  "Unsubscribes all ports from a `_pub_`, or all ports subscribed to
  `_topic_` of a `_pub_`."
  {:added v1
   :see '[pub? sub! unsub! pub!]
   :category "Pub"}
  ([pub :- IPub] (not-implemented))
  ([pub :- IPub, topic :- Any] (not-implemented)))
