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

(ns dunaj.concurrent.agent
  "Agents, a reference object to the result of an asynchronous
  computation, with additional distinctive features.

  Action is a function that will be called with agent's old state and
  the value it produces will be the new state of an agent. Actions
  are 'sent' to the agents with `<<send_BANG_,send!>>` function and
  its variants. There are additional features which are specific to
  agents' actions:

  * Actions sent to the agent are enqueued and dispatched
    consecutively.
  * Action which throws an exception sets the agent into the failed
    state. The failed state may be later restarted and not yet
    dispatched actions will continue to process the agent's state.
    See `<<restart_agent_BANG_,restart-agent!>>` for details.
  * Agent's state may be observed by
    <<dunaj.concurrent.port.api.ad#tap_BANG_,tapping>>
    into the agent and validated with a custom validator function.

  [NOTE]
  Actions to the agent may be dispatched even in a ref transaction.
  In that case, the actual sending is postponed until the transaction
  completes. This is done to avoid multiple sends when a transaction
  restarts.

  [NOTE]
  Another special case is the sending of an action while currently
  in an action. In that case, the sends to any agent are postponed
  until current action completes. See
  `<<release_pending_sends_BANG_,release-pending-sends!>>` for
  details. Note that pending sends are not dispatched if the action
  fails."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require [clojure.core :refer [agent-error]]
            [clojure.bootstrap :refer [v1]]
            [dunaj.type :refer [Fn Maybe U Any AnyFn Va]]
            [dunaj.boolean :refer [Boolean boolean]]
            [dunaj.host :refer [class-instance?]]
            [dunaj.math :refer [Integer zero? rem]]
            [dunaj.state :refer [IReference IMutable]]
            [dunaj.flow :refer [let when]]
            [dunaj.feature :refer [IMeta IMutableMeta]]
            [dunaj.poly :refer [deftype reify]]
            [dunaj.function :refer [defn apply]]
            [dunaj.concurrent :refer [IExecutor]]
            [dunaj.time :refer [IDuration milliseconds nanoseconds]]
            [dunaj.error :refer [IFailAware IErrorHandleable]]
            [dunaj.state.var :refer [Var def defalias var]]))


;;;; Implementation details

(def ^:dynamic ^:private *default-send-off-executor* :- IExecutor
  clojure.lang.Agent/soloExecutor)

(def ^:dynamic ^:private *default-send-executor* :- IExecutor
  clojure.lang.Agent/pooledExecutor)

(defn ^:private shutdown-pool! :- nil
  "Initiates a shutsdown of a given `_pool_`. Returns nil."
  [pool :- Any]
  (when (class-instance? java.util.concurrent.ExecutorService pool)
    (.shutdown ^java.util.concurrent.ExecutorService pool)))


;;;; Public API

(deftype Agent
  "A type for agents."
  {:added v1
   :see '[agent]
   :predicate 'agent?}
  clojure.lang.Agent
  IFailAware
  (-error [this] (agent-error this))
  IErrorHandleable
  (-error-handler [this]
    (reify
      IReference
      (-deref [inner] (.getErrorHandler this))
      IMutable
      (-reset! [inner f] (.setErrorHandler this f))))
  (-error-mode [this]
    (reify
      IReference
      (-deref [inner] (.getErrorMode this))
      IMutable
      (-reset! [inner m] (.setErrorMode this m))))
  ;; JVM: following protocols are already implemented
  ;; IValidator ;; extend-protocol on c.l.IRef in d.feature
  ;; IMult ;; extend-protocol on c.l.IRef in d.c.port
  IMeta
  ;; IMutableMeta ;; extend-protocol on c.l.IReference in d.feature
  IReference)

(defalias agent
  {:doc "Creates and returns an agent with an initial value of
        `_state_` and zero or more `_options_` (in any order):

        * `:meta` `metadata-map`. If supplied, it will become the
          metadata on the agent.
        * `:validator` `validate-fn`. Must be `nil` or a
          side-effect-free fn of one argument, which will be passed
          the intended new state on any state change. If the new
          state is unacceptable, the `validate-fn` should return
          `false` or throw an exception.
        * `:error-handler` `handler-fn`. Ss called if an action throws
          an exception or if `validate-fn` rejects a new state.
        * `:error-mode` `mode-keyword`. May be either `:continue`
          (the default if an error-handler is given) or `:fail`
          (the default if no error-handler is given) .

        Both error handler and validator may be also set and
        managed through their respective function in
        <<dunaj.error.api.ad#,dunaj.error>> and
        <<dunaj.feature.api.ad#,dunaj.feature>> namespaces."
   :added v1
   :see '[agent? Agent current-agent send! send-off! shutdown-agents!
          dunaj.state/deref dunaj.error/error-handler
          dunaj.error/error dunaj.error/error-mode
          dunaj.feature/validator]
   :tsig (Fn [Agent Any (Va Any)])})

(defn current-agent :- (Maybe Agent)
  "Returns the agent currently running an action on this thread,
  else `nil`."
  {:added v1
   :see '[agent dunaj.concurrent.thread/current-thread]}
  []
  clojure.core/*agent*)

(defn shutdown-agents! :- nil
  "Initiates a shutdown of the default thread pools that back
  the agent system. Running actions will complete, but no new
  actions will be accepted. Returns `nil`."
  {:added v1
   :see '[default-send-executor default-send-off-executor]}
  []
  (shutdown-pool! *default-send-executor*)
  (shutdown-pool! *default-send-off-executor*))

(defalias restart-agent!
  {:doc "When an `_agent_` is failed, changes the `_agent_` state to
        `_new-state_` and then un-fails the `_agent_` so that sends
        are allowed again.

        If a `:clear-actions` true `_option_` is given, any actions
        queued on the agent that were being held while it was failed
        will be discarded, otherwise those held actions will proceed.

        The new-state must pass the validator if any, or restart
        will throw an exception and the agent will remain failed
        with its old state and error.

        IMPORTANT: Tapped channels, if any, will NOT be notified of
        the new state. Throws an exception if the agent
        is not failed. Returns `_new-state_`."
   :added v1
   :see '[await agent]
   :arglists '([agent new-state & options])
   :tsig (Fn [Any Agent Any (Va Any)])}
  clojure.core/restart-agent)

;;; Agent dispatch

(defalias send-via!
  {:doc "Dispatch an `_action_` to an `_agent_`.
        Returns the `_agent_` immediately.
        Subsequently, in a thread supplied by `_executor_`,
        the state of the `_agent_` will be set to the value of:

        `(apply _action_ state-of-agent _args_)`

        NOTE: Action sent during another action is held until the
        current action completes (changes the agent’s state)."
   :added v1
   :arglists '([executor agent action & args])
   :see '[send! send-off! await await-for
          dunaj.concurrent/future dunaj.concurrent.thread/thread
          dunaj.concurrent.thread/daemon dunaj.concurrent/IExecutor]
   :tsig (Fn [Agent IExecutor Agent AnyFn (Va Any)])}
  clojure.core/send-via)

(def default-send-executor :- Var
  "A dynamic var holding default `send!` executor."
  {:added v1
   :see '[send! default-send-off-executor send-via! send-off!]}
  (var *default-send-executor*))

(defn send! :- Agent
  "Dispatch an `_action_` to an `_agent_`.
  Returns the `_agent_` immediately. Subsequently, in a thread from
  a thread pool, the state of the `_agent_` will be set to the
  value of:

  `(apply _action_ state-of-agent _args_)`

  NOTE: Action sent during another action is held until the current
  action completes (changes the agent’s state)."
  {:added v1
   :see '[default-send-executor send-off! send-via! await await-for
          dunaj.concurrent/future dunaj.concurrent.thread/thread
          dunaj.concurrent.thread/daemon]}
  [a :- Agent, f :- AnyFn & args :- Any]
  (apply send-via! *default-send-executor* a f args))

(def default-send-off-executor :- Var
  "A dynamic var holding default `send-off!` executor."
  {:added v1
   :see '[send-off! default-send-executor send-via! send!]}
  (var *default-send-off-executor*))

(defn send-off! :- Agent
  "Dispatch a potentially blocking `_action_` to an `_agent_`.
  Returns the `_agent_` immediately. Subsequently, in a separate
  thread, the state of the `_agent_` will be set to the value of:

  `(apply _action_ state-of-agent _args_)`

  NOTE: Action sent during another action is held until the current
  action completes (changes the agent’s state)."
  {:added v1
   :see '[default-send-off-executor send! send-via! await await-for
          dunaj.concurrent/future dunaj.concurrent.thread/thread
          dunaj.concurrent.thread/daemon]}
  [agent :- Agent, action :- AnyFn & args :- Any]
  (apply send-via! *default-send-off-executor* agent action args))

(defalias await
  {:doc "Blocks the current thread (indefinitely!) until all actions
        dispatched thus far, from this thread or agent, to the
        `_agents_` have occurred. Will block on failed agents.
        Returns `nil`.

        NOTE: `await` throws if called in an action.

        CAUTION: Will never return if a failed agent is restarted
        with `:clear-actions` set to `true`."
   :added v1
   :tsig (Fn [nil (Va Agent)])
   :see '[await-for dunaj.concurrent.thread/join restart-agent!]})

(defn await-for :- Boolean
  "Blocks the current thread until all actions dispatched thus
  far (from this thread or agent) to the `_agents_` have occurred,
  or the `_timeout_` (duration) has elapsed. Returns `false` if
  returning due to `_timeout_`, `true` otherwise."
  {:added v1
   :see '[await dunaj.concurrent.thread/join restart-agent!]}
  [timeout :- (U Integer IDuration) & agents :- Agent]
  (let [ms (milliseconds timeout)]
    (boolean (apply clojure.core/await-for ms agents))))

(defalias release-pending-sends!
  {:doc "Dispatches any pending sent actions immediately.
        Returns the number of actions dispatched.
        If no action is occurring, does nothing and returns 0.

        Normally, actions sent directly or indirectly during another
        action are held until the action completes (changes the
        agent’s state). This function can be used to dispatch any
        pending sent actions immediately.

        NOTE: This has no impact on actions sent during a ref
        transaction, which are still held until commit."
   :added v1
   :see '[current-agent send send-off send-via restart-agent!]
   :tsig (Fn [Integer])}
  clojure.core/release-pending-sends)
