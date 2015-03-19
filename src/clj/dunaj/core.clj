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

(ns dunaj.core
  "An entry point to the `dunaj API`.

  IMPORTANT: Do not require this namespace directly.
  Idiomatic use is through the `:api` section of the
  `<<dunaj.lib.api.ad#ns,ns>>` macro."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require [clojure.bootstrap]
            [clojure.core.async]
            [clojure.bridge]
            [dunaj.type]
            [dunaj.boolean]
            [dunaj.host]
            [dunaj.host.int]
            [dunaj.math]
            [dunaj.math.precise]
            [dunaj.math.unchecked]
            [dunaj.math.angle]
            [dunaj.bit]
            [dunaj.host.number]
            [dunaj.compare]
            [dunaj.state]
            [dunaj.flow :refer [let]]
            [dunaj.threading]
            [dunaj.threading.last]
            [dunaj.threading.second]
            [dunaj.feature :refer [alter-meta!]]
            [dunaj.poly]
            [dunaj.coll :refer [second assoc]]
            [dunaj.function :refer [defn fn]]
            [dunaj.concurrent]
            [dunaj.concurrent.forkjoin]
            [dunaj.coll.helper]
            [dunaj.host.batch]
            [dunaj.host.array]
            [dunaj.char]
            [dunaj.string]
            [dunaj.time]
            [dunaj.identifier]
            [dunaj.error]
            [dunaj.concurrent.thread]
            [dunaj.macro :refer [defmacro]]
            [dunaj.uri]
            [dunaj.state.var]
            [dunaj.state.ref]
            [dunaj.state.basic]
            [dunaj.buffer]
            [dunaj.uuid]
            [dunaj.concurrent.agent]
            [dunaj.namespace]
            [dunaj.concurrent.port]
            [dunaj.state.weak]
            [dunaj.set]
            [dunaj.coll.empty-list]
            [dunaj.coll.cons-seq]
            [dunaj.coll.lazy-seq]
            [dunaj.coll.vector-section]
            [dunaj.coll.bvt-vector]
            [dunaj.coll.rrbt-vector]
            [dunaj.coll.primitive-vector]
            [dunaj.coll.linked-list]
            [dunaj.coll.batched-queue]
            [dunaj.coll.tuple]
            [dunaj.coll.hamt-map]
            [dunaj.coll.hamt-set]
            [dunaj.coll.array-map]
            [dunaj.coll.rbt-sorted-map]
            [dunaj.coll.rbt-sorted-set]
            [dunaj.coll.lazy-seq-map]
            [dunaj.coll.lazy-seq-set]
            [dunaj.coll.util]
            [dunaj.coll.default :refer [vec set]]
            [dunaj.coll.recipe :refer [remove]]
            [dunaj.function.default]
            [dunaj.math.random]
            [dunaj.format]
            [dunaj.format.charset]
            [dunaj.regex]
            [dunaj.format.helper]
            [dunaj.format.parser]
            [dunaj.format.printer]
            [dunaj.format.json]
            [dunaj.format.html]
            [dunaj.format.edn]
            [dunaj.format.clj]
            [dunaj.resource]
            [dunaj.resource.helper]
            [dunaj.resource.selector]
            [dunaj.resource.file]
            [dunaj.resource.host]
            [dunaj.resource.pipe]
            [dunaj.resource.udp]
            [dunaj.resource.tcp]
            [dunaj.resource.loopback]
            [dunaj.resource.secure]
            [dunaj.resource.http]
            [dunaj.concurrent.parallel]
            [dunaj.version]
            [dunaj.lib]
            [dunaj.env]
            [dunaj.dev]
            ;; [dunaj.main] ;; circular dependency
            [dunaj.repl]))


;;;; Implementation details

(defmacro refer
  [ns-name syms]
  `(clojure.core/refer '~ns-name :only (~'ff '~syms)))


;;;; Public API

(defn init-api
  [references & args]
  (let [exclude (set (second args))
        ff (fn [syms] (vec (remove exclude syms)))]
    ;; fully qualified special symbols only
    (alter-meta! clojure.core/*ns* assoc :qualified-specials true)
    ;; Refer peculiar special symbols directly. This is needed only
    ;; for syntax quote to work correctly when handling those symbols.
    (refer clojure.core [catch finally &])
    ;; Refer common API
    (refer dunaj.type
           [Any AnyFn Fn Maybe Va Predicate KeywordMap U I Required
            Macro])
    (refer dunaj.boolean
           [Boolean boolean? boolean false? true? not and or])
    (refer dunaj.host
           [set! . .. proxy proxy-super Class class class-instance?
            ensure-class-instance provide-class keyword->class
            BatchManager AnyBatch Batch ArrayManager AnyArray Array
            definterface bean->map])
    (refer dunaj.host.int [])
    (refer dunaj.math
           [Number number? Integer integer? Float float? Decimal
            decimal? Rational rational? numerical? num zero? one?
            pos? neg? npos? nneg? even? odd? < <= > >= ==
            trunc + - * / inc dec min max quot rem mod round floor
            ceil with-precision pow sqrt])
    (refer dunaj.math.precise [])
    (refer dunaj.math.unchecked [])
    (refer dunaj.math.angle [])
    (refer dunaj.bit [])
    (refer dunaj.host.number [])
    (refer dunaj.compare
           [identical? sentinel nil? some? = not= distinct? compare])
    (refer dunaj.state
           [IReference IMutable IAdjustable IAtomic IPending
            IOpenAware io! reference? deref mutable? reset!
            adjustable? adjust! atomic? cas! switch! alter! trade!
            realized? open? cancelled? cancellable? cancel! clone])
    (refer dunaj.flow
           [let letfn if if-not if-let if-some when when-not when-let
            when-some cond condp case comment recur loop dotimes doto
            while eval quote do delay force])
    (refer dunaj.threading
           [-> ->> as-> cond-> cond->> some-> some->>])
    (refer dunaj.threading.last [])
    (refer dunaj.threading.second [])
    (refer dunaj.feature
           [IMeta IConfig validator meta assoc-meta config update-meta
            alter-meta! reset-meta! alter-config! reset-config!])
    (refer dunaj.poly
           [protocol? defprotocol satisfies? type? deftype
            type-instance? record? defrecord record-instance?
            instance? ensure-instance reify type identical-type?
            extends? extend! extend-protocol! extend-type!
            defmulti defmethod Multimethod])
    (refer dunaj.coll
           [IRed provide-sequential Transducer
            Postponed postponed? postponed advance unsafe-advance!
            Reduced reduced? reduced red? ensure-batchable
            ensure-unpackable reduce provide-collection
            reducing reduce-augmented reduce-one-augmented
            transduce transduce-one seq? seq first ffirst second
            rest next nnext nthnext nthrest when-first counted?
            count several? single? double? triple? quad? quint?
            empty? not-empty? empty not-empty peek contains? get
            get-in nth invertible? invert flippable? flip
            reversible? reverse sorted? ascending? sliceable?
            slice sectionable? sorted-sectionable? section
            homogeneous? item-type sequential? coll? conj
            associative? assoc assoc-in update update-in stacked?
            pop catenable? cat set? disj bag? disj-all map? dissoc
            dissoc-in multimap? dissoc-one list? vector?])
    (refer dunaj.function
           [invocable? invoke apply defn fn fn? trampoline
            partial comp complement fnil juxt identity constantly
            nop memoize])
    (refer dunaj.concurrent
           [IFuture IExecutor ITaskExecutor
            locking future? execute submit future promise deliver!])
    (refer dunaj.concurrent.forkjoin
           [IFoldable folding fold fold-augmented transfold])
    (refer dunaj.coll.helper [])
    (refer dunaj.host.batch [batch-manager])
    (refer dunaj.host.array [array-manager])
    (refer dunaj.char [Char char? char])
    (refer dunaj.string
           [canonical? canonical ICharSequence char-sequence? String
            string? str ->str blank?])
    (refer dunaj.time
           [IInstant instant? instant now IDuration duration?
            milliseconds nanoseconds])
    (refer dunaj.identifier
           [INamed name namespace Symbol symbol? symbol Keyword
            keyword? keyword])
    (refer dunaj.error
           [IException exception? error illegal-argument illegal-state
            io index-out-of-bounds no-such-element npe
            unsupported-operation ex-info ex-data try throw])
    (refer dunaj.concurrent.thread
           [Thread thread-local? pass! current-thread
            ensure-thread-local sleep])
    (refer dunaj.macro
           [defmacro gensym macroexpand macroexpand-1
            macroexpand-all])
    (refer dunaj.uri [])
    (refer dunaj.state.var
           [Var var? var def defonce declare with-bindings])
    (refer dunaj.state.ref
           [IRef ref? ensure commute alter reset ref dosync])
    (refer dunaj.state.basic [Atom atom box local])
    (refer dunaj.buffer [])
    (refer dunaj.uuid [Uuid uuid])
    (refer dunaj.concurrent.agent
           [Agent agent? agent current-agent shutdown-agents!
            send! send-off! await await-for
            release-pending-sends! restart-agent!])
    (refer dunaj.namespace [])
    (refer dunaj.concurrent.port
           [take! <!! <! put! >!! >! close! go go-loop thread
            chan timeout alt! alt!! tap! untap! PortVal])
    (refer dunaj.state.weak [])
    (refer dunaj.set [])
    (refer dunaj.coll.empty-list [])
    (refer dunaj.coll.cons-seq [cons])
    (refer dunaj.coll.lazy-seq [lazy-seq lazy-cat])
    (refer dunaj.coll.vector-section [])
    (refer dunaj.coll.bvt-vector [])
    (refer dunaj.coll.rrbt-vector [])
    (refer dunaj.coll.primitive-vector [])
    (refer dunaj.coll.linked-list [])
    (refer dunaj.coll.batched-queue [])
    (refer dunaj.coll.tuple [tuple pair key val])
    (refer dunaj.coll.hamt-map [])
    (refer dunaj.coll.hamt-set [])
    (refer dunaj.coll.array-map [])
    (refer dunaj.coll.rbt-sorted-map [])
    (refer dunaj.coll.rbt-sorted-set [])
    (refer dunaj.coll.lazy-seq-map [])
    (refer dunaj.coll.lazy-seq-set [])
    (refer dunaj.coll.util
           [sequence into group-by frequencies merge merge-with
            revlist batched reduce-batched
            fold-batched unpacked reduce-unpacked fold-unpacked
            every? some not-any? not-every? dorun doall for dored
            doseq last shuffle sort sort-by butlast recipe])
    (refer dunaj.coll.default
           [empty-seq empty-lst empty-que empty-vec empty-vec-of
            empty-set empty-sorted-set empty-map empty-sorted-map
            lst ->lst que ->que vec ->vec vec-of ->vec-of set ->set
            sorted-set ->sorted-set sorted-set-by ->sorted-set-by
            zipmap ->map sorted-zipmap ->sorted-map
            sorted-zipmap-by ->sorted-map-by])
    (refer dunaj.coll.recipe
           [concat zip indexed interleave traverse range repeat
            cycle repeatedly iterate map mapcat filter remove
            replace keep take-while flatten append-coll append
            keys vals take take-nth take-last drop drop-while
            drop-last interpose cap throwing-cap distinct
            dedupe reductions partition partition-all partition-by
            lines split-at split-with])
    (refer dunaj.function.default [])
    (refer dunaj.math.random [rand rand-integer rand-nth sample])
    (refer dunaj.format [parse parse-whole print print-one])
    (refer dunaj.format.charset
           [utf-8 utf-16 default-charset charset-formatter])
    (refer dunaj.regex [matches])
    (refer dunaj.format.helper [])
    (refer dunaj.format.parser [])
    (refer dunaj.format.printer [])
    (refer dunaj.format.html [html])
    (refer dunaj.format.json [json])
    (refer dunaj.format.edn [edn])
    (refer dunaj.format.clj [clj])
    (refer dunaj.resource
           [release-scope! in-scope? scope-push! grab-scope with-scope
            with-io-scope resource acquire! read read! read-one!
            write! write-one! slurp spit! exchange! format transform
            deps assoc-deps system start!])
    (refer dunaj.resource.helper [])
    (refer dunaj.resource.selector
           [selector select select-now register! deregister!])
    (refer dunaj.resource.file [file])
    (refer dunaj.resource.host [classpath])
    (refer dunaj.resource.http [http https])
    (refer dunaj.concurrent.parallel
           [pmap pmap-unordered pcalls pcalls-unordered pvalues
            pvalues-unordered])
    (refer dunaj.lib [])
    (refer dunaj.env
           [current-ns out err in out! err! pr! prn! print! println!
            pp! current-version command-line-args])
    (refer dunaj.dev
           [scratch warn-on-reflection! not-implemented assert
            time set-trace! trace set-color! pt])
    (refer dunaj.repl [])))
