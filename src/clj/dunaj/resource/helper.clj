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

(ns dunaj.resource.helper
  "Resource helpers."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Maybe U I Fn]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [Batch keyword->class set! class-instance?]]
   [dunaj.host.int :refer [i== i0 iint iloop iadd]]
   [dunaj.math :refer [Integer max neg? == zero?]]
   [dunaj.state :refer [IOpenAware IReference IMutable ICloneable
                        ensure-io reset! ensure-open open?]]
   [dunaj.flow :refer [let loop recur if do cond when if-not if-let]]
   [dunaj.poly :refer [reify deftype]]
   [dunaj.coll :refer
    [IRed ICounted IBatchedRed IHomogeneous item-type Postponed
     reduced? -reduce-batched contains? get reduce first assoc conj
     postponed? postponed unsafe-advance! unsafe-postponed]]
   [dunaj.function :refer [fn defn partial]]
   [dunaj.coll.helper :refer [reduce-with-batched* reduce*]]
   [dunaj.host.batch :refer [provide-batch-size select-item-type]]
   [dunaj.concurrent.thread :refer
    [Thread IThreadLocal IPassableThreadLocal
     current-thread ensure-thread-local]]
   [dunaj.string :refer [String string?]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.error :refer
    [IFailAware IFailable try catch throw fail! error
     fragile opened-fragile unsupported-operation]]
   [dunaj.state.var :refer [alter-root!]]
   [dunaj.coll.util :refer [merge reduce-batched]]
   [dunaj.coll.default]
   [dunaj.resource :refer
    [IImmutableReadable IReleasable IFlushable IReadable
     IAcquirableFactory IWritable ISeekable
     acquire! resource *resource-providers*]]))


;;;; Implementation details

(defn read-resource
  [form]
  {:pre [(string? form)]}
  (if-let [r (resource form)]
    r
    (throw (unsupported-operation "resource type not supported"))))

(clojure.core/alter-var-root
 #'clojure.core/default-data-readers
 clojure.core/assoc 'resource #'dunaj.resource.helper/read-resource)

(deftype ReadableResourceRecipe
  "Reads from the readable resource. Passable thread local."
  [rch :- java.nio.channels.ReadableByteChannel,
   batch-size :- (Maybe Integer),
   resource :- (I IFailable IOpenAware),
   ^:volatile-mutable thread :- (Maybe Thread)]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IThreadLocal
  IPassableThreadLocal
  (-pass! [this new-thread]
    (ensure-thread-local thread)
    (set! thread new-thread)
    this)
  ICloneable
  (-clone [this] (throw (unsupported-operation)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (ensure-io)
    (ensure-thread-local thread)
    (ensure-open resource)
    (let [st (select-item-type requested-type (item-type this))
          batch-size (provide-batch-size
                      (max (or size-hint 0) (or batch-size 0)))
          batch ^java.nio.ByteBuffer
          (java.nio.ByteBuffer/allocateDirect batch-size)
          non-blocking?
          (and
           (class-instance? java.nio.channels.SelectableChannel rch)
           (not
            (.isBlocking ^java.nio.channels.SelectableChannel rch)))
          af (fn af [ret wait?]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (unsafe-postponed
                 @ret #(af (unsafe-advance! ret) false))
                wait? (unsafe-postponed ret #(af ret false))
                (.isOpen rch)
                (let [x (fragile resource (.read rch (.clear batch)))]
                  (cond
                    (neg? x) ret
                    (or (not non-blocking?) (== batch-size x))
                    (recur (reducef ret (.flip batch)) false)
                    (zero? x) (recur ret true)
                    :else (recur (reducef ret (.flip batch)) true)))
                :else ret))]
      (af init false))))


;;;; Public API

(defmacro defreleasable
  "Defines a releasable resource (a resource with limited
  availability), which calls `-release!` when finalized.
  You must explicitly implement `IReleasable`.
  Resources usually also implement `IConfigured` and `IOpenAware`."
  {:added v1
   :see '[dunaj.resource/acquire!]
   :highlight :def}
  [name & args]
  `(dunaj.poly/deftype ~name ~@args
     ICloneable
     (~'-clone [o#] (throw (unsupported-operation)))
     java.lang.Object
     (~'finalize [o#] (dunaj.resource/-release! o#))))

(defn register-factory! :- nil
  "Registers new resource factory under `_scheme_` and returns `nil`."
  {:added v1
   :see '[dunaj.resource/resource]}
  [scheme :- String, resource-factory :- IAcquirableFactory]
  (alter-root! #'*resource-providers* assoc scheme resource-factory))

(defn readable-resource-recipe :- IRed
  "Returns a passable thread local collection recipe that reads
  from a given readable byte channel `_rch_` and forwards exceptions
  to the `_resource_` when failed."
  {:added v1}
  [resource :- (I IFailable IOpenAware),
   rch :- java.nio.channels.ReadableByteChannel,
   batch-size :- (Maybe Integer),
   thread :- (Maybe Thread)]
  (->ReadableResourceRecipe rch batch-size resource thread))

(defn basic-write! :- (U Integer Postponed)
  "Performs a write to a given writable byte channel `_wch_`,
  returning number of bytes written or postponed object if
  channel is it non blocking mode. Thread local."
  {:added v1}
  [resource :- (I IFailable IOpenAware),
   wch :- java.nio.channels.WritableByteChannel,
   batch-size :- (Maybe Integer),
   thread :- (Maybe Thread), coll :- (Maybe IRed)]
  (let [non-blocking?
        (and
         (class-instance? java.nio.channels.SelectableChannel wch)
         (not
          (.isBlocking ^java.nio.channels.SelectableChannel wch)))]
    (ensure-thread-local thread)
    (ensure-open resource)
    (reduce-batched
     (keyword->class :byte) batch-size
     (fn [ret :- Any, batch :- (Batch java.lang.Byte)]
       (let [af (fn af [ret]
                  (if-not (.hasRemaining batch)
                    ret
                    (let [l (.remaining batch)
                          x (iint
                             (fragile resource (.write wch batch)))
                          nret (iadd (iint ret) x)]
                      #_(clojure.core/println
                         "written" l (dunaj.host/class coll))
                      (if (and non-blocking? (not (i== l x)))
                        (unsafe-postponed nret #(af nret))
                        (recur nret)))))]
         (af ret)))
     (i0) (dunaj.host.batch/malleable coll))))
