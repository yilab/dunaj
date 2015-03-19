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

(ns dunaj.host.batch
  "Batches, a wrapped host collections of primitive data, providing
  fast data processing.

  Batch is an abstraction for host specific low level data
  containers that are used by host in bulk data processing and
  manipulation, such as I/O or networking.

  Batch support for given item type is host specific.
  For better interop, some batch methods should be invoked through
  host batch manager. This avoids reflection and simplify user code.
  When you know the concrete batch type, it is best to directly
  call host methods of this type.

  Batches can be created from existing arrays or they can be
  allocated. They support cloning and reducing.

  IMPORTANT: When reducing batches, the reduction function
  may not store received batch between two calls, as batches
  are usually reused (cleared and filled with next data)."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core :refer [assert throw get var declare]]
   [clojure.bootstrap :refer [def v1 deftype replace-var!]]
   [dunaj.type :refer [U Maybe AnyFn Any Fn]]
   [dunaj.boolean :refer [Boolean not and or]]
   [dunaj.host :refer
    [Class BatchManager AnyBatch AnyArray Batch
     class-instance? definterface class provide-class keyword->class]]
   [dunaj.host.int :refer [Int iint isub iinc i== imul idiv idec ipos?
                           i0 inneg? imin imax i1 i<]]
   [dunaj.math :refer [max Integer min]]
   [dunaj.compare :refer [nil? identical?]]
   [dunaj.threading :refer [->]]
   [dunaj.state :refer [ICloneable clone]]
   [dunaj.flow :refer [let if do cond loop recur when doto when-not]]
   [dunaj.poly :refer [Type extend-protocol! satisfies?]]
   [dunaj.coll :refer
    [IRed IBatchedRed reduced? postponed? postponed reduced advance
     unsafe-advance! item-type IHomogeneous IUnpackedRed -section
     -reduce-batched IReducing ISectionable ICounted count counted?
     sectionable?]]
   [dunaj.concurrent.forkjoin :refer [IFoldable folding]]
   [dunaj.coll.helper :refer
    [transduce* reduce* reduce-augmented* adapt* defxform
     finish-advance advance-fn reduced-advance strip-reduced
     reduce-unpacked* reduce-batched* cloned-advance-fn adapt]]
   [dunaj.function :refer [defn fn]]))


;;;; Implementation details

(def ^:private bms :- {Class BatchManager} @#'dunaj.host/bms)

(def ^:private ^:dynamic *default-batch-size* 32)

(def ^:private ^:dynamic *max-batch-size* 8192)

(def ^:private default-item-type :- Class
  "A default item type used when no concrete type is specified
  in the batch creation process."
  java.lang.Byte/TYPE)

(def ^:private mark-field :- java.lang.reflect.Field
  (doto (.getDeclaredField java.nio.Buffer "mark")
    (.setAccessible true)))

(defn ^:private get-mark :- Int
  [x :- java.nio.Buffer]
  (iint (.get mark-field x)))


;;;; Public API

(defn batch-manager :- (Maybe BatchManager)
  "Returns the batch manager for a given `_item-type_`, or `nil`,
  if host does not support batches of `_item-type_`."
  {:added v1
   :see '[batch-manager-from provide-batch-manager batch-support?]}
  [item-type :- (U nil Class Type)]
  (get bms (provide-class item-type)))

(defn batch-manager-from :- BatchManager
  "Returns batch manager from an existing `_batch_`."
  {:added v1
   :see '[batch-manager provide-batch-manager]}
  [batch :- AnyBatch]
  (cond (class-instance? java.nio.ByteBuffer batch)
        (batch-manager java.lang.Byte/TYPE)
        (class-instance? java.nio.CharBuffer batch)
        (batch-manager java.lang.Character/TYPE)
        (class-instance? java.nio.IntBuffer batch)
        (batch-manager java.lang.Integer/TYPE)
        (class-instance? java.nio.LongBuffer batch)
        (batch-manager java.lang.Long/TYPE)
        (class-instance? java.nio.FloatBuffer batch)
        (batch-manager java.lang.Float/TYPE)
        (class-instance? java.nio.DoubleBuffer batch)
        (batch-manager java.lang.Double/TYPE)
        (class-instance? java.nio.ShortBuffer batch)
        (batch-manager java.lang.Short/TYPE)
        (nil? batch) (throw (java.lang.NullPointerException.))
        :else (throw (java.lang.IllegalArgumentException.
                      "Batch not recognized"))))

(defn provide-batch-manager :- BatchManager
  "If batch manager `_bm_` is `nil`, finds and returns batch manager
  from a given `_batch_`. Returns `_bm_` otherwise."
  {:added v1
   :see '[batch-manager-from batch-manager]}
  [bm :- (Maybe BatchManager), batch :- AnyBatch]
  (if (nil? bm) (batch-manager-from batch) bm))

;;; Batch item type

(defn item-types-match? :- Boolean
  "Returns `true` if item types match, `false` otherwise."
  {:added v1
   :see '[decide-item-type select-item-type dunaj.coll/item-type]}
  [requested-type :- (U nil Class Type),
   available-type :- (U nil Class Type)]
  (or (nil? available-type)
      (nil? requested-type)
      (identical? requested-type available-type)
      (identical? (provide-class requested-type)
                  (provide-class available-type))))

(defn decide-item-type :- (U nil Class Type)
  "Returns concrete item type, or `nil` if any type will suffice."
  {:added v1
   :see '[item-types-match? select-item-type dunaj.coll/item-type]}
  [requested-type :- (U nil Class Type),
   available-type :- (U nil Class Type)]
  (when-not (item-types-match? requested-type available-type)
    (throw (java.lang.IllegalArgumentException.
            "Item types do not match")))
  (or requested-type available-type))

(defn select-item-type :- (U nil Class Type)
  "Returns concrete item type from a given requested and
  available types. Returns default item type (`byte`)
  if any type would suffice."
  {:added v1
   :see '[item-types-match? decide-item-type dunaj.coll/item-type]}
  ([item-type :- (U nil Class Type)]
   (select-item-type item-type nil))
  ([requested-type :- (U nil Class Type),
    available-type :- (U nil Class Type)]
   (when-not (item-types-match? requested-type available-type)
     (throw (java.lang.IllegalArgumentException.
             "Item types do not match")))
   (or (decide-item-type requested-type available-type)
       default-item-type)))

;;; Batch

(defn batch-support? :- Boolean
  "Returns `true` if host supports batches of `_item-type_`,
  otherwise returns `false`."
  {:added v1
   :see '[batch-manager select-item-type batch-on]}
  [item-type :- (U nil Class Type)]
  (not (nil? (batch-manager item-type))))

(defn batch-on :- AnyBatch
  "Returns batch based on given host array `_arr_` and `_begin_`
  and `_end_` range in that array. Uses `_bm_` if given, or creates
  one from array's item type.
  Throws if host does not support batches on a given array."
  {:added v1
   :see '[batch-support?]}
  ([arr :- AnyArray, begin, :- Int, end :- Int]
   (batch-on (batch-manager (.getComponentType (class arr)))
             arr begin end))
  ([bm :- BatchManager, arr :- AnyArray, begin :- Int, end :- Int]
   (.wrap bm arr begin (isub end begin))))

(deftype BatchWrap [ret :- Any, batch :- (Maybe AnyBatch),
                    other :- (Maybe AnyBatch)])

(defn ^:private batch-advance
  [ret :- Any, batch :- (Maybe AnyBatch), other :- (Maybe AnyBatch)]
  (cond (reduced? ret) (reduced (->BatchWrap @ret nil nil))
        (postponed? ret)
        (postponed
         (->BatchWrap @ret batch other)
         #(batch-advance (advance ret) (clone batch) (clone other))
         #(batch-advance (unsafe-advance! ret) batch other))
        :else (->BatchWrap ret batch other)))

(deftype BatchReducing
  [r :- IReducing, bm :- BatchManager, batch-size :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [w (strip-reduced wrap)
          ret (.-ret ^dunaj.host.batch.BatchWrap w)
          batch :- (Maybe AnyBatch)
          (.-batch ^dunaj.host.batch.BatchWrap w)]
      (when batch (.flip batch))
      (-> (if (and batch (.hasRemaining batch))
            (._step r ret batch)
            ret)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (->BatchWrap (.-wrap r ret)
                 (.allocate bm batch-size)
                 (.allocate bm batch-size)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.host.batch.BatchWrap wrap)))
  (-step [this wrap val]
    (let [batch :- AnyBatch (.-batch ^dunaj.host.batch.BatchWrap wrap)
          other :- AnyBatch (.-other ^dunaj.host.batch.BatchWrap wrap)
          ret (.-ret ^dunaj.host.batch.BatchWrap wrap)]
      (if (.hasRemaining batch)
        (do (.put bm batch val)
            (if (.hasRemaining batch)
              wrap
              (let [nret (._step r ret (.flip batch))]
                (.clear other)
                (batch-advance nret other batch))))
        (let [nret (._step r ret (.flip batch))]
          (.clear other)
          (.put bm other val)
          (batch-advance nret other batch)))))
  (-combine [this wrap other]
    (let [lret (._finish this wrap)
          rret (._finish this other)]
      (->BatchWrap (._combine r lret rret) nil nil))))

(def default-batch-size :- clojure.lang.Var
  "A dynamic var holding default batch size."
  {:added v1
   :see '[provide-batch-size dunaj.coll.util/reduce-batched batch]}
  (var *default-batch-size*))

;; TODO: upper limit causes problems e.g. in color stripper
#_(def max-batch-size :- clojure.lang.Var
  "A dynamic var holding maximum batch size."
  {:added v1
   :see '[default-batch-size provide-batch-size batch]}
  (var *default-batch-size*))

(defn provide-batch-size :- Int
  "Returns batch size based on given optional `_size-hint_` integer.
  Returned batch size is at least big as `_size-hint_`.

  WARNING: This promise may be dropped in later versions."
  {:added v1
   :see '[default-batch-size dunaj.coll.util/reduce-batched batch]}
  [size-hint :- (Maybe Integer)]
  (imax (iint *default-batch-size*) (iint (or size-hint (i1))))
  ;; TODO: upper limit can be introduced after users will not
  ;;       assume returned batch size will be at least big as
  ;;       size-hint
  ;; TODO: upper limit causes problems e.g. in color stripper
  #_(imin (imax (iint *default-batch-size*)
                (iint (or size-hint (i1))))
        (iint *max-batch-size*)))

(defxform batch
  "Returns a transducer that batches given step items into batches of
  `_requested-type_`, which may be a type, class or a keyword,
  and of `_size-hint_` size.
  Returns collection recipe if `_coll_` is given."
  {:added v1
   :see '[dunaj.coll.util/reduce-batched dunaj.coll.util/batched]}
  [requested-type :- (U nil Class Type clojure.lang.Keyword),
   size-hint :- (Maybe Integer)]
  :let
  [type (select-item-type (provide-class requested-type))
   batch-size (provide-batch-size size-hint)]
  :xform
  ([r] (->BatchReducing r (batch-manager type) batch-size))
  :count
  ([tc-fn]
   #(tc-fn
     (let [x (iint %)]
       (if (ipos? x) (iinc (idiv (idec x) batch-size)) (i0)))))
  :section
  ([ts-fn]
   (when tc-fn
     #(let [b (imul (iint %3) batch-size)
            e (min (count %2) (imul (iint %4) batch-size))]
        (ts-fn %1 %2 b e))))
  :unpack false)

(declare malleable)

(deftype Malleable
  [coll]
  IRed
  (-reduce [this reducef init]
    (reduce* coll reducef init))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [t (select-item-type requested-type (item-type coll))
          bm (batch-manager t)
          batch-size (provide-batch-size size-hint)
          resized (.allocate bm batch-size)
          af (fn af [ret, batch :- AnyBatch, resized :- AnyBatch,
                     to-clear?]
               (cond
                 (reduced? ret) ret
                 (postponed? ret)
                 (postponed ret
                            #(af (advance ret) (clone batch)
                                 (clone resized) to-clear?)
                            #(af (unsafe-advance! ret) batch
                                 resized to-clear?))
                 to-clear? (recur ret batch (.clear resized) false)
                 (.hasRemaining batch)
                 (cond (i< (.remaining batch) (.remaining resized))
                       (do (.copy bm batch resized) ret)
                       (ipos? (.position resized))
                       (recur (reducef ret (.flip resized))
                              batch resized true)
                       :else (reducef ret batch))
                 :else ret))
          ret (reduce-batched* requested-type size-hint coll
                               #(af % %2 resized false) init)
          faf (cloned-advance-fn [ret, resized :- AnyBatch]
                (.hasRemaining resized) (reducef ret resized)
                :else ret)]
      (faf ret (.flip resized))))
  IHomogeneous
  (-item-type [this] (item-type coll))
  ICounted
  (-count [this] (count coll))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (reduce-unpacked* coll reducef init))
  ISectionable
  (-section [this begin end]
    (malleable (-section coll begin end))))

(defn malleable :- IBatchedRed
  "Returns batchable collection which takes `_size-hint_` more
  seriously when reduced.

  Useful for cases where the cost of processing many batches is
  greater than the cost of merging batched to bigger ones.
  (one such case is writing to file)."
  {:added v1
   :see '[batch dunaj.coll.util/reduce-batched
          dunaj.coll.util/batched]}
  [coll :- []]
  (adapt (->Malleable coll) coll))

;; inject d.c.helper as it is using batch
(replace-var! dunaj.coll.helper/batch batch)

(extend-protocol! ICloneable
  java.nio.ByteBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.ByteBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos)))
  java.nio.CharBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.CharBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos)))
  java.nio.IntBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.IntBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos)))
  java.nio.LongBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.LongBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos)))
  java.nio.FloatBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.FloatBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos)))
  java.nio.DoubleBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.DoubleBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos)))
  java.nio.ShortBuffer
  (-clone [this]
    (let [cap (.capacity this)
          fb (.asReadOnlyBuffer this)
          nb (java.nio.ShortBuffer/allocate cap)
          pos (.position this)
          mark (get-mark this)]
      (.rewind fb) (.put nb fb) (.flip nb)
      (when (inneg? mark) (.position nb mark) (.mark nb))
      (.position nb pos))))

(extend-protocol! IBatchedRed
  java.nio.ByteBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :byte))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this))
  java.nio.CharBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :char))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this))
  java.nio.IntBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :int))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this))
  java.nio.LongBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :long))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this))
  java.nio.FloatBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :float))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this))
  java.nio.DoubleBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :double))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this))
  java.nio.ShortBuffer
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match?
               requested-type (keyword->class :short))
      (throw (java.lang.IllegalArgumentException.)))
    (reducef init this)))

(extend-protocol! IRed
  java.nio.ByteBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this))))
  java.nio.CharBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this))))
  java.nio.IntBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this))))
  java.nio.LongBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this))))
  java.nio.FloatBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this))))
  java.nio.DoubleBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this))))
  java.nio.ShortBuffer
  (-reduce [this reducef init]
    (let [limit (.limit this)
          af (advance-fn [ret i :- Int]
               (i== i limit) ret
               :else (recur (reducef ret (.get this i)) (iinc i)))]
      (af init (.position this)))))
