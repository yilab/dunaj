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

(ns dunaj.buffer
  "Unsynchronized mutable collections with predefined capacity.
  
  Buffers are very fast FIFO mutable collections which have their
  fixed capacity set at the creation time. They support following
  functionalities:
  
  * `<<dunaj.coll.api.ad#peek,peek>>` for getting the first inserted
    value.
  * `<<dunaj.coll.api.ad#conj_BANG_,conj!>>` for inserting new
    values and `<<dunaj.coll.api.ad#pop_BANG_,pop!>>` for removal
    of first inserted value.
  * `<<dunaj.coll.api.ad#empty_QMARK_,empty?>>`,
    `<<dunaj.coll.api.ad#full_QMARK_,full?>>`,
    `<<dunaj.coll.api.ad#brimming_QMARK_,brimming?>>`,
    `<<dunaj.coll.api.ad#capacity,capacity>>` and
    `<<dunaj.coll.api.ad#count,count>>` for querying the
    capacity and the number of items.
  * `<<dunaj.state.api.ad#clone,clone>>` for safe cloning.
  * `<<dunaj.coll.api.ad#settle_BANG_,settle!>>` for settling down
    the collection (converting to the immutable variant).

  NOTE: Buffers follow usual mutable semantics, i.e. you should
  always use a value returned from a mutable function instead of
  using the old passed-in value.

  Individual buffer types differ in the way they behave when there
  is no more space for a new item.

  CAUTION: There are no synchronization guarantees for buffer types,
  use them with care!"
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn U Maybe]]
   [dunaj.boolean :refer [Boolean not or and]]
   [dunaj.host :refer
    [ArrayManager AnyArray Class set! provide-class]]
   [dunaj.host.int :refer [Int iint iadd isub i>= ipos? i> i== i0
                           izero? iinc idec inpos? i< i2 imul]]
   [dunaj.math :refer [<]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [ICloneable clone]]
   [dunaj.flow :refer [when if let cond do when-not recur]]
   [dunaj.poly :refer [deftype Type defprotocol]]
   [dunaj.coll :refer
    [ICapped IPeekable IEmptyAware ICounted IIndexed
     IFullAware IRed IHomogeneous IBatchedRed IUnpackedRed
     ISectionable IMutableCollection IMutableStacked ISettleable
     count empty? full? pop! item-type -count reduced? postponed?
     postponed advance -capacity -conj! -pop! -peek -settle!
     -full?]]
   [dunaj.function :refer [defn fn]]
   [dunaj.concurrent.forkjoin :refer [IFoldable fork join invoke]]
   [dunaj.coll.helper :refer
    [adaptCbuS fold-sectionable prepare-ordered-section advance-fn]]
   [dunaj.host.array :refer [array-manager]]
   [dunaj.host.batch :refer
    [batch-support? batch-on item-types-match?]]
   [dunaj.error :refer
    [throw no-such-element ex-info illegal-argument]]
   [dunaj.string :refer [->str]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.state.var :refer [declare]]))


;;;; Public API

(defprotocol IPortBuffer
  "An abstract type protocols for buffers that can be used as channel
   buffers."
  {:added v1
   :predicate 'port-buffer?}
  (-close! :- nil
    "Closes buffer as a recation to the closing of a port.
     Most implementations don't need to do anything here.
     Is called when port is closed, within port's lock."
    [this]))

(declare immutable-buffer)

(deftype ImmutableBuffer
  [am :- ArrayManager, arr :- AnyArray, begin :- Int, end :- Int]
  IRed
  (-reduce [this reducef init]
    (let [al (.count am arr)
          end (if (i== end al) (i0) end)
          af (advance-fn [ret :- Any, i :- Int]
               (let [l (if (i> i end)
                         (iadd end (isub al i))
                         (isub end i))]
                 (if (izero? l)
                   ret
                   (recur (reducef ret (.get am arr i))
                          (let [ni (iinc i)]
                            (if (i== ni al) (i0) ni))))))]
      (af init begin)))
  ICounted
  (-count [this]
    (if (i> begin end)
      (iadd end (isub (.count am arr) begin))
      (isub end begin)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (when-not (item-types-match? requested-type (item-type this))
      (throw
       (illegal-argument
        (->str "requested type is not supported" requested-type))))
    (if (ipos? (-count this))
      (if (i> begin end)
        (let [af (advance-fn [ret :- Any]
                   (reducef ret (batch-on arr 0 end)))]
          (af (reducef init (batch-on arr begin (.count am arr)))))
        (reducef init (batch-on arr begin end)))
      init))
  IHomogeneous
  (-item-type [this] (.itemType am))
  IIndexed ;; Undocumented support for IIndexed
  (-nth [this index not-found]
    (if (< -1 index (count this))
      (let [al (.count am arr)
            i (iadd (iint index) begin)
            i (if (i< i al) i (isub i al))]
        (.get am arr i))
      not-found))
  ISectionable
  (-section [this nb ne]
    (let [al (.count am arr)
          ne (prepare-ordered-section nb ne (count this))
          nb (iadd begin nb)
          nb (if (i>= nb al) (isub nb al) nb)
          ne (iadd begin ne)
          ne (if (i>= ne al) (isub ne al) ne)]
      (immutable-buffer am arr nb ne)))
  IFoldable
  (-fold [coll reduce-fn pool n combinef reducef]
    (fold-sectionable coll reduce-fn pool n combinef reducef)))

(defn ^:private immutable-buffer
  [am :- ArrayManager, arr :- AnyArray, begin :- Int, end :- Int]
  (let [end (if (and (izero? end) (not (izero? begin)))
              (.count am arr)
              end)
        coll (->ImmutableBuffer am arr begin end)]
    ;; Undocumented support for IIndexed for primitive buffers
    (if (batch-support? (.itemType am)) coll (adaptCbuS coll))))

(deftype FixedBuffer
  [am :- ArrayManager, arr :- AnyArray,
   ^:unsynchronized-mutable last :- Int,
   ^:unsynchronized-mutable pos :- Int,
   ^:unsynchronized-mutable ff? :- Boolean, cam :- Int]
  IPortBuffer
  (-close! [this] nil)
  ISettleable
  (-settle! [this] (immutable-buffer am arr last pos))
  IEmptyAware
  (-empty? [this] (i== last pos))
  IFullAware
  (-full? [this] ff?)
  ICapped
  (-capacity [this] (idec cam))
  ICounted
  (-count [this]
    (if (i> last pos) (iadd pos (isub cam last)) (isub pos last)))
  IPeekable
  (-peek [this] (when-not (i== last pos) (.get am arr last)))
  ICloneable
  (-clone [this]
    (let [narr (.allocate am cam)
          _ (java.lang.System/arraycopy arr (i0) narr (i0) cam)]
      (->FixedBuffer am narr last pos ff? cam)))
  IMutableStacked
  (-pop! [this]
    (set! ff? false)
    (.set am arr last (i0)) ;; clear old val, or else we leak
    (when (i== last pos)
      (throw (no-such-element "Cannot pop empty buffer.")))
    (let [nlast (iinc last)]
      (set! last (if (i== nlast cam) (i0) nlast)))
    this)
  IMutableCollection
  (-conj! [this val]
    (when ff? (throw (ex-info "Cannot add to the full buffer.")))
    (.set am arr pos val)
    (let [npos (iinc pos)]
      (set! pos (if (i== npos cam) (i0) npos)))
    (when (i== (-count this) (idec cam)) (set! ff? true))
    this))

(deftype StretchingBuffer
  [am :- ArrayManager, arr :- AnyArray,
   ^:unsynchronized-mutable last :- Int,
   ^:unsynchronized-mutable pos :- Int, capacity :- Int
   ^:unsynchronized-mutable ff? :- Boolean, cam :- Int]
  IPortBuffer
  (-close! [this] nil)
  ISettleable
  (-settle! [this] (immutable-buffer am arr last pos))
  IEmptyAware
  (-empty? [this] (i== last pos))
  IFullAware
  (-full? [this] ff?)
  ICapped
  (-capacity [this] (idec capacity))
  ICounted
  (-count [this]
    (if (i> last pos) (iadd pos (isub cam last)) (isub pos last)))
  IPeekable
  (-peek [this] (when-not (i== last pos) (.get am arr last)))
  ICloneable
  (-clone [this]
    (let [narr (.allocate am cam)
          _ (java.lang.System/arraycopy arr (i0) narr (i0) cam)]
      (->StretchingBuffer am narr last pos capacity ff? cam)))
  IMutableStacked
  (-pop! [this]
    (when (or (i== capacity cam) (i== (-count this) (idec capacity)))
      (set! ff? false))
    (.set am arr last (i0)) ;; clear old val, or else we leak
    (when (i== last pos)
      (throw (no-such-element "Cannot pop empty buffer.")))
    (let [nlast (iinc last)]
      (set! last (if (i== nlast cam) (i0) nlast)))
    this)
  IMutableCollection
  (-conj! [this val]
    (let [l (-count this)]
      (if (and ff? (i== l (idec cam)))
        (do
          (let [ncam (imul cam (i2))
                narr (.allocate am ncam)
                special? (i> last pos)
                _ (if special?
                    (java.lang.System/arraycopy
                     arr last narr (i0) (isub cam last))
                    (java.lang.System/arraycopy arr last narr (i0) l))
                _ (when special?
                    (java.lang.System/arraycopy
                     arr 0 narr (isub cam last) pos))
                nb (->StretchingBuffer
                    am narr 0 l capacity true ncam)]
            (dunaj.coll/-conj! nb val)))
        (do
          (.set am arr pos val)
          (let [npos (iinc pos)]
            (set! pos (if (i== npos cam) (i0) npos)))
          (when (i== l (isub capacity (i2))) (set! ff? true))
          this)))))

(deftype DroppingBuffer [fb]
  IPortBuffer (-close! [this] nil)
  ISettleable (-settle! [this] (-settle! fb))
  ICapped (-capacity [this] (-capacity fb))
  ICounted (-count [this] (-count fb))
  IPeekable (-peek [this] (-peek fb))
  ICloneable (-clone [this] (->DroppingBuffer (clone fb)))
  IMutableStacked (-pop! [o] (->DroppingBuffer (-pop! fb)))
  IMutableCollection
  (-conj! [this val]
    (if (full? fb) this (->DroppingBuffer (-conj! fb val)))))

;; special protocol, because implementing IFullAware
;; would signal that coll is blocking
;; NOTE: Candidate for inclusion in public API
(defprotocol ISlidingFull
  (-sliding-full? [this]))

(deftype SlidingBuffer
  [am :- ArrayManager, arr :- AnyArray,
   ^:unsynchronized-mutable last :- Int,
   ^:unsynchronized-mutable pos :- Int,
   ^:unsynchronized-mutable ff? :- Boolean, cam :- Int]
  IPortBuffer
  (-close! [this] nil)
  ISettleable
  (-settle! [this] (immutable-buffer am arr last pos))
  IEmptyAware
  (-empty? [this] (i== last pos))
  ICapped
  (-capacity [this] (idec cam))
  ISlidingFull
  (-sliding-full? [this] ff?)
  ICounted
  (-count [this]
    (if (i> last pos) (iadd pos (isub cam last)) (isub pos last)))
  IPeekable
  (-peek [this] (when-not (i== last pos) (.get am arr last)))
  ICloneable
  (-clone [this]
    (let [narr (.allocate am cam)
          _ (java.lang.System/arraycopy arr (i0) narr (i0) cam)]
      (->SlidingBuffer am narr last pos ff? cam)))
  IMutableStacked
  (-pop! [this]
    (set! ff? false)
    (.set am arr last (i0)) ;; clear old val, or else we leak
    (when (i== last pos)
      (throw (no-such-element "Cannot pop empty buffer.")))
    (let [nlast (iinc last)]
      (set! last (if (i== nlast cam) (i0) nlast)))
    this)
  IMutableCollection
  (-conj! [this val]
    (if ff?
      (let [npos (iinc pos), nlast (iinc last)]
        (.set am arr pos val)
        (set! pos (if (i== npos cam) (i0) npos))
        (set! last (if (i== nlast cam) (i0) nlast)))
      (let [npos (iinc pos)]
        (.set am arr pos val)
        (set! pos (if (i== npos cam) (i0) npos))
        (when (i== (-count this) (idec cam)) (set! ff? true))))
    this))

(deftype PromiseBuffer
  [^:unsynchronized-mutable val :- Any]
  IPortBuffer
  (-close! [this] (set! val nil) nil)
  ISettleable
  (-settle! [this] (if val [val] []))
  IEmptyAware
  (-empty? [this] (nil? val))
  ICounted
  (-count [this] (if (nil? val) 0 1))
  IPeekable
  (-peek [this] val)
  ICloneable
  (-clone [this] (->PromiseBuffer val))
  IMutableStacked
  (-pop! [this]
    (when (nil? val)
      (throw (no-such-element "Cannot pop empty buffer.")))
    this)
  IMutableCollection
  (-conj! [this nval]
    (when (nil? val) (set! val nval))    
    this))

(defn promise-buffer :- IMutableCollection
  "Returns a new promise buffer."
  []
  (->PromiseBuffer))

(defn buffer :- IMutableCollection
  "Returns a fixed buffer of a size `_n_`. Optional `_type_` may be
  provided for buffers holding primitive values. `conj!` to the
  fixed buffer throws if there is no more space for new items."
  {:added v1
   :see '[dropping-buffer sliding-buffer stretching-buffer
          dunaj.concurrent.port/chan]}
  ([n :- Int]
     (buffer nil n))
  ([type :- (U nil Keyword Class Type), n :- Int]
     (when (inpos? n)
       (throw (illegal-argument "n is not positive integer.")))
     (let [am :- ArrayManager
           (array-manager (provide-class (or type :object)))
           arr (.allocate am (iinc n))]
       (->FixedBuffer am arr (i0) (i0) false (.count am arr)))))

(defn stretching-buffer :- IMutableCollection
  "Returns a stretching buffer of a size `_n_`. Optional `_type_` may
  be provided for buffers holding primitive values. While stretching
  buffer presents itself as a buffer of fixed length, `conj!` to the
  stretching buffer is always accepted and internal buffer size is
  enlarged if needed, while presented capacity remains.

  Is useful for channels, where transducer may produce multiple
  values from one input value."
  {:added v1
   :see '[dropping-buffer sliding-buffer buffer
          dunaj.concurrent.port/chan]}
  ([n :- Int]
     (stretching-buffer nil n))
  ([type :- (U nil Keyword Class Type), n :- Int]
     (when (inpos? n)
       (throw (illegal-argument "n is not positive integer.")))
     (let [am :- ArrayManager
           (array-manager (provide-class (or type :object)))
           arr (.allocate am (iinc n))]
       (->StretchingBuffer
        am arr (i0) (i0) (.count am arr) false (.count am arr)))))

(defn dropping-buffer :- IMutableCollection
  "Returns a dropping buffer of a size `_n_`. Optional `_type_` may be
  provided for buffers holding primitive values. `conj!` to the
  dropping buffer silently drops new items if there is no space for
  them (not yet inserted items will be dropped and buffer will remain
  the same)."
  {:added v1
   :see '[buffer sliding-buffer dunaj.concurrent.port/chan
          stretching-buffer]}
  ([n :- Int]
     (dropping-buffer nil n))
  ([type :- (U nil Keyword Class Type), n :- Int]
     (->DroppingBuffer (buffer type n))))

(defn sliding-buffer :- IMutableCollection
  "Returns a sliding buffer of a size `_n_`. Optional `_type_` may be
  provided for buffers holding primitive values. `conj!` to the
  sliding buffer silently drops oldest items if there is no space for
  them (new items will be inserted and oldest will be dropped)."
  {:added v1
   :see '[buffer dropping-buffer dunaj.concurrent.port/chan
          stretching-buffer]}
  ([n :- Int]
     (sliding-buffer nil n))
  ([type :- (U nil Keyword Class Type), n :- Int]
     (when (inpos? n)
       (throw (illegal-argument "n is not positive integer.")))
     (let [am :- ArrayManager
           (array-manager (provide-class (or type :object)))
           arr (.allocate am (iinc n))]
       (->SlidingBuffer am arr (i0) (i0) false (.count am arr)))))
