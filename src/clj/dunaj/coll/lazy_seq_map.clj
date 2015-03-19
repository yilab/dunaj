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

(ns dunaj.coll.lazy-seq-map
  "A lazy map type which is backed by a lazy seq containing
  interleaved keys and values.

  IMPORTANT: Order of items may change between two uses if the
  collection is not realized.

  Most operations fully realize returned lazy map. Following ones
  do not realize, or only partially realize the returned lazy map:

  * Does not realize:
    `<<dunaj.state.api.ad#realized_QMARK_,realized?>>`,
    `<<dunaj.coll.api.ad#seq,seq>>`,
    `<<dunaj.feature.api.ad#meta,meta>>` and
    `<<dunaj.feature.api.ad#assoc_meta,assoc-meta>>`

  * Partial realization: `<<dunaj.compare.api.ad#{under}EQ_,&#61;>>`,
    `<<dunaj.coll.api.ad#contains_QMARK_,contains?>>` and
    `<<dunaj.coll.api.ad#get,get>>`

  * Conversion to persistent map: `<<dunaj.coll.api.ad#conj,conj>>`,
    `<<dunaj.coll.api.ad#assoc,assoc>>` and
    `<<dunaj.coll.api.ad#dissoc,dissoc>>`"
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [class-instance?]]
   [dunaj.host.int :refer [iint isub ione? i==]]
   [dunaj.compare :refer
    [IHash IEquiv nil? hash IComparable identical? =
     hash-from-basis basis-seed next-basis unordered-hash-factory]]
   [dunaj.flow :refer
    [when-let cond loop recur if let do when when-not if-let if-not]]
   [dunaj.state :refer [IPending IReference alter! reset!]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta assoc-meta]]
   [dunaj.poly :refer [deftype]]
   [dunaj.coll :refer
    [first next ISequential contains? IEmptyable IRed ISeq rest
     IEmptyAware IPeekable ICounted ICollectionFactory ISeqable
     ILookup IIndexed ISectionable IReversible reduce empty? get
     section counted? seq empty single? peek IUnpackedRed ISorted
     IFlippable second nnext conj! settle! edit reduced
     IEditable ISettleable assoc! IMutableStacked IMutableMap
     IMutableAssociative IMutableCollection IPersistentCollection
     IStacked IPersistentVector conj IAssociative assoc -dissoc
     IPersistentMap count -reduce-unpacked reduced? postponed
     postponed? advance unsafe-advance!]]
   [dunaj.function :refer [IInvocable fn defn apply constantly nop]]
   [dunaj.coll.helper :refer
    [coll->iterator strip-reduced reduce-unpacked*]]
   [dunaj.error :refer [ex-info throw]]
   [dunaj.state.basic :refer [atom]]
   [dunaj.coll.tuple :refer [tuple pair key val]]
   [dunaj.coll.hamt-map]
   [dunaj.coll.array-map]
   [dunaj.coll.lazy-seq]
   [dunaj.coll.cons-seq]))


;;;; Implementation details

(defn ^:private unpacked-fn
  [f]
  (fn [val a b] (f val (pair a b))))

(defn ^:private realize-map! :- nil
  [this :- IUnpackedRed]
  (reduce-unpacked* this nop nil)
  nil)

(defn ^:private get-coll :- {}
  "Returns map from the given `data-ref` reference."
  [data-ref :- IReference]
  (key @data-ref))

(deftype LazySeqMap
  "A type for lazy map backed by a lazy seq."
  [data-ref :- IReference]
  IHash
  (-hash [this] (realize-map! this) (hash (get-coll data-ref)))
  IPending
  (-realized? [this] (not (seq (val @data-ref))))
  IEquiv
  (-equiv [this other]
    (if (not (class-instance? java.util.Map other))
      false
      (let [rf #(if (and (contains? other %2) (= (get other %2) %3))
                  %
                  (reduced false))]
        (strip-reduced (reduce-unpacked* this rf true)))))
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  ISeqable
  (-seq [this] (clojure.bridge/red-to-seq this))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [data @data-ref
          m (or (key data) {})
          af (fn af [ret m kvs]
               (cond
                (reduced? ret)
                (do (reset! data-ref (pair m kvs)) ret)
                (postponed? ret)
                (do (reset! data-ref (pair m kvs))
                    (postponed @ret
                               #(af (advance ret) m kvs)
                               #(af (unsafe-advance! ret) m kvs)))
                (seq kvs)
                (let [k (first kvs)
                      v (second kvs)
                      c (iint (count m))
                      nm (assoc m k v)]
                  (when-not (ione? (isub (iint (count nm)) c))
                    (throw (ex-info "duplicate item detected"
                                    {:key k})))
                  (recur (reducef ret k v) nm (nnext kvs)))
                :else (do (reset! data-ref (pair m kvs)) ret)))]
      #_(when (seq (val data))
        (clojure.core/println "realizing lazy map"))
      (af (reduce-unpacked* m reducef init) m (val data))))
  ICounted
  (-count [this] (realize-map! this) (count (get-coll data-ref)))
  ILookup
  (-contains? [this key]
    (let [data @data-ref
          m (or (dunaj.coll.tuple/key data) {})
          cf (fn cf [m kvs]
               (if-not (seq kvs)
                 (do (reset! data-ref (pair m kvs)) false)
                 (let [k (first kvs)
                       v (second kvs)
                       c (iint (count m))
                       nm (assoc m k v)
                       nnkvs (nnext kvs)]
                   (when-not (ione? (isub (iint (count nm)) c))
                     (throw (ex-info "duplicate item detected"
                                     {:key k})))
                   (if (= key k)
                     (do (reset! data-ref (pair nm nnkvs)) true)
                     (recur nm nnkvs)))))]
      (if (contains? m key)
        true
        (do
          #_(when (seq (val data))
            (clojure.core/println "realizing lazy map in contains"))
          (cf m (val data))))))
  (-get [this key not-found]
    (let [data @data-ref
          m (or (dunaj.coll.tuple/key data) {})
          cf (fn cf [m kvs]
               (if-not (seq kvs)
                 (do (reset! data-ref (pair m kvs)) not-found)
                 (let [k (first kvs)
                       v (second kvs)
                       c (iint (count m))
                       nm (assoc m k v)
                       nnkvs (nnext kvs)]
                   (when-not (ione? (isub (iint (count nm)) c))
                     (throw (ex-info "duplicate item detected"
                                     {:key k})))
                   (if (= key k)
                     (do (reset! data-ref (pair nm nnkvs)) v)
                     (recur nm nnkvs)))))]
      (if (contains? m key)
        (get m key)
        (do
          #_(when (seq (val data))
            (clojure.core/println "realizing lazy map in get"))
          (cf m (val data))))))
  IPersistentCollection
  (-conj [this x] (realize-map! this) (conj (get-coll this) x))
  IAssociative
  (-assoc [this k v]
    (realize-map! this)
    (assoc (get-coll this) k v))
  (-entry [this key]
    (when (contains? this key) (pair key (get this key))))
  IMeta
  (-meta [this] (.meta ^clojure.lang.IMeta (get-coll data-ref)))
  IPersistentMeta
  (-assoc-meta [this m]
    (let [data @data-ref
          map (key data)
          kvs (val data)]
      (->LazySeqMap (atom (pair (assoc-meta map m) kvs)))))
  IInvocable
  (-invoke [this key] (get this key))
  (-invoke [this key not-found] (get this key not-found))
  IPersistentMap
  (-dissoc [this key]
    (realize-map! this)
    (-dissoc (get-coll data-ref) key))

  ;; Clojure compatibility
  clojure.lang.ILookup
  (valAt [this key] (get this key))
  (valAt [this key not-found] (get this key not-found))
  clojure.lang.Associative
  (containsKey [this key] (contains? this key))

  ;; JVM integration
  java.lang.Object
  (hashCode [this]
    (realize-map! this)
    (.hashCode ^java.lang.Object (get-coll data-ref)))
  (equals [this other]
    (if (not (class-instance? java.util.Map other))
      false
      (let [rf #(if (and (contains? other %2)
                         (.equals ^java.lang.Object
                                  (get other %2) %3))
                  %
                  (reduced false))]
        (strip-reduced (reduce-unpacked* this rf true)))))
  java.util.Map
  (containsValue [this value]
    (realize-map! this)
    (.containsValue ^java.util.Map (get-coll data-ref) value))
  (entrySet [this]
    (realize-map! this)
    (.entrySet  ^java.util.Map (get-coll data-ref)))
  (get [this key] (get this key))
  (isEmpty [this] false)
  (keySet [this]
    (realize-map! this)
    (.keySet  ^java.util.Map (get-coll data-ref)))
  (size [this] (count this))
  (values [this]
    (realize-map! this)
    (.values  ^java.util.Map (get-coll data-ref)))
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  clojure.lang.MapEquivalence)


;;;; Public API

(defn lazy-seq->map :- {}
  "Returns a lazy map backed by a given lazy seq `_kvs_`, which
  contains interleaved keys and values."
  {:added v1
   :see '[dunaj.coll.lazy-seq-set/lazy-seq->set
          dunaj.coll.lazy-seq/lazy-seq
          dunaj.coll.lazy-seq/lazy-cat]}
  ([kvs :- []]
   (if-let [sp (seq kvs)] (->LazySeqMap (atom (pair {} sp))) {})))
