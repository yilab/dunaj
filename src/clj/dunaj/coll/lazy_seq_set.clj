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

(ns dunaj.coll.lazy-seq-set
  "A lazy set type which is backed by a lazy seq containing
  set items.

  IMPORTANT: Order of items may change between two uses if the
  collection is not realized.

  Most operations fully realize returned lazy set. Following ones
  do not realize, or only partially realize the returned lazy set:

  * Does not realize:
    `<<dunaj.state.api.ad#realized_QMARK_,realized?>>`,
    `<<dunaj.coll.api.ad#seq,seq>>`,
    `<<dunaj.feature.api.ad#meta,meta>>` and
    `<<dunaj.feature.api.ad#assoc_meta,assoc-meta>>`

  * Partial realization: `<<dunaj.compare.api.ad#{under}EQ_,&#61;>>`,
    `<<dunaj.coll.api.ad#contains_QMARK_,contains?>>` and
    `<<dunaj.coll.api.ad#get,get>>`

  * Conversion to persistent set: `<<dunaj.coll.api.ad#conj,conj>>`
    and `<<dunaj.coll.api.ad#disj,disj>>`"
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
     section counted? seq empty single? peek ISorted
     IFlippable second nnext conj! settle! edit reduced
     IEditable ISettleable assoc! IMutableStacked IMutableSet
     IMutableAssociative IMutableCollection IPersistentCollection
     IStacked IPersistentVector conj IAssociative assoc -dissoc
     IPersistentSet count -reduce reduced? postponed -contains? -disj
     postponed? advance unsafe-advance!]]
   [dunaj.function :refer [IInvocable fn defn apply constantly nop]]
   [dunaj.coll.helper :refer
    [coll->iterator strip-reduced reduce*]]
   [dunaj.error :refer [ex-info throw]]
   [dunaj.state.basic :refer [atom]]
   [dunaj.coll.tuple :refer [tuple pair key val]]
   [dunaj.coll.hamt-set]
   [dunaj.coll.lazy-seq]
   [dunaj.coll.cons-seq]))


;;;; Implementation details

(defn ^:private realize-set! :- nil
  [this :- IRed]
  (reduce* this nop nil)
  nil)

(defn ^:private get-coll :- #{}
  "Returns set from the given `data-ref` reference."
  [data-ref :- IReference]
  (key @data-ref))

(deftype LazySeqSet
  "A type for lazy set backed by a lazy seq."
  [data-ref :- IReference]
  ;; TODO: add persistent vector to the data-ref if the
  ;;       order of items must be preserved between two uses
  IHash
  (-hash [this] (realize-set! this) (hash (get-coll data-ref)))
  IPending
  (-realized? [this] (not (seq (val @data-ref))))
  IEquiv
  (-equiv [this other]
    (if (not (class-instance? java.util.Set other))
      false
      (let [rf #(if (contains? other %2) % (reduced false))]
        (strip-reduced (reduce* this rf true)))))
  IRed
  (-reduce [this reducef init]
    (let [data @data-ref
          s (or (key data) #{})
          af (fn af [ret s kvs]
               (cond (reduced? ret)
                     (do (reset! data-ref (pair s kvs)) ret)
                     (postponed? ret)
                     (do (reset! data-ref (pair s kvs))
                         (postponed
                          @ret
                          #(af (advance ret) s kvs)
                          #(af (unsafe-advance! ret) s kvs)))
                     (seq kvs)
                     (let [k (first kvs)
                           c (iint (count s))
                           ns (conj s k)]
                       (when-not (ione? (isub (iint (count ns)) c))
                         (throw (ex-info "duplicate item detected"
                                         {:key k})))
                       (recur (reducef ret k) ns (next kvs)))
                     :else
                     (do (reset! data-ref (pair s kvs)) ret)))]
      #_(when (seq (val data))
        (clojure.core/println "realizing lazy seq"))
      (af (reduce* s reducef init) s (val data))))
  ISeqable
  (-seq [this] (clojure.bridge/red-to-seq this))
  ICounted
  (-count [this] (realize-set! this) (count (get-coll data-ref)))
  ILookup
  (-contains? [this key]
    (let [data @data-ref
          s (or (dunaj.coll.tuple/key data) #{})
          cf (fn cf [s kvs]
               (if-not (seq kvs)
                 (do (reset! data-ref (pair s kvs)) false)
                 (let [k (first kvs)
                       c (iint (count s))
                       ns (conj s k)
                       nkvs (next kvs)]
                   (when-not (ione? (isub (iint (count ns)) c))
                     (throw (ex-info "duplicate item detected"
                                     {:key k})))
                   (if (= key k)
                     (do (reset! data-ref (pair ns nkvs)) true)
                     (recur ns nkvs)))))]
      (if (contains? s key)
        true
        (do
          #_(when (seq (val data))
            (clojure.core/println "realizing lazy set in contains"))
          (cf s (val data))))))
  (-get [this key not-found]
    (if (contains? this key) key not-found))
  IPersistentCollection
  (-conj [this x] (realize-set! this) (conj (get-coll this) x))
  IMeta
  (-meta [this] (.meta ^clojure.lang.IMeta (get-coll data-ref)))
  IPersistentMeta
  (-assoc-meta [this m]
    (let [data @data-ref
          set (key data)
          kvs (val data)]
      (->LazySeqSet (atom (pair (assoc-meta set m) kvs)))))
  IInvocable
  (-invoke [this key] (get this key))
  (-invoke [this key not-found] (get this key not-found))
  IPersistentSet
  (-disj [this key]
    (realize-set! this)
    (-disj (get-coll data-ref) key))

  ;; Clojure compatibility
  clojure.lang.ILookup
  (valAt [this key] (get this key))
  (valAt [this key not-found] (get this key not-found))
  clojure.lang.Associative
  (containsKey [this key] (contains? this key))

  ;; JVM integration
  java.lang.Object
  (hashCode [this]
    (realize-set! this)
    (.hashCode ^java.lang.Object (get-coll data-ref)))
  (equals [this other]
    (if (not (class-instance? java.util.Set other))
      false
      (let [rf #(if (and (contains? other %2)
                         (.equals ^java.lang.Object
                                  (get other %2) %3))
                  %
                  (reduced false))]
        (strip-reduced (reduce* this rf true)))))
  java.util.Set
  java.util.Collection
  (contains [this value] (contains? this value))
  (containsAll [this c]
    (clojure.core/every? #(-contains? this %) (seq c)))
  (isEmpty [this] false)
  (size [this] (count this))
  (toArray [this]
    (realize-set! data-ref)
    (.toArray  ^java.util.Set (get-coll data-ref)))
  (toArray [this a]
    (realize-set! data-ref)
    (.toArray  ^java.util.Set (get-coll data-ref) a))
  java.lang.Iterable
  (iterator [this] (coll->iterator this)))


;;;; Public API

(defn lazy-seq->set :- #{}
  "Returns a lazy set backed by a given lazy seq `_items_`."
  {:added v1
   :see '[dunaj.coll.lazy-seq-map/lazy-seq->map
          dunaj.coll.lazy-seq/lazy-seq
          dunaj.coll.lazy-seq/lazy-cat]}
  ([items :- []]
   (if-let [ss (seq items)]
     (->LazySeqSet (atom (pair #{} ss))) #{})))
