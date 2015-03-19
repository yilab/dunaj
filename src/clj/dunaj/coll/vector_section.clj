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

(ns dunaj.coll.vector-section
  "A helper namespace for pluggable vector and reversed vector
  sections.

  IMPORTANT: This is a helper namespace. It does not contain any
  public vars and there is *no need to require this namespace*
  directly."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core :refer [every? subvec]]
   [dunaj.type :refer [Any AnyFn Maybe]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host.int :refer
    [Int iint iadd i< ineg? izero? i== isub idec i0 imul i31 i1]]
   [dunaj.math :refer
    [nneg? < integer? == <= quot dec >= zero? mod > subtract add neg?
     multiply inc dec]]
   [dunaj.state :refer [IReference]]
   [dunaj.compare :refer
    [IHash IHashBasis IEquiv IComparable nil? hash = hash-from-basis
     basis-seed next-basis ordered-hash-factory]]
   [dunaj.flow :refer
    [when-let cond loop recur if let do when delay when-not]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta assoc-meta]]
   [dunaj.poly :refer [deftype defrecord defprotocol]]
   [dunaj.coll :refer
    [first next ISequential seq IEmptyable IRed ISeq IEmptyAware
     IPeekable ICounted ICollectionFactory ISeqable ILookup IIndexed
     ISectionable IReversible reduce empty? count section counted?
     seq ISliceable IPersistentCollection IStacked nth reverse
     slice empty get -reverse
     IPersistentVector conj IAssociative -empty -contains?]]
   [dunaj.function :refer [IInvocable fn defn]]
   [dunaj.coll.helper :refer
    [equiv-ordered coll->iterator index-of coll->list-iterator
     equals-ordered compare-ordered fold-sectionable
     prepare-ordered-section]]
   [dunaj.concurrent.forkjoin :refer [IFoldable]]
   [dunaj.error :refer [throw index-out-of-bounds]]
   [dunaj.state.var :refer [declare]]))


;;;; Public API

(defprotocol IVectorSectionHelper
  "A helper protocol for vector sections."
  (-reduce-vector :- Any
    "Reduces part of a vector."
    [this reducef :- AnyFn, init :- Any, begin :- Int, end :- Int])
  (-seq :- ISeq
    "Returns seq from `this`."
    [this begin :- Int, end :- Int]))

(defprotocol IReversedVectorSectionHelper
  "A helper protocol for reversed vector sections."
  (-reversed-reduce-vector :- Any
    "Reduces part of a vector in reverse order."
    [this reducef :- AnyFn, init :- Any, begin :- Int, end :- Int])
  (-reversed-seq :- ISeq
    "Returns seq from `this` in reverse order."
    [this begin :- Int, end :- Int]))

(declare reversed-vector-section)

(deftype ReversedVectorSection
  "Reversed vector section. Does not support conjoining."
  [vec :- IPersistentVector, begin :- Int, end :- Int,
   hash-basis :- IReference, hash-code :- IReference]
  IComparable
  (-compare-to [this other] (compare-ordered this other))
  IHash
  (-hash [this] (hash-from-basis @hash-basis (isub end begin)))
  IHashBasis
  (-hash-basis [this] @hash-basis)
  IEquiv
  (-equiv [this other] (equiv-ordered this other))
  IMeta
  (-meta [this] (meta vec))
  IPersistentMeta
  (-assoc-meta [this m]
    (->ReversedVectorSection
     (assoc-meta vec m) begin end hash hash-code))
  IRed
  (-reduce [this reducef init]
    (-reversed-reduce-vector vec reducef init begin end))
  ISequential
  ICounted
  (-count [this] (isub end begin))
  IEmptyable
  (-empty [this] (-empty vec))
  IPeekable
  (-peek [this] (nth vec begin nil))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (count this))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (or (neg? index) (>= index (isub end begin)))
        not-found
        (nth vec (isub (idec end) index)))))
  IReversible
  (-reverse [this] (section vec begin end))
  ISliceable
  (-slice [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (prepare-ordered-section nb ne l)]
      (let [c (slice vec (isub end ne) (isub end nb))]
        (reversed-vector-section c (i0) (isub ne nb)))))
  ISectionable
  (-section [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (prepare-ordered-section nb ne l)]
      (if (and (izero? nb) (i== ne l))
        this
        (reversed-vector-section vec (isub end ne) (isub end nb)))))
  ISeqable
  (-seq [this] (assoc-meta (-reversed-seq vec begin end) (meta vec)))
  IInvocable
  (-invoke [this arg] (nth this arg))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  IPersistentVector ;; only because of c.l.APersistentVector$Seq

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this index] (nth this index))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] @hash-code)
  (equals [this other] (equals-ordered this other))
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  java.util.Collection
  (contains [this x] (-contains? this x))
  (containsAll [this c] (every? #(-contains? this %) (seq c)))
  (isEmpty [this] (empty? this))
  (size [this] (count this))
  (toArray [this] (clojure.lang.RT/seqToArray (seq this)))
  (toArray [this a] (clojure.lang.RT/seqToPassedArray (seq this) a))
  java.util.List
  (get [this index] (nth this index))
  (indexOf [this x] (index-of this x))
  (lastIndexOf [this x] (isub (idec (isub end begin))
                              (index-of (reverse this) x)))
  (listIterator [this] (.listIterator this 0))
  (listIterator [this i] (coll->list-iterator this i))
  (subList [this nb ne] (section this nb ne))
  java.util.RandomAccess)

(defn reversed-vector-section :- ReversedVectorSection
  "Returns reversed vector section. `vec` must directly implement
  IPersistentVector and must implement IReversedVectorSectionHelper."
  [vec :- IPersistentVector, nb :- Int, ne :- Int]
  (->ReversedVectorSection
   vec nb ne
   (delay (-reversed-reduce-vector
            vec
            #(next-basis ordered-hash-factory % %2)
            (basis-seed ordered-hash-factory) nb ne))
   (delay (-reversed-reduce-vector
           vec
           #(iadd (imul (i31) %)
                  (if (nil? %2) (i0)
                      (.hashCode ^java.lang.Object %2)))
           (i1) nb ne))))

(deftype VectorSection
  "A type for persistent vector sections."
  clojure.lang.APersistentVector$SubVector
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  ;; j.u.RandomAccess
  ;; j.l.Runnable, j.u.c.Callable
  ;; NOTE: does not use IVectorSectionHelper/-seq
  IComparable
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (-reduce-vector
     (.-v this) reducef init (.-start this) (.-end this)))
  ISeqable
  ICounted
  IEmptyable
  IPeekable
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.length this))))
  (-get [this key not-found]
    (if (integer? key) (.nth this key not-found) not-found))
  IIndexed
  IReversible
  (-reverse [this]
    (reversed-vector-section (.-v this) (.-start this) (.-end this)))
  ISliceable
  (-slice [this nb ne]
    (let [l (.length this)
          nb (iint nb)
          ne (iint (prepare-ordered-section nb ne l))]
      (if (i== nb ne)
        (empty this)
        (slice (.-v this)
               (iadd nb (.-start this))
               (iadd ne (.-start this))))))
  ISectionable
  (-section [this nb ne]
    (let [l (.length this)
          ne (prepare-ordered-section nb ne l)]
      (subvec this nb ne)))
  ISequential
  IPersistentCollection
  IAssociative
  IStacked
  IInvocable
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ;; Abstract types
  IPersistentVector)

(defn vector-section :- VectorSection
  "Returns vector section. `vec` must directly implement
  IPersistentVector and must implement IVectorSectionHelper."
  [vec :- IPersistentVector, nb :- Int, ne :- (Maybe Int)]
  (let [l (.length vec)
        ne (iint (prepare-ordered-section nb ne l))]
    (if (and (izero? nb) (i== ne l))
      vec
      (clojure.lang.APersistentVector$SubVector.
       (meta vec) vec nb ne))))
