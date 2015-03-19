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

(ns dunaj.coll.bvt-vector
  "Bitmapped Vector Trie vector, a sequential persistent collection.

  Based on Phil Bagwell's
  http://infoscience.epfl.ch/record/64398/files/idealhashtrees.pdf[Hash Array Mapped Tries],
  this collection type stores its items in a trie
  (pronounced /ˈtriː/), which offers both persistency and efficient
  item lookup. Vectors have an amortized constant time access to
  their items and efficient insertion/removal of items from the rear.

  BVT vectors have support for transients, folding,
  efficient reversion and sectioning. Support for slicing is
  added through RRB-Tree based vectors.
  Besides BVT vector, Dunaj offers following persistent vector
  types:

  * <<dunaj.coll.tuple.api.ad#,tuples>> for efficient representation
    of small vectors

  * <<dunaj.coll.rrbt-vector.api.ad#,RRB-Tree vectors>> for efficient
    catenation or insertion/removal of items in the middle of a
    vector.

  * <<dunaj.coll.primitive-vector.api.ad#,primitive vectors>>
    for efficient storage of host primitive data types.

  Just like all vector types, tuples add/remove items to/from the
  rear of the collection.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  vector literal or functions defined in
  `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer [subvec]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Va U I Maybe]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [set! AnyArray]]
   [dunaj.host.int :refer [Int i< iinc i0 i<< i5 iint i<= i>= i> iadd
                           i32 idiv imul idec iand inneg? inpos? i31]]
   [dunaj.math :refer [nneg? < integer? dec >= > add neg? inc dec]]
   [dunaj.compare :refer [IHash IEquiv IComparable nil? hash =]]
   [dunaj.flow :refer [cond loop recur if let do when if-not]]
   [dunaj.threading :refer [->]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta assoc-meta]]
   [dunaj.poly :refer [deftype defrecord extend-protocol!]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable ILookup IIndexed ISectionable
     IReversible IEditable ISettleable IMutableStacked IStacked
     IMutableAssociative IMutableCollection IPersistentCollection
     IPersistentVector IAssociative
     first next postponed? postponed reduce empty? count -nth
     section counted? seq reduced? conj! settle! edit conj]]
   [dunaj.function :refer [IInvocable fn defn]]
   [dunaj.concurrent.forkjoin :refer [IFoldable]]
   [dunaj.host.array :refer [aget array-manager adapt]]
   [dunaj.coll.helper :refer
    [fold-sectionable prepare-ordered-section reduce* advance-fn]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.vector-section :refer
    [IReversedVectorSectionHelper IVectorSectionHelper
     reversed-vector-section]]))


;;;; Implementation details

(defn ^:private reduce-vector :- Any
  "Reduce section of BVT Vector."
  [vec :- clojure.lang.PersistentVector,
   reducef :- AnyFn init :- Any, begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (if (i<= (.length vec) (i32))
      (let [arr :- AnyArray (.-tail vec)
            af (advance-fn [ret :- Any, i :- Int]
                 (i< i end)
                 (recur (reducef ret (aget arr i)) (iinc i))
                 :else ret)]
        (af init begin))
      (let [end-chi (idiv (idec end) (i32))
            end-i (iand (idec end) (i31))
            af (advance-fn [ret :- Any, arr :- AnyArray,
                            nchi :- Int, i :- Int]
                 (or (i<= i end-i)
                     (and (i< i (i32)) (i<= nchi end-chi)))
                 (recur (reducef ret (aget arr i)) arr nchi (iinc i))
                 (i< end-chi nchi) ret
                 :else
                 (recur ret (.arrayFor vec (i<< nchi (i5)))
                        (iinc nchi) (i0)))]
        (af init (.arrayFor vec begin)
            (iinc (idiv begin (i32))) (iand begin (i31)))))))

(defn ^:private reduce-mutable-vector :- Any
  "Reduce section of a mutable BVT Vector."
  [vec :- clojure.lang.PersistentVector$TransientVector,
   reducef :- AnyFn init :- Any, begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (if (i<= (.count vec) (i32))
      (let [arr :- AnyArray (.-tail vec)
            af (advance-fn [ret :- Any, i :- Int]
                 (i< i end)
                 (recur (reducef ret (aget arr i)) (iinc i))
                 :else ret)]
        (af init begin))
      (let [end-chi (idiv (idec end) (i32))
            end-i (iand (idec end) (i31))
            af (advance-fn [ret :- Any, arr :- AnyArray,
                            nchi :- Int, i :- Int]
                 (or (i<= i end-i)
                     (and (i< i (i32)) (i<= nchi end-chi)))
                 (recur (reducef ret (aget arr i)) arr nchi (iinc i))
                 (i< end-chi nchi) ret
                 :else
                 (recur ret (.arrayFor vec (i<< nchi (i5)))
                        (iinc nchi) (i0)))]
        (af init (.arrayFor vec begin)
            (iinc (idiv begin (i32))) (iand begin (i31)))))))

(defn ^:private reversed-reduce-vector :- Any
  "Reduce section of a reversed BVT Vector."
  [vec :- clojure.lang.PersistentVector,
   reducef :- AnyFn init :- Any, begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (if (i<= (.length vec) (i32))
      (let [arr :- AnyArray (.-tail vec)
            af (advance-fn [ret :- Any, i :- Int]
                 (i>= i begin)
                 (recur (reducef ret (aget arr i)) (idec i))
                 :else ret)]
        (af init (idec end)))
      (let [begin-chi (idiv begin (i32))
            begin-i (iand begin (i31))
            nend (idec end)
            af (advance-fn [ret :- Any, arr :- AnyArray,
                              pchi :- Int, i :- Int]
                 (or (i>= i begin-i)
                     (and (inneg? i) (i>= pchi begin-chi)))
                 (recur (reducef ret (aget arr i)) arr pchi (idec i))
                 (i> begin-chi pchi) ret
                 :else
                 (recur ret (.arrayFor vec (i<< pchi (i5)))
                        (idec pchi) (i31)))]
        (af init (.arrayFor vec nend)
            (idec (idiv nend (i32))) (iand nend (i31)))))))

(def ^:private oam (array-manager java.lang.Object))

(extend-protocol! IRed
  clojure.lang.ArraySeq
  (-reduce [this reducef init]
    (if (inpos? (.count this))
      init
      (let [coll (adapt oam (.-array this) (.index this)
                        (iadd (.index this) (.count this)))]
        (reduce* coll reducef init))))
  clojure.lang.ArrayChunk
  (-reduce [this reducef init]
    (let [coll (adapt oam (.-array this) (.-off this) (.-end this))]
      (reduce* coll reducef init)))
  clojure.lang.PersistentVector$ChunkedSeq
  (-reduce [this reducef init]
    (let [af (advance-fn [ret n]
               n
               (let [n :- clojure.lang.PersistentVector$ChunkedSeq n]
                 (recur (reduce* (.chunkedFirst n) reducef ret)
                        (.chunkedNext n)))
               :else ret)]
      (af (reduce* (.chunkedFirst this) reducef init)
          (.chunkedNext this)))))


;;;; Public API

(deftype BvtVector
  "A type for Bitmapped Vector Trie Vector."
  clojure.lang.PersistentVector
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  ;; j.u.RandomAccess
  ;; j.l.Runnable, j.u.c.Callable
  IComparable
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (reduce-vector this reducef init (i0) (.count this)))
  ISeqable
  ICounted
  IEmptyable
  IPeekable
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.count this))))
  (-get [this key not-found]
    (if (integer? key) (.nth this key not-found) not-found))
  IIndexed
  IReversible
  (-reverse [this] (reversed-vector-section this (i0) (.count this)))
  ISectionable
  (-section [this nb ne]
    (let [l (.count this)
          ne (prepare-ordered-section nb ne l)]
      (subvec this nb ne)))
  IEditable
  ISequential
  IPersistentCollection
  IAssociative
  IStacked
  IInvocable
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ;; NOTE: ISliceable support added in rrb-vector
  ;; NOTE: ICatenable support added in rrb-vector

  ;; helpers for vector section
  ;; TODO: Seqs are suboptimal but it is hard to do it right if
  ;;       we want to utilize clojure.lang classes.
  IVectorSectionHelper
  (-reduce-vector [this reducef init begin end]
    (reduce-vector this reducef init begin end))
  (-seq [this begin end]
    (let [v (subvec this begin end)]
      (assoc-meta (clojure.lang.APersistentVector$Seq. v (i0))
        (meta this))))
  IReversedVectorSectionHelper
  (-reversed-reduce-vector [this reducef init begin end]
    (reversed-reduce-vector this reducef init begin end))
  (-reversed-seq [this begin end]
    (let [v (reversed-vector-section this begin end)]
      (assoc-meta (clojure.lang.APersistentVector$Seq. v (i0))
        (meta this))))

  ;; Abstract types
  IPersistentVector)

(deftype MutableBvtVector
  "A type for Bitmapped Vector Trie mutable Vector."
  clojure.lang.PersistentVector$TransientVector
  IRed
  (-reduce [this reducef init]
    (reduce-mutable-vector this reducef init (i0) (.count this)))
  ICounted
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.count this))))
  (-get [this key not-found]
    (if (integer? key) (.nth this key not-found) not-found))
  IIndexed
  IPeekable
  (-peek [this] (.nth this (idec (.count this)) nil))
  ;; following protocols are already implemented
  ISettleable
  IMutableCollection
  IMutableAssociative
  IMutableStacked
  IInvocable)

(def empty-bvt-vector :- BvtVector
  "An empty BVT vector."
  {:added v1
   :see '[bvt-vector-factory
          dunaj.coll.primitive-vector/empty-primitive-vector-of
          dunaj.coll.tuple/empty-tuple
          dunaj.coll.default/empty-vec dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/edit]}
  clojure.lang.PersistentVector/EMPTY)

;;; Factory

(defrecord BvtVectorFactory
  "A factory record for BVT vectors."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (settle! (reduce conj! (edit empty-bvt-vector) coll)))
  (-from-items [factory] empty-bvt-vector)
  (-from-items [factory a] (conj empty-bvt-vector a))
  (-from-items [factory a b] (conj empty-bvt-vector a b))
  (-from-items [factory a b c] (conj empty-bvt-vector a b c))
  (-from-items [factory a b c d] (conj empty-bvt-vector a b c d))
  (-from-items [factory a b c d more]
    (let [t (edit empty-bvt-vector)
          t (-> t (conj! a) (conj! b) (conj! c) (conj! d))]
         (settle! (reduce conj! t more)))))

(def bvt-vector-factory :- ICollectionFactory
  "A BVT vector factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of BVT vector can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[dunaj.coll.primitive-vector/primitive-vector-factory-of
          dunaj.coll.tuple/tuple-factory
          dunaj.coll.default/vec-factory
          dunaj.coll.default/->vec
          dunaj.coll.default/vec
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->BvtVectorFactory))
