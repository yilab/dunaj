;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; Additional copyright for parts of documentation and/or
;; underlying implementation:
;; Copyright (C) 2012, 2015, Michał Marczyk, Rich Hickey
;; and Clojure contributors
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

(ns dunaj.coll.rrbt-vector
  "Relaxed Radix Balanced Trees (RRB-Trees) Vector.

  Based on Phil Bagwell's http://infoscience.epfl.ch/record/169879/files/RMTrees.pdf[RRB-Trees], this collection type
  stores items in a relaxed radix balanced trees, which enables
  performant splitting and catenation. These features can be used
  to insert/remove items in the middle of a vector.

  RRB-Tree vectors have support for transients, folding,
  efficient reversion, sectioning, slicing and catenation.
  Besides RRB-Tree vector, Dunaj offers following persistent vector
  types:

  * <<dunaj.coll.bvt-vector.api.ad#,BVT vectors>> for efficient
    representation of medium to large vectors
  * <<dunaj.coll.tuple.api.ad#,tuples>> for efficient representation
    of small vectors
  * <<dunaj.coll.primitive-vector.api.ad#,primitive vectors>>
    for efficient storage of host primitive data types.

  IMPORTANT: This namespace does not provide any public vars,
  but instead extends BVT vector to automatically convert to
  RRB-Tree based vector on slicing. There is no need to require this
  namespace directly."
  {:authors ["Jozef Wagner"]
   :additional-copyright
   "2012, 2015, Michał Marczyk, Rich Hickey and Clojure contributors"}
  (:api bare)
  (:require
   [clojure.core.rrb-vector]
   [clojure.core.rrb-vector.rrbt :refer [as-rrbt]]
   [clojure.core.rrb-vector.nodes :refer [ranges]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any AnyFn Fn Va U I Maybe]]
   [dunaj.boolean :refer [and or not]]
   [dunaj.host :refer [AnyArray]]
   [dunaj.host.int :refer
    [Int i< iinc i0 i<< i5 iint i<= i>= i> i31 i32 idiv imul idec
     iand inneg? ineg? isub iloop izero? i>> imin iadd imax]]
   [dunaj.math :refer
    [nneg? < integer? dec >= > add neg? inc dec zero? ==]]
   [dunaj.compare :refer [IHash IEquiv IComparable nil? hash =]]
   [dunaj.flow :refer [cond loop recur if let do when if-not]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta assoc-meta]]
   [dunaj.poly :refer [deftype extend-type!]]
   [dunaj.coll :refer
    [ISequential IEmptyable IRed IEmptyAware IPeekable ICounted
     ICollectionFactory ISeqable ILookup IIndexed ISectionable
     IReversible IEditable ISettleable IMutableStacked IStacked
     IMutableAssociative IMutableCollection IPersistentCollection
     IPersistentVector IAssociative ICatenable ISliceable
     first next postponed? postponed advance reduce empty? count -nth
     section counted? seq reduced? conj! settle! edit conj]]
   [dunaj.function :refer [IInvocable fn defn]]
   [dunaj.concurrent.forkjoin :refer [IFoldable]]
   [dunaj.host.array :refer [aget]]
   [dunaj.coll.helper :refer
    [fold-sectionable prepare-ordered-section advance-fn]]
   [dunaj.error :refer [throw index-out-of-bounds]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.vector-section :refer
    [IReversedVectorSectionHelper IVectorSectionHelper
     reversed-vector-section vector-section]]
   [dunaj.coll.bvt-vector]))


;;;; Implementation details

(defn offset :- Int
  "Returns offset into array in rrbt `vec` for a given `i`.
  Use arrayFor to return the array itself."
  [vec :- clojure.core.rrb_vector.rrbt.Vector, i :- Int]
  (if (or (ineg? i) (i>= i (.-cnt vec)))
    (throw (index-out-of-bounds))
    (let [am :- clojure.core.ArrayManager (.-am vec)
          nm :- clojure.core.rrb_vector.nodes.NodeManager (.-nm vec)
          shift (.-shift vec)
          tail-off (isub (.-cnt vec) (.alength am (.-tail vec)))]
      (if (i<= tail-off i)
        (isub i tail-off)
        (iloop [i i, node (.-root vec), shift shift]
          (if (or (izero? shift) (.regular nm node))
            (iand i (i31))
            (let [arr :- AnyArray (.array nm node)
                  rngs (ranges nm node)
                  idx (iloop [j (iand (i>> i shift) (i31))]
                        (if (i< i (aget rngs j)) j (recur (iinc j))))
                  i (if (izero? idx)
                      i
                      (isub i (aget rngs (idec idx))))]
              (recur i (aget arr idx) (isub shift (i5))))))))))

(defn offset-transient :- Int
  "Returns offset into array in rrbt `vec` for a given `i`.
  Use arrayFor to return the array itself."
  [vec :- clojure.core.rrb_vector.rrbt.Transient, i :- Int]
  (if (or (ineg? i) (i>= i (.count vec)))
    (throw (index-out-of-bounds))
    (let [am :- clojure.core.ArrayManager (.-am vec)
          nm :- clojure.core.rrb_vector.nodes.NodeManager (.-nm vec)
          shift (._shift vec)
          tail-off (isub (.count vec) (.alength am (._tail vec)))]
      (if (i<= tail-off i)
        (isub i tail-off)
        (iloop [i i, node (._root vec), shift shift]
          (if (or (izero? shift) (.regular nm node))
            (iand i (i31))
            (let [arr :- AnyArray (.array nm node)
                  rngs (ranges nm node)
                  idx (iloop [j (iand (i>> i shift) (i31))]
                        (if (i< i (aget rngs j)) j (recur (iinc j))))
                  i (if (izero? idx)
                      i
                      (isub i (aget rngs (idec idx))))]
              (recur i (aget arr idx) (isub shift (i5))))))))))

(defn ^:private reduce-vector :- Any
  "Reduce section of Rrbt Vector."
  [vec :- clojure.core.rrb_vector.rrbt.Vector,
   reducef :- AnyFn, init :- Any, begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (let [am :- clojure.core.ArrayManager (.-am vec)
          af (advance-fn [ret :- Any, arr :- Any,
                            i :- Int, oi :- Int, alend :- Int]
               (i< oi alend)
               (recur (reducef ret (.aget am arr oi))
                      arr (iinc i) (iinc oi) alend)
               (i< i end)
               (let [narr (.arrayFor vec i)]
                 (recur ret narr i (i0)
                        (imin (isub end i) (.alength am narr))))
               :else ret)
          arr (.arrayFor vec begin)
          oi (offset vec begin)]
      (af init arr begin oi
          (iadd
           oi (imin (isub (.alength am arr) oi) (isub end begin)))))))

(defn ^:private reduce-transient-vector :- Any
  "Reduce section of transient Rrbt Vector."
  [vec :- clojure.core.rrb_vector.rrbt.Transient,
   reducef :- AnyFn, init :- Any, begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (let [am :- clojure.core.ArrayManager (.-am vec)
          af (advance-fn [ret :- Any, arr :- Any,
                            i :- Int, oi :- Int, alend :- Int]
               (i< oi alend)
               (recur (reducef ret (.aget am arr oi))
                      arr (iinc i) (iinc oi) alend)
               (i< i end)
               (let [narr (.arrayFor vec i)]
                 (recur ret narr i (i0)
                        (imin (isub end i) (.alength am narr))))
               :else ret)
          arr (.arrayFor vec begin)
          oi (offset-transient vec begin)]
      (af init arr begin oi
          (iadd
           oi (imin (isub (.alength am arr) oi) (isub end begin)))))))

(defn ^:private reversed-reduce-vector :- Any
  "Reduce reversed section of Rrbt Vector."
  [vec :- clojure.core.rrb_vector.rrbt.Vector,
   reducef :- AnyFn, init :- Any, begin :- Int, end :- Int]
  (if-not (i< begin end)
    init
    (let [am :- clojure.core.ArrayManager (.-am vec)
          af (advance-fn [ret :- Any, arr :- Any,
                            i :- Int, oi :- Int, albegin :- Int]
               (i>= oi albegin)
               (recur (reducef ret (.aget am arr oi))
                      arr (idec i) (idec oi) albegin)
               (i>= i begin)
               (let [narr (.arrayFor vec i)
                     noi (idec (.alength am narr))]
                 (recur ret narr i noi
                        (imax (i0) (isub noi (isub i begin)))))
               :else ret)
          nend (idec end)
          arr (.arrayFor vec nend)
          oi (offset vec nend)]
      (af init arr nend oi
          (imax (i0) (isub oi (isub nend begin)))))))


;;;; Public API

(deftype RrbtVector
  "A type for Relaxed Radix Balancex Trees (RRB-Trees) vectors."
  clojure.core.rrb_vector.rrbt.Vector
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  ;; j.u.RandomAccess
  IComparable
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (reduce-vector this reducef init (i0) (.-cnt this)))
  ISeqable ;; broken, patched in rrbt.clj to fallback to APV$Seq
  ICounted
  IEmptyable
  IPeekable
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.-cnt this))))
  (-get [this key not-found]
    (if (integer? key) (.nth this key not-found) not-found))
  IIndexed
  IReversible
  (-reverse [this] (reversed-vector-section this (i0) (.-cnt this)))
  ISectionable
  (-section [this nb ne]
    (let [l (.-cnt this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? nb) (== ne (.-cnt this)))
        this
        (vector-section this nb ne))))
  ISliceable
  (-slice [this nb ne]
    (let [l (.-cnt this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? nb) (== ne (.-cnt this)))
        this
        (.slicev this nb ne))))
  ICatenable
  (-cat [this other] (.splicev this other))
  ISequential
  IEditable
  IPersistentCollection
  IAssociative
  IStacked
  IInvocable
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ;; helpers for vector section
  ;; TODO: Seqs are suboptimal but it is hard to do it right if
  ;;       we want to utilize clojure.lang classes.
  IVectorSectionHelper
  (-reduce-vector [this reducef init begin end]
    (reduce-vector this reducef init begin end))
  (-seq [this begin end]
    (let [v (vector-section this begin end)]
      (assoc-meta (clojure.lang.APersistentVector$Seq. v 0)
        (meta this))))
  IReversedVectorSectionHelper
  (-reversed-reduce-vector [this reducef init begin end]
    (reversed-reduce-vector this reducef init begin end))
  (-reversed-seq [this begin end]
    (let [v (reversed-vector-section this begin end)]
      (assoc-meta (clojure.lang.APersistentVector$Seq. v 0)
        (meta this))))

  ;; Abstract types
  IPersistentVector)

(deftype TransientRrbtVector
  "A type for Relaxed Radix Balancex Trees (RRB-Trees) transient
  vectors."
  clojure.core.rrb_vector.rrbt.Transient
  IRed
  (-reduce [this reducef init]
    (reduce-transient-vector this reducef init (i0) (.count this)))
  ICounted
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.count this))))
  (-get [this key not-found]
    (if (integer? key) (.nth this key not-found) not-found))
  IIndexed
  IPeekable
  (-peek [this] (-nth this (idec (.count this)) nil))
  ISettleable
  IMutableCollection
  IMutableAssociative
  IMutableStacked
  IInvocable)

;;; Connect to BvtVector

(extend-type! clojure.lang.PersistentVector
  ISliceable
  (-slice [this nb ne]
    (if (and (zero? nb) (== (or ne -1) (count this)))
      this
      (dunaj.coll/-slice (as-rrbt this) nb ne)))
  ICatenable
  (-cat [this other]
    (dunaj.coll/-cat (as-rrbt this) other)))
