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

(ns dunaj.coll.recipe
  "Collection utilities that return transducers or collection
  recipes."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Generators" "Transducers"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Maybe Any AnyFn U I Va Predicate Fn]]
   [dunaj.boolean :refer [Boolean and or not true?]]
   [dunaj.host :refer [BatchManager ArrayManager AnyBatch Class
                       class-instance? provide-class keyword->class]]
   [dunaj.host.int :refer
    [Int iadd iint iinc idec i0 isub i< izero? i== iloop i<= imin i1
     imul inpos? ipos? imax i-1 ineg? i2 i>= inneg? ineg i> irem
     imax0 idiv iLF iCR]]
   [dunaj.host.number :refer [long]]
   [dunaj.math :refer [Number Integer == zero? min npos? one? quot
                       neg? < > / <= max >= pos? integer?]]
   [dunaj.math.unchecked :as mu]
   [dunaj.compare :refer
    [nil? IHash IEquiv = hash sentinel identical? defsentinel]]
   [dunaj.flow :refer [let if cond delay recur if-not do while
                       if-some when when-not if-let]]
   [dunaj.state :refer
    [IPending IReference deref clone ICloneable reset!]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [Type deftype satisfies? extend-protocol!]]
   [dunaj.threading :refer [-> ->>]]
   [dunaj.coll :refer
    [IEmptyable IRed ISequential ISeqable ISeq IEmptyAware IPeekable
     IPersistentCollection ICollectionFactory IHomogeneous ICounted
     IBatchedRed IUnpackedRed ISectionable IReversible IFlippable
     IReducing ISettleable Transducer
     item-type conj counted? count empty? reduce seq single? first
     sectionable? next section reduced? postponed? postponed advance
     -reduce-unpacked -item-type -flip get reduced sequential? full?
     -section settle! conj! -reduce-batched -reduce -count peek pop!
     -capacity -full? -peek -conj! unsafe-advance! edit second cat!
     reduce-augmented nth reducing double?]]
   [dunaj.function :refer
    [Function defn fn apply comp fn? identity complement constantly]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable -fold invoke fork join folding]]
   [dunaj.coll.helper :refer
    [reduce* reduce-batched* reduce-unpacked* fold-every -inner-coll
     adapt* prepare-ordered-section fold-sectionable defreducing
     defxform reducing-function fold* strip-reduced reduced-advance
     split-adjust reduce-augmented* transduce* transfold*
     adaptcbUs adaptcbus adaptCbuS adaptcs adaptbu adaptu adaptCS
     advance-fn finish-advance reduce-with-batched*]]
   [dunaj.host.batch :refer
    [decide-item-type item-types-match?
     provide-batch-size batch-manager select-item-type]]
   [dunaj.host.array :refer [array-manager]]
   [dunaj.string :refer [String empty-string MutableString]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.error :refer [throw ex-info illegal-argument]]
   [dunaj.state.var :refer [declare def replace-var!]]
   [dunaj.state.basic :refer [unsynchronized-reference]]
   [dunaj.buffer :refer [sliding-buffer buffer]]
   [dunaj.coll.util :refer [some into every? doall recipe]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.tuple :refer [tuple pair key val empty-tuple]]
   [dunaj.coll.default :refer []]))


;;;; Implementation details

(defsentinel nothing)

(defn ^:private count-nth-num
  [n l]
  (if (npos? l)
    0
    (let [x (long (quot l n))]
      (if (== l (mu/multiply x n)) x (mu/inc x)))))

(defn ^:private unpacked-fn
  [f]
  (fn
    ([val a b] (f val (pair a b)))
    ([val a b c] (f val (tuple a b c)))
    ([val a b c & more] (f val (cons a (cons b (cons c more)))))))


;;;; Public API

;;; Other than transducers

(deftype EmptyRecipe
  []
  IRed
  (-reduce [this reducef init] init)
  ICounted
  (-count [this] 0)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init] init)
  IUnpackedRed
  (-reduce-unpacked [this reducef init] init)
  IHomogeneous
  (-item-type [this] nil)
  ISectionable
  (-section [this nb ne] (prepare-ordered-section nb ne 0) this)
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef] (combinef)))

(def empty-recipe :- IRed
  "An empty recipe."
  {:added v1
   :see '[throwing-recipe]}
  (->EmptyRecipe))

(deftype Throwing
  [m :- (Maybe String)]
  IRed
  (-reduce [this reducef init] (throw (ex-info m)))
  ICounted
  (-count [this] 1)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (throw (ex-info m)))
  IUnpackedRed
  (-reduce-unpacked [this reducef init] (throw (ex-info m)))
  IHomogeneous
  (-item-type [this] nil)
  ISectionable
  (-section [this nb ne]
    (let [ne (prepare-ordered-section nb ne 1)]
      (if (izero? (isub (iint ne) (iint nb))) empty-recipe this)))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (throw (ex-info m))))

(defn throwing-recipe :- IRed
  "Returns a collection recipe which throws if any attempt to reduce
  it is performed."
  {:added v1
   :see '[empty-recipe]}
  [m :- (Maybe String)]
  (->Throwing m))

(declare concat)

(deftype Concat
  "A type for the concatenation of collections."
  [colls :- IRed, count-ref :- IReference]
  IRed
  (-reduce [this reducef init]
    (reduce* colls #(reduce* %2 reducef %) init))
  ICounted
  (-count [this] @count-ref)
  IHomogeneous
  (-item-type [this] (some -item-type colls))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (reduce* colls #(reduce-unpacked* %2 reducef %) init))
  ISectionable
  (-section [this nb ne]
    (let [l (iint (count this))
          ne (iint (prepare-ordered-section nb ne l))
          nl (isub ne nb)]
      (cond (izero? nl) empty-recipe
            (and (izero? nb) (i== l ne)) this
            :else
            (let [wf (iloop [cs (seq colls), left (iint nb)]
                       (let [fc (first cs), c (count (or fc []))]
                         (cond
                          (or (nil? cs) (izero? left)) cs
                          (i<= c left) (recur (next cs) (isub left c))
                          :else (cons
                                 (section fc (isub c (isub c left)))
                                 (next cs)))))
                  wf (iloop [v [], cs wf, left nl]
                       (let [fc (first cs), c (count (or fc []))]
                         (cond
                          (or (nil? cs) (izero? left)) v
                          (i<= c left)
                          (recur (conj v fc) (next cs) (isub left c))
                          :else (conj v (section fc (i0) left)))))]
              (apply concat wf)))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-every colls reduce-fn pool n combinef reducef))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [dt (decide-item-type requested-type (item-type this))
          rf #(reduce-batched* dt size-hint %2 reducef %)]
      (reduce* colls rf init))))

(defn ^:private concat? :- Boolean
  [coll :- Any]
  (and (class-instance? dunaj.coll.helper.IRedAdapter coll)
       (class-instance? dunaj.coll.recipe.Concat (-inner-coll coll))))

(defn concat :- []
  "Returns the concatenation of given input collections.
  Does not support infinite number of arguments."
  {:added v1
   :category "Primary"
   :see '[concat* append dunaj.coll.lazy-seq/lazy-cat dunaj.coll/cat
          dunaj.coll.util/merge dunaj.coll.util/into]}
  ([] empty-tuple)
  ([a :- []] a)
  ([a :- [], b :- []]
   (let [b? (and (satisfies? IBatchedRed a)
                 (satisfies? IBatchedRed b)
                 (item-types-match? (item-type a) (item-type b)))
         ca? (counted? a)
         cb? (counted? b)
         c? (and ca? cb?)
         u? (and (satisfies? IUnpackedRed a)
                 (satisfies? IUnpackedRed b))
         s? (and (sectionable? a) (sectionable? b))
         add-fn #(iadd % (iint (count %2)))
         gcc #(.-colls ^dunaj.coll.recipe.Concat (-inner-coll %))]
     (cond (or (nil? a) (and ca? (empty? a))) b
           (or (nil? b) (and cb? (empty? b))) a
           :else
           (let [colls (cond (and (concat? a) (concat? b))
                             (into (gcc a) (gcc b))
                             (concat? a) (conj (gcc a) b)
                             (concat? b) (into [a] (gcc b))
                             :else (tuple a b))]
             (adapt*
              (->Concat colls (delay (reduce add-fn (i0) colls)))
              nil c? b? u? s?)))))
  ([a :- [], b  :- [] & more :- []]
   (let [colls (cons a (cons b more))
         colls (clojure.core/remove
                #(and (counted? %) (empty? %)) colls)
         colls (clojure.core/keep clojure.core/identity colls)
         it (some #(when (satisfies? IBatchedRed %) (item-type %))
                  colls)
         b? (every? #(and (satisfies? IBatchedRed %)
                          (item-types-match? it (item-type %)))
                    colls)
         c? (every? counted? colls)
         u? (every? #(satisfies? IUnpackedRed %) colls)
         s? (every? sectionable? colls)
         gcc #(.-colls ^dunaj.coll.recipe.Concat (-inner-coll %))
         rf (fn [ret val]
              (if (concat? val) (into ret (gcc val)) (conj ret val)))
         colls (reduce rf [] colls)
         add-fn #(iadd % (iint (count %2)))]
     (if (single? colls)
       (first colls)
       (adapt* (->Concat colls (delay (reduce add-fn (i0) colls)))
               nil c? b? u? s?)))))


(deftype ZipPair
  [a :- IRed, b :- IRed]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret a b]
               (and (postponed? a) (postponed? b))
               (recur (reducef ret @a @b)
                      (unsafe-advance! a) (unsafe-advance! b))
               :else ret)]
      (af init
          (reduce #(postponed %2) nil a)
          (reduce #(postponed %2) nil b))))
  ICounted
  (-count [this]
    (imin (iint (count a)) (iint (count b)))))

(deftype FoldableZipPair
  [a :- IRed, b :- IRed]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret a b]
               (and (postponed? a) (postponed? b))
               (recur (reducef ret @a @b)
                      (unsafe-advance! a) (unsafe-advance! b))
               :else ret)]
      (af init
          (reduce #(postponed %2) nil a)
          (reduce #(postponed %2) nil b))))
  ICounted
  (-count [this]
    (imin (iint (count a)) (iint (count b))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? ne) (== ne l))
        this
        (->FoldableZipPair (section a nb ne) (section b nb ne))))))

(deftype ZipTriple
  [a :- IRed, b :- IRed, c :- IRed]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret a b c]
               (and (postponed? a) (postponed? b) (postponed? c))
               (recur (reducef ret @a @b @c) (unsafe-advance! a)
                      (unsafe-advance! b) (unsafe-advance! c))
               :else ret)]
      (af init
          (reduce #(postponed %2) nil a)
          (reduce #(postponed %2) nil b)
          (reduce #(postponed %2) nil c))))
  ICounted
  (-count [this] (min (count a) (count b) (count c))))

(deftype FoldableZipTriple
  [a :- IRed, b :- IRed, c :- IRed]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret a b c]
               (and (postponed? a) (postponed? b) (postponed? c))
               (recur (reducef ret @a @b @c) (unsafe-advance! a)
                      (unsafe-advance! b) (unsafe-advance! c))
               :else ret)]
      (af init
          (reduce #(postponed %2) nil a)
          (reduce #(postponed %2) nil b)
          (reduce #(postponed %2) nil c))))
  ICounted
  (-count [this] (min (count a) (count b) (count c)))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? ne) (== ne l))
        this
        (->FoldableZipTriple
         (section a nb ne) (section b nb ne) (section c nb ne))))))

(deftype Zip
  [colls :- IRed]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret ims]
               (every? postponed? ims)
               (recur (apply reducef ret
                             (clojure.core/map deref ims))
                      (clojure.core/map unsafe-advance! ims))
               :else ret)]
      (af init (clojure.core/map
                (fn [x] (reduce #(postponed %2) nil x)) colls))))
  ICounted
  (-count [this]
    (apply min (clojure.core/map count colls))))

(deftype FoldableZip
  [colls :- IRed]
  IRed
  (-reduce [this reducef init]
    (-reduce-unpacked this (unpacked-fn reducef) init))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret ims]
               (every? postponed? ims)
               (recur (apply reducef ret
                             (clojure.core/map deref ims))
                      (clojure.core/map unsafe-advance! ims))
               :else ret)]
      (af init (clojure.core/map
                (fn [x] (reduce #(postponed %2) nil x)) colls))))
  ICounted
  (-count [this]
    (apply min (clojure.core/map count colls)))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (if-not (and (zero? ne) (== ne l))
        (->FoldableZip (clojure.core/map #(section % nb ne) colls))
        this))))

(defn zip :- IRed
  "Returns multireducible from given colls. Reducing multireducible
  will reduce on tuple of items.

  Use `<<dunaj.coll.util.api.ad#unpacked,unpacked>>` function to
  get collection which reduces on individual items."
  {:added v1
   :category "Primary"
   :see '[dunaj.coll.util/unpacked dunaj.coll.util/reduce-unpacked
          indexed]}
  ([a :- [], b :- []]
   (let [c? (and (counted? a) (counted? b))]
     (cond (and c? (sectionable? a) (sectionable? b))
           (->FoldableZipPair a b)
           c? (->ZipPair a b)
           :else (adaptcbUs (->ZipPair a b)))))
  ([a :- [], b :- [], c :- []]
   (let [c? (and (counted? a) (counted? b) (counted? c))]
     (cond (and c? (sectionable? a) (sectionable? b) (sectionable? c))
           (->FoldableZipTriple a b c)
           c? (->ZipTriple a b c)
           :else (adaptcbUs (->ZipTriple a b c)))))
  ([a :- [], b :- [], c :- [] & colls :- []]
   (let [colls (cons a (cons b (cons c colls)))
         c? (every? counted? colls)
         s? (every? sectionable? colls)]
     (cond (and c? s?) (->FoldableZip colls)
           c? (->Zip colls)
           :else (adaptcbUs (->Zip colls))))))

(declare range)

(defn indexed :- IRed
  "Returns a multireducible with `(range)` as a first collection."
  {:added v1
   :category "Primary"
   :see '[zip dunaj.coll.util/unpacked]}
  ([coll :- []]
   (if (counted? coll)
     (zip (range (count coll)) coll)
     (zip (range) coll)))
  ([coll :- [] & colls :- []]
   (let [colls (cons coll colls)
         r (if (every? counted? colls)
             (range (apply min (clojure.core/map count colls)))
             (range))]
     (apply zip r colls))))

(deftype Interleave
  [colls :- IRed]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret vals ims]
               vals (recur (reducef ret (first vals)) (next vals) ims)
               (every? postponed? ims)
               (recur ret
                      (doall (clojure.core/map deref ims))
                      (clojure.core/map unsafe-advance! ims))
               :else ret)
          ims (clojure.core/map
               (fn [x] (reduce #(postponed %2) nil x)) colls)]
      (af init nil ims)))
  ICounted
  (-count [this]
    (imul (iint (count colls))
          (apply min (clojure.core/map count colls)))))

(declare interleave-count interleave-section)

(deftype FoldableInterleave
  [colls :- IRed]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret vals ims]
               vals (recur (reducef ret (first vals)) (next vals) ims)
               (every? postponed? ims)
               (recur ret
                      (doall (clojure.core/map deref ims))
                      (clojure.core/map unsafe-advance! ims))
               :else ret)
          ims (clojure.core/map
               (fn [x] (reduce #(postponed %2) nil x)) colls)]
      (af init nil ims)))
  ICounted
  (-count [this]
    (imul (iint (count colls))
          (apply min (clojure.core/map count colls))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn  pool n combinef reducef
                      interleave-section interleave-count)))

(defn ^:private interleave-count
  [x]
  (apply min
         (clojure.core/map
          count (.-colls ^dunaj.coll.recipe.FoldableInterleave x))))

(defn ^:private interleave-section
  [il nb ne]
  (let [l (interleave-count il)
        ne (prepare-ordered-section nb ne l)]
    (if (and (zero? nb) (== ne l))
      il
      (->FoldableInterleave
       (clojure.core/map
        #(section % nb ne)
        (.-colls ^dunaj.coll.recipe.FoldableInterleave il))))))

(defn interleave :- []
  "Returns a collection recipe which reduces first item in each coll,
  then the second etc."
  {:added v1
   :see '[interpose zip]
   :category "Primary"}
  ([] [])
  ([c1 :- []] c1)
  ([c1 :- [] & more :- []]
   (let [colls (cons c1 more)
         c? (every? counted? colls)
         s? (every? sectionable? colls)]
     (cond (and c? s?) (->FoldableInterleave colls)
           c? (->Interleave colls)
           :else (adaptcbus (->Interleave colls))))))

(deftype PreTraverse
  [root :- Any, branch-fn :- Predicate, children-fn :- AnyFn]
  IRed
  (-reduce [this reducef init]
    (let [bf #(if (branch-fn %) (cons (seq (children-fn %)) %2) %2)
          af (advance-fn [ret others]
               (nil? others) ret
               :else
               (if-let [children (seq (first others))]
                 (let [val (first children)
                       nothers (cons (next children) (next others))]
                   (recur (reducef ret val) (bf val nothers)))
                 (recur ret (next others))))]
      (af (reducef init root) (bf root nil)))))

(deftype PostTraverse
  [root :- Any, branch-fn :- Predicate, children-fn :- AnyFn]
  IRed
  (-reduce [this reducef init]
    (let [bf #(pair % (when (branch-fn %) (seq (children-fn %))))
          af (advance-fn [ret others]
               (nil? others) ret
               :else
               (let [p (first others)
                     node (key p)
                     children (val p)]
                 (if children
                   (recur ret
                          (cons (bf (first children))
                                (cons (pair node (next children))
                                      (next others))))
                   (recur (reducef ret node) (next others)))))]
      (af init (cons (bf root) nil)))))

(deftype ITWrap [node :- Any, i :- Int, children :- Any])

(deftype InTraverse
  [root :- Any, branch-fn :- Predicate, children-fn :- AnyFn,
   n :- Int]
  IRed
  (-reduce [this reducef init]
    (let [bf #(let [b? (branch-fn %)]
                (->ITWrap
                 % (if b? n (i0)) (when b? (seq (children-fn %)))))
          af (advance-fn [ret others]
               (nil? others) ret
               :else
               (let [p (first others)
                     node (.-node ^dunaj.coll.recipe.ITWrap p)
                     i (.-i ^dunaj.coll.recipe.ITWrap p)
                     children
                     (.-children ^dunaj.coll.recipe.ITWrap p)]
                 (cond
                  (izero? i)
                  (recur (reducef ret node)
                         (cons (->ITWrap nothing (idec i) children)
                               (next others)))
                  children
                  (recur ret
                         (cons (bf (first children))
                               (cons (->ITWrap
                                      node (idec i) (next children))
                                     (next others))))
                  (nothing? node) (recur ret (next others))
                  :else (recur (reducef ret node) (next others)))))]
      (af init (cons (bf root) nil)))))

(declare mapcat)

(deftype LevelTraverse
  [root :- Any, branch-fn :- Predicate, children-fn :- AnyFn]
  IRed
  (-reduce [this reducef init]
    (let [bf #(when (branch-fn %) (children-fn %))
          nl #(seq (mapcat bf %))
          af (advance-fn [ret level]
               (nil? level) ret
               :else (recur (reduce* level reducef ret) (nl level)))]
      (af (reducef init root) (nl (tuple root))))))

(defn traverse :- IRed
  "Returns a collection recipe which contains nodes from tree
  traversion starting at `_root_` node, branching if `_branch-fn_`
  returns true, using `_children-fn_` to produce children from a
  branch node. Works with infinite trees.

  Depth first search `_mode_`s:

  * `:pre` - pre-order
  * `:post` - post-order
  * `:in` - in-order
  * `N` - in-order where the node is visited after N branches are
    traversed (0 - preorder, 1 - classic inorder)

  Breadth first search modes:

  * `:level` - level order"
  {:added v1
   :category "Primary"}
  ([root :- Any]
   (traverse sequential? identity root))
  ([mode :- (U nil Keyword Integer), root :- Any]
   (traverse mode sequential? identity root))
  ([branch-fn :- Predicate, children-fn :- AnyFn, root :- Any]
   (->PreTraverse root branch-fn children-fn))
  ([mode :- (U nil Keyword Integer),
    branch-fn :- Predicate, children-fn :- AnyFn, root :- Any]
   (cond (identical? :post mode)
         (->PostTraverse root branch-fn children-fn)
         (identical? :level mode)
         (->LevelTraverse root branch-fn children-fn)
         (identical? :in mode)
         (->InTraverse root branch-fn children-fn (i1))
         (and (integer? mode) (pos? mode))
         (->InTraverse root branch-fn children-fn (iint mode))
         :else (->PreTraverse root branch-fn children-fn))))

;;; Generators

(deftype Range
  [type :- Class, start :- Number, step :- Number]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret i]
               (recur (reducef ret i) (mu/add i step)))]
      (af init start)))
  IHomogeneous
  (-item-type [this] type)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          bm (batch-manager (select-item-type requested-type type))
          batch :- AnyBatch (.allocate bm batch-size)
          bf (fn [i]
               (if (.hasRemaining batch)
                 (do (.put bm batch i) (recur (mu/add i step)))
                 i))
          af (advance-fn [ret i]
               (let [_ (.clear batch), ni (bf i)]
                 (recur (reducef ret (.flip batch)) ni)))]
      (af init start))))

(deftype FiniteRange
  [start :- Number, end :- Number, step :- Number]
  IRed
  (-reduce [this reducef init]
    (let [c (count this)
          af (advance-fn [ret i n]
               (< n c)
               (recur (reducef ret i) (mu/add i step) (mu/inc n))
               :else ret)]
      (af init start 0)))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ICounted
  (-count [this]
    (if (neg? step)
      (count-nth-num
       (mu/negate step) (mu/negate (mu/subtract end start)))
      (count-nth-num step (mu/subtract end start))))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? nb) (== ne l))
        this
        (->FiniteRange (mu/add start (mu/multiply step nb))
                       (mu/add start (mu/multiply step ne))
                       step))))
  IFlippable
  (-flip [this]
    (->FiniteRange
     (mu/add start (mu/multiply step (mu/dec (count this))))
     (let [amount (/ step 2.0)]
       (if (neg? step)
         (mu/add start amount)
         (mu/subtract start amount)))
     (mu/negate step)))
  IReversible
  (-reverse [this] (-flip this)))

(deftype FiniteBatchableRange
  [type :- Class, start :- Number, end :- Number, step :- Number]
  IRed
  (-reduce [this reducef init]
    (let [c (count this)
          af (advance-fn [ret i n]
               (< n c)
               (recur (reducef ret i) (mu/add i step) (mu/inc n))
               :else ret)]
      (af init start 0)))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ICounted
  (-count [this]
    (if (neg? step)
      (count-nth-num
       (mu/negate step) (mu/negate (mu/subtract end start)))
      (count-nth-num step (mu/subtract end start))))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? nb) (== ne l))
        this
        (->FiniteBatchableRange type
                                (mu/add start (mu/multiply step nb))
                                (mu/add start (mu/multiply step ne))
                                step))))
  IFlippable
  (-flip [this]
    (->FiniteBatchableRange
     type
     (mu/add start (mu/multiply step (mu/dec (count this))))
     (let [amount (/ step 2.0)]
       (if (neg? step)
         (mu/add start amount)
         (mu/subtract start amount)))
     (mu/negate step)))
  IReversible
  (-reverse [this] (-flip this))
  IHomogeneous
  (-item-type [this] type)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          bm (batch-manager (select-item-type requested-type type))
          batch :- AnyBatch (.allocate bm batch-size)
          bf (fn [i]
               (if (and (.hasRemaining batch)
                        (if (neg? step) (> i end) (< i end)))
                 (do (.put bm batch i)
                     (recur (mu/add i step)))
                 i))
          af (advance-fn [ret i]
               (let [_ (.clear batch), ni (bf i), _ (.flip batch)]
                 (if (.hasRemaining batch)
                   (recur (reducef ret batch) ni)
                   ret)))]
      (af init start))))

(declare repeat)

(defn range :- IRed
  "Returns a collection recipe containing numbers from `_start_` to
  `_end_`, with given `_step_`.

  Optionally supports primitive data types and batched reduce
  when `_type_` is specified.

  CAUTION: Implementation note: The implementation incrementally adds
  `_step_` amount, which is faster than using multiplication,
  but precision is gradualy lost if using floating point step."
  {:added v1
   :see '[repeat cycle]
   :category "Generators"}
  ([] (range 0 nil 1))
  ([end :- (Maybe Number)] (range 0 end 1))
  ([start :- Number, end :- (Maybe Number)] (range start end 1))
  ([start :- Number, end :- (Maybe Number), step :- Number]
   (cond (zero? step) (repeat start)
         (nil? end) (adaptcbus (->Range nil start step))
         (== end start) empty-recipe
         :else (->FiniteRange start end step)))
  ([type :- (U nil Class Type),
    start :- Number, end :- (Maybe Number), step :- Number]
   (let [t (provide-class type)]
     (cond (zero? step) (repeat t start)
           (nil? end) (->Range t start step)
           (== end start) empty-recipe
           :else (->FiniteBatchableRange t start end step)))))

(deftype Repeat
  [type :- Class, val :- Any]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (recur (reducef ret val)))] (af init)))
  IHomogeneous
  (-item-type [this] type)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          bm (batch-manager (select-item-type requested-type type))
          batch :- AnyBatch (.allocate bm batch-size)
          af (advance-fn [ret] (recur (reducef ret (.rewind batch))))]
      (while (.hasRemaining batch) (.put bm batch val))
      (af init))))

(deftype FiniteRepeat
  [type :- Class, n :- Integer, val :- Any]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret i]
               (zero? i) ret
               :else (recur (reducef ret val) (mu/dec i)))]
      (af init n)))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ICounted
  (-count [this] n)
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (if (and (zero? nb) (== ne l))
        this
        (->FiniteRepeat type (mu/subtract ne nb) val))))
  IHomogeneous
  (-item-type [this] type)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          bm (batch-manager (select-item-type requested-type type))
          batch :- AnyBatch (.allocate bm batch-size)
          af (advance-fn [ret i]
               (< i batch-size)
               (do (.rewind batch)
                   (reducef ret (.limit batch i)))
               :else
               (recur (reducef ret (.rewind batch))
                      (mu/subtract i batch-size)))]
      (while (.hasRemaining batch) (.put bm batch val))
      (af init n))))

(defn repeat :- IRed
  "Returns a collection of `_val_`-s
  (infinite, or length `_n_` if supplied). For primitive collections,
  `_type_` can be supplied as an additional argument."
  {:added v1
   :see '[range cycle]
   :category "Generators"}
  ([val :- Any]
   (repeat nil val))
  ([n :- (Maybe Integer), val :- Any]
   (if (nil? n)
     (adaptcbus (->Repeat nil val))
     (adaptCbuS (->FiniteRepeat nil n val))))
  ([type :- (U nil Class Type), n :- (Maybe Integer), val :- Any]
   (let [t (provide-class type)]
     (if (nil? n) (->Repeat t val) (->FiniteRepeat t n val)))))

(deftype Cycle
  [coll :- IRed]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (recur (reduce* coll reducef ret)))]
      (af init)))
  IHomogeneous
  (-item-type [this] (item-type coll))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (let [af (advance-fn [ret]
               (recur (reduce-unpacked* coll reducef ret)))]
      (af init)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [af (advance-fn [ret]
               (recur (reduce-batched* requested-type size-hint
                                       coll reducef ret)))]
      (af init))))

(defn cycle :- IRed
  "Returns an infinite collection of repetitions of the items
  in `_coll_`."
  {:added v1
   :see '[repeat range]
   :category "Generators"}
  [coll :- []]
  (if (or (nil? coll) (and (counted? coll) (empty? coll)))
    coll
    (adaptcs (->Cycle coll) coll)))

(deftype Repeatedly [type repf]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (recur (reducef ret (repf))))]
      (af init)))
  IHomogeneous
  (-item-type [this] type)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          bm (batch-manager (select-item-type requested-type type))
          batch :- AnyBatch (.allocate bm batch-size)
          bf (fn [batch :- AnyBatch]
               (if (.hasRemaining batch)
                 (recur (.put bm batch (repf)))
                 (.flip batch)))
          af (advance-fn [ret]
               (recur (reducef ret (bf (.clear batch)))))]
      (af init))))

(declare take)

(defn repeatedly :- IRed
  "Returns a (infinite!, or length `_n_` if supplied) collection of
  calls to `_f_`. For primitive collections,
  `_type_` can be supplied as additional arg.

  CAUTION: Be aware that batched reduce may call `_f_` more times
  than requested."
  {:added v1
   :see '[repeat range cycle]
   :category "Generators"}
  ([f :- AnyFn] (repeatedly nil f))
  ([n :- (Maybe Integer), f :- AnyFn]
   (if-not (nil? n)
     (take n (repeatedly nil f))
     (adaptcbus (->Repeatedly nil f))))
  ([type :- (U nil Class Type), n :- (Maybe Integer), f :- AnyFn]
   (if-not (nil? n)
     (take n (repeatedly type nil f))
     (->Repeatedly (provide-class type) f))))

(deftype Iterate
  [type :- Class, iterf :- AnyFn, x :- Any]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret x] (recur (reducef ret x) (iterf x)))]
      (af init x)))
  IHomogeneous
  (-item-type [this] type)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          bm (batch-manager (select-item-type requested-type type))
          batch :- AnyBatch (.allocate bm batch-size)
          bf (fn [x :- Any]
               (if (.hasRemaining batch)
                 (do (.put bm batch x) (recur (iterf x)))
                 (do (.flip batch) x)))
          af (advance-fn [ret x]
               (let [_ (.clear batch), nx (bf x)]
                 (recur (reducef ret batch) nx)))]
      (af init x))))

(defn iterate :- IRed
  "Returns a collection of `_x_`, `(_f_ _x_)`, `(_f_ (_f_ _x_))` etc.
  `_f_` must be free of side-effects. For primitive collections,
  `_type_` can be supplied as first arg."
  {:added v1
   :see '[repeatedly repeat]
   :category "Generators"}
  ([f :- AnyFn, x :- Any]
   (adaptcbus (->Iterate nil f x)))
  ([type :- (U nil Class Type), f :- AnyFn, x :- Any]
   (->Iterate (provide-class type) f x)))

;;; Stateless Transducers

(defreducing MapReducing
  "Reducing type for map"
  [r :- IReducing, mapf :- AnyFn]
  (-step [this ret val] (._step r ret (mapf val)))
  (-step [this ret val val2] (._step r ret (mapf val val2)))
  (-step [this ret val val2 val3]
    (._step r ret (mapf val val2 val3)))
  (-step [this ret val val2 val3 val4]
    (._step r ret (mapf val val2 val3 val4)))
  (-step [this ret val val2 val3 val4 more]
    (._step r ret (apply mapf val val2 val3 val4 more))))

(defxform map
  "Returns a transducer that applies `_mapf_` to each step value."
  {:added v1
   :see '[mapcat filter remove flatten keep]
   :category "Transducers"}
  [mapf :- AnyFn]
  ([r] (->MapReducing r mapf))
  :unpack false)

(defreducing MapCatReducing
  "Reducing type for mapcat."
  [r :- IReducing, mapcatf :- AnyFn, rf :- AnyFn]
  (-step [this ret val]
    (reduce* (mapcatf val) rf ret))
  (-step [this ret val val2]
    (reduce* (mapcatf val val2) rf ret))
  (-step [this ret val val2 val3]
    (reduce* (mapcatf val val2 val3) rf ret))
  (-step [this ret val val2 val3 val4]
    (reduce* (mapcatf val val2 val3 val4) rf ret))
  (-step [this ret val val2 val3 val4 more]
    (reduce* (apply mapcatf val val2 val3 val4 more) rf ret)))

(defxform mapcat
  "Returns a transducer that applies `_mapcatf_` to each step value
  and concatenates the results."
  {:added v1
   :see '[map concat* keep flatten filter remove]
   :category "Transducers"}
  [mapcatf :- AnyFn]
  ([r] (->MapCatReducing r mapcatf (reducing-function r)))
  :count false
  :section false
  :unpack false)

(defn concat* :- Transducer
  "Returns a transducer which concatenates step values."
  {:added v1
   :see '[mapcat dunaj.coll/cat concat keep map]
   :transducer true
   :category "Transducers"}
  ([] (mapcat identity))
  ([xs] (mapcat identity xs)))

(defreducing FilterReducing
  "Reducing type for filter."
  [r :- IReducing, pred :- Predicate]
  (-step [this ret val]
    (if (pred val) (._step r ret val) ret))
  (-step [this ret val val2]
    (if (pred val val2) (._step r ret val val2) ret))
  (-step [this ret val val2 val3]
    (if (pred val val2 val3) (._step r ret val val2 val3) ret))
  (-step [this ret val val2 val3 val4]
    (if (pred val val2 val3 val4)
      (._step r ret val val2 val3 val4)
      ret))
  (-step [this ret val val2 val3 val4 more]
    (if (apply pred val val2 val3 val4 more)
      (._step r ret val val2 val3 val4 more)
      ret)))

(defxform filter
  "Returns a transducer that pass only those step values which
  satisfies predicate `_pred_`"
  {:added v1
   :see '[map remove keep mapcat replace]
   :category "Transducers"}
  [pred :- Predicate]
  ([r] (->FilterReducing r pred))
  :count false
  :section false)

(defxform remove
  "Returns a transducer that removes those step values which satisfies
  predicate `_pred_`"
  {:added v1
   :see '[map filter keep mapcat replace]
   :category "Transducers"}
  [pred :- Predicate]
  ([r] (->FilterReducing r (complement pred)))
  :count false
  :section false)

(defreducing ReplaceReducing
  "Reducing type for replace"
  [r :- IReducing, m :- {}]
  (-step [this ret val] (._step r ret (get m val val))))

(defxform replace
  "Returns a transducer that replaces any step values which equal to
  the key in map `_m_` with the respective val from `_m_`."
  {:added v1
   :see '[map filter remove keep mapcat dunaj.string/replace
          dunaj.coll.util/rename-keys dunaj.coll.util/walk]
   :category "Transducers"}
  [m :- {}]
  ([r] (->ReplaceReducing r m))
  :unpack false)

(defreducing KeepReducing
  "Reducing type for keep"
  [r :- IReducing, mapf :- AnyFn]
  (-step [this ret val]
    (if-some [nval (mapf val)] (._step r ret nval) ret))
  (-step [this ret val val2]
    (if-some [nval (mapf val val2)] (._step r ret nval) ret))
  (-step [this ret val val2 val3]
    (if-some [nval (mapf val val2 val3)] (._step r ret nval) ret))
  (-step [this ret val val2 val3 val4]
    (if-some [nval (mapf val val2 val3 val4)]
      (._step r ret nval)
      ret))
  (-step [this ret val val2 val3 val4 more]
    (if-some [nval (apply mapf val val2 val3 val4 more)]
      (._step r ret nval)
       ret)))

(defxform keep
  "Returns a transducer that applies `_mapf_` to each step value
  and pass only non-nil results."
  {:added v1
   :see '[map filter remove mapcat]
   :category "Transducers"}
  [mapf :- AnyFn]
  ([r] (->KeepReducing r mapf))
  :count false
  :section false
  :unpack false)

(defreducing TakeWhileReducing
  "Reducing type for take-while."
  [r :- IReducing, pred :- Predicate]
  (-step [this ret val]
    (if (pred val) (._step r ret val) (reduced ret)))
  (-step [this ret val val2]
    (if (pred val val2) (._step r ret val val2) (reduced ret)))
  (-step [this ret val val2 val3]
    (if (pred val val2 val3)
      (._step r ret val val2 val3)
      (reduced ret)))
  (-step [this ret val val2 val3 val4]
    (if (pred val val2 val3 val4)
      (._step r ret val val2 val3 val4)
      (reduced ret)))
  (-step [this ret val val2 val3 val4 more]
    (if (apply pred val val2 val3 val4 more)
      (._step r ret val val2 val3 val4 more)
      (reduced ret))))

(defxform take-while
  "Returns a transducer that pass step values while they satisfies
  predicate `_pred_`."
  {:added v1
   :see '[take take-last take-nth drop-while split-with]
   :category "Transducers"}
  [pred :- Predicate]
  ([r] (->TakeWhileReducing r pred))
  :count false
  :section false
  :fold false)

(defreducing FlattenReducing
  "Reducing type for flatten."
  [r :- IReducing, pred :- Predicate]
  (-step [this ret val]
    (if (pred val)
      (reduce* val (reducing-function this) ret)
      (._step r ret val))))

(defxform flatten*
  "Returns a transducer that flattens step values, using predicate
  `_pred_` to determine whether to flatten the value or not."
  {:added v1
   :see '[flatten]
   :category "Transducers"}
  [pred :- Predicate]
  ([r] (->FlattenReducing r pred))
  :count false
  :section false
  :unpack false)

(defn flatten
  "Returns a transducer that flattens step values, using predicate
  `_pred_` to determine whether to flatten the value or not."
  {:added v1
   :see '[flatten*]
   :transducer true
   :tsig (Fn [Transducer]
             [IRed (U nil IRed (I Function Predicate))]
             [IRed (Maybe Predicate) []])
   :category "Transducers"}
  ([] (flatten* sequential?))
  ([coll] (if (fn? coll) (flatten* coll) (flatten* sequential? coll)))
  ([pred coll] (flatten* (or pred sequential?) coll)))

(deftype AppendCollReducing
  "Reducing type for append-coll*"
  [r :- IReducing, coll :- []]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (if (reduced? wrap)
      (._finish r wrap)
      (finish-advance (reduce* coll (reducing-function r) wrap) r)))
  (-wrap [this ret] (._wrap r ret))
  (-unwrap [this wrap] (._unwrap r wrap))
  (-step [this wrap val] (._step r wrap val)))

(defxform append-coll*
  "Returns a transducer that 'appends' coll, passing its values
  after all step values are processed."
  [coll :- []]
  ([r] (->AppendCollReducing r coll))
  :count
  ([tc-fn] #(tc-fn (mu/add (count coll) %)))
  :section false
  ;; BUGGED, drops other transducers
  #_([ts-fn]
     (when tc-fn
       (let [nu-section #(dunaj.coll/-section %1 %2 %3)]
         #(let [e %4
                b %3
                c2-count (count coll)
                c1-count (count %2)
                e (or e (iadd c2-count c1-count))
                begin-in-c2? (<= c1-count b)
                end-in-c2? (< c1-count e)]
            (cond (and begin-in-c2? end-in-c2?)
                  (ts-fn nu-section coll
                         (mu/subtract b c1-count)
                         (mu/subtract e c1-count))
                  end-in-c2?
                  (let [nu-coll (dunaj.coll/-section
                                 coll 0 (mu/subtract e c1-count))]
                    (ts-fn
                     (fn [c b e]
                       (recipe (append-coll* nu-coll)
                               (dunaj.coll/-section c b e)))
                     %2 b nil))
                  :else
                  (ts-fn nu-section %2 b e))))))
  :fold false)

(deftype AppendReducing
  "Reducing type for append*"
  [r :- IReducing, x :- Any]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (if (reduced? wrap)
      (._finish r wrap)
      (finish-advance (._step r wrap x) r)))
  (-wrap [this ret] (._wrap r ret))
  (-unwrap [this wrap] (._unwrap r wrap))
  (-step [this wrap val] (._step r wrap val)))

(defxform append*
  "Returns a transducer that 'appends' val, passing it after all step
  values are processed."
  [x :- Any]
  ([r] (->AppendReducing r x))
  :count
  ([tc-fn] #(tc-fn (mu/inc %)))
  :section false
  ;; BUGGED, drops other transducers
  #_([ts-fn]
     (when tc-fn
       (let [nu-section #(dunaj.coll/-section %1 %2 %3)]
         #(let [e %4
                c1 %2
                appended? (or (nil? e) (== e (tc-fn (count c1))))]
            (if appended?
              (ts-fn %1 c1 %3 nil)
              (ts-fn nu-section c1 %3 e))))))
  :fold false)

(declare append-coll)

(deftype AppendColl
  [coll :- IRed, tail :- IRed]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (reduce* tail reducef ret))]
      (af (reduce* coll reducef init))))
  ICounted
  (-count [this] (mu/add (count coll) (count tail)))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)
          coll-count (count coll)
          tail-count (count tail)
          begin-in-tail? (<= coll-count nb)
          end-in-tail? (< coll-count ne)]
      (cond (and begin-in-tail? end-in-tail?)
            (-section tail
                      (mu/subtract nb coll-count)
                      (mu/subtract ne coll-count))
            end-in-tail?
            (append-coll (-section tail 0 (mu/subtract ne coll-count))
                         (-section coll nb nil))
            :else
            (-section coll nb ne))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (let [fc (fn [child]
               #(-fold child reduce-fn pool n combinef reducef))]
      (invoke pool
              #(let [f1 (fc coll)
                     t2 (fork (fc tail))]
                 (combinef (f1) (join t2)))))))

(defn ^:private append? :- Boolean
  [coll :- []]
  (and (class-instance? dunaj.coll.helper.IRedAdapter coll)
       (class-instance? dunaj.coll.recipe.AppendColl
                        (dunaj.coll.helper/-inner-coll coll))))

(defn ^:private get-tail :- IRed
  [coll :- IRed]
  (.-tail ^dunaj.coll.recipe.AppendColl
          (dunaj.coll.helper/-inner-coll coll)))

(defn ^:private get-coll :- IRed
  [coll :- IRed]
  (.-coll ^dunaj.coll.recipe.AppendColl
          (dunaj.coll.helper/-inner-coll coll)))

(defn append-coll
  "Returns a transducer that 'appends' coll, passing its values after
  all step values are processed."
  {:added v1
   :see '[append prepend concat dunaj.coll/into]
   :transducer true
   :tsig (Fn [Transducer []]
             [IRed [] []])
   :category "Transducers"}
  ([xs] (append-coll* xs))
  ([xs coll]
     (let [cx? (or (nil? coll) (counted? coll))]
       (cond (and cx? (empty? coll)) xs
             ;; special case if a is already an append
             (append? coll)
             (let [tail (concat (get-tail coll) xs)]
               (adaptbu (->AppendColl (get-coll coll) tail) coll))
             :else
             (adaptbu (->AppendColl coll xs) coll)))))

(defn append
  "Returns a transducer that 'appends' val, passing it after all step
  values are processed."
  {:added v1
   :see '[append-coll prepend dunaj.coll/conj]
   :transducer true
   :tsig (Fn [Transducer Any]
             [IRed Any []])
   :category "Transducers"}
  ([x] (append* x))
  ([x coll] (append-coll [x] coll)))

(declare keys)

(deftype Keys
  [coll :- IRed]
  IRed
  (-reduce [this reducef init]
    (reduce-unpacked* coll
                      (fn
                        ([r k _] (reducef r k))
                        ([r k _ _] (reducef r k))
                        ([r k _ _ _] (reducef r k))
                        ([r k _ _ _ & _] (reducef r k)))
                      init))
  ICounted
  (-count [this] (count coll))
  ISectionable
  (-section [this nb ne] (keys (section coll nb ne)))
  IHomogeneous
  (-item-type [this] (item-type coll))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (reduce-batched*
     requested-type size-hint (map key coll) reducef init))
  ISectionable
  (-section [this nb ne]
    (-section (map key coll) nb ne))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold* (map key coll) reduce-fn pool n combinef reducef)))

(defn keys
  "Returns a transducer that treats step values as pairs and passes
  only the first item of such pair."
  {:added v1
   :see '[vals filter map remove mapcat keep dunaj.coll.tuple/pair
          dunaj.coll.tuple/key dunaj.coll/first]
   :transducer true
   :tsig (Fn [Transducer] [IRed []])
   :category "Transducers"}
  ([] (map key))
  ([coll]
   (if (satisfies? IUnpackedRed coll)
     (adaptu (->Keys coll) coll)
     (map key coll))))

(declare vals)

(deftype Vals
  [coll :- IRed]
  IRed
  (-reduce [this reducef init]
    (reduce-unpacked* coll
                      (fn
                        ([r _ v] (reducef r v))
                        ([r _ v _] (reducef r v))
                        ([r _ v _ _] (reducef r v))
                        ([r _ v _ _ & _] (reducef r v)))
                      init))
  ICounted
  (-count [this] (count coll))
  ISectionable
  (-section [this nb ne] (vals (section coll nb ne)))
  IHomogeneous
  (-item-type [this] (item-type coll))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (reduce-batched*
     requested-type size-hint (map key coll) reducef init))
  ISectionable
  (-section [this nb ne]
    (-section (map val coll) nb ne))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold* (map val coll) reduce-fn pool n combinef reducef)))

(defn vals
  "Returns a transducer that treats step values as pairs and passes
  the second item of such pair."
  {:added v1
   :see '[keys dunaj.coll.tuple/pair dunaj.coll.tuple/val
          dunaj.coll/second map filter keep]
   :transducer true
   :tsig (Fn [Transducer] [IRed []])
   :category "Transducers"}
  ([] (map val))
  ([coll]
     (if (satisfies? IUnpackedRed coll)
       (adaptu (->Vals coll) coll)
       (map val coll))))

;;; Stateful Transducers

(deftype IntWrap [ret :- Any, i :- Int])

(deftype ObjectWrap [ret :- Any, x :- Any])

(defn ^:private int-advance :- IntWrap
  [ret :- Any, i :- Int]
  (cond (reduced? ret) (reduced (->IntWrap @ret i))
        (postponed? ret)
        (postponed (->IntWrap @ret i)
                   #(int-advance (advance ret) i)
                   #(int-advance (unsafe-advance! ret) i))
        :else (->IntWrap ret i)))

(defn ^:private reduced-int-advance :- IntWrap
  [ret :- Any, i :- Int]
  (cond (reduced? ret) (reduced (->IntWrap @ret i))
        (postponed? ret)
        (postponed (->IntWrap @ret i)
                   #(reduced-int-advance (advance ret) i)
                   #(reduced-int-advance (unsafe-advance! ret) i))
        :else (reduced (->IntWrap ret i))))

(defn cloning-advance :- ObjectWrap
  [ret :- Any, x :- Any]
  (cond (reduced? ret)
        (reduced (->ObjectWrap @ret x))
        (postponed? ret)
        (postponed (->ObjectWrap @ret x)
                   #(cloning-advance (advance ret) (clone x))
                   #(cloning-advance (unsafe-advance! ret) x))
        :else (->ObjectWrap ret x)))

(defn ^:private passing-advance :- ObjectWrap
  [ret :- Any, x :- Any]
  (cond (reduced? ret)
        (reduced (->ObjectWrap @ret x))
        (postponed? ret)
        (postponed (->ObjectWrap @ret x)
                   #(passing-advance (advance ret) x)
                   #(passing-advance (unsafe-advance! ret) x))
        :else (->ObjectWrap ret x)))

(defn our-wrap?
  [wrap s]
  (and (class-instance? dunaj.coll.recipe.ObjectWrap wrap)
       (identical? s (.-x ^dunaj.coll.recipe.ObjectWrap
                          (strip-reduced wrap)))))

(deftype PrependCollReducing
  "Reducing type for prepend coll."
  [r :- IReducing, coll :- IRed, s :- Any]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (if (our-wrap? wrap s)
          (reduce* coll
                   (reducing-function r)
                   (.-ret ^dunaj.coll.recipe.ObjectWrap
                          (strip-reduced wrap)))
          (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->ObjectWrap (._wrap r ret) s))
  (-unwrap [this wrap]
    (._unwrap r
              (if (our-wrap? wrap s)
                (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
                wrap)))
  (-step [this wrap val]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (reduce* coll
                   (reducing-function r)
                   (.-ret ^dunaj.coll.recipe.ObjectWrap ret)))
              :else (._step r ret val))]
      (af wrap)))
  (-step [this wrap val val2]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (reduce* coll
                   (reducing-function r)
                   (.-ret ^dunaj.coll.recipe.ObjectWrap ret)))
              :else (._step r ret val val2))]
      (af wrap)))
  (-step [this wrap val val2 val3]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (reduce* coll
                   (reducing-function r)
                   (.-ret ^dunaj.coll.recipe.ObjectWrap ret)))
              :else (._step r ret val val2 val3))]
      (af wrap)))
  (-step [this wrap val val2 val3 val4]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (reduce* coll
                   (reducing-function r)
                   (.-ret ^dunaj.coll.recipe.ObjectWrap ret)))
              :else (._step r ret val val2 val3 val4))]
      (af wrap)))
  (-step [this wrap val val2 val3 val4 more]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (reduce* coll
                   (reducing-function r)
                   (.-ret ^dunaj.coll.recipe.ObjectWrap ret)))
              :else (._step r ret val val2 val3 val4 more))]
      (af wrap))))

(deftype PrependReducing
  "Reducing type for prepend."
  [r :- IReducing, x :- Any, s :- Any]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (if (our-wrap? wrap s)
          (._step r
                  (.-ret ^dunaj.coll.recipe.ObjectWrap
                         (strip-reduced wrap))
                  x)
          (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->ObjectWrap (._wrap r ret) s))
  (-unwrap [this wrap]
    (._unwrap r
              (if (our-wrap? wrap s)
                (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
                wrap)))
  (-step [this wrap val]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap ret) x))
              :else (._step r ret val))]
      (af wrap)))
  (-step [this wrap val val2]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap ret) x))
              :else (._step r ret val val2))]
      (af wrap)))
  (-step [this wrap val val2 val3]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap ret) x))
              :else (._step r ret val val2 val3))]
      (af wrap)))
  (-step [this wrap val val2 val3 val4]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap ret) x))
              :else (._step r ret val val2 val3 val4))]
      (af wrap)))
  (-step [this wrap val val2 val3 val4 more]
    (let [af (advance-fn [ret]
              (our-wrap? ret s)
              (recur
               (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap ret) x))
              :else (._step r ret val val2 val3 val4 more))]
      (af wrap))))

(defxform prepend*
  "Returns a transducer that 'prepends' `_x_`, passing it before first
  step value is processed."
  [x :- Any]
  ([r] (->PrependReducing r x (sentinel)))
  :count
  ([tc-fn] #(tc-fn (mu/inc %)))
  :section false
  ;; BUGGED, drops other transducers
  #_([ts-fn]
     (when tc-fn
       (let [nu-section #(dunaj.coll/-section %1 %2 %3)]
         #(let [s (iint %3)
                e (when %4 (idec %4))
                c1 %2]
            (cond (and (i== s (i0)) e (i== e (i0)))
                  (ts-fn nu-section [x] 0 1)
                  (and e (ineg? e))
                  (ts-fn nu-section [] 0 0)
                  (i== s (i0)) (ts-fn %1 c1 0 e)
                  :else (ts-fn nu-section c1
                               (idec s) (when e (imax0 e))))))))
  :fold false)

(defxform prepend-coll*
  "Returns a transducer that 'prepends' `_coll_`, passing it before
  first step value is processed."
  [coll :- []]
  ([r] (->PrependCollReducing r coll (sentinel)))
  :count
  ([tc-fn] #(tc-fn (mu/add (count coll) %)))
  :section false
  ;; BUGGED, drops other transducers
  #_([ts-fn]
     (when tc-fn
       (let [nu-section #(dunaj.coll/-section %1 %2 %3)]
         #(let [e %4
                b %3
                c1-count (count coll)
                c2-count (count %2)
                e (or e (iadd c1-count c2-count))
                begin-in-c1? (< b c1-count)
                end-in-c1? (<= e c1-count)]
            (cond (and begin-in-c1? end-in-c1?)
                  (ts-fn nu-section coll b e)
                  begin-in-c1?
                  (let [nu-coll (dunaj.coll/-section coll b nil)]
                    (ts-fn
                     (fn [c b e]
                       (recipe (prepend-coll* nu-coll)
                               (dunaj.coll/-section c b e)))
                     %2 0 (mu/subtract e c1-count)))
                  :else
                  (ts-fn nu-section %2
                         (mu/subtract b c1-count)
                         (mu/subtract e c1-count)))))))
  :fold false)

(declare prepend-coll)

(deftype PrependColl
  [coll :- IRed, head :- IRed]
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (reduce* coll reducef ret))]
      (af (reduce* head reducef init))))
  ICounted
  (-count [this] (mu/add (count coll) (count head)))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)
          head-count (count head)
          coll-count (count coll)
          begin-in-head? (< nb head-count)
          end-in-head? (<= ne head-count)]
      (cond (and begin-in-head? end-in-head?)
            (-section head nb ne)
            end-in-head?
            (prepend-coll
             (-section head nb nil)
             (-section coll 0 (mu/subtract ne head-count)))
            :else
            (-section coll (mu/subtract nb head-count)
                      (mu/subtract ne head-count)))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (let [fc (fn [child]
               #(-fold child reduce-fn pool n combinef reducef))]
      (invoke pool
              #(let [f1 (fc head)
                     t2 (fork (fc coll))]
                 (combinef (f1) (join t2)))))))

(defn ^:private prepend? :- Boolean
  [coll :- []]
  (and (class-instance? dunaj.coll.helper.IRedAdapter coll)
       (class-instance? dunaj.coll.recipe.PrependColl
                        (dunaj.coll.helper/-inner-coll coll))))

(defn ^:private get-head :- IRed
  [coll :- IRed]
  (.-head ^dunaj.coll.recipe.PrependColl
          (dunaj.coll.helper/-inner-coll coll)))

(defn ^:private get-pcoll :- IRed
  [coll :- IRed]
  (.-coll ^dunaj.coll.recipe.PrependColl
          (dunaj.coll.helper/-inner-coll coll)))

(defn prepend-coll
  "Returns a transducer that 'prepends' coll, passing its values after
  all step values are processed."
  {:added v1
   :see '[append prepend concat dunaj.coll/into]
   :transducer true
   :tsig (Fn [Transducer []]
             [IRed [] []])
   :category "Transducers"}
  ([xs] (prepend-coll* xs))
  ([xs coll]
   (let [cx? (or (nil? coll) (counted? coll))]
     (cond (and cx? (empty? coll)) xs
           ;; special case if a is already a prepend
           (prepend? coll)
           (let [head (concat xs (get-head coll))]
             (adaptbu (->PrependColl (get-pcoll coll) head) coll))
           :else (adaptbu (->PrependColl coll xs) coll)))))

(defn prepend
  "Returns a transducer that 'prepends' val, passing it after all step
  values are processed."
  {:added v1
   :see '[prepend-coll append dunaj.coll/conj]
   :transducer true
   :tsig (Fn [Transducer Any]
             [IRed Any []])
   :category "Transducers"}
  ([x] (prepend* x))
  ([x coll] (prepend-coll [x] coll)))

(deftype TakeReducing
  "Reducing type for take."
  [r :- IReducing, n :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.IntWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->IntWrap (._wrap r ret) (idec n)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.IntWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance (._step r ret val) i)
        (int-advance (._step r ret val) (idec i)))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance (._step r ret val val2) i)
        (int-advance (._step r ret val val2) (idec i)))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance (._step r ret val val2 val3) i)
        (int-advance (._step r ret val val2 val3) (idec i)))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance (._step r ret val val2 val3 val4) i)
        (int-advance (._step r ret val val2 val3 val4) (idec i)))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance (._step r ret val val2 val3 val4 more) i)
        (int-advance (._step r ret val val2 val3 val4 more)
                     (idec i))))))

(defxform take*
  "Returns a transducer that passes at most `n` step values."
  [n :- Int]
  ([r] (->TakeReducing r n))
  :count ([tc-fn] #(tc-fn (imin n (iint %))))
  :section ([ts-fn]
              (when tc-fn
                #(let [l (tc-fn (count %2))
                       ne (prepare-ordered-section %3 %4 l)]
                   (ts-fn %1 %2 %3 ne))))
  :fold false)

(defn take
  "Returns a transducer that passes at most `_n_` step values."
  {:added v1
   :see '[take-while take-nth take-last drop]
   :transducer true
   :tsig (Fn [Transducer Int] [IRed Int []])
   :category "Transducers"}
  ([n] (take* n))
  ([n coll]
   (cond (inpos? n) empty-recipe
         (and (counted? coll) (sectionable? coll))
         (section coll 0 (imin n (iint (count coll))))
         :else (take* n coll))))

(deftype TakeNthReducing
  "Reducing type for take-nth."
  [r :- IReducing, n :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.IntWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->IntWrap (._wrap r ret) (i0)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.IntWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (ipos? i)
        (int-advance ret (idec i))
        (int-advance (._step r ret val) n))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (ipos? i)
        (int-advance ret (idec i))
        (int-advance (._step r ret val val2) n))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (ipos? i)
        (int-advance ret (idec i))
        (int-advance (._step r ret val val2 val3) n))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (ipos? i)
        (int-advance ret (idec i))
        (int-advance (._step r ret val val2 val3 val4) n))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (ipos? i)
        (int-advance ret (idec i))
        (int-advance (._step r ret val val2 val3 val4 more) n)))))

(defxform take-nth*
  "Returns a transducer that passes every `n`-th step values,
  starting with first one."
  [n :- Int]
  ([r] (->TakeNthReducing r (idec n)))
  :count ([tc-fn] #(tc-fn (count-nth-num n %)))
  :section ([ts-fn]
              (when tc-fn
                #(let [l (tc-fn (count %2))
                       ne (prepare-ordered-section %3 %4 l)]
                   (ts-fn %1 %2
                          (imul (iint %3) n)
                          (min (imul (iint ne) n) (count %2))))))
  :fold false)

(declare take-nth)

(deftype TakeNth
  [coll :- IRed, n :- Int, rc :- IRed]
  IRed
  (-reduce [this reducef init] (-reduce rc reducef init))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (-reduce-batched rc requested-type size-hint reducef init))
  IHomogeneous
  (-item-type [this] (-item-type rc))
  ICounted
  (-count [this] (-count rc))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (-reduce-unpacked rc reducef init))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (prepare-ordered-section nb ne l)]
      (take-nth n (section coll (imul (iint nb) n)
                           (min (imul (iint ne) n) (count coll)))))))

(defn take-nth
  "Returns a transducer that passes every `_n_`-th step values,
  starting with first one."
  {:added v1
   :see '[take drop partition]
   :transducer true
   :tsig (Fn [Transducer Int]
             [IRed Int []])
   :category "Transducers"}
  ([n] (take-nth* n))
  ([n coll]
   (cond (inpos? n) empty-recipe
         (and (counted? coll) (sectionable? coll))
         (adaptCS (->TakeNth coll n (recipe (take-nth* n) coll)) coll)
         :else (take-nth* n coll))))

(declare equiv-packed equals-packed)

(deftype Packed [vals :- IRed]
  IHash
  (-hash [this] (hash vals))
  IEquiv
  (-equiv [this other] (equiv-packed this other))
  java.lang.Object
  (hashCode [this] (.hashCode ^java.lang.Object vals))
  (equals [this other] (equals-packed this other)))

(defn ^:private equiv-packed :- Boolean
  [this :- Packed, other :- Any]
  (if (class-instance? dunaj.coll.recipe.Packed other)
    (= (.-vals this) (.-vals ^dunaj.coll.recipe.Packed other))
    false))

(defn ^:private equals-packed :- Boolean
  [this :- Packed, other :- Any]
  (if (class-instance? dunaj.coll.recipe.Packed other)
    (.equals (.-vals this) (.-vals ^dunaj.coll.recipe.Packed other))
    false))

(deftype TakeLastReducing
  "Reducing type for take-last."
  [r :- IReducing, n :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [w (strip-reduced wrap)
          ret (.-ret ^dunaj.coll.recipe.ObjectWrap w)
          buf (.-x ^dunaj.coll.recipe.ObjectWrap w)
          rf (reducing-function r)
          rf #(if (class-instance? dunaj.coll.recipe.Packed %2)
                (apply
                 rf % (.-vals ^dunaj.coll.recipe.Packed %2))
                (rf % %2))]
      (-> (if buf (reduce* (settle! buf) rf ret) ret)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (->ObjectWrap (._wrap r ret) (sliding-buffer n)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (cloning-advance ret (conj! buf val))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (cloning-advance
       ret (conj! buf (->Packed (tuple val val2))))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (cloning-advance
       ret (conj! buf (->Packed (tuple val val2 val3))))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (cloning-advance
       ret (conj! buf (->Packed (tuple val val2 val3 val4))))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (cloning-advance
       ret (conj! buf (->Packed
                       (apply tuple val val2 val3 val4 more)))))))

(defxform take-last*
  "Returns a transducer that passes at most last `n` step values."
  [n :- Int]
  ([r] (->TakeLastReducing r n))
  :count ([tc-fn] #(tc-fn (imin n (iint %))))
  :section ([ts-fn]
              (when tc-fn
                #(let [l (tc-fn (count %2))
                       offset (max 0 (isub (iint (count %2)) n))
                       ne (prepare-ordered-section %3 %4 l)]
                   (ts-fn %1 %2
                          (mu/add %3 offset)
                          (mu/add ne offset)))))
  :fold false)

(defn take-last
  "Returns a transducer that passes at most last `_n_` step values."
  {:added v1
   :see '[drop take drop-last dunaj.coll.util/butlast
          dunaj.coll.util/last]
   :transducer true
   :tsig (Fn [Transducer Int]
             [IRed Int []])
   :category "Transducers"}
  ([n] (take-last* n))
  ([n coll]
   (cond (inpos? n) empty-recipe
         (and (counted? coll) (sectionable? coll))
         (section coll (max 0 (isub (iint (count coll)) n)))
         :else (take-last* n coll))))

(deftype DropReducing
  "Reducing type for drop."
  [r :- IReducing, n :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.IntWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->IntWrap (._wrap r ret) n))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.IntWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (int-advance (._step r ret val) i)
        (int-advance ret (idec i)))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (int-advance (._step r ret val val2) i)
        (int-advance ret (idec i)))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (int-advance (._step r ret val val2 val3) i)
        (int-advance ret (idec i)))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (int-advance (._step r ret val val2 val3 val4) i)
        (int-advance ret (idec i)))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (int-advance (._step r ret val val2 val3 val4 more) i)
        (int-advance ret (idec i))))))

(defxform drop*
  "Returns a transducer that drops `n` first step values."
  [n :- Int]
  ([r] (->DropReducing r n))
  :count ([tc-fn] #(tc-fn (imax0 (isub (iint %) n))))
  :section ([ts-fn]
              (when tc-fn
                #(let [l (tc-fn (count %2))
                       ne (prepare-ordered-section %3 %4 l)]
                   (ts-fn %1 %2 %3 (iadd ne n)))))
  :fold false)

(defn drop
  "Returns a transducer that drops `_n_` first step values."
  {:added v1
   :see '[drop-while drop-last take]
   :transducer true
   :tsig (Fn [Transducer Int]
             [IRed Int []])
   :category "Transducers"}
  ([n] (drop* n))
  ([n coll]
   (cond (inpos? n) coll
         (and (counted? coll) (sectionable? coll))
         (if (>= n (count coll)) empty-recipe (section coll n))
         :else (drop* n coll))))

(deftype DropWhileReducing
  "Reducing type for drop-while."
  [r :- IReducing, pred :- Predicate]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.IntWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->IntWrap (._wrap r ret) (i1)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.IntWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (or (izero? i) (not (pred val)))
        (int-advance (._step r ret val) (i0))
        wrap)))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (or (izero? i) (not (pred val val2)))
        (int-advance (._step r ret val val2) (i0))
        wrap)))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (or (izero? i) (not (pred val val2 val3)))
        (int-advance (._step r ret val val2 val3) (i0))
        wrap)))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (or (izero? i) (not (pred val val2 val3 val4)))
        (int-advance (._step r ret val val2 val3 val4) (i0))
        wrap)))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (or (izero? i) (not (apply pred val val2 val3 val4 more)))
        (int-advance (._step r ret val val2 val3 val4 more) (i0))
        wrap))))

(defxform drop-while
  "Returns a transducer that drops step values while they satisfies
  predicate `_pred_`."
  {:added v1
   :see '[drop take take-while drop-last capped-drop-while]
   :category "Transducers"}
  [pred :- Predicate]
  ([r] (->DropWhileReducing r pred))
  :count false
  :section false
  :fold false)

(deftype CappedDropWhileReducing
  "Reducing type for capped-drop-while."
  [r :- IReducing, pred :- Predicate, n :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.IntWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->IntWrap (._wrap r ret) n))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.IntWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (cond (or (ineg? i) (not (pred val)))
            (int-advance (._step r ret val) (i-1))
            (izero? i) (throw (ex-info "limit has been reached"))
            :else (int-advance ret (idec i)))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (cond (or (ineg? i) (not (pred val val2)))
            (int-advance (._step r ret val val2) (i-1))
            (izero? i) (throw (ex-info "limit has been reached"))
            :else (int-advance ret (idec i)))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (cond (or (ineg? i) (not (pred val val2 val3)))
            (int-advance (._step r ret val val2 val3) (i-1))
            (izero? i) (throw (ex-info "limit has been reached"))
            :else (int-advance ret (idec i)))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (cond (or (ineg? i) (not (pred val val2 val3 val4)))
            (int-advance (._step r ret val val2 val3 val4) (i-1))
            (izero? i) (throw (ex-info "limit has been reached"))
            :else (int-advance ret (idec i)))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (cond (or (ineg? i) (not (apply pred val val2 val3 val4 more)))
            (int-advance (._step r ret val val2 val3 val4 more) (i-1))
            (izero? i) (throw (ex-info "limit has been reached"))
            :else (int-advance ret (idec i))))))

(defxform capped-drop-while
  "Returns a transducer that drops step values while they satisfies
  predicate `_pred_`. Throws after more than `_n_` items are dropped."
  {:added v1
   :see '[drop-while]
   :category "Transducers"}
  [n :- Int, pred :- Predicate]
  ([r] (->CappedDropWhileReducing r pred n))
  :count false
  :section false
  :fold false)

(deftype DropLastReducing
  "Reducing type for drop-last."
  [r :- IReducing, n :- Int]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.ObjectWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->ObjectWrap (._wrap r ret) (sliding-buffer n)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf :- dunaj.buffer.SlidingBuffer
          (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          ret (if (._sliding_full_QMARK_ buf)
                (._step r ret (.peek buf))
                ret)]
      (cloning-advance ret (.conj buf val)))))

(defxform drop-last*
  "Returns a transducer that passes all but last `n` step values."
  [n :- Int]
  ([r] (->DropLastReducing r n))
  :count ([tc-fn] #(tc-fn (imax0 (isub (iint %) n))))
  :section ([ts-fn]
              (when tc-fn
                #(let [l (tc-fn (count %2))
                       ne (prepare-ordered-section %3 %4 l)]
                   (ts-fn %1 %2 %3 (iadd ne n)))))
  :fold false)

(defn drop-last
  "Returns a transducer that passes all but last `_n_` step values."
  {:added v1
   :see '[take-last drop drop-while]
   :transducer true
   :tsig (Fn [Transducer Int]
             [IRed Int []])
   :category "Transducers"}
  ([n] (drop-last* n))
  ([n coll]
   (cond (inpos? n) coll
         (and (counted? coll) (>= n (count coll))) empty-recipe
         (and (counted? coll) (sectionable? coll))
         (section coll 0 (isub (iint (count coll)) n))
         :else (drop-last* n coll))))

(deftype InterposeReducing
  "Reducing type for interpose."
  [r :- IReducing, sep :- Any, rf :- AnyFn, s :- Any]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [get-ret #(if (our-wrap? % s)
                     (.-ret ^dunaj.coll.recipe.ObjectWrap %) %)]
      (-> (if (reduced? wrap)
            (reduced (get-ret @wrap))
            (get-ret wrap))
          (finish-advance r))))
  (-wrap [this ret] (->ObjectWrap (._wrap r ret) s))
  (-unwrap [this wrap]
    (if (our-wrap? wrap s)
      (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap))
      (._unwrap r wrap)))
  (-step [this wrap val]
    (if (our-wrap? wrap s)
      (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap) val)
      (let [af (advance-fn [ret] (._step r ret val))]
        (af (._step r wrap sep)))))
  (-step [this wrap val val2]
    (if (our-wrap? wrap s)
      (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap) val val2)
      (let [af (advance-fn [ret] (._step r ret val val2))]
        (af (apply rf wrap sep)))))
  (-step [this wrap val val2 val3]
    (if (our-wrap? wrap s)
      (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
              val val2 val3)
      (let [af (advance-fn [ret] (._step r ret val val2 val3))]
        (af (apply rf wrap sep)))))
  (-step [this wrap val val2 val3 val4]
    (if (our-wrap? wrap s)
      (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
              val val2 val3 val4)
      (let [af (advance-fn [ret] (._step r ret val val2 val3 val4))]
        (af (apply rf wrap sep)))))
  (-step [this wrap val val2 val3 val4 more]
    (if (our-wrap? wrap s)
      (._step r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
              val val2 val3 val4 more)
      (let [af (advance-fn [ret]
                             (._step r ret val val2 val3 val4 more))]
        (af (apply rf wrap sep))))))

(defxform interpose
  "Returns a transducer that passes `_sep_` value between step
  values."
  {:added v1
   :see '[interleave]
   :category "Transducers"}
  [sep :- Any]
  ([r] (->InterposeReducing r sep (reducing-function r) (sentinel)))
  :count ([tc-fn] #(tc-fn (idec (imul (i2) (iint %)))))
  :section false
  :fold false)

(deftype CappedReducing
  "Reducing type for cap."
  [r :- IReducing, n :- Int, tail :- IRed]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.IntWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret] (->IntWrap (._wrap r ret) n))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.IntWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance
         (reduce* tail (reducing-function r) ret) i)
        (int-advance (._step r ret val) (idec i)))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance
         (reduce-unpacked* tail (reducing-function r) ret) i)
        (int-advance (._step r ret val val2) (idec i)))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance
         (reduce-unpacked* tail (reducing-function r) ret) i)
        (int-advance (._step r ret val val2 val3) (idec i)))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance
         (reduce-unpacked* tail (reducing-function r) ret) i)
        (int-advance (._step r ret val val2 val3 val4) (idec i)))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.IntWrap wrap)
          i (.-i ^dunaj.coll.recipe.IntWrap wrap)]
      (if (izero? i)
        (reduced-int-advance
         (reduce-unpacked* tail (reducing-function r) ret) i)
        (int-advance
         (._step r ret val val2 val3 val4 more) (idec i))))))

(defxform cap*
  "Returns a transducer that passes `tail` after passing `n` step
  values.
  Tail is not passed if less than `n` step values were passed."
  [n :- Int, tail :- IRed]
  ([r] (->CappedReducing r n tail))
  :count false
  :section false
  :fold false)

(defn cap
  "Returns a transducer that passes values from `tail` after passing
  `_n_` step values. Tail is not passed if less than `_n_` step
  values were passed."
  {:added v1
   :see '[throwing-cap capped-drop-while]
   :transducer true
   :tsig (Fn [Transducer Int IRed]
             [IRed Int IRed []])
   :category "Transducers"}
  ([n tail]
   (if (inpos? n) identity (cap* n tail)))
  ([n tail coll]
   (cond
     (or (inpos? n) (and (counted? coll) (<= (count coll) n))) coll
     (sectionable? coll) (concat (section coll 0 n) tail)
     :else (cap* n tail coll))))

(defn throwing-cap
  "Returns a transducer that throws if more than `_n_` step values
  are passed."
  {:added v1
   :see '[cap capped-drop-while]
   :transducer true
   :tsig (Fn [Transducer Int]
             [IRed Int []])
   :category "Transducers"}
  ([n]
   (cap n (throwing-recipe "limit has been reached")))
  ([n coll]
   (cap n (throwing-recipe "limit has been reached") coll)))

(extend-protocol! ICloneable
  java.util.HashSet
  (-clone [this] (.clone this)))

(deftype DistinctReducing
  "Reducing type for distinct."
  [r :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.ObjectWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->ObjectWrap (._wrap r ret) (java.util.HashSet.)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf :- java.util.HashSet
          (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (if (.add buf val)
        (cloning-advance (._step r ret val) buf)
        wrap)))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf :- java.util.HashSet
          (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2))]
      (if (.add buf packed)
        (cloning-advance (._step r ret val val2) buf)
        wrap)))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf :- java.util.HashSet
          (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2 val3))]
      (if (.add buf packed)
        (cloning-advance (._step r ret val val2 val3) buf)
        wrap)))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf :- java.util.HashSet
          (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2 val3 val4))]
      (if (.add buf packed)
        (cloning-advance (._step r ret val val2 val3 val4) buf)
        wrap)))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          buf :- java.util.HashSet
          (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (apply tuple val val2 val3 val4 more))]
      (if (.add buf packed)
        (cloning-advance (._step r ret val val2 val3 val4 more) buf)
        wrap))))

(defxform distinct
  "Returns a transducer that passes only unique step values."
  {:added v1
   :see '[dedupe filter keep]
   :category "Transducers"}
  []
  ([r] (->DistinctReducing r))
  :count false
  :section false
  :fold false)

(deftype DedupeReducing
  "Reducing type for dedupe."
  [r :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.ObjectWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->ObjectWrap (._wrap r ret) (sentinel)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (if (= prev val)
        wrap
        (cloning-advance (._step r ret val) val))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2))]
      (if (= prev packed)
        wrap
        (cloning-advance (._step r ret val val2) packed))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2 val3))]
      (if (= prev packed)
        wrap
        (cloning-advance (._step r ret val val2 val3) packed))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2 val3 val4))]
      (if (= prev packed)
        wrap
        (cloning-advance (._step r ret val val2 val3 val4) packed))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          packed (->Packed (tuple val val2 val3 val4 more))]
      (if (= prev packed)
        wrap
        (cloning-advance (._step r ret val val2 val3 val4 more)
                         packed)))))

(defxform dedupe
  "Returns a transducer that filters out consecutive duplicate step
  values."
  {:added v1
   :see '[distinct filter keep remove]
   :category "Transducers"}
  []
  ([r] (->DedupeReducing r))
  :count false
  :section false
  :fold false)

(defn ^:private reductions-advance :- ObjectWrap
  [ret :- Any, x :- Any]
  (cond (reduced? ret)
        (reduced (->ObjectWrap @ret nothing))
        (postponed? ret)
        (postponed (->ObjectWrap @ret x)
                   #(reductions-advance (advance ret) x)
                   #(reductions-advance (unsafe-advance! ret) x))
        :else (->ObjectWrap ret x)))

(deftype ReductionsReducing
  "Reducing type for reductions."
  [r :- IReducing, f :- AnyFn, init :- Any]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (let [w (strip-reduced wrap)
              ret (.-ret ^dunaj.coll.recipe.ObjectWrap w)
              prev (.-x ^dunaj.coll.recipe.ObjectWrap w)]
          (if (nothing? prev) ret (._step r ret prev)))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->ObjectWrap (._wrap r ret) init))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (reductions-advance (._step r ret prev) (f prev val))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (reductions-advance (._step r ret prev) (f prev val val2))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (reductions-advance (._step r ret prev)
                          (f prev val val2 val3))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (reductions-advance (._step r ret prev)
                          (f prev val val2 val3 val4))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          prev (.-x ^dunaj.coll.recipe.ObjectWrap wrap)]
      (reductions-advance (._step r ret prev)
                          (apply f prev val val2 val3 val4 more)))))

(defxform reductions*
  "Returns a transducer that passes intermediate results of the
  reduction of step values by `f`, starting with `init`"
  [f :- AnyFn, init :- Any]
  ([r] (->ReductionsReducing r f init))
  :count false
  :section false
  :unpack false
  :fold false)

(defn reductions
  "Returns a transducer that passes intermediate results of the
  reduction of step values by `_f_`, starting with `_init_`
  which defaults to `(_f_)`."
  {:added v1
   :see '[dunaj.coll/reduce repeatedly iterate]
   :transducer true
   :tsig (Fn [Transducer AnyFn]
             [Transducer AnyFn Any]
             [IRed AnyFn Any []])
   :category "Transducers"}
  ([f] (reductions* f (f)))
  ([f init] (reductions* f init))
  ([f init coll] (reductions* f init coll)))

(deftype PartitionWrap
  [ret :- Any, items :- ISettleable, idx :- Int])

(defn ^:private partition-advance :- PartitionWrap
  [ret :- Any, items :- ISettleable, idx :- Int]
  (cond (reduced? ret)
        (reduced (->PartitionWrap @ret nil idx))
        (postponed? ret)
        (postponed
         (->PartitionWrap @ret items idx)
         #(partition-advance (advance ret) (clone items) idx)
         #(partition-advance (unsafe-advance! ret) items idx))
        :else (->PartitionWrap ret items idx)))

(deftype PartitionReducing
  [r :- IReducing, bidx :- Int, eidx :- Int, gap :- Int,
   n :- Int, step :- Int, pad :- (U nil true IRed), ir :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (let [w (strip-reduced wrap)
              ret (.-ret ^dunaj.coll.recipe.PartitionWrap w)
              items (.-items ^dunaj.coll.recipe.PartitionWrap w)
              idx (.-idx ^dunaj.coll.recipe.PartitionWrap w)
              ef #(strip-reduced (._finish ir %))
              rf #(if (nothing? %2) % (._step r % (ef %2)))
              items (if (and items
                             (or (i== idx bidx) (i< idx eidx)))
                      (conj! items nothing)
                      items)]
          (cond
           (or (nil? pad) (nil? items)) ret
           (true? pad) (reduce* (settle! items) rf ret)
           :else
           (let [tn (if (i> n step)
                      (iinc (irem (iadd (isub n step) idx) step))
                      (iinc idx))
                 npad (take tn pad)
                 val (peek items)]
             (if (nothing? val)
               ret
               (let [val (if (reduced? val)
                           val
                           (reduce* npad #(._step ir % %2) val))]
                 (._step r ret (ef val)))))))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (let [items-count (count-nth-num step n)
          items (reduce (fn [ret _] (conj! ret nothing))
                        (sliding-buffer items-count)
                        (range (idec items-count)))]
      (->PartitionWrap (._wrap r ret) items bidx)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.PartitionWrap wrap)))
  ;; TODO: multiple step arities
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.PartitionWrap wrap)
          is (.-items ^dunaj.coll.recipe.PartitionWrap wrap)
          idx (.-idx ^dunaj.coll.recipe.PartitionWrap wrap)
          ef #(strip-reduced (._finish ir %))
          is (if (i== idx bidx) (conj! is (._wrap ir (._init ir))) is)
          nis (if (inneg? idx)
                (iloop [nis is, left (iint (-capacity is))]
                  (if (izero? left)
                    nis
                    (let [nval (peek nis)
                          nval (if (or (nothing? nval)
                                       (reduced? nval))
                                 nval
                                 (._step ir nval val))]
                      (recur (conj! nis nval) (idec left)))))
                is)
          nval (if (i== idx eidx) (peek nis) nothing)
          nret (if (nothing? nval) ret (._step r ret (ef nval)))
          nidx (if (i== idx gap) bidx (idec idx))]
      (partition-advance nret nis nidx))))

(defxform partition*
  "Returns a transducers for partitions, splitting each `_step_`
  steps, each partition having `_n_` items passed in.
  Individual partitions are
  constructed with `_ir_` augmented reducing function. `_pad_`
  may be `nil` (only complete partitions), `true` (returns all
  unfinished partitions) or a collection (pad and return first
  unfinished partition)."
  {:added v1
   :see '[partition lines partition-all partition-by]
   :category "Transducers"}
  [n :- Int, step :- Int, pad :- (U nil true IRed), ir :- IReducing]
  ([r]
   (when-not (pos? n)
     (throw (illegal-argument "n is too small")))
   (when-not (pos? step)
     (throw (illegal-argument "step is too small")))
   (let [bidx (idec (imin step n))
         eidx (if (i> n step)
                (let [x (irem n step)]
                  (if (izero? x) x (isub (imin step n) x)))
                (i0))
         gap (ineg (imax0 (isub step n)))]
     (->PartitionReducing r bidx eidx gap n step pad ir)))
  :unpack false
  :count
  ([tc-fn] #(let [cnt (if (true? pad)
                        %
                        (isub % (if (nil? pad)
                                  (idec n)
                                  (imax0 (isub (idec n) step)))))]
              (tc-fn (count-nth-num step cnt))))
  :section
  ([ts-fn] (when (and tc-fn (nil? pad))
             #(let [l (tc-fn (count %2))
                    ne (iint (prepare-ordered-section %3 %4 l))
                    nb (imul step %3)
                    ne (imin (count %2)
                             (iadd n (imul step (idec ne))))]
                (ts-fn %1 %2 nb ne))))
  :fold false)

(deftype SectionablePartition
  [r :- IReducing, n :- Int, step :- Int, pad :- (U nil true IRed),
   coll :- [], limit :- Int]
  IRed
  (-reduce [this reducef init]
    (let [l (iint (count coll))
          af (advance-fn [ret i :- Int]
               (i<= limit i) ret
               (or (true? pad) (i>= l (iadd i n)))
               (recur (let [val (section coll i (imin l (iadd i n)))]
                        (reducef ret (reduce-augmented r val)))
                      (iadd i step))
               (nil? pad) ret
               :else (let [val (take n (concat (section coll i) pad))]
                       (reducef ret (reduce-augmented r val))))]
      (af init (i0))))
  ICounted
  (-count [this]
    (if (true? pad)
      (count-nth-num step limit)
      (let [x (if (nil? pad) (idec n) (imax0 (isub (idec n) step)))]
        (count-nth-num step (imin limit (isub (count coll) x))))))
  ISectionable
  (-section [this nb ne]
    (let [l (count this)
          ne (iint (prepare-ordered-section nb ne l))]
      (if (and (zero? nb) (i== ne l))
        this
        (->SectionablePartition
         r n step pad (section coll (imul step nb))
         (imul step (isub ne nb))))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef)))

(defn partition
  "Returns a transducers for partitions, splitting each `_step_`
  steps, each partition having `_n_` items passed in. Individual
  partitions are constructed with `_ir_` augmented reducing function.
  `_pad_` may be `nil` (only complete partitions), `true`
  (returns all unfinished partitions) or a collection
  (pad and return first unfinished partition)."
  {:added v1
   :see '[partition* partition-all partition-by lines]
   :transducer true
   :tsig (Fn [Transducer Integer]
             [IRed Integer []]
             [IRed Integer Integer []]
             [IRed Integer Integer (U nil true IRed) []]
             [IRed Integer Integer AnyFn AnyFn []]
             [IRed Integer Integer (U nil true IRed) AnyFn AnyFn []])
   :category "Transducers"}
  ([n] (partition* n n nil (folding concat conj)))
  ([n coll] (partition n n coll))
  ([n step coll] (partition n step nil coll))
  ([n step pad coll] (partition n step pad concat conj coll))
  ([n step icombinef ireducef coll]
   (partition n step nil icombinef ireducef coll))
  ([n step pad icombinef ireducef coll]
   (when-not (pos? n) (throw (illegal-argument "n is too small")))
   (let [step (or step n)
         ir (folding icombinef ireducef)]
     (when-not (pos? step)
       (throw (illegal-argument "step is too small")))
     (if (and (counted? coll) (sectionable? coll))
       (->SectionablePartition ir n step pad coll (count coll))
       (partition* n step pad ir coll)))))

(defn partition-all
  "Returns a transducers for partitions, splitting each `_step_`
  steps, each partition having `_n_` items passed in. Individual
  partitions are constructed with `_ir_` augmented reducing function.
  Returns all unfinished partitions."
  {:added v1
   :see '[partition* partition partition-by lines]
   :transducer true
   :tsig (Fn [Transducer Integer]
             [IRed Integer []]
             [IRed Integer Integer []]
             [IRed Integer Integer AnyFn AnyFn []])
   :category "Transducers"}
  ([n]
   (partition* n n true (folding concat conj)))
  ([n coll]
   (partition-all n n coll))
  ([n step coll]
   (partition-all n step concat conj coll))
  ([n step icombinef ireducef coll]
   (when-not (pos? n) (throw (illegal-argument "n is too small")))
   (let [step (or step n)
         ir (folding icombinef ireducef)]
     (when-not (pos? step)
       (throw (illegal-argument "step is too small")))
     (if (and (counted? coll) (sectionable? coll))
       (->SectionablePartition ir n step true coll (count coll))
       (partition* n step true ir coll)))))

(deftype PBWrap [ret :- Any, before :- Any, before-fval :- Any,
                 after :- Any, after-fval :- Any])

(deftype FoldablePartitionByReducing
  [r :- IReducing, f :- AnyFn, ir :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [ret (.-ret ^dunaj.coll.recipe.PBWrap wrap)
          before (.-before ^dunaj.coll.recipe.PBWrap wrap)
          before-fval (.-before_fval ^dunaj.coll.recipe.PBWrap wrap)
          after (.-after ^dunaj.coll.recipe.PBWrap wrap)
          after-fval (.-after_fval ^dunaj.coll.recipe.PBWrap wrap)
          ifin #(if (nothing? %2) % (._step r % (._finish ir %2)))
          cf #(ifin (._wrap r (._init r)) %)
          ret (if (nothing? before-fval)
                ret
                (._combine r (cf before) ret))
          ret (if (nothing? after-fval)
                ret
                (._combine r ret (cf after)))]
      (->PBWrap (._finish r ret) nothing nothing nothing nothing)))
  (-wrap [this ret]
    (->PBWrap (._wrap r ret) nothing nothing nothing nothing))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.PBWrap wrap)))
  ;; TODO: multiple step arities
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.PBWrap wrap)
          before (.-before ^dunaj.coll.recipe.PBWrap wrap)
          before-fval (.-before_fval ^dunaj.coll.recipe.PBWrap wrap)
          after (.-after ^dunaj.coll.recipe.PBWrap wrap)
          after-fval (.-after_fval ^dunaj.coll.recipe.PBWrap wrap)
          ifin #(if (nothing? %2) % (._step r % (._finish ir %2)))
          new-fval (f val)]
      (if (= after-fval new-fval)
        (let [nafter (._step ir after val)]
          (->PBWrap ret before before-fval nafter after-fval))
        (let [nafter (._step ir (._wrap ir (._init ir)) val)
              nafter-fval new-fval]
          (if (nothing? before-fval)
            (->PBWrap ret after after-fval nafter nafter-fval)
            (->PBWrap (ifin ret after)
                      before before-fval nafter nafter-fval))))))
  (-combine [this wrap other]
    (let [lret (.-ret ^dunaj.coll.recipe.PBWrap wrap)
          lbefore (.-before ^dunaj.coll.recipe.PBWrap wrap)
          lbefore-fval (.-before_fval ^dunaj.coll.recipe.PBWrap wrap)
          lafter (.-after ^dunaj.coll.recipe.PBWrap wrap)
          lafter-fval (.-after_fval ^dunaj.coll.recipe.PBWrap wrap)
          rret (.-ret ^dunaj.coll.recipe.PBWrap other)
          rbefore (.-before ^dunaj.coll.recipe.PBWrap other)
          rbefore-fval (.-before_fval ^dunaj.coll.recipe.PBWrap other)
          rafter (.-after ^dunaj.coll.recipe.PBWrap other)
          rafter-fval (.-after_fval ^dunaj.coll.recipe.PBWrap other)
          ifin #(if (nothing? %2) % (._step r % (._finish ir %2)))
          cf #(ifin (._wrap r (._init r)) %)
          comb3 (fn [r :- IReducing v1 v2 v3]
                  (._combine r v1 (._combine r v2 v3)))
          lchunk? (nothing? lbefore-fval)
          rchunk? (nothing? rbefore-fval)]
      (cond
       ;; merge chunks
       (and lchunk? rchunk?)
       (if (= lafter-fval rafter-fval)
         (if (nothing? lafter-fval)
           ;; both are empty chunks
           (->PBWrap (._combine r lret rret)
                     lbefore lbefore-fval lafter lafter-fval)
           ;; neither chunk is empty
           (->PBWrap (._combine r lret rret) lbefore lbefore-fval
                     (._combine ir lafter rafter) lafter-fval))
         (if (nothing? rafter-fval)
           ;; right chunk is empty
           (->PBWrap (._combine r lret rret)
                     lbefore lbefore-fval lafter lafter-fval)
           ;; create segment
           (->PBWrap (._combine r lret rret)
                     lafter lafter-fval rafter rafter-fval)))
       ;; append chunk to right segment
       lchunk?
       (if (= lafter-fval rbefore-fval)
         ;; same fval
         (->PBWrap (._combine r lret rret)
                   (._combine ir lafter rbefore)
                   rbefore-fval rafter rafter-fval)
         ;; process and set new after
         (if (nothing? lafter-fval)
           ;; but lchunk is empty
           (->PBWrap (._combine r lret rret)
                     rbefore rbefore-fval rafter rafter-fval)
           ;; all is well
           (->PBWrap (comb3 r lret (cf rbefore) rret)
                     lafter lafter-fval rafter rafter-fval)))
       ;; append chunk to left segment
       rchunk?
       (if (= lafter-fval rafter-fval)
         ;; same fval
         (->PBWrap (._combine r lret rret) lbefore lbefore-fval
                   (._combine ir lafter rafter) lafter-fval)
         ;; process and set new after
         (if (nothing? rafter-fval)
           ;; but rchunk is empty
           (->PBWrap (._combine r lret rret)
                     lbefore lbefore-fval lafter lafter-fval)
           ;; all is well
           (->PBWrap (comb3 r lret (cf lafter) rret)
                     lbefore lbefore-fval rafter rafter-fval)))
       ;; merge segments
       :else
       (if (= lafter-fval rbefore-fval)
         ;; merge partials and process
         (->PBWrap
          (comb3 r lret (cf (._combine ir lafter rbefore)) rret)
          lbefore lbefore-fval rafter rafter-fval)
         ;; do not merge parts
         (->PBWrap
          (comb3 r lret (ifin (cf lafter) rbefore) rret)
          lbefore lbefore-fval rafter rafter-fval))))))

(deftype SPBWrap [ret :- Any, fval :- Any, part :- Any])

(defn ^:private spb-advance :- SPBWrap
  [ret :- Any, fval :- Any, part :- Any]
  (cond (reduced? ret)
        (reduced (->SPBWrap @ret fval nothing))
        (postponed? ret)
        (postponed
         (->SPBWrap @ret fval part)
         #(spb-advance (advance ret) fval (clone part))
         #(spb-advance (unsafe-advance! ret) fval part))
        :else (->SPBWrap ret fval part)))

(deftype PartitionByReducing
  [r :- IReducing, f :- AnyFn, ir :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (let [w (strip-reduced wrap)
              ret (.-ret ^dunaj.coll.recipe.SPBWrap w)
              part (.-part ^dunaj.coll.recipe.SPBWrap w)]
          (if (nothing? part)
            ret
            (._step r ret (strip-reduced (._finish ir part)))))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->SPBWrap (._wrap r ret) nothing nothing))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.SPBWrap wrap)))
  ;; TODO: multiple step arities
  (-step [this wrap val]
    ;;(cc/println "step is " val)
    (let [ret (.-ret ^dunaj.coll.recipe.SPBWrap wrap)
          fval (.-fval ^dunaj.coll.recipe.SPBWrap wrap)
          part (.-part ^dunaj.coll.recipe.SPBWrap wrap)
          irf #(if (reduced? %) % (._step ir % %2))
          ifin #(if (nothing? %2)
                  %
                  (._step r % (strip-reduced (._finish ir %2))))
          nfval (f val)]
      (if (= fval nfval)
        (->SPBWrap ret fval (irf part val))
        (spb-advance (ifin ret part)
                     nfval
                     (irf (._wrap ir (._init ir)) val))))))

(defxform foldable-partition-by*
  "Returns a transducer for foldable-only partition-by."
  {:added v1
   :see '[partition-by* partition-by partition lines]
   :category "Transducers"}
  [f :- AnyFn, ir :- IReducing]
  ([r] (->FoldablePartitionByReducing r f ir))
  :count false
  :section false
  :unpack false)

(defxform partition-by*
  "Returns a transducer for reducible-only partition-by."
  {:added v1
   :see '[foldable-partition-by* partition-by partition lines]
   :category "Transducers"}
  [f :- AnyFn, ir :- IReducing]
  ([r] (->PartitionByReducing r f ir))
  :count false
  :section false
  :unpack false
  :fold false)

(deftype PartitionBy
  [coll :- [], f :- AnyFn, ir :- IReducing]
  IRed
  (-reduce [this reducef init]
    (transduce* coll reduce* (partition-by* f ir) reducef init))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (transfold* coll reduce-fn pool n (foldable-partition-by* f ir)
                combinef reducef)))

(deftype SectionablePartitionBy
  [coll :- [], f :- AnyFn, ir :- IReducing]
  IRed
  (-reduce [this reducef init]
    (reduce* (->PartitionBy coll f ir) reducef init))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (let [l (iint (count coll))]
      (cond
       (empty? coll) (combinef)
       (i<= l (iint n)) (reduce-fn this reducef (combinef))
       :else
       (if-let [x (split-adjust #(f (nth coll %)) (idiv l (i2)) l)]
         (let [c1 (->SectionablePartitionBy (section coll 0 x) f ir)
               c2 (->SectionablePartitionBy (section coll x l) f ir)
               fc (fn [child]
                    #(-fold child reduce-fn pool n combinef reducef))]
           (invoke pool #(let [f1 (fc c1)
                               t2 (fork (fc c2))]
                           (combinef (f1) (join t2)))))
         (reduce-fn this reducef (combinef)))))))

(defn partition-by
  "Returns collection of partitions which are split each time `_f_`
  returns new value. Individual partitions are created with
  `_icombinef_` and `_ireducef_` functions."
  {:added v1
   :see '[partition-by* foldable-partition-by* lines]
   :transducer true
   :tsig (Fn [Transducer AnyFn]
             [IRed AnyFn []]
             [IRed AnyFn AnyFn []]
             [IRed AnyFn AnyFn AnyFn []])
   :category "Transducers"}
  ([f] (partition-by* f (folding concat conj)))
  ([f coll] (partition-by f concat conj coll))
  ([f ireducef coll] (partition-by f ireducef ireducef coll))
  ([f icombinef ireducef coll]
   (let [ir (folding icombinef ireducef)]
     (if (and (counted? coll) (sectionable? coll))
       (->SectionablePartitionBy coll f ir)
       (->PartitionBy coll f ir)))))

(defn ^:private newline? :- Boolean
  [i :- Int]
  (or (i== i (iLF)) (i== i (iCR))))

(deftype LinesReducing
  [r :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (let [w (strip-reduced wrap)
              ret (.-ret ^dunaj.coll.recipe.SPBWrap w)
              part (.-part ^dunaj.coll.recipe.SPBWrap w)]
          (if (or (nothing? part) (empty? part))
            ret (._step r ret (settle! part))))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->SPBWrap (._wrap r ret) (i0) (edit empty-string)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.SPBWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.SPBWrap wrap)
          fval (.-fval ^dunaj.coll.recipe.SPBWrap wrap)
          part (.-part ^dunaj.coll.recipe.SPBWrap wrap)
          nl? (newline? (iint val))]
      (cond (not nl?) (->SPBWrap ret fval (conj! part val))
            (and (i== (iint fval) (iCR)) (i== (iint val) (iLF))
                 (empty? part))
            (->SPBWrap ret (i0) part)
            :else (spb-advance (._step r ret (settle! part))
                               val (edit empty-string))))))

(defxform lines*
  "Returns a transducer for reducible-only line splitting."
  []
  ([r] (->LinesReducing r))
  :count false
  :section false
  :unpack false)

(defn ^:private count-nl
  [s]
  (let [cfn (fn [[p l] c]
              (if (and (i== (iint c) (iLF)) (i== (iint p) (iCR)))
                (pair (iint c) l)
                (pair (iint c) (iinc (iint l)))))
        ret (reduce cfn (pair (i0) (i0)) s)]
    (imax (i0) (idec (iint (second ret))))))

(defn foldable-lines
  "Returns a transducer for foldable-only line splitting."
  {:added v1
   :see '[lines]
   :transducer true
   :tsig (Fn [Transducer] [IRed []])
   :category "Transducers"}
  ([]
   (let [r (folding (fn ([] (edit empty-string)) ([x y] (cat! x y)))
                    conj!)
         mf #(cond
               (or (empty? %) (not (newline? (iint (first %)))))
               (tuple (settle! %))
               (and (single? %) (newline? (iint (first %)))) nil
               :else (repeat (count-nl (settle! %)) empty-string))]
     (comp (foldable-partition-by* #(newline? (iint %)) r)
           (mapcat mf))))
  ([coll]
   (recipe (foldable-lines) coll)))

(deftype Lines
  [coll :- []]
  IRed
  (-reduce [this reducef init]
    (transduce* coll reduce* (lines*) reducef init))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (transfold*
     coll reduce* pool n (foldable-lines) combinef reducef)))

(def ^:private lbm :- BatchManager
  (batch-manager (keyword->class :char)))

(def ^:private lam :- ArrayManager
  (array-manager (keyword->class :char)))

(deftype BLWrap
  [ret :- Any, arr-ref :- IReference, fval :- Any, part :- Any])

(defn ^:private bl-advance :- BLWrap
  [ret :- Any, arr-ref :- IReference, fval :- Any, part :- Any]
  (cond (reduced? ret)
        (reduced (->BLWrap @ret nil fval nil))
        (postponed? ret)
        (postponed
         (->BLWrap @ret arr-ref fval part)
         #(bl-advance (advance ret) (clone arr-ref) fval (clone part))
         #(bl-advance (unsafe-advance! ret) arr-ref fval part))
        :else (->BLWrap ret arr-ref fval part)))

(def ^:dynamic ^:private *default-lines-batch-size*
  32)

(defn ^:private provide-capacity! :- nil
  [am :- ArrayManager, arr-ref :- IReference, capacity :- Int]
  (let [capacity (max *default-lines-batch-size* capacity)]
    (when (or (nil? @arr-ref)
              (< (.count am @arr-ref) capacity))
      (reset! arr-ref (.allocate am capacity)))
    nil))

(defn ^:private append-part
  [part :- java.lang.StringBuilder, am :- ArrayManager,
   arr-ref :- IReference, bm :- BatchManager
   batch :- AnyBatch, begin :- Int, end :- Int]
  (let [begin (iint begin)
        end (iint end)]
    (if-not (i< begin end)
      part
      (let [part :- MutableString (or part (edit empty-string))]
        (if (.hasArray batch)
          ;; buffer backed by an array
          (.append part ^chars (.array batch)
                   (iadd begin (.arrayOffset batch))
                   (isub end begin))
          ;; no array backing
          (let [l (isub end begin)
                oldpos (.position batch)]
            (.position batch begin)
            (provide-capacity! am arr-ref l)
            (.copy bm batch @arr-ref (i0) l)
            (.position batch oldpos)
            (.append part ^chars @arr-ref (i0) l)))))))

(deftype BatchedLinesReducing
  [r :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (let [w (strip-reduced wrap)
              ret (.-ret ^dunaj.coll.recipe.BLWrap w)
              part (.-part ^dunaj.coll.recipe.BLWrap w)]
          (if (or (nothing? part) (nil? part))
            ret (._step r ret (settle! part))))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->BLWrap (._wrap r ret) (unsynchronized-reference nil)
              (i0) (edit empty-string)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.BLWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.BLWrap wrap)
          arr-ref (.-arr_ref ^dunaj.coll.recipe.BLWrap wrap)
          part (.-part ^dunaj.coll.recipe.BLWrap wrap)
          fval (.-fval ^dunaj.coll.recipe.BLWrap wrap)
          batch :- AnyBatch val
          af
          (fn af [ret, arr-ref :- IReference, part :- Any,
                 fval :- Int, last-index :- Int, index :- Int]
            (cond
             (reduced? ret)
             (reduced (->BLWrap @ret nil fval nothing))
             (postponed? ret)
             (postponed (->BLWrap @ret nil fval nothing)
                        #(af (advance ret) (clone arr-ref)
                             (clone part) fval last-index index)
                        #(af (unsafe-advance! ret) arr-ref
                             part fval last-index index))
             ;; end of batch, put to the part
             (>= index (.limit batch))
             (->BLWrap ret arr-ref fval
                       (append-part part lam arr-ref lbm batch
                                    last-index (.limit batch)))
             :else
             (let [val (iint (.get lbm batch index))]
               (cond
                (not (newline? val))
                (recur
                 ret arr-ref part fval last-index (iinc index))
                (and (i== (iCR) fval) (i== (iLF) val)
                     (i== index last-index)
                     (empty? part))
                (recur
                 ret arr-ref part (i0) (iinc index) (iinc index))
                :else
                (let [fpart (append-part part lam arr-ref lbm
                                         batch last-index index)
                      fpart (or fpart (edit empty-string))]
                  (recur
                   (._step r ret (settle! fpart)) arr-ref nil val
                   (iinc index) (iinc index)))))))]
      (af ret arr-ref part fval
          (.position batch) (.position batch)))))

(deftype BatchedLines
  [coll :- []]
  IRed
  (-reduce [this reducef init]
    (reduce-augmented*
     coll
     #(reduce-batched*
       (keyword->class :char)
       (provide-batch-size *default-lines-batch-size*)
       % %2 %3)
     (->BatchedLinesReducing (reducing reducef init))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (transfold*
     coll reduce* pool n (foldable-lines) combinef reducef)))

(defn lines
  "Returns a transducer which transform step values into string lines,
  splitting when newline is encountered. Last trailing newline is
  ignored, if present. Recognizes both LF and CR+LF newlines."
  {:added v1
   :see '[foldable-lines partition-by]
   :transducer true
   :tsig (Fn [Transducer] [IRed []])
   :category "Transducers"}
  ([]
   (lines*))
  ([coll]
   (if (and (satisfies? IBatchedRed coll)
            (item-types-match?
             (keyword->class :char) (item-type coll)))
     (->BatchedLines coll)
     (->Lines coll))))

(defn split-at :- []
  "Returns a vector of `[(take _n_ _coll_) (drop _n_ _coll_)]`."
  {:added v1
   :see '[split-with take drop]}
  [n :- Integer, coll :- IRed]
  (pair (take n coll) (drop n coll)))

(defn split-with :- []
  "Returns a vector of
  `[(take-while _pred_ _coll_) (drop-while _pred_ _coll_)]`."
  {:added v1
   :see '[split take-while drop-while]}
  [pred :- AnyFn, coll :- IRed]
  (pair (take-while pred coll) (drop-while pred coll)))


(replace-var! dunaj.coll.util/zip)
(replace-var! dunaj.string/map)
(replace-var! dunaj.string/remove)
(replace-var! dunaj.coll.helper/mapcat)
