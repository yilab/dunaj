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

(ns dunaj.host.array
  "Host arrays. Arrays do not follow classic collection API in order
  to enable their performance.

  Array is an abstraction for host array, a low level host specific
  container of objects. Depending on the use case, you
  can work directly with array, utilizing array manager, or wrap
  it into a reducible collection.

  Simplest way is to wrap array into the `ArrayColl` by calling
  `adapt`. In cases where this is not possible or you may want
  better performance, you have following options:

  * `aXXX` functions provide inline variants that get rid of boxing
    and also reflection, if you provide a proper type signature,
    or choose function with right suffix.
  * If you do not know the type of array but have array's manager,
    use it. It is fast but there will be some boxing."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary"
                ["Primitives"
                 "Functions specific to host primitive types."]]}
  (:api bare)
  (:require
   [clojure.core :refer
    [str declare get first second throw every? interpose pr-str map]]
   [clojure.bootstrap :refer [deftype defn defalias def v1]]
   [dunaj.type :refer [Fn Va U Any Maybe Predicate Macro]]
   [dunaj.boolean :refer [Boolean and or not]]
   [dunaj.host :refer [Class ArrayManager AnyArray Array
                       class-instance? class provide-class set!]]
   [dunaj.host.int :refer
    [iint iinc idec isub Int imul i== iadd i0 i31 iloop i1 i< inneg?]]
   [dunaj.math :refer [nneg? integer? < >= Integer == > zero?]]
   [dunaj.flow :refer [delay when-not let loop when if cond recur]]
   [dunaj.compare :refer
    [IComparable IHash IEquiv IHashBasis nil? =
     hash-from-basis next-basis basis-seed ordered-hash-factory]]
   [dunaj.state :refer [IReference]]
   [dunaj.poly :refer [Type]]
   [dunaj.coll :refer
    [IRed ICounted ISectionable IReversible ISeq
     IFlippable ISliceable ISequential ILookup IIndexed IHomogeneous
     IBatchedRed IPeekable ISeqable postponed? count reduced? -rest
     postponed nth -reverse seq -contains? -section]]
   [dunaj.function :refer [IInvocable fn apply]]
   [dunaj.concurrent.forkjoin :refer [IFoldable]]
   [dunaj.coll.helper :refer
    [equiv-ordered equals-ordered prepare-ordered-section index-of
     compare-ordered fold-sectionable coll->iterator
     coll->list-iterator advance-fn]]
   [dunaj.host.batch :refer
    [batch-manager select-item-type batch-on batch-support?]]))


;;;; Implementation details

(def ^:private ams :- {Class ArrayManager} @#'dunaj.host/ams)

(defn ^:private array?* :- Predicate
  [arrf :- Any]
  (let [arr-class :- Class (class (arrf 0))]
    (fn f :- Boolean [x :- Any] (class-instance? arr-class x))))


;;;; Public API

(defalias make-array
  "Returns a nil/zero-filled host array of specified `_type_` Class
  and either `_len_` length, or given dimensions."
  {:added v1
   :category "Primary"
   :see '[to-array array]
   :tsig (Fn [AnyArray Class Integer]
             [AnyArray Class Integer (Va Integer)])})

(defalias to-array
  "Returns host object array and fills it with contents of `_coll_`,
  which must be an immutable collection, array or a host collection."
  {:added v1
   :category "Primary"
   :see '[to-array-2d make-array array]
   :tsig (Fn [AnyArray IRed])})

(defalias to-array-2d
  "Returns host object array and fills it with object arrays created
  from items of `_coll_`. The items in `_coll_` must be immutable
  collections, arrays or a host collections."
  {:added v1
   :category "Primary"
   :see '[to-array]
   :tsig (Fn [AnyArray IRed])})

(defalias array
  "Returns an array with components set to the values in `_aseq_`.
  The array's component type is `_type_` if provided, or the type
  of the first value in `_aseq_` if present, or `Object`.
  All values in `_aseq_` must be compatible with the component type."
  {:added v1
   :category "Primary"
   :see '[make-array to-array]
   :tsig (Fn [AnyArray IRed]
             [AnyArray Class IRed])}
  clojure.core/into-array)

;;; Faster constructors if array type is known at compile time

(defalias boolean-array
  "Creates an array of booleans."
  {:added v1
   :category "Primitives"
   :see '[booleans boolean-array? array make-array]
   :tsig (Fn [(Array java.lang.Boolean) (U Integer IRed)]
             [(Array java.lang.Boolean) Integer Any])})

(defalias byte-array
  "Creates an array of bytes."
  {:added v1
   :category "Primitives"
   :see '[bytes byte-array? array make-array]
   :tsig (Fn [(Array java.lang.Byte) (U Integer IRed)]
             [(Array java.lang.Byte) Integer Any])})

(defalias char-array
  "Creates an array of chars."
  {:added v1
   :category "Primitives"
   :see '[chars char-array? array make-array]
   :tsig (Fn [(Array java.lang.Character) (U Integer IRed)]
             [(Array java.lang.Character) Integer Any])})

(defalias double-array
  "Creates an array of doubles."
  {:added v1
   :category "Primitives"
   :see '[doubles double-array? array make-array]
   :tsig (Fn [(Array java.lang.Double) (U Integer IRed)]
             [(Array java.lang.Double) Integer Any])})

(defalias float-array
  "Creates an array of floats."
  {:added v1
   :category "Primitives"
   :see '[floats float-array? array make-array]
   :tsig (Fn [(Array java.lang.Float) (U Integer IRed)]
             [(Array java.lang.Float) Integer Any])})

(defalias int-array
  "Creates an array of ints."
  {:added v1
   :category "Primitives"
   :see '[ints int-array? array make-array]
   :tsig (Fn [(Array java.lang.Integer) (U Integer IRed)]
             [(Array java.lang.Integer) Integer Any])})

(defalias long-array
  "Creates an array of longs."
  {:added v1
   :category "Primitives"
   :see '[longs long-array? array make-array]
   :tsig (Fn [(Array java.lang.Long) (U Integer IRed)]
             [(Array java.lang.Long) Integer Any])})

(defalias object-array
  "Creates an array of objects."
  {:added v1
   :category "Primary"
   :see '[object-array? to-array array make-array]
   :tsig (Fn [(Array java.lang.Object) (U Integer IRed)])})

(defalias short-array
  "Creates an array of shorts."
  {:added v1
   :category "Primitives"
   :see '[shorts short-array? array make-array]
   :tsig (Fn [(Array java.lang.Short) (U Integer IRed)]
             [(Array java.lang.Short) Integer Any])})

;;; Coercion to primitive array

(defalias booleans
  "Cast to boolean[]"
  {:added v1
   :category "Primitives"
   :see '[boolean-array boolean-array?]
   :tsig (Fn [(Array java.lang.Boolean) Any])})

(defalias bytes
  "Cast to byte[]"
  {:added v1
   :category "Primitives"
   :see '[byte-array byte-array?]
   :tsig (Fn [(Array java.lang.Byte) Any])})

(defalias chars
  "Cast to char[]"
  {:added v1
   :category "Primitives"
   :see '[char-array char-array?]
   :tsig (Fn [(Array java.lang.Character) Any])})

(defalias doubles
  "Cast to double[]"
  {:added v1
   :category "Primitives"
   :see '[double-array double-array?]
   :tsig (Fn [(Array java.lang.Double) Any])})

(defalias floats
  "Cast to float[]"
  {:added v1
   :category "Primitives"
   :see '[float-array float-array?]
   :tsig (Fn [(Array java.lang.Float) Any])})

(defalias ints
  "Cast to int[]"
  {:added v1
   :category "Primitives"
   :see '[int-array int-array?]
   :tsig (Fn [(Array java.lang.Integer) Any])})

(defalias longs
  "Cast to long[]"
  {:added v1
   :category "Primitives"
   :see '[long-array long-array?]
   :tsig (Fn [(Array java.lang.Long) Any])})

(defalias shorts
  "Cast to short[]"
  {:added v1
   :category "Primitives"
   :see '[short-array short-array?]
   :tsig (Fn [(Array java.lang.Short) Any])})

;;; Array predicates

(defn array? :- Boolean
  "Returns `true` if `_x_` is an array, `false` otherwise."
  {:added v1
   :category "Primary"
   :see '[object-array? byte-array?]}
  [x :- Any]
  (.isArray (class x)))

(def byte-array? :- Predicate
  "Returns `true` if `_x_` is a byte array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[bytes byte-array]}
  (array?* byte-array))

(def short-array? :- Predicate
  "Returns `true` if `_x_` is a short array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[shorts short-array]}
  (array?* short-array))

(def int-array? :- Predicate
  "Returns `true` if `_x_` is an array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[ints int-array]}
  (array?* int-array))

(def long-array? :- Predicate
  "Returns `true` if `_x_` is a long array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[longs long-array]}
  (array?* long-array))

(def float-array? :- Predicate
  "Returns `true` if `_x_` is a float array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[floats float-array]}
  (array?* float-array))

(def double-array? :- Predicate
  "Returns `true` if `_x_` is a double array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[doubles double-array]}
  (array?* double-array))

(def boolean-array? :- Predicate
  "Returns `true` if `_x_` is a boolean array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[booleans boolean-array]}
  (array?* boolean-array))

(def char-array? :- Predicate
  "Returns `true` if `_x_` is a char array, `false` otherwise."
  {:added v1
   :category "Primitives"
   :arglists '([x])
   :see '[chars char-array]}
  (array?* char-array))

(def object-array? :- Predicate
  "Returns `true` if `_x_` is an object array, `false` otherwise."
  {:added v1
   :category "Primary"
   :arglists '([x])
   :see '[object-array]}
  (array?* object-array))

;;; Mutate array

(defalias aset!
  "Sets the value `_val_` at the index/indices.
  Works on host arrays of reference types. Returns `_val_`."
  {:added v1
   :category "Primary"
   :see '[object-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset)

;; TODO: add inline to support primitives
(defalias aset-boolean!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of boolean. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[boolean-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-boolean)

;; TODO: add inline to support primitives
(defalias aset-byte!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of byte. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[byte-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-byte)

;; TODO: add inline to support primitives
(defalias aset-char!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of char. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[char-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-char)

;; TODO: add inline to support primitives
(defalias aset-double!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of double. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[double-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-double)

;; TODO: add inline to support primitives
(defalias aset-float!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of float. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[float-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-float)

;; TODO: add inline to support primitives
(defalias aset-int!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of int. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[int-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-int)

;; TODO: add inline to support primitives
(defalias aset-long!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of long. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[long-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-long)

;; TODO: add inline to support primitives
(defalias aset-short!
  "Sets the value `_val_` at the index/indices.
  Works on arrays of short. Returns `_val_`."
  {:added v1
   :category "Primitives"
   :see '[short-array? aget]
   :tsig (Fn [Any AnyArray Integer Any]
             [Any AnyArray Integer Integer (Va Any)])}
  clojure.core/aset-short)

;;; Operations

(defalias acount
  "Returns the length of the host array.
  Works on arrays of all types."
  {:added v1
   :category "Primary"
   :see '[aget dunaj.coll/count]
   :tsig (Fn [Int AnyArray])}
  clojure.core/alength)

(defalias aget
  "Returns the value at the index/indices.
  Works on host arrays of all types."
  {:added v1
   :category "Primary"
   :see '[aset! acount dunaj.coll/get]
   :tsig (Fn [Any AnyArray Int]
             [Any AnyArray Int (Va Int)])}
  clojure.core/aget)

(defalias amap
  "Maps an expression `_expr_` across an array `_a_`, using an index
  named `_idx_`, and return value named `_ret_`, initialized to a
  clone of `_a_`, then setting each item of `_ret_` to the
  evaluation of `_expr_`, returning the new array `_ret_`."
  {:added v1
   :category "Primary"
   :see '[areduce aget acount dunaj.coll.recipe/map]
   :tsig Macro}
  clojure.core/amap)

(defalias areduce
  "Reduces an expression across an array `_a_`, using an index named
  `_idx_`, and return value named `_ret_`, initialized to `_init_`,
  setting `_ret_` to the evaluation of `_expr_` at each step,
  returning `_ret_`."
  {:added v1
   :category "Primary"
   :see '[amap aget acount dunaj.coll/reduce]
   :tsig Macro}
  clojure.core/areduce)

(defalias aclone
  "Returns a clone of the host array. Works on arrays of known types."
  {:added v1
   :category "Primary"
   :see '[dunaj.state/clone]
   :tsig (Fn [AnyArray AnyArray])}
  clojure.core/aclone)

(defn array-item-type :- Class
  "Returns array's item type."
  {:added v1
   :category "Primary"
   :see '[dunaj.coll/item-type]}
  [arr :- AnyArray]
  (.getComponentType (class arr)))

;;; Array manager

(defn array-manager :- (Maybe ArrayManager)
  "Returns array manager for a given type, or `nil`
  if given type is not supported."
  {:added v1
   :category "Primary"
   :see '[array-manager-from]}
  [item-type :- (U nil Class Type)]
  (get ams (provide-class item-type)))

(defn array-manager-from :- (Maybe ArrayManager)
  "Returns array manager from an existing array `arr`."
  {:added v1
   :category "Primary"
   :see '[array-manager]}
  [arr :- AnyArray]
  (cond (byte-array? arr) (array-manager java.lang.Byte/TYPE)
        (char-array? arr) (array-manager java.lang.Character/TYPE)
        (object-array? arr) (array-manager java.lang.Object)
        (int-array? arr) (array-manager java.lang.Integer/TYPE)
        (long-array? arr) (array-manager java.lang.Long/TYPE)
        (float-array? arr) (array-manager java.lang.Float/TYPE)
        (double-array? arr) (array-manager java.lang.Double/TYPE)
        (short-array? arr) (array-manager java.lang.Short/TYPE)
        (boolean-array? arr) (array-manager java.lang.Boolean/TYPE)))

;;; Array wrapper

(declare reversed-array-coll array-coll)

(deftype ReversedArrayColl
  "A type for reversed array coll."
  [am :- ArrayManager, arr :- AnyArray
   begin :- Int, end :- Int,
   hash-basis :- IReference, hash-code :- IReference]
  IComparable
  (-compare-to [this other] (compare-ordered this other))
  IHash
  (-hash [this] (hash-from-basis @hash-basis (isub end begin)))
  IHashBasis
  (-hash-basis [this] @hash-basis)
  IEquiv
  (-equiv [this other] (equiv-ordered this other))
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret :- Any, i :- Int]
               (i< i begin) ret
               :else (recur (reducef ret (.get am arr i)) (idec i)))]
      (af init (idec end))))
  ISeqable
  (-seq [this] (when (i< begin end) this))
  ICounted
  (-count [this] (isub end begin))
  IPeekable
  (-peek [this] (when (i< begin end) (.get am arr begin)))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (isub end begin))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (inneg? index) (i< index (isub end begin)))
        (.get am arr (isub (idec end) index))
        not-found)))
  IFlippable
  (-flip [this]
    (array-coll am (.duplicate am arr begin end)
                (i0) (isub end begin)))
  IReversible
  (-reverse [this] (array-coll am arr begin end))
  ISliceable
  (-slice [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (iint (prepare-ordered-section nb ne l))]
      (reversed-array-coll
       am (.duplicate am arr (iadd begin (isub l ne)) (isub end nb))
       (i0) (isub ne nb))))
  ISectionable
  (-section [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (iint (prepare-ordered-section nb ne l))]
      (reversed-array-coll
       am arr (iadd begin (isub l ne)) (isub end nb))))
  IHomogeneous
  (-item-type [this] (.itemType am))
  ISequential
  IInvocable
  (-invoke [this index] (nth this index))
  (-invoke [this index not-found] (nth this index not-found))
  (-apply [this args]
    (let [cnt (count args)]
      (cond (== 1 cnt) (nth this (first args))
            (== 2 cnt) (nth this (first args) (second args))
            :else (throw (java.lang.RuntimeException.
                          (str "Wrong number of args (" cnt ")"))))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  ISeq
  (-rest [this]
    (if (i< begin end)
      (reversed-array-coll am arr begin (idec end))
      clojure.lang.PersistentList/EMPTY))

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this i] (nth this i))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))
  clojure.lang.ISeq
  (first [this] (when (i< begin end) (.get am arr (idec end))))
  (next [this] (seq (-rest this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] @hash-code)
  (equals [this other] (equals-ordered this other))
  (toString [this]
    (str "[" (apply str (interpose " " (map pr-str (seq this)))) "]"))
  java.util.RandomAccess
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  java.util.Collection
  (contains [this x] (-contains? this x))
  (containsAll [this c] (every? #(-contains? this %) (seq c)))
  (isEmpty [this] (i== begin end))
  (size [this] (isub end begin))
  (toArray [this] (clojure.lang.RT/seqToArray (seq this)))
  (toArray [this a] (clojure.lang.RT/seqToPassedArray (seq this) a))
  java.util.List
  (get [this index] (.nth this index))
  (indexOf [this x] (index-of this x))
  (lastIndexOf [this x]
    (isub (isub end begin) (index-of (-reverse this) x)))
  (listIterator [this] (.listIterator this 0))
  (listIterator [this i] (coll->list-iterator this i))
  (subList [this begin end] (-section this begin end)))

(defn ^:private reversed-array-coll :- ReversedArrayColl
  "Returns instance of ReversedArrayColl with delayed hashes."
  [am :- ArrayManager, arr :- AnyArray, begin :- Int, end :- Int]
  (let [begin (iint begin)]
    (->ReversedArrayColl
     am arr begin end
     (delay
      (iloop [i (idec (iint end))
              ret (basis-seed ordered-hash-factory)]
        (if (i< i begin)
          ret
          (recur
           (idec i)
           (next-basis ordered-hash-factory ret (.get am arr i))))))
     (delay
      (iloop [i (idec (iint end)), ret (i1)]
        (if (i< i begin)
          ret
          (let [v (.get am arr i)]
            (recur
             (idec i)
             (iadd (if (nil? v) (i0) (.hashCode ^java.lang.Object v))
                   (imul (i31) ret))))))))))

(deftype BatchableArrayColl
  "A type for array coll."
  [am :- ArrayManager, arr :- AnyArray
   begin :- Int, end :- Int,
   hash-basis :- IReference, hash-code :- IReference]
  IComparable
  (-compare-to [this other] (compare-ordered this other))
  IHash
  (-hash [this] (hash-from-basis @hash-basis (isub end begin)))
  IHashBasis
  (-hash-basis [this] @hash-basis)
  IEquiv
  (-equiv [this other] (equiv-ordered this other))
  IRed
  (-reduce [this reducef init]
    (let [af
          (advance-fn [ret :- Any, i :- Int]
            (i< i end) (recur (reducef ret (.get am arr i)) (iinc i))
            :else ret)]
      (af init begin)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [type (select-item-type requested-type (.itemType am))
          bm (batch-manager type)]
      (reducef init (batch-on bm arr begin end))))
  ISeqable
  (-seq [this] (when (i< begin end) this))
  ICounted
  (-count [this] (isub end begin))
  IPeekable
  (-peek [this] (when (i< begin end) (.get am arr (idec end))))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (isub end begin))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (inneg? index) (i< index (isub end begin)))
        (.get am arr (iadd begin index))
        not-found)))
  IFlippable
  (-flip [this]
    (reversed-array-coll
     am (.duplicate am arr begin end) (i0) (isub end begin)))
  IReversible
  (-reverse [this] (reversed-array-coll am arr begin end))
  ISliceable
  (-slice [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (isub end begin)))]
      (array-coll
       am (.duplicate am arr (iadd begin nb) (iadd begin ne))
       (i0) (isub ne nb))))
  ISectionable
  (-section [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (isub end begin)))]
      (array-coll am arr (iadd begin nb) (iadd begin ne))))
  IHomogeneous
  (-item-type [this] (.itemType am))
  ISequential
  IInvocable
  (-invoke [this index] (nth this index))
  (-invoke [this index not-found] (nth this index not-found))
  (-apply [this args]
    (let [cnt (count args)]
      (cond (== 1 cnt) (nth this (first args))
            (== 2 cnt) (nth this (first args) (second args))
            :else (throw (java.lang.RuntimeException.
                          (str "Wrong number of args (" cnt ")"))))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  ISeq
  (-rest [this]
    (if (i< begin end)
      (array-coll am arr (iinc begin) end)
      clojure.lang.PersistentList/EMPTY))

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this i] (nth this i))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))
  clojure.lang.ISeq
  (first [this] (when (i< begin end) (.get am arr begin)))
  (next [this] (seq (-rest this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] @hash-code)
  (equals [this other] (equals-ordered this other))
  (toString [this]
    (str "[" (apply str (interpose " " (map pr-str (seq this)))) "]"))
  java.util.RandomAccess
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  java.util.Collection
  (contains [this x] (-contains? this x))
  (containsAll [this c] (every? #(-contains? this %) (seq c)))
  (isEmpty [this] (i== begin end))
  (size [this] (isub end begin))
  (toArray [this] (.duplicate am arr begin end))
  (toArray [this a] (clojure.lang.RT/seqToPassedArray (seq this) a))
  java.util.List
  (get [this index] (.nth this index))
  (indexOf [this x] (index-of this x))
  (lastIndexOf [this x]
    (isub (isub end begin) (index-of (-reverse this) x)))
  (listIterator [this] (.listIterator this 0))
  (listIterator [this i] (coll->list-iterator this i))
  (subList [this begin end] (-section this begin end)))

(deftype ArrayColl
  "A type for array coll."
  [am :- ArrayManager, arr :- AnyArray
   begin :- Int, end :- Int,
   hash-basis :- IReference, hash-code :- IReference]
  IComparable
  (-compare-to [this other] (compare-ordered this other))
  IHash
  (-hash [this] (hash-from-basis @hash-basis (isub end begin)))
  IHashBasis
  (-hash-basis [this] @hash-basis)
  IEquiv
  (-equiv [this other] (equiv-ordered this other))
  IRed
  (-reduce [this reducef init]
    (let [af
          (advance-fn [ret :- Any, i :- Int]
            (i< i end) (recur (reducef ret (.get am arr i)) (iinc i))
            :else ret)]
      (af init begin)))
  ISeqable
  (-seq [this] (when (i< begin end) this))
  ICounted
  (-count [this] (isub end begin))
  IPeekable
  (-peek [this] (when (i< begin end) (.get am arr (idec end))))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (isub end begin))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (inneg? index) (i< index (isub end begin)))
        (.get am arr (iadd begin index))
        not-found)))
  IFlippable
  (-flip [this]
    (reversed-array-coll am (.duplicate am arr begin end)
                         (i0) (isub end begin)))
  IReversible
  (-reverse [this] (reversed-array-coll am arr begin end))
  ISliceable
  (-slice [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (isub end begin)))]
      (array-coll
       am (.duplicate am arr (iadd begin nb) (iadd begin ne))
       (i0) (isub ne nb))))
  ISectionable
  (-section [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (isub end begin)))]
      (array-coll am arr (iadd begin nb) (iadd begin ne))))
  ISequential
  IInvocable
  (-invoke [this index] (nth this index))
  (-invoke [this index not-found] (nth this index not-found))
  (-apply [this args]
    (let [cnt (count args)]
      (cond (== 1 cnt) (nth this (first args))
            (== 2 cnt) (nth this (first args) (second args))
            :else (throw (java.lang.RuntimeException.
                          (str "Wrong number of args (" cnt ")"))))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  ISeq
  (-rest [this]
    (if (i< begin end)
      (array-coll am arr (iinc begin) end)
      clojure.lang.PersistentList/EMPTY))

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this i] (nth this i))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))
  clojure.lang.ISeq
  (first [this] (when (i< begin end) (.get am arr begin)))
  (next [this] (seq (-rest this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] @hash-code)
  (equals [this other] (equals-ordered this other))
  (toString [this]
    (str "[" (apply str (interpose " " (map pr-str (seq this)))) "]"))
  java.util.RandomAccess
  java.lang.Iterable
  (iterator [this] (coll->iterator this))
  java.util.Collection
  (contains [this x] (-contains? this x))
  (containsAll [this c] (every? #(-contains? this %) (seq c)))
  (isEmpty [this] (i== begin end))
  (size [this] (isub end begin))
  (toArray [this] (.duplicate am arr begin end))
  (toArray [this a] (clojure.lang.RT/seqToPassedArray (seq this) a))
  java.util.List
  (get [this index] (.nth this index))
  (indexOf [this x] (index-of this x))
  (lastIndexOf [this x]
    (isub (isub end begin) (index-of (-reverse this) x)))
  (listIterator [this] (.listIterator this 0))
  (listIterator [this i] (coll->list-iterator this i))
  (subList [this begin end] (-section this begin end)))

(defn ^:private array-coll :- (U ArrayColl BatchableArrayColl)
  "Returns instance of ArrayColl with delayed hashes."
  [am :- ArrayManager, arr :- AnyArray, begin :- Int, end :- Int]
  (let [end (iint end)]
    ((if (batch-support? (.itemType am))
       ->BatchableArrayColl
       ->ArrayColl)
     am arr begin end
     (delay
      (iloop [i (iint begin), ret (basis-seed ordered-hash-factory)]
        (if (i== i end)
          ret
          (recur
           (iinc i)
           (next-basis ordered-hash-factory ret (.get am arr i))))))
     (delay
      (iloop [i (iint begin), ret (i1)]
        (if (i== i end)
          ret
          (let [v (.get am arr i)]
            (recur
             (iinc i)
             (iadd (if (nil? v) (i0) (.hashCode ^java.lang.Object v))
                   (imul (i31) ret))))))))))

(defn adapt :- (U ArrayColl BatchableArrayColl)
  "Returns an immutable collection which wraps part of a
  given `_arr_`-ay. Does not copy array. Assumes user has
  control over the array and its contents won't change."
  {:added v1
   :category "Primary"
   :see '[areduce amap acount aget aset! array-manager]}
  ([arr :- AnyArray]
   (let [am (array-manager-from arr)]
     (adapt am arr (i0) (.count am arr))))
  ([am :- ArrayManager, arr :- AnyArray]
   (adapt am arr (i0) (.count am arr)))
  ([arr :- AnyArray, begin :- Int, end :- Int]
   (adapt (array-manager-from arr) arr begin end))
  ([am :- ArrayManager, arr :- AnyArray, begin :- Int, end :- Int]
   (array-coll am arr begin end)))
