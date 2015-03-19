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

(ns dunaj.string
  "Characters and character sequences.

  Character sequences and strings are immutable but not persistent
  collections. Strings main purpose is to convey a textual
  information and not to be a container of characters.
  Because of this, String is not considered a sequential collection
  (sequential? predicate) and is not even equal to other persistent
  collections."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Operations"]}
  (:api bare)
  (:require
   [clojure.core :refer
    [aget . throw ->> -> declare set! first second atom]]
   [clojure.bootstrap :refer [def defn defrecord defalias defmacro
                              v1 not-implemented]]
   [clojure.bridge]
   [dunaj.type :refer [Maybe Any AnyFn Fn U Predicate]]
   [dunaj.boolean :refer [Boolean and or]]
   [dunaj.host :refer [keyword->class class-instance? Array Batch]]
   [dunaj.host.int :refer
    [i0 iadd i< iint isub i== Int iMINUS idec iinc inneg? iloop
     imul i31 iSPACE iHT iLF iCR i<= i32 ineg izero? i2 idiv i-1]]
   [dunaj.host.number :refer [long]]
   [dunaj.math :refer
    [INumerical Integer - integer? nneg? < == >= > <= neg? add dec]]
   [dunaj.compare :refer [IComparable IHash IEquiv IHashBasis not=
                          hash-from-basis hash = nil? identical?]]
   [dunaj.state :refer [IReference reset! ICloneable]]
   [dunaj.flow :refer [if let loop recur doto when cond when-not
                       if-let do delay]]
   [dunaj.threading.last :as ->>]
   [dunaj.poly :refer [satisfies? defprotocol deftype]]
   [dunaj.coll :refer
    [IEmptyable IRed ISeq IEmptyAware ICapped IEditable IFlippable
     IPeekable ICounted ICollectionFactory ISeqable ILookup IIndexed
     ISectionable IReversible IHomogeneous IBatchedRed ISliceable
     IMutableCollection IMutableAssociative IMutableStacked
     ISettleable IMutableCatenable next slice counted? assoc nth get
     reduce empty? count section reduced conj! seq -rest reduced?
     postponed? postponed -reverse -nth edit settle!
     -from-items item-type -from-coll red?]]
   [dunaj.function :refer [IInvocable fn]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable fork join invoke -fold]]
   [dunaj.coll.helper :refer
    [split-adjust strip-reduced fold-sectionable equals-ordered
     equiv-ordered compare-ordered prepare-ordered-section
     reduce-batched* advance-fn]]
   [dunaj.host.batch :refer
    [batch-on batch-manager select-item-type item-types-match?]]
   [dunaj.host.array :refer
    [array-manager char-array acount object-array]]
   [dunaj.char :refer [char char? Char whitespace?]]))


;;;; Implementation details

(def ^:private cha :- java.lang.reflect.Field
  "Makes internal String char array public."
  (doto (.getDeclaredField java.lang.String "value")
    (.setAccessible true)))

(defn ^:private get-cha :- (Array Char)
  "Returns char array from a string."
  [s :- java.lang.String]
  (.get cha s))

(def ^:private sbcha :- java.lang.reflect.Field
  "Makes internal StringBuilder char array public."
  {:added v1}
  (doto (.getDeclaredField java.lang.AbstractStringBuilder "value")
    (.setAccessible true)))

(defn ^:private get-sbcha :- (Array Char)
  "Returns char array from a mutable string"
  {:added v1}
  [s :- java.lang.AbstractStringBuilder]
  (.get sbcha s))


;;;; Public API

(defprotocol ICharSequence
  "An abstract type value protocol for char sequences."
  {:added v1
   :category "Primary"
   :predicate 'char-sequence?
   :see '[String provide-char-sequence]
   :on-interface java.lang.CharSequence
   :forbid-extensions true})

(defn ^:private ensure-char-sequence :- java.lang.String
  "Returns `_x_`, throwing if it is not a char sequence."
  [x :- ICharSequence]
  (when-not (char-sequence? x)
    (throw (java.lang.IllegalArgumentException.
            "Char sequence expected."))))

(defprotocol ICanonical
  "A value protocol for objects other than strings which
  however have a canonical textual representation. Types satisfying
  `ICanonical` protocol must accept their canonical form in their
  constructor.

  Note that while most such types also return canonical
  string with `->str`, it is not always so, see e.g. instants."
  {:added v1
   :see '[canonical]
   :category "Primary"
   :predicate 'canonical?}
  (-canonical :- java.lang.String
    "Returns canonical string representation of `_this_`."
    [this]))

(defn canonical :- (Maybe java.lang.String)
  "Returns canonical string representation of `_x_`. Returns `_x_` if
  it is a string or `nil`."
  {:added v1
   :category "Primary"
   :see '[canonical? ->str]}
  [x :- (U nil ICharSequence ICanonical)]
  (cond (nil? x) nil
        (class-instance? java.lang.String x) x
        (char-sequence? x) (.toString ^java.lang.Object x)
        :else (-canonical x)))

(declare string-section reversed-string-section)

(deftype ReversedStringSection
  "A type for reversed string section."
  [arr :- (Array Char), begin :- Int, end :- Int,
   hash-basis :- IReference, hash-code :- IReference]
  IComparable
  (-compare-to [this other]
    (compare-ordered
     this (ensure-char-sequence (if (nil? other) "" other))))
  IHash
  (-hash [this] (hash-from-basis @hash-basis (isub end begin)))
  IHashBasis
  (-hash-basis [this] @hash-basis)
  IEquiv
  (-equiv [this other]
    (let [other (or other "")]
      (if (char-sequence? other)
        (equiv-ordered this other true)
        false)))
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret :- Any, i :- Int]
               (i< i begin) ret
               :else (recur (reducef ret (aget arr i)) (idec i)))]
      (af init (idec end))))
  ISeqable
  (-seq [this] (when (i< begin end) this))
  ICounted
  (-count [this] (isub end begin))
  IEmptyable
  (-empty [this] "")
  IPeekable
  (-peek [this] (when (i< begin end) (aget arr begin)))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (isub end begin))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (inneg? index) (i< index (isub end begin)))
        (aget arr (isub (idec end) index))
        not-found)))
  IFlippable
  (-flip [this]
    (string-section (java.util.Arrays/copyOfRange arr begin end)
                    (i0) (isub end begin)))
  IReversible
  (-reverse [this] (string-section arr begin end))
  ISliceable
  (-slice [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (iint (prepare-ordered-section nb ne l))]
      (reversed-string-section
       (java.util.Arrays/copyOfRange
        arr (iadd begin (isub l ne)) (isub end nb))
       (i0) (isub ne nb))))
  ISectionable
  (-section [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (iint (prepare-ordered-section nb ne l))]
      (reversed-string-section
       arr (iadd begin (isub l ne)) (isub end nb))))
  IHomogeneous
  (-item-type [this] java.lang.Character/TYPE)
  IInvocable
  (-invoke [this index] (get this index))
  (-invoke [this index not-found] (get this index not-found))
  (-apply [this args]
    (let [cnt (count args)]
      (cond (== 1 cnt) (get this (first args))
            (== 2 cnt) (get this (first args) (second args))
            :else (throw (java.lang.RuntimeException.
                          (clojure.core/str
                           "Wrong number of args (" cnt ")"))))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  ISeq
  (-rest [this]
    (if (i< begin end)
      (reversed-string-section arr begin (idec end))
      clojure.lang.PersistentList/EMPTY))
  ICharSequence

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this i] (nth this i))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))
  clojure.lang.ISeq
  (first [this] (when (i< begin end) (aget arr (idec end))))
  (next [this] (seq (-rest this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] @hash-code)
  (equals [this other]
    (let [other (or other "")]
      (if (char-sequence? other)
        (equals-ordered this other true)
        false)))
  (toString [this] (.toString (java.lang.StringBuilder. this)))
  java.lang.CharSequence
  (charAt [this index] (nth this index))
  (length [this] (isub end begin))
  (subSequence [this begin end] (slice this begin end)))

(deftype FakeReversedCharSequence
  [arr :- (Array Char), begin :- Int, end :- Int]
  java.lang.CharSequence
  (charAt [this index] (aget arr (isub (idec end) index)))
  (length [this] (isub end begin)))

(defn ^:private reversed-string-section :- ReversedStringSection
  "Returns instance of ReversedStringSection with delayed hashes."
  [arr :- (Array Char), begin :- Int, end :- Int]
  (let [begin (iint begin)]
    (->ReversedStringSection
     arr begin end
     (delay
      (clojure.lang.Murmur3/hashUnencodedChars
       (->FakeReversedCharSequence arr begin end)))
     (delay
      (iloop [i (idec (iint end)), ret (i0)]
        (if (i< i begin)
          ret
          (let [v (aget arr i)]
            (recur (idec i) (iadd (iint v) (imul (i31) ret))))))))))

(deftype StringSection
  "A type for string section."
  [arr :- (Array Char), begin :- Int, end :- Int,
   hash-basis :- IReference, hash-code :- IReference]
  IComparable
  (-compare-to [this other]
    (compare-ordered
     this (ensure-char-sequence (if (nil? other) "" other))))
  IHash
  (-hash [this] (hash-from-basis @hash-basis (isub end begin)))
  IHashBasis
  (-hash-basis [this] @hash-basis)
  IEquiv
  (-equiv [this other]
    (let [other (or other "")]
      (if (char-sequence? other)
        (equiv-ordered this other true)
        false)))
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret :- Any, i :- Int]
               (i< i end) (recur (reducef ret (aget arr i)) (iinc i))
               :else ret)]
      (af init begin)))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [type (select-item-type requested-type
                                 java.lang.Character/TYPE)
          bm (batch-manager type)]
      (reducef init (batch-on bm arr begin end))))
  ISeqable
  (-seq [this] (when (i< begin end) this))
  ICounted
  (-count [this] (isub end begin))
  IPeekable
  (-peek [this] (when (i< begin end) (aget arr (idec end))))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (isub end begin))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (inneg? index) (i< index (isub end begin)))
        (aget arr (iadd begin index))
        not-found)))
  IFlippable
  (-flip [this]
    (reversed-string-section
     (java.util.Arrays/copyOfRange arr begin end)
     (i0) (isub end begin)))
  IReversible
  (-reverse [this] (reversed-string-section arr begin end))
  ISliceable
  (-slice [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (isub end begin)))]
      (java.lang.String. arr (iadd begin nb) (isub ne nb))))
  ISectionable
  (-section [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (isub end begin)))]
      (string-section arr (iadd begin nb) (iadd begin ne))))
  IHomogeneous
  (-item-type [this] java.lang.Character/TYPE)
  IInvocable
  (-invoke [this index] (get this index))
  (-invoke [this index not-found] (get this index not-found))
  (-apply [this args]
    (let [cnt (count args)]
      (cond (== 1 cnt) (get this (first args))
            (== 2 cnt) (get this (first args) (second args))
            :else (throw (java.lang.RuntimeException.
                          (clojure.core/str
                           "Wrong number of args (" cnt ")"))))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  ISeq
  (-rest [this]
    (if (i< begin end)
      (string-section arr (iinc begin) end)
      clojure.lang.PersistentList/EMPTY))
  ICharSequence

  ;; Clojure interop
  clojure.lang.ILookup
  (valAt [this k] (get this k))
  (valAt [this k not-found] (get this k not-found))
  clojure.lang.Indexed
  (nth [this i] (nth this i))
  clojure.lang.Reversible
  (rseq [this] (seq (-reverse this)))
  clojure.lang.ISeq
  (first [this] (when (i< begin end) (aget arr begin)))
  (next [this] (seq (-rest this)))

  ;; JVM interop
  java.lang.Object
  (hashCode [this] @hash-code)
  (equals [this other]
    (let [other (or other "")]
      (if (char-sequence? other)
        (equals-ordered this other true)
        false)))
  (toString [this] (slice this 0 nil))
  java.lang.CharSequence
  (charAt [this index] (nth this index))
  (length [this] (isub end begin))
  (subSequence [this begin end] (slice this begin end)))

(deftype FakeCharSequence
  [arr :- (Array Char), begin :- Int, end :- Int]
  java.lang.CharSequence
  (charAt [this index] (aget arr (iadd begin index)))
  (length [this] (isub end begin)))

(defn ^:private string-section :- StringSection
  "Returns instance of StringSection with delayed hashes."
  [arr :- (Array Char), begin :- Int, end :- Int]
  (let [begin (iint begin)]
    (->StringSection
     arr begin end
     (delay (clojure.lang.Murmur3/hashUnencodedChars
             (->FakeCharSequence arr begin end)))
     (delay
      (iloop [i (iint begin), ret (i0)]
        (if (i== i end)
          ret
          (let [v (aget arr i)]
            (recur (iinc i) (iadd (iint v) (imul (i31) ret))))))))))

(deftype String
  "A type for strings."
  {:added v1
   :category "Primary"
   :see '[ICharSequence ->str str empty-string]
   :predicate 'string?}
  java.lang.String
  IComparable
  IRed
  (-reduce [this reducef init]
    (let [arr (get-cha this)
          end (.length this)
          af (advance-fn [ret :- Any, i :- Int]
               (i< i end) (recur (reducef ret (aget arr i)) (iinc i))
               :else ret)]
      (af init (i0))))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [type (select-item-type requested-type
                                 java.lang.Character/TYPE)
          bm (batch-manager type)]
      (reducef
       init (batch-on bm (get-cha this) (i0) (.length this)))))
  ICounted
  (-count [this] (.length this))
  IEmptyAware
  (-empty? [this] (.isEmpty this))
  IEmptyable
  (-empty [this] "")
  IPeekable
  (-peek [this] (-nth this (idec (.length this)) nil))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.length this))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (nneg? index) (i< index (.length this)))
        (.charAt this index)
        not-found)))
  IReversible
  (-reverse [this]
    (reversed-string-section (get-cha this) (i0) (.length this)))
  ISliceable
  (-slice [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (.length this)))]
      (.substring this nb ne)))
  ISectionable
  (-section [this nb ne]
    (let [nb (iint nb)
          ne (iint (prepare-ordered-section nb ne (.length this)))]
      (if (and (izero? nb) (i== ne (.length this)))
        this
        (string-section (get-cha this) nb ne))))
  IHomogeneous
  (-item-type [this] java.lang.Character/TYPE)
  IEditable
  (-edit [this capacity-hint]
    (let [sb (java.lang.StringBuilder. this)]
      (when (and capacity-hint (> capacity-hint (.capacity sb)))
        (.ensureCapacity sb capacity-hint))
      sb))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (fold-sectionable this reduce-fn pool n combinef reducef))

  ;; Abstract types
  ICharSequence)

(def empty-string :- String
  "An empty string."
  {:added v1
   :category "Primary"
   :see '[->str str]}
  "")

(deftype MutableString
  "A type for mutable strings."
  {:added v1
   :see '[String ICharSequence]
   :category "Primary"}
  java.lang.StringBuilder
  IRed
  (-reduce [this reducef init]
    (let [arr (get-sbcha this)
          af (advance-fn [ret :- Any, i :- Int]
               (i< i (.length this))
               (recur (reducef ret (aget arr i)) (iinc i))
               :else ret)]
      (af init (i0))))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [type (select-item-type requested-type
                                 java.lang.Character/TYPE)
          bm (batch-manager type)]
      (reducef
       init (batch-on bm (get-sbcha this) (i0) (.length this)))))
  ICounted
  (-count [this] (.length this))
  ICapped
  (-capacity [this] (.capacity this))
  IPeekable
  (-peek [this] (nth this (idec (.length this)) nil))
  ICloneable
  (-clone [this]
    (let [nb (java.lang.StringBuilder. (.capacity this))]
      (.append nb (get-sbcha this) 0 (.length this))))
  ILookup
  (-contains? [this key]
    (and (integer? key) (nneg? key) (< key (.length this))))
  (-get [this key not-found]
    (if (integer? key) (nth this key not-found) not-found))
  IIndexed
  (-nth [this index not-found]
    (let [index (iint index)]
      (if (and (inneg? index) (i< index (.length this)))
        (.charAt this index)
        not-found)))
  IHomogeneous
  (-item-type [this] java.lang.Character/TYPE)
  ISettleable
  (-settle! [this] (.toString this))
  IMutableCollection
  (-conj! [this x]
    (if (string? x)
      (.append this ^java.lang.String x)
      (.append this ^java.lang.Object x)))
  IMutableAssociative
  (-assoc! [this key val]
    (let [key (iint key)
          l (.length this)]
      (cond (i== key l) (.append this val)
            (i< key l) (.setCharAt this key val)
            :else (throw (java.lang.IndexOutOfBoundsException. key))))
    this)
  IMutableStacked
  (-pop! [this]
    (when (izero? (.length this))
      (throw (java.lang.IllegalStateException.
              "Cannot pop! empty mutable string.")))
    (.deleteCharAt this (idec (.length this))))
  IMutableCatenable
  (-cat! [this other]
    (.append this (get-sbcha other)
             (i0) (.length ^java.lang.StringBuilder other)))

  ;; Abstract type protocols
  ICharSequence)

(defn ^:private cat-batch! :- MutableString
  "Returns the catenation of mutable string `ms` with the
  [`begin`, end) section of a `batch`, using array in `state`
  reference as a intermediate array in cases where `batch` is not
  backed by one. `state` is assumed to be a reference to
  persistent map, holding array under :alt-arr key, if any."
  ([ms :- MutableString, batch :- (Batch Char)]
   (cat-batch! ms batch (.position batch) (.limit batch) (atom {})))
  ([ms :- MutableString, batch :- (Batch Char),
    state :- IReference]
   (cat-batch! ms batch (.position batch) (.limit batch) state))
  ([ms :- MutableString, batch :- (Batch Char),
    begin :- Int, end :- Int, state :- IReference]
   (let [begin (iint begin), end (iint end)]
     (when (i< begin end)
       (if (.hasArray batch)
         ;; buffer backed by an array
         (.append ms ^chars (.array batch)
                  (iadd begin (.arrayOffset batch))
                  (isub end begin))
         ;; no backing array
         (let [l (isub end begin)
               oldpos (.position batch)
               alt-arr :- (Maybe (Array Char))
               (:alt-arr @state)]
           (.position batch begin)
           (when (or (nil? alt-arr) (< (acount alt-arr) l))
             (reset! state (assoc @state :alt-arr (char-array l))))
           (let [arr :- (Array Char) (:alt-arr @state)]
             (.get batch arr (i0) l)
             (.position batch oldpos)
             (.append ms arr (i0) l))))))))

;;; Factory

(defn ^:private sn [x] (if (nil? x) "" x))

(defrecord StringFactory
  "A factory record for strings."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    (cond (string? coll) coll
          (and (satisfies? IBatchedRed coll)
               (item-types-match? (keyword->class :char)
                                  (item-type coll)))
          (let [t (if (counted? coll)
                    (edit empty-string (count coll))
                    (edit empty-string))
                state (atom {})
                rf (fn [ret val] (cat-batch! ret val state))]
            (settle!
             (strip-reduced
              (reduce-batched*
               (keyword->class :char) nil coll rf t))))
          :else (let [t (if (counted? coll)
                          (edit empty-string (count coll))
                          (edit empty-string))]
                  (settle! (reduce #(conj! % (sn %2)) t coll)))))
  (-from-items [factory] empty-string)
  (-from-items [factory a] (java.lang.String/valueOf (sn a)))
  (-from-items [factory a b]
    (-> (edit empty-string) (conj! (sn a)) (conj! (sn b)) settle!))
  (-from-items [factory a b c]
    (-> (edit empty-string) (conj! (sn a)) (conj! (sn b))
        (conj! (sn c)) settle!))
  (-from-items [factory a b c d]
    (-> (edit empty-string) (conj! (sn a)) (conj! (sn b))
        (conj! (sn c)) (conj! (sn d)) settle!))
  (-from-items [factory a b c d more]
    (let [t (-> (edit empty-string) (conj! (sn a)) (conj! (sn b))
                (conj! (sn c)) (conj! (sn d)))]
      (settle! (clojure.core/reduce #(conj! % (sn %2)) t more)))))

(def string-factory :- ICollectionFactory
  "A string factory."
  {:added v1
   :category "Primary"
   :see '[empty-string ->str str]}
  (->StringFactory))

;;; Constructors

(defn str :- String
  "Returns a string with the contents of `_coll_`."
  {:added v1
   :see '[->str provide-char-sequence]
   :category "Primary"}
  [coll :- []]
  (-from-coll string-factory coll))

(defn ->str :- String
  "Returns a string containing given items, if any."
  {:added v1
   :category "Primary"}
  ([] (-from-items string-factory))
  ([a :- Any] (-from-items string-factory a))
  ([a :- Any, b :- Any] (-from-items string-factory a b))
  ([a :- Any, b :- Any, c :- Any]
   (-from-items string-factory a b c))
  ([a :- Any, b :- Any, c :- Any, d :- Any]
   (-from-items string-factory a b c d))
  ([a :- Any, b :- Any, c :- Any, d :- Any & more :- Any]
   (-from-items string-factory a b c d more)))

;;; Operations

(deftype CharSequenceAdapter
  [coll :- IRed, begin :- Int, end :- Int]
  java.lang.CharSequence
  (charAt [this idx] (nth coll (iadd begin (iint idx))))
  (length [this] (isub end begin))
  (subSequence [this nb ne]
    (let [l (isub end begin)
          nb (iint nb)
          ne (iint (prepare-ordered-section nb ne l))]
      (->CharSequenceAdapter coll (iadd begin nb) (iadd begin ne))))
  (toString [this]
    (str (if (and (izero? begin) (i== end (iint (count coll))))
           coll
           (section coll begin end)))))

(defn provide-char-sequence :- ICharSequence
  "Returns adapted `ICharSequence` from a given indexed and
  sectionable `_coll_`. Constant time, without data copying."
  {:added v1
   :category "Primary"
   :see '[char-sequence? ->str str]}
  [coll :- []]
  (cond (nil? coll) ""
        (class-instance? java.lang.CharSequence coll) coll
        :else (->CharSequenceAdapter coll (i0) (iint (count coll)))))

(defalias char->escape-string
  "Returns escape string for char or nil if none."
  {:added v1
   :see '[char->name-string]
   :category "Operations"
   :tsig {Char String}}
  clojure.core/char-escape-string)

(defalias char->name-string
  "Returns name string for char or nil if none."
  {:added v1
   :see '[char->escape-string]
   :category "Operations"
   :tsig {Char String}}
  clojure.core/char-name-string)

(defn blank? :- Boolean
  "Returns `true` if `_s_` is empty or contains only whitespace
  characters, otherwise returns `false`. Optional whitespace
  function predicate `_wf_` can be provided."
  {:added v1
   :see '[dunaj.char/whitespace? trim trim-newline]
   :category "Primary"}
  ([s :- []]
   (reduce #(if (whitespace? %2) %1 (reduced false)) true s))
  ([s :- [], wf :- Predicate]
   (reduce #(if (wf %2) %1 (reduced false)) true s)))

(defn hyphen-case :- String
  "Returns a hyphenated version of a given camel cased `_string_`."
  {:added v1
   :see '[camel-case]
   :category "Operations"}
  [string :- []]
  (when string
    (let [hyphen-fn
          (fn [v c] (cond (nil? v)
                         (edit (->str (dunaj.char/lower-case c)))
                         (dunaj.char/upper-case? c)
                         (-> v
                             (conj! \-)
                             (conj! (dunaj.char/lower-case c)))
                         :else (conj! v c)))
          r (reduce hyphen-fn nil string)]
      (if r (settle! r) empty-string))))

(deftype CamelWrap [ret :- Any, camelize :- Boolean])

(defn camel-case :- String
  "Returns a camel case version of a given hyphen cased `_string_`."
  {:added v1
   :see '[hyphen-case]
   :category "Operations"}
  [string :- []]
  (when string
    (let [camel-fn
          (fn [wrap :- (Maybe CamelWrap), c]
            (if (nil? wrap)
              (->CamelWrap
               (edit (->str (dunaj.char/upper-case c))) false)
              (let [ret (.-ret wrap)
                    camelize? (.-camelize wrap)]
                (cond
                 (i== (iMINUS) (iint c))
                 (->CamelWrap ret true)
                 camelize?
                 (->CamelWrap
                  (conj! ret (dunaj.char/upper-case c)) false)
                 :else (->CamelWrap (conj! ret c) false)))))
          r (reduce camel-fn nil string)]
      (if r
        (settle! (.-ret ^dunaj.string.CamelWrap r))
        empty-string))))

(defn index-of :- (Maybe Integer)
  "Returns first found index of `_x_` inside `_coll_`,
  or returns `nil` if `_coll_` does not contain `_x_`.
  `_x_` may be a character or a collection."
  {:added v1
   :see '[last-index-of]
   :category "Primary"}
  [coll :- [], x :- (U Char IRed)]
  (if (red? x)
    (let [res (.indexOf ^java.lang.String (str coll) (str x))]
      (when (nneg? res) res))
    (let [res
          (if (string? coll)
            (.indexOf ^java.lang.String coll (iint x))
            (let [x (iint x)
                  rf #(if (i== (iint %2) x)
                        (reduced (ineg %))
                        (idec %))]
              (reduce rf (i0) coll)))]
      (when (nneg? res) res))))

(defn last-index-of :- (Maybe Integer)
  "Returns last found index of `_x_` inside `_coll_`,
  or returns `nil` if `_coll_` does not contain `_x_`.
  `_x_` may be a character or a collection."
  {:added v1
   :see '[index-of]
   :category "Primary"}
  [coll :- [], x :- (U Char IRed)]
  (if (red? x)
    (let [res (.lastIndexOf ^java.lang.String (str coll) (str x))]
      (when (nneg? res) res))
    (let [res
          (if (string? coll)
            (.lastIndexOf ^java.lang.String coll (iint x))
            (let [x (iint x)
                  ;; TODO: faster ref or custom wrap
                  r (clojure.core/atom (i-1))
                  rf #(do (when (i== (iint %2) x) (reset! r %))
                          (iinc %))]
              (reduce rf (i0) coll)
              @r))]
      (when (nneg? res) res))))

(deftype StringPartition
  [arr :- (Array Char), partitionf :- AnyFn,
   offset :- Int, count :- Int]
  IRed
  (-reduce [coll reducef init]
    (let [end (iadd offset count)]
      (if (izero? count)
        init
        (let [af (advance-fn
                   [ret :- Any, fval :- Any, i :- Int, ua :- Int]
                   (i== i end)
                   (reducef ret (string-section arr ua i))
                   :else
                   (let [nfval (partitionf (aget arr i))]
                     (if (identical? fval nfval)
                       (recur ret fval (iinc i) ua)
                       (let [val (string-section arr ua i)]
                         (recur (reducef ret val)
                                nfval (iinc i) i)))))]
          (af init (partitionf (aget arr offset)) offset offset)))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (cond
     (izero? count) (combinef)
     (i<= count (iint n)) (reduce-fn this reducef (combinef))
     :else
     (if-let [split (split-adjust #(partitionf (aget arr %))
                                  (iadd offset (idiv count (i2)))
                                  (iadd offset count))]
       (let [c1 (->StringPartition arr partitionf offset
                                   (isub split offset))
             c2 (->StringPartition arr partitionf split
                                   (isub count (isub split offset)))
             fc (fn [child]
                  #(-fold child reduce-fn pool n combinef reducef))]
         (invoke pool #(let [f1 (fc c1)
                             t2 (fork (fc c2))]
                         (combinef (f1) (join t2)))))
       (reduce-fn this reducef (combinef))))))

(defn partition-by :- IRed
  "Applies `_partitionf_` to each value in `_string_`, splitting it
  each time `_partitionf_` returns a new value. Returns a reducible
  and foldable collection of partitions, which are themselves
  reducible, foldable and implements `ICharSequence`.

  IMPORTANT: This version works only on strings."
  {:added v1
   :see '[dunaj.coll.recipe/partition-by split]
   :category "Operations"}
  [partitionf :- AnyFn, string :- String]
  (let [arr (get-cha string)]
    (->StringPartition arr partitionf (i0) (.length string))))

;; injected in dunaj.coll.recipe
(defn ^:private map
  [f coll]
  (not-implemented))

;; injected in dunaj.coll.recipe
(defn ^:private remove
  [f coll]
  (not-implemented))

(defn split :- IRed
  "Returns reducible and foldable collection of splitted strings
  according to `_whitespace-fn_`. Returned collection does not
  contain empty strings. If `_keep-whitespace?_` is `true`
  (defaults to `false`), returned collection will contain
  'whitespace chunks'.
  If `_shared?_` is true (defaults to `false`), returned collection
  contains custom implementation of `ICharSequence` which shares
  data with input `_string_`, instead of containing separate
  String objects."
  {:added v1
   :see '[partition-by]
   :category "Operations"}
  ([string :- String]
   (split whitespace? string))
  ([whitespace-fn :- Predicate, string :- String]
   (split whitespace-fn false string))
  ([whitespace-fn :- Predicate,
    keep-whitespace? :- Boolean, string :- String]
   (split whitespace-fn keep-whitespace? false string))
  ([whitespace-fn :- Predicate,
    keep-whitespace? :- Boolean, shared? :- Boolean, string :- String]
   (let [f (fn [x :- ICharSequence] (whitespace-fn (.charAt x (i0))))]
     (->> (partition-by whitespace-fn string)
          (->>/when-not shared?
            (map (fn [x :- ICharSequence] (.toString x))))
          (->>/when-not keep-whitespace? (remove f))))))

(defn replace :- String
  "Like `clojure.string/replace`, but accepts more coll types."
  {:added v1
   :see '[replace-first]
   :category "Operations"}
  [coll :- IRed,
   match :- (U Char ICharSequence java.util.regex.Pattern),
   replacement :- (U Char ICharSequence AnyFn)]
  (clojure.string/replace
   (provide-char-sequence coll) match replacement))

(defn replace-first :- String
  "Like `clojure.string/replace-first`, but accepts more coll types."
  {:added v1
   :see '[replace]
   :category "Operations"}
  [coll :- IRed,
   match :- (U Char ICharSequence java.util.regex.Pattern),
   replacement :- (U Char ICharSequence AnyFn)]
  (clojure.string/replace-first
   (provide-char-sequence coll) match replacement))

(defn escape :- String
  "Like `clojure.string/escape`, but accepts more coll types."
  {:added v1
   :category "Operations"}
  [coll :- IRed, escape-fn :- AnyFn]
  (clojure.string/escape (provide-char-sequence coll) escape-fn))

(defn trim :- String
  "Removes whitespace from both sides of `_coll_`."
  {:added v1
   :see '[trimr triml trim-newline]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/trim (provide-char-sequence coll)))

(defn triml :- String
  "Removes whitespace from the left side of `_coll_`."
  {:added v1
   :see '[trim trimr trim-newline]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/triml (provide-char-sequence coll)))

(defn trimr :- String
  "Removes whitespace from the right side of `_coll_`."
  {:added v1
   :see '[trim triml trim-newline]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/trimr (provide-char-sequence coll)))

(defn trim-newline :- String
  "Removes all trailing newline (`\\r` or `\\n`) characters from
   `_coll_`."
  {:added v1
   :see '[trim triml trimr]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/trim-newline (provide-char-sequence coll)))

(defn lower-case :- String
  "Returns string containing lower cased contents from `_coll_`."
  {:added v1
   :see '[upper-case capitalize dunaj.char/lower-case]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/lower-case (provide-char-sequence coll)))

(defn upper-case :- String
  "Returns string containing upper cased contents from `_coll_`."
  {:added v1
   :see '[lower-case capitalize dunaj.char/upper-case]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/upper-case (provide-char-sequence coll)))

(defn capitalize :- String
  "Returns string containing contents from `_coll_` with first
  item in uppercase."
  {:added v1
   :see '[lower-case upper-case]
   :category "Operations"}
  [coll :- IRed]
  (clojure.string/capitalize (provide-char-sequence coll)))

(defn binary :- java.lang.String
  "Returns a binary string representation of number `_x_` padded on
  `_n_` bits, which defaults to 8. Supports up to 64 digits."
  {:added v1
   :see '[hexa]
   :category "Operations"}
  ([x :- Integer]
   (binary x 8))
  ([x :- Integer, n :- Integer]
   (when-not (<= 1 n 64)
     (throw (java.lang.IllegalArgumentException.
             "n is out of bounds.")))
   (let [s (->str "%" n "s")
         x (java.lang.Long/toBinaryString x)
         x (java.lang.String/format s (object-array [x]))]
     (.replace ^java.lang.String x " " "0"))))

(defn hexa :- java.lang.String
  "Returns a hexadecimal string representation of number
  `_x_` padded on `_n_` hexadecimal digits, which defaults to 4.
  Supports up to 16 digits."
  {:added v1
   :see '[binary]
   :category "Operations"}
  ([x :- Integer]
   (hexa x 4))
  ([x :- Integer, n :- Integer]
   (when-not (<= 1 n 16)
     (throw (java.lang.IllegalArgumentException.
             "n is out of bounds.")))
   (let [s (->str "0x%0" n "X")
         x (java.lang.String/format s (object-array [(long x)]))]
     x)))

;; TODO: Ideas for more fns
;; - make html or Url formatter which will escape chars
