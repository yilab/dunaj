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

(ns dunaj.format.parser
  "Fast parser engine with helper functions for tokenizer and parser
  machines."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Lazy"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Fn Maybe Any U I AnyFn Predicate]]
   [dunaj.boolean :refer [Boolean boolean or not and]]
   [dunaj.host :refer
    [Class AnyBatch BatchManager keyword->class set!]]
   [dunaj.host.int :refer
    [Int iint iinc i== i< isub izero? idec ineg? i> i< i<< imin iadd
     iloop i0 i1 i2 i3 i4 i5 i8 ixFF ione? i-1 imul idigit? ioctal?
     ihexa? ihexa->int idigit->int iHT iCR iLF iCOLON iSPACE iCOMMA
     i<= iQUOTE iAPOS iBACKSLASH ipos? iSMALL_B iSMALL_F iSMALL_N
     iSMALL_R iSMALL_U iSMALL_T]]
   [dunaj.math :refer [Integer neg? pos? <]]
   [dunaj.threading :refer [->> ->]]
   [dunaj.compare :refer [identical? nil? defsentinel]]
   [dunaj.state :refer [reset! IReference clone]]
   [dunaj.flow :refer
    [if let when cond when-not loop recur do if-not if-let]]
   [dunaj.feature :refer [IConfig assoc-meta]]
   [dunaj.poly :refer [Type satisfies? defrecord defprotocol deftype]]
   [dunaj.coll :refer
    [reduced reduce IRed IHomogeneous reduced? postponed postponed?
     advance unsafe-advance! count IBatchedRed first rest empty? next
     ISeq ISequential ISeqable seq second conj! settle! edit conj
     IPersistentList IReducing pop peek item-type reducing
     Transducer unsafe-postponed]]
   [dunaj.coll.helper :refer
    [advance-fn finish-advance defxform reduce-augmented* reduce*
     reduce-batched* reduced-advance strip-reduced]]
   [dunaj.function :refer [fn defn apply complement identity comp]]
   [dunaj.host.array :refer [array aget acount array-manager]]
   [dunaj.host.batch :refer
    [batch-manager select-item-type item-types-match? batch]]
   [dunaj.char :refer [char]]
   [dunaj.string :refer [String ->str str empty-string camel-case]]
   [dunaj.identifier :refer [INamed name symbol]]
   [dunaj.macro :refer [defmacro gensym]]
   [dunaj.error :refer [throw ex-info illegal-state]]
   [dunaj.state.basic :refer [unsynchronized-reference]]
   [dunaj.state.var :refer [Var var def]]
   [dunaj.coll.recipe :refer [partition throwing-cap map take-while]]
   [dunaj.coll.lazy-seq :refer [lazy-seq]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.default :refer [empty-seq ->lst]]
   [dunaj.coll.tuple :as ct :refer [tuple pair]]
   [dunaj.format :refer [default-formatter-batch-size]]
   [dunaj.format.helper :refer [prepend-unread string-cat-batch!]]))


;;;; Implementation details

(defsentinel nothing)

(def ^:private ^:dynamic *max-lazy-level-limit* 200)


;;;; Public API

(defprotocol ITokenizerMachine
  "A protocol for tokenizer machine. Is used in a parser engine to
  perform a conversion of input collection (of e.g. chars, bytes),
  into the collection of tokens. Parsers should implement tokenizer
  machines for tokens consisting of multiple input values."
  {:added v1
   :category "Primary"
   :predicate 'tokenizer-machine?
   :forbid-extensions true}
  (-analyze-batch! :- Any
    "Returns the result of analyzing input `_batch_` managed by `_bm_`
    batch manager, setting the position of `_batch_` to the next
    not yet analyzed item, or to the batch's current limit.
    Result is either a tokenizer machine (analysis not yet finished)
    or a token, if the analysis is finished.

    Use `leftover` function to return leftover batch alongside token,
    if items outside current batch were left not analyzed
    (backtracking across batches)."
    [this bm :- BatchManager, batch :- AnyBatch])
  (-analyze-eof! :- Any
    "Returns the result of tokenizer after the end of file has been
    reached. Result is either a `_this_` if no token is produced
    or a token, if the token could be produced.

    Use `leftover` function to return leftover batch alongside token,
    if items outside current batch were left not analyzed
    (backtracking across batches). Throws if input is incomplete and
    tokenizer is configured to throw on incomplete input."
    [this]))

(defprotocol IParserMachine
  "A protocol for parser machine. Is used in the parser engine to
  process tokens into parsed values."
  {:added v1
   :see '[ILazyParserMachine]
   :category "Primary"
   :predicate 'parser-machine?
   :forbid-extensions true}
  (-parse-value! :- Any
    "Returns the result of parsing `_value_` in the context of
    `_parents_` list of parent parser machines, including `_this_`.
    `_value_` may have been produced by a parser dispatcher
    or by a child parser machine. Result is a parser machine
    (if the parser machine has not yet finished) or a value if
    parsing has finished."
    [this value :- Any, parents :- IPersistentList])
  (-parse-eof! :- Any
    "Returns the result of parser after the end of file has been
    reached, in the context of `_parents_` list of parent parser
    machines, including `_this_`. Result is either a `_this_` if no
    value is produced or a value, if parsing is finished.
    Throws if input is incomplete and parser is configured to throw
    on incomplete input."
    [this parents :- IPersistentList]))

(defprotocol ILazyParserMachine
  "A protocol for lazy parser machine. Is used in the lazy parser
  engine to process the sequence of tokens into the sequence of
  parsed values."
  {:added v1
   :see '[IParserMachine]
   :category "Lazy"
   :predicate 'lazy-parser-machine?
   :forbid-extensions true}
  (-parse-seq :- [Any ISeq]
    "Returns the `[result rest-seq]` pair, a (mostly lazy) result of
    parsing input `_seq_` of values within `_parents_` context
    (which includes `_this_`), and a lazy seq of unparsed values.
    First item in the returned pair may be `_this_`,
    which signals that no value was produced."
    [this seq :- ISeq, parents :- IPersistentList]))

(defprotocol IParserMachineFactory
  "A factory protocol for tokenizer and parser machines."
  {:added v1
   :category "Primary"}
  (-parser-config :- {}
    "Returns a configuration map that is passed into machines
    created directly or indirectly with this parser machine factory.
    Passing of configuration map is done by convention as a first
    argument of machine constructor, with folowing keys recognized
    by the parser engine and its helper fns:

    * `:incomplete-mode` - Controls how machine should behave if
      end of file is reached before machine is done. Valid values are
      `nil` (throws), `:keep` (returns result so far or most probable
      result) or `:ignore` (behave as if there was nothing to parse).
    * `:token-item-limit` - specifies maximum number of input items
      a token can have before triggering limit exception.
    * `:container-level-limit` - specifies maximum number of nesting
      levels a container can have before triggering limit exception.
    * `:container-item-limit` - specifies maximum number of items
      a container can have before triggering limit exception.
    * `:initial-state` - specifies an initial state of a parser
      machine. Can be `nil` or a map."
    [this])
  (-parser-from-type :- (U nil Class Type)
    "Returns type of items which are to be parsed."
    [this])
  (-parser-to-type :- (U nil Class Type)
    "Returns type of parsed values."
    [this])
  (-dispatch-tokenizer :- Any
    "Returns a token or a tokenizer-machine as a result of dispatching
    on a given input `_item_` according to the `_config_`
    configuration and shared state map in `_state_` reference.
    Returns `_this_` if `_item_` should be ignored
    (e.g. whitespace)."
    [this config :- {}, state :- IReference, item :- Any])
  (-dispatch-parser :- Any
    "Returns a machine based on given `_token_` according to the
    `_config_` configuration and a shared state map in `_state_`
    reference, in the context of `_parents_` list of parent machines,
    or returns a value if `_token_` processing didn't require a
    dedicated machine.
    If the dispatcher wants to ignore a token, it should be done
    through a special parser machine called `ignore-token`."
    [this config :- {}, state :- IReference, token :- Any,
     parents :- IPersistentList]))

(defprotocol ILazyParserMachineFactory
  "A factory protocol for lazy parser machines."
  {:added v1
   :see '[IParserMachineFactory]
   :category "Lazy"}
  (-dispatch-lazy-parser :- Any
    "Returns a lazy machine based on given `_token_`, in the context
    of `_parents_` list of parent machines and a shared state map in
    `_state_` reference, or returns a value if `_token_` processing
    didn't require a dedicated machine.
    If the dispatcher wants to ignore a token, it should be done
    through a special parser machine `ignore-token`."
    [this config :- {}, state :- IReference, token :- Any,
     parents :- IPersistentList]))

;;; Helper functions for implementers of parser machines

(defn perror :- nil
  "Throws parser engine error with a given messages."
  {:added v1
   :see '[eof-handler]
   :category "Primary"}
  [& ms :- String]
  (throw (illegal-state ^java.lang.String
                        (apply ->str "parser engine error: " ms))))

(deftype LeftoverPair
  "Type for passing leftovers from a tokenizer."
  {:added v1
   :See '[leftover]
   :category "Primary"
   :predicate 'leftover?}
  [token :- Any, batch :- AnyBatch]
  clojure.lang.IMapEntry
  (key [_] token)
  (val [_] batch)
  java.util.Map$Entry
  (getKey [_] token)
  (getValue [_] batch))

(defn leftover :- LeftoverPair
  "Returns composite result of `_token_` and `_batch_` used as a
   return value in tokenizers."
  {:added v1
   :see '[leftover?]
   :category "Primary"}
  [token :- Any, batch :- AnyBatch]
  (->LeftoverPair token batch))

(defn keep? :- Boolean
  "Returns `true` if `_config_` map contains
  `[:incomplete-mode :keep]` entry, `false` otherwise."
  [config :- {}]
  (identical? :keep (:incomplete-mode config)))

(defn ignore? :- Boolean
  "Returns `true` if `_config_` map contains
  `[:incomplete-mode :ignore]` entry, `false` otherwise."
  [config :- {}]
  (identical? :ignore (:incomplete-mode config)))

(defn eof-handler :- Any
  "Returns `_machine_` if `(:incomplete-mode _config_)` is set to
  `:ignore`, returns `_val_` if `(:incomplete-mode _config_)` is set
  to `:keep`, otherwise throws with message `_m_`.
  This helper function is used inside parser machine for default
  handling of `-parse-eof!` and `-analyze-eof!`."
  {:added v1
   :see '[perror]
   :category "Primary"}
  [machine :- Any, config :- {}, val :- Any, m :- String]
  (cond (keep? config) val
        (ignore? config) machine
        :else (perror m " has reached eof.")))

(def max-lazy-level-limit :- Var
  "A dynamic var holding maximum level limit in lazy parsers.
  Used to prevent blowing the stack."
  {:added v1
   :see '[lazy-level-limit]
   :category "Lazy"}
  (var *max-lazy-level-limit*))

(defn lazy-level-limit :- Int
  "Returns container level limit based on given `_config_` map."
  {:added v1
   :see '[max-lazy-level-limit lazy-item-limit]
   :category "Lazy"}
  [config :- {}]
  (let [mlll (iint *max-lazy-level-limit*)]
    (imin mlll (iint (or (:container-level-limit config) mlll)))))

(defn lazy-item-limit :- Int
  "Returns container item limit based on given `_config_` map."
  {:added v1
   :see '[lazy-level-limit]
   :category "Lazy"}
  [config :- {}]
  (iint (or (:container-item-limit config) (i0))))

;; basic literals

(defmacro literal-tokenizer
  "Constructs tokenizer machine for data literals, with corresponding
  constructor."
  {:added v1
   :see '[skipping-tokenizer]
   :category "Primary"}
  [name contents value]
  (let [cname (symbol (->str name "-literal"))
        tname (symbol (camel-case (dunaj.identifier/name cname)))
        tc (symbol (->str "->" (dunaj.identifier/name tname)))
        x (gensym "x")
        ss (gensym "state")
        arr (gensym "arr")]
    `(do
       (deftype ~tname
         [config#
          ~(assoc-meta ss {:tag 'int :unsynchronized-mutable true})
          ~(assoc-meta arr {:tag 'ints})]
         ITokenizerMachine
         (~'-analyze-batch! [this# bm# batch#]
           (iloop [nss# ~ss]
             (if (.hasRemaining ^java.nio.Buffer batch#)
               (let [item# (.get ^dunaj.host.BatchManager bm# batch#)
                     x# (iint item#)]
                 (when-not (i== x# (aget ~arr nss#))
                   (perror "invalid item " item#))
                 (let [nss# (iinc nss#)]
                   (if (i== nss# (acount ~arr)) ~value (recur nss#))))
               (do (set! ~ss nss#) this#))))
         (~'-analyze-eof! [this#]
           (eof-handler this# config# ~value
                        ~(->str name " literal tokenizer machine"))))
       (let [a# (array (keyword->class :int) (map iint ~contents))]
         (defn ~cname
           ~(->str "Constructor for " name
                   " literal tokenizer machine.")
           [config# state# item#]
           (~tc config# (i1) a#))))))

(defmacro skipping-tokenizer
  "Constructs skipping tokenizer machine, with corresponding
  constructor. Skips `_n_` items from the input and then returns
  `_value_`."
  {:added v1
   :see '[literal-tokenizer]
   :category "Primary"}
  [name n value]
  (let [cname (symbol (->str name "-skipper"))
        tname (symbol (camel-case (dunaj.identifier/name cname)))
        tc (symbol (->str "->" (dunaj.identifier/name tname)))
        ss (gensym "state")]
    `(do
       (deftype ~tname
         [config#
          ~(assoc-meta ss {:tag 'int :unsynchronized-mutable true})]
         ITokenizerMachine
         (~'-analyze-batch! [this# bm# batch#]
           (if (i<= ~ss (.remaining ^java.nio.Buffer batch#))
             (do (.position ^java.nio.Buffer batch#
                            (iadd (.position ^java.nio.Buffer batch#)
                                  ~ss))
                 ~value)
             (do (set! ~ss (isub ~ss (.remaining ^java.nio.Buffer
                                                 batch#)))
                 (.position ^java.nio.Buffer batch#
                            (.limit ^java.nio.Buffer batch#))
                 this#)))
         (~'-analyze-eof! [this#]
           (eof-handler this# config# ~value
                        ~(->str name " skipping tokenizer machine"))))
       (defn ~cname
         ~(->str "Constructor for " name
                 " skipping tokenizer machine.")
         [config# state# item#]
         (~tc config# (iint ~n))))))

;; string literal tokenizer machine

(defn ^:private get-next-unicode :- Int
  [unicode :- Int, x :- Int]
  (when-not (ihexa? x)
    (perror "invalid character in unicode sequence " (char x)))
  (iadd (i<< unicode 4) (ihexa->int x)))

(defn ^:private get-next-octal :- Int
  [octal :- Int, x :- Int]
  (when-not (ioctal? x)
    (perror "invalid character in octal sequence " (char x)))
  (let [no (iadd (imul octal (i8)) (idigit->int x))]
    (if (i> no (ixFF)) (i-1) no)))

(deftype StringLiteral
  "String literal tokenizer with unicode and octal (optional) support,
  custom escapes and check for invalid characters."
  [config, ^:unsynchronized-mutable sb :- java.lang.StringBuilder,
   ^:unsynchronized-mutable special?, ^:unsynchronized-mutable octal,
   ^:unsynchronized-mutable unicode, invalid-pred escape-fn,
   octal-support? state]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch :- java.nio.CharBuffer batch]
      (iloop [begin (.position batch)]
        (if-not (.hasRemaining batch)
          (do
            (string-cat-batch! sb batch begin (.position batch) state)
            this)
          (let [pos (.position batch)
                c (.get batch)
                npos (.position batch)
                x (iint c)]
            (cond special?
                  (cond unicode
                        (let [nu (get-next-unicode unicode x)]
                          (if (izero? special?)
                            (do (set! sb (.append sb (char nu)))
                                (set! special? false)
                                (set! unicode nil))
                            (do (set! special? (idec special?))
                                (set! unicode nu)))
                          (recur npos))
                        octal
                        (if (ioctal? x)
                          (let [nu (get-next-octal octal x)]
                            (cond
                             (neg? nu)
                             (do
                               (set! sb (.append sb (char octal)))
                               (set! special? false)
                               (set! octal nil)
                               (.position batch (idec npos))
                               (recur begin))
                             (izero? special?)
                             (do (set! sb (.append sb (char nu)))
                                 (set! special? false)
                                 (set! octal nil)
                                 (recur npos))
                             :else
                             (do (set! special? (idec special?))
                                 (set! octal nu)
                                 (recur npos))))
                          (do (set! sb (.append sb (char octal)))
                              (set! special? false)
                              (set! octal nil)
                              (.position batch (idec npos))
                              (recur begin)))
                        (i== x (iSMALL_U)) ;; \u
                        (do (set! special? (i3))
                            (set! unicode (i0))
                            (recur npos))
                        (and octal-support? (ioctal? x))
                        (do (set! special? (i1))
                            (set! octal (get-next-octal (i0) x))
                            (recur npos))
                        :else
                        (do (set! sb (.append sb (escape-fn x)))
                            (set! special? false)
                            (recur npos)))
                  (invalid-pred x)
                  (perror "invalid character " c)
                  :else
                  (cond
                   (i== x (iQUOTE))
                   (do (string-cat-batch! sb batch begin pos state)
                       (settle! sb))
                   (i== x (iBACKSLASH))
                   (do (string-cat-batch! sb batch begin pos state)
                       (set! special? true)
                       (recur npos))
                   :else (recur begin))))))))
  (-analyze-eof! [this]
    (eof-handler this config (settle! sb)
                 "string literal tokenizer machine")))

(defn string-literal-constructor :- AnyFn
  "Returns tokenizer machine constructor for string literal
  tokenizer machine supporting unicode and octal escapes,
  using `_escape-fn_` to translate escapes, `_invalid-pred_`
  to check for invalid input and `_octal-support?_` to toggle
  support for octal escape sequences."
  {:added v1
   :see '[literal-tokenizer]
   :category "Primary"}
  [invalid-pred :- Predicate, escape-fn :- AnyFn,
   octal-support? :- Boolean]
  (fn [config :- {}, state :- IReference, item :- Any]
    (->StringLiteral config (edit empty-string) false nil nil
                     invalid-pred escape-fn octal-support? state)))

;; container parser

(defmacro container-parser
  "Constructs parser machine for container data types, with
  corresponding constructor. `_empty-container_` must be editable."
  {:added v1
   :category "Primary"}
  [name close-token separator-token empty-container]
  (let [cname (symbol (->str name "-container"))
        tname (symbol (camel-case (dunaj.identifier/name cname)))
        tc (symbol (->str "->" (dunaj.identifier/name tname)))
        contents (gensym "contents")
        separator? (gensym "separator")]
    (if (nil? separator-token)
      `(do
         (deftype ~tname
           [config#
            ~(assoc-meta contents {:unsynchronized-mutable true})]
           IParserMachine
           (~'-parse-value! [this# value# parents#]
             (if (identical? ~close-token value#)
               (settle! ~contents)
               (do (set! ~contents (conj! ~contents value#)) this#)))
           (~'-parse-eof! [this# parents#]
             (eof-handler this# config# (settle! ~contents)
                          ~(->str name " container parser machine")))
           ILazyParserMachine
           (~'-parse-seq [this# coll# parents#]
             (let [cil# (lazy-item-limit config#)
                   cll# (lazy-level-limit config#)
                   pred?# #(identical? ~close-token %)]
               (pair
                (take-until-token parents# pred?# cil# cll# coll#)
                (drop-until-token parents# pred?# cil# cll# coll#)))))
         (defn ~cname
           ~(->str "Constructor for " name
                   " container parser machine.")
           [config# state#]
           (~tc config# (edit ~empty-container))))
      `(do
         (deftype ~tname
           [config#
            ~(assoc-meta contents {:unsynchronized-mutable true})
            ~(assoc-meta separator? {:unsynchronized-mutable true})]
           IParserMachine
           (~'-parse-value! [this# value# parents#]
             (cond
               (identical? ~close-token value#)
               (do (when (and ~separator? (not (empty? ~contents)))
                     (perror "trailing separator in "
                             ~name " container parser machine"))
                   (settle! ~contents))
               (identical? ~separator-token value#)
               (do (when ~separator?
                     (perror "duplicate separator in "
                             ~name " container parser machine"))
                   (set! ~separator? true)
                   this#)
               :else
               (do (when-not ~separator?
                     (perror "missing separator in "
                             ~name " container parser machine"))
                   (set! ~separator? false)
                   (set! ~contents (conj! ~contents value#))
                   this#)))
           (~'-parse-eof! [this# parents#]
             (eof-handler this# config# (settle! ~contents)
                          ~(->str name " container parser machine")))
           ILazyParserMachine
           (~'-parse-seq [this# coll# parents#]
             (let [pred?# #(identical? ~close-token %)
                   cil# (lazy-item-limit config#)
                   cll# (lazy-level-limit config#)
                   items# (take-until-token parents#
                                            pred?# cil# cll# coll#)
                   items# (partition 2 2 [~separator-token]
                                     (cons ~separator-token items#))
                   cf# (fn [[sep# k#]]
                         (when (or (not (identical? ~separator-token
                                                    sep#))
                                   (identical? ~separator-token k#))
                           (perror "missing separator in "
                                   ~name " container parser machine"))
                         k#)]
               (pair (seq (map cf# items#))
                     (drop-until-token parents# pred?# cil#
                                       cll# coll#)))))
         (defn ~cname
           ~(->str "Constructor for " name
                   " container parser machine")
           [config# state#]
           (~tc config# (edit ~empty-container) true))))))

;; ignore parser machine

(deftype IgnoreParser []
  IParserMachine
  (-parse-value! [this value parents] value)
  (-parse-eof! [this parents] this)
  ILazyParserMachine
  (-parse-seq [this seq parents] (pair this seq)))

(def ignore-token :- IgnoreParser
  "A parser machine which ignores itself."
  {:added v1
   :category "Primary"}
  (->IgnoreParser))

;;; Lazy parser engine implementation

(defn lazy-parser :- ISeq
  "Returns a lazy seq of parsed values from `_coll_`, within the
  context of `_parents_`.
  Throws if `_level-limit_` has been reached."
  {:added v1
   :category "Lazy"}
  [parents :- IPersistentList, level-limit :- Integer, coll :- IRed]
  (when (and (pos? level-limit) (< level-limit (count parents)))
    (perror "container level count reached " (count parents)))
  (lazy-seq
   (let [s (seq coll),
         val ((or (first s) identity) parents), nseq (rest s)]
     (cond (nil? s) empty-seq
           (lazy-parser-machine? val)
           (let [r (-parse-seq val nseq (conj parents val))
                 nval (ct/key r)
                 nseq (ct/val r)]
             (if (identical? val nval)
               ;; possible stack overflow here
               (lazy-parser parents level-limit nseq)
               (cons nval (lazy-parser parents level-limit nseq))))
           :else
           (cons val (lazy-parser parents level-limit nseq))))))

(defn take-until-token :- ISeq
  "Returns a lazy seq of parsed values from `_coll_`, up until
  `_pred_` returns `true`. Throws if either `_level-limit_` or
  `_item-limit_` has been reached."
  {:added v1
   :see '[take-one drop-until-token]
   :category "Lazy"}
  [parents :- IPersistentList, pred :- AnyFn,
   item-limit :- Integer, level-limit :- Integer, coll :- IRed]
  (->> coll
       (lazy-parser parents level-limit)
       (take-while (complement pred))
       (throwing-cap item-limit)
       seq
       lazy-seq))

(defn take-one :- Any
  "Returns first parsed value from `_coll_`.
  Throws if `_level-limit_` has been reached."
  {:added v1
   :see '[drop-one take-until-token process-one]
   :category "Lazy"}
  [parents :- IPersistentList, level-limit :- Integer, coll :- IRed]
  (when (and (pos? level-limit) (< level-limit (count parents)))
    (perror "container level count reached " (count parents)))
  (let [s (seq coll),
        val ((or (first s) identity) parents), nseq (rest s)]
    (cond (nil? s) nil
          (lazy-parser-machine? val)
          (let [r (-parse-seq val nseq (conj parents val))
                nval (ct/key r)
                nseq (ct/val r)]
            (if (identical? val nval)
              (recur parents level-limit nseq)
              nval))
          :else val)))

(defn drop-until-token :- ISeq
  "Returns a lazy seq of tokens with all tokens before `_pred_`
  returns `true` removed. Note that this fn automatically skips
  tokens handled by parser machines (e.g. containers)!"
  {:added v1
   :see '[drop-one take-until-token]
   :category "Lazy"}
  [parents :- IPersistentList, pred :- AnyFn,
   item-limit :- Integer, level-limit :- Integer, coll :- IRed]
  (when (and (pos? level-limit) (< level-limit (count parents)))
    (perror "container level count reached " (count parents)))
  (lazy-seq
   (iloop [s (seq coll), limit (iint item-limit)]
     (let [val ((or (first s) identity) parents), nseq (next s)
           limit (idec limit)]
       (cond (izero? limit)
             (perror "container item count reached " item-limit)
             (nil? s) empty-seq
             (pred val) nseq
             (lazy-parser-machine? val)
             (let [r (-parse-seq val nseq (conj parents val))
                   nval (ct/key r)
                   nseq (ct/val r)
                   nlim (if (identical? val nval) (iinc limit) limit)]
               (recur nseq nlim))
             :else (recur nseq limit))))))

(defn drop-one :- ISeq
  "Returns a seq of tokens with first processed value removed.
  Note that this fn automatically skips tokens handled
  by parser machines (e.g. containers)!"
  {:added v1
   :see '[drop-until-token take-one process-one]
   :category "Lazy"}
  [parents :- IPersistentList, level-limit :- Integer, coll :- IRed]
  (when (and (pos? level-limit) (< level-limit (count parents)))
    (perror "container level count reached " (count parents)))
  (let [s (seq coll)
        val ((or (first s) identity) parents), nseq (next s)]
    (cond (nil? s) nil
          (lazy-parser-machine? val)
          (let [r (-parse-seq val nseq (conj parents val))
                nval (ct/key r)
                nseq (ct/val r)]
            (if (identical? val nval)
              (recur parents level-limit nseq)
              nseq))
          :else nseq)))

(defn process-one :- [Any ISeq]
  "Returns a pair of parsed value and rest of seq."
  {:added v1
   :see '[drop-one take-one]
   :category "Lazy"}
  [parser-machine :- IParserMachine, config :- {}, coll :- IRed,
   parents :- IPersistentList]
  (let [level-limit (lazy-level-limit config)
        val (take-one parents level-limit coll)]
    (pair (-parse-value! parser-machine val parents)
          (drop-one parents level-limit coll))))

;;; Parser engine implementation

(deftype PWrap [ret :- Any, state :- (Maybe IReference),
                machines :- IPersistentList, is :- IPersistentList])

(defn p-advance
  [ret :- Any, state :- (Maybe IReference),
   machines :- IPersistentList, is :- IPersistentList]
  (cond (reduced? ret) (reduced (->PWrap @ret state nil is))
        (postponed? ret)
        ;; deep clone of machines is needed
        (unsafe-postponed
         (->PWrap @ret state machines is)
         #(p-advance (unsafe-advance! ret) state machines is))
        :else (->PWrap ret state machines is)))

(deftype ParserEngineReducing
  [r :- IReducing, machine-factory :- IParserMachineFactory,
   config :- {}, item-limit :- Int, level-limit :- Int,
   state :- IReference]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [rf (fn [val m machines]
               (loop [oldm m
                      m (if (nothing? val)
                          (-parse-eof! m machines)
                          (-parse-value! m val machines))
                      no-val? (nothing? val)]
                 (cond
                  (and no-val? (identical? oldm m)) nothing
                  (parser-machine? m)
                  (recur
                   m (-parse-eof! m (conj (pop machines) m)) true)
                  :else m)))
          af (fn af [wrap]
               (let [ret (.-ret ^dunaj.format.parser.PWrap wrap)
                     machines
                     (.-machines ^dunaj.format.parser.PWrap wrap)]
                 (loop [val nothing
                        machines machines]
                   (if-let [m (peek machines)]
                     (recur (rf val m machines) (pop machines))
                     (if (nothing? val) ret (._step r ret val))))))]
      (-> (af (strip-reduced wrap))
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (->PWrap (._wrap r ret) state () (->lst (i0))))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.format.parser.PWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.format.parser.PWrap wrap)
          state (.-state ^dunaj.format.parser.PWrap wrap)
          machines (.-machines ^dunaj.format.parser.PWrap wrap)
          is (.-is ^dunaj.format.parser.PWrap wrap)
          af (fn af [ret, x :- Any, state :- IReference
                     machines :- IPersistentList,
                     is :- IPersistentList]
               (if-let [m (peek machines)]
                 (let [res (-parse-value! m x machines)
                       ni (iinc (iint (peek is)))
                       ris (pop is)]
                   (when (and (ipos? item-limit) (i< item-limit ni))
                     (perror "container item count reached " ni))
                   (cond
                    ;; parser still the same
                    (identical? res m)
                    (->PWrap ret state machines (conj ris ni))
                    ;; new parser, replace
                    (parser-machine? res)
                    (->PWrap ret state
                             (conj (pop machines) res) (conj ris ni))
                    ;; parser machine has finished
                    :else
                    (recur ret res state (pop machines) ris)))
                 (p-advance
                  (._step r ret x) state machines is)))
          x (-dispatch-parser
             machine-factory config state val machines)]
      (if (parser-machine? x)
        ;; new parser machine, put into history and continue
        (let [l (iint (count machines))]
          (when (and (ipos? level-limit) (i< level-limit l))
            (perror "container level count reached " l))
          (->PWrap ret state (conj machines x) (conj is (i0))))
        ;; value has been produced
        (af ret x state machines is)))))

(deftype TWrap [ret :- Any, state :- (Maybe IReference),
                machine :- (Maybe ITokenizerMachine), i :- Int])

(defn t-advance :- TWrap
  [ret :- Any, state :- (Maybe IReference),
   machine :- (Maybe ITokenizerMachine), i :- Int]
  (cond (reduced? ret) (reduced (->TWrap @ret state nil i))
        (postponed? ret)
        (postponed
         (->TWrap @ret state machine i)
         #(t-advance (advance ret) (clone state) (clone machine) i)
         #(t-advance (unsafe-advance! ret) state machine i))
        :else (->TWrap ret state machine i)))

(deftype TokenizerEngineReducing
  [r :- IReducing, machine-factory :- IParserMachineFactory,
   fbm :- BatchManager, config :- {}, item-limit :- Int,
   state :- IReference]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [af (fn af [wrap left-batch]
               (cond
                (reduced? wrap)
                (reduced (.-ret ^dunaj.format.parser.TWrap @wrap))
                (postponed? wrap)
                (postponed (.-ret ^dunaj.format.parser.TWrap @wrap)
                           #(af (advance wrap) (clone left-batch))
                           #(af (unsafe-advance! wrap) left-batch))
                (nil? left-batch)
                (let [ret (.-ret ^dunaj.format.parser.TWrap wrap)
                      m (.-machine ^dunaj.format.parser.TWrap wrap)
                      state (.-state ^dunaj.format.parser.TWrap wrap)]
                  (if (nil? m)
                    ret
                    (let [res (-analyze-eof! m)]
                      (cond
                       (identical? m res) ret
                       (leftover? res)
                       (let [val (ct/key res)
                             left-batch (ct/val res)]
                         (recur (t-advance
                                 (._step r ret val) state nil (i0))
                                left-batch))
                       :else
                       (recur
                        (t-advance (._step r ret res) state nil (i0))
                        nil)))))
                :else (recur (._step this wrap left-batch) nil)))]
      (-> (af (strip-reduced wrap) nil)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (->TWrap (._wrap r ret) state nil (i0)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.format.parser.TWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.format.parser.TWrap wrap)
          state (.-state ^dunaj.format.parser.TWrap wrap)
          machine (.-machine ^dunaj.format.parser.TWrap wrap)
          i (.-i ^dunaj.format.parser.TWrap wrap)
          af (fn af [ret, batch :- AnyBatch, state :- IReference,
                     machine :- (Maybe ITokenizerMachine), i :- Int]
               (cond
                 (reduced? ret) (reduced (->TWrap @ret state nil i))
                 (postponed? ret)
                 (postponed (->TWrap @ret state machine i)
                            #(af (advance ret) (clone batch)
                                 (clone state) (clone machine) i)
                            #(af (unsafe-advance! ret) batch
                                 state machine i))
                 (not (.hasRemaining batch))
                 (->TWrap ret state machine i)
                 (nil? machine)
                 (let [item (.get fbm batch)
                       x (-dispatch-tokenizer
                          machine-factory config state item)]
                   (cond
                     (identical? machine-factory x)
                     (recur ret batch state nil (i0))
                     (tokenizer-machine? x)
                     (recur ret batch state x (i0))
                     :else
                     (recur (._step r ret x) batch state nil (i0))))
                 :else
                 (let [pos (.position batch)
                       res (-analyze-batch! machine fbm batch)]
                   (cond
                     (tokenizer-machine? res)
                     (let [ni (iadd i (isub (.position batch) pos))]
                       (when (and (ipos? item-limit)
                                  (i< item-limit ni))
                         (perror "token item count reached " ni))
                       (recur ret batch state res ni))
                     (leftover? res)
                     (recur (._step r ret (ct/key res))
                            (prepend-unread fbm batch (ct/val res))
                            state nil (i0))
                     :else
                     (recur
                      (._step r ret res) batch state nil (i0))))))]
      (af ret val state machine i))))

(defxform tokenizer-engine*
  "A transducer which process step batches of values into
  tokens, using `machine-factory` to control tokenizing."
  [machine-factory :- IParserMachineFactory]
  ([r]
   (let [fbm (batch-manager (-parser-from-type machine-factory))
         config (-parser-config machine-factory)
         item-limit (iint (or (:token-item-limit config) (i0)))
         state (unsynchronized-reference (:initial-state config))]
     (->TokenizerEngineReducing
      r machine-factory fbm config item-limit state)))
  :unpack false
  :section false
  :fold false
  :count false)

(deftype TokenizerEngine
  [coll machine-factory state]
  IRed
  (-reduce [this reducef init]
    (let [from-type (-parser-from-type machine-factory)
          fbm (batch-manager (-parser-from-type machine-factory))
          config (-parser-config machine-factory)
          item-limit (iint (or (:token-item-limit config) (i0)))]
      (if (and (satisfies? IBatchedRed coll)
               (item-types-match? from-type (item-type coll)))
        (reduce-augmented*
         coll
         #(reduce-batched*
           from-type @default-formatter-batch-size % %2 %3)
         (->TokenizerEngineReducing
          (reducing reducef init)
          machine-factory fbm config item-limit state))
        (reduce-augmented*
         coll reduce*
         ((batch from-type nil)
          (->TokenizerEngineReducing
           (reducing reducef init)
           machine-factory fbm config item-limit state))))))
  IConfig
  (-config [this] (-parser-config machine-factory)))

(defn tokenizer-engine
  "A transducer which process step values into
  tokens, using `_machine-factory_` to control tokenizing."
  {:added v1
   :see '[parser-engine]
   :tsig (Fn [Transducer IParserMachineFactory]
             [IRed IParserMachineFactory []])
   :transducer true
   :category "Primary"}
  ([machine-factory]
   (comp
    (batch (-parser-from-type machine-factory) nil)
    (tokenizer-engine* machine-factory)))
  ([machine-factory coll]
   (let [config (-parser-config machine-factory)
         state (unsynchronized-reference (:initial-state config))]
     (->TokenizerEngine coll machine-factory state))))

(deftype ParserEngine
  [coll machine-factory state]
  IRed
  (-reduce [this reducef init]
    (let [from-type (-parser-from-type machine-factory)
          fbm (batch-manager (-parser-from-type machine-factory))
          config (-parser-config machine-factory)
          token-item-limit (iint (or (:token-item-limit config) (i0)))
          item-limit (iint (or (:container-item-limit config) (i0)))
          level-limit
          (iint (or (:container-level-limit config) (i0)))]
      (if (and (satisfies? IBatchedRed coll)
               (item-types-match? from-type (item-type coll)))
        (reduce-augmented*
         coll
         #(reduce-batched*
           from-type @default-formatter-batch-size % %2 %3)
         (->TokenizerEngineReducing
          (->ParserEngineReducing
           (reducing reducef init)
           machine-factory config item-limit level-limit state)
          machine-factory fbm config token-item-limit state))
        (reduce-augmented*
         coll reduce*
         ((batch from-type nil)
          (->TokenizerEngineReducing
           (->ParserEngineReducing
            (reducing reducef init)
            machine-factory config item-limit level-limit state)
           machine-factory fbm config token-item-limit state))))))
  IConfig
  (-config [this] (-parser-config machine-factory)))

(defxform parser-engine*
  "A transducer for parser engine."
  [machine-factory :- IParserMachineFactory]
  ([r]
   (let [fbm (batch-manager (-parser-from-type machine-factory))
         config (-parser-config machine-factory)
         token-item-limit
         (iint (or (:token-item-limit config) (i0)))
         item-limit (iint (or (:container-item-limit config) (i0)))
         level-limit (iint (or (:container-level-limit config) (i0)))
         state (unsynchronized-reference (:initial-state config))]
     ((batch (-parser-from-type machine-factory) nil)
      (->TokenizerEngineReducing
       (->ParserEngineReducing
        r machine-factory config item-limit level-limit state)
       machine-factory fbm config token-item-limit state))))
  :unpack false
  :section false
  :fold false
  :count false)

(defn parser-engine
  "A transducer which parser step items into a parsed values,
  using `_machine-factory_` to control parsing."
  {:added v1
   :see '[lazy-parser-engine tokenizer-engine]
   :tsig (Fn [Transducer IParserMachineFactory]
             [IRed IParserMachineFactory []])
   :transducer true
   :category "Primary"}
  ([machine-factory]
   (parser-engine* machine-factory))
  ([machine-factory coll]
   (let [config (-parser-config machine-factory)
         state (unsynchronized-reference (:initial-state config))]
     (->ParserEngine coll machine-factory state))))

(defn lazy-parser-engine
  "Returns a lazy seq of parsed input collection `_coll_`,
  with parser engine using `_machine-factory_` to control parsing."
  {:added v1
   :see '[parser-engine]
   :tsig (Fn [Transducer ILazyParserMachineFactory]
             [IRed ILazyParserMachineFactory []])
   :transducer true
   :category "Lazy"}
  [machine-factory coll]
  (let [config (-parser-config machine-factory)
        state (unsynchronized-reference (:initial-state config))
        level-limit (lazy-level-limit config)
        dispatch-fn (fn [val] #(-dispatch-lazy-parser
                                machine-factory config state val %))]
    (->> (->TokenizerEngine coll machine-factory state)
         (map dispatch-fn)
         (lazy-parser () level-limit))))
