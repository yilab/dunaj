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

(ns dunaj.format.json
  "JSON liberal formatter with support for lazy parsing and pretty
  printing.

  Parser assumes input is valid JSON (ECMA-404),
  treats ',' and ':' as whitespaces, protects against malicious
  literals and by default handles incomplete input.
  Not suitable if you need a strict parser.

  Printer supports naive pretty mode with ANSII color support."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [scratch v1]]
   [dunaj.type :refer [Fn Any AnyFn Maybe U I]]
   [dunaj.boolean :refer [Boolean and or not true? false?]]
   [dunaj.host :refer [Batch set! keyword->class class-instance?]]
   [dunaj.host.int :refer
    [Int iint iinc i== i< isub izero? idec ineg? i> i< i<< imax iadd
     iloop i0 i1 i2 i3 i4 i5 i8 iFF ione? i-1 imul i10 idiv idigit?
     ioctal? ihexa? ihexa->int idigit->int ismall-letter? iCAPITAL_M
     icapital-letter? iZERO iUS iDEL iHT iCR iLF iCOLON iSPACE iCOMMA
     iLBRACKET iRBRACKET iQUOTE iAPOS iBACKSLASH iSLASH iLBRACE
     iRBRACE iTILDE iSMALL_B iSMALL_F iSMALL_N iSMALL_R iMINUS
     iSMALL_U iSMALL_T iPLUS iCAPITAL_E iSMALL_E iCAPITAL_N iSMALL_X
     iCAPITAL_X iBS iHT iCR iLF iFF iCAPITAL_R iSMALL_R iDOT]]
   [dunaj.math :refer
    [max min < == > neg? + inc >= inc dec - pos? * /]]
   [dunaj.compare :refer [identical? nil? = defsentinel]]
   [dunaj.flow :refer
    [if when when-not cond do let loop recur if-not]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer
    [deftype defprotocol extend-protocol! defrecord]]
   [dunaj.coll :refer
    [first rest seq second reduce empty? slice count nth conj assoc
     ISeq ISeqable sequential? settle! edit conj! assoc!]]
   [dunaj.function :refer [fn defn identity apply]]
   [dunaj.error :refer [throw illegal-state unsupported-operation]]
   [dunaj.identifier :refer [INamed name symbol]]
   [dunaj.char :refer [Char char]]
   [dunaj.string :refer
    [String MutableString ->str empty-string str string?]]
   [dunaj.state.var :refer [def]]
   [dunaj.coll.default :refer [empty-vec empty-map vec]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.lazy-seq-map :refer [lazy-seq->map]]
   [dunaj.coll.tuple :as ct :refer [tuple pair]]
   [dunaj.coll.recipe :refer
    [map partition keep mapcat take repeat remove]]
   [dunaj.format :refer [IParserFactory IPrinterFactory parse print]]
   [dunaj.format.helper :refer [string-to-batch! string-cat-batch!]]
   [dunaj.format.parser :refer
    [parser-engine string-literal-constructor leftover IParserMachine
     ITokenizerMachine -analyze-eof! container-parser -parser-config
     skipping-tokenizer tokenizer-engine IParserMachineFactory
     ILazyParserMachineFactory ILazyParserMachine -dispatch-parser
     literal-tokenizer ignore-token lazy-parser-engine
     perror eof-handler keep? ignore? lazy-parser lazy-item-limit
     lazy-level-limit take-until-token drop-until-token]]
   [dunaj.format.printer :refer
    [IContainerPrinterMachine -printer-to-type IPrinterMachineFactory
     printer-engine -indent invalid-item-handler print-colored! red
     pretty-printer-engine print! IIndentedMachine cyan prev-indent
     next-indent base-indent color default-color custom-colorer]]))


;;;; Implementation details

(defsentinel nothing)

;;;; Parser

(defn json-whitespace? :- Boolean
  "Returns true if `x` represents a JSON whitespace Unicode code
  point, otherwise returns false."
  [x :- Int]
  (or (i== x (iSPACE)) (i== x (iLF)) (i== x (iCOMMA))
      (i== x (iCOLON)) (i== x (iHT)) (i== x (iCR))))

;;; true, false and null literals

(literal-tokenizer "true" "true" true)
(literal-tokenizer "false" "false" false)
(literal-tokenizer "null" "null" nil)

;;; number literal

(defn json-number-element? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of a JSON Number Literal, otherwise returns false."
  [x :- Int]
  (or (idigit? x) (i== x (iMINUS)) (i== x (iPLUS)) (i== x (iDOT))
      (i== x (iSMALL_E)) (i== x (iCAPITAL_E))))

(deftype JsonNumberLiteralTokenizerMachine
  "A type for a JSON Number Literal tokenizer machine."
  [config state ^:unsynchronized-mutable decimal?
   ^:unsynchronized-mutable ts :- MutableString]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch :- (Batch java.lang.Character) batch
          begin (.position batch)]
      (loop []
        (if-not (.hasRemaining batch)
          (do (string-cat-batch!
               ts batch begin (.position batch) state)
              this)
          (let [pos (.position batch)
                x (iint (.get batch))]
            (if (json-number-element? x)
              (do (when (or (i== x (iDOT))
                            (i== x (iCAPITAL_E))
                            (i== x (iSMALL_E)))
                    (set! decimal? true))
                  (recur))
              (do (string-cat-batch! ts batch begin pos state)
                  (.position batch pos)
                  (-analyze-eof! this))))))))
  (-analyze-eof! [this]
    (let [s :- String (settle! ts)]
      (cond (not decimal?) (let [bi (java.math.BigInteger. s)]
                             (if (>= (.bitLength bi) 64)
                               (clojure.lang.BigInt/fromBigInteger bi)
                               (.longValue bi)))
            (not (:bigdec config)) (java.lang.Double/valueOf s)
            clojure.core/*math-context*
            (java.math.BigDecimal. s clojure.core/*math-context*)
            :else (java.math.BigDecimal. s)))))

(defn json-number-literal
  "Returns JSON Number Literal Tokenizer Machine."
  [config state item]
  (let [ts ^java.lang.StringBuilder (edit empty-string)]
    (->JsonNumberLiteralTokenizerMachine
     config state false (.append ts (char item)))))

;;; string literal

(defn ^:private invalid-json-string-element? :- Boolean
  "Returns true if `x` is a Unicode code point of an invalid character
  inside a string literal, otherwise returns false."
  [x :- Int]
  (i< x (iSPACE)))

(defn ^:private from-escape :- Char
  "Returns character which is represented by an escape character with
  Unicode code point `x`.
  Throws if escape character is not recognized."
  [x :- Int]
  (cond (i== x (iQUOTE)) \"
        (i== x (iAPOS)) \'
        (i== x (iBACKSLASH)) \\
        (i== x (iSLASH)) \/
        (i== x (iSMALL_B)) \u0008
        (i== x (iSMALL_F)) \u000c
        (i== x (iSMALL_N)) \u000a
        (i== x (iSMALL_R)) \u000d
        (i== x (iSMALL_T)) \u0009
        :else (perror "invalid escape character " (char x))))

(def json-string-literal
  "Function which returns a JSON String Literal Tokenizer Machine."
  (string-literal-constructor
   invalid-json-string-element? from-escape false))

;;; array container

(container-parser "json-array" :json-array-close nil empty-vec)

;;; object container

(deftype JsonObjectContainer
  "JSON Object container parser machine type."
  [config ^:unsynchronized-mutable contents
   ^:unsynchronized-mutable key key-fn value-fn]
  IParserMachine
  (-parse-value! [this token parents]
    (cond (identical? :json-object-close token) (settle! contents)
          (nothing? key) (do (set! key (key-fn token)) this)
          :else (let [x (value-fn key token)]
                  (when-not (identical? x value-fn)
                    (set! contents (assoc! contents key token)))
                  (set! key nothing)
                  this)))
  (-parse-eof! [this parents]
    (eof-handler this config (settle! contents)
                 "json object container parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (let [pred? #(identical? :json-object-close %)
          item-limit (* 2 (lazy-item-limit config))
          level-limit (lazy-level-limit config)
          keyvals (take-until-token
                   parents pred? item-limit level-limit coll)
          mf (fn [p] (let [k (key-fn (ct/key p))
                           v (value-fn k (ct/val p))]
                      (when-not (identical? v value-fn) (pair k v))))]
      (pair (lazy-seq->map
             (mapcat identity (keep mf (partition 2 keyvals))))
            (drop-until-token
             parents pred? item-limit level-limit coll)))))

(defn json-object-container
  "Constructor for json object container parser machine."
  [config state]
  (->JsonObjectContainer config (edit empty-map) nothing
                         (:key-fn config) (:value-fn config)))


;;;; Printer

(deftype JsonTopContainer
  "Top level printer container for JSON printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

(def json-true-batch (string-to-batch! "true"))
(def json-false-batch (string-to-batch! "false"))
(def json-null-batch (string-to-batch! "null"))
(def json-quote-batch (string-to-batch! "\\\""))
(def json-backslash-batch (string-to-batch! "\\\\"))
(def json-slash-batch (string-to-batch! "\\/"))
(def json-bs-batch (string-to-batch! "\\b"))
(def json-ht-batch (string-to-batch! "\\t"))
(def json-lf-batch (string-to-batch! "\\n"))
(def json-ff-batch (string-to-batch! "\\f"))
(def json-cr-batch (string-to-batch! "\\r"))
(def json-ls-batch (string-to-batch! "\\u2028"))
(def json-ps-batch (string-to-batch! "\\u2029"))
(def json-level-limit-batch (string-to-batch! "#"))
(def json-item-limit-batch (string-to-batch! "..."))

;;; printer for JSON object

(deftype JsonObjectPrinter
  "JSON Object printer machine type."
  {:predicate 'json-object-printer?}
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (print! batch bm state \{))
  (-print-after! [this bm batch parents]
    (print! batch bm state \}))
  (-print-between! [this bm batch parents]
    (print! batch bm state \,)))

(defn json-object-printer
  "Constructor for JSON Object printer machine."
  [config state coll]
  (let [key-fn (:key-fn config)
        value-fn (:value-fn config)
        kf #(let [k (ct/key %), v (value-fn k (ct/val %))]
              (when-not (identical? value-fn v) (pair (key-fn k) v)))
        transformed (keep kf coll)]
    (->JsonObjectPrinter config state transformed)))

(deftype JsonEntryContainer
  [config state contents]
  IContainerPrinterMachine
  (-children [this parents] contents)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (print! batch bm state \:)))

;;; printer for JSON array

(deftype JsonContainer [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (print! batch bm state \[))
  (-print-after! [this bm batch parents]
    (print! batch bm state \]))
  (-print-between! [this bm batch parents]
    (print! batch bm state \,)))

;;; printing strings

(def ^:private zeroes :- String "0000")

(defn ^:private to-escape
  "Returns batch containing escape sequence or nil, if no
  escape sequence is needed."
  [config x]
  (let [ci (iint x)]
    (cond
     (i== ci (iQUOTE)) json-quote-batch
     (i== ci (iSLASH)) (when (:escape-slash config) json-slash-batch)
     (i== ci (iBACKSLASH)) json-backslash-batch
     (and (i< (iUS) ci) (i< ci (iDEL))) nil
     (i== ci (iBS)) json-bs-batch
     (i== ci (iHT)) json-ht-batch
     (i== ci (iLF)) json-lf-batch
     (i== ci (iFF)) json-ff-batch
     (i== ci (iCR)) json-cr-batch
     (i== ci (iint 0x2028))
     (when (:escape-js-separators config) json-ls-batch)
     (i== ci (iint 0x2029))
     (when (:escape-js-separators config) json-ps-batch)
     :else (when (:escape-unicode config)
             (let [ns (java.lang.Integer/toString ci 16)]
               (string-to-batch!
                (->str "\\u" (.substring zeroes (count ns)) ns)))))))

;;; json printer protocol with basic implementations

(defprotocol IJsonPrinter
  (-print-json!
    "Returns result or printing `this` as a json. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(extend-protocol! IJsonPrinter
  java.lang.String
  (-print-json! [this config state bm batch parents]
    (print! batch bm state \" [this #(to-escape config %)] \"))
  clojure.lang.Sequential
  (-print-json! [this config state bm batch parents]
    (if (json-object-printer? (first parents))
      (->JsonEntryContainer config state this)
      (->JsonContainer config state this)))
  clojure.lang.IPersistentMap
  (-print-json! [this config state bm batch parents]
    (json-object-printer config state this))
  clojure.lang.IRecord
  (-print-json! [this config state bm batch parents]
    (json-object-printer config state this))
  java.lang.Number
  (-print-json! [this config state bm batch parents]
    (string-to-batch! (.toString ^java.lang.Number this) bm batch))
  nil
  (-print-json! [this config state bm batch parents]
    (print! batch bm state json-null-batch))
  java.lang.Boolean
  (-print-json! [this config state bm batch parents]
    (let [b (if this json-true-batch json-false-batch)]
      (print! batch bm state b)))
  java.lang.Object
  (-print-json! [this config state bm batch parents]
    (invalid-item-handler batch bm state config this parents)))


;;;; Pretty printer

(defprotocol IJsonPrettyCount
  (-pretty-count-json
    "Returns the count estimation of the pretty printer collection
    for `this` object in inline mode."
    [this]))

(defprotocol IJsonPrettyPrinter
  (-print-pretty-json!
    "Returns result or pretty printing `this` as a json. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(defn ^:private json-pretty-mode
  "Returns true if `coll` should be printed indented in multiple
  lines, otherwise returns false."
  [config coll]
  (let [x (-pretty-count-json coll)]
    (or (neg? x) (< (:inline-threshold config) x))))

(extend-protocol! IJsonPrettyCount
  java.lang.String
  (-pretty-count-json [this] (iadd (i2) (count this)))
  clojure.lang.Keyword
  (-pretty-count-json [this] (iadd (i1) (count (->str this))))
  clojure.lang.Symbol
  (-pretty-count-json [this] (iadd (i2) (count (->str this))))
  clojure.lang.Sequential
  (-pretty-count-json [this]
    (let [rf #(iadd (iadd % 2) (-pretty-count-json %2))]
      (if (i< (count this) (i10))
        (reduce rf (i0) this)
        (idiv (imul (reduce rf (i0) (take 10 this)) (count this))
              (i10)))))
  clojure.lang.IPersistentMap
  (-pretty-count-json [this]
    (let [rf #(iadd (iinc %) (-pretty-count-json %2))]
      (if (i< (count this) (i10))
        (reduce rf (i0) this)
        (idiv (imul (reduce rf (i0) (take 10 this)) (count this))
              (i10)))))
  nil
  (-pretty-count-json [this] 4)
  java.lang.Boolean
  (-pretty-count-json [this] (if this 4 5))
  java.lang.Object
  (-pretty-count-json [this] (count (.toString this))))

(deftype JsonPrettyTopContainer
  "Top level printer container for JSON printer."
  [config state coll block?]
  IIndentedMachine
  (-indent [this] (base-indent config))
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
                      (color config state :indent :syntax)
                      \newline [\space (base-indent config)])
      (print! batch bm state \space))))

(deftype JsonPrettyObjectPrinter
  "JSON Object printer machine type."
  {:predicate 'json-pretty-object-printer?}
  [config state coll indent block?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
                      (color config state :map :syntax) \{
                      (color config state :indent :syntax)
                      \newline [\space indent])
      (print-colored! batch bm config state
                      (color config state :map :syntax) \{)))
  (-print-after! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
              (color config state :indent :syntax)
              \newline [\space (prev-indent config this)]
              (color config state :map :syntax) \})
      (print-colored! batch bm config state
                      (color config state :map :syntax) \})))
  (-print-between! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
                      (color config state :map :syntax) \,
                      (color config state :indent :syntax)
                      \newline [\space indent])
      (print-colored! batch bm config state
                      (color config state :map :syntax)
                      \, \space))))

(defn json-pretty-object-printer
  "Constructor for JSON pretty Object printer machine."
  [config state coll indent]
  (let [key-fn (:key-fn config)
        value-fn (:value-fn config)
        kf #(let [k (ct/key %), v (value-fn k (ct/val %))]
              (when-not (identical? value-fn v) (pair (key-fn k) v)))
        transformed (keep kf coll)
        block? (json-pretty-mode config (seq transformed))]
    (->JsonPrettyObjectPrinter config state transformed
                               indent block?)))

(defprotocol IEntryState
  (-entry-state [this]))

(deftype JsonPrettyEntryContainer
  {:predicate 'entry-container?}
  [config state contents indent ^:unsynchronized-mutable key?]
  IEntryState
  (-entry-state [this] key?)
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] contents)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (set! key? false)
    (print-colored! batch bm config state
                    (color config state :map :syntax) \: \space)))

(deftype JsonPrettyContainer [config state coll indent block?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
                      (color config state :vector :syntax) \[
                      (color config state :indent :syntax)
                      \newline [\space indent])
      (print-colored! batch bm config state
                      (color config state :vector :syntax) \[)))
  (-print-after! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
                      (color config state :indent :syntax)
                      \newline [\space (prev-indent config this)]
                      (color config state :vector :syntax) \])
      (print-colored! batch bm config state
                      (color config state :vector :syntax) \])))
  (-print-between! [this bm batch parents]
    (if block?
      (print-colored! batch bm config state
                      (color config state :vector :syntax) \,
                      (color config state :indent :syntax)
                      \newline [\space indent])
      (print-colored! batch bm config state
                      (color config state :vector :syntax)
                      \, \space))))

(extend-protocol! IJsonPrettyPrinter
  java.lang.String
  (-print-pretty-json! [this config state bm batch parents]
    (let [machine (first parents)
          e? (and (entry-container? machine) (-entry-state machine))]
      (print-colored! batch bm config state
                      (if e?
                        (color config state :map-key :identifier)
                        (color config state :string))
                      \" [this #(to-escape config %) config] \")))
  clojure.lang.Sequential
  (-print-pretty-json! [this config state bm batch parents]
    (let [machine (first parents)]
      (if (json-pretty-object-printer? machine)
        (->JsonPrettyEntryContainer config state this
                                    (-indent machine) true)
        (->JsonPrettyContainer config state this
                               (next-indent config machine)
                               (json-pretty-mode config this)))))
  clojure.lang.IPersistentMap
  (-print-pretty-json! [this config state bm batch parents]
    (let [machine (first parents)]
      (json-pretty-object-printer config state this
                                  (next-indent config machine))))
  java.lang.Long
  (-print-pretty-json! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.lang.Long this))]
      (print-colored! batch bm config state
                      (color config state :integer :number) b)))
  clojure.lang.BigInt
  (-print-pretty-json! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^clojure.lang.BigInt this))]
      (print-colored! batch bm config state
                      (color config state :integer :number) b)))
  java.math.BigInteger
  (-print-pretty-json! [this config state bm batch parents]
   (let [b (string-to-batch! (.toString ^java.math.BigInteger this))]
     (print-colored! batch bm config state
                     (color config state :integer :number) b)))
  java.math.BigDecimal
  (-print-pretty-json! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.math.BigDecimal this))]
      (print-colored! batch bm config state
                      (color config state :integer :number) b)))
  java.lang.Double
  (-print-pretty-json! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.lang.Double this))]
      (print-colored! batch bm config state
                      (color config state :float :number) b)))
  nil
  (-print-pretty-json! [this config state bm batch parents]
    (print-colored! batch bm config state
                    (color config state :nil :literal)
                    json-null-batch))
  java.lang.Boolean
  (-print-pretty-json! [this config state bm batch parents]
    (if this
      (print-colored! batch bm config state
                      (color config state :true :boolean :literal)
                      json-true-batch)
      (print-colored! batch bm config state
                      (color config state :false :boolean :literal)
                      json-false-batch)))
  java.nio.Buffer
  (-print-pretty-json! [this config state bm batch parents]
    (print! batch bm state this))
  java.lang.Object
  (-print-pretty-json! [this config state bm batch parents]
    (invalid-item-handler batch bm state config this parents)))

(defrecord JsonPrettyPrinterFactory
  "JSON Pretty Printer Factory record."
  [key-encode-fn value-encode-fn escape-js-separators escape-unicode
   escape-slash invalid-item indent-offset indent-size
   inline-threshold color-fn pretty-level-limit
   pretty-item-limit pretty-string-limit color?]
  IPrinterMachineFactory
  (-printer-config [this]
    {:key-fn key-encode-fn
     :value-fn value-encode-fn
     :escape-js-separators escape-js-separators
     :escape-unicode escape-unicode
     :escape-slash escape-slash
     :invalid-item invalid-item
     :indent-offset indent-offset
     :indent-size indent-size
     :inline-threshold inline-threshold
     :color-fn color-fn
     :color? color?
     :level-limit-pred
     (fn [p] (and pretty-level-limit
                 (pos? pretty-level-limit)
                 (not (class-instance?
                       dunaj.format.json.JsonPrettyObjectPrinter
                       (first p)))
                 (< pretty-level-limit
                    (count (vec (remove entry-container? p))))))
     :level-limit-print-fn
     (fn [batch bm state]
       (let [config {:color-fn color-fn}]
         (print-colored!
          batch bm config state
          (dunaj.format.printer/color config state :limit :syntax)
          json-level-limit-batch)))
     :item-limit-batch
     (let [batches [(color-fn :limit :syntax)
                    json-item-limit-batch default-color]]
       (string-to-batch!
        (apply ->str (map #(.clear ^java.nio.Buffer %) batches))))
     :string-limit pretty-string-limit
     :item-limit pretty-item-limit})
  (-printer-from-type [this] (keyword->class :object))
  (-printer-to-type [this] (keyword->class :char))
  (-top-container [this config state coll]
    (->JsonPrettyTopContainer
     config state coll (json-pretty-mode config coll)))
  (-dispatch-printer
    [this config state item bm batch parents]
    (-print-pretty-json! item config state bm batch parents))
  IPrinterFactory
  (-print [this]
    (pretty-printer-engine this))
  (-print [this coll]
    (pretty-printer-engine this coll)))


;;;; Formatter

(defrecord JsonFormatterFactory
  "JSON Formatter Factory record."
  [key-decode-fn key-encode-fn value-decode-fn value-encode-fn
   bigdec escape-js-separators escape-unicode escape-slash
   incomplete-mode lazy? invalid-item token-item-limit
   container-item-limit container-level-limit]
  IParserMachineFactory
  (-parser-from-type [this] (keyword->class :char))
  (-parser-to-type [this] (keyword->class :object))
  (-parser-config [this]
    {:key-fn key-decode-fn
     :value-fn value-decode-fn
     :bigdec bigdec
     :token-item-limit token-item-limit
     :container-item-limit container-item-limit
     :container-level-limit container-level-limit
     :incomplete-mode incomplete-mode})
  (-dispatch-tokenizer [this config state item]
    (let [x (iint item)]
      (cond (json-whitespace? x) this
            (i== x (iQUOTE)) (json-string-literal config state x)
            (i== x (iLBRACKET)) :json-array-open
            (i== x (iRBRACKET)) :json-array-close
            (i== x (iLBRACE)) :json-object-open
            (i== x (iRBRACE)) :json-object-close
            (or (i== x (iMINUS)) (idigit? x))
            (json-number-literal config state x)
            (i== x (iSMALL_T)) (true-literal config state x)
            (i== x (iSMALL_F)) (false-literal config state x)
            (i== x (iSMALL_N)) (null-literal config state x)
            :else (perror "invalid item " item))))
  (-dispatch-parser [this config state token parents]
    (cond (identical? :json-array-open token)
          (json-array-container config state)
          (identical? :json-object-open token)
          (json-object-container config state)
          :else token))
  ILazyParserMachineFactory
  (-dispatch-lazy-parser [this config state token parents]
    (-dispatch-parser this config state token parents))
  IParserFactory
  (-parse [this]
    (when lazy? (throw (unsupported-operation)))
    (parser-engine this))
  (-parse [this coll]
    ((if lazy? lazy-parser-engine parser-engine) this coll))
  IPrinterMachineFactory
  (-printer-config [this]
    {:key-fn key-encode-fn
     :value-fn value-encode-fn
     :escape-js-separators escape-js-separators
     :escape-unicode escape-unicode
     :escape-slash escape-slash
     :invalid-item invalid-item})
  (-printer-from-type [this] (keyword->class :object))
  (-printer-to-type [this] (keyword->class :char))
  (-top-container [this config state coll]
    (->JsonTopContainer config state coll))
  (-dispatch-printer [this config state item bm batch parents]
    (-print-json! item config state bm batch parents))
  IPrinterFactory
  (-print [this] (printer-engine this))
  (-print [this coll] (printer-engine this coll)))

(defn default-key-encode-fn
  [k]
  (if (class-instance? clojure.lang.Named k) (name k) (->str k)))

;;;; Public API

(def json :- (I IParserFactory IPrinterFactory)
  "A JSON formatter factory."
  {:added v1
   :see '[lazy-json pretty-json]}
  (->JsonFormatterFactory
   identity default-key-encode-fn (fn [k v] v) (fn [k v] v) false true
   true true :keep false nil 1000000 1000000 1000000))

(def lazy-json :- IParserFactory
  "A Lazy JSON formatter factory."
  {:added v1
   :see '[json pretty-json]}
  (assoc json :lazy? true))

(def json-colorer-map :- {}
  {:limit cyan})

(def pretty-json :- IPrinterFactory
  "JSON printer factory with pretty printing."
  {:added v1
   :see '[json lazy-json]}
  (->JsonPrettyPrinterFactory
   default-key-encode-fn (fn [k v] v) true true true nil 0 2 70
   (custom-colorer json-colorer-map) 5 100 200 false))


;;;; Scratch

(scratch [[clojure.core :as cc :refer [cycle]]
          [dunaj.coll.default :refer [vec]]
          [dunaj.string :refer [str]]
          [dunaj.coll :as dc :refer [conj count first]]
          [dunaj.format :refer [parse print parse-whole]]
          [clojure.core.async]
          [dunaj.concurrent.port :as dp]]
  []

  ;; Parser

  ;; token item limit
  (seq (parse json (cons \" (repeat \space))))
  (vec (parse json (cons \" (repeat \space))))

  ;; container level limit
  (seq (parse json (repeat \[)))
  (vec (parse json (repeat \[)))
  (parse lazy-json (repeat \[))

  ;; container item limit
  (seq (parse json (cons \[ (cycle [\1 \space]))))
  (vec (parse json (cons \[ (cycle [\1 \space]))))
  (parse lazy-json (cons \[ (cycle [\1 \space])))

  ;; benchmark
  (clojure.core/require '[dunaj.resource.http])
  (clojure.core/require '[dunaj.resource :refer [slurp with-scope]])

  (def url "https://www.googleapis.com/freebase/v1/topic/m/0fkf28")
  (def s (with-scope (str (slurp url))))
  (def s (str (cc/take 50000 (cc/repeat \[)))) ;; stack overflow

  (cc/time (count (vec (parse json s))))
  (cc/time (count (seq (parse json s))))
  (cc/time (count (parse lazy-json s)))

  (cc/require 'clojure.data.json)
  (cc/require 'cheshire.core)
  (cc/time (count (vec (clojure.data.json/read-str s))))
  (cc/time (count (cheshire.core/parse-string s)))

  (cc/= (first (vec (parse json s)))
        (first (dc/transduce (parse json) dc/conj [] s))
        (first (parse json s))
        (first (parse lazy-json s))
        (vec (clojure.data.json/read-str s))
        (cheshire.core/parse-string s))

  ;; channel parser

  (def c (dp/chan 10 (parse json)))

  (dp/thread (loop [x (dp/<!! c)]
               (if (nil? x)
                 (clojure.core/println "baj baj")
                 (do (clojure.core/println "got " x)
                     (recur (dp/<!! c))))))

  (dp/>!! c \t)
  (dp/>!! c \r)
  (dp/>!! c \u)
  (dp/>!! c \e)
  (dp/>!! c \space)
  (dp/close! c)

  ;; Printer

  (def coll (parse-whole json s))

  (= (str (print json [coll]))
     (apply ->str (seq (print json [coll])))
     (dc/transduce (print json) ->str "" [coll])
     (clojure.data.json/write-str coll))

  ;; benchmark
  (cc/time (count (vec (print json [coll]))))
  (cc/time (count (dc/transduce (print json) conj [] coll)))
  (cc/time (count (dunaj.coll.helper/reduce-batched*
                   (print json [coll]) dunaj.function/nop "" )))
  (cc/time (count (str (print json [coll]))))
  (cc/time (count (seq (print json [coll]))))
  (cc/time (count (clojure.data.json/write-str coll)))
  (cc/time (count (cheshire.core/generate-string coll)))

  ;; Pretty Printer

  (cc/defn dopp [x]
    (cc/println (str (print pretty-json x))))

  (cc/defn dopp [x]
    (cc/println (str (print (assoc pretty-json :color? true) x))))

  (dopp coll)

  ;; pretty string
  (dopp [(str (cc/range 1000))])

  ;; pretty level
  (dopp [0 [1 [2 [3 [4 [5 [6 [7 [8 [9]]]]]]]]]])
  (dopp [0 {1 {2 {3 {4 {5 {6 {7 {8 {9 10}}}}}}}} 2 3}])

  ;; pretty item
  (dopp [0 (vec (cc/range 1000))])

)
