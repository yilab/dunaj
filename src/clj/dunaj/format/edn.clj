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

(ns dunaj.format.edn
  "EDN liberal formatter with optional lazy support.

  Parser assumes input is valid EDN (WIP),
  protects against malicious literals and by default handles
  incomplete input. Not suitable if you need a strict parser.

  Printer supports naive pretty mode with ANSI color support."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [scratch v1]]
   [dunaj.type :refer [AnyFn Any Fn Maybe U I]]
   [dunaj.boolean :refer [Boolean and or not true? false?]]
   [dunaj.host :refer [set! keyword->class class-instance?]]
   [dunaj.host.int :refer
    [Int iint iinc i== i< isub izero? idec ineg? i> i< i<< imax iadd
     iloop i0 i1 i2 i3 i4 i5 i8 iFF ione? i-1 imul i10 idiv idigit?
     ioctal? ihexa? ihexa->int idigit->int ismall-letter?
     icapital-letter? iZERO iUS iDEL iSMALL_O iHT iCR iLF iCOLON
     iSPACE iCOMMA iLBRACKET iRBRACKET iLT iQUOTE iAPOS iBACKSLASH
     iSLASH iLBRACE iRBRACE iTILDE iSMALL_B iSMALL_F iSMALL_N
     iSMALL_R iMINUS iSTAR iSMALL_U iSMALL_T iPLUS iCAPITAL_E
     iSMALL_E iBANG iHASH iCAPITAL_N iSMALL_X iCAPITAL_X iBS iHT iCR
     iLF iFF iGT iSEMICOLON iLPAR iRPAR iUNDERSCORE iQMARK iDOLLAR
     iEQ iCAPITAL_R iSMALL_R iDOT iCAPITAL_M iPERCENT iAMP]]
   [dunaj.math :refer [max min < integer? == > neg? + inc zero? one?
                       >= inc dec number? - pos? * /]]
   [dunaj.compare :refer [identical? nil? = sentinel defsentinel]]
   [dunaj.poly :refer
    [defprotocol defrecord deftype extend-protocol!]]
   [dunaj.coll :refer
    [first rest seq second reduce contains? empty? slice count nth get
     ISeq ISeqable sequential? single? reduced conj
     assoc map? settle! edit conj! assoc!]]
   [dunaj.coll.helper]
   [dunaj.threading :refer [->]]
   [dunaj.flow :refer [if when when-not cond do let if-let loop recur
                       case condp if-not when-let]]
   [dunaj.error :refer [throw illegal-state ex-info]]
   [dunaj.poly :refer [satisfies?]]
   [dunaj.state :refer [reset! IReference]]
   [dunaj.state.var :refer [Var def declare]]
   [dunaj.identifier :refer [INamed name symbol keyword symbol]]
   [dunaj.char :refer [Char char]]
   [dunaj.string :refer [->str empty-string str index-of string?]]
   [dunaj.function :refer [fn defn identity constantly apply]]
   [dunaj.host.batch :refer [batch-manager]]
   [dunaj.host.array :refer [array-manager array to-array]]
   [dunaj.coll.default :refer [empty-map empty-vec vec empty-set]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.lazy-seq-map :refer [lazy-seq->map]]
   [dunaj.coll.lazy-seq-set :refer [lazy-seq->set]]
   [dunaj.coll.linked-list :refer [reversed-list-builder]]
   [dunaj.coll.tuple :refer [tuple pair]]
   [dunaj.coll.recipe :refer [map keep mapcat take remove take-while]]
   [dunaj.format :refer [IParserFactory IPrinterFactory parse print]]
   [dunaj.format.helper :refer [string-cat-batch! string-to-batch!]]
   [dunaj.format.parser :refer
    [parser-engine string-literal-constructor leftover IParserMachine
     ITokenizerMachine -analyze-eof! container-parser lazy-level-limit
     skipping-tokenizer tokenizer-engine IParserMachineFactory
     -parser-config ILazyParserMachine -dispatch-parser
     ILazyParserMachineFactory literal-tokenizer ignore-token drop-one
     lazy-parser-engine perror eof-handler keep? ignore? -parse-value!
     lazy-item-limit take-one take-until-token drop-until-token]]
   [dunaj.format.printer :refer
    [IContainerPrinterMachine -printer-to-type IPrinterMachineFactory
     printer-engine -indent invalid-item-handler cyan print-colored!
     green yellow magenta red default-color prev-indent print!
     IIndentedMachine next-indent base-indent custom-colorer
     default-colorer color pretty-printer-engine]]))


;;;; Implementation details

(defsentinel nothing)

;;;; Parser

(defn edn-whitespace? :- Boolean
  "Returns true if `x` represents an EDN whitespace character,
  otherwise returns false."
  [x :- Int]
  (or (java.lang.Character/isWhitespace x) (i== x (iCOMMA))))

(defn edn-newline? :- Boolean
  "Returns true if `x` represents an EDN newline character,
  otherwise returns false."
  [x :- Int]
  (i== x (iLF)))

(defn edn-delimiter? :- Boolean
  "Returns true if `x` represents an EDN delimiter character,
  otherwise returns false."
  [x :- Int]
  (or (edn-whitespace? x) (i== x (iQUOTE)) (i== x (iBACKSLASH))
      (i== x (iSEMICOLON)) (i== x (iLBRACKET)) (i== x (iRBRACKET))
      (i== x (iLBRACE)) (i== x (iRBRACE))
      (i== x (iLPAR)) (i== x (iRPAR))))

;;; special tokens

(def edn-bracket-close
  "A token for EDN close bracket ']'"
  (sentinel))

(def edn-brace-close
  "A token for EDN close brace '}'"
  (sentinel))

(def edn-par-close
  "A token for EDN close parentheses ')'"
  (sentinel))

;;; map container parser machine

(deftype EdnMapContainer
  "EDN Map container parser machine type."
  [config ^:unsynchronized-mutable contents
   ^:unsynchronized-mutable key]
  IParserMachine
  (-parse-value! [this token parents]
    (cond (identical? edn-brace-close token) (settle! contents)
          (nothing? key) (do (set! key token) this)
          :else (let [c (count contents)
                      nc (assoc! contents key token)]
                  (when (== c (count nc))
                    (perror "duplicate entry in EDN map"))
                  (set! contents nc)
                  (set! key nothing)
                  this)))
  (-parse-eof! [this parents]
    (eof-handler this config (settle! contents)
                 "EDN map container parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (let [pred? #(identical? edn-brace-close %)
          item-limit (imul (i2) (iint (lazy-item-limit config)))
          level-limit (lazy-level-limit config)
          keyvals (take-until-token parents pred?
                                    item-limit level-limit coll)]
      (pair (lazy-seq->map keyvals)
            (drop-until-token parents pred?
                              item-limit level-limit coll)))))

(defn edn-map-container
  "Constructor for EDN map container parser machine"
  [config state]
  (->EdnMapContainer config (edit empty-map) nothing))

;;; other container parser machines

(container-parser "edn-lst" edn-par-close nil reversed-list-builder)
(container-parser "edn-vec" edn-bracket-close nil empty-vec)

(deftype EdnSetContainer
  [config ^:unsynchronized-mutable contents]
  IParserMachine
  (-parse-value! [this value parents]
    (cond (identical? edn-brace-close value)
          (settle! contents)
          :else (let [c (count contents)
                      nc (conj! contents value)]
                  (when (== c (count nc))
                    (perror "duplicate entry in EDN set"))
                  (set! contents nc)
                  this)))
  (-parse-eof! [this parents]
    (eof-handler this config (settle! contents)
                 "EDN set container parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (let [cil (lazy-item-limit config)
          cll (lazy-level-limit config)
          pred? #(identical? edn-brace-close %)]
      (pair
       (lazy-seq->set (take-until-token parents pred? cil cll coll))
       (drop-until-token parents pred? cil cll coll)))))

(defn edn-set-container
  "Constructor for EDN set container parser machine."
  [config state]
  (->EdnSetContainer config (edit empty-set)))

;;; tagged literal parser

(defrecord UnknownTaggedLiteral
  "Record for unknown tagged literal."
  [tag val])

(defn ^:private default-tag-reader
  "Returns new instance of UnknownTaggedLiteral"
  [tag val]
  (->UnknownTaggedLiteral tag val))

(deftype TaggedParser
  "Type for tagged literal parser."
  [config sym record?]
  IParserMachine
  (-parse-value! [this value parents]
    (let [sn (dunaj.identifier/name sym)
          ctor? (index-of sn \.)]
      (if (and ctor? record?)
        ;; constructor or reader
        (do (when-not (:read-eval config)
              (perror "record construction not allowed"))
            (let [c (java.lang.Class/forName
                     sn false (clojure.lang.RT/baseLoader))]
              (if (map? value)
                (clojure.lang.Reflector/invokeStaticMethod
                 c "create" (to-array [value]))
                (clojure.lang.Reflector/invokeConstructor
                 c (to-array value)))))
        ;; tagged literal
        (if-let [dr (or
                     (get (:tag-readers config) sym)
                     (when-let [x (get
                                   clojure.core/default-data-readers
                                   sym)]
                       @x))]
          (dr value)
          ((or (:default-tag-reader config) default-tag-reader)
           sym value)))))
  (-parse-eof! [this parents]
    (eof-handler this config this "tagged parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (let [level-limit (lazy-level-limit config)
          value (take-one parents level-limit coll)]
      (pair (-parse-value! this value parents)
            (drop-one parents level-limit coll)))))

;;; discard parser

(deftype DiscardParser
  "Type for discard macro parser."
  [config ^:unsynchronized-mutable done?]
  IParserMachine
  (-parse-value! [this value parents]
    (if done? value (do (set! done? true) this)))
  (-parse-eof! [this parents]
    (eof-handler this config this "discard parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (let [level-limit (lazy-level-limit config)]
      (pair this (drop-one parents level-limit coll)))))

;;; number literal

(defn edn-number-start? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of an EDN Number Literal, otherwise returns false."
  [x :- Int]
  (or (idigit? x) (i== x (iMINUS)) (i== x (iPLUS))))

(defn edn-number-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of an EDN Number Literal, otherwise returns false."
  [x :- Int]
  (or (idigit? x) (i== x (iMINUS)) (i== x (iPLUS)) (i== x (iDOT))
      (i== x (iSMALL_E)) (i== x (iCAPITAL_E))
      (i== x (iCAPITAL_N)) (i== x (iCAPITAL_M))))

(deftype EdnNumberLiteralTokenizerMachine
  "Tokenizer Machine type for an EDN Number Literal"
  [config state ^:unsynchronized-mutable
   ^java.lang.StringBuilder sb
   ^:unsynchronized-mutable negative?
   ^:unsynchronized-mutable sign?
   ^:unsynchronized-mutable decimal?
   ^:unsynchronized-mutable bigdec?
   ^:unsynchronized-mutable bigint?]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch :- java.nio.CharBuffer batch
          begin (.position batch)]
      (loop []
        (if-not (.hasRemaining batch)
          (do (string-cat-batch!
               sb batch begin (.position batch) state)
              this)
          (let [pos (.position batch)
                c (.get batch)
                x (iint c)]
            (cond
             (edn-number-item? x)
             (do (cond (i== x (iDOT)) (set! decimal? true)
                       (i== x (iCAPITAL_E)) (set! decimal? true)
                       (i== x (iSMALL_E)) (set! decimal? true)
                       (i== x (iCAPITAL_M)) (do (set! decimal? true)
                                                (set! bigdec? true))
                       (i== x (iCAPITAL_N)) (set! bigint? true))
                 (recur))
             (edn-delimiter? x)
             (do (string-cat-batch! sb batch begin pos state)
                 (.position batch pos)
                 (-analyze-eof! this))
             :else (perror "invalid number item " c)))))))
  (-analyze-eof! [this]
    (let [s ^java.lang.String (settle! sb)]
      (if decimal?
        (let [s (if bigdec? (slice s 0 (dec (count s))) s)]
          (cond (not (or bigdec? (:bigdec config)))
                (java.lang.Double/valueOf ^java.lang.String s)
                clojure.core/*math-context*
                (java.math.BigDecimal.
                 ^java.lang.String s clojure.core/*math-context*)
                :else
                (java.math.BigDecimal. ^java.lang.String s)))
        (let [from (if sign? 1 0)
              v (if bigint?
                  (slice s from (dec (count s)))
                  (slice s from))
              bn (java.math.BigInteger. ^java.lang.String v)
              n (if (or bigint? (>= (.bitLength bn) 64))
                  (clojure.lang.BigInt/fromBigInteger bn)
                  (.longValue bn))]
          (if negative? (* -1 n) n))))))

(defn edn-number-literal
  "Returns EDN Number Literal Tokenizer Machine."
  [config state item]
  (let [x (iint item)
        negative? (i== x (iMINUS))
        sign? (or (i== x (iMINUS)) (i== x (iPLUS)))
        ts :- java.lang.StringBuilder (edit empty-string)]
    (->EdnNumberLiteralTokenizerMachine
     config state (.append ts (char item))
     negative? sign? false false false)))

;;; string literal

(defn ^:private from-escape :- Char
  "Returns character which is represented by an escape character with
  Unicode code point `x`.
  Throws if escape character is not recognized."
  [x :- Int]
  (cond (i== x (iQUOTE)) \"
        (i== x (iBACKSLASH)) \\
        (i== x (iSMALL_N)) \u000a
        (i== x (iSMALL_R)) \u000d
        (i== x (iSMALL_T)) \u0009
        :else (perror "invalid escape character " (char x))))

(def edn-string-literal
  "Function which returns an EDN String Literal Tokenizer Machine."
  (string-literal-constructor (constantly false) from-escape true))

;;; character literal

(def edn-character-map
  {"newline" \newline
   "return" \return
   "space" \space
   "tab" \tab})

(deftype EdnCharacterLiteralTokenizerMachine
  "Tokenizer Machine type for an EDN Character Literal."
  [config state ^:unsynchronized-mutable
   ^java.lang.StringBuilder sb character-map octal?
   delimiter-pred]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch ^java.nio.CharBuffer batch
          begin (.position batch)]
      (iloop [max (i10)]
        (when (izero? max) ;; malicious input protection
          (perror "invalid edn character " (settle! sb)))
        (if-not (.hasRemaining batch)
          (do (string-cat-batch!
               sb batch begin (.position batch) state)
              this)
          (let [pos (.position batch)
                c (.get batch)
                x (iint c)]
            (if (and (or (not (empty? sb)) (i< max (i10)))
                     (delimiter-pred x))
              (do (string-cat-batch! sb batch begin pos state)
                  (.position batch pos)
                  (-analyze-eof! this))
              (recur (idec max))))))))
  (-analyze-eof! [this]
    (let [s ^java.lang.String (settle! sb)
          c (first s)
          x (iint c)]
      (cond (single? s) c
            (and (i== x (iSMALL_U)) (== 5 (count s)))
            (char (java.lang.Integer/valueOf
                   ^java.lang.String (slice s 1) 16))
            (and octal? (i== x (iSMALL_O)) (< (count s) 5))
            (char (java.lang.Integer/valueOf
                   ^java.lang.String (slice s 1) 8))
            :else
            (or (character-map s)
                (perror "invalid edn character " s))))))

(defn edn-character-literal
  "Returns EDN Character Literal Tokenizer Machine."
  [config state item character-map octal? delimiter-pred]
  (let [ts ^java.lang.StringBuilder (edit empty-string)]
    (->EdnCharacterLiteralTokenizerMachine
     config state ts character-map octal? delimiter-pred)))

;;; comment literal

(deftype EdnLineskipTokenizerMachine
  "Tokenizer Machine type for line skipper."
  [config]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch ^java.nio.CharBuffer batch]
      (loop []
        (cond (not (.hasRemaining batch)) this
              (edn-newline? (iint (.get batch))) ignore-token
              :else (recur)))))
  (-analyze-eof! [this] this))

(defn edn-lineskip-literal
  "Returns Tokenizer Machine which skips until end of the line."
  [config state item]
  (->EdnLineskipTokenizerMachine config))

;;; symbol literal

(defn edn-symbol-start? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of an EDN Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (java.lang.Character/isAlphabetic x)
      (i== x (iDOT)) (i== x (iSTAR)) (i== x (iPLUS)) (i== x (iBANG))
      (i== x (iMINUS)) (i== x (iUNDERSCORE)) (i== x (iQMARK))
      (i== x (iDOLLAR)) (i== x (iPERCENT)) (i== x (iAMP))
      (i== x (iEQ)) (i== x (iLT)) (i== x (iGT)) (i== x (iSLASH))
      (i== x (iCOLON))))

(defn edn-symbol-digit? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid digit, otherwise returns false."
  [x :- Int]
  (java.lang.Character/isDigit x))

(defn edn-symbol-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of an EDN Symbol Literal, otherwise returns false."
  [x :- Int]
  (or (edn-symbol-start? x) (i== x (iHASH)) (edn-symbol-digit? x)))

(defn edn-strict-symbol-start? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of an EDN Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (java.lang.Character/isAlphabetic x)
      (i== x (iDOT)) (i== x (iSTAR)) (i== x (iPLUS)) (i== x (iBANG))
      (i== x (iMINUS)) (i== x (iUNDERSCORE)) (i== x (iQMARK))
      (i== x (iDOLLAR)) (i== x (iPERCENT)) (i== x (iAMP))
      (i== x (iEQ)) (i== x (iLT)) (i== x (iGT))))

(defn edn-nondigit-symbol-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of an EDN Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (edn-strict-symbol-start? x) (i== x (iHASH)) (i== x (iCOLON))))

(defn edn-strict-symbol-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of an EDN Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (edn-nondigit-symbol-item? x)
      (java.lang.Character/isDigit x)))

(defn edn-maybe-symbol-start? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of an EDN Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (i== x (iMINUS)) (i== x (iPLUS)) (i== x (iDOT))))

(defn ^:private valid-symbol? :- Boolean
  "Returns true if string `s` is a valid EDN symbol, otherwise
  returns false."
  [s]
  (let [fail (reduced 4)
        cf
        (fn [i c]
          (let [x (iint c)]
            (condp == i
              0 (cond (edn-maybe-symbol-start? x) 1
                      (edn-strict-symbol-start? x) 2
                      (i== x (iSLASH)) 3
                      :else fail)
              1 (cond (edn-nondigit-symbol-item? x) 2
                      (i== x (iSLASH)) 4
                      :else fail)
              2 (cond (edn-strict-symbol-item? x) 2
                      (i== x (iSLASH)) 4
                      :else fail)
              3 fail
              4 (cond (edn-maybe-symbol-start? x) 5
                      (edn-strict-symbol-start? x) 6
                      ;; (i== x (iSLASH)) 3
                      :else fail)
              5 (if (edn-nondigit-symbol-item? x) 6 fail)
              6 (if (edn-strict-symbol-item? x) 6 fail))))]
    (not (== 4 (reduce cf 0 s)))))

(defn ^:private valid-keyword? :- Boolean
  "Returns true if string `s` is a valid EDN keyword, otherwise
  returns false."
  ([s] (valid-keyword? s true))
  ([s leading-colon]
    (let [fail (reduced 4)
          cf
          (fn [i c]
            (let [x (iint c)]
              (condp == i
                0 (if (i== x (iCOLON)) 1 fail)
                1 (cond (i== x (iCOLON)) fail
                        (edn-strict-symbol-item? x) 2
                        :else fail)
                2 (cond (edn-strict-symbol-item? x) 2
                        (i== x (iSLASH)) 4
                        :else fail)
                3 fail
                4 (cond (edn-maybe-symbol-start? x) 5
                        (edn-strict-symbol-start? x) 6
                        ;; (i== x (iSLASH)) 3
                        :else fail)
                5 (if (edn-nondigit-symbol-item? x) 6 fail)
                6 (if (edn-strict-symbol-item? x) 6 fail))))]
      (not (== 4 (reduce cf (if leading-colon 0 1) s))))))

(deftype EdnSymbolLiteralTokenizerMachine
  "Tokenizer Machine type for an EDN Symbol Literal."
  [config state ^:unsynchronized-mutable
   ^java.lang.StringBuilder sb ^:unsynchronized-mutable special?
   keyword? tagged?]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch ^java.nio.CharBuffer batch
          begin (.position batch)]
      (loop []
        (if-not (.hasRemaining batch)
          (do (string-cat-batch!
               sb batch begin (.position batch) state)
              this)
          (let [pos (.position batch)
                c (.get batch)
                x (iint c)]
            (cond
             (edn-delimiter? x)
             (do (string-cat-batch! sb batch begin pos state)
                 (.position batch pos)
                 (-analyze-eof! this))
             (and special? (edn-symbol-digit? x))
             (do (.position batch pos)
                 (edn-number-literal config state (.charAt sb 0)))
             (edn-symbol-item? x) (do (set! special? false) (recur))
             :else (perror "invalid symbol character " c)))))))
  (-analyze-eof! [this]
    (let [s ^java.lang.String (settle! sb)]
      (when-not (if keyword?
                  (valid-keyword? s false)
                  (valid-symbol? s))
        (perror "invalid identifier " s))
      (cond tagged? (->TaggedParser config (symbol s) false)
            keyword? (keyword s)
            (= s "true") true
            (= s "false") false
            (= s "nil") nil
            :else (symbol s)))))

(defn edn-symbol-literal
  "Returns EDN Symbol Literal Tokenizer Machine."
  ([config state item]
     (edn-symbol-literal config state item false))
  ([config state item tagged?]
     (let [x (iint item)
           special?
           (or (i== x (iMINUS)) (i== x (iPLUS)) (i== x (iDOT)))
           keyword? (i== x (iCOLON))
           ts ^java.lang.StringBuilder (edit empty-string)
           ts (if keyword? ts (.append ts (char item)))]
       (->EdnSymbolLiteralTokenizerMachine
        config state ts special? keyword? tagged?))))

;;; dispatch literal

(deftype EdnDispatchTokenizerMachine
  "Tokenizer Machine type for an EDN Dispatch."
  [config state]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (if (.hasRemaining ^java.nio.CharBuffer batch)
      (let [x (iint (.get ^java.nio.CharBuffer batch))]
        (cond (i== x (iUNDERSCORE)) (->DiscardParser config false)
              (i== x (iLBRACE)) (edn-set-container config state)
              (java.lang.Character/isAlphabetic x)
              (edn-symbol-literal config state x true)
              :else (perror "unknown dispatch character" (char x))))
      this))
  (-analyze-eof! [this]
    (eof-handler this config this "dispatch tokenizer machine")))

(defn edn-dispatch
  "Returns EDN Dispatch Tokenizer Machine."
  [config state item]
  (->EdnDispatchTokenizerMachine config state))


;;;; Printer

(deftype EdnTopContainer
  "Top level printer container for EDN printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

(def edn-true-batch (string-to-batch! "true"))
(def edn-false-batch (string-to-batch! "false"))
(def edn-nil-batch (string-to-batch! "nil"))
(def edn-quote-batch (string-to-batch! "\\\""))
(def edn-backslash-batch (string-to-batch! "\\\\"))
(def edn-ht-batch (string-to-batch! "\\t"))
(def edn-lf-batch (string-to-batch! "\\n"))
(def edn-cr-batch (string-to-batch! "\\r"))
(def edn-space-batch (string-to-batch! "\\space"))
(def edn-tab-batch (string-to-batch! "\\tab"))
(def edn-newline-batch (string-to-batch! "\\newline"))
(def edn-return-batch (string-to-batch! "\\return"))
(def edn-level-limit-batch (string-to-batch! "#"))
(def edn-item-limit-batch (string-to-batch! "..."))

;;; printer for EDN map

(deftype EdnMapPrinter
  "EDN Map printer machine type."
  {:predicate 'edn-map-printer?}
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (print! batch bm state \{))
  (-print-after! [this bm batch parents]
    (print! batch bm state \}))
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

(defn edn-map-printer
  "Constructor for EDN map printer machine"
  [config state coll]
  (->EdnMapPrinter config state coll))

(deftype EdnEntryContainerPrinter
  [config state contents]
  IContainerPrinterMachine
  (-children [this parents] contents)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

;;; printer for EDN vector

(deftype EdnVecContainerPrinter
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (print! batch bm state \[))
  (-print-after! [this bm batch parents]
    (print! batch bm state \]))
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

;;; printer for EDN vector

(deftype EdnLstContainerPrinter
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (print! batch bm state \())
  (-print-after! [this bm batch parents]
    (print! batch bm state \)))
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

;;; printer for EDN set

(deftype EdnSetContainerPrinter
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (print! batch bm state \# \{))
  (-print-after! [this bm batch parents]
    (print! batch bm state \}))
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

;;; printing strings

(def ^:private zeroes "0000")

(defn ^:private to-escape
  "Returns batch containing escape sequence or nil, if no
  escape sequence is needed."
  [config x]
  (let [ci (iint x)]
    (cond (i== ci (iQUOTE)) edn-quote-batch
          (i== ci (iBACKSLASH)) edn-backslash-batch
          (and (i< (iUS) ci) (i< ci (iDEL))) nil
          (i== ci (iHT)) edn-ht-batch
          (i== ci (iLF)) edn-lf-batch
          (i== ci (iCR)) edn-cr-batch
          :else
          (when (:escape-unicode config)
            (let [ns (java.lang.Integer/toString ci 16)]
              (string-to-batch!
               (->str "\\u" (.substring ^java.lang.String zeroes
                                        (count ns)) ns)))))))

(defn ^:private to-char-escape
  "Returns batch containing escape sequence or nil, if no
  escape sequence is needed."
  [config x]
  (let [ci (iint x)]
    (cond (i== ci (iSPACE)) edn-space-batch
          (and (i< (iUS) ci) (i< ci (iDEL)))
          (string-to-batch! (->str \\ x))
          (i== ci (iHT)) edn-tab-batch
          (i== ci (iLF)) edn-newline-batch
          (i== ci (iCR)) edn-return-batch
          :else
          (let [ns (java.lang.Integer/toString ci 16)]
            (string-to-batch!
             (->str "\\u" (.substring ^java.lang.String zeroes
                                      (count ns)) ns))))))

;;; edn printer protocol with basic implementations

(defprotocol IEdnPrinter
  (-print-edn!
    "Returns result or printing `this` as a edn. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(extend-protocol! IEdnPrinter
  java.lang.String
  (-print-edn! [this config state bm batch parents]
    (print! batch bm state \" [this #(to-escape config %)] \"))
  java.util.Map$Entry
  (-print-edn! [this config state bm batch parents]
    (->EdnEntryContainerPrinter config state this))
  clojure.lang.IPersistentList
  (-print-edn! [this config state bm batch parents]
    (->EdnLstContainerPrinter config state this))
  clojure.lang.ASeq
  (-print-edn! [this config state bm batch parents]
    (->EdnLstContainerPrinter config state this))
  clojure.lang.IPersistentVector
  (-print-edn! [this config state bm batch parents]
    (if (edn-map-printer? (first parents))
      (->EdnEntryContainerPrinter config state this)
      (->EdnVecContainerPrinter config state this)))
  clojure.lang.IPersistentSet
  (-print-edn! [this config state bm batch parents]
    (->EdnSetContainerPrinter config state this))
  clojure.lang.IPersistentMap
  (-print-edn! [this config state bm batch parents]
    (edn-map-printer config state this))
  java.lang.Long
  (-print-edn! [this config state bm batch parents]
    (string-to-batch! (.toString ^java.lang.Long this) bm batch))
  java.lang.Double
  (-print-edn! [this config state bm batch parents]
    (string-to-batch! (.toString ^java.lang.Double this) bm batch))
  java.math.BigDecimal
  (-print-edn! [this config state bm batch parents]
    (print! batch bm state
            (string-to-batch! (.toString ^java.math.BigDecimal this))
            \M))
  java.math.BigInteger
  (-print-edn! [this config state bm batch parents]
    (print! batch bm state
            (string-to-batch! (.toString ^java.math.BigInteger this))
            \N))
  clojure.lang.BigInt
  (-print-edn! [this config state bm batch parents]
    (print! batch bm state
            (string-to-batch! (.toString ^clojure.lang.BigInt this))
            \N))
  java.lang.Character
  (-print-edn! [this config state bm batch parents]
    (to-char-escape config this))
  clojure.lang.Symbol
  (-print-edn! [this config state bm batch parents]
    (let [s (->str this)]
      (when-not (valid-symbol? s) (perror "invalid symbol " s))
      (string-to-batch! s)))
  clojure.lang.Keyword
  (-print-edn! [this config state bm batch parents]
    (let [s (->str this)]
      (when-not (valid-keyword? s) (perror "invalid keyword " s))
      (string-to-batch! s)))
  nil
  (-print-edn! [this config state bm batch parents]
    (print! batch bm state edn-nil-batch))
  java.lang.Boolean
  (-print-edn! [this config state bm batch parents]
    (if this
      (print! batch bm state edn-true-batch)
      (print! batch bm state edn-false-batch)))
  java.lang.Object
  (-print-edn! [this config state bm batch parents]
    (invalid-item-handler batch bm state config this parents)))

;;; instant printer

(def ^:private tludf @#'clojure.instant/thread-local-utc-date-format)

(def ^:private tlutf
  @#'clojure.instant/thread-local-utc-timestamp-format)

(extend-protocol! IEdnPrinter
  java.util.Date
  (-print-edn! [this config state bm batch parents]
    (let [utc-format (.get ^java.lang.ThreadLocal tludf)]
      (print! batch bm state
              "#inst \""
              (string-to-batch!
               (.format ^java.text.SimpleDateFormat utc-format this))
              \")))
  java.util.Calendar
  (-print-edn! [this config state bm batch parents]
    (let [calstr (clojure.core/format "%1$tFT%1$tT.%1$tL%1$tz" this)
          offset-minutes (- (.length calstr) 2)]
      (print! batch bm state
              "#inst \""
              (string-to-batch! (slice calstr 0 offset-minutes))
              \:
              (string-to-batch! (slice calstr offset-minutes
                                      (+ offset-minutes 2)))
              \")))
  java.sql.Timestamp
  (-print-edn! [this config state bm batch parents]
    (let [utc-format (.get ^java.lang.ThreadLocal tlutf)]
      (print! batch bm state
              "#inst \""
              (string-to-batch!
               (.format ^java.text.SimpleDateFormat utc-format this))
              (string-to-batch!
               (clojure.core/format ".%09d-00:00" (.getNanos this)))
              \"))))

;;; uuid printer

(extend-protocol! IEdnPrinter
  java.util.UUID
  (-print-edn! [this config state bm batch parents]
   (print! batch bm state
           "#uuid \"" (string-to-batch! (->str this)) \")))

;;; uri printer

(extend-protocol! IEdnPrinter
  java.net.URI
  (-print-edn! [this config state bm batch parents]
   (print! batch bm state
           "#uri \"" (string-to-batch! (->str this)) \")))


;;;; Pretty printer

(defprotocol IEdnPrettyCount
  (-pretty-count-edn
    "Returns the count estimation of the pretty printer collection
    for `this` object in inline mode."
    [this]))

(defprotocol IEdnPrettyPrinter
  (-print-pretty-edn!
    "Returns result or pretty printing `this` as a EDN. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(defn ^:private edn-pretty-mode
  "Returns true if `coll` should be printed indented in multiple
  lines, otherwise returns false."
  [config coll]
  (let [x (-pretty-count-edn coll)]
    (or (neg? x) (< (:inline-threshold config) x))))

(extend-protocol! IEdnPrettyCount
  java.lang.String
  (-pretty-count-edn [this] (iadd (i2) (count this)))
  clojure.lang.Keyword
  (-pretty-count-edn [this] (count (->str this)))
  clojure.lang.Symbol
  (-pretty-count-edn [this] (count (->str this)))
  clojure.lang.Sequential
  (-pretty-count-edn [this]
    (let [rf #(iadd (iinc %) (iint (-pretty-count-edn %2)))]
      (if (i< (iint (count this)) (i10))
        (reduce rf (i1) this)
        (idiv (imul (reduce rf (i1) (take 10 this))
                    (iint (count this))) (i10)))))
  clojure.lang.IPersistentSet
  (-pretty-count-edn [this]
    (let [rf #(iadd (iinc %) (iint (-pretty-count-edn %2)))]
      (if (i< (iint (count this)) (i10))
        (reduce rf (i2) this)
        (idiv (imul (reduce rf (i2) (take 10 this))
                    (iint (count this))) (i10)))))
  clojure.lang.IPersistentMap
  (-pretty-count-edn [this]
    (let [rf #(iadd % (iint (-pretty-count-edn %2)))]
      (if (< (count this) (i10))
        (reduce rf (i0) this)
        (idiv (imul (reduce rf (i0) (take 10 this))
                    (iint (count this))) (i10)))))
  java.math.BigDecimal
  (-pretty-count-edn [this]
    (iinc (.length (.toString ^java.math.BigDecimal this))))
  java.math.BigInteger
  (-pretty-count-edn [this]
    (iinc (.length (.toString ^java.math.BigInteger this))))
  clojure.lang.BigInt
  (-pretty-count-edn [this]
    (iinc (.length (.toString ^clojure.lang.BigInt this))))
  java.lang.Character
  (-pretty-count-edn [this] 2)
  nil
  (-pretty-count-edn [this] 3)
  java.lang.Boolean
  (-pretty-count-edn [this] (if this 4 5))
  java.lang.Object
  (-pretty-count-edn [this] (.length (.toString this))))

(extend-protocol! IEdnPrettyCount
  java.util.Date (-pretty-count-edn [this] 37)
  java.util.Calendar (-pretty-count-edn [this] 37)
  java.sql.Timestamp (-pretty-count-edn [this] 43)
  java.util.UUID (-pretty-count-edn [this] 44)
  java.net.URI (-pretty-count-edn
                [this] (+ (-pretty-count-edn (.toString this)) 7)))

(defn get-column
  [state]
  (or (:pretty-column @state) 0))

(defn reset-column!
  ([state]
     (reset-column! state 0))
  ([state x]
     (reset! state (assoc @state :pretty-column x))))

(defn move-column!
  [state delta]
  (let [x (get-column state)]
    (reset-column! state (+ x delta))))

(defn move-column-by-token!
  [state token]
  (let [x (get-column state)]
    (reset-column! state (-pretty-count-edn token))))

(deftype EdnPrettyTopContainer
  "Top level printer container for EDN printer."
  [config state coll block?]
  IIndentedMachine
  (-indent [this] (base-indent config))
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (reset-column! state (base-indent config)) nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (if block?
      (reset-column! state (base-indent config))
      (move-column! state 1))
    (if block?
      (print-colored! batch bm config state
                      (color config state :indent :syntax)
                      \newline [\space (base-indent config)])
      (print! batch bm state \space))))

(deftype EdnPrettyMapPrinter
  "EDN map printer machine type."
  [config state coll indent block?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :map :syntax) \{))
  (-print-after! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :map :syntax) \}))
  (-print-between! [this bm batch parents]
    (let [indent (inc indent)]
      (if block?
        (reset-column! state indent)
        (move-column! state 2))
      (if block?
        (print-colored! batch bm config state
                        (color config state :map :syntax) \,
                        (color config state :indent :syntax)
                        \newline [\space indent])
        (print-colored! batch bm config state
                        (color config state :map :syntax)
                        \, \space)))))

(defn edn-pretty-map-printer
  "Constructor for EDN pretty map printer machine"
  [config state coll indent]
  (let [block? (edn-pretty-mode config (seq coll))]
    (->EdnPrettyMapPrinter config state coll indent block?)))

(deftype EdnPrettyEntryContainer
  {:predicate 'entry-container?}
  [config state contents indent]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] contents)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (move-column! state 1)
    (print! batch bm state \space)))

(deftype EdnPrettyVecContainer
  [config state coll indent block?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :vector :syntax) \[))
  (-print-after! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :vector :syntax) \]))
  (-print-between! [this bm batch parents]
    (let [indent (inc indent)
          sb? (and block? (< (:inline-threshold config)
                             (get-column state)))]
      (if sb?
        (reset-column! state indent)
        (move-column! state 1))
      (if sb?
        (print-colored! batch bm config state
                        (color config state :indent :syntax)
                        \newline [\space indent])
        (print! batch bm state \space)))))

(deftype EdnPrettyLstContainer [config state coll indent block?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :list :syntax) \())
  (-print-after! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :list :syntax) \)))
  (-print-between! [this bm batch parents]
    (let [indent (inc indent)]
      (if block?
        (reset-column! state indent)
        (move-column! state 1))
      (if block?
        (print-colored! batch bm config state
                        (color config state :list :syntax) \,
                        (color config state :indent :syntax)
                        \newline [\space indent])
        (print! batch bm state \space)))))

(deftype EdnPrettySetContainer [config state coll indent block?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (move-column! state 2)
    (print-colored! batch bm config state
                    (color config state :set :syntax) \# \{))
  (-print-after! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :set :syntax) \}))
  (-print-between! [this bm batch parents]
    (let [indent (+ 2 indent)]
      (if block?
        (reset-column! state indent)
        (move-column! state 1))
      (if block?
        (print-colored! batch bm config state
                        (color config state :set :syntax) \,
                        (color config state :indent :syntax)
                        \newline [\space indent])
        (print! batch bm state \space)))))

(extend-protocol! IEdnPrettyPrinter
  java.lang.String
  (-print-pretty-edn! [this config state bm batch parents]
    (let [batch ^java.nio.Buffer batch
          p (.position batch)
          b (print-colored!
             batch bm config state
             (color config state :string)
             \" [this #(to-escape config %) config] \")
          b ^java.nio.Buffer b
          x (+ (if (or (nil? b) (identical? b batch)) 0 (.limit b))
               (- (.position batch) p)
               (- (.capacity ^java.nio.Buffer
                             (color config state :string)))
               (- (.capacity ^java.nio.Buffer
                             (color config state :default))))]
      ;;(cc/println "col " x)
      (move-column! state x)
      b))
  java.util.Map$Entry
  (-print-pretty-edn! [this config state bm batch parents]
    (->EdnPrettyEntryContainer
     config state this (get-column state)))
  clojure.lang.IPersistentList
  (-print-pretty-edn! [this config state bm batch parents]
    (let [machine (first parents)]
      (->EdnPrettyLstContainer config state this (get-column state)
                               (edn-pretty-mode config this))))
  clojure.lang.ASeq
  (-print-pretty-edn! [this config state bm batch parents]
    (let [machine (first parents)]
      (->EdnPrettyLstContainer config state this (get-column state)
                               (edn-pretty-mode config this))))
  clojure.lang.IPersistentVector
  (-print-pretty-edn! [this config state bm batch parents]
    (let [machine (first parents)]
      (if (class-instance? java.util.Map$Entry this)
        (->EdnPrettyEntryContainer
         config state this (get-column state))
        (->EdnPrettyVecContainer config state this (get-column state)
                                 (edn-pretty-mode config this)))))
  clojure.lang.IPersistentSet
  (-print-pretty-edn! [this config state bm batch parents]
    (let [machine (first parents)]
      (->EdnPrettySetContainer config state this (get-column state)
                               (edn-pretty-mode config this))))
  clojure.lang.IPersistentMap
  (-print-pretty-edn! [this config state bm batch parents]
    (let [machine (first parents)]
      (edn-pretty-map-printer config state this (get-column state))))
  java.lang.Long
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.lang.Long this))]
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :integer :number) b)))
  java.lang.Integer
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.lang.Integer this))]
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :integer :number) b)))
  java.lang.Double
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.lang.Double this))]
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :float :number) b)))
  java.math.BigDecimal
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.math.BigDecimal this))]
      (move-column! state (inc (.remaining ^java.nio.Buffer b)))
      (print-colored! batch bm config state
                      (color config state :decimal :number) b \M)))
  java.math.BigInteger
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^java.math.BigInteger this))]
      (move-column! state (inc (.remaining ^java.nio.Buffer b)))
      (print-colored! batch bm config state
                      (color config state :integer :number) b \N)))
  clojure.lang.BigInt
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (string-to-batch! (.toString ^clojure.lang.BigInt this))]
      (move-column! state (inc (.remaining ^java.nio.Buffer b)))
      (print-colored! batch bm config state
                      (color config state :integer :number) b \N)))
  java.lang.Character
  (-print-pretty-edn! [this config state bm batch parents]
    (let [b (to-char-escape config this)]
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :char :string) b)))
  clojure.lang.Symbol
  (-print-pretty-edn! [this config state bm batch parents]
    (let [s (->str this)
          b (string-to-batch! s)]
      (when-not (valid-symbol? s) (perror "invalid symbol " s))
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :symbol :identifier) b)))
  clojure.lang.Keyword
  (-print-pretty-edn! [this config state bm batch parents]
    (let [s (->str this)
          b (string-to-batch! s)]
      (when-not (valid-keyword? s) (perror "invalid keyword " s))
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :keyword :identifier) b)))
  nil
  (-print-pretty-edn! [this config state bm batch parents]
    (move-column-by-token! state this)
    (print-colored! batch bm config state
                    (color config state :nil :literal) edn-nil-batch))
  java.lang.Boolean
  (-print-pretty-edn!
   [this config state bm batch parents]
    (move-column-by-token! state this)
    (if this
      (print-colored! batch bm config state
                      (color config state :true :boolean :literal)
                      edn-true-batch)
      (print-colored! batch bm config state
                      (color config state :false :boolean :literal)
                      edn-false-batch)))
  java.nio.Buffer
  (-print-pretty-edn! [this config state bm batch parents]
    (move-column! state (.limit this))
    (print! batch bm state this)))

(extend-protocol! IEdnPrettyPrinter
  java.util.Date
  (-print-pretty-edn! [this config state bm batch parents]
    (let [utc-format (.get ^java.lang.ThreadLocal tludf)]
      (move-column-by-token! state this)
      (print-colored!
       batch bm config state (color config state :tagged :literal)
       "#inst " (color config state :string) "\""
       (string-to-batch!
        (.format ^java.text.SimpleDateFormat utc-format this)) \")))
  java.util.Calendar
  (-print-pretty-edn! [this config state bm batch parents]
    (let [calstr (clojure.core/format "%1$tFT%1$tT.%1$tL%1$tz" this)
          offset-minutes (- (.length calstr) 2)]
      (move-column-by-token! state this)
      (print-colored!
       batch bm config state (color config state :tagged :literal)
       "#inst " (color config state :string)
       "\"" (string-to-batch! (slice calstr 0 offset-minutes)) \:
       (string-to-batch!
        (slice calstr offset-minutes (+ offset-minutes 2))) \")))
  java.sql.Timestamp
  (-print-pretty-edn! [this config state bm batch parents]
    (let [utc-format (.get ^java.lang.ThreadLocal tlutf)]
      (move-column-by-token! state this)
      (print-colored!
       batch bm config state (color config state :tagged :literal)
       "#inst " (color config state :string) "\""
       (string-to-batch!
        (.format ^java.text.SimpleDateFormat utc-format this))
       (string-to-batch!
        (clojure.core/format ".%09d-00:00" (.getNanos this))) \"))))

(extend-protocol! IEdnPrettyPrinter
  java.util.UUID
  (-print-pretty-edn! [this config state bm batch parents]
    (move-column-by-token! state this)
    (print-colored! batch bm config state
                    (color config state :tagged :literal)
                    "#uuid " (color config state :string)
                    "\"" (string-to-batch! (->str this)) \")))

(extend-protocol! IEdnPrettyPrinter
  java.net.URI
  (-print-pretty-edn! [this config state bm batch parents]
    (move-column-by-token! state this)
    (print-colored! batch bm config state
                    (color config state :tagged :literal)
                    "#uri " (color config state :string)
                    "\"" (string-to-batch! (->str this)) \")))

(defrecord EdnPrettyPrinterFactory
  "EDN Pretty Printer Factory record."
  [invalid-item indent-offset inline-threshold color-fn
   pretty-level-limit
   pretty-item-limit pretty-string-limit color?]
  IPrinterMachineFactory
  (-printer-config [this]
    {:invalid-item invalid-item
     :indent-offset indent-offset
     :inline-threshold inline-threshold
     :color-fn color-fn
     :color? color?
     :level-limit-pred
     (fn [p] (and pretty-level-limit
                 (pos? pretty-level-limit)
                 (not (class-instance?
                       dunaj.format.edn.EdnPrettyMapPrinter
                       (first p)))
                 (< pretty-level-limit
                    (count (vec (remove entry-container? p))))))
     :level-limit-print-fn
     (fn [batch bm state]
       (let [config {:color-fn color-fn}]
         (print-colored!
          batch bm config state
          (dunaj.format.printer/color config state :limit :syntax)
          edn-level-limit-batch)))
     :item-limit-batch
     (let [batches [(color-fn :limit :syntax)
                    edn-item-limit-batch default-color]]
       (string-to-batch!
        (apply ->str (map #(.clear ^java.nio.Buffer %) batches))))
     :string-limit pretty-string-limit
     :item-limit pretty-item-limit})
  (-printer-from-type [this] (keyword->class :object))
  (-printer-to-type [this] (keyword->class :char))
  (-top-container [this config state coll]
    (->EdnPrettyTopContainer
     config state coll (edn-pretty-mode config coll)))
  (-dispatch-printer
    [this config state item bm batch parents]
    (if (satisfies? IEdnPrettyPrinter item)
      (-print-pretty-edn! item config state bm batch parents)
      (invalid-item-handler batch bm state config this parents)))
  IPrinterFactory
  (-print [this]
    (pretty-printer-engine this))
  (-print [this coll]
    (pretty-printer-engine this coll)))


;;;; Formatter

(defrecord EdnFormatterFactory
  "EDN Formatter Factory record."
  [incomplete-mode lazy? invalid-item tag-readers default-tag-reader
   token-item-limit container-item-limit container-level-limit]
  IParserMachineFactory
  (-parser-from-type [this]
    (keyword->class :char))
  (-parser-to-type [this]
    (keyword->class :object))
  (-parser-config [this]
    {:incomplete-mode incomplete-mode
     :token-item-limit token-item-limit
     :container-item-limit container-item-limit
     :container-level-limit container-level-limit
     :tag-readers (or tag-readers clojure.core/*data-readers*)
     :default-tag-reader
     (or default-tag-reader clojure.core/*default-data-reader-fn*)})
  (-dispatch-tokenizer [this config state item]
    (let [x (iint item)]
      (cond (edn-whitespace? x) this
            (i== x (iQUOTE)) (edn-string-literal config state x)
            (edn-symbol-start? x) ;; must be before number dispatch
            (edn-symbol-literal config state x)
            (edn-number-start? x)
            (edn-number-literal config state x)
            (i== x (iLBRACKET)) (edn-vec-container config state)
            (i== x (iRBRACKET)) edn-bracket-close
            (i== x (iLBRACE)) (edn-map-container config state)
            (i== x (iRBRACE)) edn-brace-close
            (i== x (iHASH)) (edn-dispatch config state x)
            (i== x (iSEMICOLON))
            (edn-lineskip-literal config state x)
            (i== x (iLPAR)) (edn-lst-container config state)
            (i== x (iRPAR)) edn-par-close
            (i== x (iBACKSLASH))
            (edn-character-literal
             config state x
             edn-character-map false edn-delimiter?)
            :else (perror "invalid item " item))))
  (-dispatch-parser [this config state token parents]
    token)
  ILazyParserMachineFactory
  (-dispatch-lazy-parser [this config state token parents]
    token)
  IParserFactory
  (-parse [this coll]
    ((if lazy? lazy-parser-engine parser-engine) this coll))
  IPrinterMachineFactory
  (-printer-config [this]
    {:invalid-item invalid-item})
  (-printer-from-type [this]
    (keyword->class :object))
  (-printer-to-type [this]
    (keyword->class :char))
  (-top-container [this config state coll]
    (->EdnTopContainer config state coll))
  (-dispatch-printer [this config state item bm batch parents]
    (-print-edn! item config state bm batch parents))
  IPrinterFactory
  (-print [this]
    (printer-engine this))
  (-print [this coll]
    (printer-engine this coll)))


;;;; Public API

(def edn :- (I IParserFactory IPrinterFactory)
  "An EDN formatter factory."
  {:added v1
   :see '[lazy-edn pretty-edn]}
  (->EdnFormatterFactory
   :keep false nil nil nil 1000000 1000000 1000000))

(def lazy-edn :- IParserFactory
  "A lazy EDN formatter factory."
  {:added v1
   :see '[edn pretty-edn]}
  (assoc edn :lazy? true))

(def edn-colorer-map
  {:limit cyan
   :char cyan})

(def pretty-edn :- IPrinterFactory
  "JSON printer factory with pretty printing."
  {:added v1
   :see '[edn lazy-edn]}
  (->EdnPrettyPrinterFactory
   nil 0 70 (custom-colorer edn-colorer-map) 5 100 200 false))


;;;; Testing

(scratch [[clojure.core :as cc :refer [cycle slurp ->>]]
          [dunaj.coll.default :refer [vec]]
          [dunaj.string :refer [str ->str]]
          [dunaj.coll :as dc :refer [conj count first]]
          [dunaj.coll.util :refer [batched]]
          [dunaj.format :refer [parse print print-one]]]
  []

 ;; Parser

 ;; duplicate items check
 (vec (parse edn "{:a 1 :a 2}"))
 (vec (parse edn "#{:a 1 :a 2}"))
 (first (parse lazy-edn "#{:a 1 2 2 16 2} "))

 ;; token item limit
 (seq (parse edn (cons \" (cc/repeat \space))))
 (vec (parse edn (cons \" (cc/repeat \space))))

 ;; container level limit
 (seq (parse edn (cc/repeat \[)))
 (vec (parse edn (cc/repeat \[)))
 (parse lazy-edn (cc/repeat \[))

 ;; container item limit
 (seq (parse edn (cons \[ (cc/cycle [\1 \space]))))
 (vec (parse edn (cons \[ (cc/cycle [\1 \space]))))
 (parse lazy-edn (cons \[ (cc/cycle [\1 \space])))

 ;; benchmark
 (def s (cc/slurp "examples/dunaj.coll.tuple.edn"))
 (def s (str (cc/take 50000 (cc/repeat \[)))) ;; stack overflow

 (cc/time (count (first (vec (parse edn s)))))
 (cc/time (count (first (seq (parse edn s)))))
 (cc/time (count (first (parse lazy-edn s))))

 (cc/require 'clojure.tools.reader.edn)
 (cc/time (count (vec (clojure.tools.reader.edn/read-string s))))

 (cc/= (first (parse edn s))
       (first (parse lazy-edn s))
       (clojure.tools.reader.edn/read-string s))

 ;; Printer

 (def coll
   (vec (parse edn (cc/slurp "examples/dunaj.coll.tuple.edn"))))

 (= (str (print edn [coll]))
    (apply ->str (seq (print edn [coll]))))

 ;; benchmark
 (cc/time (count (vec (print edn [coll]))))
 (cc/time (count (vec (batched (keyword->class :char)
                               10000 (print edn [coll])))))
 (cc/time (count (str (print edn [coll]))))
 (cc/time (count (seq (print edn [coll]))))

 ;; Pretty Printer

 (def coll
   (vec (parse edn (cc/slurp "examples/dunaj.coll.tuple.edn"))))

 (cc/defn dopp [x] (cc/println (str (print pretty-edn x))))
 (cc/defn dops [x] (str (print (cc/assoc pretty-edn :color? true) x)))

 (dopp coll)
 (cc/println (dops coll))

 (def sample
   {:long-string "This string gets truncated in our marvelous pretty
                  printer."
    :level-limit '[pretty #{printer {limits (number [1 2 3] of (3)
                                             levels [[]] printed)}}]
    :item-limit (vec (cc/range 10000 200000 ))
    :tagged-literals #inst "2014-06-11T11:30:38.047-00:00"
    :literals [false true nil]
    :symbols `[asymbol another/symbol]
    :numbers #{0 -42 3.1415M 12345N}
    :characters `(\f \o \o \newline \space \b \a \r)})

 (def sample
   {:long-string "This string gets truncated in our marvelous pretty
                  printer."
    " :item-limit" (vec (cc/range 10000 200000 ))})

 (->> sample
      (print-one (assoc pretty-edn :color? true))
      str
      cc/println)

 (dopp [sample])

 ;; pretty string
 (dopp [(str (cc/range 1000))])

 ;; pretty level
 (dopp [0 [1 [2 [3 [4 [5 [6 [7 [8 [9]]]]]]]]]])
 (dopp [0 {1 {2 {3 {4 {5 {6 {7 {8 {9 10}}}}}}}} 2 3}])

 ;; pretty item
 (dopp [0 (vec (cc/range 1000))])

 ;; instant
 (dopp [(java.util.Date.)])
 (dopp [(java.sql.Timestamp. 1234567890)])
 (dopp [(java.util.Calendar/getInstance)])
 (dopp [(java.util.UUID/randomUUID)])

)
