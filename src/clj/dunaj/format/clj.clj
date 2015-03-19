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

(ns dunaj.format.clj
  "CLJ formatter."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn AnyFn Maybe U I]]
   [dunaj.boolean :refer [Boolean and or not true? false?]]
   [dunaj.host :refer [class? set! keyword->class class-instance?]]
   [dunaj.host.int :refer
    [Int iint iinc i== i< isub izero? idec ineg? i> i< i<< imax iadd
     iloop i0 i1 i2 i3 i4 i5 i8 iFF i-1 idigit? ioctal? iCAPITAL_M iFF
     ihexa? ihexa->int idigit->int iAMP ismall-letter? iPERCENT iLPAR
     icapital-letter? iZERO iUS iDEL iHASH iHT iCR iLF iCOLON iSPACE
     iCOMMA iLBRACKET iRBRACKET iQUOTE iAPOS iBACKSLASH iSLASH iRPAR
     iLBRACE iRBRACE iTILDE iSMALL_B iSMALL_F iSMALL_N iSMALL_R iMINUS
     iUNDERSCORE iSMALL_U iSMALL_T iPLUS iCAPITAL_E iSMALL_E iBANG iLT
     iBACKQUOTE iARROWHEAD iAT i10 iSMALL_O iSEMICOLON iEQ iCAPITAL_N
     iSMALL_X iCAPITAL_X iBS iHT iCR iLF iCAPITAL_R iSMALL_R iDOT]]
   [dunaj.math :refer [max min < integer? == > neg? + inc nneg? zero?
                       one? >= inc dec number? - pos? *]]
   [dunaj.threading :refer [->]]
   [dunaj.compare :refer [identical? nil? = sentinel]]
   [dunaj.state :refer [reset! alter!]]
   [dunaj.flow :refer [if when when-not cond do let if-let eval loop
                       recur case condp if-not when-let]]
   [dunaj.feature :refer
    [IMeta IPersistentMeta assoc-meta meta meta-ref]]
   [dunaj.poly :refer
    [satisfies? defprotocol deftype extend-protocol! defrecord]]
   [dunaj.coll :refer
    [first slice seq second reduce single? reduced edit settle! dissoc
     empty? rest count nth assoc! reverse get ISeq ISeqable conj! seq?
     sequential? conj assoc map? list? vector? set? double? counted?]]
   [dunaj.function :refer [fn defn identity constantly apply]]
   [dunaj.host.batch :refer [batch-manager]]
   [dunaj.host.array :refer [array-manager array]]
   [dunaj.error :refer [throw illegal-state unsupported-operation]]
   [dunaj.macro :refer [defmacro gensym]]
   [dunaj.state.var :refer [Var var var? declare def]]
   [dunaj.state.basic :refer [unsynchronized-reference]]
   [dunaj.identifier :refer
    [name symbol namespace keyword? symbol? keyword]]
   [dunaj.char :refer [Char char char?]]
   [dunaj.string :refer [->str empty-string str index-of string?]]
   [dunaj.set :refer [union]]
   [dunaj.coll.lazy-seq-map :refer [lazy-seq->map]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.tuple :refer [tuple pair]]
   [dunaj.coll.util :refer [merge last some every? prewalk-replace]]
   [dunaj.coll.default :refer
    [empty-vec empty-map empty-sorted-map ->lst vec set]]
   [dunaj.coll.recipe :refer
    [map partition keep mapcat filter take-nth range concat remove]]
   [dunaj.format :refer [IParserFactory IPrinterFactory parse print]]
   [dunaj.regex :refer [matches]]
   [dunaj.format.helper :refer [string-to-batch! string-cat-batch!]]
   [dunaj.format.parser :refer
    [parser-engine string-literal-constructor leftover IParserMachine
     ITokenizerMachine -analyze-eof! container-parser tokenizer-engine
     IParserMachineFactory -parser-config ILazyParserMachine
     -dispatch-parser process-one ILazyParserMachineFactory
     -parse-value! literal-tokenizer ignore-token lazy-parser-engine
     perror eof-handler keep? lazy-level-limit take-one drop-one]]
   [dunaj.format.printer :refer
    [IContainerPrinterMachine -printer-to-type IPrinterMachineFactory
     printer-engine -indent invalid-item-handler print-colored! green
     yellow magenta red custom-colorer prev-indent print!
     IIndentedMachine next-indent base-indent default-colorer color
     default-color bold-red white blue bold-blue bold-magenta
     bold-cyan bold-green cyan bold-yellow pretty-printer-engine]]
   [dunaj.format.edn :refer
    [edn-delimiter? edn-whitespace? edn-newline? edn-bracket-close
     edn-brace-close edn-par-close edn-symbol-start? edn-symbol-digit?
     edn-strict-symbol-start? edn-maybe-symbol-start? ->TaggedParser
     ->DiscardParser edn-set-container edn-vec-container move-column!
     edn-map-container edn-lst-container edn-lineskip-literal
     edn-character-literal edn-quote-batch edn-backslash-batch
     edn-ht-batch edn-lf-batch edn-cr-batch edn-space-batch
     edn-tab-batch edn-newline-batch edn-return-batch reset-column!
     ->EdnTopContainer -print-edn! ->EdnPrettyTopContainer
     -pretty-count-edn entry-container? -print-pretty-edn!
     edn-level-limit-batch edn-item-limit-batch IEdnPrettyPrinter
     ->EdnPrettyEntryContainer get-column IEdnPrinter]]))


;;;; Parser

;; Uses following EDN stuff
;; - tokens
;;   - edn-bracket-close
;;   - edn-brace-close
;;   - edn-par-close
;;   - comment literal
;;   - character literal
;; - parsers
;;   - map container
;;   - set container
;;   - tagged parser
;;   - discard parser
;; - helper fns
;;   - whitespace
;;   - newline
;;   - delimiter
;;   - symbol start
;;   - symbol digit
;;   - strict symbol start
;;   - maybe symbol start

(defn clj-delimiter? :- Boolean
  "Returns true if `x` represents a CLJ delimiter character,
  otherwise returns false."
  [x :- Int]
  (or (edn-delimiter? x) (i== x (iBACKQUOTE)) (i== x (iTILDE))
      (i== x (iARROWHEAD)) (i== x (iAT))))

(deftype WrapperParser
  "Type for wrapper macro parser."
  [config :- {}, first-val :- Any]
  IParserMachine
  (-parse-value! [this value parents]
    (->lst first-val value))
  (-parse-eof! [this parents]
    (eof-handler this config this "wrapper parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (process-one this config coll parents)))

(deftype MetadataParser
  "Type for metadata macro parser."
  [config :- {}, ^:unsynchronized-mutable mv :- Any]
  IParserMachine
  (-parse-value! [this value parents]
    (if (nil? mv)
      (do (set! mv value) this)
      (let [m (cond (keyword? mv) {mv true}
                    (map? mv) mv
                    (or (symbol? mv) (string? mv)) {:tag mv}
                    :else (perror "unknown metadata type"))]
        (if (satisfies? IPersistentMeta value)
          (let [m (if (satisfies? IMeta value)
                    (merge (meta value) m)
                    m)]
            (assoc-meta value m))
          (let [mr (meta-ref value)] (alter! mr merge m) value)))))
  (-parse-eof! [this parents]
    (eof-handler this config this "metadata parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (let [level-limit (lazy-level-limit config)
          m (take-one parents level-limit coll)
          s (drop-one parents level-limit coll)
          v (take-one parents level-limit s)]
      (set! mv m)
      (pair (-parse-value! this v parents)
            (drop-one parents level-limit s)))))

(defn metadata-parser
  "Returns new metadata parser."
  [config state]
  (->MetadataParser config nil))

(deftype EvalParser
  "Type for eval macro parser."
  [config]
  IParserMachine
  (-parse-value! [this value parents]
    (eval value))
  (-parse-eof! [this parents]
    (eof-handler this config this "eval parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (process-one this config coll parents)))

;;; anonymous fn

(deftype ArgToken
  "Token type for anonymous fn arguments."
  {:predicate 'clj-arg?}
  [i])

(defn clj-arg
  "A token for CLJ anon args"
  [i]
  (->ArgToken i))

(def clj-fn
  "A token for CLJ anon fn"
  (sentinel))

(defn dispatch-arg-token
  "Returns dispatched arg token."
  [config state ^dunaj.format.clj.ArgToken t]
  (let [i (.-i t)
        fn-env (:fn-env @state)
        s (if fn-env
            (gensym (->str "arg" (or i 1) "_"))
            (symbol (if i (->str "%" (if (neg? i) "&" i)) "%")))]
    (when fn-env
      (reset! state (assoc @state :fn-env (assoc fn-env (or i 1) s))))
    s))

(deftype FnParser
  "Type for anonymous fn parser."
  [config state]
  IParserMachine
  (-parse-value! [this value parents]
    (let [argmap (:fn-env @state)
          revmap (reverse argmap)
          max-arg (or (first (first revmap)) 0)
          rf #(conj % (or (get argmap %2) (gensym "argU_")))
          args (reduce rf [] (range 1 (inc max-arg)))
          args (if-let [x (get argmap -1)] (conj args '& x) args)]
      (reset! state (dissoc @state :fn-env))
      (->lst 'clojure.core/fn* args value)))
  (-parse-eof! [this parents]
    (eof-handler this config this "fn parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (process-one this config coll parents)))

(defn dispatch-fn-token
  "Returns anon fn parser and initializes anon fn environment."
  [config state]
  (when (:fn-env @state)
    (perror "anonymous fns cannot be nested"))
  (reset! state (assoc @state :fn-env (empty-sorted-map)))
  (->FnParser config state))

;;; syntax quote

;; straightforward implementation based on LispReader.java

(defn ^:private unquote?
  [form]
  (and (list? form) (= 'clojure.core/unquote (first form))))

(defn ^:private unquote-splicing?
  [form]
  (and (list? form) (= 'clojure.core/unquote-splicing (first form))))

(declare syntax-quote)

(defn ^:private sq-expand-list
  [s gensyms-ref cns]
  (let [mf (fn [x]
             (cond (unquote? x) (->lst 'clojure.core/list (second x))
                   (unquote-splicing? x) (second x)
                   :else (->lst 'clojure.core/list
                                (syntax-quote x gensyms-ref cns))))]
    (seq (map mf s))))

(defn ^clojure.lang.Namespace ^:private namespace-for
  [sym ^clojure.lang.Namespace cns]
  (let [nssym (symbol (namespace sym))
        n (.lookupAlias cns nssym)]
    (if (nil? n) (clojure.lang.Namespace/find nssym) n)))

(defn ^:private resolve-symbol
  [sym ^clojure.lang.Namespace cns]
  (let [sn (name sym)]
    (if (index-of sn \.)
      sym
      (if (nil? (namespace sym))
        (let [mp (.getMapping cns sym)]
          (cond (nil? mp)
                (symbol (name (.name cns)) sn)
                (class-instance? java.lang.Class mp)
                (symbol nil (.getName ^java.lang.Class mp))
                (class-instance? clojure.lang.Var mp)
                (symbol (name (.name (.ns ^clojure.lang.Var mp)))
                           (name (.sym ^clojure.lang.Var mp)))
                :else nil))
        (let [sns (namespace sym)
              nns (if-let [x (namespace-for sym cns)]
                    (name (.name x))
                    nil)]
          (if (or (nil? nns) (= nns sns))
            sym
            (symbol nns (name sym))))))))

(defn ^:private syntax-quote-symbol
  [sym gensyms-ref ^clojure.lang.Namespace cns]
  (cond
   (and (nil? (namespace sym)) (= \# (last (name sym))))
   (if-let [gs (get @gensyms-ref sym)]
     gs
     (let [sn (name sym)
           ngs (symbol nil (->str (slice sn 0 (dec (count sn)))
                                     "__" (clojure.lang.RT/nextID)
                                     "__auto__"))]
       (reset! gensyms-ref (assoc @gensyms-ref sym ngs))
       ngs))
   (and (nil? (namespace sym)) (= \. (last (name sym))))
   (let [sn (name sym)
         csym (symbol nil (slice sn 0 (dec (count sn))))
         csym (resolve-symbol csym cns)]
     (symbol nil (->str (name csym) ".")))
   (and (nil? (namespace sym)) (= \. (first (name sym)))) sym
   :else
   (let [mc (when-let [nss (namespace sym)]
              (.getMapping cns (symbol nil nss)))]
     (if (class-instance? java.lang.Class mc)
       (symbol (.getName ^java.lang.Class mc) (name sym))
       (resolve-symbol sym cns)))))

(defn ^:private syntax-quote-collection
  [coll gensyms-ref cns]
  (let [wfn
        (fn [x y]
          (let [a (->lst 'clojure.core/seq
                         (cons 'clojure.core/concat
                              (sq-expand-list
                               (seq y) gensyms-ref cns)))]
            (if x (->lst 'clojure.core/apply x a) a)))]
    (cond
     (class-instance? clojure.lang.IRecord coll) coll
     (map? coll) (wfn 'clojure.core/hash-map (mapcat identity coll))
     (vector? coll) (wfn 'clojure.core/vector coll)
     (set? coll) (wfn 'clojure.core/hash-set coll)
     (or (list? coll) (class-instance? clojure.lang.ISeq coll))
     (if (seq coll) (wfn nil coll) ())
     :else (perror "unknown collection type in syntax quote"))))

(defn ^:private syntax-quote
  [form gensyms-ref cns]
  (let [ret (cond (clojure.lang.Compiler/isSpecial form)
                  (->lst 'clojure.core/quote
                        (if (nil? (namespace form))
                          (symbol "clojure.core" (name form))
                          form))
                  (symbol? form)
                  (->lst 'clojure.core/quote
                        (syntax-quote-symbol form gensyms-ref cns))
                  (unquote? form)
                  (second form)
                  (unquote-splicing? form)
                  (perror "splice not in list")
                  (class-instance?
                   clojure.lang.IPersistentCollection form)
                  (syntax-quote-collection form gensyms-ref cns)
                  (or (keyword? form) (number? form)
                      (char? form) (string? form))
                  form
                  :else (->lst 'clojure.core/quote form))]
    (if (and (satisfies? IMeta form) (not (empty? (meta form))))
      (->lst 'clojure.core/with-meta ret
            (syntax-quote (meta form) gensyms-ref cns))
      ret)))

(deftype SyntaxQuoteParser
  "Type for syntax quote parser."
  [config state]
  IParserMachine
  (-parse-value! [this value parents]
    (syntax-quote value (unsynchronized-reference {})
                  (if-let [c (:resolve-in config)]
                    (clojure.core/find-ns c)
                    clojure.core/*ns*)))
  (-parse-eof! [this parents]
    (eof-handler this config this "syntax quote parser machine"))
  ILazyParserMachine
  (-parse-seq [this coll parents]
    (process-one this config coll parents)))

;;; number literal

(defn clj-number-start? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of a CLJ Number Literal, otherwise returns false."
  [x :- Int]
  (or (idigit? x) (i== x (iMINUS)) (i== x (iPLUS))))

(defn clj-number-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of a CLJ Number Literal, otherwise returns false."
  [x :- Int]
  (or (idigit? x) (i== x (iMINUS)) (i== x (iPLUS)) (i== x (iDOT))
      (i== x (iSLASH)) (ismall-letter? x) (icapital-letter? x)))

(deftype CljNumberLiteralTokenizerMachine
  "Tokenizer Machine type for a CLJ Number Literal"
  [config state ^:unsynchronized-mutable
   ^java.lang.StringBuilder sb
   ^:unsynchronized-mutable negative?
   ^:unsynchronized-mutable sign?
   ^:unsynchronized-mutable octal?
   ^:unsynchronized-mutable decimal?
   ^:unsynchronized-mutable bigdec?
   ^:unsynchronized-mutable bigint?
   ^:unsynchronized-mutable radix
   ^:unsynchronized-mutable ratio
   ^:unsynchronized-mutable hexa]
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
             (clj-number-item? x)
             (do
               (cond (i== x (iDOT)) (set! decimal? true)
                     (i== x (iCAPITAL_E)) (set! decimal? true)
                     (i== x (iSMALL_E)) (set! decimal? true)
                     (i== x (iCAPITAL_M)) (do (set! decimal? true)
                                              (set! bigdec? true))
                     (i== x (iCAPITAL_N)) (set! bigint? true)
                     (and (nil? radix) (or (i== x (iSMALL_R))
                                           (i== x (iCAPITAL_R))))
                     (set! radix pos)
                     (i== x (iSMALL_X)) (set! hexa pos)
                     (i== x (iCAPITAL_X)) (set! hexa pos)
                     (i== x (iSLASH)) (set! ratio pos)
                     (nil? octal?) (set! octal? (i== x (iZERO))))
               (recur))
             (clj-delimiter? x)
             (do (string-cat-batch! sb batch begin pos state)
                 (.position batch pos)
                 (-analyze-eof! this))
             :else (perror "invalid number item " c)))))))
  (-analyze-eof! [this]
    (let [s ^java.lang.String (settle! sb)]
      (cond ratio
            (clojure.lang.Ratio.
             (java.math.BigInteger. ^java.lang.String
                                    (slice s 0 ratio))
             (java.math.BigInteger. ^java.lang.String
                                    (slice s (inc ratio))))
            decimal?
            (let [s (if bigdec? (slice s 0 (dec (count s))) s)]
              (if (or bigdec? (:bigdec config))
                (if clojure.core/*math-context*
                  (java.math.BigDecimal. ^java.lang.String
                   s clojure.core/*math-context*)
                  (java.math.BigDecimal. ^java.lang.String s))
                (java.lang.Double/valueOf ^java.lang.String s)))
            :else
            (let [r (cond radix
                          (java.lang.Integer/valueOf
                           ^java.lang.String
                           (slice s (if sign? 1 0) radix))
                          hexa 16
                          octal? 8
                          :else 10)
                  from (cond radix (inc radix)
                             hexa (if sign? 3 2)
                             :else (if sign? 1 0))
                  v (if bigint?
                      (slice s from (dec (count s)))
                      (slice s from))
                  bn (java.math.BigInteger.
                      ^java.lang.String v ^int r)
                  n (if (or bigint? (>= (.bitLength bn) 64))
                      (clojure.lang.BigInt/fromBigInteger bn)
                      (.longValue bn))]
              (if negative? (* -1 n) n))))))

(defn clj-number-literal
  "Returns CLJ Number Literal Tokenizer Machine."
  [config state item]
  (let [x (iint item)
        negative? (i== x (iMINUS))
        sign? (or (i== x (iMINUS)) (i== x (iPLUS)))
        octal? (when-not sign? (i== x (iZERO)))
        ts ^java.lang.StringBuilder (edit empty-string)]
    (->CljNumberLiteralTokenizerMachine
     config state (.append ts (char item))
     negative? sign? octal? false false false nil nil nil)))

;;; string literal

(defn ^:private from-escape :- Char
  "Returns character which is represented by an escape character with
  Unicode code point `x`.
  Throws if escape character is not recognized."
  [x :- Int]
  (cond (i== x (iQUOTE)) \"
        (i== x (iBACKSLASH)) \\
        (i== x (iAPOS)) \'
        (i== x (iSMALL_B)) \u0008
        (i== x (iSMALL_F)) \u000c
        (i== x (iSMALL_N)) \u000a
        (i== x (iSMALL_R)) \u000d
        (i== x (iSMALL_T)) \u0009
        :else (perror "invalid escape character " (char x))))

(def clj-string-literal
  "Function which returns a CLJ String Literal Tokenizer Machine."
  (string-literal-constructor (constantly false) from-escape true))

(def clj-character-map
  {"newline" \newline
   "return" \return
   "space" \space
   "tab" \tab
   "backspace" \backspace
   "formfeed" \formfeed})

;;; symbol literal

(defn clj-symbol-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid character of a CLJ Symbol Literal, otherwise returns false."
  [x :- Int]
  (or (edn-symbol-start? x) (edn-symbol-digit? x)
      (i== x (iHASH)) (i== x (iAPOS))))

(defn clj-nondigit-symbol-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of a CLJ Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (edn-strict-symbol-start? x) (i== x (iHASH)) (i== x (iAPOS))))

(defn clj-strict-symbol-item? :- Boolean
  "Returns true if `x` represents a Unicode code point which is a
  valid starting character of a CLJ Symbol Literal,
  otherwise returns false."
  [x :- Int]
  (or (clj-nondigit-symbol-item? x) (java.lang.Character/isDigit x)))

(defn ^:private valid-symbol? :- Boolean
  "Returns true if string `s` is a valid CLJ symbol, otherwise
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
              1 (cond (clj-nondigit-symbol-item? x) 2
                      (i== x (iCOLON)) 22
                      (i== x (iSLASH)) 4
                      :else fail)
              2 (cond (clj-strict-symbol-item? x) 2
                      (i== x (iCOLON)) 22
                      (i== x (iSLASH)) 4
                      :else fail)
              22 (if (clj-strict-symbol-item? x) 2 fail)
              3 fail
              4 (cond (edn-maybe-symbol-start? x) 5
                      (edn-strict-symbol-start? x) 6
                      (i== x (iSLASH)) 3
                      :else fail)
              5 (cond (clj-nondigit-symbol-item? x) 6
                      (i== x (iCOLON)) 66
                      :else fail)
              66 (if (clj-strict-symbol-item? x) 6 fail)
              6 (cond (clj-strict-symbol-item? x) 6
                      (i== x (iCOLON)) 66
                      :else fail))))
        res (reduce cf 0 s)]
    (not (or (== 4 res) (== 66 res) (== 22 res)))))

(defn ^:private valid-keyword? :- Boolean
  "Returns true if string `s` is a valid CLJ keyword, otherwise
  returns false."
  ([s] (valid-keyword? s true))
  ([s leading-colon]
    (let [fail (reduced 4)
          cf
          (fn [i c]
            (let [x (iint c)]
              (condp == i
                0 (if (i== x (iCOLON)) 1 fail)
                1 (cond (i== x (iCOLON)) 22
                        (clj-strict-symbol-item? x) 2
                        :else fail)
                2 (cond (clj-strict-symbol-item? x) 2
                        (i== x (iCOLON)) 22
                        (i== x (iSLASH)) 4
                        :else fail)
                22 (if (clj-strict-symbol-item? x) 2 fail)
                3 fail
                4 (cond (edn-maybe-symbol-start? x) 5
                        (edn-strict-symbol-start? x) 6
                        (i== x (iSLASH)) 3
                        :else fail)
                5 (if (clj-nondigit-symbol-item? x) 6 fail)
                66 (if (clj-strict-symbol-item? x) 6 fail)
                6 (cond (clj-strict-symbol-item? x) 6
                        (i== x (iCOLON)) 66
                        :else fail))))
          res (reduce cf (if leading-colon 0 1) s)]
      (not (or (== 4 res) (== 66 res) (== 22 res))))))

(deftype CljSymbolLiteralTokenizerMachine
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
             (clj-delimiter? x)
             (do (string-cat-batch! sb batch begin pos state)
                 (.position batch pos)
                 (-analyze-eof! this))
             (and special? (edn-symbol-digit? x))
             (do (.position batch pos)
                 (clj-number-literal config state (.charAt sb 0)))
             (clj-symbol-item? x) (do (set! special? false) (recur))
             :else (perror "invalid symbol character " c)))))))
  (-analyze-eof! [this]
    (let [s ^java.lang.String (settle! sb)]
      (when-not (if keyword?
                  (valid-keyword? s false)
                  (valid-symbol? s))
        (perror "invalid identifier " s))
      (cond tagged? (->TaggedParser config (symbol s) true)
            (and keyword? (i== (iCOLON) (iint (first s))))
            (let [i (index-of s \/)
                  ns (when i (slice s 1 i))
                  nm (slice s (if i (inc i) 1))]
              (keyword ((:resolve-ns config) ns
                           (or (:resolve-in config)
                               (.-name clojure.core/*ns*))) nm))
            keyword? (keyword s)
            (= s "true") true
            (= s "false") false
            (= s "nil") nil
            :else (symbol s)))))

(defn default-ns-resolver
  "Returns a resolved string namespace name from given `s` string
  alias or namespace name, in the context of current ns.
  Returns current string namespace name if `s` is nil."
  [s cns]
  (let [cns ^clojure.lang.Namespace (clojure.core/find-ns cns)]
    (if (empty? s)
      (name (.name cns))
      (let [xs (symbol s)]
        (if-let [ns (.lookupAlias cns xs)]
          (name (.name ns))
          (or (clojure.lang.Namespace/find xs)
              (perror "could not resolve namespace for " s)))))))

(defn clj-symbol-literal
  "Returns CLJ Symbol Literal Tokenizer Machine."
  ([config state item]
     (clj-symbol-literal config state item false))
  ([config state item tagged?]
     (let [x (iint item)
           special?
           (or (i== x (iMINUS)) (i== x (iPLUS)) (i== x (iDOT)))
           keyword? (i== x (iCOLON))
           ts ^java.lang.StringBuilder (edit empty-string)
           ts (if keyword? ts (.append ts (char item)))]
       (->CljSymbolLiteralTokenizerMachine
        config state ts special? keyword? tagged?))))

;;; regex literal

(deftype RegexLiteral
  "Regex literal tokenizer."
  [config ^:unsynchronized-mutable ^java.lang.StringBuilder sb
   ^:unsynchronized-mutable special? state]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (let [batch ^java.nio.CharBuffer batch]
      (iloop [begin (.position batch)]
        (if-not (.hasRemaining batch)
          (do (string-cat-batch!
               sb batch begin (.position batch) state)
              this)
          (let [pos (.position batch)
                c (.get batch)
                npos (.position batch)
                x (iint c)]
            (cond
             special? (do (set! special? false) (recur begin))
             (i== x (iQUOTE))
             (do (string-cat-batch! sb batch begin pos state)
                 (java.util.regex.Pattern/compile (settle! sb)))
             (i== x (iBACKSLASH))
             (do (set! special? true) (recur begin))
             :else (recur begin)))))))
  (-analyze-eof! [this]
    (eof-handler this config
                 (java.util.regex.Pattern/compile (settle! sb))
                 "regex literal tokenizer machine")))

(defn regex-literal
  "Returns regex literal."
  [config state]
  (->RegexLiteral config (edit empty-string) false state))

;;; dispatch literal

(deftype CljDispatchTokenizerMachine
  "Tokenizer Machine type for a CLJ Dispatch."
  [config state]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (if (.hasRemaining ^java.nio.CharBuffer batch)
      (let [batch ^java.nio.CharBuffer batch
            x (iint (.get batch))]
        (cond
         (i== x (iUNDERSCORE)) (->DiscardParser config false)
         (i== x (iLBRACE)) (edn-set-container config state)
         (java.lang.Character/isAlphabetic x)
         (clj-symbol-literal config state x true)
         (i== x (iBANG)) (edn-lineskip-literal config state x)
         (i== x (iLT)) (perror "invalid reader macro " (char x))
         (i== x (iQUOTE)) (regex-literal config state)
         (i== x (iAPOS)) (->WrapperParser config 'clojure.core/var)
         (and (i== x (iEQ)) (:read-eval config)) (->EvalParser config)
         (i== x (iARROWHEAD)) (metadata-parser config state)
         (i== x (iLPAR))
         (do (.position batch (dec (.position batch))) clj-fn)
         :else (perror "unknown dispatch character " (char x))))
      this))
  (-analyze-eof! [this]
    (eof-handler this config this "dispatch tokenizer machine")))

(defn clj-dispatch
  "Returns CLJ Dispatch Tokenizer Machine."
  [config state item]
  (->CljDispatchTokenizerMachine config state))

;;; unquote dispatch

(deftype UnquoteDispatchTokenizerMachine
  "Tokenizer Machine type for a CLJ Dispatch."
  [config state]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (if (.hasRemaining ^java.nio.CharBuffer batch)
      (let [batch ^java.nio.CharBuffer batch
            x (iint (.get batch))]
        (if (i== x (iAT))
          (->WrapperParser config 'clojure.core/unquote-splicing)
          (do (.position batch (dec (.position batch)))
              (->WrapperParser config 'clojure.core/unquote))))
      this))
  (-analyze-eof! [this]
    (eof-handler this config this
                 "unquote dispatch tokenizer machine")))

;;; arg tokenizer

(deftype ArgLiteralTokenizerMachine
  "Tokenizer Machine type for a CLJ % arguments."
  [config state ^java.lang.StringBuilder ts
   ^:unsynchronized-mutable rest?]
  ITokenizerMachine
  (-analyze-batch! [this bm batch]
    (if (.hasRemaining ^java.nio.CharBuffer batch)
      (let [batch ^java.nio.CharBuffer batch
            c (.get batch)
            x (iint c)]
        (cond (and (i== x (iAMP)) (empty? ts) (not rest?))
              (do (set! rest? true) this)
              (and (idigit? x) (not rest?))
              (do (.append ts c) this)
              (clj-delimiter? x)
              (do (.position batch (dec (.position batch)))
                  (-analyze-eof! this))
              :else
              (let [sts :- java.lang.StringBuilder (edit empty-string)
                    sts (.append sts \%)
                    sts (if rest? (.append sts \&) sts)
                    sts (.append sts ts)
                    sts (.append sts c)]
                (->CljSymbolLiteralTokenizerMachine
                 config state sts false false false))))
      this))
  (-analyze-eof! [this]
    (clj-arg (cond rest? -1
                   (empty? ts) nil
                   :else (java.lang.Long/valueOf
                          ^java.lang.String (settle! ts))))))

(defn arg-literal
  [config state]
  (->ArgLiteralTokenizerMachine
   config state (edit empty-string) false))


;;;; Printer

(def clj-bs-batch (string-to-batch! "\\b"))
(def clj-ff-batch (string-to-batch! "\\f"))
(def clj-backspace-batch (string-to-batch! "\\backspace"))
(def clj-formfeed-batch (string-to-batch! "\\formfeed"))

(def ^:private zeroes "0000")

(defn ^:private to-escape
  "Returns batch containing escape sequence or nil, if no
  escape sequence is needed."
  [config x]
  (let [ci (iint x)]
    (cond (i== ci (iQUOTE)) edn-quote-batch
          (i== ci (iBACKSLASH)) edn-backslash-batch
          (and (i< (iUS) ci) (i< ci (iDEL))) nil
          (i== ci (iBS)) clj-bs-batch
          (i== ci (iFF)) clj-ff-batch
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
          (i== ci (iBS)) clj-backspace-batch
          (i== ci (iFF)) clj-formfeed-batch
          (i== ci (iHT)) edn-tab-batch
          (i== ci (iLF)) edn-newline-batch
          (i== ci (iCR)) edn-return-batch
          :else
          (let [ns (java.lang.Integer/toString ci 16)]
            (string-to-batch!
             (->str "\\u" (.substring ^java.lang.String zeroes
                                      (count ns)) ns))))))

(deftype CljAnnotatedPrinter
  "CLJ Annotated printer machine type."
  [config state item]
  IContainerPrinterMachine
  (-children [this parents]
    (pair (meta item) (assoc-meta item nil)))
  (-print-before! [this bm batch parents]
    (print! batch bm state \^))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

(defprotocol ICljPrinter
  (-print-clj!
    "Returns result or printing `this` as a clj. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(extend-protocol! ICljPrinter
  java.lang.String
  (-print-clj! [this config state bm batch parents]
    (if (:human? config)
      (print! batch bm state (string-to-batch! this))
      (print! batch bm state \" [this #(to-escape config %)] \")))
  clojure.lang.Ratio
  (-print-clj! [this config state bm batch parents]
   (print! batch bm state
           (string-to-batch!
            (.toString ^java.math.BigInteger (.numerator this)))
           \/
           (string-to-batch!
            (.toString ^java.math.BigInteger (.denominator this)))))
  java.lang.Character
  (-print-clj! [this config state bm batch parents]
    (if (:human? config)
      (print! batch bm state (string-to-batch! (.toString this)))
      (to-char-escape config this)))
  clojure.lang.Symbol
  (-print-clj! [this config state bm batch parents]
    (let [s (->str this)]
      (when-not (valid-symbol? s) (perror "invalid symbol " s))
      (string-to-batch! s)))
  clojure.lang.Keyword
  (-print-clj! [this config state bm batch parents]
    (let [s (->str this)]
      (when-not (valid-keyword? s) (perror "invalid keyword " s))
      (string-to-batch! s)))
  java.lang.Class
  (-print-clj! [this config state bm batch parents]
    (let [b (string-to-batch! (.getName this))]
      (print! batch bm state b)))
  java.util.regex.Pattern
  (-print-clj! [this config state bm batch parents]
    (let [b (string-to-batch! (.pattern this))]
      (print! batch bm state \# \" b \"))))


;;;; Pretty printer

(defprotocol ICljPrettyCount
  (-pretty-count-clj
    "Returns the count estimation of the pretty printer collection
    for `this` object in inline mode."
    [this]))

(defprotocol ICljPrettyPrinter
  (-print-pretty-clj!
    "Returns result or pretty printing `this` as a CLJ. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(defn ^:private clj-pretty-mode
  [config coll]
  (let [x (if (satisfies? ICljPrettyCount coll)
            (-pretty-count-clj coll)
            (-pretty-count-edn coll))
        force-no
        (or ;;(seq? coll)
            (when (counted? coll)
              (< (:inline-item-threshold config) (count coll))))]
    (and (not force-no)
         (or (neg? x) (< (:inline-threshold config) x)))))

(deftype CljPrettyTopContainer
  "Top level printer container for CLJ printer."
  [config state coll block?]
  IIndentedMachine
  (-indent [this] (base-indent config))
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (reset-column! state (base-indent config))
    nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents]
    (if block?
      (reset-column! state (base-indent config))
      (move-column! state 1))
    (if block?
      (print-colored! batch bm config state
                      (color config state :indent :syntax)
                      \newline \newline
                      [\space (base-indent config)])
      (print! batch bm state \space))))

(deftype CljUpArrowPrettyPrinter
  "CLJ printer machine type for up arrows :)."
  [config state item]
  IContainerPrinterMachine
  (-children [this parents] [item])
  (-print-before! [this bm batch parents]
    (move-column! state 1)
    (let [r (print! batch bm state
                    (color config state :metadata :comment) \^)]
      (reset! state
              (assoc @state :color-override (constantly nil)))
      r))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]))

(defn ^:private prepare-meta
  [config state m]
  (let [t (:tag m)
        t (when (or (symbol? t) (class? t)) t)
        m (if t (dissoc m :tag) m)
        mk (vec (filter #(true? (m %)) (:meta-keywords config)))
        m (apply dissoc m mk)
        r (if t (conj mk t) mk)
        r (if (empty? m) r (conj r m))]
    (vec (map #(->CljUpArrowPrettyPrinter config state %) r))))

(deftype CljAnnotatedPrettyPrinter
  "CLJ Annotated printer machine type."
  [config state item]
  IContainerPrinterMachine
  (-children [this parents]
    (let [m (apply dissoc (meta item) (:meta-exclude config))]
      (if (empty? m)
        [(assoc-meta item nil)]
        (conj (prepare-meta config state m) (assoc-meta item nil)))))
  (-print-before! [this bm batch parents])
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]
    (reset! state (assoc @state :color-override nil))
    (move-column! state 1)
    (print-colored! batch bm config state \space)))

(deftype CljPrefixPrettyPrinter
  "CLJ printer machine type for custom prefixes."
  {:predicate 'prefix-pretty-printer?}
  [config state prefix pc items]
  IContainerPrinterMachine
  (-children [this parents] items)
  (-print-before! [this bm batch parents]
    (move-column! state pc)
    (print-colored! batch bm config state prefix))
  (-print-after! [this bm batch parents])
  (-print-between! [this bm batch parents]))

(defn ^:private in-quoted?
  "Returns true if one of parents printers is a quote printer."
  [parents]
  ;; massive hack
  (let [sf
        #(when (prefix-pretty-printer? %)
           (let [p ^dunaj.format.clj.CljPrefixPrettyPrinter %]
             (= \' (first (.clear ^java.nio.Buffer (.-prefix p))))))]
    (some sf parents)))

;; TODO: sometimes vec-like indenting would be appropriate
;; (+ 1 2 3 4 5
;;    6 7 8 9 0)
;; and sometimes the current is OK
;; (let [x 1
;;       y 2]
;;   (foo bar)
;;   (baz qux))

(deftype CljPrettyLstContainer
  [config state coll indent block? custom-indent
   ^:unsynchronized-mutable indent-left locals
   ^:unsynchronized-mutable old-bindings indent-group
   ^:unsynchronized-mutable named?]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (when locals
      (set! old-bindings (::local-bindings @state))
      (let [new-bindings (union locals old-bindings)]
        (reset! state (assoc @state ::local-bindings new-bindings))))
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :list :syntax) \())
  (-print-after! [this bm batch parents]
    (reset! state (dissoc @state ::named))
    (when locals
      (reset! state (assoc @state ::local-bindings old-bindings)))
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :list :syntax) \)))
  (-print-between! [this bm batch parents]
    (when named?
      (set! named? false)
      (reset! state (assoc @state ::named true)))
    (let [indent (if custom-indent (+ indent 2) (inc indent))]
      (if (and block? (zero? indent-left))
        (reset-column! state indent)
        (move-column! state 1))
      (if (and block? (zero? indent-left))
        (do (when (zero? indent-left)
              (set! indent-left (max 0 (dec indent-group))))
            (print-colored! batch bm config state
                            (color config state :indent :syntax)
                            \newline [\space indent]))
        (do (when-not (zero? indent-left)
              (set! indent-left (dec indent-left)))
            (print! batch bm state \space))))))

(defn ^:private resolve-local
  [state sym]
  (when-let [rm (::local-bindings @state)] (rm sym)))

(defn ^:private anon-fn?
  [coll]
  (let [anon-arg #(and (symbol? %)
                       (or (= "&" (name %))
                           (and
                            (= \# (last (name %)))
                            (nneg? (.indexOf ^java.lang.String
                                             (name %) "__")))))]
    (and (= 'clojure.core/fn* (first coll))
         (== 3 (count coll))
         (vector? (second coll))
         (nil? (meta (second coll)))
         (every? anon-arg (second coll)))))

(defn anon-transform
  [coll]
  (let [rf (fn [[m i] a]
             (cond
              (= "&" (name a)) [m nil]
              (nil? i) [(assoc m a (symbol (->str "%&"))) nil]
              :else [(assoc m a (symbol (->str "%" i))) (inc i)]))
        argmap (first (reduce rf [{} 1] (second coll)))]
    (prewalk-replace (constantly true) argmap (rest (rest coll)))))

(defn pretty-lst-container
  [config state coll parents]
  (if (anon-fn? coll)
    (->CljPrefixPrettyPrinter config state
                              (string-to-batch! "#") 1
                              (anon-transform coll))
    (let [fname (first coll)
          cns (if-let [c (:resolve-in config)]
                (clojure.core/find-ns c)
                clojure.core/*ns*)
          fvar (when (and (symbol? fname)
                          (not (resolve-local state fname)))
                 (clojure.core/ns-resolve cns fname))
          fvar (when (and (var? fvar) (not (in-quoted? parents)))
                 fvar)
          m (meta fvar)
          named? (when fvar (or (true? (:named m))
                                (and (= :optional (:named m))
                                     (symbol? (second coll)))))
          custom-indent (when fvar (:indent m))
          indent-group (or (when fvar (:indent-group m)) 0)
          indent-left (if (integer? custom-indent) custom-indent 0)
          indent-left (if (and (= :optional (:named m))
                               (symbol? (second coll)))
                        (inc indent-left)
                        indent-left)
          find-bindings (fn [] (some #(when (vector? %) %) coll))
          locals (cond (:let-bindings m)
                       (set (take-nth 2 (find-bindings)))
                       (:fn-params m) (set (find-bindings))
                       :else nil)]
      (when (:let-bindings m)
        (reset! state (assoc @state :let-bindings true)))
      (->CljPrettyLstContainer
       config state coll (get-column state)
       (clj-pretty-mode config coll) custom-indent
       indent-left locals nil indent-group named?))))

(deftype CljPrettyVecContainer
  [config state coll indent block?
   ^:unsynchronized-mutable special-binding]
  IIndentedMachine
  (-indent [this] indent)
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :vector :syntax) \[))
  (-print-after! [this bm batch parents]
    (when (:let-bindings @state)
      (reset! state (dissoc @state :let-bindings)))
    (move-column! state 1)
    (print-colored! batch bm config state
                    (color config state :vector :syntax) \]))
  (-print-between! [this bm batch parents]
    (let [indent (inc indent)
          sb? (or special-binding
                  (and block? (< (:inline-threshold config)
                                 (get-column state))))]
      (when (:let-bindings @state)
        (set! special-binding (not special-binding)))
      (if sb?
        (reset-column! state indent)
        (move-column! state 1))
      (if sb?
        (print-colored! batch bm config state
                        (color config state :indent :syntax)
                        \newline [\space indent])
        (print! batch bm state \space)))))

;; TODO: support for printing sugared type sigs (:- Foo)

(defn ^:private special-list-printer
  [config state coll]
  (when (double? coll)
    (let [f (first coll)]
      (cond
       (= 'clojure.core/var f)
       (->CljPrefixPrettyPrinter
        config state (string-to-batch! "#'") 2 (rest coll))
       (= 'clojure.core/quote f)
       (->CljPrefixPrettyPrinter
        config state (string-to-batch! "'") 1 (rest coll))
       (= 'clojure.core/deref f)
       (->CljPrefixPrettyPrinter
        config state (string-to-batch! "@") 1 (rest coll))
       :else nil))))

(defn ^:private alias-ns
  [^clojure.lang.Namespace cns nsn]
  (let [find-alias (fn [_ [alias ^clojure.lang.Namespace ns]]
                     (when (= (name (.-name ns)) nsn)
                       (reduced (name alias))))]
    (reduce find-alias nil (.getAliases cns))))

(defn ^:private resolve-ns
  [^clojure.lang.Namespace cns sym]
  (let [ons (namespace sym)]
    (if-let [r (clojure.core/ns-resolve cns sym)]
      (if (var? r)
        (let [rns (.-name (.-ns ^clojure.lang.Var r))]
          (when-not (= rns (.-name cns))
            (or (alias-ns cns ons) ons)))
        ons)
      ons)))

(defn ^:private resolve-name
  [sym state cns]
  (let [n (name sym)
        ns (namespace sym)
        ss (->str sym)]
    (if (or (index-of n \.) (nil? ns) (nil? cns))
      ss
      (let [nns (resolve-ns cns sym)]
        (if (nil? nns) (->str n) (->str nns \/ n))))))

(defn ^:private resolve-color
  [sym state cns]
  (let [n (name sym)
        ns (namespace sym)
        ss (->str sym)]
    (cond (or (matches #"[\p{Alnum}/$_]*\p{Upper}[\p{Alnum}/$_]*" ss)
              ;; TODO: improve host regexes
              (index-of n \.))
          [:host]
          (or (nil? cns) (resolve-local state sym)) nil
          (clojure.core/special-symbol? sym)
          [:special-form :core :gvar]
          :else
          (when-let [v (clojure.core/ns-resolve cns sym)]
            (if (var? v)
              (let [vns (name (.-name (.-ns ^clojure.lang.Var v)))
                    h (when-let [x (:highlight (meta v))]
                        (if (sequential? x) x [x]))
                    m (vec h)
                    m (if (matches #"(dunaj\.).*|(clojure\.).*" vns)
                        (conj m :core)
                        m)]
                (conj m :gvar))
              [:host])))))

(extend-protocol! ICljPrettyPrinter
  clojure.lang.IPersistentList
  (-print-pretty-clj! [this config state bm batch parents]
    (or (special-list-printer config state this)
        (pretty-lst-container config state this parents)))
  clojure.lang.ASeq
  (-print-pretty-clj! [this config state bm batch parents]
    (or (special-list-printer config state this)
        (pretty-lst-container config state this parents)))
  java.lang.String
  (-print-pretty-clj! [this config state bm batch parents]
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
      (move-column! state x)
      b))
  clojure.lang.IPersistentVector
  (-print-pretty-clj! [this config state bm batch parents]
    (let [machine (first parents)]
      (if (class-instance? java.util.Map$Entry this)
        (->EdnPrettyEntryContainer config state this
                                   (get-column state))
        (->CljPrettyVecContainer config state this
                                 (get-column state)
                                 (clj-pretty-mode config this)
                                 false))))
  clojure.lang.Ratio
  (-print-pretty-clj! [this config state bm batch parents]
    (let [bn (string-to-batch! (.toString ^java.math.BigInteger
                                         (.numerator this)))
          bd (string-to-batch! (.toString ^java.math.BigInteger
                                         (.denominator this)))]
      (move-column! state (+ (.remaining ^java.nio.Buffer bn)
                                 (.remaining ^java.nio.Buffer bd)
                                 1))
      (print-colored! batch bm config state
                      (color config state :rational :number)
                      bn \/ bd)))
  java.lang.Character
  (-print-pretty-clj! [this config state bm batch parents]
    (let [b (to-char-escape config this)]
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :char :string) b)))
  clojure.lang.Symbol
  (-print-pretty-clj! [this config state bm batch parents]
    (let [s (->str this)
          ns (if-let [c (:resolve-in config)]
               (clojure.core/find-ns c)
               clojure.core/*ns*)
          b (string-to-batch! (resolve-name this state ns))
          c (cond (in-quoted? parents) nil
                  (::named @state)
                  (do (reset! state (dissoc @state ::named)) [:name])
                  :else (resolve-color this state ns))]
      (when-not (valid-symbol? s) (perror "invalid symbol " s))
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (apply color config state
                             (concat c [:symbol :identifier]))
                      b)))
  clojure.lang.Keyword
  (-print-pretty-clj! [this config state bm batch parents]
    (let [s (->str this)
          b (string-to-batch! s)]
      (when-not (valid-keyword? s) (perror "invalid keyword " s))
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :keyword :identifier)
                      b)))
  java.lang.Class
  (-print-pretty-clj! [this config state bm batch parents]
    (let [b (string-to-batch! (.getName this))]
      (move-column! state (.remaining ^java.nio.Buffer b))
      (print-colored! batch bm config state
                      (color config state :class :symbol :identifier)
                      b)))
  java.util.regex.Pattern
  (-print-prettyclj! [this config state bm batch parents]
    (let [b (string-to-batch! (.pattern this))]
      (move-column! state (+ 3 (.remaining ^java.nio.Buffer b)))
      (print-colored! batch bm config state
                      (color config state :regex :string)
                      \# \" b \"))))

(defn ^:private helper-container?
  [p]
  (or (entry-container? p)
      (class-instance? dunaj.format.clj.CljAnnotatedPrettyPrinter p)
      (class-instance? dunaj.format.edn.EdnPrettyMapPrinter p)
      (class-instance? dunaj.format.clj.CljAnnotatedPrettyPrinter p)
      (class-instance? dunaj.format.clj.CljUpArrowPrettyPrinter p)
      (class-instance? dunaj.format.clj.CljPrefixPrettyPrinter p)))

(defrecord CljPrettyPrinterFactory
  "CLJ Pretty Printer Factory record."
  [invalid-item indent-offset inline-threshold inline-item-threshold
   color-fn pretty-level-limit
   pretty-item-limit pretty-string-limit color? meta?]
  IPrinterMachineFactory
  (-printer-config [this]
    {:invalid-item invalid-item
     :indent-offset indent-offset
     :inline-threshold inline-threshold
     :inline-item-threshold inline-item-threshold
     :color-fn color-fn
     :color? color?
     :meta? meta?
     :resolve-in nil
     :meta-keywords #{:private :unsynchronized-mutable
                      :volatile-mutable :const :dynamic :once}
     :meta-exclude #{:line :column}
     :level-limit-pred
     (fn [p] (and pretty-level-limit
                 (pos? pretty-level-limit)
                 (not (helper-container? (first p)))
                 (< pretty-level-limit
                    (count (vec (remove helper-container? p))))))
     :level-limit-print-fn
     (fn [batch bm state]
       (let [config {:color-fn color-fn}]
         (print-colored! batch bm config state
                         (dunaj.format.printer/color
                          config state :limit :syntax)
                         edn-level-limit-batch)))
     :item-limit-batch
     (let [batches
           [(color-fn :limit :syntax)
            edn-item-limit-batch default-color]]
       (string-to-batch!
        (apply ->str (map #(.clear ^java.nio.Buffer %) batches))))
     :string-limit pretty-string-limit
     :item-limit pretty-item-limit})
  (-printer-from-type [this] (keyword->class :object))
  (-printer-to-type [this] (keyword->class :char))
  (-top-container [this config state coll]
    (->CljPrettyTopContainer
     config state coll (clj-pretty-mode config coll)))
  (-dispatch-printer
    [this config state item bm batch parents]
    (cond (and meta?
               (satisfies? IMeta item)
               (not (empty? (meta item))))
          (->CljAnnotatedPrettyPrinter config state item)
          (satisfies? ICljPrettyPrinter item)
          (-print-pretty-clj! item config state
                              bm batch parents)
          (satisfies? IEdnPrettyPrinter item)
          (-print-pretty-edn! item config state
                              bm batch parents)
          (or (class-instance? java.nio.Buffer item)
              (satisfies? IContainerPrinterMachine item)) item
          :else
          (let [s (clojure.core/print-str item)]
            (print-colored! batch bm config state
                            (color config state :invalid)
                            (string-to-batch! s)))))
  IPrinterFactory
  (-print [this coll]
    (pretty-printer-engine this coll))
  (-print [this]
    (pretty-printer-engine this)))


;;;; Formatter

(defrecord CljFormatterFactory
  "CLJ Formatter Factory record."
  [incomplete-mode lazy? invalid-item tag-readers default-tag-reader
   token-item-limit container-item-limit container-level-limit
   resolve-ns-fn read-eval human-printer? meta?]
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
     :resolve-ns resolve-ns-fn
     :resolve-in nil
     :read-eval read-eval
     :default-tag-reader
     (or default-tag-reader clojure.core/*default-data-reader-fn*)})
  (-dispatch-tokenizer [this config state item]
    (let [x (iint item)]
      (cond (edn-whitespace? x) this
            (i== x (iQUOTE)) (clj-string-literal config state x)
            (i== x (iPERCENT)) (arg-literal config state)
            (edn-symbol-start? x) ;; before number, after arg
            (clj-symbol-literal config state x)
            (clj-number-start? x)
            (clj-number-literal config state x)
            (i== x (iLBRACKET)) (edn-vec-container config state)
            (i== x (iRBRACKET)) edn-bracket-close
            (i== x (iLBRACE)) (edn-map-container config state)
            (i== x (iRBRACE)) edn-brace-close
            (i== x (iHASH)) (clj-dispatch config state x)
            (i== x (iSEMICOLON))
            (edn-lineskip-literal config state x)
            (i== x (iLPAR)) (edn-lst-container config state)
            (i== x (iRPAR)) edn-par-close
            (i== x (iAPOS))
            (->WrapperParser config 'clojure.core/quote)
            (i== x (iAT)) (->WrapperParser config 'clojure.core/deref)
            (i== x (iTILDE))
            (->UnquoteDispatchTokenizerMachine config state)
            (i== x (iARROWHEAD)) (metadata-parser config state)
            (i== x (iBACKQUOTE))
            (->SyntaxQuoteParser config state)
            (i== x (iBACKSLASH))
            (edn-character-literal
             config state x clj-character-map true clj-delimiter?)
            :else (perror "invalid item " item))))
  (-dispatch-parser [this config state token parents]
    (cond
     (identical? token clj-fn) (dispatch-fn-token config state)
     (clj-arg? token) (dispatch-arg-token config state token)
     :else token))
  ILazyParserMachineFactory
  (-dispatch-lazy-parser [this config state token parents]
    (-dispatch-parser this config state token parents))
  IParserFactory
  (-parse [this coll]
    ((if lazy? lazy-parser-engine parser-engine) this coll))
  (-parse [this]
    (when lazy? (throw (unsupported-operation)))
    (parser-engine this))
  IPrinterMachineFactory
  (-printer-config [this]
    {:invalid-item invalid-item
     :meta? meta?
     :human? human-printer?})
  (-printer-from-type [this]
    (keyword->class :object))
  (-printer-to-type [this]
    (keyword->class :char))
  (-top-container [this config state coll]
    (->EdnTopContainer config state coll))
  (-dispatch-printer [this config state item bm batch parents]
    (cond (and (satisfies? IMeta item) meta?
               (not (empty? (meta item))))
          (->CljAnnotatedPrinter config state item)
          (satisfies? ICljPrinter item)
          (-print-clj! item config state bm batch parents)
          (satisfies? IEdnPrinter item)
          (-print-edn! item config state bm batch parents)
          :else
          (let [s (clojure.core/print-str item)]
            (print! batch bm state (string-to-batch! s)))))
  IPrinterFactory
  (-print [this]
    (printer-engine this))
  (-print [this coll]
    (printer-engine this coll)))


;;;; Public API

(def clj
  "A CLJ formatter factory."
  {:added v1
   :see '[lazy-clj pretty-clj]}
  (->CljFormatterFactory
   :keep false nil nil nil 1000000 1000000 1000000
   default-ns-resolver true false false))

(def lazy-clj
  "A lazy CLJ formatter factory."
  {:added v1
   :see '[clj pretty-clj]}
  (assoc clj :lazy? true))

(def clj-colorer-map
  {:limit cyan
   :host magenta
   :special-form bold-red
   :identifier yellow
   :def yellow
   :name bold-yellow
   :symbol white
   :gvar cyan
   :metadata yellow
   :core bold-cyan
   :char bold-green})

(def pretty-clj
  "A CLJ printer factory with pretty printing."
  {:added v1
   :see '[lazy-clj clj]}
  (->CljPrettyPrinterFactory
   nil 0 70 25 (custom-colorer clj-colorer-map)
   10 100 200 false false))
