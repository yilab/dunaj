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

(ns dunaj.format.charset
  "Charset formatter.

  Charset factories support following options:

  * `:charset` - charset type, use `charset-formatter` to set this
    option
  * `:replacement` - replacement string
  * `:malformed-mode` - see `charset-formatter` for available options
  * `:unmappable-mode` - see `charset-formatter` for available
    options"
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Maybe Va Fn Any AnyFn U I]]
   [dunaj.boolean :refer [Boolean or not and]]
   [dunaj.host :refer
    [Class Array AnyBatch BatchManager keyword->class set!]]
   [dunaj.host.int :refer [Int iint iadd i0]]
   [dunaj.math :refer [Integer max <]]
   [dunaj.compare :refer [identical? nil?]]
   [dunaj.state :refer [clone]]
   [dunaj.flow :refer [if let when cond when-not loop recur do condp]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [deftype defrecord satisfies?]]
   [dunaj.coll :refer
    [IReducing reduced IRed IHomogeneous reduced? item-type reducing
     IBatchedRed postponed postponed? advance unsafe-advance! reduce]]
   [dunaj.function :refer [Function fn defn identity comp]]
   [dunaj.coll.helper :refer
    [defxform cloned-advance-fn reduce-batched* reduced-advance
     reduce-augmented* reduce* finish-advance strip-reduced
     reduce-with-batched*]]
   [dunaj.error :refer [throw illegal-argument ex-info]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.host.batch :refer [batch-manager item-types-match? batch]]
   [dunaj.host.array :refer [array-manager]]
   [dunaj.string :refer [String ->str]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.state.var :refer [Var var def]]
   [dunaj.coll.util :refer [merge recipe]]
   [dunaj.coll.recipe :refer [map concat*]]
   [dunaj.format :refer [IParserFactory IPrinterFactory parse print
                         -parse -print default-formatter-batch-size]]
   [dunaj.format.helper :refer [prepend-unread]]))


;;;; Implementation details

(def ^:dynamic ^:private *default-malformed-mode* :- Keyword
  "Default malformed mode."
  :replace)

(def ^:dynamic ^:private *default-unmappable-mode* :- Keyword
  "Default unmappable mode."
  :replace)

(defn ^:private coding-error-action
  :- (Maybe java.nio.charset.CodingErrorAction)
  "Returns host coding error action instance for a given coder
  error `mode`, which can be one of :replace, :ignore or :report."
  [mode :- Keyword]
  (condp identical? mode
    :replace java.nio.charset.CodingErrorAction/REPLACE
    :ignore java.nio.charset.CodingErrorAction/IGNORE
    :report java.nio.charset.CodingErrorAction/REPORT))

(defn ^:private get-from-type :- Class
  "Returns item type for from coll for a given `code-mode`."
  [code-mode :- Keyword]
  (keyword->class (if (identical? :encode code-mode) :char :byte)))

(defn ^:private get-to-type :- Class
  "Returns item type for to coll for a given `code-mode`."
  [code-mode :- Keyword]
  (keyword->class (if (identical? :encode code-mode) :byte :char)))

(defn ^:private get-decoder :- java.nio.charset.CharsetDecoder
  "Returns new decoder instance based on given input args."
  [charset :- java.nio.charset.Charset,
   replacement :- (Maybe String), malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword)]
  (let [malformed-mode (or malformed-mode *default-malformed-mode*)
        unmappable-mode (or unmappable-mode *default-unmappable-mode*)
        decoder (-> (.newDecoder charset)
                    (.onMalformedInput
                     (coding-error-action malformed-mode))
                    (.onUnmappableCharacter
                     (coding-error-action unmappable-mode)))]
    (when replacement (.replaceWith decoder replacement))
    decoder))

(defn ^:private get-encoder :- java.nio.charset.CharsetEncoder
  "Returns new encoder instance based on given input args."
  [charset :- java.nio.charset.Charset,
   replacement :- (Maybe String), malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword)]
  (let [malformed-mode (or malformed-mode *default-malformed-mode*)
        unmappable-mode (or unmappable-mode *default-unmappable-mode*)
        arr (when replacement
              (let [batch :- AnyBatch
                    (.encode charset ^java.lang.String replacement)
                    byte-type (keyword->class :byte)
                    am (array-manager byte-type)
                    bm (batch-manager byte-type)
                    arr (.allocate am (.remaining batch))]
                (.copy bm batch arr 0 (.remaining batch))
                arr))
        encoder (-> (.newEncoder charset)
                    (.onMalformedInput
                     (coding-error-action malformed-mode))
                    (.onUnmappableCharacter
                     (coding-error-action unmappable-mode)))]
    (when arr (.replaceWith encoder arr))
    encoder))

(defn ^:private get-coder
  :- (U java.nio.charset.CharsetDecoder
        java.nio.charset.CharsetEncoder)
  "Returns encoder or decoder based on given input args."
  [code-mode :- Keyword, charset :- java.nio.charset.Charset,
   replacement :- (Maybe String), malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword)]
  ((if (identical? :encode code-mode) get-encoder get-decoder)
   charset replacement malformed-mode unmappable-mode))

(defn ^:private get-flush-fn :- Function
  "Returns flush function based on given input args."
  [code-mode :- Keyword, coder :- (U java.nio.charset.CharsetDecoder
                                     java.nio.charset.CharsetEncoder)]
  (if (identical? :encode code-mode)
    #(.flush ^java.nio.charset.CharsetEncoder coder %)
    #(.flush ^java.nio.charset.CharsetDecoder coder %)))

(defn ^:private get-code-fn :- Function
  "Returns encode or decode function based on given input args."
  [code-mode :- Keyword, coder :- (U java.nio.charset.CharsetDecoder
                                     java.nio.charset.CharsetEncoder)]
  (if (identical? :encode code-mode)
    #(.encode ^java.nio.charset.CharsetEncoder coder %1 %2 %3)
    #(.decode ^java.nio.charset.CharsetDecoder coder %1 %2 %3)))

(defn ^:private compute-capacity :- Integer
  "Returns capacity based on requested capacity and default formatter
  batch sizes."
  [requested-capacity :- (Maybe Integer)]
  (max @default-formatter-batch-size (or requested-capacity 0)))

(defn ^:private check-result :- nil
  "Returns nil and throws with `message` if `res` is not UNDERFLOW or
  OVERFLOW coder result."
  [res :- Any, message :- String]
  (when-not
      (or (identical? res java.nio.charset.CoderResult/UNDERFLOW)
          (identical? res java.nio.charset.CoderResult/OVERFLOW))
    (throw (ex-info message {:result res}))))

(defn ^:private overflow? :- Boolean
  "Returns true if res is OVERFLOW."
  [res :- Any]
  (identical? res java.nio.charset.CoderResult/OVERFLOW))

(defn ^:private cache-unread! :- nil
  "Moves unread data from `batch` into `cache`,
  or wipes `cache` if no unread data is present. Returns nil."
  [bm :- BatchManager, batch :- AnyBatch, cache :- AnyBatch]
  (if (.hasRemaining batch)
    (do (.clear cache)
        (.copy bm batch cache)
        (.flip cache))
    (.limit cache (i0)))
  nil)

(deftype CCWrap
  [ret :- Any, code-fn :- Function, flush-fn :- Function,
   unread-batch :- AnyBatch, to-batch :- (Maybe AnyBatch)])

(defn cc-advance
  [ret :- Any, code-fn :- Function, flush-fn :- Function,
   unread-batch :- AnyBatch, to-batch :- (Maybe AnyBatch)]
  (cond
   (reduced? ret)
   (reduced (->CCWrap @ret code-fn flush-fn unread-batch to-batch))
   (postponed? ret)
   (postponed (->CCWrap @ret code-fn flush-fn unread-batch to-batch)
              #(cc-advance (advance ret) code-fn flush-fn
                           (clone unread-batch) (clone to-batch))
              #(cc-advance (unsafe-advance! ret) code-fn flush-fn
                           unread-batch to-batch))
   :else (->CCWrap ret code-fn flush-fn unread-batch to-batch)))

(defn ^:private prepare-to-batch :- AnyBatch
  [tbm :- BatchManager,
   to-batch :- (Maybe AnyBatch), from-batch :- AnyBatch]
  (cond from-batch
        (let [capacity (compute-capacity (.remaining from-batch))]
          (if (or (nil? to-batch) (< (.capacity to-batch) capacity))
            (.allocate tbm capacity)
            to-batch))
        to-batch to-batch
        :else (.allocate tbm (compute-capacity nil))))

(deftype BatchedCharsedCoderReducing
  [r :- IReducing,
   charset :- java.nio.charset.Charset,
   replacement :- (Maybe String),
   malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword)
   code-mode :- Keyword,
   batch-size :- Integer,
   fbm :- BatchManager, tbm :- BatchManager]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [w (strip-reduced wrap)
          ret (.-ret ^dunaj.format.charset.CCWrap w)
          code-fn (.-code_fn ^dunaj.format.charset.CCWrap w)
          flush-fn (.-flush_fn ^dunaj.format.charset.CCWrap w)
          unread-batch (.-unread_batch ^dunaj.format.charset.CCWrap w)
          to-batch (.-to_batch ^dunaj.format.charset.CCWrap w)
          from-batch unread-batch
          to-batch (prepare-to-batch tbm to-batch from-batch)
          af (fn af [ret :- Any, from-batch :- AnyBatch,
                     to-batch :- (Maybe AnyBatch), state]
               (cond
                 (reduced? ret) ret
                 (postponed? ret)
                 (postponed @ret
                            #(af (advance ret) (clone from-batch)
                                 (clone to-batch) state)
                            #(af (unsafe-advance! ret) from-batch
                                 to-batch state))
                 (nil? from-batch) ret
                 (identical? :process state)
                 (let [res
                       (code-fn from-batch (.clear to-batch) false)]
                   (check-result res "coding failed")
                   (recur (if (.hasRemaining (.flip to-batch))
                            (._step r ret to-batch)
                            ret)
                          from-batch to-batch
                          (if (overflow? res) :process :final)))
                 (identical? :final state)
                 (let [res
                       (code-fn from-batch (.clear to-batch) true)]
                   (check-result res "final coding failed")
                   (recur (if (.hasRemaining (.flip to-batch))
                            (._step r ret to-batch)
                            ret)
                          from-batch to-batch
                          (if (overflow? res) :final :flush)))
                 (identical? :flush state)
                 (let [res (flush-fn (.clear to-batch))]
                   (check-result res "flush failed")
                   (recur (if (.hasRemaining (.flip to-batch))
                            (._step r ret to-batch)
                            ret)
                          from-batch to-batch
                          (if (overflow? res) :flush :done)))
                 :done ret))]
      (-> (af ret from-batch to-batch :process)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (let [coder (get-coder code-mode charset replacement
                           malformed-mode unmappable-mode)
          code-fn (get-code-fn code-mode coder)
          flush-fn (get-flush-fn code-mode coder)
          unread-batch (.limit (.allocate fbm batch-size) (i0))
          to-batch nil]
      (->CCWrap
       (._wrap r ret) code-fn flush-fn unread-batch to-batch)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.format.charset.CCWrap wrap)))
  (-step [this wrap batch]
    (let [ret (.-ret ^dunaj.format.charset.CCWrap wrap)
          code-fn (.-code_fn ^dunaj.format.charset.CCWrap wrap)
          flush-fn (.-flush_fn ^dunaj.format.charset.CCWrap wrap)
          unread-batch
          (.-unread_batch ^dunaj.format.charset.CCWrap wrap)
          to-batch (.-to_batch ^dunaj.format.charset.CCWrap wrap)
          from-batch (prepend-unread fbm batch unread-batch)
          to-batch (prepare-to-batch tbm to-batch from-batch)
          af (fn af [ret :- Any, from-batch :- AnyBatch,
                     unread-batch :- AnyBatch, to-batch :- AnyBatch]
               (cond (reduced? ret)
                     (reduced (->CCWrap @ret code-fn flush-fn
                                        nil to-batch))
                     (postponed? ret)
                     (postponed
                      (->CCWrap @ret code-fn flush-fn
                                unread-batch to-batch)
                      #(af (advance ret) (clone from-batch)
                           (clone unread-batch) (clone to-batch))
                      #(af (unsafe-advance! ret) from-batch
                           unread-batch to-batch))
                     (.hasRemaining from-batch)
                     (let [_ (.clear to-batch)
                           res (code-fn from-batch to-batch false)]
                       (check-result res "coding failed")
                       (when-not (overflow? res)
                         (cache-unread! fbm from-batch unread-batch))
                       (recur (if (.hasRemaining (.flip to-batch))
                                (._step r ret to-batch)
                                ret)
                              from-batch unread-batch to-batch))
                     :else (->CCWrap ret code-fn flush-fn
                                     unread-batch to-batch)))]
      (af ret from-batch unread-batch to-batch))))

(defxform batched-charset-coder*
  [charset :- java.nio.charset.Charset,
   replacement :- (Maybe String), malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword), code-mode :- Keyword]
  ([r] (let [from-type (get-from-type code-mode)
             to-type (get-to-type code-mode)
             batch-size (compute-capacity nil)
             fbm (batch-manager from-type)
             tbm (batch-manager to-type)]
         (->BatchedCharsedCoderReducing
          r charset replacement malformed-mode
          unmappable-mode code-mode batch-size fbm tbm)))
  :count false
  :unpack false
  :fold false
  :section false)

(deftype BatchedCharsetCoder
  "A type for charset encoder and decoder."
  [coll :- (Maybe IRed), charset :- java.nio.charset.Charset,
   replacement :- (Maybe String), malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword), code-mode :- Keyword]
  IRed
  (-reduce [this reducef init]
    (let [to-type (get-to-type code-mode)
          tbm (batch-manager to-type)
          size-hint @default-formatter-batch-size]
      (reduce-with-batched* to-type size-hint this reducef init)))
  IHomogeneous
  (-item-type [this] (get-to-type code-mode))
  IBatchedRed
  (-reduce-batched [this item-type size-hint reducef init]
    (let [from-type (get-from-type code-mode)]
      (reduce-augmented*
       coll
       #(reduce-batched* from-type size-hint % %2 %3)
       ((batched-charset-coder*
         charset replacement malformed-mode unmappable-mode code-mode)
        (reducing reducef init)))))
  IConfig
  (-config [this] {:charset charset
                   :replacement replacement
                   :malformed-mode malformed-mode
                   :unmappable-mode unmappable-mode
                   :code-mode code-mode}))

(defrecord CharsetFormatterFactory
  [charset :- java.nio.charset.Charset,
   replacement :- (Maybe String), malformed-mode :- (Maybe Keyword),
   unmappable-mode :- (Maybe Keyword)]
  IParserFactory
  (-parse [this]
    (comp (batch (keyword->class :byte) (compute-capacity nil))
          (batched-charset-coder*
           charset replacement malformed-mode unmappable-mode :decode)
          (concat*)))
  (-parse [this coll]
    (if (and (satisfies? IBatchedRed coll)
             (item-types-match? (keyword->class :byte)
                                (item-type coll)))
      (->BatchedCharsetCoder coll charset replacement
                             malformed-mode unmappable-mode :decode)
      (recipe (-parse this) coll)))
  IPrinterFactory
  (-print [this]
    (comp
     (batch (keyword->class :char) (compute-capacity nil))
     (batched-charset-coder*
      charset replacement malformed-mode unmappable-mode :encode)
     (concat*)))
  (-print [this coll]
    (if (and (satisfies? IBatchedRed coll)
             (item-types-match? (keyword->class :char)
                                (item-type coll)))
      (->BatchedCharsetCoder coll charset replacement
                             malformed-mode unmappable-mode :encode)
      (recipe (-print this) coll))))

(defn ^:private charset-formatter* :- CharsetFormatterFactory
  "Returns charset formatter factory for a given `charser` host
  charset and `opts` map."
  [charset :- java.nio.charset.Charset, opts :- {}]
   (map->CharsetFormatterFactory (merge opts {:charset charset})))


;;;; Public API

(defn charset-formatter :- (I IParserFactory IPrinterFactory)
  "Returns charset formatter for a given `_charset_` string.

  May supply following additional options:

  * `:replacement` - `nil` (default, chooses charsets default) or
    string.
  * `:malformed-mode` - `:ignore`, `:replace` (default), `:report`.
  * `:unmappable-mode` -`:ignore`, `:replace` (default), `:report`."
  {:added v1
   :see '[utf-8 utf-16 default-charset]}
  [charset :- String & {:as opts}]
  (charset-formatter* (java.nio.charset.Charset/forName charset)
                      opts))

(def utf-8 :- (I IParserFactory IPrinterFactory)
  "UTF-8 charset formatter factory."
  {:added v1
   :see '[utf-16 default-charset charset-formatter]}
  (charset-formatter* java.nio.charset.StandardCharsets/UTF_8 nil))

(def utf-16 :- (I IParserFactory IPrinterFactory)
  "UTF-16 charset formatter factory which supports byte-order mark
  when decoding and defaults to and encodes in the big endian."
  {:added v1
   :see '[utf-8 charset-formatter default-charset]}
  (charset-formatter* java.nio.charset.StandardCharsets/UTF_16 nil))

(def default-charset :- (I IParserFactory IPrinterFactory)
  "A default charset formatter factory as specified by host."
  {:added v1
   :see '[charset-formatter utf-8 utf-16]}
  (charset-formatter* (java.nio.charset.Charset/defaultCharset) nil))
