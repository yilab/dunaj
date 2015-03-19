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

(ns dunaj.format.printer
  "Fast printer engine with helper functions."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Colorer" "Indentation"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn Maybe I U AnyFn KeywordMap]]
   [dunaj.boolean :refer [Boolean or not and boolean]]
   [dunaj.host :refer
    [Class AnyBatch BatchManager keyword->class set! class-instance?]]
   [dunaj.host.int :refer
    [iint iinc i== i< isub izero? idec ineg? i> i< i<< iadd iloop i0
     i1 i2 i3 i4 i5 i8 ixFF ione? i-1 imul i27 idigit? ioctal? ihexa?
     ihexa->int idigit->int iHT iCR iLF iCOLON iSPACE iCOMMA i<= imax
     iQUOTE iAPOS iBACKSLASH ipos? iLBRACKET iSMALL_M iSMALL_B
     iSMALL_F iSMALL_N iSMALL_R iSMALL_U iSMALL_T]]
   [dunaj.math :refer
    [Integer max min < integer? == > neg? pos? zero? one?]]
   [dunaj.math.unchecked :as mu]
   [dunaj.compare :refer [identical? nil?]]
   [dunaj.state :refer [reset! realized? IReference clone]]
   [dunaj.flow :refer [if let when cond when-not loop recur do condp
                       if-not if-let delay while]]
   [dunaj.threading :refer [-> ->>]]
   [dunaj.feature :refer [IConfig meta]]
   [dunaj.poly :refer [Type satisfies? defprotocol deftype type]]
   [dunaj.coll :refer
    [reduced reduce IRed IHomogeneous reduced? assoc vector? list? pop
     IBatchedRed first second rest empty? postponed? postponed peek
     advance unsafe-advance! IReducing conj reducing Transducer
     next ISeq ISequential ISeqable seq single? count unsafe-postponed
     IPersistentList settle! edit]]
   [dunaj.coll.helper :refer
    [reduce* reduce-with-batched* reduce-augmented* finish-advance
     defxform reduced-advance strip-reduced]]
   [dunaj.host.array :refer [array-manager array aget acount]]
   [dunaj.host.batch :refer
    [batch-manager select-item-type batch-support?
     item-types-match? batch-manager-from decide-item-type
     provide-batch-size provide-batch-manager]]
   [dunaj.function :refer [Function fn defn apply comp identity]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.error :refer
    [throw illegal-argument illegal-state unsupported-operation]]
   [dunaj.identifier :refer [INamed Keyword name symbol symbol?]]
   [dunaj.state.var :refer [Var def]]
   [dunaj.state.basic :refer [unsynchronized-reference]]
   [dunaj.string :refer [->str str empty-string string? camel-case]]
   [dunaj.format :refer [default-formatter-batch-size]]
   [dunaj.coll.default :refer [->lst]]
   [dunaj.coll.util :refer [some merge]]
   [dunaj.coll.recipe :refer [concat mapcat partition cap]]
   [dunaj.coll.tuple :as ct :refer [tuple]]
   [dunaj.format.helper :refer [string-to-batch!]]))


;;;; Public API

(defprotocol IContainerPrinterMachine
  "A protocol for container printer machine. Is used in the printer
  engine to process potentionally nested data containers."
  {:added v1
   :category "Primary"
   :predicate 'container-printer-machine?
   :forbid-extensions true}
  (-children :- []
    "Returns the collection of child items which should be printed
    before `_this_` printer machine can complete. `_parents_` contains
    list of parent machines, including `_this_` and top container."
    [this parents :- IPersistentList])
  (-print-before! :- (Maybe AnyBatch)
    "Returns the batch of printed values which should be put before
    child items. Returns `nil` if printed values were instead put
    into `_batch_` or if no prefix was needed. `_parents_` contains
    list of parent machines, including `_this_` and top container.
    Batch manager `_bm_` should be used for all batch related things."
    [this bm :- BatchManager, batch :- AnyBatch,
     parents :- IPersistentList])
  (-print-after! :- (Maybe AnyBatch)
    "Returns the batch of printed values which should be put after
    child items. Returns `nil` if printed values were instead put
    into `_batch_` or if no postfix was needed. `_parents_` contains
    list of parent machines, including `_this_` and top container.
    Batch manager `_bm_` should be used for all batch related things."
    [this bm :- BatchManager, batch :- AnyBatch,
     parents :- IPersistentList])
  (-print-between! :- (Maybe AnyBatch)
    "Returns the separator which should be added between two child
    items. As the printer machine may keep an internal state, the
    result from `_this_` method is not cached but is called
    repeatedly between child items.

    Returns `nil` if printed values were put into `_batch_` instead
    or if no separator was needed. `_parents_` contains list of parent
    machines, including `_this_` and top container. Batch manager
    `_bm_` should be used for all batch related things."
    [this bm :- BatchManager, batch :- AnyBatch,
     parents :- IPersistentList]))

(defprotocol IPrinterMachineFactory
  "A factory protocol for the execution of a printer engine."
  {:added v1
   :category "Primary"}
  (-printer-config :- {}
    "Returns a configuration map that is passed into machines
    created directly or indirectly with `_this_` printer machine
    factory. Passing of configuration map is done by convention as a
    first argument of machine constructor, with folowing keys
    recognized by the printer engine and its helper fns:

    * `:invalid-item` - Controls how machine should behave if
      unprintable items is passed into the printer. Valid values are
      `nil` (throws), `:ignore` (behave as if there was no such item)
      or a function which is called with the current config, invalid
      item, parents and result of which is used for printing instead.
    * `:initial-state` - specifies an initial state of a printer
      machine. Can be `nil` or map
    * `:indent-offset` - number representing indent offset
    * `:indent-size` - number representing indent size
    * `:color?` - boolean flag whether output should be in color
    * `:color-fn` - fn taking key and returning colorizer batch
    * `:string-limit` - number representing string character limit
    * `:item-limit` - number representing item limit
    * `:item-limit-batch` - batch which should be printed if item
      limit is reached
    * `:level-limit-pred` - fn taking list of parents and returns
      `true` if level limit has been reached.
    * `:level-limit-print-fn` - fn taking batch, bm and state, and
      prints limit batch to `batch` or returns limit batch."
    [this])
  (-printer-from-type :- (U nil Class Type)
    "Returns type of items which are to be printed."
    [this])
  (-printer-to-type :- (U nil Class Type)
    "Returns type of printed values."
    [this])
  (-top-container :- IContainerPrinterMachine
    "Returns the top level container printing machine used in
    the printing engine to process top level items.
    `_coll_` may be `nil`."
    [this config :- {}, state :- IReference, coll :- (Maybe IRed)])
  (-dispatch-printer :- Any
    "Returns a batch of printed values or a container printer machine
    for printing nested data containers, as a result of printing on a
    given input `_item_` according to the `_config_` configuration.
    `_parents_` contains list of parent machines, including top
    container. Returns `nil` if printed values were put into `_batch_`
    instead. Dispatcher should use given `_bm_` batch manager for all
    batch related things."
    [this config :- {}, state :- IReference, item :- Any,
     bm :- BatchManager, batch :- AnyBatch, parents :- IReference]))

;;; Helper functions for implementers of printer machines

(defprotocol IIndentedMachine
  "A protocol for basic indentation mechanism in printer machines."
  {:added v1
   :category "Indentation"
   :see '[base-indent next-indent]}
  (-indent :- Integer
    "Returns indent amount for `_this_` machine."
    [this]))

(defn base-indent :- (Maybe Integer)
  "Returns base indentation value based on the given `_config_`. Uses
  `:indent-size` key in `_config_` to obtain the indentation size."
  {:added v1
   :category "Indentation"
   :see '[next-indent prev-indent IIndentedMachine]}
  [config :- {}]
  (:indent-offset config))

(defn next-indent :- (Maybe Integer)
  "Returns new indentation value based on the given `_config_` and
  parent `_machine_`. Uses `:indent-size` key in `_config_`
  to obtain the indentation size."
  {:added v1
   :category "Indentation"
   :see '[base-indent next-indent IIndentedMachine]}
  [config :- {}, machine :- IIndentedMachine]
  (mu/add (-indent machine) (:indent-size config)))

(defn prev-indent :- (Maybe Integer)
  "Returns previous indentation value based on the given `_config_`
  and current `_machine_`. Uses `:indent-size` key in `_config_`
  to obtain the indentation size."
  {:added v1
   :category "Indentation"
   :see '[next-indent base-indent IIndentedMachine]}
  [config :- {}, machine :- IIndentedMachine]
  (mu/subtract (-indent machine) (:indent-size config)))

(defn color? :- Boolean
  "Returns `true` if `_config_` map contains logical true `:color?`
  entry, `false` otherwise."
  [config :- {}]
  (boolean (:color? config)))

(def default-color :- AnyBatch
  "A default ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[0m"))

(def black :- AnyBatch
  "Black ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[30m"))

(def red :- AnyBatch
  "Red ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[31m"))

(def green :- AnyBatch
  "Green ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[32m"))

(def yellow :- AnyBatch
  "Yellow ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[33m"))

(def blue :- AnyBatch
  "Blue ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[34m"))

(def magenta :- AnyBatch
  "Magenta ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[35m"))

(def cyan :- AnyBatch
  "Cyan ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[36m"))

(def white :- AnyBatch
  "White ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[37m"))

(def bold-black :- AnyBatch
  "Bold black ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;30m"))

(def bold-red :- AnyBatch
  "Bold red ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;31m"))

(def bold-green :- AnyBatch
  "Bold green ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;32m"))

(def bold-yellow :- AnyBatch
  "Bold yellow ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;33m"))

(def bold-blue :- AnyBatch
  "Bold blue ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;34m"))

(def bold-magenta :- AnyBatch
  "Bold magenta ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;35m"))

(def bold-cyan :- AnyBatch
  "Bold cyan ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;36m"))

(def bold-white :- AnyBatch
  "Bold white ANSI color."
  {:added v1
   :category "Colorer"}
  (string-to-batch! "\033[1;37m"))

;; Recommended minimal set of keys:
;; :identifier, :string, :number, :literal,
;; :comment, :syntax, :invalid

;; Hierarchy used in Dunaj's built-in formatters
;; :identifier >> :keyword :map-key
;;                :symbol >> :host :name
;;                           :gvar >> [:core] >> :special-form :def
;;                                               :host :flow :warning
;; :string >> :char :regex
;; :number >> :float :decimal :rational
;;            :integer >> :octal :hexa :radix
;; :literal >> :nil :tagged
;;             :boolean >> :true :false
;; :comment >> :discard :block-comment :metadata
;; :syntax >> :map :set :list :vector
;;            :deref :function :limit :indent
;;            :quote >> :syntax-quote :var-quote
;;                      :unquote >> :unquote-splice
;; :invalid
;; :default

(def ^:private default-colorer-map :- KeywordMap
  "Default colorer map for ANSI compatible outputs."
  {:identifier magenta
   :string green
   :number red
   :literal yellow
   :comment cyan
   :syntax default-color
   :invalid bold-red
   :default default-color})

(defn default-colorer :- (Maybe AnyBatch)
  "Returns color for first found key, or returns `nil` if key(s)
  is/are not recognized."
  {:added v1
   :see '[custom-colorer]
   :category "Colorer"}
  [& keys :- Keyword]
  (some default-colorer-map keys))

(defn custom-colorer :- Function
  "Returns colorer which uses `_map_` to complement default
  colorer map."
  {:added v1
   :see '[default-colorer]
   :category "Colorer"}
  [map :- KeywordMap]
  (let [m (merge default-colorer-map map)]
    (fn [& keys] (some m keys))))

(defn color :- (Maybe AnyBatch)
  "Returns a batch which sets the color of the output according to
  the `_keys_`, or returns `nil` if keys don't map to any color.
  Uses function stored under `:color-fn` in `_config_` to perform
  the actual mapping.

  Function to be used may be overriden from the `_state_` reference
  by setting `:color-override` in the referenced map to the
  overriding fn. The overriding is used e.g. in printing metadata."
  {:added v1
   :see '[default-colorer custom-colorer]
   :category "Colorer"}
  [config :- {}, state :- IReference & keys :- Keyword]
  (apply (or (:color-override @state) (:color-fn config)) keys))

(deftype BSWrap
  [ret :- Any, step :- Any, prev :- Any, batch :- AnyBatch])

(defn bs-advance :- BSWrap
  [ret :- Any, step :- Any, prev :- Any, batch :- AnyBatch]
  (cond
    (reduced? ret) (reduced (->BSWrap @ret step prev batch))
    (postponed? ret)
    (postponed (->BSWrap @ret step prev batch)
               #(bs-advance (advance ret) step prev (clone batch))
               #(bs-advance (unsafe-advance! ret) step prev batch))
    :else (->BSWrap ret step prev batch)))

(defn ^:private provide-capacity :- AnyBatch
  "Returns new batch if `_batch_` is small,
  otherwise returns `_batch_`."
  [bm :- BatchManager, batch :- AnyBatch, size-hint :- Integer]
  (if (or (nil? batch) (i< (.capacity batch) (iint size-hint)))
    (.allocate bm (provide-batch-size size-hint))
    batch))

(deftype BatchedColorStripperReducing
  [r :- IReducing, bm :- BatchManager]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.format.printer.BSWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->BSWrap (._wrap r ret) 0 nil nil))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.format.printer.BSWrap wrap)))
  (-step [this wrap val]
    (let [bm (provide-batch-manager bm val)
          from :- AnyBatch val
          ret (.-ret ^dunaj.format.printer.BSWrap wrap)
          step (.-step ^dunaj.format.printer.BSWrap wrap)
          prev (.-prev ^dunaj.format.printer.BSWrap wrap)
          batch (.-batch ^dunaj.format.printer.BSWrap wrap)
          batch (provide-capacity bm batch (iinc (.remaining from)))]
      (.clear batch)
      (loop [step step
             prev prev]
        (if (.hasRemaining from)
          (let [v (.get bm from)
                x (iint v)]
            (cond (and (zero? step) (i== x (i27))) (recur 1 v)
                  (and (one? step) (i== x (iLBRACKET))) (recur 2 prev)
                  (and (== step 2) (i== x (iSMALL_M))) (recur 0 nil)
                  (one? step) (do (.put bm batch prev)
                                  (.put bm batch v)
                                  (recur 0 nil))
                  (zero? step) (do (.put bm batch v) (recur 0 nil))
                  :else (recur step prev)))
          (if (.hasRemaining (.flip batch))
            (bs-advance (._step r ret batch) step prev batch)
            (bs-advance ret step prev batch)))))))

(defxform batched-strip-color
  [machine-factory]
  ([r] (->BatchedColorStripperReducing
        r (batch-manager (-printer-to-type machine-factory))))
  :fold false
  :unpack false
  :count false
  :section false)

(defn perror :- nil
  "Throws printer engine error with a given messages `_ms_`."
  {:added v1
   :see '[invalid-item-handler]
   :category "Primary"}
  [& ms :- Any]
  (throw (illegal-state ^java.lang.String
                        (apply ->str "printer engine error: " ms))))

(defn ignore? :- Boolean
  "Returns `true` if `_config_` map contains `[:invalid-item :ignore]`
  entry, `false` otherwise."
  [config :- {}]
  (identical? :ignore (:invalid-item config)))

(defn throw? :- Boolean
  "Returns `true` if `_config_` map contains `[:invalid-item nil]`
  entry, `false` otherwise."
  [config :- {}]
  (nil? (:invalid-item config)))

(defn invalid-item-handler :- Any
  "Returns `nil` if `(:invalid-item _config_)` is set to `:ignore`,
  returns `(f _batch_ _bm_ _state_ _item_ _parents_)` if
  `(:invalid-item _config_)` is a function, otherwise throws.
  This helper function is used inside printer machine for
  default handling of invalid items."
  {:added v1
   :see '[perror]
   :category "Primary"}
  [batch :- AnyBatch, bm :- BatchManager, state :- IReference,
   config :- {}, item :- Any, parents :- IPersistentList]
  (cond (ignore? config) nil
        (throw? config) (perror "item not supported " (type item))
        :else ((:invalid-item config) batch bm state item parents)))

(defn ^:private alt-batch! :- AnyBatch
  "Returns alternative batch from a given `_state_` reference,
  creating it if not present."
  ([bm :- BatchManager, state :- IReference]
   (alt-batch! bm state 0))
  ([bm :- BatchManager, state :- IReference, initial-size :- Integer]
   (if-let [abatch (:alt-batch @state)]
     abatch
     (let [size (provide-batch-size
                 (max initial-size @default-formatter-batch-size))
           nbatch (.allocate bm size)]
       (reset! state (assoc @state :alt-batch nbatch))
       nbatch))))

(defn ^:private alt-batch-grow! :- AnyBatch
  "Sets `_batch_` as the new alternative batch in the `_state_`
  state reference. Returns `_batch_`."
  ([bm :- BatchManager, state :- IReference]
   (alt-batch-grow! bm state 0))
  ([bm :- BatchManager, state :- IReference, new-size :- Integer]
   (let [abatch ^java.nio.Buffer (:alt-batch @state)
         l (imax (iadd (.position abatch) (iint new-size))
                 (i<< (.capacity abatch) 1))
         nbatch (.allocate bm l)]
     (.flip abatch)
     (.copy bm abatch nbatch)
     (reset! state (assoc @state :alt-batch nbatch))
     nbatch)))

(defn print-single-batch-unchanged! :- (Maybe AnyBatch)
  "Returns nil and copies contents of `src` batch into
  `dest` batch, if `dest` has enough space, otherwise returns `src`."
  [dest :- AnyBatch, bm :- BatchManager, src :- AnyBatch]
  (if (< (.remaining dest) (.remaining src))
    src
    (do (.copy bm src dest) nil)))

(defn print-single-batch! :- (Maybe AnyBatch)
  "Returns nil and copies contents of cleared `src` batch into
  `dest` batch, if `dest` has enough space, otherwise returns `src`."
  [dest :- AnyBatch, bm :- BatchManager, src :- (Maybe AnyBatch)]
  (when-not (nil? src)
    (.clear src)
    (print-single-batch-unchanged! dest bm src)))

(defn print-single-element! :- (Maybe AnyBatch)
  "Returns nil and puts single element `el` into `dest` batch,
  if `dest` has enough space, otherwise returns alternative
  batch with given element printed."
  [dest :- AnyBatch, bm :- BatchManager,
   state :- IReference, el :- Any]
  (if (.hasRemaining dest)
    (do (.put bm dest el) nil)
    (let [abatch (alt-batch! bm state 1)]
      (.clear abatch) (.put bm abatch el) (.flip abatch))))

(defn print-element! :- AnyBatch
  "Returns batch with element `el` put into it. Batch `dest` is
  used if it contains enough space, otherwise alternative batch is
  used instead. If `dest` is already an alternative batch and has
  no space left, grows alternative batch so that element `el` fits
  into it."
  ([dest :- AnyBatch, bm :- BatchManager,
    state :- IReference, el :- Any]
   (if (.hasRemaining dest)
     (do (.put bm dest el) dest)
     (let [abatch (alt-batch! bm state)]
       (if (identical? dest abatch)
         (recur (alt-batch-grow! bm state) bm state el)
         (do (.clear abatch) (recur abatch bm state el))))))
  ([dest :- AnyBatch, bm :- BatchManager,
    state :- IReference, el :- Any, n :- Integer]
   (iloop [dest dest, i (iint n)]
     (if (izero? i)
       dest
       (recur (print-element! dest bm state el) (idec i))))))

(defn print-batch-unchanged! :- AnyBatch
  "Returns batch with contents of batch `src` copied into it. Batch
  `dest` is used if it contains enough space, otherwise alternative
  batch is used instead. If `dest` is already an alternative batch
  and has no space left, grows alternative batch so that all data
  from `src` fits into it."
  [dest :- AnyBatch, bm :- BatchManager,
   state :- IReference, src :- AnyBatch]
  (if (nil? src)
    dest
    (if-not (i< (.remaining dest) (.remaining src))
      (do (.copy bm src dest) dest)
      (let [size (.remaining src)
            abatch (alt-batch! bm state size)]
        (if (identical? dest abatch)
          (recur (alt-batch-grow! bm state size) bm state src)
          (do (.clear abatch) (recur abatch bm state src)))))))

(defn print-batch! :- AnyBatch
  "Returns batch with contents of cleared batch `src` copied into it.
  Batch `dest` is used if it contains enough space, otherwise
  alternative batch is used instead. If `dest` is already an
  alternative batch and has no space left, grows alternative batch
  so that all data from `src` fits into it."
  [dest :- AnyBatch, bm :- BatchManager,
   state :- IReference, src :- AnyBatch]
  (if (nil? src)
    dest
    (do (.clear src)
        (print-batch-unchanged! dest bm state src))))

(defn print-batch-escaped! :- AnyBatch
  "Returns batch with contents of cleared batch `src` copied into it.
  Batch `dest` is used if it contains enough space, otherwise
  alternative batch is used instead. If `dest` is already an
  alternative batch and has no space left, grows alternative batch
  so that all data from `src` fits into it. Uses escape-fn to escape
  items from `src` before copying them to the `dest`."
  [dest :- AnyBatch, bm :- BatchManager,
   state :- IReference, src :- AnyBatch, escape-fn :- Function]
  (.clear src)
  (let [lim (.limit src)
        pos (.position src)]
    (iloop [dest dest, from (.position src), i (.position src)]
      (if (i== lim i)
        ;; flush and finish
        (do (.position src from)
            (print-batch-unchanged! dest bm state src))
        (if-let [nbatch (escape-fn (.get bm src i))]
          ;; flush
          (do (.position src from)
              (.limit src i)
              (let [ndest
                    (-> dest
                        (print-batch-unchanged! bm state src)
                        (print-batch! bm state nbatch))]
                (.limit src lim)
                (recur ndest (iinc i) (iinc i))))
          ;; next char
          (recur dest from (iinc i)))))))

(defn print-finish! :- (Maybe AnyBatch)
  "Returns nil if `dest` is the same batch as `orig`,
  otherwise flips the `dest` and returns it."
  [dest :- AnyBatch, orig :- AnyBatch]
  (when-not (identical? dest orig) (.flip dest)))

(defmacro print!
  "Returns batch with stuff added, depending on the args passed into
  this macro. `_batch_` is used if it contains enough space,
  otherwise alternative batch shared through `_state_` is used.

  TODO: list possible inputs"
  {:added v1
   :see '[print-colored!]
   :category "Primary"}
  [batch bm state & [x :as body]]
  (if (and (single? body) (not (vector? x)) (not (list? x)))
    (cond (symbol? x)
          `(print-single-batch! ~batch ~bm ~x)
          (string? x)
          `(print-single-batch-unchanged!
            ~batch ~bm (string-to-batch! ~x))
          :else
          `(print-single-element! ~batch ~bm ~state ~x))
    (let [emit-print
          (fn [x]
            (cond (and (symbol? x) (:unchanged (meta x)))
                  `(print-batch-unchanged! ~bm ~state ~x)
                  (symbol? x) `(print-batch! ~bm ~state ~x)
                  (string? x) `(print-batch-unchanged!
                                ~bm ~state (string-to-batch! ~x))
                  ;; string with escape and cap
                  (and (vector? x) (< 2 (count x)))
                  `(print-batch-escaped!
                    ~bm ~state
                    (let [l# (or (:string-limit ~(second (rest x)))
                                 0)
                          s# ~(first x)
                          s# (if (and l# (pos? l#)
                                      (< l# (.length
                                             ^java.lang.String s#)))
                               (str (cap l# "..." s#))
                               s#)]
                      (string-to-batch! s#))
                    ~(second x))
                  ;; batch with escape-fn
                  (and (vector? x) (or (string? (first x))
                                       (list? (first x))
                                       (symbol? (first x))))
                  `(print-batch-escaped! ~bm ~state
                                         (string-to-batch! ~(first x))
                                         ~(second x))
                  ;; element repeated
                  (vector? x) `(print-element! ~bm ~state ~@x)
                  (or (list? x)
                      (class-instance? clojure.lang.ISeq x))
                  `(print-batch! ~bm ~state ~x)
                  :else `(print-element! ~bm ~state ~x)))
          res (loop [res [], body (seq body)]
                (if (nil? body)
                  res
                  (recur (conj res (emit-print (first body)))
                         (next body))))]
      `(-> ~batch ~@res (print-finish! ~batch)))))

(defmacro print-colored!
  "Returns batch with stuff added, depending on the args passed into
  this macro. `_batch_` is used if it contains enough space,
  otherwise alternative batch shared through `_state_` is used.

  Resets color after printing is done."
  {:added v1
   :see '[print!]
   :category "Primary"}
  [batch bm config state & [x :as body]]
  `(print! ~batch ~bm ~state ~@body
           (color ~config ~state :default)))

;;; Printer engine

(deftype PWrap [ret :- Any, state :- (Maybe IReference), first, tc,
                batch :- AnyBatch, other :- AnyBatch,
                leftover :- (Maybe AnyBatch)])

(deftype PrinterEngineReducing
  [r :- IReducing, machine-factory config bm :- BatchManager
   coll size-hint level-limit-pred level-limit-print-fn
   item-limit item-limit-batch]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [af (fn af [ret tc batch :- (Maybe AnyBatch)
                     other :- AnyBatch leftover after-done?]
               (cond
                (reduced? ret) ret
                (postponed? ret)
                (postponed
                 @ret
                 #(af (advance ret) (clone tc) (clone batch)
                      (clone other) (clone leftover) after-done?)
                 #(af (unsafe-advance! ret) tc batch
                      other leftover after-done?))
                :else
                (let []
                  (cond
                   (nil? batch) ret ;; reduced flag from underneath
                   (not (nil? leftover)) ;; flush
                   (do (.flip batch)
                       (if (.hasRemaining batch)
                         (recur (._step r ret batch) tc (.clear other)
                                batch leftover after-done?)
                         (recur (._step r ret leftover) tc
                                (.clear batch) other nil
                                after-done?)))
                   after-done?
                   (do (.flip batch)
                       (if (.hasRemaining batch)
                         (._step r ret batch)
                         ret))
                   :else
                   (recur ret tc batch other
                          (-print-after! tc bm batch (->lst tc))
                          true)))))
          w (strip-reduced wrap)
          ret (.-ret ^dunaj.format.printer.PWrap w)
          tc (.-tc ^dunaj.format.printer.PWrap w)
          batch :- AnyBatch (.-batch ^dunaj.format.printer.PWrap w)
          other :- AnyBatch (.-other ^dunaj.format.printer.PWrap w)
          leftover (.-leftover ^dunaj.format.printer.PWrap w)]
      (-> (af ret tc batch other leftover false)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (let [batch-size (provide-batch-size
                      (max (or size-hint 1)
                           @default-formatter-batch-size))
          batch (.allocate bm batch-size)
          other (.allocate bm batch-size)
          state (unsynchronized-reference (:initial-state config))
          tc (-top-container machine-factory config state coll)
          leftover (-print-before! tc bm batch (->lst tc))]
      (->PWrap (._wrap r ret) state true tc batch other leftover)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.format.printer.PWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.format.printer.PWrap wrap)
          state (.-state ^dunaj.format.printer.PWrap wrap)
          first? (.-first ^dunaj.format.printer.PWrap wrap)
          tc (.-tc ^dunaj.format.printer.PWrap wrap)
          batch (.-batch ^dunaj.format.printer.PWrap wrap)
          other (.-other ^dunaj.format.printer.PWrap wrap)
          leftover (.-leftover ^dunaj.format.printer.PWrap wrap)
          af (fn af [ret, step, state :- IReference, machines, s, sh,
                     batch :- AnyBatch, other :- AnyBatch,
                     leftover :- (Maybe AnyBatch)]
               (cond
                (reduced? ret)
                (reduced
                 (->PWrap @ret state false tc nil other leftover))
                (postponed? ret)
                (unsafe-postponed
                 (->PWrap @ret state false tc batch other leftover)
                 #(af (unsafe-advance! ret) step state machines
                      s sh batch other leftover))
                (not (nil? leftover)) ;; flush
                (do (.flip batch)
                    (if (.hasRemaining batch)
                      (recur
                       (._step r ret batch) step state machines
                       s sh (.clear other) batch leftover)
                      (recur
                       (._step r ret leftover) step state machines
                       s sh (.clear batch) other nil)))
                (and (not (nil? s)) (identical? :between step))
                (recur
                 ret :item state machines s sh batch other
                 (-print-between! (peek machines) bm batch machines))
                (identical? :before step)
                (recur
                 ret :item state machines s sh batch other
                 (-print-before! (peek machines) bm batch machines))
                (not (nil? s)) ;; next item
                (let [x (-dispatch-printer
                         machine-factory config state
                         (first s) bm batch machines)]
                  (if (container-printer-machine? x)
                    (if (and level-limit-pred
                             (level-limit-pred machines))
                      (recur ret :between state machines
                             (next s) sh batch other
                             (level-limit-print-fn batch bm state))
                      (let [machines (conj machines x)]
                        (recur ret :before state machines
                               (seq (cap item-limit [item-limit-batch]
                                         (-children x machines)))
                               (conj sh (next s)) batch other nil)))
                    (recur ret :between state machines
                           (next s) sh batch other x)))
                (empty? sh) ;; done
                (->PWrap ret state false tc batch other leftover)
                :else ;; go up
                (let [nmachines (pop machines)]
                  (recur ret :between state nmachines
                         (peek sh) (pop sh) batch other
                         (-print-after!
                          (peek machines) bm batch machines)))))]
      (af ret (if first? :dispatch :between) state (->lst tc)
          (->lst val) () batch other leftover))))

(defxform printer-engine*
  "A transducer that prints step values into batches, according to
  printer `machine-factory`."
  [machine-factory]
  ([r] (let [config (-printer-config machine-factory)
             type (-printer-to-type machine-factory)
             bm (batch-manager (select-item-type type))
             level-limit-pred (:level-limit-pred config)
             level-limit-print-fn (:level-limit-print-fn config)
             item-limit (or (:item-limit config) 0)
             item-limit-batch (:item-limit-batch config)]
         (->PrinterEngineReducing
          r machine-factory config bm nil nil level-limit-pred
          level-limit-print-fn item-limit item-limit-batch)))
  :count false
  :section false
  :unpack false
  :fold false)

(deftype PrinterEngine
  [machine-factory :- IPrinterMachineFactory, coll :- (Maybe IRed)
   pretty? :- Boolean]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* this reducef init))
  IHomogeneous
  (-item-type [this]
    (-printer-to-type machine-factory))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [config (-printer-config machine-factory)
          type (select-item-type
                requested-type (-printer-to-type machine-factory))
          bm (batch-manager (select-item-type type))
          level-limit-pred (:level-limit-pred config)
          level-limit-print-fn (:level-limit-print-fn config)
          item-limit (or (:item-limit config) 0)
          item-limit-batch (:item-limit-batch config)]
      (reduce-augmented*
       coll reduce*
       (->PrinterEngineReducing
        (if (and pretty? (color? config))
          (reducing reducef init)
          ((batched-strip-color machine-factory)
           (reducing reducef init)))
        machine-factory config bm coll size-hint level-limit-pred
        level-limit-print-fn item-limit item-limit-batch))))
  IConfig
  (-config [this] (-printer-config machine-factory)))

(defn printer-engine
  "Returns a transducer that prints step values according to printer
  `_machine-factory_`. Current implementation of printer engine
  requires that `_machine-factory_` prints to type which is supported
  by host batch manager.

  Strips any ANSI colors from the output. Use pretty-printer-engine
  if you want to pretty print."
  {:added v1
   :see '[pretty-printer-engine]
   :tsig (Fn [Transducer IPrinterMachineFactory]
             [IRed IPrinterMachineFactory []])
   :transducer true
   :category "Primary"}
  ([machine-factory]
   (comp (printer-engine* machine-factory) (mapcat identity)))
  ([machine-factory coll]
   (->PrinterEngine machine-factory coll false)))

(defn pretty-printer-engine
  "Returns a transducer that prints step values according to printer
  `_machine-factory_`. Current implementation of printer engine
  requires that `_machine-factory_` prints to type which is supported
  by host batch manager."
  {:added v1
   :see '[printer-engine]
   :tsig (Fn [Transducer IPrinterMachineFactory]
             [IRed IPrinterMachineFactory []])
   :transducer true
   :category "Primary"}
  ([machine-factory]
   (comp
    (printer-engine* machine-factory)
    (if (color? (-printer-config machine-factory))
      identity
      (batched-strip-color machine-factory))
    (mapcat identity)))
  ([machine-factory coll]
   (->PrinterEngine machine-factory coll true)))
