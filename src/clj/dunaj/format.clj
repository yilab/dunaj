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

(ns dunaj.format
  "Generic formatter (parser/printer).

  Not limited to parsing from/printing to strings, but designed to
  work with any type of data, even for binary decoding/encoding.

  Dunaj provides following built-in formatters:

  * `utf-8`/`utf-16`/`charset-formatter` - parsers and printers
  * `clj`/`edn`/`json` - parsers and printers
  * `lazy-clj`/`lazy-edn`/`lazy-json` - lazy parsers
  * `pretty-clj`/`pretty-edn`/`pretty-json` - pretty printers
  * string - a printf like printer
  * regular expression - parsing regexes
  * `html` - basic html printer"
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Fn Any Maybe Va I U Predicate]]
   [dunaj.boolean :refer [or]]
   [dunaj.math :refer [Integer]]
   [dunaj.compare :refer [sentinel identical? nil?]]
   [dunaj.flow :refer [let if when]]
   [dunaj.poly :refer [defprotocol extend-protocol!]]
   [dunaj.coll :refer [IRed Transducer reduce reduced red?
                       provide-sequential provide-collection]]
   [dunaj.function :refer [defn]]
   [dunaj.host.array :refer [to-array]]
   [dunaj.error :refer [throw illegal-argument unsupported-operation]]
   [dunaj.state.var :refer [Var var def]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.tuple :refer [tuple]]))


;;;; Implementation details

(defn ^:private ensure-one :- Any
  "Returns first item of `_coll_`,
  throws if coll contains more items."
  [coll :- IRed]
  (let [sen (sentinel)
        rfn #(if (identical? sen %1) %2 (reduced sen))
        val (reduce rfn sen coll)]
    (when (identical? sen val)
      (throw (illegal-argument
              "Collection must contain only one item.")))
    val))

(def ^:dynamic ^:private *default-formatter-batch-size* :- Integer
  "Default batch size for formatters."
  32)


;;;; Public API

(def default-formatter-batch-size :- Var
  "A dynamic var holding default formatter batch size."
  {:added v1}
  (var *default-formatter-batch-size*))

(defprotocol IParserFactory
  "A factory protocol for parsers."
  {:added v1
   :see '[parse parse-whole]
   :predicate 'parser?}
  (-parse
    "Returns a transducer which parses step items or returns recipe of
    parsed items if `_coll_` is supplied."
    {:tsig (Fn [Transducer IParserFactory]
               [IRed IParserFactory []])}
    [this]
    [this coll]))

(defprotocol IPrinterFactory
  "A factory protocol for printers."
  {:added v1
   :see '[print print-one]
   :predicate 'printer?}
  (-print
    "Returns a transducer which prints step items items or returns
    recipe of printed items if `_coll_` is supplied."
    {:tsig (Fn [Transducer IPrinterFactory]
               [IRed IPrinterFactory []])}
    [this]
    [this coll]))

;; printf like printer
(extend-protocol! IPrinterFactory
  java.lang.String
  (-print [this]
    (throw (unsupported-operation
            "transducers not supported by formatter")))
  (-print [this coll]
    (java.lang.String/format
     this (to-array (provide-sequential coll)))))

(defn parse
  "Returns a transducer that parses step items,
  with `parser` used for parsing. Returns recipe if `_coll_` is
  given."
  {:added v1
   :tsig (Fn [Transducer IParserFactory]
             [IRed IParserFactory []])
   :see '[parser? parse-whole]
   :transducer true}
  ([parser] (-parse parser))
  ([parser coll] (-parse parser coll)))

(defn parse-whole :- Any
  "Returns one item parsed from `_coll_`, with `_parser_` used for
  parsing. Entire collection must parse into one result item.
  Throws if none or multiple objects were parsed."
  {:added v1
   :see '[parser? parse]}
  [parser :- IParserFactory, coll :- IRed]
  (ensure-one (-parse parser coll)))

(defn print
  "Returns a transducer which prints step items items or returns
  recipe of printed items if `_coll_` is supplied."
  {:added v1
   :transducer true
   :see '[printer? print-one]
   :tsig (Fn [Transducer IPrinterFactory]
             [IRed IPrinterFactory []]
             [IRed IPrinterFactory Any (Va Any)])}
  ([printer] (-print printer))
  ([printer coll] (-print printer (provide-collection coll)))
  ([printer item & items] (-print printer (cons item items))))

(defn print-one :- IRed
  "Returns a reducible collection of one printed `_item_`,
  with `_printer_` used for printing."
  {:added v1
   :see '[printer? print]}
  [printer :- IPrinterFactory, item :- Any]
  (-print printer (tuple item)))
