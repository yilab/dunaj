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

(ns dunaj.format.html
  "A very basic Hiccup like HTML printer."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn AnyFn Maybe U I]]
   [dunaj.boolean :refer [Boolean and or not true? false?]]
   [dunaj.host :refer [Class class? set! keyword->class]]
   [dunaj.flow :refer [if when when-not cond do let loop recur case]]
   [dunaj.feature :refer [IMeta IPersistentMeta meta meta-ref]]
   [dunaj.poly :refer
    [defprotocol deftype extend-protocol! defrecord]]
   [dunaj.coll :refer
    [first rest seq second reduce single? reduced edit settle! conj!
     empty? slice count nth assoc! reverse get ISeq ISeqable dissoc
     sequential? conj assoc map? list? vector? set? double? seq?
     counted? provide-sequential next update pop peek]]
   [dunaj.function :refer [Function fn defn identity apply]]
   [dunaj.state.var :refer [Var var var? declare def]]
   [dunaj.identifier :refer [INamed name keyword? symbol? named?]]
   [dunaj.string :refer [->str empty-string str index-of string?]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.util :refer [merge into last some every?]]
   [dunaj.coll.default :refer [->lst vec set ->map]]
   [dunaj.coll.recipe :refer [map keep range concat remove interpose]]
   [dunaj.format :refer [IParserFactory IPrinterFactory parse print]]
   [dunaj.format.helper :refer [string-to-batch! string-cat-batch!]]
   [dunaj.format.printer :refer
    [IContainerPrinterMachine -printer-to-type cyan
     IPrinterMachineFactory printer-engine invalid-item-handler
     print-single-element! print-element! print-batch! print-finish!
     print-single-batch! print-batch-escaped! print!]]))


;;;; Implementation details

(deftype HtmlTopContainer
  "Top level printer container for HTML printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents] nil)
  (-print-after! [this bm batch parents] nil)
  (-print-between! [this bm batch parents] nil))

(deftype HtmlElementContainer
  "Element printer container for HTML printer."
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents]
    (let [c (rest coll)
          sc (first c)
          c (cond (map? sc) c
                  (keyword? sc) (cons {:class sc} (rest c))
                  (symbol? sc) (cons {:id sc} (rest c))
                  :else (cons {} c))]
      c))
  (-print-before! [this bm batch parents]
    (let [b (string-to-batch! (->str "<" (name (first coll))))]
      (print! batch bm state b)))
  (-print-after! [this bm batch parents]
    (let [b (string-to-batch! (->str "</" (name (first coll)) ">"))]
      (print! batch bm state b)))
  (-print-between! [this bm batch parents] nil))

(deftype HtmlAttributesContainer
  "Attributes printer container for HTML printer."
  {:predicate 'attribute-container?}
  [config state coll]
  IContainerPrinterMachine
  (-children [this parents] coll)
  (-print-before! [this bm batch parents]
    (when-not (empty? coll) (print! batch bm state \space)))
  (-print-after! [this bm batch parents]
    (print! batch bm state \>))
  (-print-between! [this bm batch parents]
    (print! batch bm state \space)))

(defprotocol IHtmlPrinter
  (-print-html!
    "Returns result or printing `this` as an HTML. Return value
    follows IPrinterMachineFactory/-dispatch-printer rules."
    [this config state bm batch parents]))

(extend-protocol! IHtmlPrinter
  java.lang.String
  (-print-html! [this config state bm batch parents]
    (print! batch bm state (string-to-batch! this)))
  nil
  (-print-html! [this config state bm batch parents]
    nil)
  clojure.lang.Sequential
  (-print-html! [this config state bm batch parents]
    (if (attribute-container? (first parents))
      (let [mname #(if (named? %) (name %) %)
            vals (map mname (provide-sequential (second this)))
            sv (str (interpose \space vals))
            s (->str (name (first this)) "=\"" sv "\"")]
        (print! batch bm state (string-to-batch! s)))
      (->HtmlElementContainer config state this)))
  java.lang.Object
  (-print-html! [this config state bm batch parents]
    (print! batch bm state (string-to-batch! (->str this))))
  clojure.lang.IPersistentMap
  (-print-html! [this config state bm batch parents]
    (->HtmlAttributesContainer config state this)))

(defrecord HtmlPrinterFactory
  "HTML Printer Factory record."
  []
  IPrinterMachineFactory
  (-printer-config [this]
    {})
  (-printer-from-type [this]
    (keyword->class :object))
  (-printer-to-type [this]
    (keyword->class :char))
  (-top-container [this config state coll]
    (->HtmlTopContainer config state coll))
  (-dispatch-printer [this config state item bm batch parents]
    (-print-html! item config state bm batch parents))
  IPrinterFactory
  (-print [this]
    (printer-engine this))
  (-print [this coll]
    (printer-engine this coll)))


;;;; Public API

(def html :- IPrinterFactory
  "A HTML printer factory. Currently there are no options."
  {:added v1}
  (->HtmlPrinterFactory))
