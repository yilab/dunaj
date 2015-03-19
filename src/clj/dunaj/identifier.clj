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

(ns dunaj.identifier
  "Symbolic identifiers and related protocols."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer
    [defalias defprotocol defn deftype replace-var! v1 scratch]]
   [dunaj.type :refer [Maybe Fn U]]
   [dunaj.boolean :refer [Boolean and]]
   [dunaj.host.int :refer [i== iCOLON i0 i1 iint]]
   [dunaj.compare :refer [IHash IComparable =]]
   [dunaj.flow :refer [if cond]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [extend-protocol!]]
   [dunaj.coll :refer [first slice]]
   [dunaj.function :refer [IInvocable]]
   [dunaj.string :refer [String ICanonical ReversedStringSection
                         StringSection str string?]]))


;;;; Implementation details

(defn ^:private strip-colon :- String
  [x :- String]
  (if (i== (iCOLON) (iint (.charAt x (i0)))) (.substring x (i1)) x))


;;;; Public API

(defprotocol INamed
  "A value protocol for the name string."
  {:added v1
   :see '[name INamespaced]
   :predicate 'named?
   :on-interface clojure.lang.Named
   :distinct true}
  (-name :- String
    "Returns the name string."
    {:on 'getName}
    [this]))

(defn name :- String
  "Returns the name string of object `_x_`."
  {:added v1
   :see '[named? namespace]}
  [x :- INamed]
  (-name x))

(defprotocol INamespaced
  "A value protocol for the namespace string."
  {:added v1
   :see '[namespace INamed]
   :predicate 'namespaced?
   :on-interface clojure.lang.Named
   :distinct true}
  (-namespace :- (Maybe String)
    "Returns the namespace string or `nil` if namespace is not set."
    {:on 'getNamespace}
    [this]))

(defn namespace :- (Maybe String)
  "Returns the name string of object `_x_` or `nil`
  if namespace is not set."
  {:added v1
   :see '[namespaced? name]}
  [x :- INamespaced]
  (-namespace x))

;;; Symbol type

(deftype Symbol
  "A type for symbols, symbolic identifiers."
  {:added v1
   :predicate 'symbol?
   :see '[symbol]}
  clojure.lang.Symbol
  ICanonical
  (-canonical [this] (.toString ^java.lang.Object this))
  ;; following protocols are already implemented
  IHash
  IInvocable
  IMeta
  IPersistentMeta
  INamed
  INamespaced
  IComparable)

(defn symbol :- Symbol
  "Returns a symbol from `_x_`, where `_x_` can be any named object.
  Two arg version accepts `_name_` string and an optional namespace
  string `_ns_`."
  {:added v1
   :see '[symbol?]}
  ([x :- INamed]
     (cond
      (string? x) (clojure.lang.Symbol/intern x)
      (symbol? x) x
      :else (clojure.lang.Symbol/intern (namespace x) (name x))))
  ([ns :- (Maybe String), name :- String]
     (clojure.lang.Symbol/intern ns name)))

(defalias special?
  "Returns `true` if symbol `_s_` names a special form,
  `false` otherwise."
  {:added v1
   :see '[symbol?]
   :tsig (Fn [Boolean Symbol])}
  clojure.core/special-symbol?)

;;; Keyword

(deftype Keyword
  "A type for keyword symbolic identifiers."
  {:added v1
   :see '[keyword]
   :predicate 'keyword?}
  clojure.lang.Keyword
  ICanonical
  (-canonical [this] (.toString ^java.lang.Object this))
  ;; following protocols are already implemented
  IHash
  IInvocable
  INamed
  INamespaced
  IComparable)

(defn keyword :- Keyword
  "Returns a keyword from `_x_`, where `_x_` can be any named object.
  Two arg version accepts `_name_` string and an optional namespace
  string `_ns_`."
  {:added v1
   :see '[keyword?]}
  ([x :- INamed]
     (cond
      (string? x)
      (clojure.lang.Keyword/intern ^java.lang.String (strip-colon x))
      (keyword? x) x
      (symbol? x) (clojure.lang.Keyword/intern ^clojure.lang.Symbol x)
      :else (clojure.lang.Keyword/intern (namespace x) (name x))))
  ([ns :- (Maybe String), name :- String]
     (clojure.lang.Keyword/intern ns name)))

;;;; Extend string types

(extend-protocol! INamed
  java.lang.String
  (-name [this] this)
  ReversedStringSection
  (-name [this] (.toString this))
  StringSection
  (-name [this] (.toString this)))

;;;; Clojure hooks

(replace-var! clojure.core/name)

(replace-var! clojure.core/namespace)
