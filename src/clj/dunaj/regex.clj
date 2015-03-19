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

(ns dunaj.regex
  "Regular expressions."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Fn Any U Maybe]]
   [dunaj.boolean :refer [and or]]
   [dunaj.host :refer [class-instance?]]
   [dunaj.host.int :refer [Int iint iadd i0 isub izero? i==]]
   [dunaj.math :refer [Integer]]
   [dunaj.flow :refer [if do let loop recur cond]]
   [dunaj.poly :refer [deftype]]
   [dunaj.coll :refer [IRed reduced? postponed postponed?
                       unsafe-advance! count nth unsafe-postponed]]
   [dunaj.host.array :refer [adapt]]
   [dunaj.function :refer [fn defn]]
   [dunaj.string :refer
    [ICanonical ICharSequence String str provide-char-sequence]]
   [dunaj.error :refer [throw unsupported-operation]]
   [dunaj.state.var :refer [defalias]]
   [dunaj.format :refer [IParserFactory]]))


;;;; Implementation details

(deftype ^:private RegexParser
  [pattern :- java.util.regex.Pattern, coll :- ICharSequence]
  IRed
  (-reduce [this reducef init]
    (let [m (.matcher pattern coll)
          af (fn af [ret]
               (cond
                 (reduced? ret) @ret
                 (postponed? ret)
                 (unsafe-postponed @ret #(af (unsafe-advance! ret)))
                 (.find m)
                 (recur (reducef ret (clojure.core/re-groups m)))
                 :else ret))]
      (af init))))


;;;; Public API

(deftype Regex
  "A type for regular expressions. Supports `ICanonical` and
  can be used in `dunaj.format/parse`."
  {:added v1
   :see '[regex matches split dunaj.format/parse]
   :predicate 'regex?}
  java.util.regex.Pattern
  IParserFactory
  (-parse [this]
    (throw
     (unsupported-operation "transducers not supported by regex")))
  (-parse [this coll]
    (->RegexParser this (provide-char-sequence coll)))
  ICanonical
  (-canonical [this]
    (.toString this)))

(defalias regex
  "Returns an instance of host compiled regular expression pattern."
  {:added v1
   :see '[regex? matches split dunaj.format/parse]
   :tsig (Fn [Regex (U Regex String)])}
  clojure.core/re-pattern)

(defalias matches
  "Returns the match, if any, of string `_s_` to pattern `_re_`,
  using host matcher."
  {:added v1
   :see '[regex? regex split dunaj.format/parse]
   :tsig (Fn [Any java.util.regex.Pattern ICharSequence])}
  clojure.core/re-matches)

(defn split :- []
  "Returns collection of splitted `_coll_` according to regex `_re_`."
  {:added v1
   :see '[regex? regex matches dunaj.format/parse]}
  ([re :- (U String Regex), coll :- (Maybe IRed)]
     (split re coll 0))
  ([re :- (U String Regex), coll :- (Maybe IRed), limit :- Integer]
     (adapt
      (.split (regex re) (provide-char-sequence coll) (or limit 0)))))

(defn quote :- String
  "Returns literal quoted string for use in regex pattern."
  {:added v1
   :see '[quote-replacement regex]}
  [s :- String]
  (java.util.regex.Pattern/quote s))

(defn quote-replacement :- String
  "Returns literal quoted string for use in regex based replacements,
  e.g. in `dunaj.string/replace`."
  {:added v1
   :see '[quote regex dunaj.string/replace]}
  [s :- String]
  (java.util.regex.Matcher/quoteReplacement s))
