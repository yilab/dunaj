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

(ns dunaj.version
  "A type for representing software versions.

  Supports Clojure-like version scheme:
  `MAJOR.MINOR.INCREMENTAL-QUALIFIER-SNAPSHOT`"
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require [clojure.bootstrap :refer [v1]]
            [dunaj.type :refer [Maybe]]
            [dunaj.boolean :refer [Boolean or]]
            [dunaj.compare :refer [=]]
            [dunaj.flow :refer [when-let if when let]]
            [dunaj.poly :refer [defrecord]]
            [dunaj.coll :refer [IRed]]
            [dunaj.function :refer [defn]]
            [dunaj.string :refer [String ICanonical ->str]]
            [dunaj.identifier :refer [Symbol]]
            [dunaj.state.var :refer [defalias var]]
            [dunaj.uri :refer [Uri uri]]))


;;;; Public API

(defrecord Version
  "A record for software version."
  {:added v1
   :see '[version]}
  [major minor incremental qualifier interim]
  ICanonical
  (-canonical [this]
    (->str (or major 0) \. (or minor 0) \. (or incremental 0)
           (if qualifier (->str \- qualifier) "")
           (if interim "-SNAPSHOT" ""))))

(defn version :- Version
  "Returns version object for given canonical string `_s_`."
  {:added v1
   :see '[dunaj.string/canonical dunaj.env/current-version]}
  [s :- String]
  (let [[_ major minor incremental qualifier snapshot]
        (clojure.core/re-matches
         #"(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9_]+))?(?:-(SNAPSHOT))?"
         s)]
    (->Version
     (java.lang.Integer/valueOf ^java.lang.String major)
     (java.lang.Integer/valueOf ^java.lang.String minor)
     (java.lang.Integer/valueOf ^java.lang.String incremental)
     (if (= qualifier "SNAPSHOT") nil qualifier)
     (.contains s "SNAPSHOT"))))
