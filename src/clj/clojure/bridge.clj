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

(ns clojure.bridge
  "Bridging Dunaj with Clojure. Low level stuff, do not use."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require [clojure.core :refer [defn when-let def]]
            [dunaj.compare :refer [defsentinel]]
            [dunaj.coll :refer [-reduce postponed]]
            [dunaj.coll.helper :refer [strip-reduced]]))

(def ^:private red->seq* @#'dunaj.coll/red->seq*)

(defn red-to-seq
  "Returns nil or a non-empty seq from a given reducible coll."
  [coll]
  (when-let [r (-reduce coll #(postponed %2) nil)] (red->seq* r)))
