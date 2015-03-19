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

(ns bare.core
  "An entry point to the `bare API`.

  IMPORTANT: Do not require this namespace directly.
  Idiomatic use is through the `:api` section of the
  `<<dunaj.lib.api.ad#ns,ns>>` macro.

  Bare API does not refer to any var, special form or host class.
  Is used if complete control of what is referred is needed."
  {:authors ["Jozef Wagner"]}
  (:api clojure :exclude [init-api]))


;;;; Public API

(defn init-api
  "Bare API. Nothing is refered, not even unqualified special forms."
  [references & args]
  ;; fully qualified special symbols only
  (alter-meta! *ns* assoc :qualified-specials true)
  nil)
