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

(ns dunaj.function.default
  "A helper namespace which provides default implementations for
  `dunaj.function` facilities.

  IMPORTANT: This is a helper namespace. It does not contain any
  public vars and there is *no need to require this namespace*
  directly."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [dunaj.type :refer [I Any Fn U Maybe Predicate]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [alter!]]
   [dunaj.flow :refer [retrying-delay if cond let if-let]]
   [dunaj.feature :refer [meta update-meta]]
   [dunaj.poly :refer [defrecord]]
   [dunaj.coll :refer [collection get contains? ICollectionFactory
                       IConvolutionFactory assoc hit update]]
   [dunaj.function :refer
    [IMemoizationFactory default-memoization-factory apply fn]]
   [dunaj.state.var :refer [reset-root! defalias def]]
   [dunaj.state.basic :refer [atom box]]
   [dunaj.coll.tuple :refer [pair key val]]
   [dunaj.coll.default :refer [map-factory]]
   [dunaj.coll.recipe :refer [map]]))


;;;; Public API

(defrecord BasicMemoizationFactory
  "Basic memoization factory with thread safety and pluggable cache."
  [cache-factory :- (I ICollectionFactory IConvolutionFactory),
   seed :- {Any Any}]
  IMemoizationFactory
  (-memoize [this f]
    ;; Inspired by clojure.core.memoize and clojure.core.cache.
    ;; Kudos to Michael Fogus & Meikel Brandmeyer.
    ;; See https://kotka.de/blog/2010/03/memoize_done_right.html
    (let [through #(if (contains? %1 %2)
                     (hit %1 %2)
                     (assoc %1 %2 (retrying-delay (apply f %2))))
          mf #(pair (key %) (box (val %)))
          tseed (collection map-factory (map mf seed))
          cache-ref (atom (collection cache-factory tseed))
          memoized (fn [& args]
                     (let [cache (alter! cache-ref through args)
                           val (get cache args)]
                       ;; val may get kicked out in the meantime
                       (if (nil? val) (apply f args) @val)))]
      (update-meta memoized assoc ::cache cache-ref))))

(reset-root! default-memoization-factory
             (->BasicMemoizationFactory map-factory nil))
