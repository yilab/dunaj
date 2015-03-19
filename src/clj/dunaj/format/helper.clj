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

(ns dunaj.format.helper
  "Helper fns for formatter implementations."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn]]
   [dunaj.host :refer
    [ArrayManager Batch BatchManager AnyBatch Array keyword->class]]
   [dunaj.host.int :refer [Int iadd i0 i<]]
   [dunaj.state :refer [IReference]]
   [dunaj.flow :refer [doto if let do]]
   [dunaj.function :refer [defn]]
   [dunaj.host.array :refer [array-manager]]
   [dunaj.host.batch :refer [batch-on]]
   [dunaj.char :refer [Char]]
   [dunaj.string :refer [String MutableString]]
   [dunaj.state.var :refer [def]]))


;;;; Implementation details

(def ^:private cha :- java.lang.reflect.Field
  "Makes internal String char array public."
  (doto (.getDeclaredField java.lang.String "value")
    (.setAccessible true)))

(defn ^:private get-cha :- (Array Char)
  "Returns char array from a string."
  [s :- String]
  (.get cha s))

(def ^:private cam :- ArrayManager
  (array-manager (keyword->class :char)))


;;;; Public API

(defn prepend-unread :- AnyBatch
  "Returns `_batch_` with prepended data from `_unread-batch_`,
  if any."
  {:added v1}
  [bm :- BatchManager, batch :- AnyBatch, unread-batch :- AnyBatch]
  (if (.hasRemaining unread-batch)
    (let [nbatch
          (.allocate
           bm (iadd (.remaining batch) (.remaining unread-batch)))]
      ;;(clojure.core/println "prepending" (.remaining unread-batch))
      (.copy bm unread-batch nbatch)
      (.copy bm batch nbatch)
      (.flip nbatch))
    batch))

(defn string-to-batch! :- (Batch Char)
  "Returns a batch with contents same as a given string `_s_`.
  If `_batch_` is given and has enough free space,
  puts string contents into it and returns `nil`.

  Implementation note: returned batch must have position 0 and limit
  set to buffers capacity."
  {:added v1}
  ([s :- String]
   (batch-on (get-cha s) (i0) (.length s)))
  ([s :- String, bm :- BatchManager, batch :- AnyBatch]
   (let [l (.length s)
         arr (get-cha s)]
     (if (i< (.remaining batch) l)
       (batch-on arr (i0) l)
       (do (.copyToBatch cam arr batch (i0) l) nil)))))

(def string-cat-batch!
  "Returns the catenation of mutable string `_ms_` with the
  [`_begin_`, `_end_`) section of a `_batch_`, using array in
  `_state_` reference as a intermediate array in cases where
  `_batch_` is not backed by one. `_state_` is assumed to be
  a reference to persistent map, optionally holding an array
  under `:alt-arr` key."
  {:added v1
   :tsig (Fn [MutableString MutableString (Batch Char)]
             [MutableString MutableString (Batch Char) IReference]
             [MutableString MutableString (Batch Char)
              Int Int IReference])}
  @#'dunaj.string/cat-batch!)
