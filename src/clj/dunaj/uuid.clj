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

(ns dunaj.uuid
  "RFC 4122 UUID type.

  Implementation is provided for UUID variant `1 0`, versions 3
  and 4. UUIDs can also be created from their canonical string
  representation or from integer(s).

  A data reader literal `#uuid` is available for
  a convenient UUID creation."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core :refer [assoc]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [U Fn Any]]
   [dunaj.math :refer [INumerical num Integer]]
   [dunaj.compare :refer [IComparable]]
   [dunaj.host.number :refer [biginteger]]
   [dunaj.flow :refer [let if cond]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [deftype]]
   [dunaj.function :refer [defn]]
   [dunaj.host.array :refer [byte-array? byte-array]]
   [dunaj.string :refer
    [ICharSequence char-sequence? ->str ICanonical canonical]]
   [dunaj.state.var :refer [alter-root! var replace-var!]]))


;;;; Public API

(deftype Uuid
  "A type for UUIDs."
  {:added v1
   :see '[uuid]
   :predicate 'uuid?}
  java.util.UUID
  IComparable
  ICanonical
  (-canonical [this] (.toString this))
  INumerical
  (-numerical [this]
    (let [msb (.getMostSignificantBits this)
          lsb (.getLeastSignificantBits this)
          bb (java.nio.ByteBuffer/allocate 16)]
      (.putLong bb msb)
      (.putLong bb lsb)
      (java.math.BigInteger. (.array bb)))))

(defn uuid :- Uuid
  "Returns UUID from its canonical string representation,
  from one integer or from two at most 64 bits wide integers."
  {:added v1
   :see '[uuid? random from-bytes]}
  ([val :- (U Uuid ICharSequence Integer)]
     (cond (uuid? val) val
           (char-sequence? val)
           (java.util.UUID/fromString (->str val))
           :else
           (let [b (biginteger val)]
             (uuid (.longValue (.shiftRight b 64)) (.longValue b)))))
  ([high :- Integer, low :- Integer] (java.util.UUID. high low)))

;;; Reader literal

(alter-root! #'clojure.core/default-data-readers
             assoc 'uuid (var uuid))

;;; Variant 1 0

(defn random :- Uuid
  "Returns UUID of variant `1 0` and version 4."
  {:added v1
   :see '[from-bytes uuid]}
  []
  (java.util.UUID/randomUUID))

(defn from-bytes :- Uuid
  "Returns UUID of variant `1 0` and version 3 from `_byte-array_`,
  which may be a host byte array or a collection of bytes."
  {:added v1
   :see '[random uuid]}
  [bytes :- Any]
  (let [ba (if (byte-array? bytes) bytes (byte-array bytes))]
    (java.util.UUID/nameUUIDFromBytes ba)))
