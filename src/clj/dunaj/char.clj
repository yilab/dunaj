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

(ns dunaj.char
  "Character type, predicates and operations.
  
  Dunaj uses host character type for `Char`.
  
  TIP: JVM host specific: Dunaj supports primitive chars as
  function arguments and return values (subject to the number of
  arguments and combination of argument types)."
  {:categories ["Primary" "Predicates" "Operations"]
   :authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [defn defalias v1]]
   [dunaj.type :refer [Maybe Any AnyFn Fn U Predicate]]
   [dunaj.boolean :refer [Boolean and or]]
   [dunaj.host.int :refer [iint i== i<= i32 iLF iCR]]
   [dunaj.math :refer [INumerical]]
   [dunaj.compare :refer [IComparable]]
   [dunaj.flow :refer [let]]
   [dunaj.poly :refer [deftype]]))


;;;; Public API

(deftype Char
  "A host character type.

  [NOTE]
  --
  JVM host specific: The Char type is a single 16-bit Unicode
  character.
  --"
  {:added v1
   :predicate 'char?
   :category "Primary"
   :see '[char unchecked-char dunaj.math/num dunaj.string/String
          dunaj.string/char-sequence?]}
  java.lang.Character
  INumerical
  (-numerical [this] (iint this))
  ;; following protocols are already implemented
  IComparable)

(defalias char
  {:added v1
   :tsig (Fn [Char Any])
   :category "Primary"
   :doc "Returns `_x_` coerced to `Char` type. Numeric inputs are
        subject to range checking."
   :see '[unchecked-char dunaj.math/num dunaj.host.array/char-array
          dunaj.host.array/chars dunaj.string/str
          dunaj.string/->str]})

(defalias unchecked-char
  {:added v1
   :tsig (Fn [Char Any])
   :category "Primary"
   :doc "Returns `_x_` coerced to `Char` type. Does not check numeric
        inputs for allowed ranges. Subject to rounding or
        truncation."
   :see '[char dunaj.math/num]})

(defn whitespace? :- Boolean
  "Returns `true` if `_c_` is an ASCII whitespace or control
  character, `false` otherwise."
  {:added v1
   :category "Predicates"
   :see '[dunaj.string/blank?]}
  [c :- Char]
  (i<= (iint c) (i32)))

(defn newline? :- Boolean
  "Returns `true` if `_c_` is an ASCII newline character,
  `false` otherwise."
  {:added v1
   :see '[whitespace?]
   :category "Predicates"}
  [c :- Char]
  (let [i (iint c)] (or (i== i (iLF)) (i== i (iCR)))))

(defn lower-case? :- Boolean
  "Returns `true` if `_c_` is in lower case, `false` otherwise."
  {:added v1
   :category "Predicates"
   :see '[upper-case?]}
  [c :- Char]
  (java.lang.Character/isLowerCase c))

(defn upper-case? :- Boolean
  "Returns `true` if `_c_` is in upper case, `false` otherwise."
  {:added v1
   :category "Predicates"
   :see '[lower-case?]}
  [c :- Char]
  (java.lang.Character/isUpperCase c))

(defn lower-case :- Char
  "Returns the lower case version of character `_c_`. Returns `_c_`
  if there is no lower case version."
  {:added v1
   :category "Operations"
   :see '[lower-case? dunaj.string/lower-case]}
  [c :- Char]
  (java.lang.Character/toLowerCase c))

(defn upper-case :- Char
  "Returns the upper case version of character `_c_`. Returns `_c_`
  if there is no upper case version."
  {:added v1
   :category "Operations"
   :see '[upper-case? dunaj.string/upper-case]}
  [c :- Char]
  (java.lang.Character/toUpperCase c))
