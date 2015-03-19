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

(ns dunaj.type
  "Type signatures. Provides basic facilities for declaring data
  types for vars, function arguments and return values.

  Type signatures can be used for any object which can have metadata,
  and are stored in object's metadata under `:tsig` key.
  In addition, type signatures can be used in
  binding/arg vectors of `let`, `fn` and `loop`.

  Special syntax sugar `:-` for type signatures is also available.
  If possible, host type hints are generated automatically from
  type signatures, so that user does not have to specify types twice.

  Following macros recognize type signatures, support sugared syntax
  and generate type hints:
  `def`, `defonce`, `defn`, `defnonce`, `deftype`, `defrecord` and
  `defprotocol` (in methods).

  Following macros recognize sugared syntax
  and generate type hints, but do not store type signatures in
  metadata: `fn`, `let`, `if-let`, `when-let`, `loop`, `iloop`.
  For these cases, type checking library must get type signatures
  from static analysis of source files.

  WARNING: dunaj.type API is a very early experiment with
           incomplete functionality. Expect rough edges.

  IMPORTANT: Dunaj does not include type checking facilities,
  as these are left to third party libraries, which can
  utilize and extend type signatures defined here.

  .Notes
  ****
  * `[x y z ...]` denotes any collection, items of which
    must satisfy type signatures x y z ... If no special modifiers
    (e.g. `required`) are used, items are optional and collection
    may even be empty or `nil`.
  * `{k1 v1, k2 v2, ...}` denotes collection transformable
    to the sequence of key-value pairs that satisfy type signatures
    k1 v1, k2 v2, ... If no special modifiers are used, keys are
    optional and collection may even be empty or `nil`. Values for
    optional key may also be `nil`, even if this is not indicated in
    the value signature.
  * `[]` is equivalent to `(Maybe dunaj.coll/IRed)`
  * `{}` is equivalent to `(Maybe dunaj.coll/IPersistentMap)`
  * `#{}` is equivalent to `(Maybe dunaj.coll/IPersistentSet)`
  ****"
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core :refer
    [when satisfies? throw when-not reduce empty? = first map or if
     second eval]]
   [clojure.bootstrap :refer
    [replace-var! deftype defprotocol defn def type-hint let
     defrecord v1 if-let primitive-type-hint common-type-hint]]))


;;;; Public API

(defprotocol IHintedSignature
  "A value protocol for type signatures that provide host type hints."
  {:added v1}
  (-type-hint ;; :- TypeHint ;; circular dependency
    "Returns host type hint for `_this_` type signature."
    [this]))

;; JVM HOST, BOOTSTRAPPING
(defn polymorphic-type-hint [sig]
  (when (satisfies? IHintedSignature sig) (-type-hint sig)))
(replace-var! clojure.bootstrap/polymorphic-type-hint)

(defrecord AnySignature
  "A record for type signature that represents any type."
  [])

(def Any :- AnySignature
  "A type signature representing any type."
  {:added v1}
  (->AnySignature))

(defrecord UnknownSignature
  "A record for type signature that represents any type."
  [])

(def Unknown :- UnknownSignature
  "A type signature representing unknown type."
  {:added v1}
  (->UnknownSignature))

(def Signature :- Unknown
  "A type signature for type signatures. :)"
  {:added v1}
  Any)

(def TypeHint :- Signature
  "A type signature for host type hints."
  {:added v1}
  clojure.lang.Symbol) ;; JVM HOST

(def Macro :- Signature
  "A placeholder indicating that var holds a macro and that the type
  signature is not missing. Is used mainly in defalias constructs."
  {:added v1}
  :macro)

(defrecord FnSignature
  "A record type for type signatures that represent functions.
  Type signatures for invoke methods are stored in a `_method-sigs_`
  field."
  [method-sigs :- [[Signature]]])

(def AnyFn :- Signature
  "Returns a type signature for a function with unspecified
  invoke method signatures."
  (->FnSignature nil))

(defn Fn ;; :- Signature ;; circular dependency
  "Returns a type signature for an invocable object,
  having `_msigs_` as type signatures for its invoke methods.

  IMPORTANT: Return types are first items in signature vectors!"
  {:added v1}
  [& msigs] ;; :- [[Signature]] ;; circular dependency
  (->FnSignature msigs))

(defrecord MaybeSignature
  "A record type for type signatures that represent either `nil` or
  satisfying type signature `_sig_`."
  [sig :- Signature]
  IHintedSignature
  (-type-hint [this] (type-hint sig)))

(defn Maybe :- Signature
  "Returns a type signature that represents either `nil` or object
  satisfying type signature `_sig_`."
  {:added v1}
  [sig :- Signature]
  (->MaybeSignature sig))

(defrecord HintedSignature
  "A record type for type signature that provides host type hint
  from `_hinting-sig_`, but is otherwise equivalent to `_sig_`."
  [sig :- Signature, hinting-sig :- Signature, on-class :- Signature]
  IHintedSignature
  (-type-hint [this] (type-hint hinting-sig)))

(defn Hinted :- Signature
  "Returns type signature equivalent to `_sig_` that returns
  type hint from `_hinting-sig_` as its host type hints."
  {:added v1}
  [sig :- Signature, hinting-sig :- Signature]
  (->HintedSignature sig hinting-sig (eval (type-hint hinting-sig))))

(defn NotHinted :- Signature
  "Returns type signature equivalent to `_sig_` that does not provide
  type hints.

  Assumes `nil` type signature will never have type hint."
  {:added v1}
  [sig :- Signature]
  (if (type-hint sig) (->HintedSignature sig nil nil) sig))

(defn NotPrimitive :- Signature
  "Returns type signature equivalent to `_sig_` that does not provide
  primitive type hints."
  {:added v1}
  [sig :- Signature]
  (if (type-hint sig) (->HintedSignature sig sig nil) sig))

(defrecord VariadicSignature
  "A record type for type signatures that represent homogeneous
  variable part of variadic function arguments. Type signature
  of items in the variable part is stored in a `_sig_` field."
  [sig :- Signature])

(defn Va :- Signature
  "Returns a type signature for homogeneous variable part
  of variadic function arguments, with `_sig_` representing
  type signature of its items."
  {:added v1}
  [sig :- Signature]
  (->VariadicSignature sig))

(def Predicate :- Signature
  "A type signature for one-argument predicate."
  {:added v1}
  (Fn [java.lang.Boolean Any]))  ;; JVM HOST

(def KeywordMap :- Signature
  "A type signature for map where keys are keywords."
  {:added v1}
  {clojure.lang.Keyword Any})

(defrecord UnionSignature
  "A record type for type signatures that represent union of
  signatures `_sigs_`."
  [sigs :- [Signature], on-class :- Signature]
  IHintedSignature
  (-type-hint [this] (second (common-type-hint sigs))))

(defn U :- Signature
  "Returns a type signature for union of signatures `_sigs_`."
  {:added v1}
  [& sigs :- Signature]
  (->UnionSignature
   sigs (eval (type-hint (first (common-type-hint sigs))))))

(defrecord IntersectionSignature
  "A record type for type signatures that represent intersection of
  signatures `_sigs_`."
  [sigs :- [Signature], on-class :- Signature]
  IHintedSignature
  (-type-hint [this] (second (common-type-hint sigs))))

(defn I :- Signature
  "Returns a type signature for intersection of signatures `_sigs_`."
  {:added v1}
  [& sigs :- Signature]
  (->IntersectionSignature
   sigs (eval (type-hint (first (common-type-hint sigs))))))

(defrecord RequiredSignature
  "A record type for type signatures that represent required keys or
  values, depending on the enclosing signature."
  [sig :- Signature, on-class :- Signature]
  IHintedSignature
  (-type-hint [this] (type-hint sig)))

(defn Required :- Signature
  "Returns required type signature equivalent to `_sig_`."
  {:added v1}
  [sig :- Signature]
  (->RequiredSignature sig (eval (type-hint sig))))

(defrecord RecordSignature
  "A record type for type signatures that represent records which
  implement given `_protocols_`."
  [protocols :- [Signature], on-class :- Signature]
  IHintedSignature
  (-type-hint [this] (second (common-type-hint protocols))))

(defn R :- Signature
  "Returns a type signature for record implementing `_protocols_`."
  {:added v1}
  [& protocols :- Signature]
  (->RecordSignature
   protocols (eval (type-hint (first (common-type-hint protocols))))))

;; TODO: NotNil, NotEmpty, Not
