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

(ns dunaj.poly
  "Polymorphism. Types icon:archive[], records icon:th-list[],
  protocols icon:plug[], multimethods, tag hierarchy.

  IMPORTANT: Deftype in dunaj creates map-like object similar to what
  defprotocol does, and no longer implicitly imports created class
  into the namespace (a somewhat controversial change from Clojure).

  NOTE: Documentation needs more work."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Multimethods"]}
  (:api bare)
  (:require
   [clojure.core :refer
    [methods prefers symbol throw get map? some cons remove ->>]]
   [clojure.bootstrap :refer [defalias def v1 type-map defn fn]]
   [dunaj.type :refer [Signature Required Fn Any U Va Macro]]
   [dunaj.boolean :refer [Boolean boolean or and]]
   [dunaj.host :refer [Class class class? class-instance? supers]]
   [dunaj.compare :refer [identical?]]
   [dunaj.state :refer [ICloneable]]
   [dunaj.flow :refer [if let when-let]]
   [dunaj.feature :refer [IConfig meta]]))


;;;; Public API

(def Protocol :- Signature
  "Type signature for protocols."
  {:added v1
   :see '[defprotocol protocol? satisfies?]
   :category "Primary"}
  {(Required :clojure.core/protocol) true})

(defn protocol? :- Boolean
  "Returns `true` if `_x_` is a protocol, otherwise returns `false`."
  {:added v1
   :see '[Protocol protocol? satisfies?]
   :category "Primary"}
  [x :- Any]
  (boolean (and (map? x) (get x :clojure.core/protocol false))))

(defalias defprotocol
  "Defines a var with newly created protocol as a root binding,
  defines polymorphic protocol functions, generates host interface
  and returns protocol var."
  {:added v1
   :see '[deftype dunaj.host/definterface protocol? satisfies?]
   :category "Primary"
   :indent :all
   :highlight :def
   :named true
   :tsig Macro}
  clojure.bootstrap/defprotocol)

(defalias satisfies?
  "Returns `true` if `_x_` satisfies the `_protocol_`."
  {:added v1
   :see '[extend! deftype extends?]
   :category "Primary"
   :tsig (Fn [Boolean Protocol Any])})

;;; Types and records

(def Type :- Signature
  "Type signature for types."
  {:added v1
   :see '[deftype type? type-instance?]
   :category "Primary"}
  {(Required :clojure.core/type) true})

(defn type? :- Boolean
  "Returns `true` if `_x_` is a type, otherwise returns `false`."
  {:added v1
   :see '[Type deftype type-instance?]
   :category "Primary"}
  [x :- Any]
  (boolean (and (map? x) (get x :clojure.core/type false))))

(defalias deftype
  "Generates a record, defines constructor fns and returns that type."
  {:added v1
   :see '[Type type? type-instance?]
   :category "Primary"
   :indent :all
   :highlight :def
   :named true
   :tsig Macro}
  clojure.bootstrap/deftype)

(defn type-instance? :- Boolean
  "Returns `true` if `_x_` is a type instance, otherwise returns
  `false`."
  {:added v1
   :see '[Type deftype instance? record-instance?
          dunaj.host/class-instance?]
   :category "Primary"}
  [x :- Any]
  (class-instance? clojure.lang.IType x))

(def Record :- Signature
  "Type signature for records."
  {:added v1
   :see '[record? defrecord record-instance?]
   :category "Primary"}
  {(Required :clojure.core/record) true})

(defn record? :- Boolean
  "Returns `true` if `_x_` is a record (a record type,
  not instance of a record), otherwise returns `false`."
  {:added v1
   :see '[Record defrecord record-instance? type?]
   :category "Primary"}
  [x :- Any]
  (boolean (and (map? x) (get x :clojure.core/record false))))

(defalias defrecord
  "Generates a record, defines constructor fns and returns that
  record type."
  {:added v1
   :see '[Record record? record-instance?]
   :category "Primary"
   :indent :all
   :highlight :def
   :named true
   :tsig Macro}
  clojure.bootstrap/defrecord)

(defn record-instance? :- Boolean
  "Returns `true` if `_x_` is a record instance,
  otherwise returns `false`."
  {:added v1
   :see '[Record record? defrecord instance? type-instance?
          dunaj.host/class-instance?]
   :category "Primary"}
  [x :- Any]
  (class-instance? clojure.lang.IRecord x))

(defn instance? :- Boolean
  "Returns `true` if `_x_` is an instance of `_type_`.
  Also accepts class instead of type."
  {:added v1
   :see '[dunaj.host/class-instance? record-instance? type-instance?]
   :category "Primary"
   :inline
   (fn [c x]
     `(clojure.core/let [c# ~c
                         c# (clojure.core/if (clojure.core/class? c#)
                              c#
                              (:on-class c#))]
        (clojure.lang.Util/isInstance ^java.lang.Class c# ~x)))}
  [type :- (U Class Type), x :- Any]
  (let [c (if (class? type) type (:on-class type))]
    (class-instance? ^java.lang.Class c x)))

(defn ensure-instance :- Any
  "Returns `_x_`, throwing if `_x_` is not an instance of `_type_`."
  {:added v1
   :see '[dunaj.host/ensure-class]
   :category "Primary"}
  [type :- (U Class Type), x :- Any]
  (if (instance? type x)
    x
    (throw (java.lang.IllegalArgumentException.
            "Instance is not of required type"))))

(defalias reify
  "reify is a macro with the following structure:

  `(reify options* specs*)`

  Currently there are no options.

  Each spec consists of the protocol or interface name followed by
  zero or more method bodies:

    protocol-or-interface-or-Object
    (methodName [args+] body)*

  Methods should be supplied for all methods of the desired
  protocol(s) and interface(s). You can also define overrides for
  methods of Object. Note that the first parameter must be supplied
  to correspond to the target object ('this' in Java parlance). Thus
  methods for interfaces will take one more argument than do the
  interface declarations.  Note also that recur calls to the method
  head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.

  The return type can be indicated by a type hint on the method name,
  and arg types can be indicated by a type hint on arg names. If you
  leave out all hints, reify will try to match on same name/arity
  method in the protocol(s)/interface(s) - this is preferred. If you
  supply any hints at all, no inference is done, so all hints (or
  default of Object) must be correct, for both arguments and return
  type. If a method is overloaded in a protocol/interface, multiple
  independent method definitions must be supplied.  If overloaded
  with same arity in an interface you must specify complete hints to
  disambiguate - a missing hint implies Object.

  recur works to method heads The method bodies of reify are lexical
  closures, and can refer to the surrounding local scope:

  [source,clojure]
  --
  (str (let [f \"foo\"]
       (reify Object
         (toString [this] f))))
  ;;=> \"foo\"

  (seq (let [f \"foo\"]
       (reify clojure.lang.Seqable
         (seq [this] (seq f)))))
  ;;=> (\\f \\o \\o))
  --

  reify always implements `clojure.lang.IObj` and transfers meta
  data of the form to the created object.

  [source,clojure]
  --
  (meta ^{:k :v} (reify Object (toString [this] \"foo\")))
  ;;=> {:k :v}
  --"
  {:added v1
   :see '[deftype]
   :category "Primary"
   :tsig Macro})

(defn type :- Any
  "Returns the `:type` metadata of `_x_`, or its type if none.
  Returns `nil` if type cannot be found."
  {:added v1
   :see '[dunaj.host/class]
   :category "Primary"}
  [x :- Any]
  (or (get (meta x) :type)
      (when-let [wr (get type-map (symbol (.getName (class x))))]
        (.get ^java.lang.ref.WeakReference wr))))

(defn identical-type? :- Boolean
  "Returns `true` if `x` and `y` are of identical type,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.compare/identical?]
   :category "Primary"}
  [x :- Any, y :- Any]
  (identical? (class x) (class y)))

;;; Extends

(defn extends? :- Boolean
  "Returns `true` if `type` extends `protocol`. Also accepts class
  instead of type."
  {:added v1
   :see '[extend! extend-type! extenders satisfies? extend!]
   :category "Primary"}
  [protocol :- Protocol, type :- (U Class Type)]
  (let [c (if (class? type) type (:on-class type))
        ef #(clojure.core/extends? protocol %)
        rf #(identical? java.lang.Object %)]
    (->> (supers c) (remove rf) (cons c) (some ef) boolean)))

(defalias extend!
  "Implementations of protocol methods can be provided using the
  extend construct:

  [source,clojure]
  --
  (extend! AType
    AProtocol
     {:foo an-existing-fn
      :bar (fn [a b] ...)
      :baz (fn ([a]...) ([a b] ...)...)}
    BProtocol
      {...}
    ...)
  --

  `extend!` takes a type/class (or interface, see below), and one or
  more protocol + method map pairs. It will extend the polymorphism
  of the protocol's methods to call the supplied methods when an
  AType is provided as the first argument.

  Method maps are maps of the keyword-ized method names to ordinary
  fns. This facilitates easy reuse of existing fns and fn maps, for
  code reuse/mixins without derivation or composition. You can extend
  an interface to a protocol. This is primarily to facilitate interop
  with the host (e.g. Java) but opens the door to incidental multiple
  inheritance of implementation since a class can inherit from more
  than one interface, both of which extend the protocol. It is TBD
  how to specify which impl to use. You can extend a protocol on nil.

  If you are supplying the definitions explicitly (i.e. not reusing
  existing functions or mixin maps), you may find it more convenient
  to use the `extend-type!` or `extend-protocol!` macros.

  Note that multiple independent extend clauses can exist for the
  same type, not all protocols need be defined in a single extend
  call."
  {:added v1
   :see '[extend-protocol! extend-type! extenders satisfies? extends?]
   :category "Primary"
   :indent 1
   :highlight :def
   :tsig (Fn [nil (U Class Type) (Va Any)])}
  clojure.core/extend)

(defalias extend-protocol!
  "Useful when you want to provide several implementations of the
  same protocol all at once. Takes a single protocol and the
  implementation of that protocol for one or more types.
  Expands into calls to `extend-type!`:

  [source,clojure]
  --
  (extend-protocol! Protocol
    AType
    (foo [x] ...)
    (bar [x y] ...)
    BType
    (foo [x] ...)
    (bar [x y] ...)
    AClass (foo [x] ...)
    (bar [x y] ...)
    nil
    (foo [x] ...) (bar [x y] ...))

  ;; expands into:

  (do
    (extend-type! AType Protocol (foo [x] ...) (bar [x y] ...))
    (extend-type! BType Protocol (foo [x] ...) (bar [x y] ...))
    (extend-type! AClass Protocol (foo [x] ...) (bar [x y] ...))
    (extend-type! nil Protocol (foo [x] ...) (bar [x y] ...)))
  --"
  {:added v1
   :see '[extend! extend-type! extenders satisfies? extends?]
   :category "Primary"
   :indent 1
   :highlight :def
   :tsig Macro}
  clojure.core/extend-protocol)

(defalias extend-type!
  "A macro that expands into an `extend!` call. Useful when you are
  supplying the definitions explicitly inline, `extend-type!`
  automatically creates the maps required by `extend!`.
  Propagates the class as a type hint on the first argument of all
  fns.

  [source,clojure]
  --
  (extend-type! MyType
    Countable
    (cnt [c] ...)
    Foo
    (bar [x y] ...)
    (baz ([x] ...) ([x y & zs] ...)))

  ;; expands into:

  (extend! MyType
    Countable {:cnt (fn [c] ...)}
    Foo {:baz (fn ([x] ...) ([x y & zs] ...)) :bar (fn [x y] ...)})
  --"
  {:added v1
   :see '[extend-protocol! extend! extenders satisfies? extends?]
   :category "Primary"
   :indent 1
   :highlight :def
   :tsig Macro}
  clojure.core/extend-type)

(defalias extenders
  "Returns a collection of the types/classes explicitly extending
  `_protocol_`."
  {:added v1
   :see '[extend-protocol! extend-type! extend! satisfies? extends?]
   :category "Primary"
   :tsig (Fn [[(U Type Class) Protocol]])})

;;; Multimethods

(deftype Multimethod
  "A type for multimethods. List of methods and preferreds is stored
  in the attached configuration map."
  {:added v1
   :category "Multimethods"
   :see '[defmulti defmethod]
   :predicate 'multimethod?}
  clojure.lang.MultiFn
  ICloneable
  (-clone [this] (throw (java.lang.UnsupportedOperationException.)))
  IConfig
  (-config [this] {:methods (methods this) :prefers (prefers this)}))

;; TODO: enhance signature to conform to the unified def signature
(defalias defmulti
  "Creates a new multimethod with the associated dispatch function.

  Options are key-value pairs and may be one of:

  * `:default` - The default dispatch value, defaults to `:default`
  * `:hierarchy` - The value used for hierarchical dispatch
    (e.g. ::square is-a ::shape)
  +
  Hierarchies are type-like relationships that do not depend upon
  type inheritance. By default multimethods dispatch off
  of a global hierarchy map. However, a hierarchy relationship can
  be created with the `derive!` function used to augment the root
  ancestor created with `make-hierarchy`.
  +
  Multimethods expect the value of the hierarchy option to be
  supplied as a reference type e.g. a var
  (i.e. via the Var-quote dispatch macro `#'` or the
  `dunaj.state.var/var` macro)."
  {:added v1
   :see '[Multimethod defmethod]
   :category "Multimethods"
   :indent :all
   :highlight :def
   :tsig Macro})

;; TODO: enhance signature to conform to the unified def signature
(defalias defmethod
  "Creates and installs a new method of multimethod `_multifn_`
  associated with `_dispatch-val_`."
  {:added v1
   :see '[Multimethod defmulti]
   :category "Multimethods"
   :indent 2
   :highlight :def
   :tsig Macro})

(defalias get-method
  "Given a multimethod `_multifn_` and a dispatch value
  `_dispatch-val_`, returns the dispatch fn that would apply to that
  value, or `nil` if none apply and there is no default method."
  {:added v1
   :see '[defmethod remove-method!]
   :category "Multimethods"
   :tsig (Fn [Multimethod Multimethod Any])}
  clojure.core/get-method)

(defalias prefer-method!
  "Causes the multimethod `_multifn_` to prefer matches of
  `_dispatch-val-x_` over `_dispatch-val-y_` when there is a
  conflict."
  {:added v1
   :see '[get-method remove-method! remove-all-methods!]
   :category "Multimethods"
   :tsig (Fn [Multimethod Multimethod Any Any])}
  clojure.core/prefer-method)

(defalias remove-all-methods!
  "Removes all of the methods of multimethod `_multifn_`."
  {:added v1
   :see '[get-method prefer-method! remove-method!]
   :category "Multimethods"
   :tsig (Fn [Multimethod Multimethod])}
  clojure.core/remove-all-methods)

(defalias remove-method!
  "Removes the method of multimethod `_multifn_` associated with
  `_dispatch-val_`."
  {:added v1
   :see '[get-method prefer-method! remove-all-methods!]
   :category "Multimethods"
   :tsig (Fn [Multimethod Multimethod Any])}
  clojure.core/remove-method)

;;; Inspect tag hierarchies

(defalias isa?
  "Returns true if `(= _child_ _parent_)`, or `_child_` is directly
  or indirectly derived from `_parent_`, either via host type
  inheritance relationship or a relationship established via
  `derive`. `_h_` must be a hierarchy obtained from `make-hierarchy`,
  if not supplied defaults to the global hierarchy."
  {:added v1
   :see '[derive! make-hierarchy parents]
   :category "Multimethods"
   :tsig (Fn [Boolean Any Any] [Boolean Any Any Any])})

(defalias parents
  "Returns the immediate parents of `_tag_`, either via host type
  inheritance relationship or a relationship established via
  `derive`. `_h_` must be a hierarchy obtained from `make-hierarchy`,
  if not supplied defaults to the global hierarchy."
  {:added v1
   :see '[isa? ancestors descendants]
   :category "Multimethods"
   :tsig (Fn [[Any] Any] [[Any] Any Any])})

(defalias ancestors
  "Returns the immediate and indirect parents of `_tag_`,
  either via host type inheritance relationship or a relationship
  established via `derive`. `_h_` must be a hierarchy obtained from
  `make-hierarchy`, if not supplied defaults to the global
  hierarchy."
  {:added v1
   :see '[isa? parents descendants]
   :category "Multimethods"
   :tsig (Fn [[Any] Any] [[Any] Any Any])})

(defalias descendants
  "Returns the immediate and indirect children of `_tag_`, through a
  relationship established via `derive`. `_h_` must be a hierarchy
  obtained from `make-hierarchy`, if not supplied defaults to the
  global hierarchy.

  IMPORTANT: JVM host specific: Does not work on Java type
  inheritance relationships."
  {:added v1
   :see '[isa? ancestors parents]
   :category "Multimethods"
   :tsig (Fn [[Any] Any] [[Any] Any Any])})

;;; Alter tag hierarches

(defalias make-hierarchy
  "Creates a hierarchy object for use with `derive!`, `isa?` etc."
  {:added v1
   :see '[derive! isa?]
   :category "Multimethods"
   :tsig (Fn [Any])})

(defalias derive!
  "Establishes a parent/child relationship between `_parent_` and
  `_tag_`. `_parent_` must be a namespace-qualified symbol or
  keyword and child `_tag_` can be either a namespace-qualified
  symbol or keyword or a class. `_h_` must be a hierarchy obtained
  from `make-hierarchy`, if not supplied defaults to, and modifies,
  the global hierarchy."
  {:added v1
   :see '[underive! make-hierarchy isa?]
   :category "Multimethods"
   :tsig (Fn [Any Any Any] [Any Any Any Any])}
  clojure.core/derive)

(defalias underive!
  "Removes a parent/child relationship between `_parent_` and `_tag_`.
  `_h_` must be a hierarchy obtained from `make-hierarchy`,
  if not supplied defaults to, and modifies, the global hierarchy."
  {:added v1
   :see '[derive! make-hierarchy isa?]
   :category "Multimethods"
   :tsig (Fn [Any Any Any] [Any Any Any Any])}
  clojure.core/underive)
