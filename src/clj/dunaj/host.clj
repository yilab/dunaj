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

(ns dunaj.host
  "Host interoperability.

  Arrays and batches have more functionalities defined in separate
  namespaces,
  <<dunaj.host.array.api.ad#dunaj.host.array,dunaj.host.array>> and
  <<dunaj.host.batch.api.ad#dunaj.host.batch,dunaj.host.batch>>.

  NOTE: Documentation needs more work."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer
    [do case if reify gensym symbol str = with-meta not nil? when or
     if-let cond]]
   [clojure.bootstrap :refer [defmacro deftype defalias defn def let
                              v1 primitive-type-hint defrecord]]
   [dunaj.type :refer
    [Macro Fn Any Maybe Signature TypeHint IHintedSignature]]
   [dunaj.boolean :refer [Boolean]]))


;;;; Public API

(defmacro new
  "Returns a new object constructed by calling constructor of the
  class named by `_classname_`, passing `_args_` evaluated from
  left to right into the constructor."
  {:added v1
   :see '[class-instance? dunaj.host/. dunaj.host/..]
   :highlight :host}
  [classname & args]
  `(clojure.core/new ~@args))

(defmacro set!
  "Assigns evaluated `_expr_` to the field identified by
  `_field-access-form_`. Returns `nil`.

  NOTE: Returning `nil` is a change from Clojure, as `set!` special
  form autoboxes returned value and it is preferred for mutating fns
  to return `nil` anyway."
  {:added v1
   :highlight :host}
  [field-access-form expr]
  `(do (clojure.core/set! ~field-access-form ~expr) nil))

(defmacro .
  "The '.' macro is the basis for access to host.
  It can be considered a member-access operator, and/or read
  as 'in the scope of'.

  If the first operand is a symbol that resolves to a class name,
  the access is considered to be to a static member of the named
  class. Note that nested classes are named
  `EnclosingClass$NestedClass`, if supported by host. Otherwise it
  is presumed to be an instance member and the first argument is
  evaluated to produce the target object.

  If the second operand is a symbol and no args are supplied it
  is taken to be a field access - the name of the field is the
  name of the symbol, and the value of the expression is the
  value of the field, unless there is no argument public method
  of the same name, in which case it resolves to a call to the
  method.

  If the second operand is a list, or args are supplied, it is
  taken to be a method call. The first item of the list must
  be a simple symbol, and the name of the method is the name of
  the symbol. The args, if any, are evaluated from left to right,
  and passed to the matching method, which is called, and its value
  returned. If the method has a void return type, the value of the
  expression will be nil. Note that placing the method name in a
  list with any args is optional in the canonic form, but can be
  useful to gather args in macros built upon the form."
  {:added v1
   :see '[dunaj.host/..]
   :highlight :host}
  [& args]
  `(clojure.core/. ~@args))

(defalias ..
  "Expands into a member access `(.)` of the first member on the
  first argument, followed by the next member on the result, etc."
  {:added v1
   :see '[dunaj.host/.]
   :tsig Macro
   :highlight :host}
  clojure.core/..)

(defalias proxy
  "Where

  * `class-and-interfaces` - a vector of class names
  * `args` - a (possibly empty) vector of arguments to the
             superclass constructor.
  * `f` - `(name [params*] body)` or
          `(name ([params*] body) ([params+] body) ...​)`

  Expands to code which creates a instance of a proxy class that
  implements the named class/interface(s) by calling the supplied
  fns. A single class, if provided, must be first. If not provided
  it defaults to Object.

  The interfaces names must be valid interface types. If a method
  fn is not provided for a class method, the superclass methd will
  be called. If a method fn is not provided for an interface method,
  an UnsupportedOperationException will be thrown should it be
  called. Method fns are closures and can capture the environment
  in which proxy is called. Each method fn takes an additional
  implicit first arg, which is bound to `this`. Note that while
  method fns can be provided to override protected methods, they
  have no other access to protected members, nor to super, as
  these capabilities cannot be proxied."
  {:added v1
   :tsig Macro
   :see '[proxy-super]
   :highlight :host
   :indent :all})

(defalias proxy-super
  "Use to call a superclass method in the body of a proxy method.
  Note that expansion captures `this`."
  {:added v1
   :see '[proxy]
   :tsig Macro})

;;; Class

(deftype Class
  "A host class type."
  {:added v1
   :see '[class class-instance?]
   :predicate 'class?}
  java.lang.Class)

(defalias class
  "Returns the Class of `_x_`."
  {:added v1
   :see '[Class class-instance? ensure-class-instance]
   :tsig (Fn [Class Any])
   :highlight :host})

(defalias class-instance?
  "Returns `true` is `_x_` is an instance of the class `_c_`,
  otherwise returns `true`."
  {:added v1
   :see '[class ensure-class-instance? dunaj.poly/instance?]
   :tsig (Fn [Boolean Class Any])
   :highlight :host}
  clojure.core/instance?)

(defalias ensure-class-instance
  "Returns `_x_`, throwing if `_x_` cannot be cast to class `_c_`."
  {:added v1
   :see '[class class-instance?]
   :tsig (Fn [Any Class Any])
   :highlight :host}
  clojure.core/cast)

(defn keyword->class* :- (Maybe Class)
  "Returns class from a given keyword `_k_` or `nil` if keyword is
  not recognized."
  [k :- (Maybe clojure.lang.Keyword)]
  (case k
    :byte java.lang.Byte/TYPE
    :int java.lang.Integer/TYPE
    :long java.lang.Long/TYPE
    :float java.lang.Float/TYPE
    :double java.lang.Double/TYPE
    :short java.lang.Short/TYPE
    :char java.lang.Character/TYPE
    :boolean java.lang.Boolean/TYPE
    :object java.lang.Object
    nil))

(defmacro keyword->class
  "Returns class from a given keyword `_k_` or `nil` if keyword is
  not recognized."
  {:added v1
   :see '[provide-class]
   :highlight :host}
  [k]
  (if (clojure.core/keyword? k)
    (keyword->class* k)
    `(keyword->class* ~k)))

(defn provide-class* :- (Maybe Class)
  "Returns class from a given keyword, type or class, or returns `nil`
  if `_x_` is not recognized."
  [x :- Any]
  (cond (clojure.core/keyword? x) (keyword->class* x)
        (clojure.core/class? x) x
        :else (:on-class x)))

(defmacro provide-class
  "Returns class from a given keyword, type or class, or returns `nil`
  if `_x_` is not recognized."
  {:added v1
   :see '[keyword->class]
   :highlight :host}
  [x]
  (if (clojure.core/keyword? x)
    (keyword->class* x)
    `(provide-class* ~x)))

(defalias bases
  "Returns the immediate superclass and direct interfaces of
  `_c_`, if any."
  {:added v1
   :see '[supers class-instance?]
   :tsig (Fn [[Class] Class])
   :highlight :host})

(defalias supers
  "Returns the immediate and indirect superclasses and interfaces
  of `_c_`, if any."
  {:added v1
   :arglists '([c])
   :see '[bases class-instance?]
   :tsig (Fn [[Class] Class])
   :highlight :host})

;;; Class generation

(defalias gen-class
  "When compiling, generates compiled bytecode for a class with the
  given package-qualified `:name` (which, as all names in these
  parameters, can be a string or symbol), and writes the .class
  file to the ompile-path directory. When not compiling, does
  nothing. The gen-class construct contains no implementation, as
  the implementation will be dynamically sought by the generated
  class in functions in an implementing Clojure namespace.
  Given a generated class `org.mydomain.MyClass` with a method
  named mymethod, gen-class will generate an implementation that
  looks for a function named by `(str prefix mymethod)`
  (default prefix: '-') in a Clojure namespace specified by
  `:impl-ns` (defaults to the current namespace). All inherited
  methods, generated methods, and init and main functions
  (see `:methods`, `:init`, and `:main` below) will be found
  similarly prefixed. By default, the static initializer for the
  generated class will attempt to load the Clojure support code for
  the class as a resource from the classpath, e.g. in the example
  case, `org/mydomain/MyClass__init.class`.
  This behavior can be controlled by `:load-impl-ns`

  Note that methods with a maximum of 18 parameters are supported.

  In all subsequent sections taking types, the primitive types can
  be referred to by their Java names (int, float etc), and classes
  in the java.lang package can be used without a package qualifier.
  All other classes must be fully qualified.

  Options should be a set of key/value pairs, all except for
  `:name` are optional:

  * `:name aname` - The package-qualified name of the class to be
    generated
  * `:extends aclass` - Specifies the superclass, the non-private
    methods of which will be overridden by the class. If not
    provided, defaults to Object.
  * `:implements [interface ...]` - One or more interfaces,
    the methods of which will be implemented by the class.
  * `:init name` - If supplied, names a function that will be
    called with the arguments to the constructor. Must return
    `[ [superclass-constructor-args] state]` If not supplied,
    the constructor args are passed directly to the superclass
    constructor and the state will be `nil`
  * `:constructors {[param-types] [super-param-types], ...}` -
    By default, constructors are created for the generated class
    which match the signature(s) of the constructors for the
    superclass. This parameter may be used to explicitly specify
    constructors, each entry providing a mapping from a constructor
    signature to a superclass constructor signature. When you supply
    this, you must supply an :init specifier.
  * `:post-init name` - If supplied, names a function that will be
    called with the object as the first argument, followed by the
    arguments to the constructor. It will be called every time an
    object of this class is created, immediately after all the
    inherited constructors have completed. Its return value is
    ignored.
  * `:methods [ [name [param-types] return-type], ...]` - The
    generated class automatically defines all of the non-private
    methods of its superclasses/interfaces. This parameter can be
    used to specify the signatures of additional methods of the
    generated class. Static methods can be specified with
    `^{:static true}` in the signature’s metadata.
    Do not repeat superclass/interface signatures here.
  * `:main boolean` - If supplied and true, a static public main
    function will be generated. It will pass each string of the
    `String[]` argument as a separate argument to a function called
    `(str prefix main)`.
  * `:factory name` - If supplied, a (set of) public static factory
    function(s) will be created with the given name, and the same
    signature(s) as the constructor(s).
  * `:state name` - If supplied, a public final instance field with
    the given name will be created. You must supply an `:init`
    function in order to provide a value for the state. Note that,
    though final, the state can be a ref or agent, supporting the
    creation of Java objects with transactional or asynchronous
    mutation semantics.
  * `:exposes {protected-field-name {:get name :set name}, ...}` -
    Since the implementations of the methods of the generated class
    occur in Clojure functions, they have no access to the inherited
    protected fields of the superclass. This parameter can be used
    to generate public getter/setter methods exposing the protected
    field(s) for use in the implementation.
  * `:exposes-methods {super-method-name exposed-name, ...}` - It is
    sometimes necessary to call the superclass' implementation of an
    overridden method. Those methods may be exposed and referred in
    the new method implementation by a local name.
  * `:prefix string` - Default: '-'. Methods called e.g. Foo will be
    looked up in vars called prefixFoo in the implementing ns.
  * `:impl-ns name` - Default: the name of the current ns.
    Implementations of methods will be looked up in this namespace.
  * `:load-impl-ns boolean` - Default: `true`. Causes the static
    initializer for the generated class to reference the load code
    for the implementing namespace. Should be `true` when
    implementing-ns is the default, `false` if you intend to load
    the code via some other method."
  {:added v1
   :see '[gen-interface dunaj.lib/ns]
   :tsig Macro
   :highlight :def
   :indent 0})

(defalias gen-interface
  "When compiling, generates compiled bytecode for an interface with
  the given package-qualified `:name` (which, as all names in these
  parameters, can be a string or symbol), and writes the .class
  file to the compile-path directory. When not compiling, does
  nothing.

  In all subsequent sections taking types, the primitive types can
  be referred to by their Java names (int, float etc), and classes
  in the java.lang package can be used without a package qualifier.
  All other classes must be fully qualified.

  Options should be a set of key/value pairs, all except for `:name`
  are optional:

  * `:name aname` - The package-qualified name of the class to be
    generated
  * `:extends [interface ...]` - One or more interfaces, which will
    be extended by this interface.
  * `:methods [ [name [param-types] return-type], ...]` - This
    parameter is used to specify the signatures of the methods of
    the generated interface. Do not repeat superinterface signatures
    here."
  {:added v1
   :see '[gen-class dunaj.lib/ns definterface]
   :tsig Macro
   :highlight :def
   :indent 0})

(defalias definterface
  "Creates a new Java interface with the given name and method sigs.
  The method return types and parameter types may be specified
  with type hints, defaulting to Object if omitted.

  [source,clojure]
  --
  (definterface MyInterface
    (^int method1 [x])
    (^Bar method2 [^Baz b ^Quux q]))
  --"
  {:added v1
   :see '[dunaj.poly/defprotocol gen-class gen-interface]
   :tsig Macro
   :highlight :def
   :indent :all
   :named true}
  clojure.core/definterface2)

;;; Misc

(defalias bean->map
  {:added v1
   :tsig (Fn [{clojure.lang.Keyword Any} Any])
   :highlight :host}
  clojure.core/bean)

;;; Batch manager

(definterface BatchManager
  (^java.lang.Class itemType [])
  (^java.nio.Buffer wrap [arr ^int offset ^int length])
  (^java.nio.Buffer allocate [^int size])
  (^java.nio.Buffer readOnly [^java.nio.Buffer buf])
  (get [^java.nio.Buffer buf])
  (get [^java.nio.Buffer buf ^int index])
  (^java.nio.Buffer put [^java.nio.Buffer buf val])
  (^java.nio.Buffer put [^java.nio.Buffer buf ^int index val])
  (^java.nio.Buffer copy [^java.nio.Buffer src ^java.nio.Buffer dst])
  (^java.nio.Buffer copy [^java.nio.Buffer src
                          arr ^int offset ^int length]))

(def AnyBatch :- Signature
  "A type signature representing batch."
  {:added v1
   :see '[Batch BatchManager dunaj.host.batch/batch-manager]}
  java.nio.Buffer) ;; JVM HOST

(def ^:private batch-sigs :- {Any Signature}
  ;; JVM HOST
  {'byte java.nio.ByteBuffer
   'short java.nio.ShortBuffer
   'int java.nio.IntBuffer
   'long java.nio.LongBuffer
   'float java.nio.FloatBuffer
   'double java.nio.DoubleBuffer
   'char java.nio.CharBuffer})

(defn Batch :- Signature
  "Returns a type signature for a host batch that has items
  satisfying type signature `_sig_`, or returns `AnyBatch` if
  signature is not supported by host.

  Assumes that host supports generic batches and that host batches
  are hinted through non-primitive type hints."
  {:added v1
   :see '[AnyBatch BatchManager dunaj.host.batch/batch-manager]}
  [sig :- Signature]
  (or (batch-sigs (primitive-type-hint sig)) AnyBatch))

(def BatchManager :- Signature
  "A type signature representing batch manager.

  Batch manager provides following host methods:

  * `(^java.lang.Class itemType [])` - returns item type
  * `(^java.nio.Buffer wrap [arr ^int offset ^int length])` -
    returns new batch which wraps given array section
  * `(^java.nio.Buffer allocate [^int size])` - returns new batch
    with given size
  * `(^java.nio.Buffer readOnly [^java.nio.Buffer buf])` - returns
    new batch which shares data but is read only
  * `(get [^java.nio.Buffer buf])` - returns item at current position
    and increases position
  * `(get [^java.nio.Buffer buf ^int index])` - returns item at
    given position
  * `(^java.nio.Buffer put [^java.nio.Buffer buf val])` - puts
    item and increases posision
  * `(^java.nio.Buffer put [^java.nio.Buffer buf ^int index val])` -
    puts item at given position
  * `(^java.nio.Buffer copy [^java.nio.Buffer src ^java.nio.Buffer
    dst])` - copy contents from src batch to dst batch
  * `(^java.nio.Buffer copy [^java.nio.Buffer src arr ^int offset
    ^int length])` - copy content from src batch to arr array."
  {:added v1
   :see '[Batch AnyBatch dunaj.host.batch/batch-manager]}
  dunaj.host.BatchManager)

(defmacro ^:private mk-bm [ts bts ets]
  (let [gbuf (gensym)
        tgbuf (with-meta gbuf {:tag bts})
        gdst (gensym)
        tgdst (with-meta gdst {:tag bts})
        gval (gensym)
        tgval (with-meta gval {:tag ts})
        garr (gensym)
        tgarr (with-meta garr {:tag (symbol (str ts "s"))})]
    `(reify dunaj.host.BatchManager
       (itemType [_] ~ets)
       (wrap [_ ~garr offset# length#]
         (clojure.core/. ~bts ~'wrap ~tgarr offset# length#))
       (allocate [_ size#] (clojure.core/. ~bts ~'allocate size#))
       (readOnly [_ ~gbuf] (.asReadOnlyBuffer ~tgbuf))
       (get [_ ~gbuf] (.get ~tgbuf))
       (get [_ ~gbuf index#] (.get ~tgbuf index#))
       (put [_ ~gbuf ~gval] (.put ~tgbuf ~tgval))
       (put [_ ~gbuf index# ~gval] (.put ~tgbuf index# ~tgval))
       (copy [_ ~gbuf ~gdst] (.put ~tgdst ~tgbuf))
       (copy [_ ~gbuf ~garr offset# length#]
         (.get ~tgbuf ~tgarr offset# length#)))))

(def ^:private bms :- {Class BatchManager}
  {java.lang.Byte/TYPE
   (mk-bm byte java.nio.ByteBuffer java.lang.Byte/TYPE)
   java.lang.Character/TYPE
   (mk-bm char java.nio.CharBuffer java.lang.Character/TYPE)
   java.lang.Integer/TYPE
   (mk-bm int java.nio.IntBuffer java.lang.Integer/TYPE)
   java.lang.Long/TYPE
   (mk-bm long java.nio.LongBuffer java.lang.Long/TYPE)
   java.lang.Float/TYPE
   (mk-bm float java.nio.FloatBuffer java.lang.Float/TYPE)
   java.lang.Double/TYPE
   (mk-bm double java.nio.DoubleBuffer java.lang.Double/TYPE)
   java.lang.Short/TYPE
   (mk-bm short java.nio.ShortBuffer java.lang.Short/TYPE)})

;;; Array manager

(definterface ArrayManager
  (^java.lang.Class itemType [])
  (allocate [^int size])
  (duplicate [arr])
  (duplicate [arr ^int begin ^int end])
  (^int count [arr])
  (get [arr ^int index])
  (set [arr ^int index val])
  (copyToBatch [arr ^java.nio.Buffer buf ^int offset ^int length])
  (sort [arr]))

(defrecord ArraySignature
  "A record type for type signatures that represent host arrays.
  Type signature of items in the array is stored in a `_sig_` field.
  Host type hint for array is stored in a `_type-hint_` field.

  NOTE: Low level."
  {:see '[dunaj.type/Signature]}
  [sig :- Signature, type-hint :- TypeHint]
  IHintedSignature
  (-type-hint [this] type-hint))

(def ^:private any-array-hint :- TypeHint
  'objects) ;; JVM HOST

(def ^:private array-hints :- {Any TypeHint}
  ;; JVM HOST
  {'byte 'bytes
   'short 'shorts
   'int 'ints
   'long 'longs
   'float 'floats
   'double 'doubles
   'boolean 'booleans
   'char 'chars})

(def AnyArray :- ArraySignature
  "A type signature for heterogeneous host arrays or host arrays
  with unspecified item type."
  {:added v1
   :see '[Array ArrayManager dunaj.host.array/array-manager]}
  (->ArraySignature Any any-array-hint))

(defn Array :- ArraySignature
  "Returns a type signature for a host array that has items
  satisfying type signature `_sig_`, or returns AnyArray.

  Assumes that host supports arrays with unspecified type
  and that host arrays are hinted through non-primitive type hints."
  {:added v1
   :see '[AnyArray ArrayManager dunaj.host.array/array-manager]}
  [sig :- Signature]
  (if-let [ph (array-hints (primitive-type-hint sig))]
    (->ArraySignature sig ph)
    (->ArraySignature sig any-array-hint)))

(def ArrayManager :- Signature
  "A type signature representing array manager.

  Array manager provides following host methods:

  * `(^java.lang.Class itemType [])` - returns item type
  * `(allocate [^int size])` - returns new array of given size
  * `(duplicate [arr])` - returns new array with content copied
    from arr
  * `(duplicate [arr ^int begin ^int end])` - returns new array
    with content copied from array section arr
  * `(^int count [arr])` - returns number of items in the array
  * `(get [arr ^int index])` - returns item at given position
  * `(set [arr ^int index val])` - sets item at given position
  * `(copyToBatch [arr ^java.nio.Buffer buf ^int offset ^int
    length])` - copies contents from arr into batch buf
  * `(sort [arr])` - sorts the array in place with natural ordering."
  {:added v1
   :see '[Array AnyArray dunaj.host.array/array-manager]}
  dunaj.host.ArrayManager)

(defmacro ^:private mk-am [ts ets bts]
  (let [garr (gensym)
        tgarr (with-meta garr {:tag (symbol (str ts "s"))})
        gval (gensym)
        tgval (if (= ts 'object) gval (with-meta gval {:tag ts}))
        buf? (not (nil? bts))
        gbuf (gensym)
        tgbuf (when buf? (with-meta gbuf {:tag bts}))]
    `(reify dunaj.host.ArrayManager
       (itemType [_] ~ets)
       (allocate [_ size#] (~(symbol "clojure.core"
                                     (str ts "-array")) size#))
       (duplicate [_ ~garr] (clojure.core/aclone ~tgarr))
       (duplicate [_ ~garr begin# end#]
         (java.util.Arrays/copyOfRange ~tgarr begin# end#))
       (count [_ ~garr] (clojure.core/alength ~tgarr))
       (get [_ ~garr index#] (clojure.core/aget ~tgarr index#))
       (set [_ ~garr index# ~gval]
         (clojure.core/aset ~tgarr index# ~tgval))
       ~@(when buf?
           `[(copyToBatch [_ ~garr ~gbuf offset# length#]
                          (.put ~tgbuf ~tgarr offset# length#))])
       (sort [_ ~garr]
         (java.util.Arrays/sort ~tgarr)))))

(def ^:private ams :- {Class ArrayManager}
  {java.lang.Byte/TYPE
   (mk-am byte java.lang.Byte/TYPE java.nio.ByteBuffer)
   java.lang.Character/TYPE
   (mk-am char java.lang.Character/TYPE java.nio.CharBuffer)
   java.lang.Integer/TYPE
   (mk-am int java.lang.Integer/TYPE java.nio.IntBuffer)
   java.lang.Long/TYPE
   (mk-am long java.lang.Long/TYPE java.nio.LongBuffer)
   java.lang.Float/TYPE
   (mk-am float java.lang.Float/TYPE java.nio.FloatBuffer)
   java.lang.Double/TYPE
   (mk-am double java.lang.Double/TYPE java.nio.DoubleBuffer)
   java.lang.Short/TYPE
   (mk-am short java.lang.Short/TYPE java.nio.ShortBuffer)
   java.lang.Object (mk-am object java.lang.Object nil)})
