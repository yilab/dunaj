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

(ns dunaj.coll.helper
  "Collection helpers for the implementation of new collection types.

  The best documentation for this namespace is to read `dunaj.coll.*`
  sources, as vars defined in this namespace are all heavily used
  there."
  {:authors ["Jozef Wagner"]
   :categories
   ["Primary" "Reducers" "Folds"
    ["Interop"
     "Functions that ease implementing host interoperability for
     custom collection types."]
    ["Adapters"
     "Adapters are used to limit, in runtime, the functionality of a
     collection (usually a collection recipe), so that it mimics
     characteristics of original collection. That way Dunaj can
     provide more features for processed collections apart from
     `IRed`."]]}
  (:api bare)
  (:require
   [clojure.core :refer
    [throw str gensym for keyword? list symbol map? conj count first
     take-while drop-while vector? cons hash-map second contains?
     list? concat]]
   [clojure.bootstrap :refer
    [defmacro v1 def not-implemented strip-sigs-vec get-sigs-vec]]
   [dunaj.type :refer [Any AnyFn Fn Maybe U]]
   [dunaj.boolean :refer [Boolean and or not false? true? boolean]]
   [dunaj.host :refer [Class provide-class class-instance?]]
   [dunaj.host.int :refer [Int iinc iadd imul i0 i-1 i1 i31 iloop i2
                           imax idiv i== iint i<= izero? ione? i< i>]]
   [dunaj.math :refer [Integer == neg? > zero?]]
   [dunaj.compare :refer [nil? = identical? compare]]
   [dunaj.flow :refer [if do let cond recur when when-not loop]]
   [dunaj.feature :refer [assoc-meta]]
   [dunaj.poly :refer
    [Type extend-protocol! defprotocol deftype satisfies?]]
   [dunaj.coll :refer
    [Reduced IHomogeneous IRed ICounted ISectionable IBatchedRed
     IUnpackedRed reduced? reduced red? section rest -section
     sequential? -reduce-batched -reduce reduce -reduce-unpacked
     sectionable? seq counted? item-type postponed IReducing
     postponed? advance unsafe-advance!]]
   [dunaj.function :refer [defn fn apply comp identity]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable -fold fold fork join invoke ForkJoinPool folding]]))


;;;; Implementation details

(defn ^:private unpacked-fn [f] (fn [val xs] (apply f val xs)))

;; injected in dunaj.coll.recipe
(defn mapcat
  ([f] (not-implemented))
  ([f coll] (not-implemented)))

;; injected in dunaj.host.batch
(defn batch
  ([requested-type size-hint] (not-implemented))
  ([requested-type size-hint coll] (not-implemented)))

(defn ^:private item-types-match? :- Boolean
  "Returns `true` if item types match, `false` otherwise."
  [requested-type :- (U nil Class Type),
   available-type :- (U nil Class Type)]
  (or (nil? available-type)
      (nil? requested-type)
      (identical? requested-type available-type)
      (identical? (provide-class requested-type)
                  (provide-class available-type))))


;;;; Public API

;;; Reducing

(defn provide-reduced :- Reduced
  "Returns reduced reference to `_x_`, or returns `_x_` if it is
  already a reduced reference."
  {:added v1
   :category "Reducers"
   :see '[strip-reduced dunaj.coll/reduced dunaj.coll/reduced?]}
  [x :- Any]
  (if (reduced? x) x (reduced x)))

(defn strip-reduced :- Any
  "Returns the underlying referenced object if `_x_` is reduced,
  otherwise returns `_x_`."
  {:added v1
   :category "Reducers"
   :see '[provide-reduced dunaj.coll/reduced dunaj.coll/reduced?]}
  [x :- Any]
  (if (reduced? x) @x x))

(def reducing-function
  "Returns a reducing function created from augmented reducing
  function `_r_`."
  {:added v1
   :arglists '([r])
   :tsig (Fn [AnyFn IReducing])
   :category "Reducers"
   :see '[dunaj.coll/reducing]}
  @#'dunaj.coll/reducing-function)

(def reduce*
  "Returns the result of reduction of `_coll_` with `_reducef_`,
  starting with `_init_`. Returns `_init_` if `_coll_` is `nil` or
  empty.

  IMPORTANT: May return reduced object."
  {:added v1
   :arglists '([coll reducef init])
   :see '[dunaj.coll/reduce reduce-batched* reduce-unpacked*
          reduce-augmented* transduce*]
   :category "Reducers"}
  @#'dunaj.coll/reduce*)

;; TODO: add it to the public API?
(def red->seq* :- Any
  @#'dunaj.coll/red->seq*)

(def reduce-unpacked*
  "Returns the result of unpacked reduction of `_coll_` with
  `_reducef_`, starting with `_init_`.
  Returns `_init_` if `_coll_` is `nil` or empty.

  IMPORTANT: May return reduced object."
  {:added v1
   :arglists '([coll reducef init])
   :see '[dunaj.coll/reduce reduce-batched* reduce*
          reduce-augmented* transduce*]
   :category "Reducers"}
  @#'dunaj.coll/reduce-unpacked*)

(def reduce-batched*
  "Returns the result of batched reduction of `_coll_` with
  `_reducef_`, starting with `_init_`.
  Returns `_init_` if `_coll_` is `nil` or empty.
  May supply `_item-type_` and `_size-hint_` parameters for
  underlying batched reduction.

  IMPORTANT: May return reduced object."
  {:added v1
   :arglists '([coll reducef init]
               [item-type size-hint coll reducef init])
   :see '[dunaj.coll/reduce reduce-unpacked* reduce*
          reduce-augmented* transduce*]
   :category "Reducers"}
  @#'dunaj.coll/reduce-batched*)

(defn reduce-with-batched*
  "Returns the result of batched reduction of `_coll_` with
  `_reducef_` which takes item instead of batch of items.

  IMPORTANT: May return reduced object."
  {:added v1
   :see '[reduce-batched*]
   :category "Reducers"}
  ([coll reducef init]
   (reduce-with-batched* nil nil coll reducef init))
  ([item-type size-hint coll reducef init]
   (let [rf (fn [ret batch] (reduce* batch reducef ret))]
     (reduce-batched* item-type size-hint coll rf init))))

(def reduce-augmented*
  "Returns a result of the reduction of `_coll_` with `_reduce-fn_`
  function taking `coll` `reducef` `init` in that order
  (e.g. `reduce*`), and with the augmented reducing function `_r_`
  and with initial value `_init_`,
  which defaults to `(-init r)`.

  IMPORTANT: May return reduced object."
  {:added v1
   :arglists '([coll reduce-fn r] [coll reduce-fn r init])
   :see '[dunaj.coll/reduce reduce-unpacked* reduce*
          reduce-batched* transduce*]
   :category "Reducers"}
  @#'dunaj.coll/reduce-augmented*)

(def transduce*
  "Returns a result of the reduction of `_coll_` with `_reduce-fn_`
  function and with the classic reducing function `_reducef_`,
  initial value `_init_` (which defaults to `(_reducef_)` if not
  provided), and transducer `_xform_`.

  IMPORTANT: May return reduced object."
  {:added v1
   :arglists '([coll reduce-fn xform reducef]
               [coll reduce-fn xform reducef init])
   :see '[dunaj.coll/reduce reduce-unpacked* reduce*
          reduce-batched* reduce-augmented*]
   :category "Reducers"}
  @#'dunaj.coll/transduce*)

(def fold*
  "Returns result of performing fold on a collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  until collection length is equal or less than `_n_`. Uses
  `_combinef_` and `_reducef_` for reduction.

  Uses `_reduce-fn_` taking `coll` `reducef` and `init` in that
  order for performing actual reduction (put e.g. `reduce*` there)."
  {:added v1
   :arglists '([coll reduce-fn pool n combinef reducef])
   :tsig (Fn [Any [] AnyFn ForkJoinPool Integer AnyFn AnyFn])
   :see '[fold-augmented* fold-batched* fold-unpacked* transfold*
          reduce-augmented* reduce* reduce-batched* reduce-unpacked*]
   :category "Folds"}
  @#'dunaj.concurrent.forkjoin/fold*)

(def fold-augmented*
  "Returns result of performing fold on a collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  until collection length is equal or less than `_n_`. Uses
  augmented reducing function `_r_` for reduction.

  Uses `_reduce-fn_` taking `coll` `reducef` and `init` in that
  order for performing actual reduction (put e.g. `reduce*` there)."
  {:added v1
   :arglists '([coll reduce-fn pool n r])
   :tsig (Fn [Any [] AnyFn ForkJoinPool Integer IReducing])
   :see '[fold* fold-batched* fold-unpacked* transfold*
          reduce-augmented* reduce* reduce-batched* reduce-unpacked*]
   :category "Folds"}
  @#'dunaj.concurrent.forkjoin/fold-augmented*)

(defn fold-batched* :- Any
  "Returns result of performing fold on a batchable collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  until collection length is equal or less than `_n_`. Uses
  `_combinef_` and `_reducef_` for reduction.

  Uses batched reduction, passing `_requested-type_` and
  `_size-hint_` arguments."
  {:added v1
   :see '[fold* fold-augmented* fold-unpacked* transfold*
          reduce-augmented* reduce* reduce-batched* reduce-unpacked*]
   :category "Folds"}
  [coll :- [], requested-type :- (U nil Class Type),
   size-hint :- (Maybe Integer), pool :- ForkJoinPool,
   n :- Integer, combinef :- AnyFn, reducef :- AnyFn]
  (if (and (satisfies? IBatchedRed coll)
           (item-types-match? requested-type (item-type coll)))
    (fold* coll #(reduce-batched* requested-type size-hint % %2 %3)
           pool n combinef reducef)
    (fold* (batch requested-type size-hint coll) reduce*
           pool n combinef reducef)))

(defn fold-unpacked* :- Any
  "Returns result of performing fold on a batchable collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  until collection length is equal or less than `_n_`. Uses
  `_combinef_` and `_reducef_` for reduction.

  Uses unpacked reduction, faking it for collection that do not
  implement `<<dunaj.coll.spi.ad#IUnpackedRed,IUnpackedRed>>`."
  {:added v1
   :see '[fold* fold-batched* fold-augmented* transfold*
          reduce-augmented* reduce* reduce-batched* reduce-unpacked*]
   :category "Folds"}
  [coll :- [], pool :- ForkJoinPool,
   n :- Integer, combinef :- AnyFn, reducef :- AnyFn]
  (if (satisfies? IUnpackedRed coll)
    (fold* coll reduce-unpacked* pool n combinef reducef)
    (fold* coll reduce* pool n combinef (unpacked-fn reducef))))

(def transfold*
  "Returns result of performing fold on a collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  until collection length is equal or less than `_n_`. Uses
  transducer `_xform_`, `_combinef_` and `_reducef_` for reduction.

  Uses `_reduce-fn_` taking `coll` `reducef` and `init` in that
  order for performing actual reduction (put e.g. `reduce*` there)."
  {:added v1
   :arglists '([coll reduce-fn pool n xform reducef]
               [coll reduce-fn pool n xform combinef reducef])
   :tsig (Fn [Any IRed AnyFn ForkJoinPool Integer AnyFn AnyFn])
   :category "Folds"}
  @#'dunaj.concurrent.forkjoin/transfold*)

;; NOTE: same macro is in dunaj.coll
(defmacro advance-fn
  "Constructs a function which correctly handle reduced and
  postponed `ret`, a arg which must be the first argument in the
  arg vector. Other arguments are passed without any processing
  for both safe and unsafe advance.

  `(advance-fn [ret param-1] (do-something))` is transformed into:

  [source,clojure,linesnum]
  --
  (fn af [ret param-1]
    (cond (reduced? ret)
          ret
          (postponed? ret)
          (postponed ret
                     #(af (advance ret) param-1)
                     #(af (unsafe-advance! ret) param-1))
          :else
          (do-something)))
  --

  TIP: Multiple cond clauses may be passed into advance-fn."
  {:added v1
   :category "Reducers"
   :see '[cloned-advance-fn finish-advance reduced-advance]}
  [fname-or-params & cond-body]
  (let [fname? (not (clojure.core/vector? fname-or-params))
        fname (if fname? fname-or-params (gensym "fname"))
        params (if fname?
                   (clojure.core/first cond-body)
                   fname-or-params)
        cond-body (if fname?
                    (clojure.core/rest cond-body)
                    cond-body)
        cond-body (if (clojure.core/odd?
                       (clojure.core/count cond-body))
                    (cons :else cond-body)
                    cond-body)
        ret (clojure.core/first params)
        args (clojure.bootstrap/strip-sigs-vec
              (clojure.core/rest params))]
    `(clojure.bootstrap/fn ~fname ~params
       (dunaj.flow/cond
        (reduced? ~ret) ~ret
        (postponed? ~ret)
        (postponed (dunaj.state/deref ~ret)
                   #(~fname (advance ~ret) ~@args)
                   #(~fname (unsafe-advance! ~ret) ~@args))
        ~@cond-body))))

(defmacro cloned-advance-fn
  "Constructs a function which correctly handle reduced and
  postponed `ret`, which must be the first argument in the
  params vector. Other arguments are cloned in safe advance and
  passed without processing for unsafe advance."
  {:added v1
   :category "Reducers"
   :see '[advance-fn finish-advance reduced-advance]}
  [fname-or-params & cond-body]
  (let [fname? (not (clojure.core/vector? fname-or-params))
        fname (if fname? fname-or-params (gensym "fname"))
        bindings (if fname?
                   (clojure.core/first cond-body)
                   fname-or-params)
        cond-body (if fname?
                    (clojure.core/rest cond-body)
                    cond-body)
        cond-body (if (clojure.core/odd?
                       (clojure.core/count cond-body))
                    (cons :else cond-body)
                    cond-body)
        ret (clojure.core/first bindings)
        args (clojure.bootstrap/strip-sigs-vec
              (clojure.core/rest bindings))
        cargs (clojure.core/map #(list 'dunaj.state/clone %) args)]
    `(clojure.bootstrap/fn ~fname ~bindings
       (dunaj.flow/cond
        (reduced? ~ret) ~ret
        (postponed? ~ret)
        (postponed (dunaj.state/deref ~ret)
                   #(~fname (advance ~ret) ~@cargs)
                   #(~fname (unsafe-advance! ~ret) ~@args))
        ~@cond-body))))

(def finish-advance
  "Returns the result of calling `-finish` on the augmented reducing
  function `_r_`, while correctly handling potentionally postponed
  result `ret`.

  NOTE: By convention, `-finish` should be called also when `ret` is
  a reduced result."
  {:added v1
   :arglists '([ret r])
   :tsig (Fn [Any Any IReducing])
   :category "Reducers"
   :see '[advance-fn cloned-advance-fn reduced-advance]}
  @#'dunaj.coll/finish-advance)

(def reduced-advance
  "Returns the reduced result based on `ret`, which may also be
  a reduced value, while correctly handling potentionally postponed
  result. Can control whether the result should be reduced or not
  with `_was-reduced?_` flag."
  {:added v1
   :arglists '([ret] [ret was-reduced?])
   :tsig (Fn [Any Any] [Any Any Boolean])
   :category "Reducers"
   :see '[advance-fn cloned-advance-fn finish-advance]}
  @#'dunaj.coll/reduced-advance)

(defmacro defreducing
  "Defines a stateless augmented reducing function type which
  delegates all but stepping functionality to the underlying
  augmented reducing function, which must be stored under the
  first field in the created type."
  {:added v1
   :category "Primary"
   :see '[defxform]}
  [& body]
  (let [[bb ab] (clojure.core/split-with
                 (clojure.core/comp clojure.core/not
                                    clojure.core/vector?)
                 body)
        bv (clojure.core/first ab)
        r (clojure.core/first bv)
        ab (clojure.core/rest ab)]
    `(deftype ~@bb ~bv
       dunaj.coll/IReducing
       (-init [this#]
         (._init ^dunaj.coll.IReducing ~r))
       (-finish [this# wrap#]
         (._finish ^dunaj.coll.IReducing ~r wrap#))
       (-wrap [this# ret#]
         (._wrap ^dunaj.coll.IReducing ~r ret#))
       (-unwrap [this# wrap#]
         (._unwrap ^dunaj.coll.IReducing ~r wrap#))
       (-combine [this wrap# other#]
         (._combine ^dunaj.coll.IReducing ~r wrap# other#))
       ~@ab)))

;;; Coll adapter

(defprotocol IRedAdapter
  "A value protocol for collection adapters.
  Collection adapters are a mechanism to limit, in runtime,
  extended protocols a collection wants to support."
  {:added v1
   :see '[adapt]}
  (-inner-coll :- IRed
    "Returns the inner collection which `_this_` adapter wraps."
    [this]))

(defmacro gen-cbus
  "Defines an Adapter type which provides ICounted, IBatchedRed,
  IUnpackedRed and ISectionable, based on the values of `c?`, `b?`,
  `u?` and `s?`, respectively."
  [c? b? u? s?]
  (let [name (symbol (str "Reducible"
                             (if c? "C" "c")
                             (if b? "B" "b")
                             (if u? "U" "u")
                             (if s? "S" "s")))
        coll (gensym "coll__")]
    `(deftype ~name [~coll]
       IRedAdapter
       (-inner-coll [this] ~coll)
       IRed
       (-reduce [this# f# init#] (dunaj.coll/-reduce ~coll f# init#))
       IFoldable
       (-fold [this# reduce-fn# pool# n# combinef# reducef#]
         (dunaj.concurrent.forkjoin/-fold
          ~coll reduce-fn# pool# n# combinef# reducef#))
       ~@(when c?
           `[ICounted
             (-count [this#] (dunaj.coll/-count ~coll))])
       ~@(when b?
           `[IBatchedRed
             (-reduce-batched
              [this# item-type# size-hint# f# init#]
              (dunaj.coll/-reduce-batched
               ~coll item-type# size-hint# f# init#))
             IHomogeneous
             (-item-type [this#] (dunaj.coll/-item-type ~coll))])
       ~@(when u?
           `[IUnpackedRed
             (-reduce-unpacked
              [this# f# init#]
              (dunaj.coll/-reduce-unpacked ~coll f# init#))])
       ~@(when s?
           `[ISectionable
             (-section
              [this# nb# ne#]
              (dunaj.coll/-section ~coll nb# ne#))]))))

(defmacro emit-cbus
  "Generates all possible collection adapter types."
  []
  `(do
     ~@(let [opts [true false]]
         (for [c opts b opts u opts s opts]
           `(gen-cbus ~c ~b ~u ~s)))))

(emit-cbus)

;; nil, true, false
(defmacro gen-adapt
  "Defines an adapter constructor, based on values of `c`, `b`, `u`
  and `s`. Values of arguments can be one of:
  - `true`: adapter will always have this protocol enabled
  - `false`: adapter will always have this protocol disabled
  - `nil`: adapter constructor will check whether input collection
    supports given protocol and only then will the coll adapter
    supports it."
  [c b u s]
  (let [c? (gensym "c?")
        b? (gensym "b?")
        u? (gensym "u?")
        s? (gensym "s?")
        getc #(cond (nil? %) "" (false? %) %2 :else %3)
        names (str "adapt" (getc c "c" "C") (getc b "b" "B")
                   (getc u "u" "U") (getc s "s" "S"))
        all? (== 9 (count names))
        name (symbol names)
        name (assoc-meta name {:added v1 :see ''[adapt*]
                               :category "Adapters"})
        coll (gensym "coll__")
        to (gensym "to__")
        bs []
        bs (if (nil? c) (conj bs c? `(counted? ~to)) bs)
        bs (if (nil? b) (conj bs b? `(clojure.core/satisfies?
                                      dunaj.coll/IBatchedRed ~to)) bs)
        bs (if (nil? u)
             (conj bs u? `(clojure.core/satisfies?
                           dunaj.coll/IUnpackedRed ~to))
             bs)
        bs (if (nil? s) (conj bs s? `(sectionable? ~to)) bs)
        pf (fn [p pred big small]
             (cond (nil? p) {:pred pred :big big :small small}
                   (false? p) small
                   :else big))
        bd (fn bd [s p]
             (let [fp (first p)]
               (cond
                (nil? fp) (list (symbol (str "->Reducible" s)) coll)
                (map? fp) `(if ~(:pred fp)
                             ~(bd (str s (:big fp)) (rest p))
                             ~(bd (str s (:small fp)) (rest p)))
                :else (bd (str s fp) (rest p)))))]
    `(defn ~name [~coll ~@(when-not all? [to])]
       (let [~@bs] ~(bd "" [(pf c c? \C \c) (pf b b? \B \b)
                            (pf u u? \U \u) (pf s s? \S \s)])))))

(defmacro emit-adapt
  "Generates all possible collection adapter constructors."
  []
  `(do
     ~@(let [opts [nil true false]]
         (for [c opts b opts u opts s opts]
           `(gen-adapt ~c ~b ~u ~s)))))

(emit-adapt)

(defn adapt* :- IRedAdapter
  "Returns an adapted `_coll_` with protocol support based on whether
  `_coll_` and `_to_` supports them and whether given argument is
  `true` (support), `nil` (depends on `_to_`) or `false`
  (do not support). Returned collection always supports
  `<<dunaj.coll.spi.ad#IRed,IRed>>` and
  `<<dunaj.concurrent.forkjoin.spi.ad#IFoldable,IFoldable>>`

  Following functionalities are supported:

  * `_oc_` - `<<dunaj.coll.spi.ad#ICounted,ICounted>>`
  * `_ob_` - `<<dunaj.coll.spi.ad#IBatchedRed,IBatchedRed>>` and
    `<<dunaj.coll.spi.ad#IHomogeneous,IHomogeneous>>`
  * `_ou_` - `<<dunaj.coll.spi.ad#IUnpackedRed,IUnpackedRed>>`
  * `_os_` - `<<dunaj.coll.spi.ad#ISectionable,ISectionable>>`

  Special functions beginning with `adapt` are autogenerated for
  cases where some desired features are known at compile-time."
  {:added v1
   :category "Adapters"}
  [coll :- [], to :- [],
   oc :- (Maybe Boolean), ob :- (Maybe Boolean),
   ou :- (Maybe Boolean), os :- (Maybe Boolean)]
  (let [c? (if (nil? oc) (and (counted? coll) (counted? to)) oc)
        b? (if (nil? ob)
             (and
              (clojure.core/satisfies? dunaj.coll/IBatchedRed coll)
              (clojure.core/satisfies? dunaj.coll/IBatchedRed to))
             ob)
        u? (if (nil? ou)
             (and
              (clojure.core/satisfies? dunaj.coll/IUnpackedRed coll)
              (clojure.core/satisfies? dunaj.coll/IUnpackedRed to))
             ou)
        s? (if (nil? os)
             (and (sectionable? coll) (sectionable? to))
             os)]
    (if s?
      (if c?
        (if b?
          (if u? (->ReducibleCBUS coll) (->ReducibleCBuS coll))
          (if u? (->ReducibleCbUS coll) (->ReducibleCbuS coll)))
        (if b?
          (if u? (->ReduciblecBUS coll) (->ReduciblecBuS coll))
          (if u? (->ReduciblecbUS coll) (->ReduciblecbuS coll))))
      (if c?
        (if b?
          (if u? (->ReducibleCBUs coll) (->ReducibleCBus coll))
          (if u? (->ReducibleCbUs coll) (->ReducibleCbus coll)))
        (if b?
          (if u? (->ReduciblecBUs coll) (->ReduciblecBus coll))
          (if u? (->ReduciblecbUs coll) (->Reduciblecbus coll)))))))

;;; JVM Interop and comparison helpers

(defn index-of :- Int
  "Returns index of `_x_` in `_coll_` computed by reducing,
  using `<<dunaj.compare.api.ad#{under}EQ{under},&#61;>>`
  for equality comparison."
  {:added v1
   :category "Interop"}
  [coll :- IRed, x :- Any]
  (let [i (reduce #(if (= x %2) (reduced (reduced %)) (iinc %))
                  (i0) coll)]
    (if (reduced? i) @i (i-1))))

(defn ordered-hash-code :- Int
  "Returns host hash code for an ordered collection `_coll_`.

  NOTE: This function is specific to JVM host."
  {:added v1
   :category "Interop"
   :see '[unordered-hash-code equals-ordered]}
  [coll :- IRed]
  (reduce #(iadd (imul (i31) %)
                 (if (nil? %2) (i0) (.hashCode ^java.lang.Object %2)))
          (i1) coll))

(defn unordered-hash-code :- Int
  "Returns host hash code for an unordered collection `_coll_`.

  NOTE: This function is specific to JVM host."
  {:added v1
   :category "Interop"
   :see '[ordered-hash-code]}
  [coll :- IRed]
  (reduce
   #(iadd % (if (nil? %2) (i0) (.hashCode ^java.lang.Object %2)))
   (i0) coll))

(defn coll->iterator :- java.util.Iterator
  "Returns
  https://docs.oracle.com/javase/8/docs/api/java/util/Iterator.html[Iterator]
  for a given `_coll_`.

  NOTE: This function is specific to JVM host."
  {:added v1
   :category "Interop"
   :see '[coll->list-iterator]}
  [coll :- []]
  (let [s :- java.lang.Iterable
        (or (seq coll) clojure.lang.PersistentList/EMPTY)]
    (.iterator s)))

(defn coll->list-iterator :- java.util.ListIterator
  "Returns
  https://docs.oracle.com/javase/8/docs/api/java/util/ListIterator.html[ListIterator]
  for a given `_coll_` starting at `_i_` position, which defaults
  to 0.

  NOTE: This function is specific to JVM host."
  {:added v1
   :category "Interop"
   :see '[coll->iterator]}
  ([coll :- []]
   (let [s :- java.util.List
         (or (seq coll) clojure.lang.PersistentList/EMPTY)]
     (.listIterator s)))
  ([coll :- [], i :- Int]
   (let [s :- java.util.List
         (or (seq coll) clojure.lang.PersistentList/EMPTY)]
     (.listIterator s i))))

(defn equiv-ordered :- Boolean
  "Returns `true` if given sequential collections are equivalent,
  returns `false` otherwise. If `_any-immutable?_` is `true`, accepts
  any ordered collection, not just sequential ones."
  {:added v1
   :category "Primary"
   :see '[equals-ordered compare-ordered]}
  ([coll :- [], other :- []]
   (equiv-ordered coll other false))
  ([coll :- [], other :- [], any-immutable? :- Boolean]
   (if (or (and (not any-immutable?) (not (sequential? other)))
           (and (counted? coll) (counted? other)
                (not (== (count coll) (count other)))))
     false
     (loop [a1 (reduce #(postponed %2) nil coll)
            a2 (reduce #(postponed %2) nil other)]
       (cond (and (postponed? a1) (postponed? a2))
             (if (clojure.lang.Util/equiv @a1 @a2)
               (recur (unsafe-advance! a1) (unsafe-advance! a2))
               false)
             (postponed? a1) false
             (postponed? a2) false
             :else (clojure.lang.Util/equiv a1 a2))))))

(defn equals-ordered :- Boolean
  "Returns `true` if given sequential collections are equivalent
  according to host equality criteria, otherwise returns
  `false`. If `_any-immutable?_` is `true`, accepts any ordered
  collection, not just sequential one."
  {:added v1
   :category "Interop"
   :see '[equiv-ordered ordered-hash-code compare-ordered]}
  ([coll :- [], other :- []]
   (equals-ordered coll other false))
  ([coll :- [], other :- [], any-immutable? :- Boolean]
   (if (or (and (not any-immutable?) (not (sequential? other)))
           (and (counted? coll) (counted? other)
                (not (== (count coll) (count other)))))
     false
     (loop [a1 (reduce #(postponed %2) nil coll)
            a2 (reduce #(postponed %2) nil other)]
       (cond (and (postponed? a1) (postponed? a2))
             (if (clojure.lang.Util/equals @a1 @a2)
               (recur (unsafe-advance! a1) (unsafe-advance! a2))
               false)
             (postponed? a1) false
             (postponed? a2) false
             :else (clojure.lang.Util/equals a1 a2))))))

(defn compare-ordered :- Integer
  "Returns -1, 0 or 1 as a result of comparing two ordered collections
  with default comparator."
  {:added v1
   :category "Primary"
   :see '[equiv-ordered equals-ordered]}
  [coll :- [], other :- []]
  (let [c1? (counted? coll)
        c2? (counted? other)
        c1 (when c1? (iint (count coll)))
        c2 (when c2? (iint (count other)))]
    (cond
     (and c1? c2? (i< c1 c2)) (i-1)
     (and c1? c2? (i> c1 c2)) (i1)
     :else (loop [a1 (reduce #(postponed %2) nil coll)
                  a2 (reduce #(postponed %2) nil other)]
             (cond (and (postponed? a1) (postponed? a2))
                   (let [c (compare @a1 @a2)]
                     (if (zero? c)
                       (recur (unsafe-advance! a1)
                              (unsafe-advance! a2)) c))
                   (postponed? a1) (i1)
                   (postponed? a2) (i-1)
                   :else (compare a1 a2))))))

;;; Section

(defn prepare-ordered-section :- Integer
  "Returns `_new-end_` or `_length_`, if `_new-end_` is `nil`.
  Throws if arguments are out of bounds of the interval
  `[new-begin, new-begin + length)`."
  {:added v1
   :category "Primary"
   :see '[dunaj.coll/section]}
  [new-begin :- Integer, new-end :- (Maybe Integer),
   length :- Integer]
  (let [new-end (or new-end length)]
    (when (or (neg? new-begin)
              (> new-begin new-end)
              (> new-end length))
      (throw (java.lang.IndexOutOfBoundsException.
              (str new-begin " " new-end " " length))))
    new-end))

;;; Folding

(defn split-adjust :- (Maybe Int)
  "Adjusts place where split should happen. Stops at position at
  which `_f_` returns a different result from one returned at
  `_start_` position. Returns `nil` if `_end_` has been reached and
  no such position was found."
  {:added v1
   :category "Folds"
   :see '[fold-sectionable]}
  [f :- AnyFn, start :- Int, end :- Int]
  (let [fval (f start)]
    (iloop [x (iinc start)]
      (cond (i== x end) nil
            (identical? fval (f x)) (recur (iinc x))
            :else x))))

(defn fold-sectionable :- Any
  "Returns result of performing fold on a sectionable collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  until collection length is equal or less than `_n_`. Uses
  `_combinef_` and `_reducef_` for reduction.

  Uses `_reduce-fn_` taking `coll` `reducef` and `init` in that
  order for performing actual reduction (put e.g. `reduce*` there).

  Uses `_section-fn_`, which defaults to
  `<<dunaj.coll.api.ad#section,section>>`, for sectioning collection.

  Uses `_count-fn_`, which defaults to
  `<<dunaj.coll.api.ad#count,count>>`, for counting collection."
  {:added v1
   :category "Folds"
   :see '[split-adjust fold-every]}
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn]
   (fold-sectionable
    coll reduce-fn pool n combinef reducef section))
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn,
    section-fn :- AnyFn]
   (fold-sectionable
    coll reduce-fn pool n combinef reducef section-fn count))
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn,
    section-fn :- AnyFn, count-fn :- AnyFn]
   (let [cnt (iint (count-fn coll))
         n (imax (i1) (iint n))]
     (if (i<= cnt n)
       (reduce-fn coll reducef (combinef))
       (let [split (idiv cnt (i2))
             v1 (section-fn coll 0 split)
             v2 (section-fn coll split cnt)
             fc (fn [child]
                  #(-fold child reduce-fn pool n combinef reducef))]
         (invoke pool #(let [f1 (fc v1), t2 (fork (fc v2))]
                         (combinef (f1) (join t2)))))))))

(defn fold-every :- Any
  "Returns result of performing fold on every item in the collection
  `_coll_`, using forkjoin `_pool_` for executing tasks, dividing
  each item until collection length is equal or less than `_n_`. Uses
  `_combinef_` and `_reducef_` for reduction.

  Uses `_reduce-fn_` taking `coll` `reducef` and `init` in that
  order for performing actual reduction (put e.g. `reduce*` there).

  Uses `_section-fn_`, which defaults to
  `<<dunaj.coll.api.ad#section,section>>`, for sectioning items in a
  collection.

  Uses `_count-fn_`, which defaults to
  `<<dunaj.coll.api.ad#count,count>>`, for counting items in a
  collection."
  {:added v1
   :category "Folds"
   :see '[split-adjust fold-every]}
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn]
   (fold-every coll reduce-fn pool n combinef reducef section))
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn,
    section-fn :- AnyFn]
   (fold-every
    coll reduce-fn pool n combinef reducef section-fn count))
  ([coll :- IRed, reduce-fn :- AnyFn, pool :- ForkJoinPool,
    n :- Integer, combinef :- AnyFn, reducef :- AnyFn,
    section-fn :- AnyFn, count-fn :- AnyFn]
   (let [dofn (fn dofn [coll]
                (let [cnt (iint (count-fn coll))]
                  (cond
                    (izero? cnt) (combinef)
                    (ione? cnt) (fold* (first coll) reduce-fn pool
                                       n combinef reducef)
                    :else (let [split (idiv cnt (i2))
                                v1 (section-fn coll 0 split)
                                v2 (section-fn coll split cnt)
                                forked (fork #(dofn v2))]
                            (combinef (dofn v1) (join forked))))))]
     (invoke pool #(dofn coll)))))

;;; recipe

(deftype Recipe
  "A type for collection recipe."
  [coll :- IRed, xform :- AnyFn,
   count-fn :- AnyFn, section-fn :- AnyFn, foldable? :- Boolean]
  IRed
  (-reduce [this reducef init]
    (transduce* coll reduce* xform reducef init))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (if foldable?
      (transfold* coll reduce-fn pool n xform combinef reducef)
      (reduce-augmented* coll reduce-fn
                         (xform (folding combinef reducef)))))
  ICounted
  (-count [this] (count-fn (count coll)))
  IHomogeneous
  (-item-type [this] nil) ;; assumes xform will support requested type
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [nxform (comp (mapcat identity)
                       xform
                       (batch requested-type size-hint))
          rtype (when (item-types-match? requested-type
                                         (item-type coll))
                  requested-type)]
      (transduce* coll #(reduce-batched* rtype size-hint % %2 %3)
                  nxform reducef init)))
  IUnpackedRed
  (-reduce-unpacked [this reducef init]
    (transduce* coll reduce-unpacked* xform reducef init))
  ISectionable
  (-section [this nb ne]
    (section-fn #(->Recipe (dunaj.coll/-section %1 %2 %3)
                           xform count-fn section-fn foldable?)
                coll nb ne)))

(deftype XTraits
  "A type for xform traits."
  [count :- Any, section :- Any, unpack :- Any, fold :- Any])

(defn recipe :- []
  "Returns a reducible collection which uses `_xform_` for reducing
  `_coll_`. Optionally supports fold, section, count, batched and
  unpacked reduce based on which features are supported by
  `_xform_` and `_coll_`."
  [xform :- AnyFn, coll :- []]
  (let [cc? (counted? coll)
        cb? (satisfies? dunaj.coll/IBatchedRed coll)
        cu? (satisfies? dunaj.coll/IUnpackedRed coll)
        cs? (sectionable? coll)
        traits :- XTraits
        (xform (->XTraits
                (when cc? identity) #(%1 %2 %3 %4) cu? true))
        tc-fn (.-count traits)
        tu? (.-unpack traits)
        ts-fn (.-section traits)
        tf? (.-fold traits)
        c? (boolean (and cc? tc-fn))
        u? (boolean (and cu? tu?))
        s? (boolean (and cs? ts-fn))
        adapt-fn (fn [r] (adapt* r nil c? cb? u? s?))
        ts-fn #(adapt-fn (ts-fn %1 %2 %3 %4))]
    (adapt-fn (->Recipe coll xform tc-fn ts-fn tf?))))

(defmacro defxform
  "Defines a transducer with a `_name_` and with contents created
  based on given `_keyvals_`. Supported keys are (`nil` means pass):

  * `:xform` - one arg fn decl taking augmented recipe and returning
    augmented recipe
  * `:count` - `nil`/`false`/`one arg fn decl` taking nested count
    function and returning one arg function (or `nil`/`false`)
  * `:section` - `nil`/`false`/`one arg fn decl` taking nested
    section function and returning four arg function
    (or `nil`/`false`)
  * `:unpack` - `true`/`false`/`nil`/`one arg fn decl` taking nested
    unpack flag and returning boolean value (or `nil`)
  * `:fold` - (`true`)/`false`/`nil`/`one arg fn decl` taking nested
    fold flag and returning boolean value (or `nil`)"
  {:added v1
   :category "Primary"
   :see '[defreducing]}
  [name & keyvals]
  (let [pre (take-while (comp not vector?) keyvals)
        body (drop-while (comp not vector?) keyvals)
        bvec (first body)
        sbvec (strip-sigs-vec bvec)
        tsigm (clojure.core/vec
               (cons 'dunaj.coll/Transducer (get-sigs-vec bvec)))
        tsigcm (clojure.core/vec
                (concat '[dunaj.coll/IRed]
                        (get-sigs-vec bvec) '[[]]))
        tsig (list 'dunaj.type/Fn tsigm tsigcm)
        body (rest body)
        body (if (keyword? (first body)) body (cons :xform body))
        bm (apply hash-map body)
        coll (gensym "coll__")
        r (first (first (:xform bm)))
        tc (if (list? (:count bm))
             (let [c (first (first (:count bm)))]
               (or c (gensym "tcfn")))
             (gensym "tcfn"))
        ts (if (list? (:section bm))
             (let [c (first (first (:section bm)))]
               (or c (gensym "tsfn")))
             (gensym "tsfn"))
        tu (if (list? (:unpack bm))
             (let [c (first (first (:unpack bm)))]
               (or c (gensym "tu")))
             (gensym "tu"))
        tf (if (list? (:fold bm))
             (let [c (first (first (:fold bm)))]
               (or c (gensym "tf")))
             (gensym "tf"))
        name (clojure.core/vary-meta name clojure.core/assoc
                                     :transducer true
                                     :tsig tsig)]
    ;; TODO: correct tsigs
    `(defn ~name
       ~@pre
       (~sbvec
        (fn [~r]
          (let ~(or (:let bm) [])
            (if (class-instance? dunaj.coll.IReducing ~r)
              ~(cons 'clojure.core/do (rest (:xform bm)))
              (let [traits# :- XTraits ~r
                    ~tc (.-count traits#)
                    ~ts (.-section traits#)
                    ~tu (.-unpack traits#)
                    ~tf (.-fold traits#)
                    ~tc ~(cond (nil? (:count bm)) tc
                               (list? (:count bm))
                               (list 'clojure.core/when
                                     tc (second (:count bm)))
                               :else (:count bm))
                    ~ts ~(cond (nil? (:section bm)) ts
                               (list? (:section bm))
                               (list 'clojure.core/when
                                     ts (second (:section bm)))
                               :else (:section bm))
                    ~tu ~(cond (nil? (:unpack bm)) tu
                               (list? (:unpack bm))
                               (list 'clojure.core/when
                                     tu (second (:unpack bm)))
                               :else (:unpack bm))
                    ~tf ~(cond (nil? (:fold bm)) tf
                               (list? (:fold bm))
                               (list 'clojure.core/when
                                     tf (second (:fold bm)))
                               :else (:fold bm))]
                (->XTraits ~tc ~ts ~tu ~tf))))))
       (~(conj sbvec coll)
        (recipe (~name ~@sbvec) ~coll)))))
