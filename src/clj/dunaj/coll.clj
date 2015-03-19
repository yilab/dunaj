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

(ns dunaj.coll
  "Immutable, mutable and persistent collections.
  Reducers and transducers icon:puzzle-piece[].
  
  In Dunaj, a collection in a broadest sense is defined as an object
  that implements `<<dunaj.coll.spi.ad#IRed,IRed>>`.
  There are no other requirements for such object.
  In Dunaj, any object is considered a collection as long as
  it is reducible by implementing `IRed` protocol.
  Predicate for such collection is called
  `<<dunaj.coll.api.ad#red{under}QMARK{under},red?>>`.
  Other names for a collection that at least implements `IRed`
  include '`reducible`', '`reducible collection`' or just
  '`a collection`' (last one is the most used in Dunaj docs).
  
  .Major groups of collections
  ****
  In addition to reducible collections, dunaj defines 5 mutually
  exclusive groups of collections:

  [discrete]
  === Immutable collections
  
  - implement `IRed`
  - do not implement `<<dunaj.coll.spi.ad#IPersistentCollection,IPersistentCollection>>`
    nor `<<dunaj.coll.spi.ad#ISeq,ISeq>>`
  - are not persistent
  - can be `<<dunaj.coll.spi.ad#IEditable,IEditable>>` -
    return mutable collection, preferably transient
  - have items stored in memory or lazily evaluated with caching
  - examples: `<<dunaj.string.api.ad#String,String>>`,
    `<<dunaj.host.array.api.ad#adapt,ArrayColl>>` or
    `ImmutableBuffer`

  [discrete]
  === Persistent collections

  - implement `IRed` and `IPersistentCollection`
  - do not implement `ISeq`
  - can be `IEditable` - return mutable collection, preferably
    transient
  - have items stored in memory or lazily evaluated with caching
  ** in later case, time guarantees may not be met on a first call,
     when lazy parts of the collection are realized.
  - examples: `<<dunaj.coll.array-map.api.ad#,ArrayMap>>`,
    `<<dunaj.coll.bvt-vector.api.ad#,BvtVector>>`

  [discrete]
  === Seqs
  
  - implement `IRed`, `IPersistentCollection` and `ISeq`
  - are usually wrappers on top of other collections
  - have items stored in memory or lazily evaluated with caching
  - examples: `<<dunaj.coll.linked-list.api.ad,LinkedList>>`,
    `<<dunaj.coll.lazy-seq.api.ad#,Lazy>>`,
    `<<dunaj.coll.cons-seq.api.ad#,Cons>>`

  [discrete]
  === Mutable collections
  
  - do not necessarily implement `IRed`
  - if implement `IRed`, must also be
    `<<dunaj.concurrent.thread.spi.ad#IThreadLocal,IThreadLocal>>` or
    `<<dunaj.coll.spi.ad#ISnapshoatable,ISnapshotable>>`
  - implement some mutable protocols, usually
    `<<dunaj.coll.spi.ad#IMutableCollection,IMutableCollection>>`
  - can be `<<dunaj.coll.spi.ad#ISettleable,ISettleable>>`
    or `ISnapshotable`
  - transient collections are mutable collections that implement
    at least `IRed`, `IThreadLocal` and `ISettleable`
  - examples: collections created with `<<edit!,edit!>>`,
    concurrent hash-trie, ``<<dunaj.buffer.api.ad#buffer,Buffer>>``

  [discrete]
  === Collection recipes

  - implement `IRed`
  - do not implement `IPersistentCollecion` or `IMutableCollection`
  - no items stored in memory, recompute for each reduce
  - may give different results for same reduce calls, when e.g.
    connected to a resource, represent a random number generator
    or when the computation they perform takes values from
    some reference or mutable collection.
  - can represent input from a resource, performing I/O
    operations when reduced. Such collections must be
    reduced within <<dunaj.state.api.ad#io!,dunaj.state/io!>> block.
  - examples:
  ** results from reducers
     (`<<dunaj.coll.recipe.api.ad#concat,concat>>`,
     `<<dunaj.coll.recipe.api.ad#append,append>>`, ...)
  ** generators (`<<dunaj.coll.recipe.api.ad#range,range>>`,
     `<<dunaj.coll.recipe.api.ad#repeat,repeat>>`,
     `<<dunaj.coll.recipe.api.ad#cycle,cycle>>`, ...)
  ** input from resources (`<<dunaj.resource.api.ad#read!,read!>>`)
  ****

  Host or third party collections can be easily integrated with
  Dunaj collections by extending `IRed` (and optionally other
  Dunaj collection protocols). Standard host collections are already
  supported, and extensions are available for host interfaces too
  (e.g. http://docs.oracle.com/javase/8/docs/api/java/util/List.html[`j.u.List`], http://docs.oracle.com/javase/8/docs/api/java/util/Collection.html[`j.u.Collection`] on JVM)
  
  A host array is not considered a collection.
  Use `<<dunaj.host.array.api.ad#adapt,dunaj.host.array/adapt>>`
  function to transform array into a Dunaj collection variant.
  
  [NOTE]
  --
  Every collection which implements `IRed` is seqable
  (transformable to `ISeq`) in O(1).
  
  Docstrings mention what kind of collection they expect or provide:
  
  * e.g. if docstring states that a function accepts a collection,
    user can pass any object implementing `IRed`
  * pay attention to what kind of collection a function produces
    (e.g. `<<dunaj.coll.recipe.api.ad#map,map>>`,
    `<<dunaj.coll.recipe.api.ad#take,take>>` or
    `<<dunaj.coll.recipe.api.ad#filter,filter>>`
    all return collection recipes!)
  --"
  {:categories ["Primary" "Seqs" "Features" "Persistent"
                "Mutable" "Factory"]
   :authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer
    [throw extend-protocol satisfies? partition apply assert str meta
     with-meta lazy-seq gensym cons]]
   [clojure.core.protocols :refer [coll-reduce]]
   [clojure.bootstrap :refer
    [defn replace-var! defalias def fn v1 defmacro strip-sigs-vec]]
   [dunaj.type :refer [Fn Any Va Maybe AnyFn U Signature]]
   [dunaj.boolean :refer [Boolean boolean and or not]]
   [dunaj.host :refer [. class class-instance? set! AnyBatch Class]]
   [dunaj.host.int :refer [iinc i0 iint]]
   [dunaj.math :refer [Integer > == zero? odd?]]
   [dunaj.compare :refer [sentinel nil? identical? defsentinel]]
   [dunaj.state :refer [IReference]]
   [dunaj.flow :refer
    [if if-not cond let when-not when loop recur if-let]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [Type deftype defprotocol identical-type?]]))


;;;; Implementation details

(defsentinel nothing)


;;;; Public API

(deftype Postponed
  "A type for postponed reduction. A continuation for reductions.
  Holds an intermediate result, accessible by dereferencing the
  postponed object. Reduction can be continued by calling `advance`
  or `unsafe-advance!` functions."
  {:added v1
   :predicate 'postponed?
   :category "Primary"
   :see '[postponed advance unsafe-advance! Reduced]}
  [ret :- Any, advancef :- AnyFn, uadvancef :- AnyFn]
  IReference
  (-deref [this] ret))

(defn postponed :- Postponed
  "Returns a postponed reference to the intermediate result `_ret_`,
  with `_advancef_` as a function that continues the reduction in a
  safe way (can call `advance` on same object multiple times),
  with `_uadvancef_` as a function that continues the reduction in
  an unsafe way (one call to `unsafe-advance!` per object only),
  and which defaults to `(constantly ret)`. The caller can get the
  intermediate result by dereferencing the returned object.
  The reduction can be continued by calling `advance` or
  `unsafe-advance!` function."
  {:added v1
   :category "Primary"
   :see '[Postponed postponed? advance unsafe-advance!
          unsafe-postponed]}
  ([ret :- Any]
   (let [f (fn [] ret)] (->Postponed ret f f)))
  ([ret :- Any, advancef :- AnyFn, uadvancef :- AnyFn]
   (->Postponed ret advancef uadvancef)))

(defn unsafe-postponed :- Postponed
  "Returns a postponed reference to the intermediate result `_ret_`,
  with `_uadvancef_` as a function that continues the reduction in
  an unsafe way (one call to `unsafe-advance!` per object only),
  and which defaults to `(constantly ret)`. The caller can get the
  intermediate result by dereferencing the returned object.
  The reduction is continued by calling `unsafe-advance!` function."
  {:added v1
   :category "Primary"
   :see '[Postponed postponed? postponed unsafe-advance!]}
  [ret :- Any, uadvancef :- AnyFn]
  (->Postponed
   ret #(throw (java.lang.UnsupportedOperationException.)) uadvancef))

(defn advance :- Any
  "Continues with the reduction of a `_postponed_` result and returns
  the reduced result, applying same rules as in the `reduce`
  function. May again return another postponed object. Usually much
  slower than `unsafe-advance!`, but can be called multiple times
  for same postponed object. May not always be supported."
  {:added v1
   :inline (fn [x] `((.-advancef
                     ~(with-meta x {:tag 'dunaj.coll.Postponed}))))
   :category "Primary"
   :see '[postponed unsafe-advance! dunaj.coll.helper/advance-fn]}
  [postponed :- Postponed]
  ((.-advancef postponed)))

(defn unsafe-advance! :- Any
  "Continues with the reduction of a `_postponed_` result and returns
  the reduced result, applying same rules as in the
  `reduce` function. May again return another postponed object.
  It is not safe to call this function more than once
  on a same postponed object. Use `advance` function for that."
  {:added v1
   :inline (fn [x] `((.-uadvancef
                     ~(with-meta x {:tag 'dunaj.coll.Postponed}))))
   :category "Primary"
   :see '[postponed advance dunaj.coll.helper/advance-fn
          unsafe-postponed]}
  [postponed :- Postponed]
  ((.-uadvancef postponed)))

(deftype Reduced
  "A reduced reference type.
  Signals that reduction should finish early."
  {:added v1
   :predicate 'reduced?
   :category "Primary"
   :see '[reduced]}
  clojure.lang.Reduced
  ;; JVM: following protocols are already implemented
  IReference)

(defalias reduced
  "Returns a reduced object which holds a reference to the `_x_`. Is
  used for early termination of a reduction."
  {:added v1
   :tsig (Fn [Reduced Any])
   :category "Primary"
   :see '[Reduced reduced? dunaj.coll.helper/provide-reduced
         dunaj.coll.helper/strip-reduced]})

(defmacro ^:private advance-fn
  [fname-or-args & cond-body]
  (let [fname? (not (clojure.core/vector? fname-or-args))
        fname (if fname? fname-or-args (gensym "fname"))
        args (if fname? (clojure.core/first cond-body) fname-or-args)
        cond-body (if fname? (clojure.core/rest cond-body) cond-body)
        cond-body (if (odd? (clojure.core/count cond-body))
                    (cons :else cond-body)
                    cond-body)
        ret (clojure.core/first args)
        rargs (strip-sigs-vec (clojure.core/rest args))]
    `(clojure.bootstrap/fn ~fname ~args
       (dunaj.flow/cond
        (reduced? ~ret) ~ret
        (postponed? ~ret)
        (postponed (dunaj.state/deref ~ret)
                   #(~fname (advance ~ret) ~@rargs)
                   #(~fname (unsafe-advance! ~ret) ~@rargs))
        ~@cond-body))))

;;; Reducible collections

(defprotocol IRed
  "A value protocol for reducible collections."
  {:added v1
   :predicate 'red?
   :category "Primary"
   :see '[IBatchedRed IUnpackedRed reduce]}
  (-reduce :- Any
    "Returns the reduction of `_this_` with a reducing function
    `_reducef_` and with a starting value `_init_`, or returns
    a postponed object, if the reduction was postponed either
    by the reducef or by the collection itself. May return reduced
    object. Implementations must correctly handle reduced and
    postponed references and care must be taken to correctly handle
    postponed results returned from the underlying collection,
    if the implementation has one."
    [this reducef :- AnyFn, init :- Any]))

(defprotocol IBatchedRed
  "A value protocol for batchable collections, which reduce in batches
  to optimize data processing. Note that reducing function
  `_reducef_` must accept batch instead of just one value.
  Batchable collections must also implement `IHomogeneous`."
  {:added v1
   :category "Primary"
   :see '[IRed IUnpackedRed dunaj.coll.util/reduce-batched
          dunaj.coll.util/batched dunaj.coll.helper/reduce-batched*
          ensure-batchable]}
  (-reduce-batched :- Any
    "Returns the reduction of sequence of batches with reducing
    function `_reducef_` and with a starting value `_init_`.
    Batch size may be hinted with `_size-hint_`.
    Batch must be of type `_item-type_`. If `_item-type_` is `nil`,
    collection uses its default type.
    May return postponed or reduced object.
    Implementations must correctly handle reduced and postponed
    references and care must be taken to correctly handle
    postponed results returned from underlying collection,
    if the implementation has one."
    [this item-type :- (U nil Class Type),
     size-hint :- (Maybe Integer), reducef :- (Fn [Any Any AnyBatch]),
     init :- Any]))

(defn ensure-batchable :- IBatchedRed
  "Returns `_coll_`, throwing if it is not batchable."
  {:added v1
   :category "Primary"
   :see '[ensure-unpackable]}
  [coll :- Any]
  (when-not (satisfies? IBatchedRed coll)
    (throw (java.lang.IllegalArgumentException.)))
  coll)

(defprotocol IUnpackedRed
  "A value protocol for multireducible collections.
  Multireducible collections can reduce a multiarg function
  (more than 2 args), instead of sending multiple values as a
  vector. Is supported for multireducibles, indexed reducibles
  and maps."
  {:added v1
   :category "Primary"
   :see '[IRed dunaj.coll.util/reduce-unpacked
          dunaj.coll.util/unpacked dunaj.coll.helper/reduce-unpacked*
          ensure-unpackable]}
  (-reduce-unpacked :- Any
    "Returns the reduction of `_this_` where multiple values are
    sent directly into reducing function `_reducef_` instead of
    creating a temporary vector to send multiple values.
    May return postponed or reduced object.
    Implementations must correctly handle reduced and postponed
    references and care must be taken to correctly handle
    postponed results returned from underlying collection,
    if the implementation has one."
    [this reducef :- AnyFn, init :- Any]))

(defn ensure-unpackable :- IUnpackedRed
  "Returns `_coll_`, throwing if it is not unpackable."
  {:added v1
   :category "Primary"
   :see '[ensure-batchable]}
  [coll :- Any]
  (when-not (satisfies? IUnpackedRed coll)
    (throw (java.lang.IllegalArgumentException.)))
  coll)

(defn ^:private strip-reduced :- Any
  [x :- Any]
  (if (reduced? x) @x x))

(defn ^:private reduce* :- Any
  [coll :- [], reducef :- AnyFn, init :- Any]
  (if (nil? coll) init (-reduce coll reducef init)))

(defn ^:private reduce-unpacked* :- Any
  [coll :- [], reducef :- AnyFn, init :- Any]
  (if (nil? coll) init (-reduce-unpacked coll reducef init)))

(defn ^:private reduce-batched* :- Any
  ([coll :- [], reducef :- AnyFn, init :- Any]
   (if (nil? coll) init (-reduce-batched coll nil nil reducef init)))
  ([item-type :- (U nil Class Type), size-hint :- (Maybe Integer),
    coll :- [], reducef :- AnyFn, init :- Any]
   (if (nil? coll)
       init
       (-reduce-batched coll item-type size-hint reducef init))))

(defn reduce :- Any
  "Returns the reduction of `_coll_` with a reducing function
  `_reducef_` and with a starting value `_init_`, or returns an
  `Postponed` reference holding the intermediate result,
  if the reduction was postponed either from the `_reducef_`
  or by the collection itself. The reduction can then be continued
  by calling `advance` function on the postponed reference.

  Reduction process may be controlled in `_reducef_` by returning
  `reduced` or `postponed` references.
  If `_init_` is not supplied, `(reducef)` is used instead.
  In either case, the initial value may not be a postponed or
  reduced object.

  Reducing function `_reducef_` should behave differently based
  on number of arguments:

  * `(reducef)` - returns identity value. Used only if initial value
                  `_init_` is not provided by the caller.
  * `(reducef ret val)` - applies `val` on the `ret` result of
                          a previous reduction step.
  * `(reducef ret val & more)` - used in multireducibles, applies
    `val` and all other arguments into `ret`. Number of required
    arguments is specified by the multireducible collection
    `_coll_`."
  {:added v1
   :category "Primary"
   :see '[reduce-augmented dunaj.coll.helper/reduce*
          dunaj.concurrent.forkjoin/fold
          dunaj.coll.util/reduce-unpacked
          dunaj.coll.util/reduce-batched]}
  ([reducef :- AnyFn, coll :- []]
     (reduce reducef (reducef) coll))
  ([reducef :- AnyFn, init :- Any, coll :- []]
     (strip-reduced (reduce* coll reducef init))))

(defn reduce-orig :- Any
  ([f :- AnyFn, coll :- Any]
     ;; class instance as we want to preseve c.c/reduce semantics
     ;; with c.l collection classes
     (if-not (class-instance? dunaj.coll.IRed coll)
       (coll-reduce coll f)
       (let [so (sentinel)
             delayed-fn #(if (identical? so %1) %2 (f %1 %2))
             res (strip-reduced (-reduce coll delayed-fn so))]
         ;; must avoid iterating twice over the coll
         (strip-reduced (if (identical? so res) (f) res)))))
  ([f :- AnyFn, init :- Any, coll :- Any]
     (if (class-instance? dunaj.coll.IRed coll)
       (strip-reduced (-reduce coll f init))
       (coll-reduce coll f init))))

;; Replace original reduce so that the old code will work
;; with new collections
(replace-var! clojure.core/reduce reduce-orig)

(defn provide-collection :- IRed
  "Returns `_x_` if it is a collection,
  otherwise returns a list containing `_x_`."
  {:added v1
   :category "Primary"
   :see '[red? provide-sequential]}
  [x :- Any]
  (cond (red? x) x
        (nil? x) clojure.lang.PersistentList/EMPTY
        :else (clojure.lang.PersistentList. x)))

;;; Transducers

(def Transducer :- Signature
  "A type signature for transducers."
  {:added v1
   :category "Primary"}
  Any)

(defprotocol IReducing
  "A value protocol for augmented reducing functions."
  {:added v1
   :see '[reduce-augmented reducing-function reducing]
   :forbid-extensions true
   :category "Primary"}
  (-init :- Any
    "Returns default initial unwrapped value.
    May NOT return reduced or postponed object."
    [this])
  (-finish :- Any
    "Returns final unwrapped result, and performs flushing or cleaning
    of internal state. May return reduced or postponed object.
    Input is not postponed but may be reduced, which signals that
    finalization of early terminated reduction should be performed.
    Implementation should return reduced object at least when
    `_wrap_` is reduced."
    [this wrap :- Any])
  (-wrap :- Any
    "Returns wrapped result `_ret_`. Initializes reducing state.
    May NOT return reduced or postponed object.
    Inputs are not reduced nor postponed."
    [this ret :- Any])
  (-unwrap :- Any
    "Returns unwrapped result `_ret_`.
    May NOT return reduced or postponed object.
    Inputs are not reduced nor postponed."
    [this wrap :- Any])
  (-step :- Any
    "Returns new wrapped result based on applying given input values
    to the `_wrap_`. May return reduced or postponed object.
    Inputs are not reduced nor postponed."
    [this wrap :- Any, val :- Any]
    [this wrap :- Any, val :- Any, val2 :- Any]
    [this wrap :- Any, val :- Any, val2 :- Any, val3 :- Any]
    [this wrap :- Any, val :- Any, val2 :- Any, val3 :- Any,
     val4 :- Any]
    [this wrap :- Any, val :- Any, val2 :- Any, val3 :- Any,
     val4 :- Any, more :- [Any]])
  (-combine :- Any
    "Returns wrapped result combined from `_wrap_` and `_other_`.
    May NOT return reduced or postponed object.
    Inputs will not be finished and are not reduced nor postponed.
    May NOT return reduced or postponed object."
    [this wrap :- Any, other :- Any]))

(deftype BareReducing
  "An augmented reducing type that uses `_reducef_` for reducing
  steps and for an initial value, without fold support."
  [reducef :- AnyFn]
  IReducing
  (-init [this] (reducef))
  (-finish [this wrap] wrap)
  (-wrap [this ret] ret)
  (-unwrap [this wrap] wrap)
  (-step [this wrap val] (reducef wrap val))
  (-step [this wrap val val2] (reducef wrap val val2))
  (-step [this wrap val val2 val3] (reducef wrap val val2 val3))
  (-step [this wrap val val2 val3 val4]
    (reducef wrap val val2 val3 val4))
  (-step [this wrap val val2 val3 val4 more]
    (apply reducef wrap val val2 val3 val4 more)))

(deftype Reducing
  "An augmented reducing type which uses `_reducef_` for reducing
  steps and `_init_` for an initial value, without fold support."
  [reducef :- AnyFn, init :- Any]
  IReducing
  (-init [this] init)
  (-finish [this wrap] wrap)
  (-wrap [this ret] ret)
  (-unwrap [this wrap] wrap)
  (-step [this wrap val] (reducef wrap val))
  (-step [this wrap val val2] (reducef wrap val val2))
  (-step [this wrap val val2 val3] (reducef wrap val val2 val3))
  (-step [this wrap val val2 val3 val4]
    (reducef wrap val val2 val3 val4))
  (-step [this wrap val val2 val3 val4 more]
    (apply reducef wrap val val2 val3 val4 more)))

(defn reducing :- IReducing
  "Returns an augmented reducing function which uses `_reducef_` for
  reducing steps and `_init_` for an initial value, without fold
  support. Uses `(reducef)` as an initial value if `_init_` is not
  supplied."
  {:added v1
   :see '[dunaj.coll.helper/reducing-function
          dunaj.concurrent.forkjoin/folding
          dunaj.coll.helper/defreducing
          dunaj.coll.helper/defxform]
   :category "Primary"}
  ([reducef :- AnyFn]
   (->BareReducing reducef))
  ([reducef :- AnyFn, init :- Any]
   (->Reducing reducef init)))

(defn ^:private reducing-function :- AnyFn
  "Returns a reducing function created from augmented reducing
  function `_r_`."
  [r :- IReducing]
  (fn
    ([] (._init r))
    ([wrap] (._finish r wrap))
    ([wrap val] (._step r wrap val))
    ([wrap val val2] (._step r wrap val val2))
    ([wrap val val2 val3] (._step r wrap val val2 val3))
    ([wrap val val2 val3 val4] (._step r wrap val val2 val3 val4))
    ([wrap val val2 val3 val4 & more]
     (._step r wrap val val2 val3 val4 more))))

(defn ^:private finish-advance :- Any
  [ret :- Any, r :- IReducing]
  (if (postponed? ret)
    (postponed (._unwrap r @ret)
               #(finish-advance (advance ret) r)
               #(finish-advance (unsafe-advance! ret) r))
    (._finish r ret)))

(defn ^:private reduced-advance :- Any
  ([ret :- Any]
   (cond (reduced? ret) ret
         (postponed? ret)
         (postponed @ret
                    #(reduced-advance (advance ret))
                    #(reduced-advance (unsafe-advance! ret)))
         :else (reduced ret)))
  ([ret :- Any, was-reduced? :- Boolean]
   (cond (reduced? ret) ret
         (postponed? ret)
         (postponed
          @ret
          #(reduced-advance (advance ret) was-reduced?)
          #(reduced-advance (unsafe-advance! ret) was-reduced?))
         was-reduced? (reduced ret)
         :else ret)))

(defn ^:private unwrap-advance :- Any
  [wrap :- Any, r :- IReducing]
  (cond (reduced? wrap) (reduced (._unwrap r @wrap))
        (postponed? wrap)
        (postponed (._unwrap r @wrap)
                   #(unwrap-advance (advance wrap) r)
                   #(unwrap-advance (unsafe-advance! wrap) r))
        :else (._unwrap r wrap)))

(defn ^:private reduce-augmented*
  "Returns a result of the reduction of `_coll_` with `_reduce-fn_`
  function taking `coll reducef init` in that order, and with the
  augmented reducing function `_r_` and with initial value `_init_`,
  which defaults to `(-init r)`. May return reduced object."
  ([coll :- IRed, reduce-fn :- AnyFn, r :- IReducing]
     (reduce-augmented* coll reduce-fn r (._init r)))
  ([coll :- IRed, reduce-fn :- AnyFn, r :- IReducing, init :- Any]
     (-> (reduce-fn coll (reducing-function r) (._wrap r init))
         (finish-advance r))))

(defn reduce-augmented :- Any
  "Returns a result of the reduction of `_coll_` with the augmented
  reducing function `_r_` and with initial value `_init_`, which
  defaults to `(-init r)`. May return postponed object."
  {:added v1
   :see '[reduce-one-augmented reduce transduce
          dunaj.coll.helper/reduce-augmented*]
   :category "Primary"}
  ([r :- IReducing, coll :- IRed]
     (strip-reduced (reduce-augmented* coll reduce* r (._init r))))
  ([r :- IReducing, init :- Any, coll :- IRed]
     (strip-reduced (reduce-augmented* coll reduce* r init))))

(defn reduce-one-augmented :- Any
  "Returns a result of the reduction of one `_item_` with
  the augmented reducing function `_r_` and with initial value
  `_init_`, which defaults to `(-init r)`.
  May return postponed object."
  {:added v1
   :see '[reduce-augmented reduce transduce
          dunaj.coll.helper/reduce-augmented*]
   :category "Primary"}
  ([r :- IReducing, item :- Any]
     (reduce-one-augmented r (._init r) item))
  ([r :- IReducing, init :- Any, item :- Any]
     (-> (._step r (._wrap r init) item)
         (finish-advance r)
         strip-reduced)))

(defn ^:private transduce* :- Any
  "Returns a result of the reduction of `_coll_` with `_reduce-fn_`
  function and with the classic reducing function `_reducef_`,
  initial value `_init_` (which defaults to `(_reducef_)` if not
  provided), and transducer `_xform_`.
  May return a reduced or postponed result."
  ([coll :- IRed, reduce-fn, :- AnyFn,
    xform :- AnyFn, reducef :- AnyFn]
     (reduce-augmented* coll reduce-fn (xform (reducing reducef))))
  ([coll :- IRed, reduce-fn :- AnyFn,
    xform :- AnyFn, reducef :- AnyFn, init :- Any]
     (reduce-augmented* coll reduce-fn
                        (xform (reducing reducef init)))))

(defn transduce :- Any
  "Returns a result of the reduction of `_coll_` with the classic
  reducing function `_reducef_`, initial value `_init_`
  (which defaults to `(_reducef_)` if not provided),
  and transducer `_xform_`. May return a postponed result."
  {:added v1
   :see '[reduce-augmented transduce-one dunaj.coll.helper/transduce*
          dunaj.coll.helper/defxform]
   :category "Primary"}
  ([xform :- AnyFn, reducef :- AnyFn, coll :- IRed]
     (reduce-augmented (xform (reducing reducef)) coll))
  ([xform :- AnyFn, reducef :- AnyFn, init :- Any, coll :- IRed]
     (reduce-augmented (xform (reducing reducef init)) coll)))

(defn transduce-one :- Any
  "Returns a result of the reduction of one `_item_` with the classic
  reducing function `_reducef_`, initial value `_init_`
  (which defaults to `(_reducef_)` if not provided,
  and transducer `_xform_`. May return a postponed result."
  {:added v1
   :see '[reduce-augmented transduce dunaj.coll.helper/transduce*
          dunaj.coll.helper/defxform]
   :category "Primary"}
  ([xform :- AnyFn, reducef :- AnyFn, item :- Any]
     (reduce-one-augmented (xform (reducing reducef)) item))
  ([xform :- AnyFn, reducef :- AnyFn, init :- Any, item :- Any]
     (reduce-one-augmented (xform (reducing reducef init)) item)))

;;; Seq

(defprotocol ISeq
  "An abstract type value protocol for sequences.
  For backwards compatibility with Clojure, implementations should
  also implement `c.l.ISeq/first` and `c.l.ISeq/next` methods.

  Note for implementers: If seq can be empty, it is important
  to check that calling `seq` on an empty seq object returns `nil`."
  {:added v1
   :see '[rest]
   :predicate 'seq?
   :on-interface clojure.lang.ISeq
   :forbid-extensions true
   :category "Seqs"}
  (-rest :- ISeq
    "Returns a possibly empty sequence without first item."
    {:on 'more}
    [this]))

(defprotocol ISeqable
  "A value protocol for collections that can be transformed to seqs
  which are faster than ones created with a default IRed -> ISeq
  transformation."
  {:added v1
   :see '[seq]
   :on-interface clojure.lang.Seqable
   :forbid-extensions true
   :category "Seqs"}
  (-seq :- (Maybe ISeq)
    "Returns a seq representing `_this_` collection or `nil`,
    if `_this_` is empty."
    {:on 'seq}
    [this]))

;; Seq is patched in Clojure sources to support transformations of
;; IRed objects
(defalias seq
  "Returns a seq on the collection `_coll_`. If the collection
  is empty, returns `nil`. `(seq nil)` returns `nil`.
  seq works on any collection."
  {:added v1
   :tsig (Fn [nil nil] [(Maybe ISeq) IRed])
   :see '[rest reduce next dunaj.coll.util/sequence]
   :category "Seqs"})

(defn ^:private ^clojure.lang.ISeq red->seq*
  "Returns a lazy seq from a given one-step intermediate."
  [ret]
  (if (class-instance? dunaj.coll.Postponed ret)
    (clojure.lang.Cons.
     (.-ret ^dunaj.coll.Postponed ret)
     (lazy-seq (red->seq* ((.-uadvancef ^dunaj.coll.Postponed ret)))))
    clojure.lang.PersistentList/EMPTY))

(defn first :- Any
  "Returns the first item in the collection `_coll_`, or `_not-found_`
  (defaults to `nil`) if `_coll_` is `nil` or empty."
  {:added v1
   :see '[second ffirst rest next]
   :category "Primary"}
  ([coll :- []]
   (reduce #(reduced %2) nil coll))
  ([coll :- [], not-found :- Any]
   (reduce #(reduced %2) not-found coll)))

(defn ffirst :- Any
  "Returns the first item of the first item in the collection
  `_coll_`, or `_not-found_` (defaults to `nil`) if `_coll_` is `nil`
  or empty."
  {:added v1
   :see '[first second seq rest]
   :category "Primary"}
  ([coll :- []]
     (first (first coll)))
  ([coll :- [], not-found :- Any]
     (first (first coll) not-found)))

(defn second :- Any
  "Returns the second item of the collection `_coll_`, or `nil` if
  `_coll_` is `nil` or has less than two items."
  {:added v1
   :see '[first rest next seq]
   :category "Primary"}
  [coll :- []]
  (let [rf #(when-not (identical? nothing %) (reduced %2))
        ret (reduce rf nothing coll)]
    (when-not (identical? nothing ret) ret)))

(defalias rest
  "Returns a possibly empty seq of the items after the first.
  Calls `seq` on `_coll_`."
  {:added v1
   :see '[next nthrest first]
   :tsig (Fn [ISeq []])
   :category "Seqs"})

(defn next :- (Maybe ISeq)
  "Returns a seq of the items after the first. Calls `seq` on
  `_coll_`. If there are no more items, returns `nil`."
  {:added v1
   :see '[rest nthnext first]
   :category "Seqs"}
  [coll :- []]
  (seq (rest coll)))

(defn nnext :- (Maybe ISeq)
  "Returns a seq of the items after the second. Calls seq on `_coll_`.
  If there are no more items, returns `nil`."
  {:added v1
   :see '[rest next nthnext]
   :category "Seqs"}
  [coll :- []]
  (next (rest coll)))

(defalias nthnext
  "Returns the `_n_`-th next of coll, or returns `(seq _coll_)`
  when `_n_` is 0."
  {:added v1
   :see '[nthrest next nnext first seq]
   :tsig (Fn [(Maybe ISeq) [] Integer])
   :category "Seqs"})

(defalias nthrest
  "Returns the `_n_`-th rest of coll, or returns `_coll_` when
  `_n_` is 0."
  {:added v1
   :see '[nthnext rest first seq]
   :tsig (Fn [ISeq [] Integer])
   :category "Seqs"})

(defmacro when-first
  "`bindings => x xs`

  Roughly the same as `(when (seq xs) (let [x (first xs)] body))`
  but `xs` is evaluated only once."
  {:added v1
   :see '[first]
   :category "Primary"}
  [bindings & body]
  (let [[x xs] bindings]
    `(let [nothing# (dunaj.compare/sentinel)
           xs# (dunaj.coll/first ~xs nothing#)]
       (clojure.core/when-not (dunaj.compare/identical? nothing# xs#)
         (let [~x xs#] ~@body)))))

;; extend IRed on ISeq for a fallback reduce
(extend-protocol IRed
  clojure.lang.IRecord
  (-reduce [this reducef init]
    (-reduce (seq this) reducef init))
  clojure.lang.ISeq
  (-reduce [this reducef init]
    ;;(clojure.core/println "fallback reduce" (class this))
    (if-not (seq this)
      init
      (let [af (advance-fn [ret :- Any, s :- ISeq]
                 (let [x (.next s)]
                   (if (identical-type? x s)
                     (recur (reducef ret (.first x)) x)
                     (reduce* x reducef ret))))]
        (af (reducef init (.first this)) this)))))

;;; Counted collections

(defprotocol ICounted
  "A value protocol for counted collections."
  {:added v1
   :predicate 'counted?
   :see '[count full-aware? empty-aware?]
   :on-interface clojure.lang.Counted
   :category "Features"}
  (-count :- Integer
    "Returns the size of `_this_`, in constant time."
    {:on 'count}
    [this]))

(defn count :- Integer
  "Returns the size of a collection `_coll_`."
  {:added v1
   :see '[empty? full? counted? several? single?]
   :category "Features"}
  [coll :- []]
  (cond (class-instance? clojure.lang.Counted coll)
        (.count ^clojure.lang.Counted coll)
        (satisfies? ICounted coll) (-count coll)
        ;; reduce is faster than c.c.count on seqs
        :else (reduce (fn [ret _] (iinc ret)) (i0) coll)))

;; c.c.count is patched to support ICounted extensions in c.l.RT
;; Support ICounted in old code, slower but supports protocol
;; extensions
(replace-var! clojure.core/counted?)

(defn several? :- Boolean
  "Returns `true` if `_coll_` contains more than one item, returns
  `false` otherwise."
  {:added v1
   :see '[count single? empty?]
   :category "Features"}
  [coll :- []]
  (> (count coll) 1))

(defn single? :- Boolean
  "Returns `true` if `_coll_` contains exactly one item,
  returns `false` otherwise."
  {:added v1
   :see '[count several? double? empty?]
   :category "Features"}
  [coll :- []]
  (== (count coll) 1))

(defn double? :- Boolean
  "Returns `true` if `_coll_` contains exactly two items,
  returns `false` otherwise."
  {:added v1
   :see '[count several? single? triple? empty?]
   :category "Features"}
  [coll :- []]
  (== (count coll) 2))

(defn triple? :- Boolean
  "Returns `true` if `_coll_` contains exactly three items,
  returns `false` otherwise."
  {:added v1
   :see '[count several? single? double? quad? empty?]
   :category "Features"}
  [coll :- []]
  (== (count coll) 3))

(defn quad? :- Boolean
  "Returns `true` if `_coll_` contains exactly four items,
  returns `false` otherwise."
  {:added v1
   :see '[count several? single? triple? quint? empty?]
   :category "Features"}
  [coll :- []]
  (== (count coll) 4))

(defn quint? :- Boolean
  "Returns `true` if `_coll_` contains exactly five items,
  returns `false` otherwise."
  {:added v1
   :see '[count several? single? quad? empty?]
   :category "Features"}
  [coll :- IRed]
  (== (count coll) 5))

;;; Full collections

(defprotocol IFullAware
  "A value protocol for collections which can be full."
  {:added v1
   :see '[capped?]
   :category "Features"
   :predicate 'full-aware?}
  (-full? :- Boolean
    "Returns `true` if `_this_` is full."
    [this]))

(defn full? :- Boolean
  "Returns `true` if `_coll_` satisfies `IFullAware` protocol and is
  full, otherwise returns `false`. Full collections (usually)
  don't accept new items and they throw on `conj`."
  {:added v1
   :see '[brimming? count full-aware? capacity]
   :category "Features"}
  [coll :- Any]
  (and (full-aware? coll) (-full? coll)))

(defprotocol ICapped
  "A value protocol for collections that have a capacity."
  {:added v1
   :see '[capacity brimming?]
   :category "Features"
   :predicate 'capped?}
  (-capacity :- Integer
    "Returns capacity of a given `_this_`."
    [this]))

(defn capacity :- Integer
  "Returns capacity of a given capped `_coll_`."
  {:added v1
   :see '[brimming? count capped? full?]
   :category "Features"}
  [coll :- ICapped]
  (-capacity coll))

(defn brimming? :- Boolean
  "Returns `true` if `_coll_` is capped and the number of items in it
  equals to its capacity, otherwise returns `false`. Note that this
  is not equal to calling `full?`, as `full?` additionally
  indicates that collection does not accepts any more items.
  Conjoining a brimming collection is dependent on the collection
  type. Examples include throwing (fixed buffer), ignoring the new
  item (dropping buffer), dropping oldest item (sliding buffer) or
  increase its capacity (transient string)."
  {:added v1
   :see '[full? capacity count]
   :category "Features"}
  [coll :- []]
  (and (capped? coll) (== (capacity coll) (count coll))))

;;; Empty collections

(defprotocol IEmptyAware
  "A value protocol for collections that can be empty."
  {:added v1
   :see '[empty?]}
  (-empty? :- Boolean
    "Returns `true` if `_this_` is `nil` or has no items."
    [this]))

(defn empty? :- Boolean
  "Returns `true` if `_coll_` is `nil` or has no items, returns
  `false` otherwise."
  {:added v1
   :see '[full? brimming? not-empty? empty not-empty]
   :category "Features"}
  [coll :- []]
  (or (nil? coll) (-empty? coll)))

(extend-protocol IEmptyAware
  java.lang.Object
  (-empty? [this]
    (if (class-instance? clojure.lang.Counted this)
      (zero? (count this))
      (not (seq this)))))

;; Support IEmptyAware in old code
(replace-var! clojure.core/empty?)

(defn not-empty? :- Boolean
  "Returns `true` if `_coll_` is not empty, returns `false`
  otherwise."
  {:added v1
   :see '[full? brimming? empty? empty not-empty]
   :category "Features"}
  [coll :- []]
  (not (empty? coll)))

(defprotocol IEmptyable
  "A value protocol for collections that can return their empty
  version."
  {:added v1
   :see '[empty]
   :category "Features"
   :predicate 'emptyable?
   :on-interface clojure.lang.IPersistentCollection
   :distinct true}
  (-empty :- IRed
    "Returns the empty collection of same category as `_this_`.
    Metadata must be preserved."
    {:on 'empty}
    [this]))

(defn empty :- []
  "Returns an empty collection of the same category as `_coll_`,
  or `nil` if `_coll_` is not emptyable. Metadata is preserved."
  {:added v1
   :see '[full? brimming? not-empty? empty? not-empty emptyable?]
   :category "Features"}
  [coll :- []]
  (when-not (nil? coll) (-empty coll)))

(extend-protocol IEmptyable
  java.lang.Object
  (-empty [this] nil))

(defn not-empty :- []
  "If `_coll_` is empty, returns `nil`, else `_coll_`."
  {:added v1
   :see '[full? brimming? not-empty? empty? empty emptyable?]
   :category "Features"}
  [coll :- []]
  (when-not (empty? coll) coll))

;;; Peekable collections

;; NB: Separation from PersistentStack is mainly because
;;     MutableStack implementations should also support -peek
(defprotocol IPeekable
  "A value protocol for peekable collections."
  {:added v1
   :see '[peek]
   :category "Features"
   :predicate 'peekable?
   :on-interface clojure.lang.IPersistentStack
   :distinct true}
  (-peek :- Any
    "Returns item from a collection at a position specified
    by the implementation. It may be the first item (Lists, Queues),
    or it may be the last item (Vector). Peeking an empty collection
    should return `nil`."
    {:on 'peek}
    [this]))

(defn peek :- Any
  "Returns item from a collection `_coll_` at a position specified
  by the implementation. It may be the first item (Lists, Queues),
  or it may be the last item (Vector). Returns `nil` if `_coll_` is
  `nil` or empty."
  {:added v1
   :see '[first dunaj.coll.util/last pop]
   :category "Features"}
  [coll :- (Maybe IPeekable)]
  (when coll (-peek coll)))

;; Support IPeekable in old code
(replace-var! clojure.core/peek)

;;; Collections with lookup

(defprotocol ILookup
  "A value protocol for collections with lookup. Lookup collections
  implement fast access to their items based on a key.
  For backwards compatibility with Clojure, implementations should
  also implement both `c.l.ILookup/valAt` methods."
  {:added v1
   :see '[contains? get]
   :category "Features"
   :predicate 'lookup?}
  (-contains? :- Boolean
    "Returns `true` if `_this_` collection contains an entry with a
    given `_key_`."
    [this key :- Any])
  (-get :- Any
    "Returns value for a given `_key_`, or returns `_not-found_`
    if item is not found. Returns collection that implements
    `ICounted`, if there are multiple values for a given key
    (e.g. in multimaps or bags)."
    [this key :- Any, not-found :- Any]))

(defn contains? :- Boolean
  "Returns `true` if `_coll_` contains entry with a given `_key_`.
  Returns `false` if `_coll_` is `nil`."
  {:added v1
   :see '[get lookup? nth]
   :category "Features"}
  [coll :- (Maybe ILookup), key :- Any]
  (if (nil? coll) false (boolean (-contains? coll key))))

(defn get :- Any
  "Returns item for a given `_key_`, or returns `_not-found_` if item
  is not found. Returns `_not-found_` also if `_coll_` does not
  support `ILookup`. If `_not-found_` is not supplied and item is not
  found, returns `nil`. Returns collection implementing `ICounted`,
  if there are multiple values for a given key
  (e.g. in multimap or bag)."
  {:added v1
   :see '[contains? lookup? nth get-in]
   :category "Features"}
  ([coll :- Any, key :- Any]
   (get coll key nil))
  ([coll :- Any, key :- Any, not-found :- Any]
   (if (nil? coll) not-found (-get coll key not-found))))

(extend-protocol ILookup
  java.lang.Object
  (-get [this key not-found] not-found)
  clojure.lang.IRecord
  (-contains? [this key] (clojure.core/contains? this key))
  (-get [this key not-found] (clojure.core/get this key not-found)))

;; c.c.contains? is patched to support ILookup in c.l.RT
;; c.c.get is patched to support ILookup in c.l.RT

(defalias get-in
  "Returns the value in a nested associative structure,
  where `_ks_` is a sequence of keys. Returns `nil` if the key
  is not present, or the `_not-found_` value if supplied."
  {:added v1
   :see '[get nth contains? lookup?]
   :category "Features"
   :tsig (Fn [Any Any [Any]]
             [Any Any [Any] Any])})

;;; Indexed collections

(defprotocol IIndexed
  "A value protocol for indexed collections. Indexed collections
  implement fast access to the nth item with integer index.
  For backwards compatibility with Clojure, implementations should
  also implement 2 args `c.l.Indexed/nth` method."
  {:added v1
   :see '[nth]
   :category "Features"
   :predicate 'indexed?
   :on-interface clojure.lang.Indexed}
  (-nth :- Any
    "Returns item at `_index_` position, or returns `_not-found_`
    if position is out of bounds."
    {:on 'nth}
    [this index :- Integer not-found :- Any]))

(defn nth :- Any
  "Returns item at `_index_` position, or returns `_not-found_`
  if position is out of bounds. Throws an exception if `_not-found_`
  is not supplied, `_coll_` is not `nil` and item is not found.
  Returns `nil` (or `_not-found_`) if `_coll_` is `nil`."
  {:added v1
   :see '[get get-in contains? indexed?]
   :category "Features"}
  ([coll :- [], index :- Integer]
     (when-not (nil? coll)
       (let [nothing (sentinel)
             r (nth coll index nothing)]
         (if (identical? r nothing)
           (throw (java.lang.IndexOutOfBoundsException.))
           r))))
  ([coll :- [], index :- Integer, not-found :- Any]
     (if (nil? coll) not-found (-nth coll index not-found))))

;; c.c.nth is patched to support ILookup in c.l.RT

;;; Invertible collections

(defprotocol IInvertible
  "A value protocol for invertible collections. Example of
  invertible collection is set or bimap."
  {:added v1
   :see '[invert]
   :category "Features"
   :predicate 'invertible?}
  (-invert :- IInvertible
    "Returns inverted collection from `_this_`."
    [this]))

(defn invert :- IInvertible
  "Returns inverted collection from `_coll_` in better than linear
  time. Example of such collection is set or bimap."
  {:added v1
   :see '[reverse flip dunaj.coll.util/map-invert]
   :category "Features"}
  [coll :- IInvertible]
  (-invert coll))

;; Notes on reversing a collection
;; flip    - returns collection of same type with reverse order of
;;           items, better than linear time. Rarely supported.
;; reverse - returns seqable and reducible collection with reverse
;;           order of items, constant time. (old c.c/rseq)
;; revlist - returns persistent list containing items from
;;           coll in a reverse order of items. (old c.c/reverse)
;; revlist is the most expensive, use it as the last resort

;;; Flipable collections

(defprotocol IFlippable
  "A value protocol for flipable collections."
  {:added v1
   :see '[flip]
   :category "Features"
   :predicate 'flippable?}
  (-flip :- IFlippable
    "Returns flipped collection from `_this_`, of same type."
    [this]))

(defn flip :- IFlippable
  "Returns flipped collection from `_coll_` in better than linear
  time. Returned collection is of the same type as `_coll_`.
  Throws if collection is not reversible. Should be used
  when collection type has to be preserved, which is not very often.
  Supported mainly in sorted collections.
  Use `reverse` if you are OK with collection of a different type.
  Use `revlist` if any of above fails."
  {:added v1
   :see '[reverse dunaj.coll.util/revlist invert flippable?]
   :category "Features"}
  [coll :- IFlippable]
  (-flip coll))

;;; Reversible collections

(defprotocol IReversible
  "Value protocol for reversible collections.
  For backwards compatibility with Clojure, implementations should
  also implement `c.l.Reversible/rseq` method, which returns
  reversed Seq."
  {:added v1
   :see '[reverse]
   :category "Features"
   :predicate 'reversible?}
  (-reverse :- IRed
    "Returns reversed collection from `_this_` in constant time.
    Does not preserve type, is at least a collection."
    [this]))

(defn reverse :- IRed
  "Returns a collection with reverse order of items,
  in constant time. Throws if `_coll_` is not reversible.
  Use `flip` if you need to preserve type. Use `revlist`
  for collections which are not reversible nor flippable."
  {:added v1
   :see '[flip dunaj.coll.util/revlist invert reversible?]
   :category "Features"}
  [coll :- IReversible]
  (-reverse coll))

;;; Sorted collections

(defprotocol ISorted
  "Value protocol for sorted collections.
  Implementation should place comparator in the configuration.
  For backwards compatibility with Cojure, implementations should
  also implement `c.l.Sorted` interface and its methods."
  {:added v1
   :see '[ascending?]
   :category "Features"
   :predicate 'sorted?}
  (-key :- Any
    "Returns a key (from `_item_`) that is used to sort `_this_`."
    [this item :- Any])
  (-ascending? :- Boolean
    "Returns `true` if `_this_` is sorted in ascending order with
    regards to the comparator used for sorting, otherwise returns
    `false`. Users should use `flip` function to change the order
    of sorting."
    [this]))

(defn ascending? :- Boolean
  "Returns `true` if `_coll_` is sorted in ascending order, otherwise
   returns `false`. Throws if collection is not sorted."
  {:added v1
   :see '[dunaj.coll.util/sort dunaj.coll.util/sort-by sorted?]
   :category "Features"}
  [coll :- ISorted]
  (boolean (-ascending? coll)))

;;; Sliceable

(defprotocol ISliceable
  "Value protocol for sliceable collections."
  {:added v1
   :see '[slice]
   :category "Features"
   :predicate 'sliceable?}
  (-slice :- ISliceable
    "Returns new collection which is the result of slicing `_this_`
    from `_begin_` inclusive into the `_end_` exclusive. `nil` in
    `_begin_` defaults to the first item and `nil` in the `_end_`
    defaults to last item in `_this_`. Resulting coll will be
    of same type and will not hold onto items not belonging to it."
    [this begin :- Any, end :- Any]))

(defn slice :- ISliceable
  "Returns new collection which is the result of slicing `_coll_` from
  `_begin_` inclusive into the `_end_` exclusive. `nil` in `_begin_`
  defaults to the first item and `nil` in the `_end_` defaults to
  the last item in the `_coll_`. Resulting coll will be of same type
  and will not hold onto items not belonging to it.
  Operation is better than linear time."
  {:added v1
   :see '[section sliceable?]
   :category "Features"}
  ([coll :- ISliceable, begin :- Any]
   (-slice coll begin nil))
  ([coll :- ISliceable, begin :- Any, end :- Any]
   (-slice coll begin end)))

;;; Sectionable collections

(defprotocol ISectionable
  "Value protocol for indexed sectionable collections."
  {:added v1
   :see '[section]
   :category "Features"
   :predicate 'sectionable?}
  (-section :- []
    "Returns new collection which is the result of sectioning `_this_`
    from `_begin_` inclusive into the `_end_` exclusive. `nil` in
    `_begin_` defaults to the first item and `nil` in the `_end_`
    defaults to the last item in `_this_`. Resulting coll will be
    of similar type and will share underlying data with `_this_` and
    thus will hold onto all original items. Operation is constant
    time though. Returned collection does not have to be persistent."
    [this begin :- Integer, end :- Integer]))

(defprotocol ISortedSectionable
  "Value protocol for sorted sectionable collections."
  {:added v1
   :see '[section]
   :category "Features"
   :predicate 'sorted-sectionable?}
  (-sorted-section :- []
    "Returns new collection which is the result of sectioning `_this_`
    from `_begin_` inclusive into the `_end_` exclusive. `nil` in
    `_begin_` defaults to the first item and `nil` in the `_end_`
    defaults to the last item in `_this_`. Resulting coll will be of
    similar type and will share underlying data with `_this_` and
    thus will hold onto all original items. Operation is constant
    time though. Returned collection does not have to be persistent."
    [this begin :- Any, end :- Any]))

(defn section :- []
  "Returns new collection which is the result of sectioning `_coll_`
  from `_begin_` inclusive into the `_end_` exclusive. `nil` in
  `_begin_` defaults to the first item and `nil` in the `_end_`
  defaults to the last item in the `_coll_`. Resulting coll will be
  of similar type but will share underlying data with original
  `_coll_` and thus will hold onto all original items. Operation is
  constant time though. `_begin_` and `_end_` may be numbers for
  indexed collections and arbitrary objects for e.g. sorted sets or
  maps. Note that section of some types is not persistent.
  This fn is the replacement for all c.c/subs, subvec, subseq ..."
  {:added v1
   :see '[sectionable? sorted-sectionable? slice]
   :category "Features"}
  ([coll :- ISectionable, begin :- Any]
   (section coll begin nil))
  ([coll :- ISectionable, begin :- Any, end :- Any]
   ;; There are two protocols for sectionable types, because source
   ;; awareness requires indexed sectionable, not the sorted one.
   (if (sectionable? coll)
     (-section coll begin end)
     (-sorted-section coll begin end))))

;;; Homogeneous collections

(defprotocol IHomogeneous
  "A value protocol for homogeneous collections."
  {:added v1
   :see '[item-type]
   :category "Features"
   :predicate 'homogeneous?}
  (-item-type :- (U nil Class Type)
    "Returns type of items in `_this_` homogeneous collection. Returns
    `nil` if `_this_` can produce items of any requested type."
    [this]))

(defn item-type :- (U nil Class Type)
  "Returns type of items in `_coll_` homogeneous collection. Returns
  `nil` if `_coll_` can produce items of any requested type.
  Throws if `_coll_` is not homogeneous."
  {:added v1
   :category "Features"}
  [coll :- IHomogeneous]
  (-item-type coll))

;;; Editable collections

(defprotocol IEditable
  "A value protocol for immutable collections that can be converted
  to mutable ones in a constant time. For backwards compatibility
  with Clojure, implementations should also implement one arg
  `c.l.IEditableCollection/asTransient` method."
  {:added v1
   :category "Features"
   :see '[edit settle!]
   :predicate 'editable?
   :on-interface clojure.lang.IEditableCollection}
  (-edit :- Any
    "Returns the mutable collection based on `_this_`.
    This operation must be (amortized) constant time if `_this_`
    is a persistent collection."
    {:on 'asTransient}
    [this capacity-hint :- (Maybe Integer)]))

;; c.l.IEditableCollection is patched to support two arg version

(defn edit :- Any
  "Returns the mutable collection based on `_coll_`. This operation is
  (amortized) constant time if `_coll_` is a persistent collection.
  Throws if `_coll_` is not editable."
  {:added v1
   :see '[settle! editable?]
   :category "Features"}
  ([coll :- IEditable]
   (-edit coll nil))
  ([coll :- IEditable, capacity-hint :- (Maybe Integer)]
   (-edit coll capacity-hint)))

;;; Mutable collections

(defprotocol ISettleable
  "A mutable collection protocol for settleable collections,
  a mutable collections that can be transformed into immutable
  or persistent variants."
  {:added v1
   :see '[settle!]
   :predicate 'settleable?
   :on-interface clojure.lang.ITransientCollection
   :distinct true
   :category "Mutable"}
  (-settle! :- IRed
    "Returns the immutable (and preferably persistent) version of
    `_this_`. Any subsequent operations on original `_this_` should
    throw. This operation is (amortized) constant time only if
    resulting coll will be a persistent collection. (for other colls,
    there may be normalizing/trimming to size, ...)"
    {:on 'persistent}
    [this]))

(defn settle! :- IRed
  "Returns the immutable version of a mutable `_coll_`.
  Any subsequent operations on original `_coll_` will throw."
  {:added v1
   :see '[edit settleable? snapshot!]
   :category "Mutable"}
  [coll :- ISettleable]
  (-settle! coll))

(defprotocol ISnapshotable
  "A mutable collection protocol for snapshotable collections,
  a concurrent mutable collections that can provide immutable
  or persistent snapshot of its state."
  {:added v1
   :see '[snapshot!]
   :predicate 'snapshotable?
   :category "Mutable"}
  (-snapshot! :- IRed
    "Returns the immutable or persistent snapshot of `_this_`.
    Both `_this_` and the returned object can be further freely
    used."
    [this]))

(defn snapshot! :- IRed
  "Returns the immutable or persistent snapshot of `_coll_`.
  Both `_coll_` and the returned object can be further freely used.
  Mutable snapshots (if supported) are performed with `clone`."
  {:added v1
   :see '[settle! snapshotable? edit dunaj.state/clone]
   :category "Mutable"}
  [coll :- ISnapshotable]
  (-snapshot! coll))

(defprotocol IMutableCollection
  "A collection protocol for mutable collections."
  {:added v1
   :see '[conj!]
   :on-interface clojure.lang.ITransientCollection
   :distinct true}
  (-conj! :- IMutableCollection
    "Returns the modified `_this_` with `_x_` conjoined. Original
    `_this_` will never be used again."
    {:on 'conj}
    [this x :- Any]))

(defn conj! :- IMutableCollection
  "Returns `_coll_` with items conjoined. Mutates `_coll_`. Do not use
  original `_coll_` after this call, but use returned
  collection instead."
  {:added v1
   :see '[conj edit settle! assoc! pop!]
   :category "Mutable"}
  ([coll :- IMutableCollection]
   coll)
  ([coll :- IMutableCollection, x :- Any]
   (-conj! coll x))
  ([coll :- IMutableCollection, x :- Any, y :- Any]
   (-conj! (-conj! coll x) y))
  ([coll :- IMutableCollection, x :- Any, y :- Any, z :- Any]
   (-conj! (-conj! (-conj! coll x) y) z))
  ([coll :- IMutableCollection, x :- Any, y :- Any, z :- Any,
    a :- Any]
   (-conj! (-conj! (-conj! (-conj! coll x) y) z) a))
  ([coll :- IMutableCollection, x :- Any, y :- Any, z :- Any,
    a :- Any & more :- Any]
   (reduce
    conj! (-conj! (-conj! (-conj! (-conj! coll x) y) z) a) more)))

(defprotocol IMutableAssociative
  "A mutable collection protocol for associative collections.
  For backwards compatibility with clojure, implementations may be
  needed to implement `c.l.ITransientVector/assocN` method."
  {:added v1
   :see '[assoc!]
   :on-interface clojure.lang.ITransientAssociative}
  (-assoc! :- IMutableAssociative
    "assoc[iate]. Returns a new collection, that contains the mapping
    of `_key_` to `_val_`. For multimap, `_val_` must be a collection
    and this method replaces all previous vals with those in `_val_`.
    Mutates `_this_`."
    {:on 'assoc}
    [this key :- Any, val :- Any]))

(defn assoc! :- IMutableAssociative
  "When applied to a mutable map, adds mapping of key(s) to val(s).
  When applied to a mutable vector, sets the val at index.
  Note that index must be pass:[<=] `(count vector)`. Do not use
  original `_coll_` after this call, but use returned
  collection instead."
  {:added v1
   :see '[assoc edit settle! conj! pop!]
   :category "Mutable"}
  ([coll :- IMutableAssociative]
   coll)
  ([coll :- IMutableAssociative, key :- Any, val :- Any]
   (-assoc! coll key val))
  ([coll :- IMutableAssociative, key :- Any, val :- Any & kvs :- Any]
   (let [ret (-assoc! coll key val)]
     (if kvs (recur ret (first kvs) (second kvs) (nnext kvs)) ret))))

(defprotocol IMutableStacked
  "A mutable collection protocol for stacked collections."
  {:added v1
   :see '[pop!]
   :on-interface clojure.lang.ITransientVector
   :distinct true}
  (-pop! :- IMutableStacked
    "Returns `_this_` without one item at a position specified by the
    implementation. It may be the first item (Lists, Queues),
    or it may be the last item (Vector). Mutates `_this_`."
    {:on 'pop}
    [this]))

(defn pop! :- IMutableStacked
  "Returns `_coll_` without one item at a position specified by the
  implementation. It may be the first item (Lists, Queues),
  or it may be the last item (Vector). Do not use original `_coll_`
  after this call, but use returned collection instead."
  {:added v1
   :see '[pop edit settle! conj! assoc!]
   :category "Mutable"}
  [coll :- IMutableStacked]
  (-pop! coll))

(defprotocol IMutableCatenable
  "A value protocol for mutable catenable collections."
  {:added v1
   :see '[cat!]}
  (-cat! :- IMutableCatenable
    "Returns the catenation of `_this_` with `_other_` collection of
    same type. Resulting collection is of the same type as `_this_`.
    Mutates `_this_`."
    [this other :- IMutableCatenable]))

(defn cat! :- IMutableCatenable
  "Returns the catenation of `_coll_` with `_other_` collection of
  similar type. Resulting collection is of the same type.
  Operation is faster than linear time. Do not use original `_coll_`
  after this call, but use returned collection instead."
  {:added v1
   :see '[cat edit settle! conj! dunaj.coll.recipe/concat
          dunaj.coll.recipe/mapcat dunaj.coll.recipe/concat*]
   :category "Mutable"}
  [coll :- IMutableCatenable, other :- IMutableCatenable]
  (-cat! coll other))

(defprotocol IMutableSet
  "An abstract type mutable protocol for sets. For backwards
  compatibility with Clojure, implementations should also implement
  both `c.l.ITransientSet/contains` and `c.l.ITransientSet/get`."
  {:added v1
   :see '[disj!]
   :on-interface clojure.lang.ITransientSet
   :forbid-extensions true}
  (-disj! :- IMutableSet
    "Returns the `_this_` with `_key_` removed. For bags, only one
    instance of `_key_` is removed. Mutates `_this_`."
    {:on 'disjoin}
    [this key :- Any]))

(defn disj! :- IMutableSet
  "Returns the `_coll_` with `_key_` removed. For bags, only one
  instance of `_key_` is removed. Do not use original `_coll_` after
  this call, but use returned collection instead."
  {:added v1
   :see '[disj disj-all!]
   :category "Mutable"}
  ([coll :- IMutableSet]
   coll)
  ([coll :- IMutableSet, key :- Any]
   (-disj! coll key))
  ([coll :- IMutableSet, key :- Any & ks :- Any]
   (let [ret (-disj! coll key)]
     (if ks (recur ret (first ks) (next ks)) ret))))

(defprotocol IMutableBag
  "An abstract type mutable protocol for bags."
  {:added v1
   :see '[disj-all!]
   :category :abstract}
  (-disj-all! :- IMutableBag
    "Returns `_this_` with all instances of `_key_` removed.
    Mutates `_this_`."
    [coll key :- Any]))

(defn disj-all! :- IMutableBag
  "Returns the `_coll_` with all instances of `_key_` removed.
  Do not use original `_coll_` after this call, but use returned
  collection instead."
  {:added v1
   :see '[disj! disj-all]
   :category "Mutable"}
  [coll :- IMutableBag, key :- Any]
  (-disj-all! coll key))

(defprotocol IMutableMap
  "An abstract type mutable protocol for maps."
  {:added v1
   :see '[dissoc!]
   :on-interface clojure.lang.ITransientMap
   :forbid-extensions true}
  (-dissoc! :- IMutableMap
    "dissoc[iate]. Returns a new coll that does not contain any
    mapping for key. Mutates `_this_`."
    {:on 'without}
    [this key :- Any]))

(defn dissoc! :- IMutableMap
  "dissoc[iate]. Returns a new coll that does not contain any
  mapping for key. Do not use original `_coll_` after this call,
  but use returned collection instead."
  {:added v1
   :see '[dissoc dissoc-one!]
   :category "Mutable"}
  ([coll :- IMutableMap]
   coll)
  ([coll :- IMutableMap, key :- Any]
   (-dissoc! coll key))
  ([coll :- IMutableMap, key :- Any & ks :- Any]
   (let [ret (-dissoc! coll key)]
     (if ks (recur ret (first ks) (next ks)) ret))))

(defprotocol IMutableMultiMap
  "An abstract type mutable protocol for multimaps."
  {:added v1
   :see '[dissoc-one!]}
  (-dissoc-one! :- IMutableMultiMap
    "dissoc[iate]. Returns a new coll that does not contain mapping
    specified by `_key_` and `_val_`. Mutates `_this_`."
    [this key :- Any, val :- Any]))

(defn dissoc-one! :- IMutableMultiMap
  "dissoc[iate]. Returns a new coll that does not contain mapping
  specified by `_key_` and `_val_`. Do not use original `_coll_`
  after this call, but use returned collection instead."
  {:added v1
   :see '[dissoc-one dissoc!]
   :category "Mutable"}
  [coll :- IMutableMultiMap, key :- Any, val :- Any]
  (-dissoc-one! coll key val))

;;; Sequential

(defprotocol ISequential
  "A value protocol for sequential collections. Sequential collections
  are dunaj collections which main purpose is to serve as an ordered
  collection of items. This precludes e.g. String from being a
  sequential, as string's main purpose is to convey a textual
  information."
  {:added v1
   :see '[sequential?]
   :on-interface clojure.lang.Sequential
   :forbid-extensions true})

(defn sequential? :- Boolean
  "Returns `true` if `_x_` is a sequential collection as defined by
  the `ISequential` protocol, or if `_x_` is a host sequential
  collection. Returns `false` otherwise."
  {:added v1
   :see '[red? coll? provide-sequential]
   :category "Primary"}
  [x :- Any]
  (or (class-instance? clojure.lang.Sequential x)
      (class-instance? java.util.List x))) ;; JVM HOST

(defn provide-sequential :- IRed
  "Returns `_x_` if it is a sequential collection,
  otherwise returns list containing `_x_`."
  {:added v1
   :see '[provide-collection sequential? coll? red? seq]
   :category "Primary"}
  [x :- Any]
  (cond (sequential? x) x
        (nil? x) clojure.lang.PersistentList/EMPTY
        :else (clojure.lang.PersistentList. x)))

;;; Persistent collections

(defprotocol IPersistentCollection
  "A value protocol for persistent collections."
  {:added v1
   :see '[conj]
   :category "Persistent"
   :predicate 'coll?
   :on-interface clojure.lang.IPersistentCollection
   :forbid-extensions true
   :distinct true}
  (-conj :- IPersistentCollection
    "Returns the `_this_` collection with `x` conjoined."
    {:on 'cons}
    [coll x :- Any]))

(defn conj :- IPersistentCollection
  "Returns `_coll_` with given items conjoined.
  Conjoins into empty list if `_coll_` is `nil`."
  {:added v1
   :see '[assoc conj! pop dunaj.coll.recipe/append]
   :category "Persistent"}
  ([] [])
  ([coll :- (Maybe IPersistentCollection)] coll)
  ([coll :- (Maybe IPersistentCollection), x :- Any]
   (if (nil? coll) (clojure.core/list x) (-conj coll x)))
  ([coll :- (Maybe IPersistentCollection), x1 :- Any, x2 :- Any]
   (-conj (conj coll x1) x2))
  ([coll :- (Maybe IPersistentCollection), x1 :- Any, x2 :- Any,
    x3 :- Any]
   (-conj (-conj (conj coll x1) x2) x3))
  ([coll :- (Maybe IPersistentCollection), x1 :- Any, x2 :- Any,
    x3 :- Any, x4 :- Any]
   (-conj (-conj (-conj (conj coll x1) x2) x3) x4))
  ([coll :- (Maybe IPersistentCollection), x1 :- Any, x2 :- Any,
    x3 :- Any, x4 :- Any, x5 :- Any]
   (-conj (-conj (-conj (-conj (conj coll x1) x2) x3) x4) x5))
  ([coll :- (Maybe IPersistentCollection), x1 :- Any, x2 :- Any,
    x3 :- Any, x4 :- Any, x5 :- Any & more :- Any]
   (if (editable? coll)
     (let [t (edit coll)
           t (-> t
                 (conj! x1)
                 (conj! x2)
                 (conj! x3)
                 (conj! x4)
                 (conj! x5))]
       (settle! (reduce conj! t more)))
     (let [coll (-> coll
                    (conj x1)
                    (-conj x2)
                    (-conj x3)
                    (-conj x4)
                    (-conj x5))]
       (reduce -conj coll more)))))

(defprotocol IAssociative
  "A value protocol for associative collections.
  For backwards compatibility with Clojure, implementations should
  also implement `c.l.Associative/containsKey`."
  {:added v1
   :see '[assoc]
   :category "Persistent"
   :predicate 'associative?
   :on-interface clojure.lang.Associative
   :forbid-extensions true}
  (-entry
    "Returns the entry from `_this_` for a given `_key_` as a counted
    collection with `_key_` as a first item. Depending on a type,
    there may be a value as a second item, or they may be multiple
    values (multimap) present."
    ;; TODO: multimap won't work, entryAt must return IMapEntry
    {:on 'entryAt}
    [this key :- Any])
  (-assoc :- IAssociative
    "assoc[iate]. Returns a new collection, that contains the
    mapping of `_key_` to `_val_`. For multimap, `_val_` must be a
    collection and this method replaces all previous vals with
    those in `_val_`."
    {:on 'assoc}
    [this key :- Any, val :- Any]))

(defn assoc :- IAssociative
  "assoc[iate]. When applied to a map, returns a new map of the
  same (hashed/sorted) type, that contains the mapping of key(s) to
  val(s). When applied to a vector, returns a new vector that
  contains val at index. Note - index must be pass:[<=]
  (count vector). If `_coll_` is `nil`, empty map will be used
  instead. For multimap, val must be a collection and
  this method replaces all previous vals with those in val."
  {:added v1
   :see '[conj assoc-in assoc! dunaj.coll.util/into
          dunaj.coll.util/merge dunaj.coll.util/merge-with
          associative?]
   :category "Persistent"}
  ([] {})
  ([coll :- (Maybe IAssociative)] coll)
  ([coll :- (Maybe IAssociative), key :- Any, val :- Any]
   (if (nil? coll) (-assoc {} key val) (-assoc coll key val)))
  ([coll :- (Maybe IAssociative), key1 :- Any, val1 :- Any,
    key2 :- Any, val2 :- Any]
   (-assoc (assoc coll key1 val1) key2 val2))
  ([coll :- (Maybe IAssociative), key1 :- Any, val1 :- Any,
    key2 :- Any, val2 :- Any, key3 :- Any, val3 :- Any]
   (-assoc (-assoc (assoc coll key1 val1) key2 val2) key3 val3))
  ([coll :- (Maybe IAssociative), key1 :- Any, val1 :- Any,
    key2 :- Any, val2 :- Any, key3 :- Any, val3 :- Any,
    key4 :- Any, val4 :- Any]
   (-> coll
       (assoc key1 val1)
       (-assoc key2 val2)
       (-assoc key3 val3)
       (-assoc key4 val4)))
  ([coll :- (Maybe IAssociative), key1 :- Any, val1 :- Any,
    key2 :- Any, val2 :- Any, key3 :- Any, val3 :- Any,
    key4 :- Any, val4 :- Any & keyvals :- Any]
   (let [coll (if (nil? coll) {} coll)]
     (if (editable? coll)
       (let [t (edit coll)
             t (-> t
                   (assoc! key1 val1)
                   (assoc! key2 val2)
                   (assoc! key3 val3)
                   (assoc! key4 val4))]
         (settle!
          (reduce (fn [t kv]
                    (assoc! t (first kv) (second kv)))
                  t
                  (partition 2 keyvals))))
       (let [coll (-> coll
                      (-assoc key1 val1)
                      (-assoc key2 val2)
                      (-assoc key3 val3)
                      (-assoc key4 val4))]
         (loop [coll coll kvs (seq keyvals)]
           (if kvs
             (if (next kvs)
               (recur (-assoc coll (first kvs) (second kvs))
                      (nnext kvs))
               (throw
                (java.lang.IllegalArgumentException.
                 (str "assoc expects even number of arguments "
                      "after map/vector, found odd number"))))
             coll)))))))

(defalias assoc-in
  "Associates a value in a nested associative structure, where
  `_ks_` is a sequence of keys and `_v_` is the new value and
  returns a new nested structure. If any levels do not exist,
  maps will be created."
  {:added v1
   :see '[update assoc assoc! update-in associative?]
   :category "Persistent"
   :tsig (Fn [IAssociative (Maybe IAssociative) IRed Any])})

(defalias update
  "'Updates' a value in an associative structure, where `_k_`
  is a key and `_f_` is a function that will take the old
  value and any supplied args and return the new value,
  and returns a new structure. If the key does not exist,
  `nil` is passed as the old value."
  {:added v1
   :see '[update-in assoc assoc-in associative?]
   :category "Persistent"
   :tsig (Fn [IAssociative (Maybe IAssociative) AnyFn (Va Any)])})

(defalias update-in
  "'Updates' a value in a nested associative structure, where
  `_ks_` is a sequence of keys and `_f_` is a function that
  will take the old value and any supplied args and return
  the new value, and returns a new nested structure.
  If any levels do not exist, maps will be created."
  {:added v1
   :see '[update assoc assoc-in]
   :category "Persistent"
   :tsig (Fn [IAssociative (Maybe IAssociative)
              IRed AnyFn (Va Any)])})

;;; Stacked

(defprotocol IStacked
  "A value protocol for stacked collections."
  {:added v1
   :see '[pop]
   :category "Persistent"
   :predicate 'stacked?
   :on-interface clojure.lang.IPersistentStack
   :forbid-extensions true}
  (-pop :- IStacked
    "Returns `_this_` without one item at a position specified by the
    implementation. It may be the first item (Lists, Queues), or it
    may be the last item (Vector). Throws if `_this_` is empty."
    {:on 'pop}
    [this]))

(defn pop :- (Maybe IStacked)
  "Returns `_coll_` without one item at a position specified by the
  implementation. It may be the first item (Lists, Queues), or it may
  be the last item (Vector). Returns `nil` if `_coll_` is `nil`.
  Throws if `_coll_` is empty."
  {:added v1
   :see '[rest peek dunaj.coll.recipe/drop pop! stacked?]
   :category "Persistent"}
  [coll :- (Maybe IStacked)]
  (when coll (-pop coll)))

;;; Catenable

(defprotocol ICatenable
  "A value protocol for catenable collections."
  {:added v1
   :see '[cat]
   :category "Persistent"
   :predicate 'catenable?}
  (-cat :- ICatenable
    "Returns the catenation of `_this_` with `_other_` collection of
    same type. Resulting collection is of the same type as `_this_`.
    Operation must be faster than linear time and may
    share data with original collections."
    [this other :- ICatenable]))

(defn cat :- ICatenable
  "Returns the catenation of `_coll_` with `_other_` collection of
  same type. Resulting collection is of the same type.
  Operation is faster than linear time."
  {:added v1
   :see '[dunaj.coll.recipe/concat conj cat! catenable?]
   :category "Persistent"}
  [coll :- ICatenable, other :- ICatenable]
  (-cat coll other))

;; Collection with usage tracking

(defprotocol IUsageTracking
  "A value protocol for collections tracking the usage
  of their items. Used e.g. for LRU caches."
  {:added v1
   :see '[hit]
   :category "Persistent"
   :predicate 'usage-tracking?}
  (-hit :- IUsageTracking
    "Returns collection which has processed a hit (usage)
    notification of a value associated with `_key_`. Undefined
    behavior if `_this_` does not contain mapping for `_key_`."
    [this key :- Any]))

(defn hit :- (Maybe IUsageTracking)
  "Returns a collection which has processed a hit (usage)
  notification of a value associated with `_key_`. Undefined
  behavior if `_coll_` does not contain mapping for `_key_`.
  Returns the same collection if it does not implement usage
  tracking. Returns `nil` if `_coll_` is `nil`."
  {:added v1
   :see '[usage-tracking?]
   :category "Persistent"}
  [coll :- [], key :- Any]
  (if (usage-tracking? coll) (-hit coll key) coll))

;;; Abstract types

(defprotocol IPersistentSet
  "An abstract type value protocol for persistent sets. For backwards
  compatibility with Clojure, implementations should also implement
  both `c.l.IPersistentSet/contains` and `c.l.IPersistnetSet/get`."
  {:added v1
   :see '[disj]
   :category "Persistent"
   :predicate 'set?
   :on-interface clojure.lang.IPersistentSet
   :forbid-extensions true}
  (-disj :- IPersistentSet
    "Returns the `_this_` with `_key_` removed. For bags, only one
    instance of `_key_` is removed."
    {:on 'disjoin}
    [this key :- Any]))

(defn disj :- (Maybe IPersistentSet)
  "Returns the `_coll_` with `_key_` removed. For bags, only one
  instance of `_key_` is removed. Returns `nil` if `_coll_` is
  `nil`."
  {:added v1
   :see '[dissoc disj-all disj! set?]
   :category "Persistent"}
  ([coll :- (Maybe IPersistentSet)] coll)
  ([coll :- (Maybe IPersistentSet), key :- Any]
   (when-not (nil? coll) (-disj coll key)))
  ([coll :- (Maybe IPersistentSet), key :- Any & keys :- Any]
   (when coll
     (let [ret (disj coll key)]
       (if keys (recur ret (first keys) (next keys)) ret)))))

(defprotocol IPersistentBag
  "An abstract type value protocol for persistent bags."
  {:added v1
   :see '[disj-all]
   :category "Persistent"
   :predicate 'bag?}
  (-disj-all :- IPersistentBag
    "Returns the `_this_` with all instances of `_key_` removed."
    [this key :- Any]))

(defn disj-all :- (Maybe IPersistentBag)
  "Returns the `_coll_` with all instances of `_key_` removed.
  Returns `nil` if `_coll_` is `nil`."
  {:added v1
   :see '[dissoc disj disj-all! bag?]
   :category "Persistent"}
  [coll :- (Maybe IPersistentBag), key :- Any]
  (when-not (nil? coll) (-disj-all coll key)))

(defprotocol IPersistentMap
  "An abstract type value protocol for persistent maps."
  {:added v1
   :see '[dissoc]
   :category "Persistent"
   :predicate 'map?
   :on-interface clojure.lang.IPersistentMap
   :forbid-extensions true}
  (-dissoc :- IPersistentMap
    "dissoc[iate]. Returns a new coll that does not contain any
    mapping for key."
    {:on 'without}
    [this key :- Any]))

(defn dissoc :- (Maybe IPersistentMap)
  "dissoc[iate]. Returns a new coll that does not contain any
  mapping for key. Returns `nil` if `_coll_` is `nil`."
  {:added v1
   :see '[dissoc-in dissoc-one dissoc! map?]
   :category "Persistent"}
  ([coll :- (Maybe IPersistentMap)] coll)
  ([coll :- (Maybe IPersistentMap), key :- Any]
   (when-not (nil? coll) (-dissoc coll key)))
  ([coll :- (Maybe IPersistentMap), key1 :- Any, key2 :- Any]
   (when-not (nil? coll) (-> coll (-dissoc key1) (-dissoc key2))))
  ([coll :- (Maybe IPersistentMap), key1 :- Any, key2 :- Any,
    key3 :- Any]
   (when-not (nil? coll)
     (-> coll (-dissoc key1) (-dissoc key2) (-dissoc key3))))
  ([coll :- (Maybe IPersistentMap) key1 :- Any, key2 :- Any,
    key3 :- Any & ks :- Any]
   (when-not (nil? coll)
     (if (editable? coll)
       (let [t (edit coll)
             t (-> t (dissoc! key1) (dissoc! key2) (dissoc! key3))]
         (settle! (reduce dissoc! t ks)))
       (let [coll (-> coll
                      (-dissoc key1)
                      (-dissoc key2)
                      (-dissoc key3))]
         (reduce dissoc! coll ks))))))

(defn dissoc-in :- (Maybe IPersistentMap)
  "Dissociates a value in a nested associative structure, where `_ks_`
  is a sequence of keys and returns a new nested structure."
  {:added v1
   :see '[map? dissoc dissoc-one dissoc!]
   :category "Persistent"}
  [m :- (Maybe IPersistentMap), [k & ks]]
  (if ks
    (if-let [v (get m k)] (assoc m k (dissoc-in v ks)) m)
    (dissoc m k)))

(defprotocol IPersistentMultiMap
  "Abstract type value protocol for persistent multimaps."
  {:added v1
   :see '[dissoc-one]
   :category "Persistent"
   :predicate 'multimap?}
  (-dissoc-one :- IPersistentMultiMap
    "dissoc[iate]. Returns a new coll that does not contain mapping
    specified by key and val."
    [this key :- Any, val :- Any]))

(defn dissoc-one :- IPersistentMultiMap
  "dissoc[iate]. Returns a new coll that does not contain
  `[_key_ _val_]` mapping. Returns `nil` if `_coll_` is `nil`."
  {:added v1
   :see '[multimap? dissoc dissoc-on dissoc-one!]
   :category "Persistent"}
  [coll :- (Maybe IPersistentMultiMap), key :- Any, val :- Any]
  (when-not (nil? coll) (-dissoc-one coll key val)))

(defprotocol IPersistentList
  "Abstract type value protocol for persistent lists."
  {:added v1
   :category "Persistent"
   :predicate 'list?
   :on-interface clojure.lang.IPersistentList
   :forbid-extensions true})

(defprotocol IPersistentVector
  "Abstract type value protocol for persistent vectors. For backwards
  compatibility with clojure, implementations should also implement
  `c.l.IPersistentVector/assocN` and `c.l.IPersistentVector/length`."
  {:added v1
   :category "Persistent"
   :predicate 'vector?
   :on-interface clojure.lang.IPersistentVector
   :forbid-extensions true})

;;; Collection factories

(defprotocol ICollectionFactory
  "A factory protocol for collections."
  {:added v1
   :see '[collection ->collection]}
  (-from-coll :- IRed
    "Returns a new collection from the contents of collection
    `_this_`, which can also be `nil`."
    [this coll :- []])
  (-from-items :- IRed
    "Returns a new collection that contains given items."
    [this]
    [this a :- Any]
    [this a :- Any, b :- Any]
    [this a :- Any, b :- Any, c :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any, more :- IRed]))

(defprotocol IConvolutionFactory
  "Factory protocol for convoluted collections."
  {:added v1
   :see '[convolution ->convolution]}
  (-convolute :- IRed
    "Returns new collection which will contain convoluted contents
    from given collections.
    Factory is not required to support all arities."
    [this c1 :- IRed, c2 :- IRed]
    [this c1 :- IRed, c2 :- IRed, c3 :- IRed]
    [this c1 :- IRed, c2 :- IRed, c3 :- IRed, c4 :- IRed]
    [this c1 :- IRed, c2 :- IRed, c3 :- IRed, c4 :- IRed,
     more :- IRed])
  (-from-interleaved :- IRed
    "Returns new collection from given interleaved pieces.
    Factory is not required to support all arities."
    [this]
    [this a :- Any, b :- Any]
    [this a :- Any, b :- Any, c :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any, e :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any, e :- Any, f :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any, e :- Any, f :- Any,
     g :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any, e :- Any, f :- Any,
     g :- Any, h :- Any]
    [this a :- Any, b :- Any, c :- Any, d :- Any, e :- Any, f :- Any,
     g :- Any, h :- Any, more :- Any]))

(defn collection :- IRed
  "Returns a new collection created by `_factory_` that will contain
  same contents as collection `_coll_`, which can also be `nil`."
  {:added v1
   :see '[->collection convolution ->convolution]
   :category "Factory"}
  [factory :- ICollectionFactory, coll :- []]
  (-from-coll factory coll))

(defn ->collection
  "Returns a new collection created by `_factory_` which will contain
  given items, if any."
  {:added v1
   :see '[collection convolution ->convolution]
   :category "Factory"
   :tsig (Fn [IRed ICollectionFactory]
             [IRed ICollectionFactory Any]
             [IRed ICollectionFactory Any Any]
             [IRed ICollectionFactory Any Any Any]
             [IRed ICollectionFactory Any Any Any Any]
             [IRed ICollectionFactory Any Any Any Any (Va Any)])}
  ([factory] (-from-items factory))
  ([factory a] (-from-items factory a))
  ([factory a b] (-from-items factory a b))
  ([factory a b c] (-from-items factory a b c))
  ([factory a b c d] (-from-items factory a b c d))
  ([factory a b c d & more] (-from-items factory a b c d more)))

(defn convolution
  "Returns a new collection which will contain convoluted contents
  from given collections. Used e.g. for maps."
  {:added v1
   :see '[->collection collection ->convolution]
   :category "Factory"
   :tsig (Fn [IRed IConvolutionFactory Any Any]
             [IRed IConvolutionFactory Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any (Va Any)])}
  ([factory c1 c2] (-convolute factory c1 c2))
  ([factory c1 c2 c3] (-convolute factory c1 c2 c3))
  ([factory c1 c2 c3 c4] (-convolute factory c1 c2 c3 c4))
  ([factory c1 c2 c3 c4 & more]
   (-convolute factory c1 c2 c3 c4 more)))

(defn ->convolution
  "Returns new collection from given interleaved pieces, if any."
  {:added v1
   :see '[->collection convolution collection]
   :category "Factory"
   :tsig (Fn [IRed IConvolutionFactory]
             [IRed IConvolutionFactory Any Any]
             [IRed IConvolutionFactory Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any Any Any Any]
             [IRed IConvolutionFactory Any Any Any Any Any Any Any
              Any]
             [IRed IConvolutionFactory Any Any Any Any Any Any Any Any
              (Va Any)])}
  ([factory] (-from-interleaved factory))
  ([factory a b] (-from-interleaved factory a b))
  ([factory a b c] (-from-interleaved factory a b c))
  ([factory a b c d] (-from-interleaved factory a b c d))
  ([factory a b c d e] (-from-interleaved factory a b c d e))
  ([factory a b c d e f] (-from-interleaved factory a b c d e f))
  ([factory a b c d e f g] (-from-interleaved factory a b c d e f g))
  ([factory a b c d e f g h]
   (-from-interleaved factory a b c d e f g h))
  ([factory a b c d e f g h & more]
   (-from-interleaved factory a b c d e f g h more)))
