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

(ns clojure.bootstrap
  "Bootstrapping Dunaj. Low level stuff, do not use."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.core :as cc :refer
    [if when string? first rest and map identical? nth symbol empty?
     pos? with-meta count remove name apply merge map? > count *ns*
     map-indexed when-not namespace var intern seq int / inc drop
     identity vary-meta type == concat throw some butlast or meta conj
     second = nnext false? get nil? not vec cons next condp if-not
     recur keyword? string? boolean eval list symbol? io! str assoc
     vector? cond not= true? false? reduce]]))

;;; Dunaj versions

(cc/def v1 "1.0")

(cc/def vc "0.3.7")

;;; Helpers

(cc/defn mname
  "Returns name of `_x_`, or nil if `_x_` is not named."
  [x]
  (when (cc/instance? clojure.lang.Named x) (name x)))

(cc/defn ^:private several?
  "Returns true if `_coll_` contains more than one item."
  [coll]
  (> (count coll) 1))

(cc/defn ^:private positions
  "Returns a lazy sequence containing the positions at which `_pred_`
  is `true` for items in `_coll_`."
  [pred coll]
  (cc/keep-indexed #(when (pred %2) %) coll))

(cc/defmacro not-implemented
  "A placeholder for not implemented piece of code."
  ([] `(throw (java.lang.UnsupportedOperationException.)))
  ([m] `(throw (java.lang.UnsupportedOperationException. ~m))))

(cc/defmacro illegal
  "Emits throwing illegal-argument exception."
  [m] `(throw (java.lang.IllegalArgumentException. ~m nil)))

(cc/defn type?
  "Returns `true` if `_x_` is a type created with `deftype`,
  otherwise returns `false`."
  [x]
  (and (map? x) (:clojure.core/type x)))

(cc/defn record?
  "Returns `true` if `_x_` is a type created with `defrecord`,
  otherwise returns `false`."
  [x]
  (and (map? x) (:clojure.core/record x)))

(cc/defn protocol?
  "Returns `true` if `_x_` is a protocol created with `defprotocol`,
  otherwise returns `false`."
  [x]
  (and (map? x) (:clojure.core/protocol x)))

(cc/defn milliseconds
  "Returns number of milliseconds from given duration or number."
  [val]
  ;; implementation added in dunaj.time
  (not-implemented))

;;; Type signatures

(cc/def ^:private primitive-hints
  {java.lang.Byte 'byte
   java.lang.Short 'short
   java.lang.Integer 'int
   java.lang.Long 'long
   java.lang.Float 'float
   java.lang.Double 'double
   java.lang.Boolean 'boolean
   java.lang.Character 'char})

(cc/def ^:private primitive-code-map
  {'byte \B
   'short \S
   'int \I
   'long \L ;; \J
   'float \F
   'double \D
   'boolean \X ;; \Z
   'char \C
   nil \O})

(cc/def ^:private ^java.lang.reflect.Field prima
  (cc/doto (.getDeclaredField
            clojure.lang.Compiler$FnMethod "primitiveTypes")
    (.setAccessible true)))

(cc/defn primitive-type-hint
  "Returns primitive type hint (a symbol) for a given signature
  `_sig_`, or `nil` if `_sig_` does not have primitive type hint."
  [sig]
  (cc/let [maybec (or (:on-class sig) (:on-interface sig)
                      (when (cc/class? sig) sig) (cc/type sig))]
    (primitive-hints maybec)))

(cc/defn polymorphic-type-hint
  "Returns \"polymorphic\" host type hint symbol for a given signature
  `_sig_` or returns `nil`, if `_sig_` has no type hint assigned."
  [sig]
  ;; implementation added in dunaj.type
  (not-implemented))

(cc/defn type-hint
  "Returns host type hint symbol for a given signature `_sig_`,
  or returns `nil`, if signature does not have a type hint
  associated with it."
  [sig]
  (cond (type? sig) (:on sig)
        (record? sig) (:on sig)
        (protocol? sig)
        (cc/when-let [i (:on-interface sig)]
          (and (:forbid-extensions sig)
               (not (:distinct sig))
               (cc/symbol (.getName ^java.lang.Class i))))
        (cc/fn? sig) nil
        (cc/class? sig)
        (cc/symbol (.getName ^java.lang.Class sig))
        (nil? sig) nil
        (cc/symbol? sig)
        (cc/symbol (.getName ^java.lang.Class (cc/type sig)))
        (cc/identical? :macro sig) nil
        (cc/keyword? sig)
        (cc/symbol (.getName ^java.lang.Class (cc/type sig)))
        (cc/string? sig)
        (cc/symbol (.getName ^java.lang.Class (cc/type sig)))
        (cc/char? sig)
        (cc/symbol (.getName ^java.lang.Class (cc/type sig)))
        (cc/number? sig)
        (cc/symbol (.getName ^java.lang.Class (cc/type sig)))
        (or (true? sig) (false? sig))
        (cc/symbol (.getName ^java.lang.Class (cc/type sig)))
        :else (polymorphic-type-hint sig)))

(cc/defn ^:private combine-hints
  "Returns a combined type hint from two type hints `_x_` and `_y_`.
  Returns more generic hint (e.g. superclass/superinterface)."
  [x y]
  (cond (= x y) x
        (and (not (nil? x)) (not (nil? y))
             (.isAssignableFrom ^java.lang.Class (eval x) (eval y)))
        x
        (and (not (nil? x)) (not (nil? y))
             (.isAssignableFrom ^java.lang.Class (eval y) (eval x)))
        y
        :else nil))

(cc/defn common-type-hint
  "Returns `[primitive-sig type-hint]` from given `_sigs_`.
  Returned items may be `nil`."
  [sigs]
  (cc/let [nothing (java.lang.Object.)
           nothing? #(cc/identical? nothing %)
           rf (cc/fn [[prim-sig hint] sig]
                (if (nil? sig)
                  ;; nil clears primitive sigs but not normal type
                  ;; hints, as null is valid value for any
                  ;; non-primitive type
                  [nil hint]
                  [(cc/let [x (primitive-type-hint sig)
                            y (primitive-type-hint prim-sig)]
                     (cond (nothing? prim-sig) sig
                           (= x y) prim-sig
                           :else nil))
                   (cc/let [x (type-hint sig)]
                     (if (nothing? hint)
                       x
                       (combine-hints hint x)))]))
           [prim-sig hint] (reduce rf [nothing nothing] sigs)]
    [(when-not (nothing? prim-sig) prim-sig)
     (when-not (nothing? hint) hint)]))

;;; def*

;; map of all deftypes and defrecords
(cc/defonce type-map {})

(cc/defn ^:private parse-args
  "Returns [parsed-metadata leftover-arguments].
  Parses in a way that leftover arguments will never be empty,
  if `_args_` is not empty."
  [args]
  (cc/let [doc-string (when (and (several? args)
                                 (string? (first args)))
                        {:doc (first args)})
           args (if doc-string (rest args) args)
           attr-map (when (and (several? args) (map? (first args)))
                      (first args))
           args (if attr-map (rest args) args)]
    [(merge attr-map doc-string) args]))

(cc/defn strip-sigs-vec
  "Returns vector `_v_` without type signatures."
  [v]
  (cc/let [strip-fn (cc/fn [[ret strip?] val]
                      (cond strip? [ret false]
                            (identical? :- val) [ret true]
                            :else [(conj ret val) false]))]
    (with-meta (first (reduce strip-fn [[] false] v)) (meta v))))

(cc/defn strip-sigs-fdecl
  "Returns `_fdecl_` without type signatures.
  `_fdecl_` is `((params-vec & body) ...)`."
  [fdecl]
  (map (cc/fn [[v & body]] (cons (strip-sigs-vec v) body)) fdecl))

(cc/defn get-sigs-vec
  "Returns vec of sigs extracted from vector `_v_`. `_bindings?_`
  arg specifies whether given vec is a binding vector (loop, let)."
  ([v] (get-sigs-vec v false))
  ([v bindings?]
     (cc/let [[state ret rest?]
              (reduce
               (cc/fn [[state ret rest?] val]
                 (cond
                  (and (== 0 state) (= "&" (mname val))) [0 ret true]
                  (== 0 state) [1 ret rest?]
                  (== 2 state) [(if bindings? 3 0)
                                (conj ret (if rest?
                                            (list 'dunaj.type/Va val)
                                            val))
                                rest?]
                  (== 3 state) [0 ret rest?]
                  (identical? :- val) [2 ret rest?]
                  (and (not bindings?) (= "&" (mname val)))
                  [0 (conj ret 'dunaj.type/Any) true]
                  :else [(if bindings? 0 1)
                         (conj ret (if rest?
                                     '(dunaj.type/Va dunaj.type/Any)
                                     'dunaj.type/Any))
                         rest?]))
               [0 [] false] v)]
       (if (== 0 state) ret (conj ret 'dunaj.type/Any)))))

(cc/defn fn-sig
  "Returns type signature for a function,
   based on output signature `_out-sig_` and sugared `_fdecl_`."
  ([fdecl]
     (fn-sig 'dunaj.type/Any fdecl))
  ([out-sig fdecl]
     (cons 'dunaj.type/Fn
           (map #(vec (cons out-sig
                            (get-sigs-vec (first %)))) fdecl))))

(cc/defn fn-sig-normalize
  "Returns normalized `_fn-sig_` function signature. If `_fn-sig_` is
  a vector, put it into a `Fn` type signature, otherwise returns
  unmodified `_fn-sig_`."
  [fn-sig]
  (when fn-sig
    (if (cc/vector? fn-sig) (list 'dunaj.type/Fn fn-sig) fn-sig)))

;; TODO: support for nil not eliminating non-primitive hints
;; e.g. (Fn [nil nil] [Integer Integer])
(cc/defn decorate-vec
  "Returns vec `_v_` decorated with hints taken from `_vsigs_`,
  which is a `[sigs sigs sigs...]`."
  ([v vsigs] (decorate-vec v vsigs false))
  ([v vsigs primitives?]
     (if (empty? vsigs)
       v
       (cc/let [out-hint
                (when primitives?
                  (primitive-type-hint
                   (first (common-type-hint (map first vsigs)))))
                out-meta (meta v)
                out-meta (if (and out-hint (nil? (:tag out-meta)))
                           (assoc out-meta :tag out-hint)
                           out-meta)
                get-hint (cc/fn [[prim-sig hint]]
                           (or (when primitives?
                                 (primitive-type-hint prim-sig))
                               hint))
                ah #(get-hint (common-type-hint %))
                mf
                (cc/fn [i x]
                  (cc/if-let [h (ah (map #(nth % (inc i) nil) vsigs))]
                    (vary-meta x assoc :tag h)
                    x))]
         (with-meta (vec (map-indexed mf v)) out-meta)))))

(cc/defn decorate-binding-vec
  "Returns binding vec `_v_` decorated with type hints taken from
  `_sigs_`."
  [v sigs]
  (if (empty? sigs)
    v
    (cc/let [out-meta (meta v)
             get-hint #(or (primitive-type-hint %) (type-hint %))
             mf (cc/fn [i x]
                  (cc/if-let [h (when (and (cc/even? i)
                                           (cc/instance?
                                            clojure.lang.IObj x))
                                  (get-hint (nth sigs (/ i 2) nil)))]
                    (vary-meta x assoc :tag h)
                    x))]
      (with-meta (vec (map-indexed mf v)) out-meta))))

(cc/defn primitive-code [vsigs]
  "Returns primitive code (a string) for given `_vsigs_`, which is
  a `[sigs sigs sigs...]`."
  (cc/let [vsigs (map #(when-not (cc/empty? %)
                         (concat (rest %) [(first %)])) vsigs)
           pf (cc/fn [& xs]
                (primitive-code-map
                 (primitive-type-hint (first (common-type-hint xs)))))
           pc (when-not (empty? vsigs) (apply map pf vsigs))]
    (cc/apply cc/str pc)))

(cc/defn decorate-methods
  "Returns decorated `_fdecl_` based on given `_fn-sig_`.
  fdecl is `((params-vec & body) ...)`"
  [fdecl fn-sig]
  (cc/let [count-argvec
           (cc/fn [v] (count (remove #(= "&" (mname %)) v)))
           variable (cc/fn [v] (some #(= "&" (mname %)) v))
           m-sigs (cc/fn [v] (cc/filter #(== (count %)
                                             (inc (count-argvec v)))
                                        (:method-sigs fn-sig)))
           primitives? #(cc/contains? (.get prima nil)
                                      (primitive-code %))]
    ;; TODO: optionally warn if primitive hints cannot be generated
    (map (cc/fn [[v & body]]
           (cc/let [coll (m-sigs v)
                    prim (and (primitives? coll) (not (variable v)))]
             (cons (decorate-vec v coll prim) body)))
         fdecl)))

(cc/defn ^:private fn-type-hint
  "Returns a return value type hint for a given `_fn-sig_`."
  [fn-sig]
  (second (common-type-hint (map first (:method-sigs fn-sig)))))

(cc/defmacro defonce
  "Like clojure.core/defonce, but with support for type signatures."
  [name & args]
  (cc/let [out-sig? (cc/identical? :- (first args))
           out-sig (when out-sig? (second args))
           args (if out-sig? (cc/drop 2 args) args)
           [m args] (parse-args args)
           m (merge {} (meta name) m)
           _ (when (and out-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           m (if out-sig? (assoc m :tsig out-sig) m)
           esig (cc/eval (:tsig m))
           m (cc/if-let [oh (type-hint esig)]
               (if (:tag m)
                 m
                 (assoc m :tag (cc/list 'clojure.core/quote oh)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           name (with-meta name m)]
    `(clojure.core/defonce ~name ~@args)))

(cc/defmacro defalias
  "Defines a var with the same root binding and metadata as the
  `aliased` var. For the definition process see `state/def`
  docstring. `aliased` var name is optional and if not provided,
  it will be constructed as `clojure.core/<name>`. Consult the
  language reference for the list of special metadata keys."
  [name & args]
  (cc/let [[m args] (parse-args (concat args [nil]))
           m (merge {} (meta name) m)
           esig (cc/eval (:tsig m))
           m (cc/if-let [oh (type-hint esig)]
               (if (:tag m)
                 m
                 (assoc m :tag (cc/list 'clojure.core/quote oh)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           args (butlast args)
           aliased (if (several? args)
                     (illegal "Too many arguments to defalias.")
                     (first args))
           aliased (or aliased (symbol "clojure.core" (cc/name name)))
           aliased (if (and (empty? (namespace aliased))
                            (pos? (or (first
                                       (positions #{\.}
                                                  (cc/name aliased)))
                                      0)))
                     (symbol (cc/name aliased) (cc/name name))
                     aliased)
           m (assoc m :alias (cc/list 'clojure.core/quote aliased))]
    `(clojure.core/let
         [var# (var ~aliased)
          n# (with-meta '~name (merge (meta var#) ~m))]
       (if (.hasRoot var#) (intern *ns* n# @var#) (intern *ns* n#)))))

(cc/defmacro def
  "Like clojure.core/def, but with support for type signatures and
  unified syntax."
  [name & args]
  (cc/let [out-sig? (cc/identical? :- (first args))
           out-sig (when out-sig? (second args))
           args (if out-sig? (cc/drop 2 args) args)
           [m args] (parse-args args)
           m (merge {} (meta name) m)
           _ (when (and out-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           m (if out-sig?
               (assoc m :tsig (or out-sig 'dunaj.type/Any))
               m)
           m (if (:tsig m) m (assoc m :tsig 'dunaj.type/Any))
           esig (cc/eval (:tsig m))
           m (cc/if-let [oh (type-hint esig)]
               (if (:tag m)
                 m
                 (assoc m :tag (cc/list 'clojure.core/quote oh)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           name (with-meta name m)]
    `(clojure.core/def ~name ~@args)))

(cc/defmacro defn
  "Like clojure.core/defn, but with support for type signatures."
  [name & args]
  (cc/let [out-sig? (cc/identical? :- (first args))
           out-sig (when out-sig? (second args))
           args (if out-sig? (cc/drop 2 args) args)
           docstring (when (string? (first args)) {:doc (first args)})
           args (if docstring (rest args) args)
           m (when (map? (first args)) (first args))
           args (if m (rest args) args)
           fdecl (if (vector? (first args)) (list args) args)
           pm (when (map? (cc/last fdecl)) (cc/last fdecl))
           fdecl (if pm (butlast fdecl) fdecl)
           m (merge {} (meta name) m pm docstring)
           sugar-sig? (or out-sig? (cc/some #(cc/identical? :- %)
                                            (cc/mapcat first fdecl)))
           _ (when (and sugar-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           m (if sugar-sig?
               (assoc m :tsig
                      (if out-sig?
                        (fn-sig out-sig fdecl)
                        (fn-sig fdecl)))
               m)
           m (if (:tsig m)
               (assoc m :tsig (fn-sig-normalize (:tsig m)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           fdecl (strip-sigs-fdecl fdecl)
           esig (cc/eval (:tsig m))
           m (cc/if-let [oh (fn-type-hint esig)]
               (if (:tag m)
                 m
                 (assoc m :tag (cc/list 'clojure.core/quote oh)))
               m)
           name (with-meta name nil)
           fdecl (if esig (decorate-methods fdecl esig) fdecl)]
    `(clojure.core/defn ~name ~m ~@fdecl)))

(cc/defmacro defnonce
  "defs name to have the root value of the expr iff the named var has
  no root value, else expr is unevaluated"
  [name & args]
  (cc/let [out-sig? (cc/identical? :- (first args))
           out-sig (when out-sig? (second args))
           args (if out-sig? (cc/drop 2 args) args)
           docstring (when (string? (first args)) {:doc (first args)})
           args (if docstring (rest args) args)
           m (when (map? (first args)) (first args))
           args (if m (rest args) args)
           fdecl (if (vector? (first args)) (list args) args)
           pm (when (map? (cc/last fdecl)) (cc/last fdecl))
           fdecl (if pm (butlast fdecl) fdecl)
           m (merge {} (meta name) m pm docstring)
           sugar-sig? (or out-sig? (cc/some #(cc/identical? :- %)
                                            (cc/mapcat first fdecl)))
           _ (when (and sugar-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           m (if sugar-sig?
               (assoc m :tsig (if out-sig?
                                (fn-sig out-sig fdecl)
                                (fn-sig fdecl)))
               m)
           m (if (:tsig m)
               (assoc m :tsig (fn-sig-normalize (:tsig m)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           fdecl (strip-sigs-fdecl fdecl)
           esig (cc/eval (:tsig m))
           m (cc/if-let [oh (fn-type-hint esig)]
               (if (:tag m)
                 m
                 (assoc m :tag (cc/list 'clojure.core/quote oh)))
               m)
           name (with-meta name nil)
           fdecl (if esig (decorate-methods fdecl esig) fdecl)]
    `(clojure.core/let [v# (clojure.core/def ~name)]
       (clojure.core/when-not (.hasRoot v#)
         (defn ~name ~@fdecl)))))

(cc/defmacro defmacro
  "Like clojure.core/defmacro."
  [name & args]
  `(clojure.core/defmacro ~name ~@args))

(cc/defmacro defmacroonce
  "defs name to have the root value of the expr iff the named var
  has no root value, else expr is unevaluated"
  [name & args]
  (cc/let [[metadata args] (parse-args args)
           name (vary-meta name merge metadata)]
    `(clojure.core/let [v# (clojure.core/def ~name)]
       (clojure.core/when-not (.hasRoot v#)
         (defmacro ~name ~@args))
       (.setMacro ^clojure.lang.Var v#))))

(cc/defn gen-hints
  "Returns vec of type hints for a protocol method.
  This is later used in defprotocol to select correct
  interface method for parasitic protocols."
  [fn-sig]
  (cc/let [sigs (:method-sigs fn-sig)
           get-hint #(or (primitive-type-hint %) (type-hint %))
           mf (cc/fn [& hs]
                ;; TODO: can use combine-hints? here
                (reduce #(when (= % (get-hint %2)) %)
                        (get-hint (first hs)) hs))]
    (when-not (empty? sigs) (vec (drop 2 (apply map mf sigs))))))

(cc/defn prepare-sigs
  "Returns function which returns
  `[return-value-hint protocol-method-hints]` for a protocol method.
  Very hackish."
  [esig]
  (cc/fn []
    (cc/let [esig (eval esig)]
      [(fn-type-hint esig) (gen-hints esig)])))

(cc/defmacro defprotocol
  "Defines a var with newly created protocol as a root binding,
  defines polymorphic protocol functions, generates host interface
  and returns protocol var."
  [name & opts+sigs]
  (cc/loop [stripped []
            todo opts+sigs
            name (cc/vary-meta name cc/assoc :defprotocol true)]
    (cc/let [sig (first todo)]
      (condp #(%1 %2) sig
        string? (recur (conj stripped sig) (next todo) name)
        map? (recur stripped (next todo) (vary-meta name merge sig))
        keyword? (recur (conj stripped sig (second todo))
                        (nnext todo) name)
        nil?
        (cc/let [parasite? (:on-interface (meta name))
                 forbidden? (:forbid-extensions (meta name))
                 soft? (not (:distinct (meta name)))]
          (cc/as->
           (vec `(clojure.core/do)) ret
           (conj ret
                 `(~(if parasite?
                      `clojure.core/defprotocol2
                      `clojure.core/defprotocol) ~name ~@stripped)
                 #_(`(clojure.core/alter-var-root
                      (var ~name) assoc :on-interface
                      (eval (:on ~name)))))
           (cc/let [mn (eval (meta name))
                    predicate (:predicate mn)
                    category (:category mn)
                    category (if (= :abstract category)
                               "abstract type"
                               category)
                    category (when category
                               (str (cc/name category) " "))
                    argvec ^{:tag 'boolean} ['x]]
             (if predicate
               (cond (and parasite? soft?)
                     (conj ret
                           `(clojure.bootstrap/defn ~predicate
                              ~(str "Returns `true` if object `_x_`"
                                    " satisfies `" name "` "
                                    "protocol, `false` otherwise.")
                              {:added ~(:added mn)
                               :see ~(list 'clojure.core/quote
                                           (clojure.core/vec
                                            (cons name (:see mn))))
                               :category ~(:category mn)
                               :tsig dunaj.type/Predicate
                               :tag ~'java.lang.Boolean
                               :inline
                               (clojure.core/fn [f#]
                                 (clojure.core/list
                                  'clojure.core/or
                                  (clojure.core/list
                                   'clojure.core/instance?
                                   ~parasite?
                                   f#)
                                  (clojure.core/list
                                   'clojure.core/satisfies?
                                   (clojure.core/symbol
                                    ~(clojure.core/name
                                      (clojure.core/ns-name
                                       *ns*))
                                    (clojure.core/name '~name))
                                   f#)))}
                              ~argvec
                              (clojure.core/or
                               (clojure.core/instance? ~parasite? ~'x)
                               (clojure.core/satisfies? ~name ~'x))))
                     forbidden?
                     (cc/let [iname
                              (or (:on-interface (meta name))
                                  (symbol
                                   (str (clojure.core/munge
                                         (clojure.core/namespace-munge
                                          *ns*))
                                        "."
                                        (clojure.core/munge name))))]
                       (conj ret
                             `(clojure.bootstrap/defn ~predicate
                                ~(str "Returns `true` if object `_x_`"
                                      " directly implements `"
                                      name "` "
                                      "protocol, `false` otherwise.")
                                {:added ~(:added mn)
                                 :see ~(list 'clojure.core/quote
                                             (clojure.core/vec
                                              (cons name (:see mn))))
                                 :category ~(:category mn)
                                 :tsig dunaj.type/Predicate
                                 :tag ~'java.lang.Boolean
                                 :inline (clojure.core/fn [f#]
                                           (clojure.core/list
                                            'clojure.core/instance?
                                            ~iname
                                            f#))}
                                ~argvec
                                (clojure.core/instance? ~iname ~'x))))
                     :else
                     (conj ret
                           `(clojure.bootstrap/defn ~predicate
                              ~(str "Returns `true` if object `_x_`"
                                    " satisfies `" name "` "
                                    "protocol, `false` otherwise.")
                              {:added ~(:added mn)
                               :see ~(list 'clojure.core/quote
                                           (clojure.core/vec
                                            (cons name (:see mn))))
                               :category ~(:category mn)
                               :tag ~'java.lang.Boolean
                               :tsig dunaj.type/Predicate
                               :inline (clojure.core/fn [f#]
                                         (clojure.core/list
                                          'clojure.core/satisfies?
                                          (clojure.core/symbol
                                           ~(clojure.core/name
                                             (clojure.core/ns-name
                                              *ns*))
                                           (clojure.core/name '~name))
                                          f#))}
                              ~argvec
                              (clojure.core/satisfies? ~name ~'x))))
               ret))
           (apply list ret)))
        (cc/let [out-sig? (cc/identical? :- (second sig))
                 out-sig (when out-sig? (nth sig 2))
                 [m args] (parse-args
                           (if out-sig? (drop 3 sig) (next sig)))
                 m (merge {} (meta (first sig)) m)
                 sugar-sig? (or out-sig?
                                (cc/some #(cc/identical? :- %)
                                         (cc/mapcat identity args)))
                 _ (when (and sugar-sig? (:tsig m))
                     (illegal
                      "duplicate definition of type signature"))
                 m (if sugar-sig?
                     (assoc m :tsig
                            (if out-sig?
                              (fn-sig out-sig (map list args))
                              (fn-sig (map list args))))
                     m)
                 m (if (:tsig m)
                     (assoc m :tsig (fn-sig-normalize (:tsig m)))
                     m)
                 m (if (:tsig m)
                     (assoc m :qtsig
                            (cc/list 'clojure.core/quote (:tsig m)))
                     m)
                 args (map strip-sigs-vec args)
                 om m
                 pf (prepare-sigs (:tsig m))
                 m (cc/eval (cc/dissoc m :tsig))
                 ;; TODO: collect evaluated sigs into metadata of
                 ;; protocol
                 m (assoc m :tsig (:tsig om) :pf pf)
                 new-name (with-meta (first sig) m)
                 sig (cons new-name args)]
          (recur (conj stripped sig) (next todo) name))))))

(cc/defmacro deftype
  "Generates a type, defines constructor fns and returns that type."
  [name & args]
  (cc/let [[metadata args] (parse-args args)
           m (merge {} (meta name) metadata)
           predicate (:predicate m)
           alias? (not (vector? (first args)))
           sugar-sig? (and (not alias?) (cc/some #(cc/identical? :- %)
                                                 (first args)))
           _ (when (and sugar-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           stripped-vec (when (not alias?)
                          (strip-sigs-vec (first args)))
           m (if sugar-sig?
               (assoc m :tsig (get-sigs-vec (first args)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           m (assoc m :fields
                    (cc/list 'clojure.core/quote stripped-vec))
           args (if alias? args (cons stripped-vec (rest args)))
           esig (cc/eval (:tsig m))
           args (if (and (not alias?) esig)
                  (cons (decorate-vec
                         (first args) [(cons nil esig)] true)
                        (rest args))
                  args)
           ns-part (clojure.core/namespace-munge *ns*)
           classname (if alias?
                       (first args)
                       (symbol (str ns-part "." name)))
           name (with-meta name {:tsig (:tsig m)})
           argvec ^{:tag 'boolean} ['x]
           ret (if predicate
                 [`(clojure.bootstrap/defn ~(cc/eval predicate)
                     ~(str "Returns `true` if object `_x_` is "
                           "an instance of `" name
                           "` type, `false` otherwise.")
                     {:added ~(:added m)
                      :see ~(list 'clojure.core/quote
                                  (clojure.core/vec
                                   (cons name (eval (:see m)))))
                      :category ~(:category m)
                      :tsig dunaj.type/Predicate
                      :tag ~'java.lang.Boolean
                      :inline (clojure.core/fn [o#]
                                (list 'clojure.core/instance?
                                      ~classname o#))}
                     ~argvec
                     (clojure.core/instance? ~classname ~'x))])
           ret (if (and alias? (several? args))
                 (conj ret `(clojure.core/extend-type ~classname
                              ~@(rest args)))
                 ret)
           ret (conj ret name)]
    (if alias?
      `(clojure.core/do
         (def ~name)
         (def ~(with-meta name m)
           {:on '~classname
            :on-class ~classname
            :clojure.core/type true
            :tsig ~(:tsig m)
            :alias? true
            :var (var ~name)})
         (clojure.core/alter-var-root
          #'clojure.bootstrap/type-map assoc
          (clojure.core/quote ~classname)
          (java.lang.ref.WeakReference. ~name))
         ~@ret)
      `(clojure.core/do
         (clojure.core/deftype2 ~(with-meta name m) ~@args)
         (clojure.core/alter-var-root
          #'clojure.bootstrap/type-map assoc
          (clojure.core/quote ~classname)
          (java.lang.ref.WeakReference. ~name))
         ~@ret))))

(cc/defmacro defrecord
  "Generates a record, defines constructor fns and returns that type."
  [name & args]
  (cc/let [[metadata args] (parse-args args)
           m (merge {} (meta name) metadata)
           predicate (:predicate m)
           sugar-sig? (cc/some #(cc/identical? :- %) (first args))
           _ (when (and sugar-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           stripped-vec (strip-sigs-vec (first args))
           m (if sugar-sig?
               (assoc m :tsig (get-sigs-vec (first args)))
               m)
           m (if (:tsig m)
               (assoc m :qtsig
                      (cc/list 'clojure.core/quote (:tsig m)))
               m)
           m (assoc m :fields
                    (cc/list 'clojure.core/quote stripped-vec))
           args (cons stripped-vec (rest args))
           esig (cc/eval (:tsig m))
           args (if esig
                  (cons (decorate-vec
                         (first args) [(cons nil esig)] true)
                        (rest args))
                  args)
           ns-part (clojure.core/namespace-munge *ns*)
           classname (symbol (str ns-part "." name))
           name (with-meta name {:tsig (:tsig m)})
           argvec ^{:tag 'boolean} ['o]
           ret (if predicate
                 [`(clojure.bootstrap/defn ~(cc/eval predicate)
                     ~(str "Returns `true` if object `_x_` is"
                           "an instance of `" name
                           "` record, `false` otherwise.")
                     {:added ~(:added m)
                      :see ~(list 'clojure.core/quote
                                  (clojure.core/vec
                                   (cons name (eval (:see m)))))
                      :category ~(:category m)
                      :tsig dunaj.type/Predicate
                      :tag ~'java.lang.Boolean
                      :inline (clojure.core/fn [o#]
                                (list 'clojure.core/instance?
                                      ~classname o#))}
                     ~argvec
                     (clojure.core/instance? ~classname ~'o))])
           ret (conj ret name)]
    `(clojure.core/do
       (clojure.core/defrecord2 ~(with-meta name m) ~@args)
       (clojure.core/alter-var-root
        #'clojure.bootstrap/type-map
        assoc
        (clojure.core/quote ~classname)
        (java.lang.ref.WeakReference. ~name))
       ~@ret)))

;;; fn, loop, let et al.

(cc/defmacro fn
  "Returns new function created from given args."
  [& args]
  (cc/let [noname? (not (symbol? (first args)))
           name (if noname? 'foo (first args))
           args (if noname? args (rest args))
           out-sig? (cc/identical? :- (first args))
           out-sig (when out-sig? (second args))
           args (if out-sig? (cc/drop 2 args) args)
           docstring (when (string? (first args)) {:doc (first args)})
           args (if docstring (rest args) args)
           m (when (map? (first args)) (first args))
           args (if m (rest args) args)
           fdecl (if (vector? (first args)) (list args) args)
           pm (when (map? (cc/last fdecl)) (cc/last fdecl))
           fdecl (if pm (butlast fdecl) fdecl)
           m (merge {} (meta name) m pm docstring)
           sugar-sig? (or out-sig? (cc/some #(cc/identical? :- %)
                                            (cc/mapcat first fdecl)))
           _ (when (and sugar-sig? (:tsig m))
               (illegal "duplicate definition of type signature"))
           m (if sugar-sig?
               (assoc m :tsig
                      (if out-sig?
                        (fn-sig out-sig fdecl)
                        (fn-sig fdecl)))
               m)
           m (if (:tsig m)
               (assoc m :tsig (fn-sig-normalize (:tsig m)))
               m)
           fdecl (strip-sigs-fdecl fdecl)
           esig (cc/eval (:tsig m))
           m (cc/if-let [oh (fn-type-hint esig)]
               (if (:tag m)
                 m
                 (assoc m :tag (cc/list 'clojure.core/quote oh)))
               m)
           name (with-meta name nil)
           fdecl (if esig (decorate-methods fdecl esig) fdecl)]
    `(clojure.core/fn ~@(if noname? [] [name]) ~@fdecl)))

(cc/defn ^:private decorate-bindings
  "Returns binding vector decorated with type hints taken from
  type signatures, present in the `_bindings_` vector."
  [bindings]
  (cc/let [sugared? (cc/some #(cc/identical? :- %) bindings)
           esig (when sugared? (cc/eval (get-sigs-vec bindings true)))
           bindings (strip-sigs-vec bindings)]
    (if esig (decorate-binding-vec bindings esig) bindings)))

(cc/defmacro let
  "Generates a let with type signature, yay!"
  [bindings & body]
  `(clojure.core/let ~(decorate-bindings bindings) ~@body))

(cc/defmacro if-let
  "Generates an if-let with type signature, yay!"
  [bindings & body]
  `(clojure.core/if-let ~(decorate-bindings bindings) ~@body))

(cc/defmacro when-let
  "Generates a when-let with type signature, yay!"
  [bindings & body]
  `(clojure.core/when-let ~(decorate-bindings bindings) ~@body))

(cc/defmacro loop
  "Generates a loop with type signatures, yay!"
  [bindings & body]
  `(clojure.core/loop ~(decorate-bindings bindings) ~@body))

(cc/defmacro iloop
  "Generates an iloop with type signatures, yay!"
  [bindings & body]
  `(clojure.core/iloop* ~(decorate-bindings bindings) ~@body))

;;; primitive value tester

(cc/definterface IPrimitiveTester
  (getType [^byte x])
  (getType [^boolean x])
  (getType [^short x])
  (getType [^int x])
  (getType [^long x])
  (getType [^char x])
  (getType [^float x])
  (getType [^double x])
  (getType [^java.lang.Object x]))

(clojure.core/deftype PrimitiveTester []
  IPrimitiveTester
  (getType [this ^byte x] :byte)
  (getType [this ^boolean x] :boolean)
  (getType [this ^short x] :short)
  (getType [this ^int x] :int)
  (getType [this ^long x] :long)
  (getType [this ^float x] :float)
  (getType [this ^double x] :double)
  (getType [this ^char x] :char)
  (getType [this ^java.lang.Object x] :object))

(defmacro pt
  "Returns keyword based on return value type.
  Used to determine whether value is of a primitive type."
  [x]
  `(.getType (PrimitiveTester.) ~x))

;;; asserts

(cc/defmacro assert-primitive
  "Asserts that forms return primitive values."
  ([form]
     `(clojure.core/assert
       (not= :object (pt ~form))
       (str "form " '~form " does not yield a primitive")))
  ([form & more]
     `(clojure.core/do
        (assert-primitive ~form)
        ~@(cc/map #(cc/list `assert-primitive %) more))))

(cc/defmacro assert-boolean
  "Asserts that forms return primitive boolean values."
  ([form]
     `(clojure.core/assert
       (clojure.core/let [r# ~form]
         (and (= :boolean (pt r#))
              (or (true? r#) (false? r#))))
       (str "form " '~form " does not yield a boolean primitive")))
  ([form & more]
     `(clojure.core/do (assert-boolean ~form)
                       ~@(cc/map #(cc/list `assert-boolean %) more))))

(cc/defmacro assert-int
  "Asserts that forms return primitive int values."
  ([form]
     `(clojure.core/assert
       (clojure.core/let [r# ~form]
         (and (= :int (pt r#))))
       (str "form " '~form " does not yield an int primitive")))
  ([form & more]
     `(clojure.core/do (assert-int ~form)
                       ~@(cc/map #(cc/list `assert-int %) more))))

;;; dev helpers

(defmacro scratch
  "Skips evaluation of body but uses rq for additional ns requires."
  [rq & body]
  (if (empty? rq)
    `(clojure.core/comment)
    `(clojure.core/require
      ~@(cc/map #(cc/list 'clojure.core/quote %) rq))))

(defmacro replace-var!
  "Replaces `_dest_` var root with `_source_` value."
  ([dest]
     (cc/let [source (cc/symbol (cc/name dest))]
       `(clojure.core/do
          (clojure.core/alter-var-root
           (clojure.core/var ~dest)
           (clojure.core/constantly ~source))
          nil)))
  ([dest source]
     `(clojure.core/do
        (clojure.core/alter-var-root
         (clojure.core/var ~dest)
         (clojure.core/constantly ~source))
        nil)))
