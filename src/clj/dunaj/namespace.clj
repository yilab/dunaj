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

(ns dunaj.namespace
  "Creating, setting and inspecting namespaces.

  IMPORTANT: By design, there is no namespace type."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Mappings"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.core :refer
    [set map list into first second find-ns chunked-seq? chunk-first
     chunk-next persistent! transient ns-name all-ns create-ns throw
     remove-ns gensym ns-unmap ns-aliases the-ns]]
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Fn AnyFn U Maybe Va I]]
   [dunaj.boolean :refer [Boolean and not]]
   [dunaj.host :refer [Class class-instance?]]
   [dunaj.compare :refer [nil? =]]
   [dunaj.state :refer [IMutable IReference IAtomic ICloneable]]
   [dunaj.flow :refer [let loop recur if do]]
   [dunaj.poly :refer [reify]]
   [dunaj.coll :refer [ILookup conj seq seq? rest next]]
   [dunaj.function :refer [fn defn]]
   [dunaj.string :refer [String ->str]]
   [dunaj.identifier :refer [Symbol symbol? name]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.state.var :refer [Var defalias]]))


;;;; Implementation details

(defn ^:private ^:static reduce1
  ([f coll]
     (let [s (seq coll)]
       (if s
         (reduce1 f (first s) (next s))
         (f))))
  ([f val coll]
     (let [s (seq coll)]
       (if s
         (if (chunked-seq? s)
           (recur f
                  (.reduce (chunk-first s) f val)
                  (chunk-next s))
           (recur f (f val (first s)) (next s)))
         val))))

(defn ^:private ^:static into1
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  [to from]
  (if (class-instance? clojure.lang.IEditableCollection to)
    (persistent! (reduce1 clojure.core/conj! (transient to) from))
    (reduce1 conj to from)))

(defn ^:private ^java.util.concurrent.atomic.AtomicReference get-ar
  [symbol]
  (.-aliases ^clojure.lang.Namespace (the-ns symbol)))

(defn ^:private ns->sym
  [m]
  (let [tf (fn [[k v]] [k (ns-name v)])]
    (into {} (map tf m))))

(defn ^:private sym->ns
  [m]
  (let [tf (fn [[k v]] [k (the-ns v)])]
    (into {} (map tf m))))


;;;; Public API

(defalias munge
  "Returns a host lib name for a namespace named by a `_ns_` symbol."
  {:added v1
   :category "Primary"
   :see '[ns?]
   :tsig (Fn [String Symbol])}
  clojure.core/namespace-munge)

(defn ns? :- Boolean
  "Returns `true` if a namespace named by the `_symbol_` is present,
  otherwise returns `false`."
  {:added v1
   :category "Primary"
   :see '[all mappings munge]}
  [symbol :- Symbol]
  (not (nil? (find-ns symbol))))

(defn all :- #{Symbol}
  "Returns the set of symbols representing all available namespaces."
  {:added v1
   :category "Primary"
   :see '[ns? mappings]}
  []
  (set (map ns-name (all-ns))))

(defn meta :- {}
  "Returns metadata map for a namespace named by the `symbol`."
  {:added v1
   :category "Primary"
   :see '[dunaj.feature/meta mappings ns?]}
  [symbol :- Symbol]
  (clojure.core/meta (find-ns symbol)))

(defn create! :- nil
  "Creates a namespace named by the `_symbol_`. Returns `nil`.
  Does nothing if namespace already exists."
  {:added v1
   :category "Primary"
   :see '[ns? mappings remove! intern!]}
  [symbol :- Symbol]
  (create-ns symbol)
  nil)

(defn remove! :- nil
  "Removes a namespace named by the `_symbol_`. Returns `nil`.
  Does nothing if namespace does not exists."
  {:added v1
   :category "Primary"
   :see '[ns? mappings create! unmap!]}
  [symbol :- Symbol]
  (remove-ns symbol)
  nil)

;;; Mappings

(defalias resolve
  "Returns the var or host Class to which a symbol `_sym_` will be
  resolved in the namespace named by `_ns_` symbol (unless found in
  the environment `_env_`), else `nil`.

  Note that if `_sym_` is fully qualified, the var/Class to which
  it resolves need not be present in the `_ns_`."
  {:added v1
   :category "Mappings"
   :see '[mappings intern! refer! unmap!]
   :tsig (Fn [(U Var Class) Symbol Symbol]
             [(U Var Class) Symbol ILookup Symbol])}
  clojure.core/ns-resolve)

(defalias mappings
  "Returns a map of all the mappings for the namespace named by the
  `_ns_` symbol."
  {:added v1
   :category "Mappings"
   :see '[refers imports interns publics aliases intern!]
   :tsig (Fn [{Symbol (U Var Class)} Symbol])}
  clojure.core/ns-map)

(defalias refers
  "Returns a map of the refer mappings for the namespace named by
  the `_ns_` symbol."
  {:added v1
   :category "Mappings"
   :see '[mappings imports interns publics aliases intern! refer!]
   :tsig (Fn [{Symbol Var} Symbol])}
  clojure.core/ns-refers)

(defalias imports
  "Returns a map of the import mappings for the namespace named by
  the `_ns_` symbol."
  {:added v1
   :category "Mappings"
   :see '[refers mappings interns publics aliases intern!]
   :tsig (Fn [{Symbol Class} Symbol])}
  clojure.core/ns-imports)

(defalias interns
  "Returns a map of the intern mappings for the namespace named by
  the `_ns_` symbol."
  {:added v1
   :category "Mappings"
   :see '[refers imports mappings publics aliases intern!]
   :tsig (Fn [{Symbol Var} Symbol])}
  clojure.core/ns-interns)

(defalias publics
  "Returns a map of the public intern mappings for the namespace
  named by the `_ns_` symbol."
  {:added v1
   :category "Mappings"
   :see '[refers imports mappings interns aliases intern!]
   :tsig (Fn [{Symbol Var} Symbol])}
  clojure.core/ns-publics)

(defn aliases :- (I IReference IMutable)
  "Returns a mutable reference to the map of all the aliases for the
  namespace named by the `_symbol_`."
  {:added v1
   :category "Mappings"
   :see '[refers imports mappings interns publics intern!]}
  [symbol :- Symbol]
  (reify
    IReference
    (-deref [o] (ns->sym (ns-aliases symbol)))
    ICloneable
    (-clone [this] (throw (java.lang.UnsupportedOperationException.)))
    IMutable
    (-reset! [o val]
      (let [ar (get-ar symbol)]
        (.set ar (sym->ns val))
        val))
    IAtomic
    (-cas! [o oldval newval]
      (let [ar (get-ar symbol)
            v (.get ar)]
        (if (= v (sym->ns oldval))
          (.compareAndSet ar v (sym->ns newval))
          false)))))

(defalias intern!
  "Finds or creates a var named by the symbol `_name_` in the
  namespace named by the `_ns_` symbol, setting its root binding
  to `_val_` if supplied. The namespace `_ns_` must exist.

  The var will adopt any metadata from the `_name_` symbol.
  Returns the var."
  {:added v1
   :category "Mappings"
   :see '[unmap! import! refer! create! dunaj.env/current-ns]
   :tsig (Fn [Var Symbol Symbol]
             [Var Symbol Symbol Any])}
  clojure.core/intern)

(defmacro import!
  "import-list => (package-symbol class-name-symbols*)

  For each name in class-name-symbols, adds a mapping from name to
  the class named by package.name to the namespace identified by
  `_ns_` symbol.

  Use `:import` in the `ns` macro in preference to calling this
  directly."
  {:added v1
   :category "Mappings"
   :see '[unmap! intern! refer! create! dunaj.env/current-ns]}
  [ns & import-symbols-or-lists]
  (let [specs (map #(if (and (seq? %)
                             (= 'clojure.core/quote (first %)))
                      (second %)
                      %)
                   import-symbols-or-lists)
        ns-symbol ns
        ns (gensym)]
    `(let [~ns (clojure.core/the-ns ~ns-symbol)]
       ~@(map #(list `clojure.core/. ns 'importClass
                     (clojure.lang.RT/classForName %))
                (reduce1 (fn [v spec]
                          (if (symbol? spec)
                            (conj v (name spec))
                            (let [p (first spec) cs (rest spec)]
                              (into1 v (map #(->str p "." %) cs)))))
                        [] specs)))))

(defalias refer!
  "refers to all public vars of `_ns-sym_` in `_ons_`,
  subject to filters. filters can include at most one each of:

  * `:exclude` `list-of-symbols`
  * `:only` `list-of-symbols`
  * `:rename` `map-of-fromsymbol-tosymbol`

  For each public interned var in the namespace named by the symbol,
  adds a mapping from the name of the var to the var to the `_ons_`
  namespace. Throws an exception if name is already mapped to
  something else in the `_ons_`. Filters can be used to select a
  subset, via inclusion or exclusion, or to provide a mapping to
  a symbol different from the var’s name, in order to prevent
  clashes."
  {:added v1
   :category "Mappings"
   :see '[unmap! intern! import! create! dunaj.env/current-ns]
   :tsig (Fn [nil Symbol Symbol (Va Any)])}
  clojure.core/refer*)

(defn unmap! :- nil
  "Removes the mappings for the `_symbol_` from the namespace named by
  the `_ns_`. Returns `nil`.
  Does nothing if mapping does not exists."
  {:added v1
   :category "Mappings"
   :see '[refer! intern! import! create! dunaj.env/current-ns]}
  [ns :- Symbol, symbol :- Symbol]
  (ns-unmap ns symbol)
  nil)
