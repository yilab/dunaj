;; Copyright (C) 2008, 2015, Chris Houser, Christophe Grand,
;; Stephen Gilardi, Michel Salim, Jozef Wagner. All rights reserved.
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

(ns dunaj.repl
  "REPL tools.

  All of the vars defined here are automatically refered when
  dunaj REPL is started (when inside `dunaj.user` namespace),
  through `dunaj.main` or through `clojure.core/dunaj!`."
  {:authors ["Chris Houser" "Christophe Grand"
             "Stephen Gilardi" "Michel Salim" "Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Any Maybe Fn Any Va U]]
   [dunaj.boolean :refer [and or]]
   [dunaj.host :refer [proxy proxy-super class-instance? class]]
   [dunaj.math :refer [Integer dec + - min]]
   [dunaj.compare :refer [= not=]]
   [dunaj.flow :refer
    [when-let cond if let when dotimes loop recur if-let]]
   [dunaj.feature :refer [IMeta meta assoc-meta]]
   [dunaj.threading :refer [->]]
   [dunaj.coll :refer [assoc first transduce count seq]]
   [dunaj.function :refer [defn comp identity apply fn]]
   [dunaj.char :refer [whitespace? char]]
   [dunaj.string :refer [String ->str str]]
   [dunaj.identifier :refer [name Symbol symbol]]
   [dunaj.error :refer
    [IException exception? throw illegal-state ex-data]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.namespace :refer [resolve all publics]]
   [dunaj.state.var :refer [defalias def var with-bindings]]
   [dunaj.concurrent.thread :refer [Thread current-thread]]
   [dunaj.coll.util :refer [doseq sort-by recipe dored sort]]
   [dunaj.coll.recipe :refer
    [concat mapcat map vals lines drop-while keys filter take remove]]
   [dunaj.format :refer [parse print]]
   [dunaj.regex :refer [Regex regex regex?]]
   [dunaj.env :refer
    [println! current-ns print! prn! out err default-printer]]))


;;;; Implemnetation details

(defn ^:private namespace-doc :- IMeta
  [nspace :- clojure.lang.Namespace]
  (assoc (meta nspace) :name (clojure.core/ns-name nspace)))

(defn ^:private print-doc :- nil
  [m :- IMeta]
  (println! "-------------------------")
  (println! (->str (when-let [ns (:ns m)]
                     (->str (clojure.core/ns-name ns) "/"))
                   (:name m)))
  (cond (:forms m) (doseq [f (:forms m)] (print! "  ") (prn! f))
        (:arglists m) (prn! (:arglists m)))
  (when (:macro m) (println! "Macro."))
  (when (:protocol m) (println! "Protocol method."))
  (let [xf (comp (lines) (map #(drop-while whitespace? %)) (map str))]
    (dored [x xf (:doc m)] (println! x)))
  nil)


;;;; Public API

(def ^:dynamic *1 :- Any
  "Bound in a repl thread to the most recent value printed."
  {:added v1
   :see '[*2 *3 *e]}
  nil)

(def ^:dynamic *2 :- Any
  "Bound in a repl thread to the second most recent value printed."
  {:added v1
   :see '[*1 *3 *e]}
  nil)

(def ^:dynamic *3 :- Any
  "Bound in a repl thread to the third most recent value printed."
  {:added v1
   :see '[*2 *1 *e]}
  nil)

(def ^:dynamic *e :- (Maybe IException)
  "Bound in a repl thread to the most recent exception
  caught by the repl."
  {:added v1
   :see '[*2 *3 *1]}
  nil)

(defalias use!
  "Like `dunaj.lib/require!`, but also refers to each lib’s namespace
  using `dunaj.namespace/refer!`. Use `:use` in the `ns` macro in
  preference to calling this directly.

  `use!` accepts additional options in libspecs: `:exclude`, `:only`,
  `:rename`. The arguments and semantics for `:exclude`, `:only`,
  and `:rename` are the same as those documented for
  `dunaj.namespace/refer!`."
  {:added v1
   :tsig (Fn [Any (Va Any)])
   :see '[dunaj.lib/require!]}
  clojure.core/use)

(defn find-doc :- nil
  "Prints documentation for any var whose documentation or name
  contains a match for `_re-string-or-pattern_`."
  {:added v1
   :see '[doc source apropos]}
  [re-string-or-pattern :- (U String Regex)]
    (let [re (regex re-string-or-pattern)
          ms (concat
              (mapcat
               #(sort-by
                 :name (map meta (vals (clojure.core/ns-interns %))))
               (clojure.core/all-ns))
              (map namespace-doc (clojure.core/all-ns)))]
      (doseq [m ms
              :when (and (:doc m)
                         (or (first (parse re (:doc m)))
                             (first (parse re (name (:name m))))))]
        (print-doc m)))
    nil)

(defmacro doc
  "Prints documentation for a var or namespace given its
  symbolic `_name_`."
  {:added v1
   :see '[find-doc source apropos]}
  [name]
  (cond (clojure.core/find-ns name)
        `(#'print-doc (#'namespace-doc (clojure.core/find-ns '~name)))
        (resolve (current-ns) name) `(#'print-doc (meta (var ~name)))
        :else nil))

(defn source-fn :- (Maybe String)
  "Returns a string of the source code for the given symbol `_x_`,
  if it can find it.  This requires that the symbol resolve to a
  Var defined in a namespace for which the .clj is in the classpath.
  Returns `nil` if it can't find the source.
  For most REPL usage, `source` is more convenient."
  {:added v1
   :see '[source]}
  [x :- Symbol]
  (when-let [v (resolve (current-ns) x)]
    (when-let [filepath (:file (meta v))]
      (when-let [strm (.getResourceAsStream
                       (clojure.lang.RT/baseLoader) filepath)]
        (clojure.core/with-open [rdr (java.io.LineNumberReader.
                         (java.io.InputStreamReader. strm))]
          (dotimes [_ (dec (:line (meta v)))] (.readLine rdr))
          (let [text (java.lang.StringBuilder.)
                pbr (proxy [java.io.PushbackReader] [rdr]
                      (read []
                        ;; WARNING: Reflection
                        (let [i (proxy-super read)]
                          (.append text (char i))
                          i)))]
            (if (= :unknown clojure.core/*read-eval*)
              (throw
               (illegal-state (->str "Unable to read source while "
                                     "*read-eval* is :unknown.")))
              (clojure.core/read (java.io.PushbackReader. pbr)))
            (->str text)))))))

(defmacro source
  "Prints the source code for the given symbol `_x_`,
  if it can find it. This requires that the symbol resolve to a Var
  defined in a namespace for which the .clj is in the classpath."
  {:added v1
   :see '[source-fn]}
  [x]
  `(println! (or (source-fn '~x) "Source not found.")))

(defn apropos :- []
  "Given a regular expression or stringable thing, return a seq of all
  public definitions in all currently-loaded namespaces that match
  the `_str-or-pattern_`."
  {:added v1
   :see '[source doc]}
  [str-or-pattern :- (U String Regex)]
  (let [matches?
        (if (regex? str-or-pattern)
          #(parse str-or-pattern (->str %))
          #(.contains (->str %) (->str str-or-pattern)))]
    (sort (mapcat (fn [ns]
                    (let [ns-name (->str ns)]
                      (map #(symbol ns-name (->str %))
                           (filter matches? (keys (publics ns))))))
                  (all)))))

(defn dir-fn :- []
  "Returns a sorted seq of symbols naming public vars in
  a namespace named by `_ns_` symbol."
  ;; TODO: filter out vars without :added metadata
  {:added v1
   :see '[dir]}
  [ns :- Symbol]
  (sort (map (comp #(assoc-meta % nil) first) (publics ns))))

(defmacro dir
  "Prints a sorted directory of public vars in a namespace"
  {:added v1
   :see '[dir-fn]}
  [nsname]
  `(doseq [v# (dir-fn '~nsname)] (println! v#)))

(defn demunge :- String
  "Given a string representation of a fn class,
  as in a stack trace element, returns a readable version."
  {:added v1
   :see '[root-cause stack-element-str]}
  [fn-name :- String]
  (clojure.lang.Compiler/demunge fn-name))

(defn root-cause :- IException
  "Returns the initial cause of an exception or error by peeling off
  all of its wrappers"
  {:added v1
   :see '[demunge stack-element-str pst]}
  [t :- IException]
  (loop [cause t]
    (if (and (class-instance?
              clojure.lang.Compiler$CompilerException cause)
             (not= (.source ^clojure.lang.Compiler$CompilerException
                            cause) "NO_SOURCE_FILE"))
      cause
      (if-let [cause (.getCause cause)]
        (recur cause)
        cause))))

(defn stack-element-str :- String
  "Returns a (possibly unmunged) string representation of a
  StackTraceElement `_el_`."
  {:added v1
   :see '[pst root-cause demunge]}
  [el :- java.lang.StackTraceElement]
  (let [file (.getFileName el)
        clojure-fn? (and file (or (.endsWith file ".clj")
                                  (= file "NO_SOURCE_FILE")))]
    (->str (if clojure-fn?
             (demunge (.getClassName el))
             (->str (.getClassName el) "." (.getMethodName el)))
           " (" (.getFileName el) ":" (.getLineNumber el) ")")))

(defn pst :- nil
  "Prints a stack trace of the exception `_e_`, to the `_depth_`
  requested. If none supplied, uses the root cause of the most
  recent repl exception (`*e`), and a depth of 12."
  {:added v1
   :see '[stack-element-str root-cause demunge]}
  ([] (pst 12))
  ([e-or-depth :- (U Integer IException)]
     (if (exception? e-or-depth)
       (pst e-or-depth 12)
       (when-let [e *e]
         (pst (root-cause e) e-or-depth))))
  ([e :- IException, depth :- Integer]
     (with-bindings [out @err]
       (println!
        (->str
         (-> e class .getSimpleName) " "
         (.getMessage e)
         (when-let [info (ex-data e)]
           (->str " " (print default-printer info)))))
       (let [st (.getStackTrace e)
             cause (.getCause e)]
         (doseq [el (take depth
                          (remove
                           #(#{"clojure.lang.RestFn"
                               "clojure.lang.AFn"}
                             (.getClassName
                              ^java.lang.StackTraceElement %))
                           (seq st)))]
           (println! (->str \tab (stack-element-str el))))
         (when cause
           (println! "Caused by:")
           (pst cause
                (min depth
                     (+ 2 (- (count (seq (.getStackTrace cause)))
                             (count (seq st)))))))))))
