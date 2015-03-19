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

(ns dunaj.env
  "Run-time environment vars, facilities for printing to console."
  {:authors ["Jozef Wagner"]
   :additional-copyright true
   :categories ["Primary" "Console"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [scratch v1]]
   [dunaj.type :refer [Maybe I U AnyFn Fn Any Macro]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.flow :refer [when-let if let when]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [reify]]
   [dunaj.coll :refer [IRed assoc]]
   [dunaj.function :refer [defn apply]]
   [dunaj.string :refer [String str]]
   [dunaj.identifier :refer [Symbol]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.state.var :refer [Var defalias var def defonce declare]]
   [dunaj.uri :refer [Uri uri]]
   [dunaj.concurrent.thread :refer [pass!]]
   [dunaj.coll.tuple :refer [key]]
   [dunaj.format :refer [IParserFactory IPrinterFactory print parse]]
   [dunaj.format.charset :refer [default-charset]]
   [dunaj.format.clj :refer [clj pretty-clj]]
   [dunaj.resource :refer
    [IReadable IWritable IFlushable write-one! write!
     acquire! format grab-scope -write! -read!]]
   [dunaj.resource.host :refer
    [output-stream input-stream writer reader]]
   [dunaj.version :refer [Version version]]))


;;;; Implementation details

(defonce ^:private out-w-scope
  (-> (writer clojure.core/*out* :keep-open? true)
      acquire!
      (pass! nil)
      grab-scope))

(def ^:private ^:dynamic *out* :- IWritable
  (key out-w-scope))

(defonce ^:private err-w-scope
  (-> (writer clojure.core/*err* :keep-open? true)
      acquire!
      (pass! nil)
      grab-scope))

(def ^:private ^:dynamic *err* :- IWritable
  (key err-w-scope))

(defonce ^:private in-w-scope
  (-> (reader clojure.core/*in* :keep-open? true)
      acquire!
      (pass! nil)
      grab-scope))

(def ^:private ^:dynamic *in* :- IWritable
  (key in-w-scope))

(def ^:private ^:dynamic *color* :- Boolean
  false)

(def ^:private ^:dynamic *meta* :- Boolean
  false)

(def ^:private ^:dynamic *default-printer* :- IPrinterFactory
  clj)

(def ^:private ^:dynamic *default-pretty-printer* :- IPrinterFactory
  pretty-clj)

(declare host-newline)

(defn ^:private newline! :- nil
  "Writes a platform-specific newline to *out*"
  {:added v1}
  []
  (write! *out* host-newline)
  (when clojure.core/*flush-on-newline*
    (dunaj.resource/flush! *out*))
  nil)


;;;; Public API

(defmacro in-bare-ns
  "Sets current namespace to the namespace named by the symbol `_ns_`,
  creating it if needed. Uses `bare` API preset."
  {:added v1
   :category "Primary"
   :see '[in-ns dunaj.lib/ns]
   :highlight :flow}
  [ns]
  `(clojure.core/in-ns-bare ~ns))

;; NOTE: in-ns cannot be macro, as in-ns is a very special symbol
(defalias in-ns
  "Sets current namespace to the namespace named by the symbol `_ns_`,
  creating it if needed. Uses `clojure` API preset."
  {:added v1
   :category "Primary"
   :see '[in-bare-ns dunaj.lib/ns]
   :arglists '([ns])
   :highlight :flow}
  clojure.core/in-ns)

(defn compiling? :- Boolean
  "Returns `true` if compiling files, `false` otherwise."
  {:added v1
   :category "Primary"
   :see '[dunaj.lib/compile! current-file]}
  []
  clojure.core/*compile-files*)

(defn command-line-args :- (Maybe IRed)
  "Returns a collection of the supplied command line arguments,
  or `nil` if none were supplied."
  {:added v1
   :category "Primary"}
  []
  clojure.core/*command-line-args*)

(defn current-file :- (Maybe Uri)
  "Returns URI of the currently evaluated file or `nil`,
  if there is no file, e.g. in the REPL."
  {:added v1
   :category "Primary"
   :see '[compiling?]}
  []
  (when-let [f clojure.core/*file*]
    (uri f)))

(defn current-ns :- Symbol
  "Returns a symbol representing the current namespace."
  {:added v1
   :category "Primary"}
  []
  (.name ^clojure.lang.Namespace clojure.core/*ns*))

(defn current-version :- Version
  "Returns current version."
  {:added v1
   :category "Primary"
   :see '[dunaj.version/version]}
  []
  (version (clojure.core/clojure-version)))

(def data-readers :- Var
  "A dynamic var which stores current data readers mappings, excluding
  default ones, created by the system."
  {:added v1
   :category "Primary"
   :see '[default-data-reader-fn]}
  (var clojure.core/*data-readers*))

(def default-data-reader-fn :- Var
  "A dynamic var which stores default data reader fn."
  {:added v1
   :category "Primary"
   :see '[data-readers]}
  (var clojure.core/*default-data-reader-fn*))

(def out :- Var
  "A dynamic var holding default output resource."
  {:added v1
   :category "Console"
   :see '[print! out! err in flush!]}
  (var *out*))

(def err :- Var
  "A dynamic var holding default error resource."
  {:added v1
   :category "Console"
   :see '[err! in out]}
  (var *err*))

(def in :- Var
  "A dynamic var holding default input resource."
  {:added v1
   :category "Console"
   :see '[out err]}
  (var *in*))

(defn flush! :- nil
  "Flushes default output resource. Returns `nil`."
  {:added v1
   :category "Console"
   :see '[out out!]}
  []
  (dunaj.resource/flush! *out*))

(def host-newline :- String
  "A host newline string."
  {:added v1
   :category "Console"
   :see '[print! out!]}
  (java.lang.System/getProperty "line.separator"))

(defn out! :- nil
  "Prints contents of `_coll_` to the current output, returning
  `nil`."
  {:added v1
   :category "Console"
   :see '[out err! print!]}
  [coll]
  (write! *out* coll))

(defn err! :- nil
  "Prints contents of `coll` to the current error output,
  returning `nil`."
  {:added v1
   :category "Console"
   :see '[out! err]}
  [coll]
  (write! *err* coll))

(def color :- Var
  "A dynamic var holding color flag used in pretty printing."
  {:added v1
   :category "Console"
   :see '[with-color dunaj.dev/set-color! pp!]}
  (var *color*))

(defmacro with-color
  "A macro which executes `_body_` with color flag set to `true`."
  {:added v1
   :category "Console"
   :see '[color dunaj.dev/set-color! pp!]}
  [& body]
  `(dunaj.state.var/with-bindings {color true} ~@body))

(def meta :- Var
  "A dynamic var holding meta flag used in pretty printing."
  {:added v1
   :category "Console"
   :see '[with-meta pp!]}
  (var *meta*))

(defmacro with-meta
  "A macro which executes `_body_` with meta flag set to true."
  {:added v1
   :category "Console"
   :see '[meta pp!]}
  [& body]
  `(dunaj.state.var/with-bindings {meta true} ~@body))

(def default-printer :- Var
  "A dynamic var holding default printer factory used in pr! and prn!
  functions."
  {:added v1
   :category "Console"
   :see '[default-pretty-printer pr! prn!]}
  (var *default-printer*))

(def default-pretty-printer :- Var
  "A dynamic var holding default pretty printer factory used in
  pretty! function."
  {:added v1
   :category "Console"
   :see '[default-printer pp!]}
  (var *default-pretty-printer*))

(defn pr! :- nil
  "Prints given items to the current output and returns `nil`."
  {:added v1
   :category "Console"
   :see '[prn! print!]}
  [& xs :- Any]
  (out! (print *default-printer* xs)) nil)

(defn prn! :- nil
  "Prints given items to the current output and returns `nil`.
  Prints newline at the end."
  {:added v1
   :category "Console"
   :see '[pr! println!]}
  [& xs :- Any]
  (apply pr! xs) (newline!) nil)

(defn print! :- nil
  "Prints given items to the current output and returns `nil`.
  Meant for human consumption.
  Does not print quotes around strings, does not escape chars."
  {:added v1
   :category "Console"
   :see '[println! pp! pr!]}
  [& xs :- Any]
  (let [printer (assoc *default-printer* :human-printer? true)]
    (out! (print printer xs)) nil))

(defn println! :- nil
  "Prints given items to the current output and returns `nil`.
  Meant for human consumption.

  Does not print quotes around strings, does not escape chars.
  Prints newline at the end."
  {:added v1
   :category "Console"
   :see '[print! prn! pp!]}
  [& xs :- Any]
  (let [printer (assoc *default-printer* :human-printer? true)]
    (out! (print printer xs)) (newline!) nil))

(defn pp! :- nil
  "Pretty prints given items to the current output and returns `nil`."
  {:added v1
   :category "Console"
   :see '[println! print!]}
  [& xs :- Any]
  (let [printer *default-pretty-printer*
        printer (if *color* (assoc printer :color? true) printer)
        printer (if *meta* (assoc printer :meta? true) printer)]
    (out! (print printer xs)) (newline!) nil))
