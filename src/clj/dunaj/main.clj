;; Copyright (C) 2014, 2015,
;; Stephen C. Gilardi, Rich Hickey, Jozef Wagner.
;; All rights reserved.
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

(ns dunaj.main
  "Top level main function for REPL and scripts."
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [AnyFn Maybe Any]]
   [dunaj.boolean :refer [and or]]
   [dunaj.host :refer [class-instance? class set!]]
   [dunaj.host.number :refer [int]]
   [dunaj.math :refer [zero?]]
   [dunaj.compare :refer [= not= sentinel identical?]]
   [dunaj.flow :refer
    [loop if recur do let cond when-not eval when when-let doto]]
   [dunaj.threading :refer [->]]
   [dunaj.feature :refer [meta]]
   [dunaj.coll :refer [count assoc first conj seq empty?]]
   [dunaj.host.array :refer [aget]]
   [dunaj.function :refer [defn fn apply identity]]
   [dunaj.char :refer [Char whitespace? char]]
   [dunaj.string :refer [String ->str canonical str]]
   [dunaj.error :refer [IException try catch throw]]
   [dunaj.namespace :refer [resolve]]
   [dunaj.identifier :refer [Keyword name symbol]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.state.var :refer [declare def var]]
   [dunaj.coll.default :refer [->map]]
   [dunaj.coll.util :refer [dored doseq some]]
   [dunaj.format :refer [print parse print-one]]
   [dunaj.lib :refer [require!]]
   [dunaj.env :refer [current-ns print! println! prn! flush!
                      default-printer current-version pp!]]
   [dunaj.repl :refer [root-cause stack-element-str]]
   [dunaj.user])
  (:gen-class))


;;;; Implementation details

(declare main)

(defmacro with-bindings
  "Executes body in the context of thread-local bindings for several
  vars that often need to be set!: *ns* *warn-on-reflection*
  *math-context* *print-meta* *print-length* *print-level*
  *compile-path* *command-line-args* *1 *2 *3 *e"
  [& body]
  `(clojure.core/binding
       [clojure.core/*ns* clojure.core/*ns*
        clojure.core/*warn-on-reflection*
        clojure.core/*warn-on-reflection*
        clojure.core/*math-context* clojure.core/*math-context*
        clojure.core/*print-meta* clojure.core/*print-meta*
        clojure.core/*print-length* clojure.core/*print-length*
        clojure.core/*print-level* clojure.core/*print-level*
        clojure.core/*data-readers* clojure.core/*data-readers*
        clojure.core/*default-data-reader-fn*
        clojure.core/*default-data-reader-fn*
        clojure.core/*compile-path* (java.lang.System/getProperty
                                     "clojure.compile.path" "classes")
        clojure.core/*command-line-args*
        clojure.core/*command-line-args*
        clojure.core/*unchecked-math* clojure.core/*unchecked-math*
        clojure.core/*assert* clojure.core/*assert*
        dunaj.repl/*1 nil
        dunaj.repl/*2 nil
        dunaj.repl/*3 nil
        dunaj.repl/*e nil]
     ~@body))

(defn repl-prompt :- nil
  "Default :prompt hook for repl"
  []
  (print! (str (print "%s=> " (name (current-ns))))))

(defn skip-if-eol :- Keyword
  "If the next character on stream s is a newline, skips it, otherwise
  leaves the stream untouched. Returns :line-start, :stream-end,
  or :body to indicate the relative location of the next character
  on s. The stream must either be an instance of
  LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing all of CR, LF, and CRLF to a
  single \\newline."
  [s :- java.io.PushbackReader]
  (let [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     :else (do (.unread s c) :body))))

(defn skip-whitespace :- Keyword
  "Skips whitespace characters on stream s. Returns :line-start,
  :stream-end, or :body to indicate the relative location of the
  next character on s. Interprets comma as whitespace and semicolon
  as comment to end of line.
  Does not interpret #! as comment to end of line because only one
  character of lookahead is available. The stream must either be an
  instance of LineNumberingPushbackReader or duplicate its behavior
  of both supporting .unread and collapsing all of CR, LF, and CRLF
  to a single \\newline."
  [s :- clojure.lang.LineNumberingPushbackReader]
  (loop [c (.read s)]
    (cond
     (= c (int \newline)) :line-start
     (= c -1) :stream-end
     (= c (int \;)) (do (.readLine s) :line-start)
     (or (whitespace? (char c)) (= c (int \,))) (recur (.read s))
     :else (do (.unread s c) :body))))

(defn repl-read :- Any
  "Default :read hook for repl. Reads from *in* which must either be
  an instance of LineNumberingPushbackReader or duplicate its
  behavior of both supporting .unread and collapsing all of CR, LF,
  and CRLF into a single \\newline. repl-read:
   - skips whitespace, then
     - returns request-prompt on start of line, or
     - returns request-exit on end of stream, or
     - reads an object from the input stream, then
       - skips the next input character if it's end of line, then
       - returns the object."
  [request-prompt :- Any, request-exit :- Any]
  (or ({:line-start request-prompt :stream-end request-exit}
       (skip-whitespace clojure.core/*in*))
      (let [input (clojure.core/read)]
        (skip-if-eol clojure.core/*in*)
        input)))

(defn repl-exception :- IException
  "Returns the root cause of throwables"
  [throwable :- IException]
  (root-cause throwable))

(defn repl-caught :- nil
  "Default :caught hook for repl"
  [e :- IException]
  (let [ex :- IException (repl-exception e)
        tr (.getStackTrace ex)
        el (when-not (zero? (count (seq tr))) (aget tr 0))]
    (clojure.core/binding [clojure.core/*out* clojure.core/*err*]
      (println!
       (->str (-> ex class .getSimpleName)
              " " (.getMessage ex) " "
              (when-not (class-instance?
                         clojure.lang.Compiler$CompilerException ex)
                (->str " " (if el
                             (stack-element-str el)
                             "[trace missing]"))))))))

(def repl-requires :- Any
  "A sequence of lib specs that are applied to `require`
  by default when a new command-line REPL is started."
  '[])

(defmacro with-read-known
  "Evaluates body with *read-eval* set to a \"known\" value,
  i.e. substituting true for :unknown if necessary."
  [& body]
  `(clojure.core/binding
       [clojure.core/*read-eval*
        (if (= :unknown clojure.core/*read-eval*)
          true
          clojure.core/*read-eval*)]
     ~@body))

(defn ^:private repl
  "Generic, reusable, read-eval-print loop. By default, reads from
  *in*, writes to *out*, and prints exception summaries to *err*.
  If you use the default :read hook, *in* must either be an instance
  of LineNumberingPushbackReader or duplicate its behavior of both
  supporting .unread and collapsing CR, LF, and CRLF into a single
  \\newline. Options are sequential keyword-value pairs. Available
  options and their defaults:

  - :init, function of no arguments, initialization hook called
    with bindings for set!-able vars in place.
    default: #()

  - :need-prompt, function of no arguments, called before each
    read-eval-print except the first, the user will be prompted
    if it returns true.
     default: (if (instance? LineNumberingPushbackReader *in*)
                #(.atLineStart *in*)
                #(identity true))

  - :prompt, function of no arguments, prompts for more input.
    default: repl-prompt

  - :flush, function of no arguments, flushes output
    default: flush

  - :read, function of two arguments, reads from *in*:
      - returns its first argument to request a fresh prompt
        - depending on need-prompt, this may cause the repl to
          prompt before reading again
      - returns its second argument to request an exit from the
        repl
      - else returns the next object read from the input stream
    default: repl-read

  - :eval, function of one argument, returns the evaluation of its
    argument
    default: eval

  - :print, function of one argument, prints its argument to the
    output
    default: prn

  - :caught, function of one argument, a throwable, called when
    read, eval, or print throws an exception or error
    default: repl-caught"
  [& options]
  (let [cl (.getContextClassLoader (java.lang.Thread/currentThread))]
    (.setContextClassLoader (java.lang.Thread/currentThread)
                            (clojure.lang.DynamicClassLoader. cl)))
  (let [{:keys [init need-prompt prompt flush read eval print caught]
         :or {init        #()
              need-prompt
              (if (class-instance?
                   clojure.lang.LineNumberingPushbackReader
                   clojure.core/*in*)
                #(.atLineStart
                  ^clojure.lang.LineNumberingPushbackReader
                  clojure.core/*in*)
                #(identity true))
              prompt      repl-prompt
              flush       flush!
              read        repl-read
              eval        eval
              print       pp!
              caught      repl-caught}}
        (apply ->map options)
        request-prompt (sentinel)
        request-exit (sentinel)
        read-eval-print
        (fn []
          (try
            (let [read-eval clojure.core/*read-eval*
                  input (with-read-known
                          (read request-prompt request-exit))]
              (or (#{request-prompt request-exit} input)
                  (let [value (clojure.core/binding
                                  [clojure.core/*read-eval* read-eval]
                                (eval input))]
                    (print value)
                    (set! dunaj.repl/*3 dunaj.repl/*2)
                    (set! dunaj.repl/*2 dunaj.repl/*1)
                    (set! dunaj.repl/*1 value))))
            (catch java.lang.Throwable e
              (caught e)
              (set! dunaj.repl/*e e))))]
    (with-bindings
      (try
        (init)
        (catch java.lang.Throwable e
          (caught e)
          (set! dunaj.repl/*e e)))
      (prompt)
      (flush)
      (loop []
        (when-not
            (try (identical? (read-eval-print) request-exit)
                 (catch java.lang.Throwable e
                   (caught e)
                   (set! dunaj.repl/*e e)
                   nil))
          (when (need-prompt)
            (prompt)
            (flush))
          (recur))))))

(defn ^:private load-script
  "Loads Clojure source from a file or resource given its path. Paths
  beginning with @ or @/ are considered relative to classpath."
  [path :-  String]
  (if (.startsWith path "@")
    (clojure.lang.RT/loadResourceScript
     (.substring path (if (.startsWith path "@/") 2 1)))
    (clojure.lang.Compiler/loadFile path)))

(defn ^:private init-opt
  "Loads a script."
  [path]
  (load-script path))

(defn ^:private eval-opt
  "Evals expressions in str, prints each non-nil result using prn."
  [str]
  (dored [form (parse (assoc @default-printer :read-eval true) str)]
         (when-let [val (eval form)] (prn! val))))

(defn ^:private init-dispatch
  "Returns the handler associated with an init opt."
  [opt]
  ({"-i"     init-opt
    "--init" init-opt
    "-e"     eval-opt
    "--eval" eval-opt} opt))

(defn ^:private initialize
  "Common initialize routine for repl, script, and null opts."
  [args inits]
  (dunaj.env/in-ns 'dunaj.user)
  (set! clojure.core/*command-line-args* args)
  (doseq [[opt arg] inits]
    ((init-dispatch opt) arg)))

(defn ^:private main-opt
  "Calls the -main function from a namespace with string arguments
  from the command line."
  [[_ main-ns & args] inits]
  (with-bindings
    (initialize args inits)
    (apply (resolve (doto (symbol main-ns) require!) '-main) args)))

(defn ^:private repl-opt
  "Starts a repl with args and inits.
  Prints greeting if no eval options were present."
  [[_ & args] inits]
  (when-not (some #(= eval-opt (init-dispatch (first %))) inits)
    (println! "Dunaj" (canonical (current-version))))
  (repl :init (fn []
                (initialize args inits)
                (when-not (empty? repl-requires)
                  (apply require! repl-requires))))
  (prn!)
  (java.lang.System/exit 0))

(defn ^:private script-opt
  "Runs a script from a file, resource, or standard in with
  args and inits"
  [[path & args] inits]
  (with-bindings
    (initialize args inits)
    (if (= path "-")
      (clojure.core/load-reader clojure.core/*in*)
      (load-script path))))

(defn ^:private null-opt
  "No repl or script opt present, just binds args and run inits."
  [args inits]
  (with-bindings
    (initialize args inits)))

(defn ^:private help-opt :- nil
  "Prints help text for main."
  [_ _]
  (println! (:doc (meta (var main)))))

(defn ^:private main-dispatch :- AnyFn
  "Returns the handler associated with a main option."
  [opt :- (Maybe String)]
  (or
   ({"-r"     repl-opt
     "--repl" repl-opt
     "-m"     main-opt
     "--main" main-opt
     nil      null-opt
     "-h"     help-opt
     "--help" help-opt
     "-?"     help-opt} opt)
   script-opt))


;;;; Public API

(defn main :- nil
  "Usage: java -cp dunaj.jar dunaj.main [init-opt*] [main-opt] [arg*]

  With no options or args, runs an interactive Read-Eval-Print Loop

  init options:
    -i, --init path     Load a file or resource
    -e, --eval string   Evaluate expressions in string,
                        print non-nil values

  main options:
    -m, --main ns-name  Call the -main function from a namespace
                        with args
    -r, --repl          Run a repl
    path                Run a script from from a file or resource
    -                   Run a script from standard input
    -h, -?, --help      Print this help message and exit

  operation:

    - Establishes thread-local bindings for commonly set!-able vars
    - Enters the dunaj.user namespace
    - Binds command line args so that they can be retrieved with
      (command-line-args)
    - Runs all init options in order
    - Calls a -main function or runs a repl or script if requested

  The init options may be repeated and mixed freely, but must appear
  before any main option. The appearance of any eval option before
  running a repl suppresses the usual repl greeting message:
  \"Dunaj ~(canonical (current-version))\".

  Paths may be absolute or relative in the filesystem or relative to
  classpath. Classpath-relative paths have prefix of @ or @/"
  {:added v1}
  [& args :- Any]
  (try
    (if args
      (loop [[opt arg & more :as args] args inits []]
        (if (init-dispatch opt)
          (recur more (conj inits [opt arg]))
          ((main-dispatch opt) args inits)))
      (repl-opt nil nil))
    (finally
      (flush!))))

(defn -main
  [& args]
  (apply main args))
