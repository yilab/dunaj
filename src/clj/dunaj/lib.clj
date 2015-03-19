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

(ns dunaj.lib
  "Facilities for loading and compiling libs.

  NOTE: Documentation needs more work."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [scratch v1]]
   [dunaj.type :refer [Any AnyFn U I Maybe Fn Macro]]
   [dunaj.boolean :refer [and not or]]
   [dunaj.host :refer [class-instance? keyword->class proxy set!]]
   [dunaj.host.int :refer [iint iinc i-1 i0 i< iadd]]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [io! reset!]]
   [dunaj.flow :refer [if let do when when-let cond recur loop]]
   [dunaj.feature :refer [config]]
   [dunaj.poly :refer [deftype satisfies?]]
   [dunaj.coll :refer
    [IRed slice not-empty red? homogeneous? item-type empty? seq first
     next]]
   [dunaj.function :refer [defn]]
   [dunaj.host.array :refer [aset-char!]]
   [dunaj.host.batch :refer [item-types-match?]]
   [dunaj.string :refer [String last-index-of]]
   [dunaj.identifier :refer [symbol?]]
   [dunaj.state.basic :refer [local]]
   [dunaj.state.var :refer [defalias var with-bindings]]
   [dunaj.uri :refer [Uri]]
   [dunaj.coll.util :refer [merge]]
   [dunaj.format :refer [parse]]
   [dunaj.format.charset :refer [utf-8]]
   [dunaj.resource :refer
    [IAcquirableFactory IImmutableReadable IReadable
     readable? read! acquire! read]]
   [dunaj.resource.helper]
   [dunaj.resource.host :refer [coll-reader]]))


;;;; Public API

(defalias ns
  "Same as `clojure.core/ns`, with additional support for `:api`."
  {:added v1
   :see '[loaded dunaj.env/in-ns dunaj.env/in-bare-ns]
   :indent :all
   :highlight :def
   :tsig Macro})

(defalias loaded
  "Returns a sorted set of symbols naming the currently loaded libs."
  {:added v1
   :see '[load! require!]
   :tsig (Fn [IRed])}
  clojure.core/loaded-libs)

(defn load! :- Any
  "Reads and evaluates data from `_x_`, where the location of data is
  given in `_uri_` or in `_path_` and `_name_`. Returns the result of
  evaluation of last expression in data source. `_x_` may be one of:

  * reducible collection (e.g. string, vector, ...)
  * resources or resource factories
    (e.g. file resource, classpath resource)
  * host reader object (e.g. java.io.Reader instance in JVM)

  If input data is in bytes, it is automatically parsed with utf-8
  parser before reading."
  {:added v1
   :see '[compile! require! loaded ns]}
  ([x :- (U IRed java.io.Reader IAcquirableFactory IReadable)]
   (load! x nil nil))
  ([x :- (U IRed java.io.Reader IAcquirableFactory IReadable),
    uri :- (U nil String Uri)]
   (if (nil? uri)
     (load! x nil nil)
     (let [uri (dunaj.uri/uri uri)
           path (.toString uri)
           name (when-let [i (last-index-of path \/)]
                  (slice path (iinc i)))]
       (load! x path name))))
  ([x :- (U IRed java.io.Reader IAcquirableFactory IReadable),
    path :- (Maybe String), name :- (Maybe String)]
   (let [be? (and (empty? path) (empty? name))]
     (cond
       ;; java Reader instance
       (class-instance? java.io.Reader x)
       (io!
        ;;(clojure.core/println "loading..." path name)
        (clojure.lang.Compiler/load
         x (not-empty path)
         (if (empty? name) "NO_SOURCE_FILE" name)))
       ;; byte coll
       (and (homogeneous? x)
            (item-types-match? (keyword->class :byte) (item-type x)))
       (recur (parse utf-8 x) path name)
       ;; reducible, assume chars
       (red? x) (recur (coll-reader x) path name)
       ;; opened resource
       (readable? x)
       (if be?
         (load! (read! x) (:uri (config x)))
         (recur (read! x) path name))
       ;; acquirable factory
       :else (recur (acquire! x) path name)))))

(defalias require!
  "Same as `clojure.core/require`.

  Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that
  identifies multiple libs whose names share a common prefix,
  or a flag that modifies how all the identified libs are loaded.

  Use `:require` in the `ns` macro in preference to calling this
  directly."
  {:added v1
   :see '[load! compile!]}
  clojure.core/require)

(defn compile! :- nil
  "Compiles given `_lib_`.
  May supply additional options (e.g. `:path`)."
  {:added v1
   :see '[dunaj.env/compiling? dunaj.env/current-file require! load!]}
  [lib & {:as opts}]
  (let [path (or (:path opts) clojure.core/*compile-path*)
        opts (merge clojure.core/*compiler-options* opts)]
    (with-bindings {(var clojure.core/*compile-path*) path
                    (var clojure.core/*compiler-options*) opts}
      (clojure.core/compile lib))
    nil))
