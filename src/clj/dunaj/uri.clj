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

(ns dunaj.uri
  "RFC 3986 URI type.

  A data reader literal `#uri` is available for
  a convenient URI creation."
  {:authors ["Jozef Wagner"]}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [U]]
   [dunaj.boolean :refer [Boolean]]
   [dunaj.math :refer [Integer]]
   [dunaj.compare :refer [IComparable identical?]]
   [dunaj.flow :refer [if condp]]
   [dunaj.poly :refer [deftype]]
   [dunaj.coll :refer [ILookup]]
   [dunaj.function :refer [defn]]
   [dunaj.string :refer [ICanonical String ->str]]
   [dunaj.identifier :refer [name INamed]]))


;;;; Public API

(deftype Uri
  "A URI type.

  Use dunaj.coll/get to obtains parts of the URI. Following keys are
  supported: `:scheme`, `:scheme-specific-part`, `:authority`,
  `:user-info`, `:host`, `:port`, `:path`, `:query` and `:fragment`."
  {:added v1
   :see '[uri]
   :predicate 'uri?}
  java.net.URI
  ICanonical
  (-canonical [this] (.toString this))
  ILookup
  (-contains? [this key] (#{:scheme :scheme-specific-part :authority
                            :user-info :host :port :path :query
                            :fragment} key))
  (-get [this key not-found]
    (condp identical? key
      :scheme (.getScheme this)
      :scheme-specific-part (.getSchemeSpecificPart this)
      :authority (.getAuthority this)
      :user-info (.getUserInfo this)
      :host (.getHost this)
      :port (.getPort this)
      :path (.getPath this)
      :query (.getQuery this)
      :fragment (.getFragment this)
      not-found))
  ;; following protocols are already implemented
  IComparable)

(defn uri :- Uri
  "Returns URI from its canonical string representation `_val_`.
  or from `_scheme_`, `_host_` and `_port_`.
  Returns `_val_` if it already is a URI object."
  {:added v1
   :see '[uri?]}
  ([val :- (U String Uri)]
   (if (uri? val) val (java.net.URI/create val)))
  ([scheme :- INamed, host :- String, port :- Integer]
   (java.net.URI. (name scheme) nil host port nil nil nil)))

;;; Predicates

(defn absolute? :- Boolean
  "Returns true if `_uri_` has a scheme, otherwise returns false."
  {:added v1
   :see '[opaque?]}
  [uri :- Uri]
  (.isAbsolute uri))

(defn opaque? :- Boolean
  "Returns true if `_uri_` is absolute and its scheme specific part
  does not begin with slash `/`."
  {:added v1
   :see '[absolute?]}
  [uri :- Uri]
  (.isOpaque uri))

;;; Transformations

(defn normalize :- Uri
  "Returns normalized `_uri_`."
  {:added v1
   :see '[relativize resolve]}
  [uri :- Uri]
  (.normalize uri))

(defn relativize :- Uri
  "Returns relativized `_uri_` againsts `_root_` uri."
  {:added v1
   :see '[normalize resolve]}
  [root :- Uri, uri :- Uri]
  (.relativize root uri))

(defn resolve :- Uri
  "Returns uri created by resolving `_val_` against `_root_` uri.
  Calls `uri` on `_val_`."
  {:added v1
   :see '[normalize relativize]}
  [root :- Uri, val :- (U String Uri)]
  (.resolve root (uri val)))

;;; data reader

(clojure.core/defmethod clojure.core/print-method
  java.net.URI [x ^java.io.Writer w]
  (.write w (->str "#uri \"" (.toString ^java.net.URI x) "\"")))

(clojure.core/defmethod clojure.core/print-dup java.net.URI [o w]
  (clojure.core/print-method o w))

(clojure.core/alter-var-root #'clojure.core/default-data-readers
                             clojure.core/assoc 'uri #'dunaj.uri/uri)
