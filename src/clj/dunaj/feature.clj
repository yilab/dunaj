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

(ns dunaj.feature
  "Feature protocols for metadata, configuration and validation."
  {:authors ["Jozef Wagner"]
   :categories ["Metadata" "Configuration"]}
  (:api bare)
  (:require
   [clojure.core :refer [atom extend-type apply satisfies?]]
   [clojure.bootstrap :refer [deftype defn defprotocol defalias v1]]
   [dunaj.type :refer [Any KeywordMap I Fn AnyFn]]
   [dunaj.compare :refer [identical?]]
   [dunaj.state :refer
    [IMutable IReference IAtomic atomic? reset! alter!]]
   [dunaj.flow :refer [when if let do]]))


;;;; Public API

(defprotocol IValidator
  "A feature protocol for objects that can be validated."
  {:added v1
   :see '[validator]
   :predicate 'validable?}
  (-validator :- (I IMutable IReference)
    "Returns mutable reference to the current validator fn.
    Referenced value is `nil` if no validator is set."
    [this]))

(defn validator :- (I IMutable IReference)
  "Returns mutable reference to the current validator fn attached
  to `_x_`. Referenced value is `nil` if no validator is set."
  {:added v1
   :see '[validable?]}
  [x :- IValidator]
  (-validator x))

(deftype ^:private Validator4IRef
  [^clojure.lang.IRef x]
  IReference
  (-deref [this] (.getValidator x))
  IMutable
  (-reset! [this f] (.setValidator x f) f))

(extend-type clojure.lang.IRef
  IValidator
  (-validator [this] (->Validator4IRef this)))

(defprotocol IMeta
  "A feature protocol for objects carying metadata."
  {:added v1
   :see '[meta]
   :category "Metadata"
   :on-interface clojure.lang.IMeta
   :forbid-extensions true}
  (-meta :- KeywordMap
    "Returns metadata map attached to `_this_`."
    {:on 'meta}
    [this]))

(defalias meta
  "Returns the metadata map attached to object `_x_`, returns `nil`
  if there is no metadata."
  {:added v1
   :see '[meta-red assoc-meta]
   :arglists '([x])
   :category "Metadata"
   :tsig (Fn [KeywordMap Any])})

(defprotocol IPersistentMeta
  "A feature protocol for objects with persistent metadata."
  {:added v1
   :see '[IMeta IMutableMeta]
   :category "Metadata"
   :on-interface clojure.lang.IObj
   :forbid-extensions true}
  (-assoc-meta :- IPersistentMeta
    "Returns `_this_` with `_m_` associated as object's metadata."
    {:on 'withMeta}
    [this m :- KeywordMap]))

(defalias assoc-meta
  "Returns `_x_` with `_m_` associated as object's metadata."
  {:added v1
   :see '[update-meta meta]
   :arglists '([x m])
   :category "Metadata"
   :tsig (Fn [IPersistentMeta IPersistentMeta KeywordMap])}
  clojure.core/with-meta)

(defprotocol IMutableMeta
  "A feature protocol for objects with mutable metadata."
  {:added v1
   :see '[IMeta IPersistentMeta]
   :category "Metadata"}
  (-meta-ref :- (I IMutable IReference)
    "Returns mutable reference to object's metadata."
    [this]))

(defn meta-ref :- (I IMutable IReference)
  "Returns mutable reference to object's metadata."
  {:added v1
   :see '[reset-meta! alter-meta!]
   :category "Metadata"}
  [x :- IMutableMeta]
  (-meta-ref x))

(deftype ^:private MutableMeta4IReference
  [^clojure.lang.IReference x]
  IReference
  (-deref [this] (.meta x))
  IMutable
  (-reset! [this newval] (.resetMeta x newval))
  IAtomic
  (-cas! [this oldval newval]
    (let [a (atom true)
          f #(do (reset! a true)
                 (if (identical? % oldval)
                   newval
                   (do (reset! a false) %)))]
      (.alterMeta x f nil)
      @a)))

(extend-type clojure.lang.IReference
  IMutableMeta
  (-meta-ref [this] (->MutableMeta4IReference this)))

(defprotocol IConfig
  "A feature protocol for configured objects."
  {:added v1
   :see '[config]
   :category "Configuration"}
  (-config :- KeywordMap
    "Returns a configuration attached to `_this_` object."
    [this]))

(defn config :- KeywordMap
  "Returns configuration map for a given object, or `nil` if
  no configuration is attached to the object."
  {:added v1
   :see '[config-ref assoc-config]
   :category "Configuration"}
  [x :- Any]
  (when (satisfies? IConfig x) (-config x)))

(defprotocol IPersistentConfig
  "A feature protocol for objects with persistent configuration."
  {:added v1
   :see '[IConfig IMutableConfig]
   :category "Configuration"}
  (-assoc-config :- IPersistentConfig
    "Returns `_this_` with `_conf_` object associated as its
    configuration."
    [this conf :- KeywordMap]))

(defn assoc-config :- IPersistentConfig
  "Returns an object of the same type and value as `_x_`, with
  `_conf_` as its configuration."
  {:added v1
   :see '[update-config config]
   :category "Configuration"}
  [x :- IPersistentConfig, conf :- KeywordMap]
  (-assoc-config x conf))

(defprotocol IMutableConfig
  "A feature protocol for mutable configuration."
  {:added v1
   :see '[IConfig IPersistentConfig]
   :category "Configuration"}
  (-config-ref :- (I IMutable IReference)
    "Returns mutable reference to object's configuration."
    [this]))

(defn config-ref :- (I IMutable IReference)
  "Returns mutable reference to object's configuration.

  NOTE: Returned reference also usually implements
  `<<dunaj.state.spi.ad#IAdjustable,IAdjustable>>` protocol."
  {:added v1
   :see '[reset-config! alter-config!]
   :category "Configuration"}
  [x :- IMutableConfig]
  (-config-ref x))

;;; Convenience functions

(defn update-meta :- IPersistentMeta
  "Returns an object of the same type and value as `_x_`, with
  `(apply _f_ (meta _x_) args)` as its metadata."
  {:added v1
   :see '[assoc-meta meta]
   :category "Metadata"}
  [x :- IPersistentMeta, f :- AnyFn & args :- Any]
  (assoc-meta x (apply f (meta x) args)))

(defn update-config :- IPersistentConfig
  "Returns an object of the same type and value as `_x_`, with
  `(apply _f_ (config _x_) args)` as its configuration."
  {:added v1
   :see '[assoc-config config]
   :category "Configuration"}
  [x :- IPersistentConfig, f :- AnyFn & args :- Any]
  (assoc-config x (apply f (config x) args)))

(defn alter-meta! :- Any
  "Changes metadata with `(apply _f_ meta args)`.
  Returns new metadata. Mutates `_x_`."
  {:added v1
   :see '[reset-meta! meta-ref meta]
   :category "Metadata"}
  [x :- IMutableMeta, f :- AnyFn & args :- Any]
  (let [mr (meta-ref x)]
    (if (atomic? mr)
      (apply alter! mr f args)
      (reset! mr (apply f @mr args)))))

(defn reset-meta! :- Any
  "Resets metadata to `_m_`.
  Returns new metadata. Mutates `_x_`."
  {:added v1
   :see '[alter-meta! meta-ref meta]
   :category "Metadata"}
  [x :- IMutableMeta, m :- KeywordMap]
  (reset! (meta-ref x) m))

(defn alter-config! :- Any
  "Changes configuration with `(apply _f_ config args)`.
  Returns new configuration. Mutates `_x_`."
  {:added v1
   :see '[reset-config! config-ref config]
   :category "Configuration"}
  [x :- IMutableConfig, f :- AnyFn & args :- Any]
  (let [cr (config-ref x)]
    (if (atomic? cr)
      (apply alter! cr f args)
      (reset! cr (apply f @cr args)))))

(defn reset-config! :- Any
  "Resets configuration to `_conf_`.
  Returns new configuration. Mutates `_x_`."
  {:added v1
   :see '[alter-config! config-ref config]
   :category "Configuration"}
  [x :- IMutableConfig, conf :- KeywordMap]
  (reset! (config-ref x) conf))
