;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; Additional copyright for parts of documentation and/or
;; underlying implementation:
;; Copyright (C) 2013, 2015, LonoCloud
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

(ns dunaj.threading.last
  "Macros for syntax threading into last item in forms.

  See <<dunaj.threading.second.api.ad#,dunaj.threading.second>> for
  macros which thread into second item instead.

  NOTE: Using https://github.com/LonoCloud/synthread[synthread] for
  underlying implementation.

  IMPORTANT: The diamond symbol `<>` is bound and rebound to the
  current topic as it is threaded through the forms.

  Idiomatic use is to alias this namespace under `pass:[->>]`
  symbol.

  NOTE: Documentation needs more work."
  {:authors ["Jozef Wagner"]
   :additional-copyright "2013, 2015, LonoCloud"}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [defalias v1]]
   [lonocloud.synthread-last]
   [dunaj.type :refer [Macro]]))


;;;; Public API

;; Copyright LonoCloud
;; https://github.com/LonoCloud/synthread

;; TODO: indents

(defalias do
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/do)

(defalias if
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/if)

(defalias if-let
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/if-let)

(defalias when
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/when)

(defalias when-not
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/when-not)

(defalias when-let
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/when-let)

(defalias cond
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/cond)

(defalias for
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/for)

(defalias let
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/let)

(defalias fn
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/fn)

(defalias first
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/first)

(defalias second
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/second)

(defalias nth
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/nth)

(defalias last
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/last)

(defalias rest
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/rest)

(defalias update
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/assoc)

(defalias update-in
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/in)

(defalias each
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/each)

(defalias each-as
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/each-as)

(defalias key
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/key)

(defalias val
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/val)

(defalias apply
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/apply)

(defalias reset
  {:added v1
   :tsig Macro}
  lonocloud.synthread-last/reset)
