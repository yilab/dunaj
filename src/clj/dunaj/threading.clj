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

(ns dunaj.threading
  "Syntax threading macros."
  {:authors ["Jozef Wagner"]
   :categories
   ["Primary"
    ["Second"
     "More syntax threading macros are available in
     <<dunaj.threading.second.api.ad#,dunaj.threading.second>>
     namespace"]
    ["Last"
     "More syntax threading macros are available in
     <<dunaj.threading.last.api.ad#,dunaj.threading.last>>
     namespace."]]
   :additional-copyright true}
  (:api bare)
  (:require
   [clojure.bootstrap :refer [defalias v1]]
   [dunaj.type :refer [Macro]]))


;;;; Public API

(defalias ->
  "Threads the expr `_x_` through the `_forms_`. Inserts `_x_` as the
  second item in the first form, making a list of it if it is not
  a list already. If there are more forms, inserts the first form as
  the second item in second form, etc."
  {:added v1
   :see '[->> dunaj.threading.second/do]
   :category "Second"
   :indent 1
   :highlight :flow
   :tsig Macro})

(defalias ->>
  "Threads the expr `_x_` through the `_forms_`. Inserts `_x_` as the
  last item in the first form, making a list of it if it is not
  a list already. If there are more forms, inserts the first form as
  the last item in second form, etc."
  {:added v1
   :category "Last"
   :see '[-> dunaj.threading.last/do]
   :indent 1
   :highlight :flow
   :tsig Macro})

(defalias as->
  "Binds `_name_` to `_expr_`, evaluates the first form in the lexical
  context of that binding, then binds name to that result, repeating
  for each successive form, returning the result of the last form."
  {:added v1
   :category "Primary"
   :see '[dunaj.threading.second/let dunaj.threading.last/let]
   :indent 2
   :highlight :flow
   :tsig Macro})

(defalias cond->
  "Takes an expression and a set of test/form pairs.
  Threads `_expr_` (via `pass:[->]`) through each form for which the
  corresponding test expression is logically true.

  NOTE: Unlike cond branching, `pass:[cond->]` threading does not
  short circuit after the first true test expression."
  {:added v1
   :category "Second"
   :see '[cond->> dunaj.threading.second/if]
   :indent 1
   :indent-group 2
   :highlight :flow
   :tsig Macro})

(defalias cond->>
  "Takes an expression and a set of test/form pairs.
  Threads `_expr_` (via `pass:[->>]`) through each form for which the
  corresponding test expression is logically true.

  NOTE: Unlike cond branching, `pass:[cond->>]` threading does not
  short circuit after the first true test expression."
  {:added v1
   :category "Last"
   :see '[cond-> dunaj.threading.last/if]
   :indent 1
   :highlight :flow
   :indent-group 2
   :tsig Macro})

(defalias some->
  "When `_expr_` is not `nil`, threads it into the first form
  (via `pass:[->]`), and when that result is not `nil`, through the
  next etc."
  {:added v1
   :category "Second"
   :see '[some->> dunaj.threading.second/when]
   :indent 1
   :highlight :flow
   :tsig Macro})

(defalias some->>
  "When `_expr_` is not `nil`, threads it into the first form
  (via `pass:[->>]`), and when that result is not `nil`,
  through the next etc."
  {:added v1
   :category "Last"
   :see '[some-> dunaj.threading.last/when]
   :indent 1
   :highlight :flow
   :tsig Macro})
