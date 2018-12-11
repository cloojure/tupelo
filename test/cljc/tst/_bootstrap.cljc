;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst._bootstrap
  "This namespace is used to perform one-time tasks during testing, such as printing the
  Clojure version."
  (:require
    [clojure.string :as str]
    #?@(:clj [
              [schema.core :as s]
              [tupelo.test   :refer [define-fixture dotest is isnt is= isnt= nonblank= testing throws?]]
              [tupelo.core :as i]
              [tupelo.string :as ts]
              ])
    #?@(:cljs [
               [schema.core :as s]
               [tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= nonblank= testing throws?]]
               [tupelo.core :as i :include-macros true]
               [tupelo.string :as ts :include-macros true]
               ])
  ))

#?(:cljs (enable-console-print!))

(s/set-fn-validation! true) ; enforce fn schemas

; Prismatic Schema type definitions
; #todo add to Schema docs
; (set! *warn-on-reflection* true)  ; #todo enable?

(dotest
  (i/print-versions)
  ;(spyx (s/fn-validation?))
  )
