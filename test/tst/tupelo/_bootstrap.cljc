;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo._bootstrap
  "This namespace is used to perform one-time tasks during testing, such as printing the
  Clojure version and enabling Plumatic Schema function validation."
  (:use clojure.test )
  (:require [schema.core :as s]
            [tupelo.core :as t] ))

; Prismatic Schema type definitions
; #todo add to Schema docs
(s/set-fn-validation! true) ; enforce fn schemas

(deftest t-bootstrap
  (t/print-versions))
