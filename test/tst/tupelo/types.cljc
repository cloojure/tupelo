;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.types
  (:use tupelo.types clojure.test)
  (:require [schema.core :as s] ))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(deftest t-boolean?
  (is (boolean? true))
  (is (boolean? false))
  (is (not (boolean? :hello)))
  (is (not (boolean? "hello")))
  (is (not (boolean? 'hello)))
  (is (not (boolean? 1)))
  (is (not (boolean? 0)))
  (is (not (boolean? 3.14)))
  (is (not (boolean? \K)))
)

; #awt #todo:  Add other tests
