;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.strvec
  (:use clojure.test )
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.strvec :as ts]
  ))
(t/refer-tupelo)

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

; #todo add generative testing?
; #todo add clojure.spec testing?


(deftest t-take
  (is (= [        ] (ts/take 0 "abc")))
  (is (= [\a      ] (ts/take 1 "abc")))
  (is (= [\a \b   ] (ts/take 2 "abc")))
  (is (= [\a \b \c] (ts/take 3 "abc")))
  (is (= [\a \b \c] (ts/take 4 "abc"))))

(deftest t-drop
  (is (= [\a \b \c] (ts/drop 0 "abc")))
  (is (= [   \b \c] (ts/drop 1 "abc")))
  (is (= [      \c] (ts/drop 2 "abc")))
  (is (= [        ] (ts/drop 3 "abc")))
  (is (= [        ] (ts/drop 4 "abc"))))

