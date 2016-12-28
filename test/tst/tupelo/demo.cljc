;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.demo
  (:use clojure.test)
  (:require
    [clojure.string :as str]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t] ))
(t/refer-tupelo)

; #todo add generative testing?
; #todo add clojure.spec testing?

(deftest t-take
  (is= [        ] (take 0 "abc"))
  (is= [\a      ] (take 1 "abc"))
  (is= [\a \b   ] (take 2 "abc"))
  (is= [\a \b \c] (take 3 "abc"))
  (is= [\a \b \c] (take 4 "abc")))

(deftest t-drop
  (is= [\a \b \c] (drop 0 "abc"))
  (is= [   \b \c] (drop 1 "abc"))
  (is= [      \c] (drop 2 "abc"))
  (is= [        ] (drop 3 "abc"))
  (is= [        ] (drop 4 "abc")))

(deftest t-string
  (is= [\a \b \c] (vec "abc"))
  (is= "abc" (str/join [\a \b \c] ))
)

