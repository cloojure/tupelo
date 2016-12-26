;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.version
  (:use tupelo.version
        clojure.test )
  (:require
    [clojure.core :as clj]
    [clojure.string :as str]
    [schema.core :as sk]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
  ))
(t/refer-tupelo)

; Prismatic Schema type definitions
(sk/set-fn-validation! true)   ; #todo add to Schema docs


;---------------------------------------------------------------------------------------------------
; Java version testing functions & macros

(defn fn-any [] 42)
(defn fn7 [] (min-java-1-7 7))
(defn fn8 [] (min-java-1-8 8))

(deftest t-java-version
  (when false
    (spyx (is-java-1-7?))
    (spyx (is-java-1-8?))
    (spyx (is-java-1-7-plus?))
    (spyx (is-java-1-8-plus?)))

  (when (is-java-1-7?)
    (t/throws? (fn8)))

  (when (is-java-1-8-plus?)
    (is= 8 (fn8)))

  (is= 7 (fn7))
  (is= 42 (fn-any))
)

