;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.string
  "Tupelo - Making Clojure even sweeter"
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

; Prismatic Schema type definitions
(s/set-fn-validation! true)  ; #todo add to Schema docs
; #todo add to project.clj (esp for tupelo-app template, user/dev profile)

(s/defn tupstr-drop :- s/Str  ; #todo -> tupelo.string
  "Drops the first N chars of a string, returning a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join (cc/drop n txt)))

(s/defn tupstr-take :- s/Str  ; #todo -> tupelo.string
  "Drops the first N chars of a string, returning a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join (cc/take n txt)))

