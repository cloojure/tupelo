;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.strvec
  "Manipulate vectors as strings of characters."
  (:refer-clojure :exclude [drop take] )
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.core :as tsch]
  ))
(t/refer-tupelo)

; Prismatic Schema type definitions
(s/set-fn-validation! true)  ; #todo add to Schema docs
; #todo add to project.clj (esp for tupelo-app template, user/dev profile)

; #todo add readme

(s/defn drop :- tsch/StrVec
  "Drops the first N chars of a sequence, returning a StrVec result."
  [n    :- s/Int
   txt  :- tsch/List]
  (vec (cc/drop n (seq txt))))

(s/defn take :- tsch/StrVec
  "Drops the first N chars of a sequence, returning a StrVec result."
  [n    :- s/Int
   txt  :- tsch/List]
  (vec (cc/take n (seq txt))))

(s/defn ->string :- s/Str
  "Drops the first N chars of a sequence, returning a StrVec result."
  [sv  :- tsch/StrVec]
  (str/join sv))

