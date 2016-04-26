;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.async
  "tupelo - Making Clojure even sweeter"
  (:require [clojure.core                 :as clj]
            [clojure.core.async           :as clj.async]
            [schema.core                  :as s]
            [tupelo.types                 :as types]
            [tupelo.schema                :as ts]
            )
  (:gen-class))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs
; #todo add to project.clj (esp for tupelo-app template, user/dev profile)

; #todo finish this? 
; (defn refer-tupelo-async [] nil)
; #todo or just copy all core.async as alias?
;   (:require [clojure.core.async 
;      :refer [ go go-loop chan buffer close! thread alts! alts!! timeout ]] )

; #todo add tests
; #todo add docs to README
(defmacro put-go! [& args]
  `(clj.async/>! ~@args))

(defmacro take-go! [& args]
  `(clj.async/<! ~@args))

(defn put-now! [& args]
  (apply clj.async/>!! args))

(defn take-now! [& args]
  (apply clj.async/<!! args))

(defn put-later! [& args]
  (apply clj.async/put! args))

(defn take-later! [& args]
  (apply clj.async/take! args))

