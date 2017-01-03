;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.async
  "tupelo - Making Clojure even sweeter"
  (:require [clojure.core.async :as async]
            [tupelo.core :as t]
            [schema.core :as s] )
  (:refer-clojure :exclude [vec] )
  (:gen-class))
(t/refer-tupelo)

; #todo finish this? 
; (defn refer-tupelo-async [] nil)
; #todo or just copy all core.async as alias?
;   (:require [clojure.core.async 
;      :refer [ go go-loop chan buffer close! thread alts! alts!! timeout ]] )

; #todo add tests
; #todo add docs to README
(defmacro put-go!
  "Puts a value onto a channel. Only valid in a `go` block.
   Will park if channel is full. Equivalent to clojure.core.async/>! "
  [ch val]
  `(async/>! ~ch ~val))

(defmacro take-go!
  "Takes a value from a channel and returns it. Only valid in a `go` block.
   Will park if no value is available. Equivalent to clojure.core.async/<!  "
  [ch]
  `(async/<! ~ch))

(defn put-now! [& args]
  "Puts a value onto a channel.
  Will block if channel is full. Equivalent to `clojure.core.async/<!!`  "
  (apply async/>!! args))

(defn take-now! [& args]
  "Takes a value from a channel and returns it.
  Will block if no value is available. Equivalent to `clojure.core.async/<!!`  "
  (apply async/<!! args))

(defn put-later! [& args]
  (apply async/put! args))

(defn take-later! [& args]
  (apply async/take! args))

(defn vec
  "Extract all values from a channel into a vector."
  [ch]
  (take-now! (async/into [] ch)))
