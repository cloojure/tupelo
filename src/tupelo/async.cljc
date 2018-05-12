;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.async
  "tupelo - Making Clojure even sweeter"
  #?@(:clj [
  (:use tupelo.core)
  (:require [clojure.core.async :as async]
            [tupelo.core :as t]
            [schema.core :as s] )
  (:refer-clojure :exclude [vec] )
  (:gen-class)
            ])
      )

#?(:clj (do
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

(defn put-now!
  "Puts a value onto a channel.
  Will block if channel is full. Equivalent to `clojure.core.async/<!!`  "
  [ch val]
  (async/>!! ch val))

(defn take-now!
  "Takes a value from a channel and returns it.
  Will block if no value is available. Equivalent to `clojure.core.async/<!!`  "
  [ch]
  (async/<!! ch))

(defn put-later!
  "Puts a value onto a channel from another thread.
  Equivalent to `clojure.core.async/put!` "
  [port val & args]
  (apply async/put! port val args))

(defn take-later!
  "Takes a value from a channel & passes it to the supplied function, using another thread.
  Equivalent to `clojure.core.async/take!` "
  [& args]
  (apply async/take! args))

(defn vec
  "Extract all values from a channel into a vector."
  [ch]
  (take-now! (async/into [] ch)))

))
