;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.gen
  "Tupelo - Clojure With A Spoonful of Honey"
  #?@(:clj [
  (:refer-clojure :exclude [rand-nth constantly] )
  (:require
    [clojure.string :as str]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :as tst]
    [clojure.test.check.generators :as tcgen]
    [clojure.test.check.properties :as prop]
    [tupelo.core :as i]
    [tupelo.chars :as char]
  )
            ]))

; #todo: for test.check v2:
;     fmap        -> map

; #todo need tests & doc

; #todo
; maybe
;   [tupelo.test.check            :as ttc]
;   [tupelo.test.check.properties :as tprop]
;   [tupelo.test.check.generators :as tgen]

;   (gen/sample gen/int 20) -> (tgen/sample {:quantity 20} gen/int )
;                                             ^^^ ctx is always optional 1st arg
;   gen/sample-seq            -> tgen/->lazy-seq
;   tc/quick-check            -> (ttc/quick-check {:num-tests 100} ...)

#?(:clj (do
(defn txt-join
  "Wraps the supplied generator using clojure.string/join."
  [xgen]
  (tcgen/fmap str/join xgen))

(defn txt-join-space
  "Wraps the supplied generator using #(clojure.string/join \\space %)."
  [xgen]
  (tcgen/fmap #(str/join \space %) xgen))

(defn maybe-vec
  "Given a string generator, randomly return either the original string or a vector of its chars."
  [gen-arg]
  (tcgen/one-of [ (tcgen/fmap vec gen-arg)
                (tcgen/fmap identity gen-arg) ] ))

(defn rand-nth
  "Returns a generator yielding a random element from the supplied collection.
  Equivalent to `gen/elements` "
  [coll]
  (tcgen/elements coll))

(defn constantly
  "Returns a generator which always yields the supplied argument w/o.
  Equivalent to `gen/return` "
  [arg]
  (tcgen/return arg))

(defn vector+
  "Return a non-empty vector (1 or more items) selected using the supplied generator."
  [gen-arg]
  (tcgen/not-empty (tcgen/vector gen-arg)))

; returns exactly 1 char
(def char-whitespace-horiz      (rand-nth char/whitespace-horiz))
(def char-whitespace-eol        (rand-nth char/whitespace-eol))
(def char-whitespace            (rand-nth char/whitespace))
(def char-alpha                 (rand-nth char/alpha))
(def char-alphanumeric          (rand-nth char/alphanumeric))
; #todo char-visible, char-visible-no-dquote, char-visible-no-squote,

; returns a string (len = 0 or more)
(def whitespace-horiz           (txt-join (tcgen/vector char-whitespace-horiz)))
(def whitespace-eol             (txt-join (tcgen/vector char-whitespace-eol)))
(def whitespace                 (txt-join (tcgen/vector char-whitespace)))

; returns a string (len = 1 or more)
(def whitespace-horiz+          (txt-join (vector+ char-whitespace-horiz)))
(def whitespace-eol+            (txt-join (vector+ char-whitespace-eol)))
(def whitespace+                (txt-join (vector+ char-whitespace)))

; returns a string (len = 0 or more)
(def word-alpha                 (txt-join       (tcgen/vector char-alpha)))
(def word-alphanumeric          (txt-join       (tcgen/vector char-alphanumeric)))
(def words-alpha                (txt-join-space (tcgen/vector word-alpha)))
(def words-alphanumeric         (txt-join-space (tcgen/vector word-alphanumeric)))

; returns a string (len = 1 or more)
(def word-alpha+                (txt-join       (vector+ char-alpha)))
(def word-alphanumeric+         (txt-join       (vector+ char-alphanumeric)))
(def words-alpha+               (txt-join-space (vector+ word-alpha+)))
(def words-alphanumeric+        (txt-join-space (vector+ word-alphanumeric+)))

(def identifier (tcgen/let [first-char  char-alpha
                            other-chars word-alphanumeric]
                  (str first-char other-chars)))

(def char-eol
  "A single EOL char"
  (rand-nth char/whitespace-eol))
(def chars-eol+
  "One or more EOL chars."
  (vector+ char-eol))

))
