;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.gen
  "Tupelo - Making Clojure even sweeter"
  (:refer-clojure :exclude [rand-nth constantly] )
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [clojure.test.check :as tc]
    [clojure.test.check.clojure-test :as tst]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [potemkin.namespaces :as pns]
    [schema.core :as s]
    [tupelo.impl :as i]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))

; #todo: for test.check v2:
;     fmap        -> map
;     elements    -> rand-nth

; #todo need tests & doc

(defn txt-join
  "Wraps the supplied generator using clojure.string/join."
  [xgen]
  (gen/fmap str/join xgen))

(defn txt-join-space
  "Wraps the supplied generator using #(clojure.string/join \\space %)."
  [xgen]
  (gen/fmap #(str/join \space %) xgen))

(defn maybe-vec
  "Given a string generator, randomly return either the original string or a vector of its chars."
  [gen-arg]
  (gen/one-of [ (gen/fmap vec gen-arg)
                (gen/fmap identity gen-arg) ] ))

(defn rand-nth
  "Returns a generator yielding a random element from the supplied collection.
  Equivalent to `gen/elements` "
  [coll]
  (gen/elements coll))

(defn constantly
  "Returns a generator which always yields the supplied argument w/o.
  Equivalent to `gen/return` "
  [arg]
  (gen/return arg))

(defn vector+
  "Return a non-empty vector (1 or more items) selected using the supplied generator."
  [gen-arg]
  (gen/not-empty (gen/vector gen-arg)))

; returns exactly 1 char
(def char-whitespace-horiz      (rand-nth ts/chars-whitespace-horiz))
(def char-whitespace-eol        (rand-nth ts/chars-whitespace-eol))
(def char-whitespace            (rand-nth ts/chars-whitespace))
(def char-alpha                 (rand-nth ts/chars-alpha))
(def char-alphanumeric          (rand-nth ts/chars-alphanumeric))
; #todo char-visible, char-visible-no-dquote, char-visible-no-squote,

; returns a string (len = 0 or more)
(def whitespace-horiz           (txt-join (gen/vector char-whitespace-horiz)))
(def whitespace-eol             (txt-join (gen/vector char-whitespace-eol)))
(def whitespace                 (txt-join (gen/vector char-whitespace)))

; returns a string (len = 1 or more)
(def whitespace-horiz+          (txt-join (vector+ char-whitespace-horiz)))
(def whitespace-eol+            (txt-join (vector+ char-whitespace-eol)))
(def whitespace+                (txt-join (vector+ char-whitespace)))

; returns a string (len = 0 or more)
(def word-alpha                 (txt-join       (gen/vector char-alpha)))
(def word-alphanumeric          (txt-join       (gen/vector char-alphanumeric)))
(def words-alpha                (txt-join-space (gen/vector word-alpha)))
(def words-alphanumeric         (txt-join-space (gen/vector word-alphanumeric)))

; returns a string (len = 1 or more)
(def word-alpha+                (txt-join       (vector+ char-alpha)))
(def word-alphanumeric+         (txt-join       (vector+ char-alphanumeric)))
(def words-alpha+               (txt-join-space (vector+ word-alpha+)))
(def words-alphanumeric+        (txt-join-space (vector+ word-alphanumeric+)))

(def identifier (gen/let [first-char    char-alpha
                          other-chars   word-alphanumeric]
                  (str first-char other-chars)))

(def char-eol
  "A single EOL char"
  (rand-nth ts/chars-whitespace-eol))
(def chars-eol+
  "One or more EOL chars."
  (vector+ char-eol))

