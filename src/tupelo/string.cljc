;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.string
  "Tupelo - Making Clojure even sweeter"
  (:refer-clojure :exclude [drop take] )
  (:require
    [clojure.core :as clj]
    [clojure.string :as str]
    [schema.core :as s]
  ))

(s/defn drop :- s/Str  ; #todo add readme
  "Drops the first N chars of a string, returning a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join (clj/drop n txt)))

(s/defn take :- s/Str  ; #todo add readme
  "Drops the first N chars of a string, returning a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join (clj/take n txt)))

(s/defn indent :- s/Str  ; #todo add readme
  "Indents a string by pre-pending N spaces. Returns a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (let [indent-str (str/join (repeat n \space))]
    (str indent-str txt)))

(s/defn indent-lines :- s/Str  ; #todo add readme
  "Splits out each line of txt using clojure.string/split-lines, then
  indents each line by prepending N spaces. Joins lines together into
  a single string result, with each line terminated by a single \newline."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join
    (for [line (str/split-lines txt) ]
      (str (indent n line) \newline))))

(s/defn increasing :- s/Bool
  "Returns true if a pair of strings are in increasing lexicographic order."
  [a :- s/Str
   b :- s/Str ]
  (neg? (compare a b)))

(s/defn increasing-or-equal :- s/Bool
  "Returns true if a pair of strings are in increasing lexicographic order, or equal."
  [a :- s/Str
   b :- s/Str ]
  (or (= a b)
      (increasing a b)))

(defn index-of [search-str tgt-str]
  (.indexOf search-str tgt-str))

(defn starts-with? [search-str tgt-str]
  (zero? (index-of search-str tgt-str)))

; #todo add undent (verify only leading whitespace removed)
; #todo add undent-lines
