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
    [clojure.core :as cc]
    [clojure.string :as str]
    [potemkin.namespaces :as pns]
    [schema.core :as s]
    [tupelo.impl :as i]
  ))

(pns/import-fn i/char-seq)

; #todo: docstrings
(def chars-whitespace-horiz   (set [\space \tab]))
(def chars-whitespace-eol     (set [\return \newline \formfeed]))
(def chars-whitespace         (into chars-whitespace-horiz chars-whitespace-eol))
(def chars-lowercase          (into (sorted-set) (char-seq \a \z)))
(def chars-uppercase          (into (sorted-set) (char-seq \A \Z)))
(def chars-digit              (into (sorted-set) (char-seq \0 \9)))
(def chars-alpha              (reduce into [chars-lowercase chars-uppercase] ))
(def chars-alphanumeric       (reduce into [chars-alpha chars-digit] ))
(def chars-visible
  "Set of all visible (printing) ASCII chars from exclamation point (33) to tilde (126). Excludes all whitespace & control chars."
  (into (sorted-set) (mapv char (i/thru 33 126))))
(def chars-text
  "Set of chars used in 'normal' text. Includes all visible chars plus whitespace & EOL chars."
  (into chars-visible chars-whitespace))

(defn alphanumeric?       [& args] (every? #(contains? chars-alphanumeric %) (i/strcat args)))
(defn whitespace-horiz?   [& args] (every? #(contains? chars-whitespace-horiz %) (i/strcat args)))
(defn whitespace-eol?     [& args] (every? #(contains? chars-whitespace-eol %) (i/strcat args)))
(defn whitespace?         [& args] (every? #(contains? chars-whitespace %) (i/strcat args)))
(defn lowercase?          [& args] (every? #(contains? chars-lowercase %) (i/strcat args)))
(defn uppercase?          [& args] (every? #(contains? chars-uppercase %) (i/strcat args)))
(defn digit?              [& args] (every? #(contains? chars-digit %) (i/strcat args)))
(defn alpha?              [& args] (every? #(contains? chars-alpha %) (i/strcat args)))
(defn visible?            [& args] (every? #(contains? chars-visible %) (i/strcat args)))
(defn text?               [& args] (every? #(contains? chars-text %) (i/strcat args)))

; #todo -> tupelo.string
(defn collapse-whitespace ; #todo readme & blog
  "Replaces all consecutive runs of whitespace characters (including newlines) with a single space.
   Removes any leading or trailing whitespace. Returns a string composed of all tokens
   separated by a single space."
  [it]
  (-> it
    str/trim
    (str/replace #"\s+" " ")))

(s/defn equals-ignore-spacing :- s/Bool  ; #todo readme & blog
  "Compares arguments for equality using tupelo.misc/collapse-whitespace.
   Equivalent to separating tokens by whitespace and comparing the resulting sequences."
  [& args :- [s/Str]]
  (let [ws-collapsed-args (mapv collapse-whitespace args)]
    (apply = ws-collapsed-args)))

; #todo need (squash)         -> (collapse-whitespace (strcat args))       ; (smash ...)         ?
; #todo need (squash-equals?) -> (apply = (mapv squash args))              ; (smash-equals? ...)  ?
;    or (equals-base) or (equals-root) or (squash-equals) or (base-equals) or (core-equals) or (equals-collapse-string...)

(s/defn double-quotes->single-quotes :- s/Str ; #todo readme & blog
  [arg :- s/Str]
  (str/replace arg \" \'))

; #todo -> tupelo.string
(s/defn single-quotes->double-quotes :- s/Str ; #todo readme & blog
  [arg :- s/Str]
  (str/replace arg \' \"))

; #todo need tests
(defn normalize-str
  "Returns a 'normalized' version of str-in, stripped of leading/trailing
   blanks, and with all non-alphanumeric chars converted to hyphens."
  [str-in]
  (-> str-in
    str/trim
    (str/replace #"[^a-zA-Z0-9]" "-")))
; #todo replace with other lib

; %todo define current mode only for (str->kw "ab*cd #()xyz" :sloppy), else throw
(defn str->kw-normalized       ; #todo need test, README
  "Returns a keyword constructed from a normalized string"
  [arg]
  (keyword (normalize-str arg)))

; #todo throw if bad string
(defn str->kw       ; #todo need test, README
  "Returns a keyword constructed from a normalized string"
  [arg]
  (keyword arg))

(defn kw->str       ; #todo need test, README
  "Returns the string version of a keyword, stripped of the leading ':' (colon)."
  [arg]
  (str/join (cc/drop 1 (str arg))))

(defn snake->kabob
  "Converts a string from a_snake_case_value to a-kabob-case-value"
  [arg]
  (str/replace arg \_ \- ))

(defn kabob->snake
  "Converts a string from a-kabob-case-value to a_snake_case_value"
  [arg]
  (str/replace arg \- \_ ))

(defn kw-snake->kabob [kw]
  (-> kw
    (kw->str)
    (snake->kabob)
    (str->kw)))

(defn kw-kabob->snake [kw]
  (->> kw
    (kw->str)
    (kabob->snake)
    (str->kw)))

;-----------------------------------------------------------------------------

(s/defn drop :- s/Str  ; #todo add readme
  "Drops the first N chars of a string, returning a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join (cc/drop n txt)))

(s/defn take :- s/Str  ; #todo add readme
  "Drops the first N chars of a string, returning a string result."
  [n    :- s/Int
   txt  :- s/Str]
  (str/join (cc/take n txt)))

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
