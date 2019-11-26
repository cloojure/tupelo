;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.chars
  "Tupelo - Making Clojure even sweeter"
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    #?(:cljs [goog.string :as gstring] )
    ))

;-----------------------------------------------------------------------------
(def code-point-backspace
  "The unicode code-point for the backspace char."
  8)
(def code-point-tab
  "The unicode code-point for the horizontal tab char."
  9)
(def code-point-newline
  "The unicode code-point for the newline char."
  10)
(def code-point-tab-vertical
  "The unicode code-point for the vertical tab char."
  11)
(def code-point-formfeed
  "The unicode code-point for the form-feed char."
  12)
(def code-point-return
  "The unicode code-point for the return char."
  13)
(def code-point-escape
  "The unicode code-point for the escape char."
  27)
(def code-point-del
  "The unicode code-point for the delete char."
  127)
;-----------------------------------------------------------------------------

; #todo: docstrings
(s/def whitespace-horiz :- tsk/Set
  "Horizontal whitespace (space & tab)"
  (set [\space \tab]))

(s/def whitespace-eol :- tsk/Set
  "Vertical whitespace (return & newline)"
  (set [\return \newline]))

(s/def whitespace :- tsk/Set
  "All whitespace (vertical & horizontal)"
  (t/glue whitespace-horiz whitespace-eol))

(s/def lowercase :- tsk/Set
  "Lowercase chars a-z"
  (into (sorted-set) (t/chars-thru \a \z)))

(s/def uppercase :- tsk/Set
  "Uppercase chars A-Z"
  (into (sorted-set) (t/chars-thru \A \Z)))

(s/def digit :- tsk/Set
  "Digit chars 0-9"
  (into (sorted-set) (t/chars-thru \0 \9)))

(s/def hex :- tsk/Set
  "Hexadecimal chars 0-9, a-f, A-F"
  (into (sorted-set) (flatten [(t/chars-thru \a \f) (t/chars-thru \A \F) (t/chars-thru \0 \9)])))

(s/def alpha :- tsk/Set
  "All alphabetic chars (either case)"
  (t/glue lowercase uppercase))

(s/def alphanumeric :- tsk/Set
  "All alphabetic & digit chars"
  (t/glue alpha digit))

(s/def visible :- tsk/Set
  "Set of all visible (printing) ASCII chars from exclamation point (33) to tilde (126).
  Excludes all whitespace & control chars."
  (into (sorted-set) (mapv t/codepoint->char (t/thru 33 126))))

(s/def visible-no-dquote :- tsk/Set
  "All visible (printing) ASCII chars except double-quote."
  (set/difference visible #{\"}))

(s/def visible-no-squote :- tsk/Set
  "All visible (printing) ASCII chars except single-quote."
  (set/difference visible #{\'}))

(s/def text :- tsk/Set
  "Set of chars used in 'normal' text. Includes all visible chars plus whitespace & EOL chars."
  (t/glue visible whitespace))

(defn alphanumeric?
  "Returns true iff char is in tupelo.chars/alphanumeric"
  [ch] (contains? alphanumeric ch))
(defn whitespace-horiz?
  "Returns true iff char is in tupelo.chars/whitespace-horiz"
  [ch] (contains? whitespace-horiz ch))
(defn whitespace-eol?
  "Returns true iff char is in tupelo.chars/whitespace-eol"
  [ch] (contains? whitespace-eol ch))
(defn whitespace?
  "Returns true iff char is in tupelo.chars/whitespace"
  [ch] (contains? whitespace ch))
(defn lowercase?
  "Returns true iff char is in tupelo.chars/lowercase"
  [ch] (contains? lowercase ch))
(defn uppercase?
  "Returns true iff char is in tupelo.chars/uppercase"
  [ch] (contains? uppercase ch))
(defn digit?
  "Returns true iff char is in tupelo.chars/digit"
  [ch] (contains? digit ch))
(defn hex?
  "Returns true iff char is in tupelo.chars/hex"
  [ch] (contains? hex ch))
(defn alpha?
  "Returns true iff char is in tupelo.chars/alpha"
  [ch] (contains? alpha ch))
(defn visible?
  "Returns true iff char is in tupelo.chars/visible"
  [ch] (contains? visible ch))
(defn text?
  "Returns true iff char is in tupelo.chars/text"
  [ch] (contains? text ch))

#?(:cljs
   (do

     ;-----------------------------------------------------------------------------
     (def char-nbsp (gstring/unescapeEntities "&nbsp;")) ; get a char we can use in hiccup
     ;-----------------------------------------------------------------------------
     (defn nbsp
       "Return a string of N non-breaking-space (NBSP) chars (default=1)."
       ([] (nbsp 1))
       ([N] (str/join (repeat N char-nbsp))))

     ; #todo => tupelo.cljs.key-value-string ???
     (def kvs-enter "Enter")
     (def kvs-tab "Tab")
     (def kvs-escape "Escape")


     ))



