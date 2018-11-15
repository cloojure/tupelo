;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.char
  "Tupelo - Making Clojure even sweeter"
  #?(:clj (:refer-clojure :exclude [drop take]))
  (:require
    [clojure.set :as set]
    #?@(:clj [
              [schema.core :as s]
              [tupelo.core :as t]
              [tupelo.schema :as tsk]])))

     ; #todo: docstrings
     (s/def whitespace-horiz :- tsk/Set
       (set [\space \tab]))

     (s/def whitespace-eol :- tsk/Set
       (set [\return \newline]))

     (s/def whitespace :- tsk/Set
       (t/glue whitespace-horiz whitespace-eol))

     (s/def lowercase :- tsk/Set
       (into (sorted-set) (t/chars-thru \a \z)))

     (s/def uppercase :- tsk/Set
       (into (sorted-set) (t/chars-thru \A \Z)))

     (s/def digit :- tsk/Set
       (into (sorted-set) (t/chars-thru \0 \9)))

     (s/def hex :- tsk/Set
       (into (sorted-set) (flatten [(t/chars-thru \a \f) (t/chars-thru \A \F) (t/chars-thru \0 \9)])))

     (s/def alpha :- tsk/Set
       (t/glue lowercase uppercase))

     (s/def alphanumeric :- tsk/Set
       (t/glue alpha digit))

     (s/def visible :- tsk/Set
       "Set of all visible (printing) ASCII chars from exclamation point (33) to tilde (126).
       Excludes all whitespace & control chars."
       (into (sorted-set) (mapv t/int->char (t/thru 33 126))))

     (s/def visible-no-dquote :- tsk/Set
       "All visible (printing) ASCII chars except double-quote."
       (set/difference visible #{\"}))

     (s/def visible-no-squote :- tsk/Set
       "All visible (printing) ASCII chars except double-quote."
       (set/difference visible #{\'}))

     (s/def text :- tsk/Set
       "Set of chars used in 'normal' text. Includes all visible chars plus whitespace & EOL chars."
       (t/glue visible whitespace))

     (defn alphanumeric? [ch] (contains? alphanumeric ch))
     (defn whitespace-horiz? [ch] (contains? whitespace-horiz ch))
     (defn whitespace-eol? [ch] (contains? whitespace-eol ch))
     (defn whitespace? [ch] (contains? whitespace ch))
     (defn lowercase? [ch] (contains? lowercase ch))
     (defn uppercase? [ch] (contains? uppercase ch))
     (defn digit? [ch] (contains? digit ch))
     (defn hex? [ch] (contains? hex ch))
     (defn alpha? [ch] (contains? alpha ch))
     (defn visible? [ch] (contains? visible ch))
     (defn text? [ch] (contains? text ch))

