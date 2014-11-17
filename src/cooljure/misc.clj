;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.misc
  "Cooljure - Cool stuff you wish was in Clojure.  Misc functions."
  (:require [clojure.string     :as str])
  (:use cooljure.core))

(defn normalize-str
 "[str-in]
  Returns a 'normalized' version of str-in, stripped of leading/trailing
  blanks, and with all non-alphanumeric chars converted to hyphens."
  [str-in]
  (-> str-in
      str/trim
      (str/replace #"[^a-zA-Z0-9]" "-") ))
  ; AWTAWT TODO: replace with other lib

(defn str->kw
 "[str-in]
  Returns a keyword constructed from the normalized str-in"
  [str-in]
  (keyword (normalize-str str-in)) )
  ; AWTAWT TODO: replace with other lib

(defn take-dist
 "[n coll]
  Returns a sequence of n items from a collection, distributed
  evenly between first & last elements, which are always included."
  ; AWTAWT TODO: write tests, incl degenerate cases of N=0,1,2, etc
  [n coll]
  {:pre [(pos? n)] }
  (if (= n 1)
    (first coll)
    (let [interval    (Math/round (double (/ (count coll) (- n 1))))
          result      (flatten [ (take (- n 1) (take-nth interval coll))
                                 (last coll) ] ) ] 
      result )))

(defn char-seq
  "Given two characters (or numerical equivalents), returns a seq of characters
  (inclusive) from the first to the second.  Characters must be in ascending order."
  [start-char stop-char]
  {:pre [ (char start-char) (char stop-char) ] }
    ; These "dummy" casts are to ensure that any input integer values are within the valid
    ; range for Unicode characters
  (let [start-val   (int start-char)
        stop-val    (int stop-char)]
    (when-not (<= start-val stop-val)
      (throw (IllegalArgumentException. 
        (str "char-seq: start-char must come before stop-char."
        "  start-val=" start-val "  stop-val=" stop-val))))
    (mapv char (range start-val (inc stop-val)))))

(defn seq->str
  "Convert a seq into a string (using pr) with a space preceding each value"
  [seq-in]
  (with-out-str
    (doseq [it (seq seq-in)]
      (print \space)
      (pr it))))

