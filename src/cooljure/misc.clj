(ns cooljure.misc
  (:require [clojure.string             :as str]
            [clojure.java.io            :as io] ))

(set! *warn-on-reflection* false)

(defn normalize-str
  "Returns a 'normalized' version of the input string, stripped of leading/trailing
  blanks, and with all non-alphanumeric chars converted to hyphens."
  [orig-str]
  (-> orig-str
      str/trim
      (str/replace #"[^a-zA-Z0-9]" "-") ))

(defn str->kw
  "Returns a keyword from the normalized input string."
  [orig-str]
  (keyword (normalize-str orig-str)) )

(defn take-dist
  "Returns a sequence of N items from a collection, distributed
  evenly between first & last elements, which are always included."
  ; AWTAWT TODO: write tests, incl degenerate cases of N=0,1,2, etc
  [n coll]
  (let [interval    (Math/round (double (/ (count coll) (- n 1))))
        result      (flatten [ (take (- n 1) (take-nth interval coll))
                               (last coll) ] )
       ] result ))

