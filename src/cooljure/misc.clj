;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure.
            Misc functions."
      :author "Alan Thompson"}
  cooljure.misc
  (:require [clojure.string     :as str]) )

(defn normalize-str
 "[orig-str]
  Returns a 'normalized' version of orig-str, stripped of leading/trailing
  blanks, and with all non-alphanumeric chars converted to hyphens."
  [orig-str]
  (-> orig-str
      str/trim
      (str/replace #"[^a-zA-Z0-9]" "-") ))
  ; AWTAWT TODO: replace with other lib

(defn str->kw
 "[orig-str]
  Returns a keyword from the normalized orig-str"
  [orig-str]
  (keyword (normalize-str orig-str)) )
  ; AWTAWT TODO: replace with other lib

(defn take-dist
 "[n coll]
  Returns a sequence of n items from a collection, distributed
  evenly between first & last elements, which are always included."
  ; AWTAWT TODO: write tests, incl degenerate cases of N=0,1,2, etc
  [n coll]
  (let [interval    (Math/round (double (/ (count coll) (- n 1))))
        result      (flatten [ (take (- n 1) (take-nth interval coll))
                               (last coll) ] )
       ] result ))

