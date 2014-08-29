;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "cooljure/explicit - functions that avoid ambiguity"
      :author "Alan Thompson"}
  cooljure.explicit
    (:refer-clojure :exclude [get get-in] )
    (:require [clojure.string               :as str]
              [clojure.core                 :as clj]
              [cooljure.core                :as cool] ))

(defn get 
  "A fail-fast version of get. For map m & key k, returns the value v associated with k in
  m.  Throws an exception if k is not present in m."
  [m k]
  (if (contains? m k)
    (clj/get m k)
    (throw (IllegalArgumentException.    
              (str  "Key not present in map:" \newline
                    "  map: " m  \newline
                    "  key: " k  \newline )))))

(defn get-in
  "A fail-fast version of get-in. For map m & keys ks, returns the value v associated with ks in
  m, as for (get-in m ks). Throws an exception if the path ks is not present in m."
  [m  ks]
  (let [result (clj/get-in m ks ::not-found) ]
    (if (= result ::not-found)
      (throw (IllegalArgumentException.    
                (str  "Key seq not present in map:" \newline
                      "  map : " m  \newline
                      "  keys: " ks  \newline )))
      result )))

; awtawt TODO:  add in dissoc-in as (update-in ... dissoc)
;
; awtawt TODO:  add in dissoc-empty-vals to recursively delete all k-v pairs 
;               where val is nil or empty?
