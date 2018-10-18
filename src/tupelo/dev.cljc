;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
#?(:clj (do

(ns tupelo.dev
  "Code under development"
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.impl :as i] ))

(defn find-idxs-impl
  [idxs data tgt]
  (apply i/glue
    (i/forv [[idx val] (i/indexed data)]
      (let [idxs-curr (i/append idxs idx)]
           (if (sequential? val) ; #todo does not work for vector tgt
             (find-idxs-impl idxs-curr val tgt)
             (if (= val tgt)
               [{:idxs idxs-curr :val val}]
               []))))))

(s/defn find-idxs
  "Given an N-dim data structure (nested vectors/lists) & a target value, returns
  a list of maps detailing where index values where the target value is found.

    (is= (find-idxs  [[ 1  2 3]
                      [10 11  ]
                      [ 9  2 8]]  2)
      [{:idxs [0 1], :val 2}
       {:idxs [2 1], :val 2}]) "
  [data  :- [s/Any]
   tgt :- s/Any]
  (find-idxs-impl [] data tgt))

(defn combinations-duplicate [coll n]
  "Returns all combinations of elements from the input collection, presevering duplicates."
  (let [values     (vec coll)
        idxs       (range (count values))
        idx-combos (combo/combinations idxs n)
        combos     (i/forv [idx-combo idx-combos]
                     (mapv #(nth values %) idx-combo))]
    combos))

(defn parse-string [line]
  (mapv read-string (str/split line #" ")))

) )
