;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.dev
  "Code under development"
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t] ))
(t/refer-tupelo)

(defn find-idxs-impl
  [idxs data tgt]
  (apply glue
    (forv [[idx val] (indexed data)]
      (let [idxs-curr (append idxs idx)]
           (if (sequential? val) ; #todo does not work for vector tgt
             (find-idxs-impl idxs-curr val tgt)
             (if (= val tgt)
               [{:idxs idxs-curr :val val}]
               [nil]))))))

(s/defn find-idxs
  "Given a vector of nested vectors (or lists) nested"
  [data  :- [s/Any]
   tgt :- s/Any]
  (keep-if not-nil? (find-idxs-impl [] data tgt)))

(defn combinations-duplicate [coll n]
  "Returns all combinations of elements from the input collection, presevering duplicates."
  (let [values     (vec coll)
        idxs       (range (count values))
        idx-combos (combo/combinations idxs n)
        combos     (forv [idx-combo idx-combos]
                     (mapv #(nth values %) idx-combo))]
    combos))

(defn parse-string [line]
  (mapv read-string (str/split line #" ")))

