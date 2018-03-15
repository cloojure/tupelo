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

(defmacro vals->map
  "Called with a list of symbols like `(vals->map a b c)` returns a map
   like {:a a :b b :c c}.

       (let [a 1
             b 2
             c 3]
         (vals->map a b c))  ;=>  {:a 1 :b 2 :c 3} }

   See `with-map-vals` for simple destructuring of such maps."
  [& symbols]
  `(i/vals->map ~@symbols))

(defmacro with-map-vals
  "Given a map like {:a 1 :b 2 :c 3} (such as generated by `(vals->map a b c)`),
  performs safe `let` destructuring using `grab` like:

     (let [some-map  {:a 1 :b 2 :c 3} } ]
       (with-map-vals some-map [a b c]
          (+ a b c)))  ;=>  6

  `with-map-vals` is safe for typos since `grab` will throw is the requrested map key is not present.
  See `vals->map` for simple creation of labelled data maps."
  [the-map items-vec & forms]
  `(i/with-map-vals ~the-map ~items-vec ~@forms))
