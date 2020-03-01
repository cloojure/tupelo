;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.vec
  (:refer-clojure :exclude [load ->VecNode get set])
  #?(:clj (:require
            [clojure.core :as cc]
            [schema.core :as s]
            [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab glue map-entry indexed
                                       forv vals->map fetch-in let-spy xlast xfirst keep-if drop-if
                                       it-> cond-it-> xfirst xsecond xthird xlast ]]
            [tupelo.schema :as tsk]
            ))
  #?(:cljs (:require
             [clojure.core :as cc]
             [schema.core :as s]
             [tupelo.core :as t  :include-macros true
                  :refer [spy spyx spyxx spyx-pretty grab glue map-entry indexed
                          forv vals->map fetch-in let-spy xlast xfirst keep-if drop-if
                          it-> cond-it-> xfirst xsecond xthird xlast
                          ]
              ]
             [tupelo.schema :as tsk]
             )))

; #todo add indexes
; #todo add sets (primative only or EID) => map with same key/value
; #todo copy destruct syntax for search

#?(:cljs (enable-console-print!))

(s/defn coerce->vec :- tsk/Vec
  "Coerce any scalar values to a vector"
  [arg]
  (if (sequential? arg)
    (vec arg)
    [arg]))

(s/defn validate-indexes-complete :- s/Any ; #todo maybe => tupelo.data.indexing
  "Validates that a collection of N index values includes all values in [0..N)."
  [idxs]
  (let [expected-set (cc/set (range (count idxs)))
        actual-set   (cc/set idxs)]
    (assert (= expected-set actual-set)))
  idxs)

(s/defn verify-idxs
  "Assets that an integer index is non-negative and less than a bound"
  [tgt :- tsk/Vec
   idxs :- [s/Int]]
  (let [bound (count tgt)]
    (doseq [idx idxs]
      (assert (and (int? idx)
                (<= 0 idx) (< idx bound)))))
  idxs)

(defn pred-index
  "Given a predicate fn and a collection of values, returns the index values for which the
  predicate is true & false like:
    (pred-index #(zero? (rem % 3)) [0 10 20 30 40 50 60 70 80])
      => {:idxs-true   [0 3 6]
          :idxs-false  [1 2 4 5 7 8] } "
  [pred coll]
  (let [pred-indexed (indexed (mapv pred coll))
        grouped      (glue {true [] false []} ; ensure both t/f are present
                       (group-by xsecond pred-indexed))
        result       {:idxs-true  (it-> grouped
                                    (grab true it)
                                    (mapv xfirst it))
                      :idxs-false (it-> grouped
                                    (grab false it)
                                    (mapv xfirst it))}]
    result))

;  #todo maybe (set-idx dst idxs src)  ?   (<dest> <bridge> <src>)

(def IndexSpec
 (s/cond-pre [s/Int] s/Int) )

(s/defn get :- tsk/Vec
  "Given a source vector V and a list of index values `idx`, returns a vector: [ V(idx-1) V(idx-2) ...]"
  [src :- tsk/Vec
   idxs :- IndexSpec]
  (let [idxs      (coerce->vec idxs)
        bound-src (count src)]
    (verify-idxs src idxs)
    (forv [idx idxs]
      (nth src idx))))

(s/defn set :- tsk/Vec
  "Given a dest vector V, a list of index values `idx`, and a conforming src vector,
  returns a modified V such that V(idx-j) = src[j] for j in [0..len(idx)] "
  [dest :- tsk/Vec
   idxs :- IndexSpec
   src :- tsk/Vec]
  (let [idxs (coerce->vec idxs)
        src (coerce->vec src)]
    (verify-idxs dest idxs)
    (assert (= (count idxs) (count src)))
    (loop [result    (transient dest)
           idxs-vals idxs
           elems     src]
      (if (empty? idxs-vals)
        (persistent! result)
        (let [result-next (assoc! result (first idxs-vals) (first elems))]
          (recur result-next
            (rest idxs-vals)
            (rest elems)))))))

(s/defn set-lax :- tsk/Vec
  "Like vec-put-idxs, but is lax in accepting the src vector. If a scalar is supplied,
  it is replaced with `(repeat <src>)`. If the src vector is longer than the idxs, it is truncated
  to conform."
  [dest :- tsk/Vec
   idxs :- IndexSpec
   src :- s/Any]
  (let [idxs-len (count idxs)
        src-use  (cond
                   (not (sequential? src)) (repeat idxs-len src)
                   (< idxs-len (count src)) (t/xtake idxs-len src)
                   :else src)]
    (set dest idxs src-use)))

(s/defn del :- tsk/Vec
  "Delete elements at the specified indexes."
  [src :- tsk/Vec
   idxs :- IndexSpec]
  (let [drop-idx? (t/->set (coerce->vec idxs))
        idxs-keep (drop-if drop-idx? (range (count src)))]
    (verify-idxs src idxs)
    (forv [idx idxs-keep]
      (nth src idx))))










