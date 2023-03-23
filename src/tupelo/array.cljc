;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.array
  #?(:cljs (:require-macros [tupelo.core]))
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty forv vals->map glue truthy? falsey?]]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    ))

; #todo:  scalar/{add,mul}
; #todo:  ebe/{add,mul}

;-----------------------------------------------------------------------------
(s/defn num-rows :- s/Int
  "Returns the number of rows of an Array."
  [arr :- tsk/Array]
  (count arr))

(s/defn num-cols :- s/Int
  "Returns the number of cols of an Array."
  [arr :- tsk/Array]
  (count (arr 0)))

(s/defn ^:no-doc check-row-idx
  [arr :- tsk/Array
   idx :- s/Int]
  (let [limit (num-rows arr)]
    (when-not (< -1 idx limit)
      (throw (ex-info "Row index out of range" (vals->map idx limit))))))

(s/defn ^:no-doc check-col-idx
  [arr :- tsk/Array
   idx :- s/Int]
  (let [limit (num-cols arr)]
    (when-not (< -1 idx limit)
      (throw (ex-info "Col index out of range" (vals->map idx limit))))))

(s/defn ^:no-doc check-array-indexes
  [arr :- tsk/Array
   ii :- s/Int
   jj :- s/Int]
  (check-row-idx arr ii)
  (check-col-idx arr jj))

;-----------------------------------------------------------------------------
(s/defn new :- tsk/Array
  "Return a new Array (vector-of-vectors) of size=[nrows ncols], initialized to `init-val` (default=nil)"
  ([nrows :- s/Int
    ncols :- s/Int]
   (tupelo.array/new nrows ncols nil))
  ([nrows :- s/Int
    ncols :- s/Int
    init-val :- s/Any]
   (assert (and (pos? nrows) (pos? ncols)))
   (forv [ii (range nrows)]
     (vec (repeat ncols init-val)))))

(s/defn zeros :- tsk/Array
  "Return a new Array (vector-of-vectors) of size=[nrows ncols], initialized to zero"
  [nrows :- s/Int
   ncols :- s/Int]
  (tupelo.array/new nrows ncols 0))

(s/defn ones :- tsk/Array
  "Return a new Array (vector-of-vectors) of size=[nrows ncols], initialized to one"
  [nrows :- s/Int
   ncols :- s/Int]
  (tupelo.array/new nrows ncols 1))

(s/defn elem-get :- s/Any
  "Gets an Array element"
  [arr :- tsk/Array
   ii :- s/Int
   jj :- s/Int]
  (check-array-indexes arr ii jj)
  (get-in arr [ii jj]))

(s/defn elem-set :- tsk/Array
  "Puts a value into an Array element, returning the updated Array."
  [arr :- tsk/Array
   ii :- s/Int
   jj :- s/Int
   newVal :- s/Any]
  (check-array-indexes arr ii jj)
  (assoc-in arr [ii jj] newVal))

(s/defn row-vals->array :- tsk/Array
  "Return a new Array of size=[nrows ncols] with its rows constructed from from row-data."
  [nrows :- s/Int
   ncols :- s/Int
   row-data :- tsk/Vec]
  (assert (and (pos? nrows) (pos? ncols)))
  (assert (= (* nrows ncols) (count row-data)))
  (mapv vec
    (partition ncols row-data)))

(s/defn col-vals->array :- tsk/Array
  "Return a new Array of size=[nrows ncols] with its columns constructed from from col-data."
  [nrows :- s/Int
   ncols :- s/Int
   col-data :- tsk/Vec]
  (assert (and (pos? nrows) (pos? ncols)))
  (assert (= (* nrows ncols) (count col-data)))
  (let [data-vec (vec col-data)]
    (forv [ii (range nrows)]
      (forv [jj (range ncols)]
        (nth data-vec (+ ii (* jj nrows)))))))

(s/defn edn-rows->array :- tsk/Array
  "Return a new Array initialized from row-vecs. Rows must all have same length."
  [row-vecs :- tsk/Array]
  (let [nrows (count row-vecs)
        ncols (count (first row-vecs))]
    (assert (apply = ncols (mapv count row-vecs)))
    (dotimes [ii nrows]
      (assert sequential? (nth row-vecs ii)))
    (mapv vec row-vecs)))

(s/defn edn-cols->array :- tsk/Array
  "[col-vecs]
  Return a new Array initialized from col-vecs. Cols must all have same length."
  [col-vecs :- tsk/Array]
  (let [ncols (count col-vecs)
        nrows (count (first col-vecs))]
    (assert (apply = nrows (mapv count col-vecs)))
    (dotimes [jj ncols]
      (assert sequential? (nth col-vecs jj)))
    (col-vals->array nrows ncols (apply glue col-vecs))))

(s/defn row-get :- tsk/Vec
  "Gets an Array row"
  [arr :- tsk/Array
   ii :- s/Int]
  (check-row-idx arr ii)
  (forv [jj (range (num-cols arr))]
    (elem-get arr ii jj)))

(s/defn col-get :- tsk/Vec
  "Gets an Array col"
  [arr :- tsk/Array
   jj :- s/Int]
  (check-col-idx arr jj)
  (forv [ii (range (num-rows arr))]
    (elem-get arr ii jj)))

(s/defn rows-append :- tsk/Array
  "Appends one or more rows onto an array."
  [orig :- tsk/Array
   & rows :- [tsk/Vec]]
  (let [row-lens (mapv count rows)]
    (assert (apply = (num-cols orig) row-lens)))
  (into orig rows))

(s/defn cols-append :- tsk/Array
  "Appends one or more cols onto an array."
  [orig :- tsk/Array
   & cols :- [tsk/Vec]]
  (let [nrows    (num-rows orig)
        col-lens (mapv count cols)]
    (assert (apply = nrows col-lens))
    (forv [ii (range nrows)]
      (glue (row-get orig ii) (forv [col cols]
                                (nth col ii))))))

(s/defn row-set :- tsk/Array
  "Sets an Array row"
  [orig :- tsk/Array
   ii :- s/Int
   new-row :- tsk/Vec]
  (check-row-idx orig ii)
  (assert (= (num-cols orig) (count new-row)))
  (let [nrows  (num-rows orig)
        result (glue
                 (forv [ii (range ii)] (row-get orig ii))
                 [new-row]
                 (forv [ii (range (inc ii) nrows)] (row-get orig ii)))]
    result))

(s/defn col-set :- tsk/Array
  "Sets an Array col"
  [orig :- tsk/Array
   jj :- s/Int
   new-col :- tsk/Vec]
  (check-col-idx orig jj)
  (let [nrows  (num-rows orig)
        >>     (assert (= nrows (count new-col)))
        result (forv [ii (range nrows)]
                 (let [curr-row (row-get orig ii)
                       new-val  (nth new-col ii)
                       new-row  (t/replace-at curr-row jj new-val)]
                   new-row))]
    result))

(s/defn row-drop :- tsk/Array
  "Drop one or more rows from an array"
  [orig :- tsk/Array
   & idxs-drop :- [s/Int]]
  (let [idxs-all  (set (range (num-rows orig)))
        idxs-drop (set idxs-drop)
        idxs-keep (sort (set/difference idxs-all idxs-drop))]
    (forv [ii idxs-keep]
      (row-get orig ii))))

(s/defn col-drop :- tsk/Array
  "Drop one or more colss from an array"
  [orig :- tsk/Array
   & idxs-drop :- [s/Int]]
  (let [idxs-all  (set (range (num-cols orig)))
        idxs-drop (set idxs-drop)
        idxs-keep (sort (set/difference idxs-all idxs-drop))]
    (forv [ii (range (num-rows orig))]
      (forv [jj idxs-keep]
        (elem-get orig ii jj)))))

;#todo make both rows/cols -> submatrix result
(s/defn array->rows :- tsk/Array
  "Usage:
     (array->rows arr)           Returns all array rows
     (array->rows arr row-idxs)  Returns array rows specified by row-idxs
     (array->rows arr low high)  Returns array rows in half-open interval [low..high) "
  ([arr] (array->rows arr 0 (num-rows arr)))
  ([arr row-idxs]
   (forv [ii row-idxs]
     (row-get arr ii)))
  ([arr :- tsk/Array
    low :- s/Int
    high :- s/Int]
   (check-row-idx arr low)
   (check-row-idx arr (dec high))
   (assert (< low high))
   (forv [ii (range low high)]
     (row-get arr ii))))
; #todo need parallel rows-set

(s/defn array->row-vals :- tsk/Vec
  "Returns the concatenation of all array rows."
  [arr :- tsk/Array]
  (apply glue arr))

(s/defn array->col-vals :- tsk/Vec
  "Returns the concatenation of all array cols."
  [arr :- tsk/Array]
  (forv [jj (range (num-cols arr))
         ii (range (num-rows arr))]
    (elem-get arr ii jj)))

(s/defn array->cols :- tsk/Array
  "Usage:
     (array->cols arr)           Returns all array cols
     (array->cols arr col-idxs)  Returns array cols specified by col-idxs
     (array->cols arr low high)  Returns array cols in half-open interval [low..high) "
  ([arr] (array->cols arr 0 (num-cols arr)))
  ([arr col-idxs]
   (forv [jj col-idxs]
     (col-get arr jj)))
  ([arr :- tsk/Array
    low :- s/Int
    high :- s/Int]
   (check-col-idx arr low)
   (check-col-idx arr (dec high))
   (assert (< low high))
   (forv [jj (range low high)]
     (col-get arr jj))))
; #todo need parallel cols-set

(s/defn transpose :- tsk/Array
  "Returns the transpose of an array"
  [orig :- tsk/Array]
  (forv [jj (range (num-cols orig))]
    (col-get orig jj)))

(s/defn flip-ud :- tsk/Array
  "Flips an array in the up-down direction,
  reversing the order of the rows of an array"
  [orig :- tsk/Array]
  (forv [ii (reverse (range (num-rows orig)))]
    (row-get orig ii)))

(s/defn flip-lr :- tsk/Array
  "Flips an array in the left-right direction,
  reversing the order of the cols of an array"
  [orig :- tsk/Array]
  (forv [ii (range (num-rows orig))]
    (vec (reverse (row-get orig ii)))))

(s/defn rotate-left :- tsk/Array
  "Rotates an array 90 deg counter-clockwise."
  [orig :- tsk/Array]
  (forv [jj (reverse (range (num-cols orig)))]
    (col-get orig jj)))

(s/defn rotate-right :- tsk/Array
  "Rotates an array 90 deg clockwise."
  [orig :- tsk/Array]
  (forv [jj (range (num-cols orig))]
    (vec (reverse (col-get orig jj))))) ; reverse yields a seq, not a vec! doh!

(s/defn glue-vert :- tsk/Array
  "Concatenates 2 or more arrays vertically. Arrays must all have the same number of cols."
  [& arrays :- [tsk/Array]]
  (assert (pos? (count arrays)))
  (let [ncol-vals (mapv num-cols arrays)]
    (assert (apply = ncol-vals)))
  (apply glue arrays))

(s/defn glue-horiz :- tsk/Array
  "Concatenates 2 or more arrays horizontally. Arrays must all have the same number of rows."
  [& arrays :- [tsk/Array]]
  (assert (pos? (count arrays)))
  (let [nrow-vals (mapv num-rows arrays)]
    (assert (apply = nrow-vals)))
  (forv [ii (range (num-rows (t/xfirst arrays)))]
    (apply glue (mapv #(row-get % ii) arrays))))

(s/defn diagonal-main :- tsk/Vec ; #todo: copy to mutable
  "Returns the main diagonal of an array"
  [arr :- tsk/Array]
  (assert (= (num-rows arr)
            (num-cols arr)))
  (let [rows-indexed (t/indexed arr)]
    (t/forv [[idx row] rows-indexed]
      (elem-get arr idx idx))))

(s/defn diagonal-anti :- tsk/Vec ; #todo: copy to mutable
  "Returns the anti-diagonal of an array"
  [arr :- tsk/Array]
  (diagonal-main (rotate-left arr)))

(s/defn symmetric? :- s/Bool
  "Returns true iff an array is symmetric"
  [arr :- tsk/Array]
  (let [nrows (num-rows arr)
        ncols (num-cols arr)]
    (and (= nrows ncols)
      (every? truthy?
        (for [ii (range 0 nrows)
              jj (range ii ncols)] (= (elem-get arr ii jj)
                                     (elem-get arr jj ii)))))))

(s/defn array->str :- s/Str
  "Returns a string representation of an array"
  [arr :- tsk/Array]
  (let [result (str/join
                 (flatten
                   (for [ii (range (num-rows arr))]
                     (t/append
                       (for [jj (range (num-cols arr))]
                         (let [val (str (elem-get arr ii jj))]
                           (ts/pad-left val 8)))
                       \newline))))]
    result))

