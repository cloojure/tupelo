;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.array.mutable
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.string :as ts]
    [tupelo.schema :as tsk]

    #?(:clj  [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty forv vals->map glue truthy? falsey?]]
       :cljs [tupelo.core :as t :include-macros true
              :refer [spy spyx spyxx spy-pretty spyx-pretty forv vals->map glue truthy? falsey?]])
    )
  (:import [java.util Arrays]))

#?(:clj
   (do    ; #todo fix this

     (def Array
       "Plumatic Schema type definition for a 2-D array of values (a vector of vectors)."
       {:data  s/Any
        :nrows s/Int
        :ncols s/Int})

     (s/defn num-rows :- s/Int
       "Returns the number of rows of an Array."
       [arr :- Array]
       (:nrows arr))

     (s/defn num-cols :- s/Int
       "Returns the number of cols of an Array."
       [arr :- Array]
       (:ncols arr))

     (s/defn ^:no-doc check-row-idx
       [arr :- Array
        idx :- s/Int]
       (let [limit (num-rows arr)]
         (when-not (< -1 idx limit)
           (throw (ex-info "Row index out of range" (vals->map idx limit))))))

     (s/defn ^:no-doc check-col-idx
       [arr :- Array
        idx :- s/Int]
       (let [limit (num-cols arr)]
         (when-not (< -1 idx limit)
           (throw (ex-info "Col index out of range" (vals->map idx limit))))))

     (s/defn ^:no-doc check-array-indexes
       [arr :- Array
        ii :- s/Int
        jj :- s/Int]
       (check-row-idx arr ii)
       (check-col-idx arr jj))

     (s/defn ^:no-doc idx :- s/Int
       [arr :- Array
        ii :- s/Int
        jj :- s/Int]
       (check-array-indexes arr ii jj)
       (+ (* ii (:ncols arr)) jj))

     (s/defn create :- Array
       "Return a new Array (vector-of-vectors) of size=[nrows ncols], initialized to `init-val` (default=nil)"
       ([nrows :- s/Int
         ncols :- s/Int]
        (create nrows ncols nil))
       ([nrows :- s/Int
         ncols :- s/Int
         init-val :- s/Any]
        (assert (and (pos? nrows) (pos? ncols)))
        (let [num-elems (* nrows ncols)
              result    (glue
                          (vals->map nrows ncols)
                          {:data (object-array num-elems)})]
          (Arrays/fill (:data result) init-val)
          result)))

     (s/defn elem-get :- s/Any
       "Gets an Array element"
       [arr :- Array
        ii :- s/Int
        jj :- s/Int]
       (aget (:data arr) (idx arr ii jj)))

     (s/defn elem-set :- Array
       "Puts a value into an Array element, returning the updated Array."
       [arr :- Array
        ii :- s/Int
        jj :- s/Int
        newVal :- s/Any]
       (aset (:data arr) (idx arr ii jj) newVal)
       arr)

     (s/defn array->str :- s/Str
       "Returns a string representation of an array"
       [arr :- Array]
       (let [result (str/join
                      (flatten
                        (for [ii (range (num-rows arr))]
                          (t/append
                            (for [jj (range (num-cols arr))]
                              (let [val (str (elem-get arr ii jj))]
                                (ts/pad-left val 8)))
                            \newline))))]
         result))

     (s/defn array->edn :- [[s/Any]]
       "Returns a persistant EDN data structure (vector-of-vectors) from the array."
       [arr :- Array]
       (mapv vec (partition (:ncols arr) (:data arr))))

     (s/defn rows->array :- Array
       "Return a new Array initialized from row-vecs. Rows must all have same length."
       [row-vecs :- [[s/Any]]]
       (let [nrows (count row-vecs)
             ncols (count (first row-vecs))]
         (assert (apply = ncols (mapv count row-vecs)))
         (dotimes [ii nrows]
           (assert sequential? (nth row-vecs ii)))
         (let [arr (create nrows ncols)]
           (dotimes [ii nrows]
             (dotimes [jj ncols]
               (elem-set arr ii jj
                 (get-in row-vecs [ii jj]))))
           arr)))

     (s/defn edn->array :- Array
       "Synonym for rows->array"
       [row-vecs :- [[s/Any]]]
       (rows->array row-vecs))

     (s/defn equals :- s/Bool
       "Returns true if two Arrays contain equal data"
       [x :- Array
        y :- Array]
       (and
         (= (:nrows x) (:nrows y))
         (= (:ncols x) (:ncols y))
         (= (seq (:data x)) (seq (:data y)))))

     (s/defn row-get :- tsk/Vec
       "Gets an Array row"
       [arr :- Array
        ii :- s/Int]
       (check-row-idx arr ii)
       (forv [jj (range (num-cols arr))]
         (elem-get arr ii jj)))

     (s/defn col-get :- tsk/Vec
       "Gets an Array col"
       [arr :- Array
        jj :- s/Int]
       (check-col-idx arr jj)
       (forv [ii (range (num-rows arr))]
         (elem-get arr ii jj)))

     (s/defn array->row-vals :- tsk/Vec
       "Returns the concatenation of all array rows."
       [arr :- Array]
       (forv [ii (range (num-rows arr))
              jj (range (num-cols arr))]
         (elem-get arr ii jj)))

     (s/defn array->col-vals :- tsk/Vec
       "Returns the concatenation of all array cols."
       [arr :- Array]
       (forv [jj (range (num-cols arr))
              ii (range (num-rows arr))]
         (elem-get arr ii jj)))

     (s/defn row-vals->array :- Array
       "Return a new Array of size=[nrows ncols] with its rows constructed from from row-data."
       [nrows :- s/Int
        ncols :- s/Int
        row-data :- tsk/Vec]
       (assert (and (pos? nrows) (pos? ncols)))
       (assert (= (* nrows ncols) (count row-data)))
       (let [result (glue
                      (vals->map nrows ncols)
                      {:data (object-array row-data)})]
         result))

     (s/defn transpose :- Array
       "Returns the transpose of an array. Returns a new array."
       [orig :- Array]
       (row-vals->array (:ncols orig) (:nrows orig)
         (array->col-vals orig)))

     (s/defn col-vals->array :- Array
       "Return a new Array of size=[nrows ncols] with its columns constructed from from col-data."
       [nrows :- s/Int
        ncols :- s/Int
        col-data :- tsk/Vec]
       (assert (and (pos? nrows) (pos? ncols)))
       (assert (= (* nrows ncols) (count col-data)))
       (let [result (create nrows ncols)]
         (dotimes [ii nrows]
           (dotimes [jj ncols]
             (elem-set result ii jj
               (nth col-data (+ ii (* jj nrows))))))
         result))

     (s/defn flip-ud :- Array
       "Flips an array in the up-down direction,
       reversing the order of the rows of an array. Returns a new array."
       [orig :- Array]
       (let [nrows  (:nrows orig)
             ncols  (:ncols orig)
             result (create nrows ncols)]
         (dotimes [ii nrows]
           (let [ii-orig (- nrows ii 1)]
             (dotimes [jj ncols]
               (elem-set result ii jj
                 (elem-get orig ii-orig jj)))))
         result))

     (s/defn flip-lr :- Array
       "Flips an array in the left-right direction,
       reversing the order of the cols of an array. Returns a new array."
       [orig :- Array]
       (let [nrows  (:nrows orig)
             ncols  (:ncols orig)
             result (create nrows ncols)]
         (dotimes [jj ncols]
           (let [jj-orig (- ncols jj 1)]
             (dotimes [ii nrows]
               (elem-set result ii jj
                 (elem-get orig ii jj-orig)))))
         result))

     (s/defn rotate-left :- Array
       "Rotates an array 90 deg counter-clockwise. Returns a new array."
       [orig :- Array]
       (let [M      (:nrows orig)
             N      (:ncols orig)
             result (create N M)] ; transpose shape
         (dotimes [ii M]
           (dotimes [jj N]
             (let [i2 (- N 1 jj)
                   j2 ii]
               ;(spyx [[ii jj] [i2 j2]])
               (elem-set result i2 j2
                 (elem-get orig ii jj)))))
         result))

     (s/defn rotate-right :- Array
       "Rotates an array 90 deg counter-clockwise. Returns a new array."
       [orig :- Array]
       (let [M      (:nrows orig)
             N      (:ncols orig)
             result (create N M)] ; transpose shape
         (dotimes [ii M]
           (dotimes [jj N]
             (let [j2 (- M 1 ii)
                   i2 jj]
               ;(spyx [[ii jj] [i2 j2]])
               (elem-set result i2 j2
                 (elem-get orig ii jj)))))
         result))

     ;#todo make both rows/cols -> submatrix result
     (s/defn array->rows :- [[s/Any]]
       "Usage:
          (array->rows arr)           Returns all array rows
          (array->rows arr row-idxs)  Returns array rows specified by row-idxs
          (array->rows arr low high)  Returns array rows in half-open interval [low..high) "
       ([arr] (array->rows arr 0 (num-rows arr)))
       ([arr
         row-idxs :- [s/Int]]
        (forv [ii row-idxs]
          (row-get arr ii)))
       ([arr :- Array
         low :- s/Int
         high :- s/Int]
        (check-row-idx arr low)
        (check-row-idx arr (dec high))
        (assert (< low high))
        (forv [ii (range low high)]
          (row-get arr ii))))
     ; #todo need parallel rows-set

     (s/defn array->cols :- [[s/Any]]
       "Usage:
          (array->cols arr)           Returns all array cols
          (array->cols arr col-idxs)  Returns array cols specified by col-idxs
          (array->cols arr low high)  Returns array cols in half-open interval [low..high) "
       ([arr] (array->cols arr 0 (num-cols arr)))
       ([arr
         col-idxs :- [s/Int]]
        (forv [jj col-idxs]
          (col-get arr jj)))
       ([arr :- Array
         low :- s/Int
         high :- s/Int]
        (check-col-idx arr low)
        (check-col-idx arr (dec high))
        (assert (< low high))
        (forv [jj (range low high)]
          (col-get arr jj))))
     ; #todo need parallel cols-set

     (s/defn cols->array :- Array
       "[col-vecs]
       Return a new Array initialized from col-vecs. Cols must all have same length."
       [col-vecs :- [[s/Any]]]
       (let [ncols (count col-vecs)
             nrows (count (first col-vecs))]
         (assert (apply = nrows (mapv count col-vecs)))
         (dotimes [jj ncols]
           (assert sequential? (nth col-vecs jj)))
         (col-vals->array nrows ncols (apply glue col-vecs))))

     (s/defn symmetric? :- s/Bool
       "Returns true iff an array is symmetric"
       [arr :- Array]
       (let [nrows (num-rows arr)
             ncols (num-cols arr)]
         (and (= nrows ncols)
           (every? truthy?
             (for [ii (range 0 nrows)
                   jj (range ii ncols)] (= (elem-get arr ii jj)
                                          (elem-get arr jj ii)))))))

     (s/defn row-set :- Array
       "Sets an Array row"
       [arr :- Array
        ii :- s/Int
        new-row :- tsk/Vec]
       (check-row-idx arr ii)
       (let [ncols (num-cols arr)]
         (assert (= ncols (count new-row)))
         (dotimes [jj ncols]
           (elem-set arr ii jj
             (nth new-row jj)))
         arr))

     (s/defn col-set :- Array
       "Sets an Array col"
       [arr :- Array
        jj :- s/Int
        new-col :- tsk/Vec]
       (check-col-idx arr jj)
       (let [nrows (num-rows arr)]
         (assert (= nrows (count new-col)))
         (dotimes [ii nrows]
           (elem-set arr ii jj
             (nth new-col ii)))
         arr))

     (s/defn row-drop :- Array
       "Drop one or more rows from an array"
       [orig :- Array
        & idxs-drop :- [s/Int]]
       (let [idxs-all  (set (range (num-rows orig)))
             idxs-drop (set idxs-drop)
             idxs-keep (sort (set/difference idxs-all idxs-drop))]
         (rows->array
           (forv [ii idxs-keep]
             (row-get orig ii)))))

     (s/defn col-drop :- Array
       "Drop one or more cols from an array"
       [orig :- Array
        & idxs-drop :- [s/Int]]
       (let [idxs-all  (set (range (num-cols orig)))
             idxs-drop (set idxs-drop)
             idxs-keep (sort (set/difference idxs-all idxs-drop))]
         (cols->array
           (forv [jj idxs-keep]
             (col-get orig jj)))))

     (s/defn rows-append :- Array
       "Appends one or more rows onto an array. Returns a new array."
       [arr :- Array
        & rows :- [tsk/Vec]]
       (let [nrows-orig  (num-rows arr)
             nrows-added (count rows)
             nrows-total (+ nrows-orig nrows-added)
             row-lens    (mapv count rows)]
         (assert (apply = (num-cols arr) row-lens))
         (row-vals->array nrows-total (num-cols arr)
           (glue
             (array->row-vals arr)
             (apply glue rows)))))

     (s/defn cols-append :- Array
       "Appends one or more cols onto an array. Returns a new array."
       [arr :- Array
        & cols :- [tsk/Vec]]
       (let [ncols-orig  (num-cols arr)
             ncols-added (count cols)
             ncols-total (+ ncols-orig ncols-added)
             col-lens    (mapv count cols)]
         (assert (apply = (num-rows arr) col-lens))
         (col-vals->array (num-rows arr) ncols-total
           (glue
             (array->col-vals arr)
             (apply glue cols)))))

     (s/defn glue-vert :- Array
       "Concatenates 2 or more arrays vertically. Arrays must all have the same number of cols. Returns a new array."
       [& arrays :- [Array]]
       (assert (pos? (count arrays)))
       (let [ncol-vals (mapv num-cols arrays)]
         (assert (apply = ncol-vals)))
       (let [nrows-total (reduce + (mapv num-rows arrays))
             ncols       (num-cols (first arrays))]
         (row-vals->array nrows-total ncols
           (apply glue
             (mapv array->row-vals arrays)))))

     (s/defn glue-horiz :- Array
       "Concatenates 2 or more arrays horizontally. Arrays must all have the same number of rows. Returns a new array."
       [& arrays :- [Array]]
       (assert (pos? (count arrays)))
       (let [nrow-vals (mapv num-rows arrays)]
         (assert (apply = nrow-vals)))
       (let [ncols-total (reduce + (mapv num-cols arrays))
             nrows       (num-rows (first arrays))]
         (col-vals->array nrows ncols-total
           (apply glue
             (mapv array->col-vals arrays)))))

     ))



