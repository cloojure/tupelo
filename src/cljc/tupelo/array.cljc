(ns tupelo.array
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [schema.core :as s]
    #?@(:clj [[tupelo.core :as t :refer [spy spyx spyxx forv  glue grab vals->map truthy?]]
              [tupelo.string :as ts]
              ])
    #?@(:cljs [[tupelo.core :as t :refer [spy spyx spyxx forv glue grab vals->map truthy?] :include-macros true]
               [tupelo.string :as ts :include-macros true]
               [reagent.format :as rfmt]
               ])
    ))

(def Vector
  "A 1-D array of values (a vector of vectors)."
  [s/Any] )

(def Array
  "A 2-D array of values (a vector of vectors)."
  [[s/Any]] )

(s/defn create :- Array
  "([nrows ncols] [nrows ncols init-val])
  Return a new Array of size=[nrows ncols] initialized to zero (or init-val if supplied)"
  ( [nrows  :- s/Int
     ncols  :- s/Int]
      (create nrows ncols nil))
  ( [nrows      :- s/Int
     ncols      :- s/Int
     init-val   :- s/Any]
    (assert (and (pos? nrows) (pos? ncols)))
    (forv [ii (range nrows)]
      (vec (repeat ncols init-val)))))

(s/defn vec->array-rows :- Array
  "([nrows ncols data-vec])
  Return a new Array of size=[nrows ncols] row-wise from data-vec."
  [nrows :- s/Int
   ncols :- s/Int
   data-vec :- Vector]
  (assert (and (pos? nrows) (pos? ncols)))
  (assert (= (* nrows ncols) (count data-vec)))
  (mapv vec
    (partition ncols data-vec)))

(s/defn vec->array-cols :- Array
  "([nrows ncols data-vec])
  Return a new Array of size=[nrows ncols] col-wise from data-vec."
  [nrows :- s/Int
   ncols :- s/Int
   data-vec :- Vector]
  (assert (and (pos? nrows) (pos? ncols)))
  (assert (= (* nrows ncols) (count data-vec)))
  (let [data-vec (vec data-vec)]
    (forv [ii (range nrows)]
      (forv [jj (range ncols) ]
        (nth data-vec (+ ii (* jj nrows)))))))

(s/defn rows->array :- Array
  "[row-vecs]
  Return a new Array initialized from row-vecs. Rows must all have same length."
  [row-vecs :- Array]
  (let [nrows (count row-vecs)
        ncols (count (first row-vecs))]
    (assert (apply = ncols (mapv count row-vecs)))
    (dotimes [ii nrows]
      (assert sequential? (nth row-vecs ii)))
    (mapv vec row-vecs)))

(s/defn cols->array :- Array
  "[col-vecs]
  Return a new Array initialized from col-vecs. Cols must all have same length."
  [col-vecs :- Array]
  (let [ncols (count col-vecs)
        nrows (count (first col-vecs))]
    (assert (apply = nrows (mapv count col-vecs)))
    (dotimes [jj ncols]
      (assert sequential? (nth col-vecs jj)))
    (vec->array-cols nrows ncols (apply glue col-vecs))))

(s/defn num-rows :- s/Int
  "Returns the number of rows of an Array."
  [arr :- Array]
  (count arr))

(s/defn num-cols :- s/Int
  "Returns the number of cols of an Array."
  [arr :- Array]
  (count (arr 0)))

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

(s/defn elem-get :- s/Any
  "Gets an Array element"
  [arr  :- Array
   ii   :- s/Int
   jj   :- s/Int ]
  (check-array-indexes arr ii jj)
  (get-in arr [ii jj]))

(s/defn elem-set :- Array
  "Puts a value into an Array element, returning the updated Array."
  [arr     :- Array
   ii      :- s/Int
   jj      :- s/Int
   newVal  :- s/Any]
  (check-array-indexes arr ii jj)
  (assoc-in arr [ii jj] newVal))

(s/defn row-get :- Vector
  "Gets an Array row"
  [arr  :- Array
   ii   :- s/Int ]
  (check-row-idx arr ii)
  (forv [jj (range (num-cols arr)) ]
    (elem-get arr ii jj)) )

(s/defn row-set :- Array
  "Sets an Array row"
  [orig :- Array
   ii :- s/Int
   new-row :- Vector]
  (check-row-idx orig ii)
  (assert (= (num-cols orig) (count new-row)))
  (let [nrows  (num-rows orig)
        result (glue
                 (forv [ii (range ii)] (row-get orig ii))
                 [new-row]
                 (forv [ii (range (inc ii) nrows)] (row-get orig ii))) ]
    result))

;#todo make both rows/cols -> submatrix result
(s/defn rows-get :- Array
  "[arr]          Returns all array rows
   [arr row-idxs] Returns array rows specified by row-idxs
   [arr low high] Returns array rows in half-open interval [low..high) "
  ([arr] (rows-get arr 0 (num-rows arr)))
  ([arr row-idxs]
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

(s/defn array-rows->vec :- Vector
  "Returns the concatenation of all array rows."
  [arr :- Array]
  (apply glue arr))

(s/defn array-cols->vec :- Vector
  "Returns the concatenation of all array cols."
  [arr :- Array]
  (forv [jj (range (num-cols arr))
         ii (range (num-rows arr)) ]
    (elem-get arr ii jj)))

(s/defn col-get :- Vector
  "Gets an Array col"
  [arr  :- Array
   jj   :- s/Int ]
  (check-col-idx arr jj)
  (forv [ii (range (num-rows arr)) ]
    (elem-get arr ii jj)))

(s/defn col-set :- Array
  "Sets an Array col"
  [orig :- Array
   jj :- s/Int
   new-col :- Vector]
  (check-col-idx orig jj)
  (let [nrows  (num-rows orig)
        >>     (assert (= nrows (count new-col)))
        result (forv [ii (range nrows)]
                 (let [curr-row (row-get orig ii)
                       new-val  (nth new-col ii)
                       new-row  (t/replace-at curr-row jj new-val)]
                   new-row))]
    result))

(s/defn cols-get :- Array
  "[arr]          Returns all array cols
   [arr col-idxs] Returns array cols specified by col-idxs
   [arr low high] Returns array cols in half-open interval [low..high) "
  ([arr] (cols-get arr 0 (num-cols arr)))
  ([arr col-idxs]
    (forv [jj col-idxs]
      (col-get arr jj)))
  ([arr :- Array
    low :- s/Int
    high :- s/Int]
    (check-col-idx arr low)
    (check-col-idx arr (dec high))
    (assert (< low high))
    (forv [jj (range low high)]
      (col-get arr jj))) )
; #todo need parallel cols-set

(s/defn transpose :- Array
  [orig :- Array]
  (forv [jj (range (num-cols orig)) ]
    (col-get orig jj)))

(s/defn flip-ud :- Array
  [orig :- Array]
  (forv [ii (reverse (range (num-rows orig))) ]
    (row-get orig ii)))

(s/defn flip-lr :- Array
  [orig :- Array]
  (forv [ii (range (num-rows orig))]
    (vec (reverse (row-get orig ii)))))

(s/defn rot-left :- Array
  [orig :- Array]
  (forv [jj (reverse (range (num-cols orig)))]
    (col-get orig jj)))

(s/defn rot-right :- Array
  [orig :- Array]
  (forv [jj (range (num-cols orig))]
    (vec (reverse (col-get orig jj))))) ; reverse yields a seq, not a vec! doh!

(s/defn symmetric? :- s/Bool
  [arr :- Array]
  (let [nrows (num-rows arr)
        ncols (num-cols arr)]
    (and (= nrows ncols)
      (every? truthy?
        (for [ii (range  0 nrows)
              jj (range ii ncols)] (= (elem-get arr ii jj)
                                      (elem-get arr jj ii)))))))

(s/defn row-drop :- Array
  "Drop one or more rows from an array"
  [orig :- Array
   & idxs-drop :- [s/Int]]
  (let [idxs-all  (set (range (num-rows orig)))
        idxs-drop (set idxs-drop)
        idxs-keep (sort (set/difference idxs-all idxs-drop))]
    (forv [ii idxs-keep]
      (row-get orig ii))))

(s/defn col-drop :- Array
  "Drop one or more colss from an array"
  [orig :- Array
   & idxs-drop :- [s/Int]]
  (let [idxs-all  (set (range (num-cols orig)))
        idxs-drop (set idxs-drop)
        idxs-keep (sort (set/difference idxs-all idxs-drop))]
    (forv [ii (range (num-rows orig))]
      (forv [jj idxs-keep]
        (elem-get orig ii jj)))))

(s/defn row-add :- Array
  [orig :- Array
   & rows :- [Vector] ]
  (let [row-lens (mapv count rows)]
    (assert (apply = (num-cols orig) row-lens)))
  (into orig rows))

(s/defn col-add :- Array
  [orig :- Array
   & cols :- [Vector]]
  (let [nrows    (num-rows orig)
        col-lens (mapv count cols)]
    (assert (apply = nrows col-lens))
    (forv [ii (range nrows)]
      (glue (row-get orig ii) (forv [col cols]
                                (nth col ii))))))

(s/defn glue-vert :- Array
  [& arrays :- [Array] ]
  (assert (pos? (count arrays)))
  (let [ncol-vals (mapv num-cols arrays)]
    (assert (apply = ncol-vals)))
  (apply glue arrays))

(s/defn glue-horiz :- Array
  [& arrays :- [Array] ]
  (assert (pos? (count arrays)))
  (let [nrow-vals (mapv num-rows arrays)]
    (assert (apply = nrow-vals)))
  (forv [ii (range (num-rows (t/xfirst arrays)))]
    (apply glue (mapv #(row-get % ii) arrays))))

(s/defn array->str :- s/Str
  [arr :- Array]
  (let [result (str/join
                 (flatten
                   (for [ii (range (num-rows arr))]
                     (t/append
                       (for [jj (range (num-cols arr))]
                         (let [val (str (elem-get arr ii jj))]
                           (ts/pad-left val 8)))
                       \newline))))]
    result ))

















