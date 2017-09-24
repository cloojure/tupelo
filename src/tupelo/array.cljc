(ns tupelo.array
  (:require [schema.core :as s]
            [tupelo.core :as t]
            [clojure.set :as set]))
(t/refer-tupelo)

(def Vector
  "A 2-D array of values (a vector of vectors)."
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
      (throw (IllegalArgumentException. (format "Row index %d out of range; limit = %d" idx limit))))))

(s/defn ^:no-doc check-col-idx
  [arr :- Array
   idx :- s/Int]
  (let [limit (num-cols arr)]
    (when-not (< -1 idx limit)
      (throw (IllegalArgumentException. (format "Col index %d out of range; limit = %d" idx limit))))))

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
    (elem-get arr ii jj))
  )

(s/defn rows :- Array
  "Gets an Array row"
  [arr  :- Array
   i-min   :- s/Int
   i-max   :- s/Int ]
  (check-row-idx arr i-min)
  (check-row-idx arr (dec i-max))
  (assert (< i-min i-max))
  (forv [ii (range i-min i-max) ]
    (row-get arr ii))
  )

(s/defn col-get :- Vector
  "Gets an Array col"
  [arr  :- Array
   jj   :- s/Int ]
  (check-col-idx arr jj)
  (forv [ii (range (num-rows arr)) ]
    (elem-get arr ii jj)))

(s/defn cols :- Array
  "Gets an Array col"
  [arr  :- Array
   j-min   :- s/Int
   j-max   :- s/Int ]
  (check-col-idx arr j-min)
  (check-col-idx arr (dec j-max))
  (assert (< j-min j-max))
  (forv [jj (range j-min j-max) ]
    (col-get arr jj)))

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
        (for [ii (range nrows)
              jj (range ncols)] (=
                                  (elem-get arr ii jj)
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

; #todo set-row, set-col

(s/defn toString :- s/Str
  [arr :- Array]
  (with-out-str
    (dotimes [ii (num-rows arr)]
      (dotimes [jj (num-cols arr)]
        (print (format "%8s" (elem-get arr ii jj))))
      (newline))))

