(ns tupelo.array
  (:use tupelo.core)
  (:require
    [schema.core        :as s] ))

(s/set-fn-validation! true)

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
    (forv [ii (range nrows)]
      (into [] (repeat ncols init-val)))))

(s/defn num-rows :- s/Int
  "Returns the number of rows of an Array."
  [arr :- Array]
  (count arr))

(s/defn num-cols :- s/Int
  "Returns the number of cols of an Array."
  [arr :- Array]
  (count (arr 0)))

; #todo -> elem-set/elem-get
(s/defn set-elem :- Array
  "Puts a value into an Array element, returning the updated Array."
  [arr  :- Array
   ii      :- s/Int
   jj      :- s/Int
   newVal  :- s/Any]
  {:pre [ (<= 0 ii) (< ii (num-rows arr))
          (<= 0 jj) (< jj (num-cols arr)) ] }
  (assoc-in arr [ii jj] newVal))

(s/defn get-elem :- s/Any
  "Gets an Array element"
  [arr  :- Array
   ii      :- s/Int
   jj      :- s/Int ]
  {:pre [ (<= 0 ii) (< ii (num-rows arr))
          (<= 0 jj) (< jj (num-cols arr)) ] }
  (get-in arr [ii jj]))

(s/defn get-row :- [s/Any]
  "Gets an Array row"
  [arr  :- Array
   ii   :- s/Int ]
  {:pre [ (<= 0 ii) (< ii (num-rows arr)) ] }
  (forv [jj (range (num-cols arr)) ]
    (get-elem arr ii jj)))

(s/defn get-col :- [s/Any]
  "Gets an Array column"
  [arr  :- Array
   jj      :- s/Int ]
  {:pre [ (<= 0 jj) (< jj (num-cols arr)) ] }
  (forv [ii (range (num-rows arr)) ]
    (get-elem arr ii jj)))

(s/defn transpose :- Array
  [arr :- Array]
  (let [nrows  (num-rows arr)
        ncols  (num-cols arr)
        result (atom (create ncols nrows))]
    (dotimes [ii (num-rows arr)]
      (dotimes [jj (num-cols arr)]
        (swap! result set-elem jj ii (get-elem arr ii jj))))
    @result ))


(s/defn symmetric? :- s/Bool
  [arr  :- Array ]
  (let [nrows (num-rows arr)
        ncols (num-cols arr) ]
    (and  (= nrows ncols)
          (every? truthy? 
            (for [ii (range nrows)
                  jj (range ncols) ]
              (= (get-elem arr ii jj)
                 (get-elem arr jj ii)))))))

(defn toString [arr]
  (with-out-str
    (dotimes [ii (num-rows arr)]
      (dotimes [jj (num-cols arr)]
        (print (format "%8s" (get-elem arr ii jj))))
      (newline))))

