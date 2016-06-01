(ns tst.tupelo.array
  (:use tupelo.array)
  (:use clojure.test 
        tupelo.core )
  (:require
    [schema.test :as st]
    [tupelo.misc :as tm]
    [clojure.string :as str]))

(use-fixtures :once st/validate-schemas)

(deftest t-arrays
  (let [a34  (create 3 4 :a)
        a34f (flatten a34)]
    (is (= 3 (count a34) (num-rows a34)))
    (is (= 4 (count (a34 0)) (num-cols a34)))
    (is (= 12 (count a34f)))
    (is (every? #(= :a %) a34f))
    (is (every? #(= :a %) (forv [ii (range (num-rows a34))
                                 jj (range (num-cols a34))]
                            (get-elem a34 ii jj)))))

  (let [a34  (create 3 4)
        a34f (flatten a34)]
    (is (= 3 (count a34) (num-rows a34)))
    (is (= 4 (count (a34 0)) (num-cols a34)))
    (is (= 12 (count a34f)))
    (is (every? #(= 0 %) a34f))
    (is (every? #(= 0 %) (forv [ii (range (num-rows a34))
                                jj (range (num-cols a34))]
                           (get-elem a34 ii jj)))))

  (let [a34 (atom (create 3 4))]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (swap! a34 set-elem ii jj (str ii jj))))
    (let [target [["00" "01" "02" "03"]
                  ["10" "11" "12" "13"]
                  ["20" "21" "22" "23"]]
          result (toString @a34)]
      (is (= (tm/collapse-whitespace result)
             (tm/collapse-whitespace "00      01      02      03
                                      10      11      12      13
                                      20      21      22      23")))
      (is (= target @a34))

      (is (= (get-row target 0) [ "00" "01" "02" "03"]))
      (is (= (get-row target 1) [ "10" "11" "12" "13"]))
      (is (= (get-row target 2) [ "20" "21" "22" "23"]))

      (is (= (get-col target 0) [ "00" "10" "20"]))
      (is (= (get-col target 1) [ "01" "11" "21"]))
      (is (= (get-col target 2) [ "02" "12" "22"]))
      (is (= (get-col target 3) [ "03" "13" "23"]))
    ))
  )

(deftest t-symmetric
  (is (symmetric? [[1 2]
                   [2 1]]))
  (is (symmetric? [[1 2 3]
                   [2 4 5]
                   [3 5 6]]))
  )

