(ns tst.tupelo.array
  (:use clojure.test
        tupelo.core)
  (:require
    [schema.test :as st]
    [tupelo.array :as tar]
    [tupelo.misc :as tm]
    [clojure.string :as str]))

(use-fixtures :once st/validate-schemas)

(deftest t-arrays
  (let [a34  (tar/create 3 4 :a)
        a34f (flatten a34)]
    (is (= 3 (count a34) (tar/num-rows a34)))
    (is (= 4 (count (a34 0)) (tar/num-cols a34)))
    (is (= 12 (count a34f)))
    (is (every? #(= :a %) a34f))
    (is (every? #(= :a %) (forv [ii (range (tar/num-rows a34))
                                 jj (range (tar/num-cols a34))]
                            (tar/get-elem a34 ii jj)))))

  (let [a34  (tar/create 3 4)
        a34f (flatten a34)]
    (is (= 3 (count a34) (tar/num-rows a34)))
    (is (= 4 (count (a34 0)) (tar/num-cols a34)))
    (is (= 12 (count a34f)))
    (is (every? nil? a34f))
    (is (every? nil? (forv [ii (range (tar/num-rows a34))
                            jj (range (tar/num-cols a34))]
                       (tar/get-elem a34 ii jj)))))

  (let [a34    (atom (tar/create 3 4))
        target [["00" "01" "02" "03"]
                ["10" "11" "12" "13"]
                ["20" "21" "22" "23"]]
        target-tx  [["00" "10" "20"]
                    ["01" "11" "21"]
                    ["02" "12" "22"]
                    ["03" "13" "23"]]
       ]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (swap! a34 tar/set-elem ii jj (str ii jj))))
    (is (= (tm/collapse-whitespace (tar/toString @a34))
           (tm/collapse-whitespace "00      01      02      03
                                    10      11      12      13
                                    20      21      22      23" )))
    (is (= target @a34))
    (is (= target-tx (tar/transpose @a34)))

    (is (= (tar/get-row target 0) ["00" "01" "02" "03"]))
    (is (= (tar/get-row target 1) ["10" "11" "12" "13"]))
    (is (= (tar/get-row target 2) ["20" "21" "22" "23"]))

    (is (= (tar/get-col target 0) ["00" "10" "20"]))
    (is (= (tar/get-col target 1) ["01" "11" "21"]))
    (is (= (tar/get-col target 2) ["02" "12" "22"]))
    (is (= (tar/get-col target 3) ["03" "13" "23"]))
    )
  )

(deftest t-symmetric
  (is (tar/symmetric? [[1 2]
                       [2 1]]))
  (is (tar/symmetric? [[1 2 3]
                       [2 4 5]
                       [3 5 6]])))

