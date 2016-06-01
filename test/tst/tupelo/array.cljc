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
        target           [[00 01 02 03]
                          [10 11 12 13]
                          [20 21 22 23]]

        target-flip-ud   [[20 21 22 23]
                          [10 11 12 13]
                          [00 01 02 03]]

        target-flip-lr   [[03 02 01 00]
                          [13 12 11 10]
                          [23 22 21 20]]

        target-tx        [[00 10 20]
                          [01 11 21]
                          [02 12 22]
                          [03 13 23]]

        target-rot-left-1  [[03 13 23]
                            [02 12 22]
                            [01 11 21]
                            [00 10 20]]
        target-rot-left-2  [[23 22 21 20]
                            [13 12 11 10]
                            [03 02 01 00]]
        target-rot-left-3  [[20 10 00]
                            [21 11 01]
                            [22 12 02]
                            [23 13 03]]
       ]
    (dotimes [ii 3]
      (dotimes [jj 4]
        (swap! a34 tar/set-elem ii jj (+ (* ii 10) jj))))
    (is (= (tm/collapse-whitespace (tar/toString @a34))
           (tm/collapse-whitespace " 0       1       2       3
                                    10      11      12      13
                                    20      21      22      23" )))

    (is (= (tar/get-row target 0) [00 01 02 03]))
    (is (= (tar/get-row target 1) [10 11 12 13]))
    (is (= (tar/get-row target 2) [20 21 22 23]))

    (is (= (tar/get-col target 0) [00 10 20]))
    (is (= (tar/get-col target 1) [01 11 21]))
    (is (= (tar/get-col target 2) [02 12 22]))
    (is (= (tar/get-col target 3) [03 13 23]))

    (is (= target @a34))
    (is (= target-flip-ud (tar/flip-ud target)))
    (is (= target-flip-lr (tar/flip-lr target)))
    (is (= target-tx      (tar/transpose target)))

    (is (= target-rot-left-1 (-> target (tar/rot-left))))
    (is (= target-rot-left-2 (-> target (tar/rot-left) (tar/rot-left))))
    (is (= target-rot-left-3 (-> target (tar/rot-left) (tar/rot-left) (tar/rot-left))))
    (is (= target            (-> target (tar/rot-left) (tar/rot-left) (tar/rot-left) (tar/rot-left))))

    (is (= target-rot-left-3 (-> target (tar/rot-right))))
    (is (= target-rot-left-2 (-> target (tar/rot-right) (tar/rot-right))))
    (is (= target-rot-left-1 (-> target (tar/rot-right) (tar/rot-right) (tar/rot-right))))
    (is (= target            (-> target (tar/rot-right) (tar/rot-right) (tar/rot-right) (tar/rot-right))))

    )
  )

(deftest t-get-rows
  (let [demo  [[00 01 02 03]
               [10 11 12 13]
               [20 21 22 23]]
        ]
    (is (thrown? AssertionError (tar/get-rows demo 0 0)))
    (is (= (tar/get-rows demo 0 1)  [[00 01 02 03]] ))
    (is (= (tar/get-rows demo 0 2)  [[00 01 02 03]
                                     [10 11 12 13]] ))
    (is (= (tar/get-rows demo 0 3)  [[00 01 02 03]
                                     [10 11 12 13]
                                     [20 21 22 23]] ))
    (is (= (tar/get-rows demo 1 3)  [[10 11 12 13]
                                     [20 21 22 23]] ))
    (is (= (tar/get-rows demo 2 3)  [[20 21 22 23]] ))
    (is (thrown? AssertionError (tar/get-rows demo 3 3)))))

(deftest t-get-cols
  (let [demo  [[00 01 02 03]
               [10 11 12 13]
               [20 21 22 23]]
       ]
  (is (thrown? AssertionError (tar/get-cols demo 0 0)))
  (is (= (tar/get-cols demo 0 1)   [[00 10 20]] ))
  (is (= (tar/get-cols demo 0 2)   [[00 10 20]
                                    [01 11 21]] ))
  (is (= (tar/get-cols demo 0 3)   [[00 10 20]
                                    [01 11 21]
                                    [02 12 22]] ))
  (is (= (tar/get-cols demo 0 4)   [[00 10 20]
                                    [01 11 21]
                                    [02 12 22]
                                    [03 13 23]] ))
  (is (= (tar/get-cols demo 1 4)   [[01 11 21]
                                    [02 12 22]
                                    [03 13 23]] ))
  (is (= (tar/get-cols demo 2 4)   [[02 12 22]
                                    [03 13 23]] ))
  (is (= (tar/get-cols demo 3 4)   [[03 13 23]] ))
  (is (thrown? AssertionError (tar/get-cols demo 4 4)))))

(deftest t-symmetric
  (is (tar/symmetric? [[1 2]
                       [2 1]]))
  (is (tar/symmetric? [[1 2 3]
                       [2 4 5]
                       [3 5 6]])))

