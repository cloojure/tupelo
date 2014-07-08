;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.core-test
  (:require [clojure.contrib.seq    :as seq]
            [cooljure.core          :refer :all]
            [clojure.test           :refer :all] ))

(deftest truthy-falsey-tst
  (let [data [true :a 1 false nil] ]
    (testing "basic usage"
      (let [truthies    (filter boolean data)       ; coerce to primitive type
            falsies     (filter not     data) ]     ; unnatural syntax
        (is (and  (= truthies [true :a 1] )
                  (= falsies  [false nil] ) )))
      (let [truthies    (filter truthy? data)
            falsies     (filter falsey? data) ]
        (is (and  (= truthies [true :a 1] )
                  (= falsies  [false nil] ) ))))

    (testing "improved usage"
      (let [count-if (comp count filter) ]
        (let [num-true    (count-if boolean data)   ; awkward phrasing
              num-false   (count-if not     data) ] ; doesn't feel natural
          (is (and  (= 3 num-true) 
                    (= 2 num-false) )))
        (let [num-true    (count-if truthy? data)   ; matches intent much better
              num-false   (count-if falsey? data) ]
          (is (and  (= 3 num-true)
                    (= 2 num-false) )))))

    (testing "contrib"
      (let [ result (seq/separate boolean data) ]
        (is (= [ [true :a 1] [false nil] ]      ; corece to primitive boolean feels wrong
               result )))
      (let [ result (seq/separate truthy? data) ]
        (is (= [ [true :a 1] [false nil] ]
               result ))))))

(deftest any-tst
  (testing "basic usage"
    (is (= true   (any? odd? [1 2 3] ) ))
    (is (= false  (any? odd? [2 4 6] ) ))
    (is (= false  (any? odd? []      ) )) ))

(deftest not-empty-tst
  (testing "basic usage"
    (is (every?     not-empty? ["1" [1] '(1) {:1 1} #{1} ] ))
    (is (not-any?   not-empty? ["" [] () '() {} #{} nil] ))

    (is (= [true true true true true] 
            (map not-empty? ["1" [1] '(1) {:1 1} #{1} ] ) ))
    (is (= [false false false false false false false ]
            (map not-empty? ["" [] () '() {} #{} nil] ) ))))

(deftest conjv-tst
  (testing "basic usage"
    (is (= [  2  ]  (conjv  []  2   )))
    (is (= [  2  ]  (conjv '()  2   )))
    (is (= [  2 3]  (conjv  []  2  3)))
    (is (= [  2 3]  (conjv '()  2  3)))

    (is (= [1 2 3]  (conjv  [1] 2  3)))
    (is (= [1 2 3]  (conjv '(1) 2  3)))
    (is (= [1 2 3]  (conjv  [1  2] 3)))
    (is (= [1 2 3]  (conjv '(1  2) 3)))

    (is (= [1 2 3 4]  (conjv  [1  2] 3 4)))
    (is (= [1 2 3 4]  (conjv '(1  2) 3 4)))
    (is (= [1 2 3 4]  (conjv  [1] 2  3 4)))
    (is (= [1 2 3 4]  (conjv '(1) 2  3 4))) )

  (testing "vector elements"
    (is (=    [[1 2] [3 4]  [5 6] ]
      (conjv '([1 2] [3 4]) [5 6] ) )))

  (testing "lazy seqs/apply"
    (is (= [0 1 2 3 4 5] (conjv (range 4) 4 5)))
    (is (= [0 1 2 3 4 5] (apply conjv [0] (range 1 6)))) ))

(deftest keyvals-test
  (testing "basic usage"
    (let [mm {:a 1 :b 2 :c 3} ]
      (is (= mm (apply hash-map (keyvals mm)))) )))
; AWTAWT TODO: add test.generative

