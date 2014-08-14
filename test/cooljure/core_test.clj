;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.core-test
  (:require [clojure.contrib.seq    :as seq]
            [clojure.string         :as str]
            [cooljure.core          :refer :all]
            [clojure.test           :refer :all] ))

(deftest truthy-falsey-tst
  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [truthies    (filter boolean data)       ; coerce to primitive type
            falsies     (filter not     data) ]     ; unnatural syntax
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
                  (= falsies  [false nil] ) )))
      (let [truthies    (filter truthy? data)
            falsies     (filter falsey? data) ]
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
                  (= falsies  [false nil] ) ))))

    (testing "improved usage"
      (let [count-if (comp count filter) ]
        (let [num-true    (count-if boolean data)   ; awkward phrasing
              num-false   (count-if not     data) ] ; doesn't feel natural
          (is (and  (= 6 num-true) 
                    (= 2 num-false) )))
        (let [num-true    (count-if truthy? data)   ; matches intent much better
              num-false   (count-if falsey? data) ]
          (is (and  (= 6 num-true)
                    (= 2 num-false) )))))

    (testing "contrib"
      ; corece to primitive boolean feels wrong
      (let [ result (seq/separate boolean data) ]
        (is (= [ [true :a 'my-symbol 1 "hello" \x] 
                 [false nil] ]
               result )))
      ; separate by truthy? (or not)
      (let [ result (seq/separate truthy? data) ]
        (is (= [ [true :a 'my-symbol 1 "hello" \x] 
                 [false nil] ]
               result ))))))

(deftest any-tst
  (testing "basic usage"
    (is (= true   (any? odd? [1 2 3] ) ))
    (is (= false  (any? odd? [2 4 6] ) ))
    (is (= false  (any? odd? []      ) )) ))

(deftest not-empty-tst
  (testing "basic usage"
    (is (every?     not-empty? ["1" [1] '(1) {:1 1} #{1}    ] ))
    (is (not-any?   not-empty? [""  []  '()  {}     #{}  nil] ))

    (is (= (map not-empty? ["1" [1] '(1) {:1 1} #{1} ] )
           [true true true true true]  ))
    (is (= (map not-empty? ["" [] '() {} #{} nil] )
           [false false false false false false ] ))))

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
    (let [m1 {:a 1 :b 2 :c 3} 
          m2 {:a 1 :b 2 :c [3 4]} ]
      (is (= m1 (apply hash-map (keyvals m1))))
      (is (= m2 (apply hash-map (keyvals m2)))) 
    )))
; AWTAWT TODO: add test.generative

(deftest with-exception-default-test
  (testing "basic usage"
    (is (thrown?    Exception                       (/ 1 0)))
    (is (= nil      (with-exception-default nil     (/ 1 0))))
    (is (= :dummy   (with-exception-default :dummy  (/ 1 0))))
    (is (= 123      (with-exception-default 0       (Long/parseLong "123"))))
    (is (= 0        (with-exception-default 0       (Long/parseLong "12xy3"))))
    ))

(deftest spy-test
  (testing "basic usage"
    (is (= "hi => 5" 
        (str/trim (with-out-str (spy-first (+ 2 3) "hi"))) ))
    (is (= "hi => 5" 
        (str/trim (with-out-str (spy-last "hi"  (+ 2 3)))) ))
    (is (= "(+ 2 3) => 5" 
        (str/trim (with-out-str (spy-expr (+ 2 3)))) ))
    (is (= "5" 
        (str/trim (with-out-str (spy-val (+ 2 3)))) ))

    (is (= "first => 5\nsecond => 25"
        (str/trim (with-out-str (-> 2 
                                    (+ 3) 
                                    (spy-first "first" )
                                    (* 5)
                                    (spy-first "second") )))))

    (is (= "first => 5\nsecond => 25"
        (str/trim (with-out-str (->> 2 
                                    (+ 3) 
                                    (spy-last "first" )
                                    (* 5)
                                    (spy-last "second") )))))
))

(deftest grab-test
  (testing "basic usage"
    (let [map1  {:a 1 :b 2}]
      (is (= 1                                  (grab map1 :a)))
      (is (= 2                                  (grab map1 :b)))
      (is (thrown?    IllegalArgumentException  (grab map1 :c))) )))

(deftest grab-in-test
  (testing "basic usage"
    (let [map1  {:a1 "a1"
                 :a2 { :b1 "b1"
                       :b2 { :c1 "c1"
                             :c2 "c2" }}} ]
      (is (= (grab-in map1 [:a1] ) "a1" ))
      (is (= (grab-in map1 [:a2 :b1] ) "b1" ))
      (is (= (grab-in map1 [:a2 :b2 :c1] ) "c1" ))
      (is (= (grab-in map1 [:a2 :b2 :c2] ) "c2" ))
      (is (thrown? IllegalArgumentException  (grab-in map1 [:a9]) )) 
      (is (thrown? IllegalArgumentException  (grab-in map1 [:a2 :b9]) )) 
      (is (thrown? IllegalArgumentException  (grab-in map1 [:a2 :b2 :c9]) )) 
    )))

(deftest t-rel=
  (is (rel= 1 1 :digits 4 ))
  (is (rel= 1 1 :tol    0.01 ))

  (is (thrown? IllegalArgumentException  (rel= 1 1 )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 4)))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :xxdigits 4      )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :digits   4.1    )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :digits   0      )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :digits  -4      )))

  (is (thrown? IllegalArgumentException  (rel= 1 1 :tol    -0.01    )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :tol     "xx"    )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :xxtol   0.01    )))

  (is      (rel= 1 1.001 :digits 3 ))
  (is (not (rel= 1 1.001 :digits 4 )))
  (is      (rel= 123450000 123456789 :digits 4 ))
  (is (not (rel= 123450000 123456789 :digits 6 )))

  (is      (rel= 1 1.001 :tol 0.01 ))
  (is (not (rel= 1 1.001 :tol 0.0001 )))
)

(comment    ; example usage w/o -> macro
  (def cust->zip
    "A map from (int) customer-id to (string-5) zipcode, like { 96307657 \"54665\", ...}"
    (spy-last "#00 map:"
      (into (sorted-map) 
        (spy-last "#01 for:"
          (for [cust-zip-map cust-zips]
            (spy-last "#02 vec:" 
              [ (:customer-id  cust-zip-map)
                (:zipcode      cust-zip-map)  ] ))))))
)

