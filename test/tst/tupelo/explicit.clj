;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tst.tupelo.explicit
  (:require [clojure.string         :as str]
            [tupelo.explicit      :as x]
            [clojure.test           :refer :all] ))

(deftest get-t
  (testing "basic usage"
    (let [map1  {:a 1 :b 2}]
      (is (= 1                                  (x/get map1 :a)))
      (is (= 2                                  (x/get map1 :b)))
      (is (thrown?    IllegalArgumentException  (x/get map1 :c))) )))

(deftest get-in-t
  (testing "basic usage"
    (let [map1  {:a1 "a1"
                 :a2 { :b1 "b1"
                       :b2 { :c1 "c1"
                             :c2 "c2" }}} ]
      (is (= (x/get-in map1 [:a1] ) "a1" ))
      (is (= (x/get-in map1 [:a2 :b1] ) "b1" ))
      (is (= (x/get-in map1 [:a2 :b2 :c1] ) "c1" ))
      (is (= (x/get-in map1 [:a2 :b2 :c2] ) "c2" ))
      (is (thrown? IllegalArgumentException  (x/get-in map1 [:a9]) )) 
      (is (thrown? IllegalArgumentException  (x/get-in map1 [:a2 :b9]) )) 
      (is (thrown? IllegalArgumentException  (x/get-in map1 [:a2 :b2 :c9]) )) 
    )))

