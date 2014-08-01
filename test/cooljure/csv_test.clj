;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.csv-test
  (:require [clojure.java.io            :as io]
            [clojure.string             :as str]
            [cooljure.csv               :refer :all]
            [clojure.test               :refer :all] ))

(def user-dir (System/getProperty "user.dir"))

(def csv-test-1-str
"zip_postal_code,store$num,chain#rank
01002,00006,4
01002,00277,5
01003,00277,5
01008,01217,5
01009,00439,5
01020,01193,5" )

(def csv-test-2-str
"ZIP_POSTAL_CODE|STORE_NUM|CHAIN_RANK
01002|00006|4
01002|00277|5
01003|00277|5
01008|01217|5
01009|00439|5
01020|01193|5" )

(def test1-expected
  [ { :zip-postal-code "01002" :store-num "00006" :chain-rank "4" }
    { :zip-postal-code "01002" :store-num "00277" :chain-rank "5" }
    { :zip-postal-code "01003" :store-num "00277" :chain-rank "5" }
    { :zip-postal-code "01008" :store-num "01217" :chain-rank "5" }
    { :zip-postal-code "01009" :store-num "00439" :chain-rank "5" }
    { :zip-postal-code "01020" :store-num "01193" :chain-rank "5" } ] )

(def test2-expected
  [ { :zipcode "01002" :store-id    6 }
    { :zipcode "01002" :store-id  277 }
    { :zipcode "01003" :store-id  277 }
    { :zipcode "01008" :store-id 1217 }
    { :zipcode "01009" :store-id  439 }
    { :zipcode "01020" :store-id 1193 } ] )

(def test3-expected
  { :zip-postal-code    ["01002" "01002" "01003" "01008" "01009" "01020"]
    :store-num          ["00006" "00277" "00277" "01217" "00439" "01193"]
    :chain-rank         [    "4"     "5"     "5"     "5"     "5"     "5"] } )

(def test4-expected
  { :zipcode            ["01002" "01002" "01003" "01008" "01009" "01020"]
    :store-id           [     6     277     277    1217     439    1193 ] } )

(deftest row-maps->col-vecs-test
  (testing "row-maps->col-vecs-test-1"
    (let [result (row-maps->col-vecs test1-expected) ]
    (is (= result test3-expected)) ))
  (testing "row-maps->col-vecs-test-2"
    (let [result (row-maps->col-vecs test2-expected) ]
    (is (= result test4-expected)) )))

(deftest col-vecs->row-maps-test
  (testing "col-vecs->row-maps-test-1"
    (let [result (col-vecs->row-maps test3-expected) ]
    (is (= result test1-expected)) ))
  (testing "col-vecs->row-maps-test-2"
    (let [result (col-vecs->row-maps test4-expected) ]
    (is (= result test2-expected)) )))

(deftest parse-csv->row-maps-test
  (testing "parse-csv->row-maps-test-1"
    (let [result (parse-csv->row-maps csv-test-1-str ) ]
    (is (= result test1-expected)) ))

  (testing "parse-csv->row-maps-test-2"
    (let [raw-maps  (parse-csv->row-maps csv-test-2-str :delimiter \| )
          result    (map #(hash-map :store-id (Long/parseLong (:STORE-NUM %))
                                    :zipcode                  (:ZIP-POSTAL-CODE %) )
                         raw-maps ) ]
    (is (= result test2-expected)) )))

(deftest parse-csv->col-vecs-test
  (testing "parse-csv->col-vecs-test-1"
    (let [result    (parse-csv->col-vecs csv-test-1-str) ]
    (is (= result test3-expected)) ))

  (testing "parse-csv->col-vecs-test-2"
    (let [raw-maps  (parse-csv->col-vecs csv-test-2-str :delimiter \| )
          result    { :store-id (map #(Long/parseLong %) (:STORE-NUM       raw-maps))
                      :zipcode                           (:ZIP-POSTAL-CODE raw-maps) } ]
    (is (= result test4-expected)) )))

