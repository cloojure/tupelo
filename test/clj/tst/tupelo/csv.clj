;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.csv
  (:use tupelo.csv tupelo.core tupelo.test)
  (:import  [java.io StringReader] ) )

(def test1-str-no-label
"01002,00006,4
01002,00277,5
01003,00277,5
01008,01217,5
01009,00439,5
01020,01193,5" )

(def test1-str-label
  (str "zip_postal_code,store$num,chain#rank" \newline test1-str-no-label) )

(def test2-str-label
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

(dotest
  ; basic parse-csv->rows test, using String
  (let [result (parse->rows test1-str-label)]
    (is (= result test1-expected)))

  ; read PSV file instead of default CS"
  (let [raw-maps (parse->rows test2-str-label :delimiter \|)
        result   (map #(hash-map :store-id (Long/parseLong (:STORE-NUM %))
                         :zipcode (:ZIP-POSTAL-CODE %))
                   raw-maps)]
    (is (= result test2-expected)))

  ; no header row in file, user spec :labels
  (let [result (parse->rows test1-str-no-label
                 :labels [:zip-postal-code :store-num :chain-rank])]
    (is (= result test1-expected))))

  (dotest
    ; basic parse-csv->rows test, using Reader
    (let [result (parse->rows (StringReader. test1-str-label))]
      (is (= result test1-expected))) )

(dotest
  (is= {} (rows->cols []))
  (is= [] (cols->rows {}))
  (let [result (rows->cols test1-expected)]
    (is (= result test3-expected)))
  (let [result (rows->cols test2-expected)]
    (is (= result test4-expected))))

(dotest
  (let [result (cols->rows test3-expected)]
    (is (= result test1-expected)))
  (let [result (cols->rows test4-expected)]
    (is (= result test2-expected))))

(dotest
  (let [result (parse->cols test1-str-label)]
    (is (= result test3-expected)))

  (let [raw-maps (parse->cols test2-str-label :delimiter \|)
        result   {:store-id (map #(Long/parseLong %) (:STORE-NUM raw-maps))
                  :zipcode  (:ZIP-POSTAL-CODE raw-maps)}]
    (is (= result test4-expected))))







