;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.csv
  (:use tupelo.csv tupelo.core tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.parse :as parse]
    [tupelo.string :as str])
  (:import
    [java.io StringReader]
    ))

(def csv1-str-nohdr
  "01002,00006, 4
  01002,00277,   5
  01003,00277, 5
  01008,01217, 5
  01009,00439, 5
  01020,01193,  5  ")

(def csv1-str-hdr
  (str "zip_postal_code,store$num,chain#rank"
    \newline
    csv1-str-nohdr))

(def psv2-str-hdr
  "ZIP_POSTAL_CODE|STORE_NUM|CHAIN_RANK
  01002|00006|4
  01002|00277|5
  01003|00277|5
  01008|01217|5
  01009|00439|5
  01020|01193|5")

(def csv1-entity
  [{:zip-postal-code "01002" :store-num "00006" :chain-rank "4"}
   {:zip-postal-code "01002" :store-num "00277" :chain-rank "5"}
   {:zip-postal-code "01003" :store-num "00277" :chain-rank "5"}
   {:zip-postal-code "01008" :store-num "01217" :chain-rank "5"}
   {:zip-postal-code "01009" :store-num "00439" :chain-rank "5"}
   {:zip-postal-code "01020" :store-num "01193" :chain-rank "5"}])

(def csv1-attr
  {:zip-postal-code ["01002" "01002" "01003" "01008" "01009" "01020"]
   :store-num       ["00006" "00277" "00277" "01217" "00439" "01193"]
   :chain-rank      ["4" "5" "5" "5" "5" "5"]})

(def psv2-entity
  [{:zipcode "01002" :store-id 6}
   {:zipcode "01002" :store-id 277}
   {:zipcode "01003" :store-id 277}
   {:zipcode "01008" :store-id 1217}
   {:zipcode "01009" :store-id 439}
   {:zipcode "01020" :store-id 1193}])

(def psv2-attr
  {:zipcode  ["01002" "01002" "01003" "01008" "01009" "01020"]
   :store-id [6 277 277 1217 439 1193]})

(verify
  (is= {} (entities->attrs []))
  (is= [] (attrs->entities {}))

  (is= csv1-attr (entities->attrs csv1-entity))
  (is= psv2-attr (entities->attrs psv2-entity))

  (is= csv1-entity (-> csv1-entity entities->attrs attrs->entities))
  (is= psv2-entity (-> psv2-entity entities->attrs attrs->entities)))

(verify
  (is= (csv->table csv1-str-hdr)
    [["zip_postal_code" "store$num" "chain#rank"]
     ["01002" "00006" " 4"]
     ["  01002" "00277" "   5"]
     ["  01003" "00277" " 5"]
     ["  01008" "01217" " 5"]
     ["  01009" "00439" " 5"]
     ["  01020" "01193" "  5  "]])

  (is= (csv->table psv2-str-hdr {:separator \|})
    [["ZIP_POSTAL_CODE" "STORE_NUM" "CHAIN_RANK"]
     ["  01002" "00006" "4"]
     ["  01002" "00277" "5"]
     ["  01003" "00277" "5"]
     ["  01008" "01217" "5"]
     ["  01009" "00439" "5"]
     ["  01020" "01193" "5"]]))

(verify
  (is= (csv->entities csv1-str-hdr)
    [{:zip-postal-code "01002" :store-num "00006" :chain-rank "4"}
     {:zip-postal-code "01002" :store-num "00277" :chain-rank "5"}
     {:zip-postal-code "01003" :store-num "00277" :chain-rank "5"}
     {:zip-postal-code "01008" :store-num "01217" :chain-rank "5"}
     {:zip-postal-code "01009" :store-num "00439" :chain-rank "5"}
     {:zip-postal-code "01020" :store-num "01193" :chain-rank "5"}])
  (is= (csv->attrs csv1-str-hdr)
    {:store-num ["00006" "00277" "00277" "01217" "00439" "01193"]
     :zip-postal-code ["01002" "01002" "01003" "01008" "01009" "01020"]
     :chain-rank ["4" "5" "5" "5" "5" "5"]})
  (is= (csv->entities csv1-str-hdr {:keywordize-keys? false})
    [{"zip_postal_code" "01002" "store$num" "00006" "chain#rank" "4"}
     {"zip_postal_code" "01002" "store$num" "00277" "chain#rank" "5"}
     {"zip_postal_code" "01003" "store$num" "00277" "chain#rank" "5"}
     {"zip_postal_code" "01008" "store$num" "01217" "chain#rank" "5"}
     {"zip_postal_code" "01009" "store$num" "00439" "chain#rank" "5"}
     {"zip_postal_code" "01020" "store$num" "01193" "chain#rank" "5"}])
  (is= (csv->entities csv1-str-hdr {:headers-to-use [:a :b :c]})
    [{:a "01002" :b "00006" :c "4"}
     {:a "01002" :b "00277" :c "5"}
     {:a "01003" :b "00277" :c "5"}
     {:a "01008" :b "01217" :c "5"}
     {:a "01009" :b "00439" :c "5"}
     {:a "01020" :b "01193" :c "5"}])
  (is= (csv->entities csv1-str-nohdr {:headers? false})
    [{0 "01002" 1 "00006" 2 "4"}
     {0 "01002" 1 "00277" 2 "5"}
     {0 "01003" 1 "00277" 2 "5"}
     {0 "01008" 1 "01217" 2 "5"}
     {0 "01009" 1 "00439" 2 "5"}
     {0 "01020" 1 "01193" 2 "5"}]))

(verify
  (is= (csv->entities psv2-str-hdr {:separator \|})
    [{:zip-postal-code "01002" :store-num "00006" :chain-rank "4"}
     {:zip-postal-code "01002" :store-num "00277" :chain-rank "5"}
     {:zip-postal-code "01003" :store-num "00277" :chain-rank "5"}
     {:zip-postal-code "01008" :store-num "01217" :chain-rank "5"}
     {:zip-postal-code "01009" :store-num "00439" :chain-rank "5"}
     {:zip-postal-code "01020" :store-num "01193" :chain-rank "5"}]))

(verify
  (let [attr-map (csv->attrs psv2-str-hdr {:separator \|})
        result   {:store-id (mapv parse/parse-long (grab :store-num attr-map))
                  :zipcode  (:zip-postal-code attr-map)}]
    (is= result psv2-attr)))

(verify
  (let [test2-str-label-error "zipcode, store_num, chain_rank
                                  01002,00006,4
                                  01002,00277,5
                                  01003,00277"]
    (throws? (csv->entities test2-str-label-error))))

(verify
  ; basic parse-csv->rows test, using String
  (let [result (csv->entities csv1-str-hdr)]
    (is= result csv1-entity))

  ; no header row in file, user spec :labels
  (let [result (csv->entities csv1-str-nohdr
                 {:headers? false
                  :headers-to-use [:zip-postal-code :store-num :chain-rank]})]
    (is= result csv1-entity))
  (let [result (csv->entities csv1-str-nohdr
                 {:headers? false})]
    (is= result ; #todo write variant to take vvv input => unlabeled CSV file
      [{0 "01002" 1 "00006" 2 "4"}
       {0 "01002" 1 "00277" 2 "5"}
       {0 "01003" 1 "00277" 2 "5"}
       {0 "01008" 1 "01217" 2 "5"}
       {0 "01009" 1 "00439" 2 "5"}
       {0 "01020" 1 "01193" 2 "5"}]))

  ; basic parse-csv->rows test, using Reader
  (let [result (csv->entities (StringReader. csv1-str-hdr))]
    (is= result csv1-entity))

  ; read PSV file instead of default CSV"
  (let [result (forv [entity (csv->entities psv2-str-hdr {:separator \|})]
                 {:store-id (parse/parse-long (grab :store-num entity))
                  :zipcode  (grab :zip-postal-code entity)})]
    (is= result psv2-entity))
  )

(verify
  (let [sample-edn [{:aa-key "aaa" :bb-key "b,b"} ; 2nd val needs to be quoted
                    {:aa-key "aa2" :bb-key "bb2"}]]
    ; is verified-keys working?
    (is= (verified-keys sample-edn) [:aa-key :bb-key])
    (throws?
      (verified-keys
        [{:aa-key "aaa" :bb-X "b,b"} ; "bb" keys don't match
         {:aa-key "aa2" :bb-key "bb2"}]))

    ; 'b,b' value quoted correctly
    (let [result (str/quotes->single (entities->csv sample-edn {:force-quote? true}))
          expected "'aa-key','bb-key'
                    'aaa','b,b'
                    'aa2','bb2' "]
      (is-nonblank-lines= result expected))

    (let [edn-str-keys (walk/postwalk
                         (fn [item]
                           (cond-it-> item
                             (keyword? it) (kw->str it)))
                         sample-edn)
          result       (str/quotes->single (entities->csv edn-str-keys  {:force-quote? true
                                                                         :key-fn str
                                                                         }))
          expected "'aa-key','bb-key'
                    'aaa','b,b'
                    'aa2','bb2'  "]
      (is (str/nonblank-lines= result expected)))))

(verify
  (is-nonblank-lines=
    (entities->csv
      [{:zip-postal-code "01002" :store-num 6 :chain-rank 4}
       {:zip-postal-code "01002" :store-num 277 :chain-rank 5}
       {:zip-postal-code "01003" :store-num 277 :chain-rank 5}
       {:zip-postal-code "01008" :store-num 1217 :chain-rank 5}
       {:zip-postal-code "01009" :store-num 439 :chain-rank 5}
       {:zip-postal-code "01020" :store-num 1193 :chain-rank 5}])
    "chain-rank,store-num,zip-postal-code
    4,6,01002
    5,277,01002
    5,277,01003
    5,1217,01008
    5,439,01009
    5,1193,01020 "))

