;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.csv
  (:use tupelo.csv tupelo.core tupelo.test)
  (:require
    [camel-snake-kebab.core :as csk]
    [tupelo.parse :as parse]
    [tupelo.string :as str]
    [clojure.walk :as walk])
  (:import
    [java.io StringReader]
    ))

(def data1-str-no-label
  "01002,00006,4
  01002,00277,5
  01003,00277,5
  01008,01217,5
  01009,00439,5
  01020,01193,5")

(def data1-str
  (str "zip_postal_code,store$num,chain#rank"
    \newline
    data1-str-no-label))

(def data2-str-label
  "ZIP_POSTAL_CODE|STORE_NUM|CHAIN_RANK
  01002|00006|4
  01002|00277|5
  01003|00277|5
  01008|01217|5
  01009|00439|5
  01020|01193|5")

(def expected-1-entity
  [{:zip-postal-code "01002" :store-num "00006" :chain-rank "4"}
   {:zip-postal-code "01002" :store-num "00277" :chain-rank "5"}
   {:zip-postal-code "01003" :store-num "00277" :chain-rank "5"}
   {:zip-postal-code "01008" :store-num "01217" :chain-rank "5"}
   {:zip-postal-code "01009" :store-num "00439" :chain-rank "5"}
   {:zip-postal-code "01020" :store-num "01193" :chain-rank "5"}])

(def expected-1-attr
  {:zip-postal-code ["01002" "01002" "01003" "01008" "01009" "01020"]
   :store-num       ["00006" "00277" "00277" "01217" "00439" "01193"]
   :chain-rank      ["4" "5" "5" "5" "5" "5"]})

(def expected-2-entity
  [{:zipcode "01002" :store-id 6}
   {:zipcode "01002" :store-id 277}
   {:zipcode "01003" :store-id 277}
   {:zipcode "01008" :store-id 1217}
   {:zipcode "01009" :store-id 439}
   {:zipcode "01020" :store-id 1193}])

(def expected-2-attr
  {:zipcode  ["01002" "01002" "01003" "01008" "01009" "01020"]
   :store-id [6 277 277 1217 439 1193]})

(dotest
  ; basic parse-csv->rows test, using String
  (let [result (csv->entities data1-str)]
    (is= result expected-1-entity))

  ; no header row in file, user spec :labels
  (let [result (csv->entities data1-str-no-label
                 :labels [:zip-postal-code :store-num :chain-rank])]
    (is= result expected-1-entity))

  ; basic parse-csv->rows test, using Reader
  (let [result (csv->entities (StringReader. data1-str))]
    (is= result expected-1-entity))

  ; read PSV file instead of default CSV"
  (let [result (forv [entity (csv->entities data2-str-label :delimiter \|)]
                 {:store-id (parse/parse-long (grab :STORE-NUM entity))
                  :zipcode  (grab :ZIP-POSTAL-CODE entity)})]
    (is= result expected-2-entity)))

(dotest
  (is= {} (entities->attrs []))
  (is= [] (attrs->entities {}))

  (is= (entities->attrs expected-1-entity) expected-1-attr)
  (is= (entities->attrs expected-2-entity) expected-2-attr)

  (is= (attrs->entities expected-1-attr) expected-1-entity)
  (is= (attrs->entities expected-2-attr) expected-2-entity))

(dotest
  (is= (csv->attrs data1-str) expected-1-attr)

  (let [raw-maps (csv->attrs data2-str-label :delimiter \|)
        result   {:store-id (mapv parse/parse-long (grab :STORE-NUM raw-maps))
                  :zipcode  (:ZIP-POSTAL-CODE raw-maps)}]
    (is= result expected-2-attr)))

(dotest
  (let [test2-str-label-error "zipcode, store_num, chain_rank
                                  01002,00006,4
                                  01002,00277,5
                                  01003,00277"]
    (throws? (csv->entities test2-str-label-error))))

(dotest
  (let [sample-edn [{:aa-key "aaa" :bb-key "b,b"} ; 2nd val needs to be quoted
                    {:aa-key "aa2" :bb-key "bb2"}]]
    ; is verified-keys working?
    (is= (verified-keys sample-edn) [:aa-key :bb-key])
    (throws?
      (verified-keys
        [{:aa-key "aaa" :bb-X "b,b"} ; "bb" keys don't match
         {:aa-key "aa2" :bb-key "bb2"}]))

    ; 'b,b' value quoted correctly
    (do
      (let [edn-str-keys (walk/postwalk
                           (fn [item]
                             (cond-it-> item
                               (keyword? it) (kw->str it)))
                           sample-edn)]
        (is (str/nonblank-lines=
              (entities->csv edn-str-keys) ; not map key tx
              (str/quotes->double
                "aa-key,bb-key
                 aaa,'b,b'
                 aa2,bb2 "))))

      ; 'b,b' value quoted correctly
      (is (str/nonblank-lines=
            (entities->csv sample-edn csk/->snake_case_string)
            (str/quotes->double
              "aa_key,bb_key
               aaa,'b,b'
               aa2,bb2 "))))
    ))





