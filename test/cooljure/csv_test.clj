(ns cooljure.csv-test
  (:require [clojure.java.io            :as io]
            [clojure.string             :as str]
            [clojure-csv.core           :as csv]
            [cooljure.csv               :as cool-csv]
            [cooljure.misc              :as cool-misc]
            [kits.parse                 :as kparse] )
  (:use [cooljure.csv]
        [clojure.test] )
)
(set! *warn-on-reflection* false)

(deftest keyvals-test
  (testing "basic usage"
    (let [mm {:a 1 :b 2 :c 3} ]
      (is (= mm (apply hash-map (keyvals mm)))) )))
; AWTAWT TODO: add test.generative

(def home-dir (System/getProperty "user.home"))
(def store-zip-file  
  (str home-dir 
    (first [
      "/local-data/store-to-zip.psv"
      "/local-data/store-to-zip.csv"
    ] )))

(defn load-store-zip-maps-v0
  [store-zip-file]
  (let [csv-data    (csv/parse-csv (slurp store-zip-file) :delimiter \| )
        hdrs-kw     (mapv cool-misc/str->kw (first csv-data))
        data-maps   (for [data-row (rest csv-data)]
                      (let [raw-map   (zipmap hdrs-kw data-row)
                            data-map  (hash-map 
                                        :store-id (kparse/str->long (:STORE-NUM raw-map))
                                        :zipcode  (:ZIP-POSTAL-CODE raw-map) )
                      ] data-map ))
  ] data-maps ))
(def store-zip-maps-v0 (load-store-zip-maps-v0  store-zip-file)) 

(defn load-store-zip-maps
  [store-zip-file]
  (let [_ (println "a1")
        raw-maps    (cool-csv/parse-csv->maps store-zip-file :delimiter \| )
        _ (println "a2")
        data-maps   (map #(hash-map :store-id (kparse/str->long (:STORE-NUM %))
                                    :zipcode                    (:ZIP-POSTAL-CODE %) )
                      raw-maps )
        _ (println "a3")
  ] data-maps ))
(def store-zip-maps (load-store-zip-maps  store-zip-file)) 

(deftest basic
  (testing "basic usage"
    (println "store-zip-maps-v0")
    (prn (take 5 store-zip-maps-v0))
    (println "store-zip-maps")
    (prn (take 5 store-zip-maps))

    (is (= 1 1)) ))

