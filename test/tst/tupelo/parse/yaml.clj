(ns tst.tupelo.parse.yaml
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.data :as data]
    [clojure.string :as str]
    [tupelo.parse.yaml :as yaml]
    ))

(verify
  (let [document "\n- Hesperiidae\n- Papilionidae\n- Apatelodidae\n- Epiplemidae"]
    (is= (yaml/parse document)
      ["Hesperiidae" "Papilionidae" "Apatelodidae" "Epiplemidae"]))

  (is= (yaml/parse "{a: 1, 5: 'abc'}") {:a 1 5 "abc"})

  (is= (yaml/parse "{a: 1, 'c': true}") {:a 1 :c true})

  (is= (yaml/parse "a: 1\nb: 2\nc:\n  - aaa\n  - bbb") {:a 1 :b 2 :c ["aaa" "bbb"]})

  (let [document "a: 1
                 'c': true
                  z: 3 "
        doc-src  (it-> document
                   (str/split-lines it)
                   (mapv str/trim it)
                   (str/join \newline it))]
    (is= (yaml/parse doc-src) {:a 1 :c true :z 3})))

(verify
  (is= (yaml/parse-all "bbb\n---\nccc\n---\nddd")
    ["bbb" "ccc" "ddd"]))

(verify
  (is= (yaml/parse "[]") [])
  (is= (yaml/parse "[2]") [2])
  (is= (yaml/parse "[2,3]") [2 3])
  (is= (yaml/parse "[2,a,true]") [2 "a" true]) )

;----------------------------------------------------------------------------
(verify
  (is-nonblank= "a" (yaml/edn->yaml "a"))
  (is-nonblank= "1" (yaml/edn->yaml 1))
  (is-nonblank= "true" (yaml/edn->yaml true))
  (is-nonblank-lines= (yaml/edn->yaml [2 "a" true])
    " - 2
      - a
      - true ")
  (is-nonblank-lines= (yaml/edn->yaml {:a 1 :b 2 :c 3})
    "a: 1
     b: 2
     c: 3" ))

;----------------------------------------------------------------------------
(def yaml-str
"
tupelo:
  name: Tupelo
  url: https://github.com/wizard-enterprises/tupelo
  categories:
    - Data Transformation
    - Date and Time
    - Datomic
    - Misc. Functions
    - Unit Testing
    - HTML Parsers
    - JSON Parsers
    - YAML Parsers
    - XML Parsers
  platforms: [clj, cljs]
" )

(verify
  (let [edn-data   {:tupelo
                    {:name       "Tupelo"
                     :url        "https://github.com/wizard-enterprises/tupelo"
                     :categories ["Data Transformation"
                                  "Date and Time"
                                  "Datomic"
                                  "Misc. Functions"
                                  "Unit Testing"
                                  "HTML Parsers"
                                  "JSON Parsers"
                                  "YAML Parsers"
                                  "XML Parsers"]
                     :platforms  ["clj" "cljs"]}}
        yaml-str-2 "tupelo:
                      name: Tupelo
                      url: https://github.com/wizard-enterprises/tupelo
                      categories: [Data Transformation, Date and Time, Datomic, Misc. Functions, Unit
                                   Testing, HTML Parsers, JSON Parsers, YAML Parsers, XML Parsers]
                      platforms: [clj, cljs] "

        edn-parsed (yaml/parse yaml-str)
        m1         (:tupelo edn-parsed)]
    (is (map? edn-parsed))
    (is (map? m1))
    (is= edn-parsed edn-data)

    (is-nonblank-lines= (yaml/edn->yaml edn-data)
      "tupelo:
         name: Tupelo
         url: https://github.com/wizard-enterprises/tupelo
         categories:
         - Data Transformation
         - Date and Time
         - Datomic
         - Misc. Functions
         - Unit Testing
         - HTML Parsers
         - JSON Parsers
         - YAML Parsers
         - XML Parsers
         platforms:
         - clj
         - cljs")


    (is= edn-data (-> edn-data
                    (yaml/edn->yaml)
                    (yaml/parse)))

    ))
