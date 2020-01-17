(ns tst.tupelo.parse.yaml
  (:use  tupelo.core tupelo.test)
  (:require
    [clojure.data :as data]
    [clojure.string :as str]
    [tupelo.parse.yaml :as yaml]
    ))

(dotest
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

(dotest
  (is= (yaml/parse-all "bbb\n---\nccc\n---\nddd")
    ["bbb" "ccc" "ddd"]))

(dotest
  (is= (yaml/parse "[]") [])
  (is= (yaml/parse "[2]") [2])
  (is= (yaml/parse "[2,3]") [2 3])
  (is= (yaml/parse "[2,a,true]") [2 "a" true]) )

;----------------------------------------------------------------------------
(dotest
  (is= "a\n" (yaml/edn->yaml "a"))
  (is= "1\n" (yaml/edn->yaml 1))
  (is= "true\n" (yaml/edn->yaml true))
  (is= "[2, a, true]\n" (yaml/edn->yaml [2 "a" true]))
  (is= "{a: 1, b: 2, c: 3}\n" (yaml/edn->yaml {:a 1 :b 2 :c 3})))

;----------------------------------------------------------------------------
(def yaml-str
"
tupelo:
  name: Tupelo
  url: https://github.com/cloojure/tupelo
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

(dotest
  (let [edn-data   {:tupelo
                    {:name       "Tupelo"
                     :url        "https://github.com/cloojure/tupelo"
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
                      url: https://github.com/cloojure/tupelo
                      categories: [Data Transformation, Date and Time, Datomic, Misc. Functions, Unit
                                   Testing, HTML Parsers, JSON Parsers, YAML Parsers, XML Parsers]
                      platforms: [clj, cljs] "

        edn-parsed (yaml/parse yaml-str)
        m1         (:tupelo edn-parsed)]
    (is (map? edn-parsed))
    (is (map? m1))
    (is= edn-parsed edn-data)

    (when false
      (nl) (println :awt01)
      (println (yaml/edn->yaml edn-data)))
    (is-nonblank= yaml-str-2 (yaml/edn->yaml edn-data))
    (is= edn-data (-> edn-data
                    (yaml/edn->yaml)
                    (yaml/parse)))

    (let [dumped ()])
    ))




