(ns tst.tupelo.parse.yaml
  (:use tupelo.parse.yaml tupelo.core tupelo.test)
  (:require [clojure.string :as str]))

(dotest
  (let [document "\n- Hesperiidae\n- Papilionidae\n- Apatelodidae\n- Epiplemidae"]
    (is= (load-yaml-from-string document)
      ["Hesperiidae" "Papilionidae" "Apatelodidae" "Epiplemidae"]))

  (is= (load-yaml-from-string "{a: 1, 5: 'abc'}") {:a 1 5 "abc"})

  (is= (load-yaml-from-string "{a: 1, 'c': true}") {:a 1 :c true})

  (is= (load-yaml-from-string "a: 1\nb: 2\nc:\n  - aaa\n  - bbb") {:a 1 :b 2 :c ["aaa" "bbb"]})

  (let [document "a: 1
                 'c': true
                  z: 3 "
        doc-src  (it-> document
                   (str/split-lines it)
                   (mapv str/trim it)
                   (str/join \newline it))]
    (is= (load-yaml-from-string doc-src) {:a 1 :c true :z 3})))

(dotest
  (is= (load-all-yaml-from-string "bbb\n---\nccc\n---\nddd")
    ["bbb" "ccc" "ddd"]))

(dotest
  (is= (load-yaml-from-string "[]") [])
  (is= (load-yaml-from-string "[2]") [2])
  (is= (load-yaml-from-string "[2,3]") [2 3])
  (is= (load-yaml-from-string "[2,a,true]") [2 "a" true]) )

;----------------------------------------------------------------------------
(dotest
  (is= "a\n" (yaml-dump-to-string "a"))
  (is= "1\n" (yaml-dump-to-string 1))
  (is= "true\n" (yaml-dump-to-string true))
  (is= "[2, a, true]\n" (yaml-dump-to-string [2 "a" true]))
  (is= "{a: 1, b: 2, c: 3}\n" (yaml-dump-to-string {:a 1 :b 2 :c 3})))

