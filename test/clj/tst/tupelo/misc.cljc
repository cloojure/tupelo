;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns ^:fast
  tst.tupelo.misc
  (:use tupelo.impl tupelo.misc tupelo.test )
  (:require
    #?@(:clj [
    [tupelo.impl    :as i ]
              [tupelo.string    :as ts ]
              ])
  ))

#?(:clj (do


(dotest
  (let [data [1 2 3]]
    (is (= (drop 0 data) [1 2 3]))
    (is (= (drop 1 data) [  2 3]))
    (is (= (drop 2 data) [    3]))
    (is (= (drop 3 data) [     ])))
)

(dotest
  (is (#{:windows :linux :mac} (get-os))))

(dotest
  (when (= :linux (get-os))
    (let [result (shell-cmd "ls -ldF *")]
         (when false ; set true -> debug print
           (println "(:out result)")
           (println (:out result)))
      (is (= 0 (:exit result))))
    (let [result (shell-cmd "ls /bin/bash")]
         (is (= 0 (:exit result)))
      (is (= 1 (count (re-seq #"/bin/bash" (:out result))))))
    (binding [*os-shell* "/bin/sh"]
      (let [result (shell-cmd "ls /bin/*sh")]
           (is (= 0 (:exit result)))
        (is (< 0 (count (re-seq #"/bin/bash" (:out result)))))))

    (throws? RuntimeException (shell-cmd "LLLls -ldF *"))))

(dotest
  (dots-config! {:dots-per-row 10 :decimation 1})
  (is= (ts/collapse-whitespace (with-out-str
                                 (with-dots
                                   (doseq [x (range 9)]
                                     (dot)))))
    (ts/collapse-whitespace
      "0 .........
       9 total"))

  (dots-config! {:dots-per-row 10 :decimation 3})
  (is= (ts/collapse-whitespace (with-out-str
                                 (with-dots
                                   (doseq [x (range 99)]
                                     (dot)))))
    (ts/collapse-whitespace
      "  0 ..........
        30 ..........
        60 ..........
        90 ...
        99 total")))

(dotest
  (is=         (factorial 0)          1)
  (is=         (factorial 1)          1)
  (is=         (factorial 2)          2)
  (is=         (factorial 3)          6)
  (is=         (factorial 4)         24)
  (is=         (factorial 5)        120)
  (is=         (factorial 6)        720)
  (is=         (factorial 7)       5040)
  (is=         (factorial 8)      40320)
  (is=         (factorial 9)     362880)
  (is=         (factorial 10)   3628800)
  (is (i/rel=  (factorial 15) 1.307674368e+12 :digits 10))
  (throws? (factorial 1.5))
  (throws? (factorial -1))
  (throws? (factorial -1)))

(dotest
;              0 1 2  3    4    5    6    7   8 9]
  (let [data [ 0 1 2 0xAA 0xFA 0xFF 0xDA 0xDD 8 9] ]
    (is= 5 (first (find-pattern [0xFF 0xDA] data))))
  (let [data [ 0 1 2 3 0 1 2 3 0 1 2 3] ]
    (is= [0 4 8]
      (find-pattern [0      ] data)
      (find-pattern [0 1    ] data)
      (find-pattern [0 1 2  ] data)
      (find-pattern [0 1 2 3] data))
    (is= [1 5 9]
      (find-pattern [1      ] data)
      (find-pattern [1 2    ] data)
      (find-pattern [1 2 3  ] data))
    (is= [1 5]
      (find-pattern [1 2 3 0] data))
    (is= [2 6 10]
      (find-pattern [2      ] data)
      (find-pattern [2 3    ] data))
    (is= [2 6]
      (find-pattern [2 3 0  ] data)
      (find-pattern [2 3 0 1] data)))
  (is= [0] (find-pattern [0]   [0 1 2 3] ))
  (is= [1] (find-pattern [1]   [0 1 2 3] ))
  (is= [2] (find-pattern [2]   [0 1 2 3] ))
  (is= [3] (find-pattern [3]   [0 1 2 3] ))
  (is= [ ] (find-pattern [3 4] [0 1 2 3] ))
  (is= [ ] (find-pattern [9]   [0 1 2 3] ))
)

(dotest
  (is= (bytes->hex-str (byte-array (range 32)))
    (str
      "00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f"
      "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "1a" "1b" "1c" "1d" "1e" "1f"))
  (is= (bytes->hex-str (byte-array (range (- 256 16) 256)))
    (str "f0" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "fa" "fb" "fc" "fd" "fe" "ff" ))

  (let [uuid-val    #uuid "0b37e120-2c65-11e7-aa8d-91b7120fbbd1" ]  ; tagged-literal literal for UUID type
    (is= uuid-val #uuid "0b37e120-2c65-11e7-aa8d-91b7120fbbd1")
    (is= (class uuid-val) java.util.UUID )
    (is= (pr-str uuid-val) "#uuid \"0b37e120-2c65-11e7-aa8d-91b7120fbbd1\"" )
    (is= (uuid->str uuid-val) "e604d9bbcfb53cee6c3f305992c4a1531972b7a1" )
    (is= (str->sha "hello") "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d")))

))
