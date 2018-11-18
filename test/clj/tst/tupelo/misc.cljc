;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.misc
  (:require
    [tupelo.misc :as misc]
    #?@(:clj [[schema.core :as s]
              [tupelo.test :refer [define-fixture dotest is isnt is= isnt= set= nonblank= testing throws?]]
              [tupelo.core :as t :refer [spy spyx spyxx]]
              [tupelo.string :as ts]
              ])
    #?@(:cljs [[schema.core :as s]
               [tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= set= nonblank= testing throws?]]
               [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
               [tupelo.string :as ts :include-macros true]
               [goog.crypt :as crypt]
               [goog.crypt.Sha1]
               ])))

#?(:cljs (enable-console-print!))


(dotest
  (let [data [1 2 3]]
    (is (= (drop 0 data) [1 2 3]))
    (is (= (drop 1 data) [  2 3]))
    (is (= (drop 2 data) [    3]))
    (is (= (drop 3 data) [     ]))))

(dotest
  (is= (misc/factorial 0) 1)
  (is= (misc/factorial 1) 1)
  (is= (misc/factorial 2) 2)
  (is= (misc/factorial 3) 6)
  (is= (misc/factorial 4) 24)
  (is= (misc/factorial 5) 120)
  (is= (misc/factorial 6) 720)
  (is= (misc/factorial 7) 5040)
  (is= (misc/factorial 8) 40320)
  (is= (misc/factorial 9) 362880)
  (is= (misc/factorial 10) 3628800)
  (is (t/rel= (misc/factorial 15) 1.307674368e+12 :digits 10))
  (throws? (misc/factorial 1.5))
  (throws? (misc/factorial -1))
  (throws? (misc/factorial -1)))

(dotest
  ;              0 1 2  3    4    5    6    7   8 9]
  (let [data [0 1 2 0xAA 0xFA 0xFF 0xDA 0xDD 8 9]]
    (is= 5 (first (misc/find-pattern [0xFF 0xDA] data))))
  (let [data [0 1 2 3 0 1 2 3 0 1 2 3]]
    (is= [0 4 8]
      (misc/find-pattern [0] data)
      (misc/find-pattern [0 1] data)
      (misc/find-pattern [0 1 2] data)
      (misc/find-pattern [0 1 2 3] data))
    (is= [1 5 9]
      (misc/find-pattern [1] data)
      (misc/find-pattern [1 2] data)
      (misc/find-pattern [1 2 3] data))
    (is= [1 5]
      (misc/find-pattern [1 2 3 0] data))
    (is= [2 6 10]
      (misc/find-pattern [2] data)
      (misc/find-pattern [2 3] data))
    (is= [2 6]
      (misc/find-pattern [2 3 0] data)
      (misc/find-pattern [2 3 0 1] data)))
  (is= [0] (misc/find-pattern [0] [0 1 2 3]))
  (is= [1] (misc/find-pattern [1] [0 1 2 3]))
  (is= [2] (misc/find-pattern [2] [0 1 2 3]))
  (is= [3] (misc/find-pattern [3] [0 1 2 3]))
  (is= [] (misc/find-pattern [3 4] [0 1 2 3]))
  (is= [] (misc/find-pattern [9] [0 1 2 3])) )


(dotest
  (is= (misc/str->sha "abc") "a9993e364706816aba3e25717850c26c9cd0d89d")
  (is= (misc/str->sha "abd") "cb4cc28df0fdbe0ecf9d9662e294b118092a5735"))

;----- toptop -----------------------------------------------------------------------------

#?(:clj
   (do


     (dotest
       (is (#{:windows :linux :mac} (misc/get-os))))

     (dotest
       (when (= :linux (misc/get-os))
         (let [result (misc/shell-cmd "ls -ldF *")]
           (when false ; set true -> debug print
             (println "(:out result)")
             (println (:out result)))
           (is (= 0 (:exit result))))
         (let [result (misc/shell-cmd "ls /bin/bash")]
           (is (= 0 (:exit result)))
           (is (= 1 (count (re-seq #"/bin/bash" (:out result))))))
         (binding [misc/*os-shell* "/bin/sh"]
           (let [result (misc/shell-cmd "ls /bin/*sh")]
             (is (= 0 (:exit result)))
             (is (< 0 (count (re-seq #"/bin/bash" (:out result)))))))

         (throws? RuntimeException (misc/shell-cmd "LLLls -ldF *"))))

     (dotest
       (misc/dots-config! {:dots-per-row 10 :decimation 1})
       (is= (ts/collapse-whitespace (with-out-str
                                      (misc/with-dots
                                        (doseq [x (range 9)]
                                          (misc/dot)))))
         (ts/collapse-whitespace
           "0 .........
            9 total"))

       (misc/dots-config! {:dots-per-row 10 :decimation 3})
       (is= (ts/collapse-whitespace (with-out-str
                                      (misc/with-dots
                                        (doseq [x (range 99)]
                                          (misc/dot)))))
         (ts/collapse-whitespace
           "  0 ..........
             30 ..........
             60 ..........
             90 ...
             99 total")))

; ----- gogo -----------------------------------------------------------------------------
     (dotest
       (is= (misc/bytes->hex-str (byte-array (range 32)))
         (str
           "00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f"
           "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "1a" "1b" "1c" "1d" "1e" "1f"))
       (is= (misc/bytes->hex-str (byte-array (range (- 256 16) 256)))
         (str "f0" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "fa" "fb" "fc" "fd" "fe" "ff"))

       (let [uuid-val #uuid "0b37e120-2c65-11e7-aa8d-91b7120fbbd1"] ; tagged-literal literal for UUID type
         (is= uuid-val #uuid "0b37e120-2c65-11e7-aa8d-91b7120fbbd1")
         (is= (class uuid-val) java.util.UUID)
         (is= (pr-str uuid-val) "#uuid \"0b37e120-2c65-11e7-aa8d-91b7120fbbd1\"")
         (is= (misc/uuid->str uuid-val) "e604d9bbcfb53cee6c3f305992c4a1531972b7a1")
         (is= (misc/str->sha "hello") "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d")))

     ))
