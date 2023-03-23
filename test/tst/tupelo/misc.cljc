;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.misc
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             ; [tupelo.core]
             [tupelo.misc]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab]]
    [tupelo.testy :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]]

    [tupelo.misc :as misc]

    #?(:clj [tupelo.java-time :as tjt])
    #?(:cljs [goog.crypt :as crypt])
    #?(:cljs [goog.crypt.Sha1])
    )
  #?(:clj
     (:import
       [java.lang Byte Integer]
       [java.time Clock Instant ZonedDateTime]
       [java.sql Timestamp]
       [java.util Date UUID]
       )))

;---------------------------------------------------------------------------------------------------
#?(:cljs (enable-console-print!))
;---------------------------------------------------------------------------------------------------

(dotest
  (is= "00c81555" (misc/hash->hex 5))
  (is= "64c47d9a" (misc/hash->hex [5]))
  (is= "7bc71a4c" (misc/hash->hex [5 6 :a "hello"]))
  (is= "2e1d6bb4" (misc/hash->hex "xyz1"))
  (is= "41f1824c" (misc/hash->hex "xyz1" "abd"))
  (is= "8410d26a" (misc/hash->hex ["xyz1" "abc"]))
  (is= "14e51713" (misc/hash->hex ["xyz2" "abc"])))

;---------------------------------------------------------------------------------------------------
(dotest
  (is= 5 (misc/namespace-strip 5))
  (is= "abc" (misc/namespace-strip "abc"))
  (is= :item (misc/namespace-strip :something.really.big/item))
  (is= 'item (misc/namespace-strip 'something.really.big/item))
  (is= (misc/walk-namespace-strip
         {:a 1 :my/b [1 'her/c 3] :his/d {:e 5 :other/f :very.last/one}})
    {:a 1 :b [1 'c 3] :d {:e 5 :f :one}}))

;---------------------------------------------------------------------------------------------------
(defrecord Dummy [a b c])
(dotest
  (let [arec  (->Dummy 1 2 3)
        amap  {:a 1 :b 2 :c 3}
        amap2 (misc/walk-rec->map arec)]
    (is= (record? arec) true)
    (is= (map? arec) true)

    (is= (record? amap) false)
    (is= (map? amap) true)

    (is= (record? amap2) false)
    (is= (map? amap2) true)

    (isnt= amap arec)
    (is= amap amap2)))


;---------------------------------------------------------------------------------------------------
(dotest
  (let [data [1 2 3]]
    (is= (t/type-name-str data) ; #todo => tst.tupelo.core
      #?(:clj  "clojure.lang.PersistentVector"
         :cljs "cljs.core/PersistentVector"))
    (is (= (drop 0 data) [1 2 3]))
    (is (= (drop 1 data) [  2 3]))
    (is (= (drop 2 data) [    3]))
    (is (= (drop 3 data) [     ]))))

(dotest
  (is= 1 (misc/boolean->binary true))
  (is= 0 (misc/boolean->binary false))
  (throws? (misc/boolean->binary "hello"))
  (throws? (misc/boolean->binary 234)))

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
  (is= (misc/str->sha "abd") "cb4cc28df0fdbe0ecf9d9662e294b118092a5735")
  (is= (misc/str->sha "hello") "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d")

  (is= "356a192b7913b04c54574d18c28d46e6395428ab" (misc/edn->sha 1))
  (is= "e8dc057d3346e56aed7cf252185dbe1fa6454411" (misc/edn->sha 1.0))
  (is= "a4839edbf020b8c1ac398fa119979fc5384f52d4" (misc/edn->sha :a))
  (is= "7b3ce68b6c2f7d67dae4210eeb83be69f978e2a8" (misc/edn->sha "a"))
  (is= "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8" (misc/edn->sha (quote a)))

  (is= "6d780b01458b623aa5f77db71ac9a02ff1d5ecda" (misc/edn->sha [1 2 3])) ; vec of len 3
  (is= "aad1409b889ef360dad475dc32649f26d9df142a" ; vec, list, seq of len 2
    (misc/edn->sha [1 2])
    (misc/edn->sha (list 1 2))
    (misc/edn->sha (seq [1 2])))
  (is= "9c59f4aebb1a5db2f29609c027155f5867f1060d" ; vector & lazy seq
    (misc/edn->sha (vec (range 999)))
    (misc/edn->sha (range 999)))
  (is= "6d78b62f48aafe38bbbb2a977f0d578109c0c8e2" ; map order doesn't matter
    (misc/edn->sha {:a 1 :b 2})
    (misc/edn->sha {:b 2 :a 1}))
  (is= "c071ca0471e2ed68a46db1db4c8cf84c2a1c7806" ; set order doesn't matter
    (misc/edn->sha #{1 2 :b :a})
    (misc/edn->sha #{:a 1 :b 2})))

(dotest
  (is= misc/int->hex {0 \0, 1 \1, 2 \2, 3 \3, 4 \4, 5 \5, 6 \6, 7 \7, 8 \8, 9 \9, 10 \a, 11 \b, 12 \c, 13 \d, 14 \e, 15 \f})
  (is= misc/hex->int {\0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \a 10, \b 11, \c 12, \d 13, \e 14, \f 15})

  (let [unsigned-vals   [0 15 16 240 255]
        signed-vals     (misc/bytes-unsigned->signed unsigned-vals)
        unsigned-vals-2 (misc/bytes-signed->unsigned signed-vals)
        hex-expected    "000f10f0ff"]
    (is= unsigned-vals unsigned-vals-2)
    (is= signed-vals [0 15 16 -16 -1])
    (is= hex-expected
      (misc/bytes-unsigned->hex-str unsigned-vals)
      (misc/bytes-signed->hex-str signed-vals))
    (let [unsigned-vals-reverse (misc/hex-str->unsigned-bytes hex-expected)
          signed-vals-reverse   (misc/hex-str->signed-bytes hex-expected)]
      (is= unsigned-vals unsigned-vals-reverse)
      (is= signed-vals signed-vals-reverse))

    #?(:cljs
       (do
         (is= "000f10f0ff" (crypt/byteArrayToHex (into-array unsigned-vals))) ; ***** must be unsigned bytes *****
         (let [u (random-uuid)]
           ;(spyx u)
           (is= cljs.core/UUID (type u))
           ;(spyx (misc/uuid->sha1 u))
           ))))

  (let [vals (range 32)]
    (is=
      (misc/bytes-unsigned->hex-str vals)
      (misc/bytes-signed->hex-str vals)
      (str
        "00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "0a" "0b" "0c" "0d" "0e" "0f"
        "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "1a" "1b" "1c" "1d" "1e" "1f")))

  (let [unsigned-vals (range (- 256 16) 256)
        signed-vals   (misc/bytes-unsigned->signed unsigned-vals)]
    (is= unsigned-vals (misc/bytes-signed->unsigned signed-vals))
    (is=
      (misc/bytes-unsigned->hex-str unsigned-vals)
      (str "f0" "f1" "f2" "f3" "f4" "f5" "f6" "f7" "f8" "f9" "fa" "fb" "fc" "fd" "fe" "ff")))

  (let [uuid-val #uuid "0b37e120-2c65-11e7-aa8d-91b7120fbbd1"] ; tagged-literal literal for UUID type
   #?(:clj
      (t/when-clojure-1-9-plus
        (is (uuid? uuid-val))))
   (is= uuid-val #uuid "0b37e120-2c65-11e7-aa8d-91b7120fbbd1")
    (is= (pr-str uuid-val) "#uuid \"0b37e120-2c65-11e7-aa8d-91b7120fbbd1\"")
    (is= (type uuid-val) (do #?(:clj java.util.UUID)
                             #?(:cljs cljs.core/UUID)))
    (is= (misc/uuid->sha uuid-val) "03a49d4729c971a0dc8ddf8d8847290416ad58d2")))

(dotest
  (let [mm (t/unlazy {:a 1
                    :b (t/thru 21 30)
                    :c 3
                    :d 4
                    :e 5
                    :f 6
                    :g 7
                    :h 8
                    :i 9
                    :j 10})]
    ))

;---------------------------------------------------------------------------------------------------
(dotest
  (defn probe-state []
    (let [result (atom #{})]
      (misc/when-debug-flag :a1 (swap! result conj :a1))
      (misc/when-debug-flag :a2 (swap! result conj :a2))
      (misc/when-debug-flag :b2 (swap! result conj :b2))
      (misc/when-debug-flag :b3 (swap! result conj :b3))
      @result))

  (dotest
    ; (t/spyx :awt01-enter misc/*debug-flags*)
    (is= #{} (probe-state))
    (misc/with-debug-flag :a1
      ; (t/spyx :awt01-m1 misc/*debug-flags*)
      (is= #{:a1} (probe-state))
      (misc/with-debug-flag :a2
        ; (t/spyx :awt01-m2 misc/*debug-flags*)
        (is= #{:a1 :a2} (probe-state))
        (misc/with-debug-flag :b2
          ; (t/spyx :awt01-m3 misc/*debug-flags*)
          (is= #{:a1 :a2 :b2} (probe-state)))))
    ; (t/spyx :awt01-exit misc/*debug-flags*)
    (is= #{} (probe-state))))

;---------------------------------------------------------------------------------------------------
#?(:clj
   (do
     (dotest
       (is (#{:windows :linux :mac} (misc/get-os))))

     ; #todo fixed 2019-4-13  #remove if keeps working
     ;***************************************************************************************************
     ;***** WARNING!  (OBE) These tests using BASH or ZSH will cause lein test-refresh to malfunction!
     ;***** WARNING!  (OBE) We mark them as ^:slow to prevent test-refresh from attempting to run them
     ;***************************************************************************************************
     (dotest ; was:   cljtst/deftest ^:slow t-shell-cmd-165
         (when (= :linux (misc/get-os))
           (let [result (misc/shell-cmd "ls -ldF *")]
             (when false ; set true -> debug print
               (println "(:out result)")
               (println (:out result)))
             (is (= 0 (:exit result))))
           (let [result (misc/shell-cmd "ls /bin/bash")]
             (is (= 0 (:exit result)))
             (is (= 1 (count (re-seq #"/bin/bash" (:out result))))))
           (throws? RuntimeException (misc/shell-cmd "LLLls -ldF *"))))

     (dotest
       (when (= :linux (misc/get-os))
         (binding [misc/*os-shell* "/bin/sh"]
           (let [result (misc/shell-cmd "ls /bin/*sh")]
             (is= 0 (:exit result))
             (is (pos? (count (re-seq #"/bin/bash" (:out result)))))))
         (binding [misc/*os-shell* "/bin/zsh"]
           (let [result (misc/shell-cmd "ls /bin/*sh")]
             (is= 0 (:exit result))
             (is (pos? (count (re-seq #"/bin/bash" (:out result)))))))))

     (dotest
       (when true
         (misc/dots-reset!)
         (misc/dots-config! {:dots-per-row 10 :decimation 1})
         (let [result   (with-out-str
                          (misc/with-dots
                            (doseq [x (range 9)]
                              (misc/dot))))
               expected "0 .........
                         9 total"]
           (is-nonblank= result expected)))

       (when true
         (misc/dots-reset!)
         (misc/dots-config! {:dots-per-row 10 :decimation 3})
         (let [result   (with-out-str
                          (misc/with-dots
                            (doseq [x (range 99)]
                              (misc/dot))))
               expected "  0 ..........
                          30 ..........
                          60 ..........
                          90 ...
                          99 total"]
           (is-nonblank= result expected)))

       (when true
         (misc/dots-reset!)
         (misc/dots-config! {:enabled? false})
         (let [result (with-out-str
                        (misc/with-dots
                          (doseq [x (range 99)]
                            (misc/dot))))]
           (is-nonblank= result ""))))

     ;(dotest
     ;  (spyx-pretty (misc/stacktrace-info (RuntimeException. "dummy"))))

     (do
       (defn add2 [x y]
         (let [add2-info        (misc/fn-info)
               add2-caller-info (misc/fn-info-caller)
               sum              (+ 2 3)]
           (t/vals->map add2-info add2-caller-info sum)))
       (defn add2-parent [] (add2 2 3))

       (dotest
         (let [result (add2-parent)]
           ; (spyx-pretty result)
           (comment ; sample results
             {:add2-info
                   {:class-name "tst.tupelo.misc$add2",
                    :file-name "misc.cljc",
                    :method-name "invokeStatic",
                    :line-num 213,
                    :ns-name "tst.tupelo.misc",
                    :fn-name "add2"},
              :add2-caller-info
                   {:class-name "tst.tupelo.misc$add2_parent",
                    :file-name "misc.cljc",
                    :method-name "invokeStatic",
                    :line-num 217,
                    :ns-name "tst.tupelo.misc",
                    :fn-name "add2_parent"},
              :sum 5}
             )

           (is= (get-in result [:add2-info :class-name]) "tst.tupelo.misc$add2")
           (is= (get-in result [:add2-info :ns-name]) "tst.tupelo.misc")
           (is= (get-in result [:add2-info :fn-name]) "add2")

           (is= (get-in result [:add2-caller-info :class-name]) "tst.tupelo.misc$add2_parent")
           (is= (get-in result [:add2-caller-info :ns-name]) "tst.tupelo.misc")
           (is= (get-in result [:add2-caller-info :fn-name]) "add2_parent" ))))
     ))

;---------------------------------------------------------------------------------------------------
#?(:cljs
   (do
     (dotest
       (is= (misc/grouper #"[a-z0-9][A-Z]" "aTaTa")
         [{:groups ["aT"] :match "aT" :index 0 :last-index 2 :input "aTaTa"}
          {:groups ["aT"] :match "aT" :index 2 :last-index 4 :input "aTaTa"}])

       (is= (misc/grouper #"((\d+)-(\d+))" "672-345-456-3212")
         [{:groups ["672-345"  "672-345"  "672"  "345"] :match "672-345"  :index 0 :last-index  7 :input "672-345-456-3212"}
          {:groups ["456-3212" "456-3212" "456" "3212"] :match "456-3212" :index 8 :last-index 16 :input "672-345-456-3212"}]))
     ))

