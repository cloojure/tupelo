;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.misc
  (:use tupelo.misc
        tupelo.core
        clojure.test )
  (:require [clojure.string   :as str]
            [schema.core      :as s]
            [tupelo.misc      :as misc] ))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(set! *warn-on-reflection* true)

(deftest t-sample
  (let [data [1 2 3]]
    (is (= (drop 0 data) [1 2 3]))
    (is (= (drop 1 data) [  2 3]))
    (is (= (drop 2 data) [    3]))
    (is (= (drop 3 data) [     ])))
)

(deftest collapse-whitespace-t
  (testing "basic usage"
    (is (= "abc def g hij kl"
            (misc/collapse-whitespace "  abc    def			g
                                       hij kl	 " )))))

(deftest str->kw-t
  (testing "basic usage"
    (is (= :abc-def-gh-qrs (misc/str->kw "abc def*gh_qrs")))))

(deftest char-seq-t
  (is (= [\a ]              (misc/char-seq \a \a)))
  (is (= [\a \b]            (misc/char-seq \a \b)))
  (is (= [\a \b \c]         (misc/char-seq \a \c)))

  (is (= [\a ]              (misc/char-seq 97 97)))
  (is (= [\a \b]            (misc/char-seq 97 98)))
  (is (= [\a \b \c]         (misc/char-seq 97 99)))

  (is (thrown? Exception    (misc/char-seq 987654321 987654321 )))
  (is (thrown? Exception    (misc/char-seq \c \a)))
  (is (thrown? Exception    (misc/char-seq 99 98)))
)

(deftest seq->str-t
  (is (= " 1 2 3"           (misc/seq->str (byte-array [1 2 3]))))
  (is (= " :a :b 3 4"     (misc/seq->str [:a :b 3 4])))
  (is (= " \\a \\b \\c"     (misc/seq->str "abc"))))

(deftest shell-cmd-t
  (testing "no errors"
    (let [result (shell-cmd "ls -ldF *")]
      (when false  ; set true -> debug print
        (println "(:out result)" )
        (println  (:out result)  ))
      (is (= 0 (:exit result))))
    (let [result (shell-cmd "ls /bin/bash")]
      (is (= 0 (:exit result)))
      (is (= 1 (count (re-seq #"/bin/bash" (:out result))))))
    (binding [*os-shell* "/bin/sh"]
      (let [result (shell-cmd "ls /bin/*sh")]
        (is (= 0 (:exit result)))
        (is (< 0 (count (re-seq #"/bin/bash" (:out result)))))))
    )

  (testing "errors"
    (is (thrown? RuntimeException (shell-cmd "LLLls -ldF *")))))

(deftest t-dots
  (dots-config! {:dots-per-row 10  :decimation 1} )
  (is (= "         0 .........\n         9 total\n"
         (with-out-str (with-dots (doseq [x (range 9)]
                                    (dot))))))
  (dots-config! {:dots-per-row 10  :decimation 3} )
  (is (= "         0 ..........\n        30 ..........\n        60 ..........\n        90 ...\n        99 total\n"
         (with-out-str (with-dots (doseq [x (range 99)]
                                    (dot)))))))
(deftest t-factorial
  (is (=     (factorial 0)          1))
  (is (=     (factorial 1)          1))
  (is (=     (factorial 2)          2))
  (is (=     (factorial 3)          6))
  (is (=     (factorial 4)         24))
  (is (=     (factorial 5)        120))
  (is (=     (factorial 6)        720))
  (is (=     (factorial 7)       5040))
  (is (=     (factorial 8)      40320))
  (is (=     (factorial 9)     362880))
  (is (=     (factorial 10)   3628800))
  (is (rel=  (factorial 15) 1.307674368e+12 :digits 10))
  (is (thrown? Exception (factorial 1.5)))
  (is (thrown? Exception (factorial -1)))
  (is (thrown? Exception (factorial -1))))

(deftest t-permute-multiset

; (is (=  (permute-multiset-1 [        ])  [])  ; #todo should be error
  (is (=  (permute-multiset-1 [:a      ])  [[:a]] ))
  (is (=  (permute-multiset-1 [:a :b   ])  [[:a :b] [:b :a]] ))
  (is (=  (permute-multiset-1 [:a :b :c])  [[:a :b :c] [:a :c :b]
                                            [:b :a :c] [:b :c :a]
                                            [:c :a :b] [:c :b :a]] ))

; (is (=  (permute-multiset-2 [        ])  []))  ; #todo should be error
  (is (=  (permute-multiset-2 [:a      ])  [[:a]] ))
  (is (=  (permute-multiset-2 [:a :b   ])  [[:a :b] [:b :a]] ))
  (is (=  (permute-multiset-2 [:a :b :c])  [[:a :b :c] [:a :c :b]
                                            [:b :a :c] [:b :c :a]
                                            [:c :a :b] [:c :b :a]] ))

; (is (=  (permute-multiset-3 [        ])  [[]]))  ; #todo should be error
  (is (=  (permute-multiset-3 [:a      ])  [[:a]] ))
  (is (=  (permute-multiset-3 [:a :b   ])  [[:a :b] [:b :a]] ))
  (is (=  (permute-multiset-3 [:a :b :c])  [[:a :b :c] [:a :c :b]
                                            [:b :a :c] [:b :c :a]
                                            [:c :a :b] [:c :b :a]] ))
)

(deftest t-permute-set
  (is (= (permute-set-1 #{:a      }) #{ [:a      ]            } ))
  (is (= (permute-set-1 #{:a :b   }) #{ [:a :b   ] [:b :a   ] } ))
  (is (= (permute-set-1 #{:a :b :c}) #{ [:a :b :c] [:a :c :b]
                                        [:b :a :c] [:b :c :a]
                                        [:c :a :b] [:c :b :a] } ))
; (is (thrown? IllegalArgumentException (permute-set-1 #{} )))
)

(deftest t-permute-lazy
  (is (=  (permute-lazy-1 [:a      ])  [[:a]] ))
  (is (=  (permute-lazy-1 [:a :b   ])  [[:a :b] [:b :a]] ))
  (is (=  (permute-lazy-1 [:a :b :c])  [[:a :b :c] [:a :c :b]
                                        [:b :a :c] [:b :c :a]
                                        [:c :a :b] [:c :b :a]] ))
  (is (thrown? IllegalArgumentException (permute [] ))))

(deftest t-permute-tail-1
  (is (= (set (permute-tail-1 [:a]))        #{ [:a      ]            } ))
  (is (= (set (permute-tail-1 [:a :b]))     #{ [:a :b   ] [:b :a   ] } ))
  (is (= (set (permute-tail-1 [:a :b :c]))  #{ [:a :b :c] [:a :c :b]
                                               [:b :a :c] [:b :c :a]
                                               [:c :a :b] [:c :b :a] } )))
(deftest t-permute-tail-2
  (is (= (set (permute-tail-2 [:a]))        #{ [:a      ]            } ))
  (is (= (set (permute-tail-2 [:a :b]))     #{ [:a :b   ] [:b :a   ] } ))
  (is (= (set (permute-tail-2 [:a :b :c]))  #{ [:a :b :c] [:a :c :b]
                                               [:b :a :c] [:b :c :a]
                                               [:c :a :b] [:c :b :a] } )))
(deftest t-permute-tail-3
  (is (= (set (permute-tail-3 [:a]))        #{ [:a      ]            } ))
  (is (= (set (permute-tail-3 [:a :b]))     #{ [:a :b   ] [:b :a   ] } ))
  (is (= (set (permute-tail-3 [:a :b :c]))  #{ [:a :b :c] [:a :c :b]
                                               [:b :a :c] [:b :c :a]
                                               [:c :a :b] [:c :b :a] } )))

(deftest t-permute-gen
  (is (thrown? IllegalArgumentException (permute [] )))

  (is (= (set (permute [:a]))        #{ [:a      ]            } ))
; (newline) (newline)
  (is (= (set (permute [:a :b]))     #{ [:a :b   ] [:b :a   ] } ))
; (newline) (newline)
  (is (= (set (permute [:a :b :c]))  #{ [:a :b :c] [:a :c :b]
                                        [:b :a :c] [:b :c :a]
                                        [:c :a :b] [:c :b :a] } ))
; (newline) (newline)
; (pretty (permute [:a :b :c :d]))
  (is (= (set (permute [:a :b :c :d]))
         #{ [ :a :b :c :d ]
            [ :a :b :d :c ]
            [ :a :c :b :d ]
            [ :a :c :d :b ]
            [ :a :d :b :c ]
            [ :a :d :c :b ]

            [ :b :a :c :d ]
            [ :b :a :d :c ]
            [ :b :c :a :d ]
            [ :b :c :d :a ]
            [ :b :d :a :c ]
            [ :b :d :c :a ]

            [ :c :a :b :d ]
            [ :c :a :d :b ]
            [ :c :b :a :d ]
            [ :c :b :d :a ]
            [ :c :d :a :b ]
            [ :c :d :b :a ]

            [ :d :a :b :c ]
            [ :d :a :c :b ]
            [ :d :b :a :c ]
            [ :d :b :c :a ]
            [ :d :c :a :b ]
            [ :d :c :b :a ] } ))
)

(deftest ^:slow t-permute-lazy-count
  (let [check-num-perm (fn [n] (is (= (spy :msg (format "(check-num-perm %d)" n)
                                        (count (permute (thru 1 n))))
                                     (factorial n)))) ]
    (check-num-perm  1)
    (check-num-perm  2)
    (check-num-perm  3)
    (check-num-perm  4)
    (check-num-perm  5)
    (check-num-perm  6)
    (check-num-perm  7)
    (check-num-perm  8)
    (check-num-perm  9)
  ; (check-num-perm 10)
  ))

(deftest ^:slow t-permute-nest-timing
  (let [size 9]
    (println (format "timing (count (permute-nest-1 (thru 1 %d)))" size))
    (time (spyx (count (permute-nest-1 (thru 1 size)))))
    (println (format "timing (count (permute-nest-2 (thru 1 %d)))" size))
    (time (spyx (count (permute-nest-2 (thru 1 size))))))

    (comment
      ; on 8-core Intel i7-4790 CPU @ 3.60GHz (size -> 10)
      > lein test :only tst.tupelo.misc/t-permute-nest-timing
      timing (count (permute-nest-1 (thru 1 10)))
      (count (permute-nest-1 (thru 1 size))) => 3628800
      "Elapsed time: 40525.615739 msecs"
      timing (count (permute-nest-2 (thru 1 10)))
      (count (permute-nest-2 (thru 1 size))) => 3628800
      "Elapsed time: 17750.659604 msecs"
    ))




(deftest t-permute-nest-1

  (is (set (permute-nest-1 [:a]))        #{ [:a      ]            } )
  (is (set (permute-nest-1 [:a :b]))     #{ [:a :b   ] [:b :a   ] } )
  (is (set (permute-nest-1 [:a :b :c]))  #{ [:a :b :c] [:a :c :b]
                                            [:b :a :c] [:b :c :a]
                                            [:c :a :b] [:c :b :a] } )
)
(permute-nest-1 [:a :b :c :d])
