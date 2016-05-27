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
            [tupelo.misc      :as misc]
            [clojure.math.combinatorics  :as combo]
            [criterium.core :as crit]
  ))

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
                                     (factorial n))))
        check-same-perm     (fn [n]
                              (println "checking n=" n)
                              (is (= (into #{} (combo/permutations (thru 1 n)))
                                     (into #{} (permute-multiset-1 (thru 1 n))))))
    ]

    (check-same-perm  1)
    (check-same-perm  2)
    (check-same-perm  3)
    (check-same-perm  4)
    (check-same-perm  5)
    (check-same-perm  6)
    (check-same-perm  7)
    (check-same-perm  8)
  ))

(deftest ^:slow t-permute-nest-timing
  (let [size 8]
    (newline)
    (println "size=" size)

    (newline)
    (println "permute-multiset-1")
    (crit/quick-bench (into [] (permute-multiset-1 (thru 1 size))))

    (newline)
    (println "permute-multiset-2")
    (crit/quick-bench (into [] (permute-multiset-2 (thru 1 size))))

    (newline)
    (println "permute-multiset-3")
    (crit/quick-bench (into [] (permute-multiset-3 (thru 1 size))))

    (newline)
    (println "permute-set-1")
    (crit/quick-bench (into [] (permute-set-1 (set (thru 1 size)))))

    (newline)
    (println "permute-tail-1")
    (crit/quick-bench (into [] (permute-tail-1 (thru 1 size))))

    (newline)
    (println "permute-tail-2")
    (crit/quick-bench (into [] (permute-tail-2 (thru 1 size))))

    (newline)
    (println "permute-tail-3")
    (crit/quick-bench (into [] (permute-tail-3 (thru 1 size))))

    (newline)
    (println "permute-lazy-1")
    (crit/quick-bench (into [] (permute-lazy-1 (thru 1 size))))

    (newline)
    (println "permute-nest-1")
    (crit/quick-bench (into [] (permute-nest-1 (thru 1 size))))

    (newline)
    (println "permute-nest-2")
    (crit/quick-bench (into [] (permute-nest-2 (thru 1 size))))

    (newline)
    (println "combo/permutations")
    (crit/quick-bench (into [] (combo/permutations (thru 1 size))))
))


(deftest t-permute-nest-1
  (is (set (permute-nest-1 [:a]))        #{ [:a      ]            } )
  (is (set (permute-nest-1 [:a :b]))     #{ [:a :b   ] [:b :a   ] } )
  (is (set (permute-nest-1 [:a :b :c]))  #{ [:a :b :c] [:a :c :b]
                                            [:b :a :c] [:b :c :a]
                                            [:c :a :b] [:c :b :a] } )
)
(permute-nest-1 [:a :b :c :d])
