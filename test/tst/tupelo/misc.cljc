;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.misc
  (:use tupelo.misc tupelo.test clojure.test )
  (:require [clojure.string   :as str]
            [tupelo.core      :as t ]
  ))
(t/refer-tupelo)

(set! *warn-on-reflection* true)

(deftest t-sample
  (let [data [1 2 3]]
    (is (= (drop 0 data) [1 2 3]))
    (is (= (drop 1 data) [  2 3]))
    (is (= (drop 2 data) [    3]))
    (is (= (drop 3 data) [     ])))
)

(deftest t-get-os
  (is (#{:windows :linux :mac} (get-os))))

(deftest shell-cmd-t
  (when (= :linux (get-os))

    (testing "no errors"
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
      )

    (testing "errors"
      (is (thrown? RuntimeException (shell-cmd "LLLls -ldF *"))))))

(deftest t-dots
  (dots-config! {:dots-per-row 10  :decimation 1} )
  (is (= (collapse-whitespace "         0 .........\n         9 total\n")
         (collapse-whitespace (with-out-str
                                (with-dots
                                  (doseq [x (range 9)]
                                    (dot)))))))

  (dots-config! {:dots-per-row 10  :decimation 3} )
  (is (= (collapse-whitespace "  0 ..........
                                30 ..........
                                60 ..........
                                90 ...
                                99 total  ")
        (collapse-whitespace (with-out-str
                               (with-dots
                                 (doseq [x (range 99)]
                                   (dot))))))))
(deftest t-factorial
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
  (is (t/rel=  (factorial 15) 1.307674368e+12 :digits 10))
  (throws? Exception (factorial 1.5))
  (throws? Exception (factorial -1))
  (throws? Exception (factorial -1)))

