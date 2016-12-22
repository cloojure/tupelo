;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.misc
  (:use tupelo.misc
        clojure.test )
  (:require [clojure.string   :as str]
            [schema.core      :as s]
            [tupelo.core      :as t]
            [tupelo.misc      :as misc]
            [clojure.math.combinatorics  :as combo]
  ))

(t/refer-tupelo)
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
  (is (= "abc def g hij kl"
          (misc/collapse-whitespace "  abc    def			g
                                     hij kl	 " ))))

(deftest t-equals-ignore-spacing
  (is (equals-ignore-spacing "a" ))
  (is (equals-ignore-spacing "a" "  a "))
  (is (equals-ignore-spacing "a" "  a  " "   a" "a   "))

  (is (equals-ignore-spacing "    
Whenever you find yourself on the side of the majority, it is time to pause and reflect.
Don't go around saying the world owes you a living. The world owes you nothing. It was here first.
I have never let my schooling interfere with my education.
If you tell the truth, you don't have to remember anything.
Mark Twain          "
      
"		Whenever you find yourself on the side of the majority, it is time to pause and reflect.
      Don't go around saying the world owes you a living. The world owes you nothing. It was here first.
      I have never let my schooling interfere with my education.
      If you tell the truth, you don't have to remember anything.
      Mark Twain			"

"      Whenever you find yourself on the side of the
       majority, it is time to pause and reflect.      Don't go
       around saying the world owes you a living. 	The world
       		owes you nothing. 		It was here first.  I have never
       let my schooling interfere with my education.  If you
       tell the truth, you don't have to remember
       anything.
       Mark Twain      		" )))

(deftest t-double-quotes<->single-quotes
  (is (= (double-quotes->single-quotes (str \")) (str \')))
  (is (= (single-quotes->double-quotes (str \')) (str \")))
  (let [s1 "I said, 'Yes, please.'"
        s2 "I said, \"Yes, please.\"" ]
      (is (= s1 (-> s2 double-quotes->single-quotes)))
      (is (= s2 (-> s1 single-quotes->double-quotes)))
      (is (= s2 (-> s2
                  double-quotes->single-quotes
                  single-quotes->double-quotes)))
      (is (= s1 (-> s1
                  single-quotes->double-quotes
                  double-quotes->single-quotes)))))

(deftest t-get-os
  (is (#{:windows :linux :mac} (misc/get-os))))

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
  (is (=       (factorial 0)          1))
  (is (=       (factorial 1)          1))
  (is (=       (factorial 2)          2))
  (is (=       (factorial 3)          6))
  (is (=       (factorial 4)         24))
  (is (=       (factorial 5)        120))
  (is (=       (factorial 6)        720))
  (is (=       (factorial 7)       5040))
  (is (=       (factorial 8)      40320))
  (is (=       (factorial 9)     362880))
  (is (=       (factorial 10)   3628800))
  (is (t/rel=  (factorial 15) 1.307674368e+12 :digits 10))
  (is (thrown? Exception (factorial 1.5)))
  (is (thrown? Exception (factorial -1)))
  (is (thrown? Exception (factorial -1))))

