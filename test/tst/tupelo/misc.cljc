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
  (is (= "         0 .........\n         9 total\n"
         (with-out-str (with-dots (doseq [x (range 9)]
                                    (dot))))))
  (dots-config! {:dots-per-row 10  :decimation 3} )
  (is (= "         0 ..........\n        30 ..........\n        60 ..........\n        90 ...\n        99 total\n"
         (with-out-str (with-dots (doseq [x (range 99)]
                                    (dot)))))))
(deftest t-permute
  (is (=  (permute [        ])  []))
  (is (=  (permute [:a      ])  [[:a]] ))
  (is (=  (permute [:a :b   ])  [[:a :b] [:b :a]] ))
  (is (=  (permute [:a :b :c])  [[:a :b :c] [:a :c :b]
                                 [:b :a :c] [:b :c :a]
                                 [:c :a :b] [:c :b :a]] )))
