;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.misc-test
  (:require [clojure.string     :as str]
            [cooljure.misc      :as misc])
  (:use clojure.test))

(deftest kw->dbstr-t
  (testing "basic usage"
    (is (= :abc-def-gh (misc/str->kw "abc_def_gh"))) ))

(deftest str->kw-t
  (testing "basic usage"
    (is (= :abc-def-gh-qrs (misc/str->kw "abc def*gh_qrs"))) ))

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

