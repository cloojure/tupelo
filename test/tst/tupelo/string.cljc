;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.string
  (:use clojure.test )
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

; #todo add generative testing?
; #todo add clojure.spec testing?

(deftest misc
  ; clojure.core/str works correctly for various string combinations
  (is (= ""     (str "" )))
  (is (= "a"    (str "" "a")))
  (is (= "a"    (str "" "a" "")))
  (is (= "ab"   (str "a" "b")))
  (is (= "ab"   (str "" "a" "b" "")))
  (is (= "abc"  (str "a" "" "bc")))
  (is (= "abc"  (str "a" "bc" "")))
  (is (= "abc"  (str "a" "bc")))
  (is (= "abc"  (str "ab" "c")))
  (is (= "abc"  (str "a" "b" "c")))

  ; clojure.core/str works correctly for mixed strings and chars
  (is (= "abc"  (str \a  "b" "c")))
  (is (= "abc"  (str "a" \b  "c")))
  (is (= "abc"  (str "a" "b" \c )))
  (is (= "abc"  (str \a  \b  "c")))
  (is (= "abc"  (str \a  "b" \c )))
  (is (= "abc"  (str "a" \b  \c )))
  (is (= "abc"  (str \a  \b  \c )))

  ; clojure.core/str failure cases
  (is (not (= "abc" (cc/drop 1 (str "xabc")))))
  (is (not (= "abc" (cc/take 3 (str "abcxxx")))))

  ; Cannot fix it by using (str ...) on output of take/drop, since result is a LazySeq
  (is (not (= "abc" (str (cc/drop 1 (str "xabc"))))))
  (is (not (= "abc" (str (cc/take 3 (str "abcxxx"))))))
  (is (truthy? (re-find #"clojure.lang.LazySeq@.*" (str (cc/drop 1 (str "xabc"))))))
  (is (truthy? (re-find #"clojure.lang.LazySeq@.*" (str (cc/take 3 (str "abcxxx"))))))

  ; Can fix it using str/join
  (is (= "abc" (str/join (cc/drop 1 (str "xabc")))))
  (is (= "abc" (str/join (cc/take 3 (str "abcxxx")))))

  ; Demo that str/join takes a single collection arg
  (is (= "" (str/join [ "" ])))
  (is (= "a" (str/join [ "" "a"])))
  (is (= "abc" (str/join [ "" "a" "bc"])))

  ; A sequence is not a string, but tupelo.core/strcat can turn a sequence into a string.
  ; Also works to flatten out all nested collections.
  (is (not (= "abc"           (seq "abc"))))
  (is      (= "abc" (t/strcat (seq "abc"))))
  (is      (= "abcde" (t/strcat ["" \a \b \c "de"] )))
  (is      (= "abcde" (t/strcat ["" \a \b [\c ["d" \e]]] )))
)

; verify that (str/starts-with? ...) does what we think
(deftest t-starts-with?
  (is (str/starts-with? "abcde" "a"))
  (is (str/starts-with? "abcde" "ab"))
  (is (str/starts-with? "abcde" "abc"))

  (is (not (str/starts-with? "abcde" "b")))
  (is (not (str/starts-with? "abcde" "bc")))

  (is (not (str/starts-with? "a" "ab")))
  (is (not (str/starts-with? "ab" "abc"))))

(deftest t-tupstr-take
  (is (= ""    (ts/take 0 "abc")))
  (is (= "a"   (ts/take 1 "abc")))
  (is (= "ab"  (ts/take 2 "abc")))
  (is (= "abc" (ts/take 3 "abc")))
  (is (= "abc" (ts/take 4 "abc"))))

(deftest t-tupstr-drop
  (is (= "abc" (ts/drop 0 "abc")))
  (is (= "bc"  (ts/drop 1 "abc")))
  (is (= "c"   (ts/drop 2 "abc")))
  (is (= ""    (ts/drop 3 "abc")))
  (is (= ""    (ts/drop 4 "abc"))))

(deftest t-indent
  (is (= "abc"    (ts/indent 0 "abc")))
  (is (= " abc"   (ts/indent 1 "abc")))
  (is (= "  abc"  (ts/indent 2 "abc")))
  (is (= "   abc" (ts/indent 3 "abc")))

  (is (= "ab"    (ts/indent 0 "ab")))
  (is (= " ab"   (ts/indent 1 "ab")))
  (is (= "  ab"  (ts/indent 2 "ab")))
  (is (= "   ab" (ts/indent 3 "ab")))

  (is (= "a"    (ts/indent 0 "a")))
  (is (= " a"   (ts/indent 1 "a")))
  (is (= "  a"  (ts/indent 2 "a")))
  (is (= "   a" (ts/indent 3 "a")))

  (is (= ""    (ts/indent 0 "")))
  (is (= " "   (ts/indent 1 "")))
  (is (= "  "  (ts/indent 2 "")))
  (is (= "   " (ts/indent 3 ""))))

(deftest t-indent
  ; clojure accepts either CR/LF or LF (CR=/return & LF=\newline) as line-separator
  (is (= "abc\n"    (ts/indent-lines 0      "abc"                    )))
  (is (= "abc\n"    (ts/indent-lines 0 (str "abc"         \newline  ))))
  (is (= "abc\n"    (ts/indent-lines 0 (str "abc" \return \newline  ))))

  ; counterexample: clojure doesn't accept \formfeed or solo \return as line-separator
  (isnt (= "abc\n"    (ts/indent-lines 0 (str "abc" \formfeed ))))
  (isnt (= "abc\n"    (ts/indent-lines 0 (str "abc" \return   ))))

  (is (= "  abc\n  def\n"    (ts/indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def\n"    (ts/indent-lines 2 (str "abc" \newline "def" \newline ))))

  (is (= "abc\ndef\n"         (ts/indent-lines 0 (str "abc" \newline "def" ))))
  (is (= " abc\n def\n"       (ts/indent-lines 1 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def\n"     (ts/indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "   abc\n   def\n"   (ts/indent-lines 3 (str "abc" \newline "def" ))))
)

