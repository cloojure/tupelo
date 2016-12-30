;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.string
  (:use tupelo.test clojure.test )
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [tupelo.core :as t]
    [tupelo.string :as tstr]
  ))
(t/refer-tupelo)

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


(deftest collapse-whitespace-t
  (is (= "abc def g hij kl"
        (tstr/collapse-whitespace "  abc    def			g
                                     hij kl	 " ))))

(deftest t-equals-ignore-spacing
  (is (tstr/equals-ignore-spacing "a" ))
  (is (tstr/equals-ignore-spacing "a" "  a "))
  (is (tstr/equals-ignore-spacing "a" "  a  " "   a" "a   "))

  (is (tstr/equals-ignore-spacing "
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
  (is (= (tstr/double-quotes->single-quotes (str \")) (str \')))
  (is (= (tstr/single-quotes->double-quotes (str \')) (str \")))
  (let [s1 "I said, 'Yes, please.'"
        s2 "I said, \"Yes, please.\"" ]
    (is (= s1 (-> s2 tstr/double-quotes->single-quotes)))
    (is (= s2 (-> s1 tstr/single-quotes->double-quotes)))
    (is (= s2 (-> s2
                tstr/double-quotes->single-quotes
                tstr/single-quotes->double-quotes)))
    (is (= s1 (-> s1
                tstr/single-quotes->double-quotes
                tstr/double-quotes->single-quotes)))))

(deftest str->kw-t
  (testing "basic usage"
    (is (= :abc-def-gh-qrs (tstr/str->kw "abc def*gh_qrs")))))

(deftest seq->str-t
  (is (= " 1 2 3"           (t/seq->str (byte-array [1 2 3]))))
  (is (= " :a :b 3 4"       (t/seq->str [:a :b 3 4])))
  (is (= " \\a \\b \\c"     (t/seq->str "abc"))))















(deftest t-increasing
  (isnt (tstr/increasing "abc" "a"))
  (isnt (tstr/increasing "abc" "ab"))
  (isnt (tstr/increasing "abc" "abc"))
  (is   (tstr/increasing "abc" "abd"))
  (is   (tstr/increasing "abc" "abcd"))
  (is   (tstr/increasing "abc" "ad"))
  (is   (tstr/increasing "abc" "b"))

  (isnt (tstr/increasing-or-equal "abc" "a"))
  (isnt (tstr/increasing-or-equal "abc" "ab"))
  (is   (tstr/increasing-or-equal "abc" "abc"))
  (is   (tstr/increasing-or-equal "abc" "abd"))
  (is   (tstr/increasing-or-equal "abc" "abcd"))
  (is   (tstr/increasing-or-equal "abc" "ad"))
  (is   (tstr/increasing-or-equal "abc" "b"))
)


(deftest t-tupstr-take
  (is (= ""    (tstr/take 0 "abc")))
  (is (= "a"   (tstr/take 1 "abc")))
  (is (= "ab"  (tstr/take 2 "abc")))
  (is (= "abc" (tstr/take 3 "abc")))
  (is (= "abc" (tstr/take 4 "abc"))))

(deftest t-tupstr-drop
  (is (= "abc" (tstr/drop 0 "abc")))
  (is (= "bc"  (tstr/drop 1 "abc")))
  (is (= "c"   (tstr/drop 2 "abc")))
  (is (= ""    (tstr/drop 3 "abc")))
  (is (= ""    (tstr/drop 4 "abc"))))

(deftest t-indent
  (is (= "abc"    (tstr/indent 0 "abc")))
  (is (= " abc"   (tstr/indent 1 "abc")))
  (is (= "  abc"  (tstr/indent 2 "abc")))
  (is (= "   abc" (tstr/indent 3 "abc")))

  (is (= "ab"    (tstr/indent 0 "ab")))
  (is (= " ab"   (tstr/indent 1 "ab")))
  (is (= "  ab"  (tstr/indent 2 "ab")))
  (is (= "   ab" (tstr/indent 3 "ab")))

  (is (= "a"    (tstr/indent 0 "a")))
  (is (= " a"   (tstr/indent 1 "a")))
  (is (= "  a"  (tstr/indent 2 "a")))
  (is (= "   a" (tstr/indent 3 "a")))

  (is (= ""    (tstr/indent 0 "")))
  (is (= " "   (tstr/indent 1 "")))
  (is (= "  "  (tstr/indent 2 "")))
  (is (= "   " (tstr/indent 3 ""))))

(deftest t-indent
  ; clojure accepts either CR/LF or LF (CR=/return & LF=\newline) as line-separator
  (is (= "abc\n"    (tstr/indent-lines 0      "abc"                    )))
  (is (= "abc\n"    (tstr/indent-lines 0 (str "abc"         \newline  ))))
  (is (= "abc\n"    (tstr/indent-lines 0 (str "abc" \return \newline  ))))

  ; counterexample: clojure doesn't accept \formfeed or solo \return as line-separator
  (isnt (= "abc\n"    (tstr/indent-lines 0 (str "abc" \formfeed ))))
  (isnt (= "abc\n"    (tstr/indent-lines 0 (str "abc" \return   ))))

  (is (= "  abc\n  def\n"    (tstr/indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def\n"    (tstr/indent-lines 2 (str "abc" \newline "def" \newline ))))

  (is (= "abc\ndef\n"         (tstr/indent-lines 0 (str "abc" \newline "def" ))))
  (is (= " abc\n def\n"       (tstr/indent-lines 1 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def\n"     (tstr/indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "   abc\n   def\n"   (tstr/indent-lines 3 (str "abc" \newline "def" ))))
)

(deftest t-index-of
  (is= 0 (tstr/index-of "abc" "a"))
  (is= 0 (tstr/index-of "abc" "ab"))
  (is= 0 (tstr/index-of "abc" "abc"))
  (is= 1 (tstr/index-of "abc" "b"))
  (is= 1 (tstr/index-of "abc" "bc"))
  (is= 2 (tstr/index-of "abc" "c"))
  (is= -1 (tstr/index-of "abc" "d"))
)

(deftest t-starts-with?
  ; clojure.string
  (t/min-clojure-1-8
    (is      (str/starts-with? "abcde" "a"))
    (is      (str/starts-with? "abcde" "ab"))
    (is      (str/starts-with? "abcde" "abc"))

    (is (not (str/starts-with? "abcde" "b")))
    (is (not (str/starts-with? "abcde" "bc")))

    (is (not (str/starts-with? "a" "ab")))
    (is (not (str/starts-with? "ab" "abc")))
  )

  ; tupelo.string
  (do
    (is      (tstr/starts-with? "abcde" "a"))
    (is      (tstr/starts-with? "abcde" "ab"))
    (is      (tstr/starts-with? "abcde" "abc"))

    (is (not (tstr/starts-with? "abcde" "b")))
    (is (not (tstr/starts-with? "abcde" "bc")))

    (is (not (tstr/starts-with? "a" "ab")))
    (is (not (tstr/starts-with? "ab" "abc"))))
)
