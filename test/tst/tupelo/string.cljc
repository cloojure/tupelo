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
    [tupelo.string :as ts]
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
        (ts/collapse-whitespace "  abc    def			g
                                     hij kl	 " ))))

(deftest t-equals-ignore-spacing
  (is (ts/equals-ignore-spacing "a" ))
  (is (ts/equals-ignore-spacing "a" "  a "))
  (is (ts/equals-ignore-spacing "a" "  a  " "   a" "a   "))

  (is (ts/equals-ignore-spacing "
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
  (is (= (ts/quote-double->single (str \")) (str \')))
  (is (= (ts/quote-single->double (str \')) (str \")))
  (let [s1 "I said, 'Yes, please.'"
        s2 "I said, \"Yes, please.\"" ]
    (is (= s1 (-> s2 ts/double-quotes->single-quotes)))
    (is (= s2 (-> s1 ts/single-quotes->double-quotes)))
    (is (= s2 (-> s2
                ts/double-quotes->single-quotes
                ts/single-quotes->double-quotes)))
    (is (= s1 (-> s1
                ts/single-quotes->double-quotes
                ts/double-quotes->single-quotes)))))

(dotest
  (is= :abc-def-gh-qrs (ts/str->kw-normalized "abc def*gh_qrs"))
  (is= "abc" (ts/kw->str :abc))

  (is= (ts/snake->kabob "some_multiple_word_str") "some-multiple-word-str")
  (is= (ts/kabob->snake "some-multiple-word-str") "some_multiple_word_str")

  (is= (ts/kw-snake->kabob :some_multiple_word_kw) :some-multiple-word-kw)
  (is= (ts/kw-kabob->snake :some-multiple-word-kw) :some_multiple_word_kw)
)

(deftest seq->str-t
  (is (= " 1 2 3"           (t/seq->str (byte-array [1 2 3]))))
  (is (= " :a :b 3 4"       (t/seq->str [:a :b 3 4])))
  (is (= " \\a \\b \\c"     (t/seq->str "abc"))))

(deftest t-increasing
  (isnt (ts/increasing "abc" "a"))
  (isnt (ts/increasing "abc" "ab"))
  (isnt (ts/increasing "abc" "abc"))
  (is   (ts/increasing "abc" "abd"))
  (is   (ts/increasing "abc" "abcd"))
  (is   (ts/increasing "abc" "ad"))
  (is   (ts/increasing "abc" "b"))

  (isnt (ts/increasing-or-equal "abc" "a"))
  (isnt (ts/increasing-or-equal "abc" "ab"))
  (is   (ts/increasing-or-equal "abc" "abc"))
  (is   (ts/increasing-or-equal "abc" "abd"))
  (is   (ts/increasing-or-equal "abc" "abcd"))
  (is   (ts/increasing-or-equal "abc" "ad"))
  (is   (ts/increasing-or-equal "abc" "b"))
)

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

(deftest t-index-of
  (is= 0 (ts/index-of "abc" "a"))
  (is= 0 (ts/index-of "abc" "ab"))
  (is= 0 (ts/index-of "abc" "abc"))
  (is= 1 (ts/index-of "abc" "b"))
  (is= 1 (ts/index-of "abc" "bc"))
  (is= 2 (ts/index-of "abc" "c"))
  (is= -1 (ts/index-of "abc" "d"))
)

(deftest t-starts-with?
  ; clojure.string
  (t/when-clojure-1-8-plus
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
    (is      (ts/starts-with? "abcde" "a"))
    (is      (ts/starts-with? "abcde" "ab"))
    (is      (ts/starts-with? "abcde" "abc"))

    (is (not (ts/starts-with? "abcde" "b")))
    (is (not (ts/starts-with? "abcde" "bc")))

    (is (not (ts/starts-with? "a" "ab")))
    (is (not (ts/starts-with? "ab" "abc"))))

  ;-----------------------------------------------------------------------------
  ; break out
  (is   (ts/alphanumeric? \a))
  (is   (ts/alphanumeric? [\a]))
  (is   (ts/alphanumeric? "a"))
  (is   (ts/alphanumeric? "abc"))
  (isnt (ts/alphanumeric? "*"))
  (isnt (ts/alphanumeric? "ab*de"))
  (isnt (ts/alphanumeric? \=))

  (is (ts/whitespace-horiz?                ts/chars-whitespace-horiz))
  (is (ts/whitespace-eol?                  ts/chars-whitespace-eol))
  (is (ts/whitespace?                      ts/chars-whitespace))
  (is (ts/lowercase?                       ts/chars-lowercase))
  (is (ts/uppercase?                       ts/chars-uppercase))
  (is (ts/digit?                           ts/chars-digit))
  (is (ts/alpha?                           ts/chars-alpha))
  (is (ts/visible?                         ts/chars-visible))
  (is (ts/text?                            ts/chars-text))

  (is (apply ts/whitespace-horiz?          (vec ts/chars-whitespace-horiz)))
  (is (apply ts/whitespace-eol?            (vec ts/chars-whitespace-eol)))
  (is (apply ts/whitespace?                (vec ts/chars-whitespace)))
  (is (apply ts/lowercase?                 (vec ts/chars-lowercase)))
  (is (apply ts/uppercase?                 (vec ts/chars-uppercase)))
  (is (apply ts/digit?                     (vec ts/chars-digit)))
  (is (apply ts/alpha?                     (vec ts/chars-alpha)))
  (is (apply ts/visible?                   (vec ts/chars-visible)))
  (is (apply ts/text?                      (vec ts/chars-text)))

)

