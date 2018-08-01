;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.string
  (:use tupelo.core tupelo.test )
  (:require
    #?@(:clj [
    [clojure.core :as cc]
    [clojure.string :as str]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [tupelo.core :as t]
    [tupelo.char :as char]
    [tupelo.string :as ts]
              ])
  ))

; #todo add generative testing?
; #todo add clojure.spec testing?

#?(:clj
   (do

(dotest
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


(dotest
  (is (= "abc def g hij kl"
        (ts/collapse-whitespace "  abc    def			g
                                     hij kl	 " ))))

(dotest
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

(dotest
  (is= (ts/quotes->single (str \")) (str \'))
  (is= (ts/quotes->double (str \')) (str \"))
  (let [s1 "I said, 'Yes, please.'"
        s2 "I said, \"Yes, please.\"" ]
    (is= s1 (-> s2 ts/quotes->single))
    (is= s2 (-> s1 ts/quotes->double))
    (is= s2 (-> s2 ts/quotes->single
                   ts/quotes->double))
    (is= s1 (-> s1 ts/quotes->double
                   ts/quotes->single))))

(dotest
  (is= :abc-def-gh-qrs (ts/str->kw-normalized "abc def*gh_qrs"))
  (is= "abc" (ts/kw->str :abc))

  (is= (ts/snake->kabob "some_multiple_word_str") "some-multiple-word-str")
  (is= (ts/kabob->snake "some-multiple-word-str") "some_multiple_word_str")

  (is= (ts/kw-snake->kabob :some_multiple_word_kw) :some-multiple-word-kw)
  (is= (ts/kw-kabob->snake :some-multiple-word-kw) :some_multiple_word_kw)
)

(dotest
  (is (= " 1 2 3"           (t/seq->str (byte-array [1 2 3]))))
  (is (= " :a :b 3 4"       (t/seq->str [:a :b 3 4])))
  (is (= " \\a \\b \\c"     (t/seq->str "abc"))))

(dotest
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

(dotest
  (is (= ""    (ts/take 0 "abc")))
  (is (= "a"   (ts/take 1 "abc")))
  (is (= "ab"  (ts/take 2 "abc")))
  (is (= "abc" (ts/take 3 "abc")))
  (is (= "abc" (ts/take 4 "abc"))))

(dotest
  (is (= "abc" (ts/drop 0 "abc")))
  (is (= "bc"  (ts/drop 1 "abc")))
  (is (= "c"   (ts/drop 2 "abc")))
  (is (= ""    (ts/drop 3 "abc")))
  (is (= ""    (ts/drop 4 "abc"))))

(dotest
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

(dotest
  ; clojure accepts either CR/LF or LF (CR=/return & LF=\newline) as line-separator
  (is (= "abc"    (ts/indent-lines 0      "abc"                    )))
  (is (= "abc"    (ts/indent-lines 0 (str "abc"         \newline  ))))
  (is (= "abc"    (ts/indent-lines 0 (str "abc" \return \newline  ))))

  ; counterexample: clojure doesn't accept \formfeed or solo \return as line-separator
  (isnt (= "abc"    (ts/indent-lines 0 (str "abc" \formfeed ))))
  (isnt (= "abc"    (ts/indent-lines 0 (str "abc" \return   ))))

  (is (= "  abc\n  def"    (ts/indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def"    (ts/indent-lines 2 (str "abc" \newline "def" \newline ))))

  (is (= "abc\ndef"         (ts/indent-lines 0 (str "abc" \newline "def" ))))
  (is (= " abc\n def"       (ts/indent-lines 1 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def"     (ts/indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "   abc\n   def"   (ts/indent-lines 3 (str "abc" \newline "def" ))))
)

(dotest
  (is= 0 (ts/index-of "abc" "a"))
  (is= 0 (ts/index-of "abc" "ab"))
  (is= 0 (ts/index-of "abc" "abc"))
  (is= 1 (ts/index-of "abc" "b"))
  (is= 1 (ts/index-of "abc" "bc"))
  (is= 2 (ts/index-of "abc" "c"))
  (is= -1 (ts/index-of "abc" "d"))
)

(dotest
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

  ;-----------------------------------------------------------------------------
  ; tupelo.string
  (is (ts/starts-with? "abcde" "a"))
  (is (ts/starts-with? "abcde" "ab"))
  (is (ts/starts-with? "abcde" "abc"))

  (is (not (ts/starts-with? "abcde" "b")))
  (is (not (ts/starts-with? "abcde" "bc")))

  (is (not (ts/starts-with? "a" "ab")))
  (is (not (ts/starts-with? "ab" "abc")))

  (is (ts/contains-match? "abcde" #"abc"))
  (is (ts/contains-match? "abcde" #"abc.*"))
  (is (ts/contains-match? "abcde" #".bc.*"))
  (is (ts/contains-match? "abcde" #"^ab"))
  (is (ts/contains-match? "abcde" #"bc"))
  (isnt (ts/contains-match? "abcde" #"^bc"))
  (isnt (ts/contains-match? "abcde" #".bc9.*"))

  ; regex special chars don't work in tgt-str
  (is (ts/contains-str? "abcde" "abc"))
  (is (ts/contains-str? "abcde" "bc"))
  (isnt (ts/contains-str? "abcde" "abc.*"))
  (isnt (ts/contains-str? "abcde" ".bc.*"))
  (isnt (ts/contains-str? "abcde" "^ab"))
  (isnt (ts/contains-str? "abcde" "^bc"))

  ; regex special chars OK in both search-str & tgt-str
  (is (ts/contains-str? "abc.*de" "abc.*"))
  (is (ts/contains-str? "a.bc.*de" ".bc.*"))
  (is (ts/contains-str? "^abcde" "^ab"))
  (is (ts/contains-str? "a^bcde" "^bc"))

)

(dotest
  ;-----------------------------------------------------------------------------
  ; break out
  (is   (ts/alphanumeric? \a))
  (is   (ts/alphanumeric? [\a]))
  (is   (ts/alphanumeric? "a"))
  (is   (ts/alphanumeric? "abc"))
  (isnt (ts/alphanumeric? "*"))
  (isnt (ts/alphanumeric? "ab*de"))
  (isnt (ts/alphanumeric? \=))

  (is (ts/whitespace?   "")) ; empty string counts as "whitespace"
  (is (ts/whitespace?   " "))
  (is (ts/whitespace?   " \n"))
  (is (ts/whitespace?   " \r"))
  (is (ts/whitespace? (str \tab "   "  "\n")))
  (isnt (ts/whitespace? (str \tab " xyz "  "\n")))

  (is (ts/whitespace-horiz?                char/whitespace-horiz))
  (is (ts/whitespace-eol?                  char/whitespace-eol))
  (is (ts/whitespace?                      char/whitespace))
  (is (ts/lowercase?                       char/lowercase))
  (is (ts/uppercase?                       char/uppercase))
  (is (ts/digit?                           char/digit))
  (is (ts/alpha?                           char/alpha))
  (is (ts/visible?                         char/visible))
  (is (ts/text?                            char/text))

  (is (apply ts/whitespace-horiz?          (vec char/whitespace-horiz)))
  (is (apply ts/whitespace-eol?            (vec char/whitespace-eol)))
  (is (apply ts/whitespace?                (vec char/whitespace)))
  (is (apply ts/lowercase?                 (vec char/lowercase)))
  (is (apply ts/uppercase?                 (vec char/uppercase)))
  (is (apply ts/digit?                     (vec char/digit)))
  (is (apply ts/alpha?                     (vec char/alpha)))
  (is (apply ts/visible?                   (vec char/visible)))
  (is (apply ts/text?                      (vec char/text)))

)

))
