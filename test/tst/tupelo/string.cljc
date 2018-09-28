;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.string
  (:use tupelo.string tupelo.test )
  (:require
    #?@(:clj [
    [clojure.core :as cc]
    [clojure.string :as str]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [tupelo.core  :as t]
    [tupelo.impl  :as i]
    [tupelo.char :as char]

  ])
))

; #todo add generative testing?
; #todo add clojure.spec testing?

#?(:clj
   (do

     (deftest-focus
       (is= ""          (tabs->spaces ""))
       (is= "x"         (tabs->spaces "x"))
       ;     01234567012345670123456701234567
       (is= "        x" (tabs->spaces (str/join [\tab \x])))
       (is= "0       x" (tabs->spaces (str/join [\0 \tab \x])))
       (is= "01      x" (tabs->spaces (str/join [\0 \1 \tab \x])))
       (is= "012     x" (tabs->spaces (str/join [\0 \1 \2 \tab \x])))
       (is= "0123    x" (tabs->spaces (str/join [\0 \1 \2 \3 \tab \x])))
       (is= "01234   x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \tab \x])))
       (is= "012345  x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \tab \x])))
       (is= "0123456 x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \tab \x])))
       ;     01234567012345670123456701234567
       (is= "01234567        x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7   \tab \x])))
       (is= "012345670       x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0  \tab \x])))
       (is= "0123456701      x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \tab \x])))
       (is= "01234567012     x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \tab \x])))
       (is= "012345670123    x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \tab \x])))
       (is= "0123456701234   x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \tab \x])))
       (is= "01234567012345  x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \5 \tab \x])))
       (is= "012345670123456 x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \5 \6 \tab \x])))
       ;     01234567012345670123456701234567
       (is= "0123456701234567        x" (tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \5 \6 \7 \tab \x])))
       ;     01234567012345670123456701234567


       (is= ""          (tabs->spaces 4 ""))
       (is= "x"         (tabs->spaces 4 "x"))
       ;     0123012301230123
       (is= "    x" (tabs->spaces 4 (str/join [\tab \x])))
       (is= "0   x" (tabs->spaces 4 (str/join [\0 \tab \x])))
       (is= "01  x" (tabs->spaces 4 (str/join [\0 \1 \tab \x])))
       (is= "012 x" (tabs->spaces 4 (str/join [\0 \1 \2 \tab \x])))
       ;     0123012301230123
       (is= "0123    x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \tab \x])))
       (is= "01234   x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \tab \x])))
       (is= "012345  x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \tab \x])))
       (is= "0123456 x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \tab \x])))
       ;     0123012301230123
       (is= "01234567    x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \tab \x])))
       (is= "012345678   x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \8 \tab \x])))
       (is= "0123456789  x" (tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \tab \x])))

       ;     0123012301230123
       (is= "01  a   b" (tabs->spaces 4 (str/join [\0 \1 \tab \a \tab \b]))))

(deftest
  ; clojure.core/str works correctly for various string combinations
  (is= ""     (str "" ))
  (is= "a"    (str "" "a"))
  (is= "a"    (str "" "a" ""))
  (is= "ab"   (str "a" "b"))
  (is= "ab"   (str "" "a" "b" ""))
  (is= "abc"  (str "a" "" "bc"))
  (is= "abc"  (str "a" "bc" ""))
  (is= "abc"  (str "a" "bc"))
  (is= "abc"  (str "ab" "c"))
  (is= "abc"  (str "a" "b" "c"))

  ; clojure.core/str works correctly for mixed strings and chars
  (is= "abc"  (str \a  "b" "c"))
  (is= "abc"  (str "a" \b  "c"))
  (is= "abc"  (str "a" "b" \c ))
  (is= "abc"  (str \a  \b  "c"))
  (is= "abc"  (str \a  "b" \c ))
  (is= "abc"  (str "a" \b  \c ))
  (is= "abc"  (str \a  \b  \c ))

  ; clojure.core/str failure cases
  (isnt= "abc" (cc/drop 1 (str "xabc")))
  (isnt= "abc" (cc/take 3 (str "abcxxx")))

  ; Cannot fix it by using (str ...) on output of take/drop, since result is a LazySeq
  (isnt= "abc" (str (cc/drop 1 (str "xabc"))))
  (isnt= "abc" (str (cc/take 3 (str "abcxxx"))))

  (is (i/truthy? (re-find #"clojure.lang.LazySeq@.*" (str (cc/drop 1 (str "xabc"))))))
  (is (i/truthy? (re-find #"clojure.lang.LazySeq@.*" (str (cc/take 3 (str "abcxxx"))))))

  ; Can fix it using str/join
  (is= "abc" (str/join (cc/drop 1 (str "xabc"))))
  (is= "abc" (str/join (cc/take 3 (str "abcxxx"))))

  ; Demo that str/join takes a single collection arg
  (is (= "" (str/join [ "" ])))
  (is (= "a" (str/join [ "" "a"])))
  (is (= "abc" (str/join [ "" "a" "bc"])))

  ; A sequence is not a string, but tupelo.core/strcat can turn a sequence into a string.
  ; Also works to flatten out all nested collections.
  (is (not (= "abc"           (seq "abc"))))
  (is      (= "abc" (t/strcat (seq "abc"))))
  (is      (= "abcde" (t/strcat ["" \a \b \c "de"] )))
  (is      (= "abcde" (t/strcat ["" \a \b [\c ["d" \e]]] ))) )

(deftest
  (is (= "abc def g hij kl"
        (collapse-whitespace "  abc    def			g
                                     hij kl	 " ))))
(deftest
  (is (equals-ignore-spacing? "a" ))
  (is (equals-ignore-spacing? "a" "  a "))
  (is (equals-ignore-spacing? "a" "  a  " "   a" "a   "))

  (is (equals-ignore-spacing? "
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

(deftest
  (is= (quotes->single (str \")) (str \'))
  (is= (quotes->double (str \')) (str \"))
  (let [s1 "I said, 'Yes, please.'"
        s2 "I said, \"Yes, please.\"" ]
    (is= s1 (-> s2 quotes->single))
    (is= s2 (-> s1 quotes->double))
    (is= s2 (-> s2 quotes->single
                   quotes->double))
    (is= s1 (-> s1 quotes->double
                   quotes->single))))

(deftest
  (is= :abc-def-gh-qrs (str->kw-normalized "abc def*gh_qrs"))
  (is= "abc" (i/kw->str :abc))

  (is= (snake->kabob "some_multiple_word_str") "some-multiple-word-str")
  (is= (kabob->snake "some-multiple-word-str") "some_multiple_word_str")

  (is= (kw-snake->kabob :some_multiple_word_kw) :some-multiple-word-kw)
  (is= (kw-kabob->snake :some-multiple-word-kw) :some_multiple_word_kw)
)

(deftest
  (is (= " 1 2 3"           (t/seq->str (byte-array [1 2 3]))))
  (is (= " :a :b 3 4"       (t/seq->str [:a :b 3 4])))
  (is (= " \\a \\b \\c"     (t/seq->str "abc"))))

(deftest
  (isnt (increasing? "abc" "a"))
  (isnt (increasing? "abc" "ab"))
  (isnt (increasing? "abc" "abc"))
  (is   (increasing? "abc" "abd"))
  (is   (increasing? "abc" "abcd"))
  (is   (increasing? "abc" "ad"))
  (is   (increasing? "abc" "b"))

  (isnt (increasing-or-equal? "abc" "a"))
  (isnt (increasing-or-equal? "abc" "ab"))
  (is   (increasing-or-equal? "abc" "abc"))
  (is   (increasing-or-equal? "abc" "abd"))
  (is   (increasing-or-equal? "abc" "abcd"))
  (is   (increasing-or-equal? "abc" "ad"))
  (is   (increasing-or-equal? "abc" "b"))
)

(deftest
  (is (= ""    (take 0 "abc")))
  (is (= "a"   (take 1 "abc")))
  (is (= "ab"  (take 2 "abc")))
  (is (= "abc" (take 3 "abc")))
  (is (= "abc" (take 4 "abc"))))

(deftest
  (is (= "abc" (drop 0 "abc")))
  (is (= "bc"  (drop 1 "abc")))
  (is (= "c"   (drop 2 "abc")))
  (is (= ""    (drop 3 "abc")))
  (is (= ""    (drop 4 "abc"))))

(deftest
  (is (= "abc"    (indent 0 "abc")))
  (is (= " abc"   (indent 1 "abc")))
  (is (= "  abc"  (indent 2 "abc")))
  (is (= "   abc" (indent 3 "abc")))

  (is (= "ab"    (indent 0 "ab")))
  (is (= " ab"   (indent 1 "ab")))
  (is (= "  ab"  (indent 2 "ab")))
  (is (= "   ab" (indent 3 "ab")))

  (is (= "a"    (indent 0 "a")))
  (is (= " a"   (indent 1 "a")))
  (is (= "  a"  (indent 2 "a")))
  (is (= "   a" (indent 3 "a")))

  (is (= ""    (indent 0 "")))
  (is (= " "   (indent 1 "")))
  (is (= "  "  (indent 2 "")))
  (is (= "   " (indent 3 ""))))

(deftest
  ; clojure accepts either CR/LF or LF (CR=/return & LF=\newline) as line-separator
  (is (= "abc"    (indent-lines 0      "abc"                    )))
  (is (= "abc"    (indent-lines 0 (str "abc"         \newline  ))))
  (is (= "abc"    (indent-lines 0 (str "abc" \return \newline  ))))

  ; counterexample: clojure doesn't accept \formfeed or solo \return as line-separator
  (isnt (= "abc"    (indent-lines 0 (str "abc" \formfeed ))))
  (isnt (= "abc"    (indent-lines 0 (str "abc" \return   ))))

  (is (= "  abc\n  def"    (indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def"    (indent-lines 2 (str "abc" \newline "def" \newline ))))

  (is (= "abc\ndef"         (indent-lines 0 (str "abc" \newline "def" ))))
  (is (= " abc\n def"       (indent-lines 1 (str "abc" \newline "def" ))))
  (is (= "  abc\n  def"     (indent-lines 2 (str "abc" \newline "def" ))))
  (is (= "   abc\n   def"   (indent-lines 3 (str "abc" \newline "def" ))))
)

(deftest
  (is= 0 (str/index-of "abc" "a"))
  (is= 0 (str/index-of "abc" "ab"))
  (is= 0 (str/index-of "abc" "abc"))
  (is= 1 (str/index-of "abc" "b"))
  (is= 1 (str/index-of "abc" "bc"))
  (is= 2 (str/index-of "abc" "c"))
  (is= nil (str/index-of "abc" "d"))
)

(deftest
  ; clojure.string
  (t/when-clojure-1-8-plus
    (is (str/starts-with? "abcde" "a"))
    (is (str/starts-with? "abcde" "ab"))
    (is (str/starts-with? "abcde" "abc"))

    (is (not (str/starts-with? "abcde" "b")))
    (is (not (str/starts-with? "abcde" "bc")))

    (is (not (str/starts-with? "a" "ab")))
    (is (not (str/starts-with? "ab" "abc"))))

  ;-----------------------------------------------------------------------------
  ; tupelo.string
  (is (contains-match? "abcde" #"abc"))
  (is (contains-match? "abcde" #"abc.*"))
  (is (contains-match? "abcde" #".bc.*"))
  (is (contains-match? "abcde" #"^ab"))
  (is (contains-match? "abcde" #"bc"))
  (isnt (contains-match? "abcde" #"^bc"))
  (isnt (contains-match? "abcde" #".bc9.*"))

  ; regex special chars don't work in tgt-str
  (is (contains-str? "abcde" "abc"))
  (is (contains-str? "abcde" "bc"))
  (isnt (contains-str? "abcde" "abc.*"))
  (isnt (contains-str? "abcde" ".bc.*"))
  (isnt (contains-str? "abcde" "^ab"))
  (isnt (contains-str? "abcde" "^bc"))

  ; regex special chars OK in both search-str & tgt-str
  (is (contains-str? "abc.*de" "abc.*"))
  (is (contains-str? "a.bc.*de" ".bc.*"))
  (is (contains-str? "^abcde" "^ab"))
  (is (contains-str? "a^bcde" "^bc"))

  (let [search-str "Hello there, you.
                        How are you
                           doing today?"]
    (is (equals-ignore-spacing? (grep #"hello" search-str) ""))
    (is (equals-ignore-spacing? (grep #"Hello" search-str) "Hello there, you."))
    (is (equals-ignore-spacing? (grep #"(?i)hello" search-str) "Hello there, you."))
    (is (equals-ignore-spacing? (grep #"you" search-str)
          "Hello there, you.
           How are you"))
    (is (equals-ignore-spacing? (grep #"today." search-str) "doing today?"))
    (is (equals-ignore-spacing? (fgrep "today." search-str) ""))))

(deftest
  (is   (alphanumeric? \a))
  (is   (alphanumeric? [\a]))
  (is   (alphanumeric? "a"))
  (is   (alphanumeric? "abc"))
  (isnt (alphanumeric? "*"))
  (isnt (alphanumeric? "ab*de"))
  (isnt (alphanumeric? \=))

  (is (whitespace?   "")) ; empty string counts as "whitespace"
  (is (whitespace?   " "))
  (is (whitespace?   " \n"))
  (is (whitespace?   " \r"))
  (is (whitespace? (str \tab "   "  "\n")))
  (isnt (whitespace? (str \tab " xyz "  "\n")))

  (is (whitespace-horiz?                char/whitespace-horiz))
  (is (whitespace-eol?                  char/whitespace-eol))
  (is (whitespace?                      char/whitespace))
  (is (lowercase?                       char/lowercase))
  (is (uppercase?                       char/uppercase))
  (is (digit?                           char/digit))
  (is (alpha?                           char/alpha))
  (is (visible?                         char/visible))
  (is (text?                            char/text))

  (is (apply whitespace-horiz?          (vec char/whitespace-horiz)))
  (is (apply whitespace-eol?            (vec char/whitespace-eol)))
  (is (apply whitespace?                (vec char/whitespace)))
  (is (apply lowercase?                 (vec char/lowercase)))
  (is (apply uppercase?                 (vec char/uppercase)))
  (is (apply digit?                     (vec char/digit)))
  (is (apply alpha?                     (vec char/alpha)))
  (is (apply visible?                   (vec char/visible)))
  (is (apply text?                      (vec char/text))) )



))
