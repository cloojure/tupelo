;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.string
  (:refer-clojure :exclude [take drop])
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros [tupelo.test]))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.core :as cc]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty forv]]
    [tupelo.chars :as char]
    [tupelo.string :as str]
    [tupelo.test :refer [testing is verify verify-focus
                         is isnt is= isnt= is-set= is-nonblank= is-nonblank-lines=
                         throws? throws-not?
                         ]]
    ))

; #todo add generative testing?
; #todo add clojure.spec testing?

(verify
  (testing "single string"
    (is (= "" (str/clip 0 "abcdefg")))
    (is (= "a" (str/clip 1 "abcdefg")))
    (is (= "ab" (str/clip 2 "abcdefg")))
    (is (= "abc" (str/clip 3 "abcdefg")))
    (is (= "abcd" (str/clip 4 "abcdefg")))
    (is (= "abcde" (str/clip 5 "abcdefg"))))
  (testing "two strings"
    (is (= "" (str/clip 0 "abc defg")))
    (is (= "a" (str/clip 1 "abc defg")))
    (is (= "ab" (str/clip 2 "abc defg")))
    (is (= "abc" (str/clip 3 "abc defg")))
    (is (= "abc " (str/clip 4 "abc defg")))
    (is (= "abc d" (str/clip 5 "abc defg"))))
  (testing "two strings & char"
    (is (= "" (str/clip 0 "ab" \c "defg")))
    (is (= "a" (str/clip 1 "ab" \c "defg")))
    (is (= "ab" (str/clip 2 "ab" \c "defg")))
    (is (= "abc" (str/clip 3 "ab" \c "defg")))
    (is (= "abcd" (str/clip 4 "ab" \c "defg")))
    (is (= "abcde" (str/clip 5 "ab" \c "defg"))))
  (testing "two strings & digit"
    (is (= "" (str/clip 0 "ab" 9 "defg")))
    (is (= "a" (str/clip 1 "ab" 9 "defg")))
    (is (= "ab" (str/clip 2 "ab" 9 "defg")))
    (is (= "ab9" (str/clip 3 "ab" 9 "defg")))
    (is (= "ab9d" (str/clip 4 "ab" 9 "defg")))
    (is (= "ab9de" (str/clip 5 "ab" 9 "defg"))))
  (testing "vector"
    (is (= "" (str/clip 0 [1 2 3 4 5])))
    (is (= "[" (str/clip 1 [1 2 3 4 5])))
    (is (= "[1" (str/clip 2 [1 2 3 4 5])))
    (is (= "[1 2" (str/clip 4 [1 2 3 4 5])))
    (is (= "[1 2 3 4" (str/clip 8 [1 2 3 4 5])))
    (is (= "[1 2 3 4 5]" (str/clip 16 [1 2 3 4 5]))))
  (testing "map"
    (is (= "" (str/clip 0 (sorted-map :a 1 :b 2))))
    (is (= "{" (str/clip 1 (sorted-map :a 1 :b 2))))
    (is (= "{:" (str/clip 2 (sorted-map :a 1 :b 2))))
    (is (= "{:a " (str/clip 4 (sorted-map :a 1 :b 2))))
    (is (= "{:a 1, :" (str/clip 8 (sorted-map :a 1 :b 2))))
    (is (= "{:a 1, :b 2}" (str/clip 16 (sorted-map :a 1 :b 2)))))
  (testing "set"
    (let [tst-set (sorted-set 5 4 3 2 1)]
      (is (= "" (str/clip 0 tst-set)))
      (is (= "#" (str/clip 1 tst-set)))
      (is (= "#{" (str/clip 2 tst-set)))
      (is (= "#{1 " (str/clip 4 tst-set)))
      (is (= "#{1 2 3 " (str/clip 8 tst-set)))
      (is (= "#{1 2 3 4 5}" (str/clip 16 tst-set))))))

(verify
  (is= "" (str/tabs->spaces ""))
  (is= "x" (str/tabs->spaces "x"))
  ;     01234567012345670123456701234567
  (is= "        x" (str/tabs->spaces (str/join [\tab \x])))
  (is= "0       x" (str/tabs->spaces (str/join [\0 \tab \x])))
  (is= "01      x" (str/tabs->spaces (str/join [\0 \1 \tab \x])))
  (is= "012     x" (str/tabs->spaces (str/join [\0 \1 \2 \tab \x])))
  (is= "0123    x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \tab \x])))
  (is= "01234   x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \tab \x])))
  (is= "012345  x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \tab \x])))
  (is= "0123456 x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \tab \x])))
  ;     01234567012345670123456701234567
  (is= "01234567        x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \tab \x])))
  (is= "012345670       x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \tab \x])))
  (is= "0123456701      x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \tab \x])))
  (is= "01234567012     x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \tab \x])))
  (is= "012345670123    x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \tab \x])))
  (is= "0123456701234   x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \tab \x])))
  (is= "01234567012345  x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \5 \tab \x])))
  (is= "012345670123456 x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \5 \6 \tab \x])))
  ;     01234567012345670123456701234567
  (is= "0123456701234567        x" (str/tabs->spaces (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \0 \1 \2 \3 \4 \5 \6 \7 \tab \x])))
  ;     01234567012345670123456701234567


  (is= "" (str/tabs->spaces 4 ""))
  (is= "x" (str/tabs->spaces 4 "x"))
  ;     0123012301230123
  (is= "    x" (str/tabs->spaces 4 (str/join [\tab \x])))
  (is= "0   x" (str/tabs->spaces 4 (str/join [\0 \tab \x])))
  (is= "01  x" (str/tabs->spaces 4 (str/join [\0 \1 \tab \x])))
  (is= "012 x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \tab \x])))
  ;     0123012301230123
  (is= "0123    x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \tab \x])))
  (is= "01234   x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \tab \x])))
  (is= "012345  x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \tab \x])))
  (is= "0123456 x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \tab \x])))
  ;     0123012301230123
  (is= "01234567    x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \tab \x])))
  (is= "012345678   x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \8 \tab \x])))
  (is= "0123456789  x" (str/tabs->spaces 4 (str/join [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \tab \x])))

  ;     0123012301230123
  (is= "01  a   b" (str/tabs->spaces 4 (str/join [\0 \1 \tab \a \tab \b]))))

(verify
  (let [text-blk (str/join \newline
                   ["one two three four five six seven eight nine ten"
                    "one two three four five six seven eight nine ten"
                    "one two three four five six seven eight nine ten"])]
    (is (str/nonblank= (str/clip-text 30 text-blk)
          (str/join \newline ["one two three four five six se"
                              "one two three four five six se"
                              "one two three four five six se"])))))

(verify
  (is= (str/join \newline ["    a"
                           "0   a"])
    (str/tabs->spaces 4
      (str/join [\tab \a \newline
                 \0 \tab \a])))
  (is= (str/join \newline ["    ab"
                           "0   a"])
    (str/tabs->spaces 4
      (str/join [\tab \a \b \newline
                 \0 \tab \a])))
  (is= (str/join \newline ["    abc"
                           "0   a"])
    (str/tabs->spaces 4
      (str/join [\tab \a \b \c \newline
                 \0 \tab \a])))
  (is= (str/join \newline ["    abcd"
                           "0   a"])
    (str/tabs->spaces 4
      (str/join [\tab \a \b \c \d \newline
                 \0 \tab \a]))))

(verify
  ; clojure.core/str works correctly for various string combinations
  (is= "" (str ""))
  (is= "a" (str "" "a"))
  (is= "a" (str "" "a" ""))
  (is= "ab" (str "a" "b"))
  (is= "ab" (str "" "a" "b" ""))
  (is= "abc" (str "a" "" "bc"))
  (is= "abc" (str "a" "bc" ""))
  (is= "abc" (str "a" "bc"))
  (is= "abc" (str "ab" "c"))
  (is= "abc" (str "a" "b" "c"))

  ; clojure.core/str works correctly for mixed strings and chars
  (is= "abc" (str \a "b" "c"))
  (is= "abc" (str "a" \b "c"))
  (is= "abc" (str "a" "b" \c))
  (is= "abc" (str \a \b "c"))
  (is= "abc" (str \a "b" \c))
  (is= "abc" (str "a" \b \c))
  (is= "abc" (str \a \b \c))

  ; clojure.core/str failure cases
  (isnt= "abc" (cc/drop 1 (str "xabc")))
  (isnt= "abc" (cc/take 3 (str "abcxxx")))

  ; Cannot fix it by using (str ...) on output of take/drop, since result is a LazySeq
  (isnt= "abc" (str (cc/drop 1 (str "xabc"))))
  (isnt= "abc" (str (cc/take 3 (str "abcxxx"))))

  #?(:clj (do
            (is (t/truthy? (re-find #"clojure.lang.LazySeq@.*" (str (cc/drop 1 (str "xabc"))))))
            (is (t/truthy? (re-find #"clojure.lang.LazySeq@.*" (str (cc/take 3 (str "abcxxx"))))))))

  ; Can fix it using str/join
  (is= "abc" (str/join (cc/drop 1 (str "xabc"))))
  (is= "abc" (str/join (cc/take 3 (str "abcxxx"))))

  ; Demo that str/join takes a single collection arg
  (is (= "" (str/join [""])))
  (is (= "a" (str/join ["" "a"])))
  (is (= "abc" (str/join ["" "a" "bc"])))

  ; A sequence is not a string, but tupelo.core/strcat can turn a sequence into a string.
  ; Also works to flatten out all nested collections.
  (isnt (= "abc" (seq "abc")))
  (is (= "abc" (t/strcat (seq "abc"))))
  (is (= "abcde" (t/strcat ["" \a \b \c "de"])))
  (is (= "abcde" (t/strcat ["" \a \b [\c ["d" \e]]]))))

(verify
  (is (= "abc def g hij kl"
        (str/whitespace-collapse "  abc    def			g
                                     hij kl	 ")))
  (is (= "abc" (str/whitespace-remove "abc")))
  (is (= "" (str/whitespace-remove "")))
  (is= "abcdef"
    (str/whitespace-remove "abc def")
    (str/whitespace-remove "  abc def")
    (str/whitespace-remove "abc def  ")
    (str/whitespace-remove "  abc def  ")
    (str/whitespace-remove "  a    bc def  ")
    (str/whitespace-remove (str "  abc " \newline " def  ")))
  (is= "a"
    (str/whitespace-remove "a")
    (str/whitespace-remove "a ")
    (str/whitespace-remove " a")
    (str/whitespace-remove (str "  a " \newline "   ")))
  (is= ""
    (str/whitespace-remove "")
    (str/whitespace-remove "  ")
    (str/whitespace-remove (str "  " \newline "   "))))

(verify
  (is (str/nonblank= "a"))
  (is (str/nonblank= "a" "  a "))
  (is (str/nonblank= "a" "  a  " "   a" "a   "))

  (is-nonblank= "a" "  a ")
  (is-nonblank= "a" "  a  " "   a" "a   ")

  (is (str/nonblank= "
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
                               Mark Twain      		"))
  (is (str/nonblank-lines= "a
                          b "
        "  a
           b"))
  (isnt (str/nonblank-lines= "a
                          b "
          "  a b "))

  (is-nonblank-lines= "a
                          b "
    "  a
       b"))

(verify
  (is= (str/quotes->single (str \")) (str \'))
  (is= (str/quotes->double (str \')) (str \"))
  (let [s1 "I said, 'Yes, please.'"
        s2 "I said, \"Yes, please.\""]
    (is= s1 (-> s2 str/quotes->single))
    (is= s2 (-> s1 str/quotes->double))
    (is= s2 (-> s2 str/quotes->single
              str/quotes->double))
    (is= s1 (-> s1 str/quotes->double
              str/quotes->single))))

(verify
  (let [kabob-str "abc-de-f-ghi"
        snake-str "abc_de_f_ghi"
        kabob-kw  (keyword kabob-str)
        snake-kw  (keyword snake-str)
        kabob-sym (symbol kabob-str)
        snake-sym (symbol snake-str)]
    (is= "abc" (name :abc))
    (is= "abc" (name (symbol "abc")))

    (is= kabob-kw (str/->kabob-kw kabob-str))
    (is= kabob-kw (str/->kabob-kw kabob-kw))
    (is= kabob-kw (str/->kabob-kw kabob-sym))
    (is= kabob-kw (str/->kabob-kw snake-str))
    (is= kabob-kw (str/->kabob-kw snake-kw))
    (is= kabob-kw (str/->kabob-kw snake-sym))

    (is= kabob-str (str/->kabob-str kabob-str))
    (is= kabob-str (str/->kabob-str kabob-kw))
    (is= kabob-str (str/->kabob-str kabob-sym))
    (is= kabob-str (str/->kabob-str snake-str))
    (is= kabob-str (str/->kabob-str snake-kw))
    (is= kabob-str (str/->kabob-str snake-sym))

    (is= snake-kw (str/->snake-kw kabob-str))
    (is= snake-kw (str/->snake-kw kabob-kw))
    (is= snake-kw (str/->snake-kw kabob-sym))
    (is= snake-kw (str/->snake-kw snake-str))
    (is= snake-kw (str/->snake-kw snake-kw))
    (is= snake-kw (str/->snake-kw snake-sym))

    (is= snake-str (str/->snake-str kabob-str))
    (is= snake-str (str/->snake-str kabob-kw))
    (is= snake-str (str/->snake-str kabob-sym))
    (is= snake-str (str/->snake-str snake-str))
    (is= snake-str (str/->snake-str snake-kw))
    (is= snake-str (str/->snake-str snake-sym)))

  (is= (str/->kabob-str "some_multiple_word_str") "some-multiple-word-str")
  (is= (str/->snake-str "some-multiple-word-str") "some_multiple_word_str")

  (is= (str/->kabob-kw :some_multiple_word_kw) :some-multiple-word-kw)
  (is= (str/->snake-kw :some-multiple-word-kw) :some_multiple_word_kw))

(verify
  (is= :abc-def-gh-qrs (str/str->kw-normalized "abc def*gh_qrs"))
  (is= :ABC-DEF-gh-qrs (str/str->kw-normalized "ABC DEF*gh_qrs"))
  (is= :abc-def-gh-qrs
    (str/clojurize-key "abc def*gh_qrs")
    (str/clojurize-key "abc DEF*gh_qrs")
    (str/clojurize-key "ABC def*GH_QRS")))

; #todo need cljs tests (or delete completely?)
#?(:clj (do
          (verify
            (is (= [65 66 67] (into [] (str/str->byte-array "ABC"))))
            (is (= "ABC" (str/byte-array->str (byte-array [65 66 67]))))
            (is (= "Hello World!" (-> "Hello World!" (str/str->byte-array) (str/byte-array->str)))))

          (verify
            (is (= " :a :b 3 4" (t/seq->str [:a :b 3 4])))
            (is (= " \\a \\b \\c" (t/seq->str "abc")))
            (is (= " 1 2 3" (t/seq->str (byte-array [1 2 3])))))
          ))

(verify
  (isnt (str/increasing? "abc" "a"))
  (isnt (str/increasing? "abc" "ab"))
  (isnt (str/increasing? "abc" "abc"))
  (is (str/increasing? "abc" "abd"))
  (is (str/increasing? "abc" "abcd"))
  (is (str/increasing? "abc" "ad"))
  (is (str/increasing? "abc" "b"))

  (isnt (str/increasing-or-equal? "abc" "a"))
  (isnt (str/increasing-or-equal? "abc" "ab"))
  (is (str/increasing-or-equal? "abc" "abc"))
  (is (str/increasing-or-equal? "abc" "abd"))
  (is (str/increasing-or-equal? "abc" "abcd"))
  (is (str/increasing-or-equal? "abc" "ad"))
  (is (str/increasing-or-equal? "abc" "b")))

(verify
  (is= (str/walk-strings->keywords {"aa" ["bb" 33 "dd"]}) {:aa [:bb 33 :dd]})
  (is= (str/walk-keywords->strings {:aa [:bb 33 :dd]}) {"aa" ["bb" 33 "dd"]})

  (is= (str/walk-clojurize-keys {"aa" ["aval" 33 {"nested" "map"}]
                                 :bb  "bval"
                                 "cc" #{"c1" "c2" :ck1}})
    {:aa ["aval" 33 {:nested "map"}]
     :bb "bval"
     :cc #{:ck1 "c2" "c1"}}))

(verify
  (is (= "" (str/take 0 "abc")))
  (is (= "a" (str/take 1 "abc")))
  (is (= "ab" (str/take 2 "abc")))
  (is (= "abc" (str/take 3 "abc")))
  (is (= "abc" (str/take 4 "abc"))))

(verify
  (is (= "abc" (str/drop 0 "abc")))
  (is (= "bc" (str/drop 1 "abc")))
  (is (= "c" (str/drop 2 "abc")))
  (is (= "" (str/drop 3 "abc")))
  (is (= "" (str/drop 4 "abc"))))

(verify
  (is (= "abc" (str/indent 0 "abc")))
  (is (= " abc" (str/indent 1 "abc")))
  (is (= "  abc" (str/indent 2 "abc")))
  (is (= "   abc" (str/indent 3 "abc")))

  (is (= "ab" (str/indent 0 "ab")))
  (is (= " ab" (str/indent 1 "ab")))
  (is (= "  ab" (str/indent 2 "ab")))
  (is (= "   ab" (str/indent 3 "ab")))

  (is (= "a" (str/indent 0 "a")))
  (is (= " a" (str/indent 1 "a")))
  (is (= "  a" (str/indent 2 "a")))
  (is (= "   a" (str/indent 3 "a")))

  (is (= "" (str/indent 0 "")))
  (is (= " " (str/indent 1 "")))
  (is (= "  " (str/indent 2 "")))
  (is (= "   " (str/indent 3 ""))))

(verify
  ; clojure accepts either CR/LF or LF (CR=/return & LF=\newline) as line-separator
  (is (= "abc" (str/indent-lines 0 "abc")))
  (is (= "abc" (str/indent-lines 0 (str "abc" \newline))))
  (is (= "abc" (str/indent-lines 0 (str "abc" \return \newline))))

  ; counterexample: clojure doesn't accept \formfeed or solo \return as line-separator
  (isnt (= "abc" (str/indent-lines 0 (str "abc" \formfeed))))
  (isnt (= "abc" (str/indent-lines 0 (str "abc" \return))))

  (is (= "  abc\n  def" (str/indent-lines 2 (str "abc" \newline "def"))))
  (is (= "  abc\n  def" (str/indent-lines 2 (str "abc" \newline "def" \newline))))

  (is (= "abc\ndef" (str/indent-lines 0 (str "abc" \newline "def"))))
  (is (= " abc\n def" (str/indent-lines 1 (str "abc" \newline "def"))))
  (is (= "  abc\n  def" (str/indent-lines 2 (str "abc" \newline "def"))))
  (is (= "   abc\n   def" (str/indent-lines 3 (str "abc" \newline "def"))))
  )

(verify
  (is= 0 (str/index-of "abc" "a"))
  (is= 0 (str/index-of "abc" "ab"))
  (is= 0 (str/index-of "abc" "abc"))
  (is= 1 (str/index-of "abc" "b"))
  (is= 1 (str/index-of "abc" "bc"))
  (is= 2 (str/index-of "abc" "c"))
  (is= nil (str/index-of "abc" "d"))
  )

; search for successive string fragments (regex-safe)
(verify
  (is (str/contains-str-frags? "abcde" "a"))
  (is (str/contains-str-frags? "abcde" "a" "e"))
  (is (str/contains-str-frags? "abcde" "a" "c"))
  (is (str/contains-str-frags? "abcde" "a" "c" "e"))

  (isnt (str/contains-str-frags? "abcde" "z"))
  (isnt (str/contains-str-frags? "abcde" "e" "a"))
  (isnt (str/contains-str-frags? "abcde" "c" "a"))
  (isnt (str/contains-str-frags? "abcde" "c" "a" "e"))

  (is (str/contains-str-frags? "ab.*de" "a"))
  (is (str/contains-str-frags? "ab.*de" "a" "e"))
  (is (str/contains-str-frags? "ab.*de" "a" ".*"))
  (is (str/contains-str-frags? "ab.*de" "a" ".*" "e"))

  (is (str/contains-str-frags? "ab.de" "a"))
  (is (str/contains-str-frags? "ab.de" "a" "e"))
  (is (str/contains-str-frags? "ab.de" "a" "."))
  (is (str/contains-str-frags? "ab.de" "a" "." "e"))

  (isnt (str/contains-str-frags? "abcde" "a" "."))
  (isnt (str/contains-str-frags? "abcde" "a" "."))
  (isnt (str/contains-str-frags? "abcde" "a" ".*"))
  (isnt (str/contains-str-frags? "abcde" "a" ".*")))

(verify
  ; clojure.string
  ; (t/when-clojure-1-8-plus)
  (is (str/starts-with? "abcde" "a"))
  (is (str/starts-with? "abcde" "ab"))
  (is (str/starts-with? "abcde" "abc"))

  (isnt (str/starts-with? "abcde" "b"))
  (isnt (str/starts-with? "abcde" "bc"))

  (isnt (str/starts-with? "a" "ab"))
  (isnt (str/starts-with? "ab" "abc"))

  ;-----------------------------------------------------------------------------
  ; tupelo.string
  (is (str/contains-match? "abcde" #"abc"))
  (is (str/contains-match? "abcde" #"abc.*"))
  (is (str/contains-match? "abcde" #".bc.*"))
  (is (str/contains-match? "abcde" #"^ab"))
  (is (str/contains-match? "abcde" #"bc"))
  (isnt (str/contains-match? "abcde" #"^bc"))
  (isnt (str/contains-match? "abcde" #".bc9.*"))

  ; regex special chars don't work in tgt-str
  (is (str/contains-str? "abcde" "abc"))
  (is (str/contains-str? "abcde" "bc"))
  (isnt (str/contains-str? "abcde" "abc.*"))
  (isnt (str/contains-str? "abcde" ".bc.*"))
  (isnt (str/contains-str? "abcde" "^ab"))
  (isnt (str/contains-str? "abcde" "^bc"))

  ; regex special chars OK in both search-str & tgt-str
  (is (str/contains-str? "abc.*de" "abc.*"))
  (is (str/contains-str? "a.bc.*de" ".bc.*"))
  (is (str/contains-str? "^abcde" "^ab"))
  (is (str/contains-str? "a^bcde" "^bc"))

  (let [search-str "Hello there, you.
                        How are you
                           doing today?"]
    (is (str/nonblank= (str/grep #"hello" search-str) ""))
    (is (str/nonblank= (str/grep #"Hello" search-str) "Hello there, you."))
    (is (str/nonblank= (str/grep #"(?i)hello" search-str) "Hello there, you."))
    (is (str/nonblank= (str/grep #"you" search-str)
          "Hello there, you.
           How are you"))
    (is (str/nonblank= (str/grep #"today." search-str) "doing today?"))
    (is (str/nonblank= (str/fgrep "today." search-str) ""))))

(verify
  (throws? (str/lowercase=))
  (throws? (str/lowercase= "Camel-Case"))
  (is (str/lowercase= "abc" "ABC"))
  (is (str/lowercase= "Camel-Case" "camel-case" "CAMEL-CASE")))

(verify
  (is (str/alphanumeric? \a))
  (is (str/alphanumeric? [\a]))
  (is (str/alphanumeric? "a"))
  (is (str/alphanumeric? "abc"))
  (isnt (str/alphanumeric? "*"))
  (isnt (str/alphanumeric? "ab*de"))
  (isnt (str/alphanumeric? \=))

  (is (str/whitespace? "")) ; empty string counts as "whitespace"
  (is (str/whitespace? " "))
  (is (str/whitespace? " \n"))
  (is (str/whitespace? " \r"))
  (is (str/whitespace? (str \tab "   " "\n")))
  (isnt (str/whitespace? (str \tab " xyz " "\n")))

  (is (str/whitespace-horiz? char/whitespace-horiz))
  (is (str/whitespace-eol? char/whitespace-eol))
  (is (str/whitespace? char/whitespace))
  (is (str/lowercase? char/lowercase))
  (is (str/uppercase? char/uppercase))
  (is (str/digit? char/digit))
  (is (str/alpha? char/alpha))
  (is (str/visible? char/visible))
  (is (str/text? char/text))

  (is (apply str/whitespace-horiz? (vec char/whitespace-horiz)))
  (is (apply str/whitespace-eol? (vec char/whitespace-eol)))
  (is (apply str/whitespace? (vec char/whitespace)))
  (is (apply str/lowercase? (vec char/lowercase)))
  (is (apply str/uppercase? (vec char/uppercase)))
  (is (apply str/digit? (vec char/digit)))
  (is (apply str/alpha? (vec char/alpha)))
  (is (apply str/visible? (vec char/visible)))
  (is (apply str/text? (vec char/text))))

(verify
  (is= (str/pad-left "a" 4) "   a")
  (is= (str/pad-left "ab" 4) "  ab")
  (is= (str/pad-left "abc" 4) " abc")
  (is= (str/pad-left "abcd" 4) "abcd")
  (is= (str/pad-left "abcde" 4) "abcde")

  (is= (str/pad-right "a" 4) "a   ")
  (is= (str/pad-right "ab" 4) "ab  ")
  (is= (str/pad-right "abc" 4) "abc ")
  (is= (str/pad-right "abcd" 4) "abcd")
  (is= (str/pad-right "abcde" 4) "abcde")

  (is= (str/pad-left "a" 4 \-) "---a")
  (is= (str/pad-left "ab" 4 \-) "--ab")
  (is= (str/pad-left "abc" 4 \-) "-abc")
  (is= (str/pad-left "abcd" 4 \-) "abcd")
  (is= (str/pad-left "abcde" 4 \-) "abcde")

  (is= (str/pad-right "a" 4 \-) "a---")
  (is= (str/pad-right "ab" 4 \-) "ab--")
  (is= (str/pad-right "abc" 4 \-) "abc-")
  (is= (str/pad-right "abcd" 4 \-) "abcd")
  (is= (str/pad-right "abcde" 4 \-) "abcde"))

(verify
  (let [xx 123.456789012345]
    (is= "Cost: 00123" (str/format "Cost: %05d" 123))
    (is= "Cost:   123.46" (str/format "Cost: %8.2f" xx))))

