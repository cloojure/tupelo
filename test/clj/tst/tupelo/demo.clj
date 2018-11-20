;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.demo
  (:use tupelo.test)
  (:require
    [clojure.string :as str]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t] )
  (:import java.time.Instant) )

; #todo add generative testing?
; #todo add clojure.spec testing?

(dotest
  (is (.isAfter (Instant/now) ; create clojure.java.time ns
        (Instant/parse "2017-12-31T13:14:15z")))
  (is (= 5 (+ 2 3))))

; (x ...)     =>  "function call on x" (parens = "function call")
; ... x ...   =>  "substitute the value of x (local or global Var)"
; Note that '5 is just shorthand for (quote 5), where `quote` is a "special form"
; (i.e compiler built-in function, not a normal function). Using the (quote ...)
; form "turns off" the normal behavior of evaluating (substituting) symbols & function calls.
(dotest
  (is= 5 '5 (quote 5))
  (is= [1 2 3] '[1 2 3] (quote [1 2 3]))
  (is= [1 2 3] '(1 2 3) (quote (1 2 3)))

  ; Nested quotes are weird. Don't do it unless you have a weird problem to solve.
  ; And even then, expect a lot of trial & error before it comes out right.
  (isnt= [1 2 3] '(1 2 '3))
  (is=
    '(1 2 '3)
    '(1 2 (quote 3))
    (quote (1 2 (quote 3)))
    [1 2 '(quote 3)]
    [1 2 ['quote 3]]
    [1 2 [(quote quote) 3]]
    [1 2 [(symbol "quote") 3]]
    [1 2 '(quote 3)]
    [1 2 ''3] ))

(dotest             ; empty lists/vectors
  (is= ()           ; should be error! parens => function call (here with missing function)
    []              ; preferred
    `() `[]         ; redundant quoting
    (list)))        ; build an empty list


(dotest
  (is= nil        (seq []))

  (is= []         (take 1 nil))
  (is= []         (take 1 []))
  (is= []         (take 2 []))

  (is= []         (take 0 [1 2]))
  (is= [1]        (take 1 [1 2]))
  (is= [1 2]      (take 2 [1 2]))
  (is= [1 2]      (take 3 [1 2]))

  (is= [        ] (take 0 "abc"))
  (is= [\a      ] (take 1 "abc"))
  (is= [\a \b   ] (take 2 "abc"))
  (is= [\a \b \c] (take 3 "abc"))
  (is= [\a \b \c] (take 4 "abc")))

(dotest
  (is= []         (drop 1 nil))
  (is= []         (drop 1 []))
  (is= []         (drop 2 []))

  (is= [1 2]      (drop 0 [1 2]))
  (is= [  2]      (drop 1 [1 2]))
  (is= []         (drop 2 [1 2]))
  (is= []         (drop 3 [1 2]))

  (is= [\a \b \c] (drop 0 "abc"))
  (is= [   \b \c] (drop 1 "abc"))
  (is= [      \c] (drop 2 "abc"))
  (is= [        ] (drop 3 "abc"))
  (is= [        ] (drop 4 "abc")))

(dotest
  (is= [\a \b \c] (seq "abc"))
  (is= [\a \b \c] (vec "abc"))

  (is= "abc" (str \a \b \c))
  (is= "abc" (str "ab" "c"))
  (is= "abc" (str "ab" \c ))

  (is= "abc" (str/join [\a \b \c] ))
  (is= "abc" (str/join ["ab" "c"] ))
  (is= "abc" (str/join ["ab" \c ] ))
)

(dotest
  (is   (sequential?  []     ))
  (is   (sequential?  [1 2 3]))
  (is   (sequential? '(1 2 3)))
  (isnt (sequential?  42     ))
  (isnt (sequential?  :x     ))
  (isnt (sequential?  "abc"  ))
  (isnt (sequential?  {:a 1} ))
  (isnt (sequential? #{:a 1} )) )

(dotest
  (is (every? odd? [1 3 5])))

(dotest
  (is= [] (range 0 -1))
  (is= [] (range 0 0))
  (is= [0] (range 0 1))
  (is= [0 1] (range 0 2)))

(dotest
  (is= #{} (empty #{1 2 3}))
  (is= [] (empty [1 2 3]))
  (is= {} (empty {:a 1 :b 2})) )

