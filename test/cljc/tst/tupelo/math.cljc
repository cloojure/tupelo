;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns ^:test-refresh/focus  tst.tupelo.math
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             ; [tupelo.core]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [schema.core :as s]
    [tupelo.math :as math]
    [tupelo.types :as types]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.testy :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture ]]

    #?(:cljs [goog.string.format] )

     )
  )

;---------------------------------------------------------------------------------------------------
#?(:cljs (enable-console-print!))
;---------------------------------------------------------------------------------------------------

(dotest
  (is= (math/factorial 0) 1)
  (is= (math/factorial 1) 1)
  (is= (math/factorial 2) 2)
  (is= (math/factorial 3) 6)
  (is= (math/factorial 4) 24)
  (is= (math/factorial 5) 120)
  (is= (math/factorial 6) 720)
  (is= (math/factorial 7) 5040)
  (is= (math/factorial 8) 40320)
  (is= (math/factorial 9) 362880)
  (is= (math/factorial 10) 3628800)
  (is (t/rel= (math/factorial 15) 1.307674368e+12 :digits 10))

  ;(newline)
  ;(println :factorial-fail--start)
  (throws? (math/factorial 1.5))
  (throws? (math/factorial -1))
  (throws? (math/factorial -1))
  ; (println :factorial-fail--end)
  )

(dotest
  (let [sqrt-2     1.414213562
        sqrt-2-rnd (math/round-N sqrt-2 2)
        error      (- 1.414 sqrt-2-rnd)]
    (is (<= 0 (Math/abs error) 0.01))
    (is (t/rel= sqrt-2-rnd sqrt-2 :tol 0.01)))
  (let [val     12345
        val-rnd (math/round-N val -2)
        error   (- val val-rnd)]
    (is (<= 0 (Math/abs error) 100))
    (is (t/rel= val-rnd val :tol 100))
    (is (t/rel= val-rnd val :digits 2))
    (isnt (t/rel= val-rnd val :digits 4))))

#?(:clj
   (do
     ; Works correctly using BigDecimal/setScale & RoundingMode arg
     (dotest
       (let [a6 1.112233
             a4 1.1122
             a2 1.11

             b6 1.667788
             b4 1.6677
             b2 1.66]
         (is= 1.112233M (math/->bigdec-N a6 6))
         (is= 1.112200M (math/->bigdec-N a4 6))
         (is= 1.110000M (math/->bigdec-N a2 6))
         (is= 1.667788M (math/->bigdec-N b6 6))
         (is= 1.667700M (math/->bigdec-N b4 6))
         (is= 1.660000M (math/->bigdec-N b2 6))

         (is= 1.1122M (math/->bigdec-N a6 4))
         (is= 1.1122M (math/->bigdec-N a4 4))
         (is= 1.1100M (math/->bigdec-N a2 4))
         (is= 1.6678M (math/->bigdec-N b6 4))
         (is= 1.6677M (math/->bigdec-N b4 4))
         (is= 1.6600M (math/->bigdec-N b2 4))

         (is= 1.11M (math/->bigdec-N a6 2))
         (is= 1.11M (math/->bigdec-N a4 2))
         (is= 1.11M (math/->bigdec-N a2 2))
         (is= 1.67M (math/->bigdec-N b6 2))
         (is= 1.67M (math/->bigdec-N b4 2))
         (is= 1.66M (math/->bigdec-N b2 2))

         (is= 1.11M (math/->bigdec-N (str a6) 2))
         (is= 1.11M (math/->bigdec-N (str a4) 2))
         (is= 1.11M (math/->bigdec-N (str a2) 2))
         (is= 1.67M (math/->bigdec-N (str b6) 2))
         (is= 1.67M (math/->bigdec-N (str b4) 2))
         (is= 1.66M (math/->bigdec-N (str b2) 2))
     ))

     ; Can accept value as a Double, BigDecimal, or String
     (dotest
       (let [a6 1.112233
             a4 1.1122
             a2 1.11

             b6 1.667788
             b4 1.6677
             b2 1.66]
         (is= 1.11M
           (math/->bigdec-2 a6)
           (math/->bigdec-2 (bigdec a6))
           (math/->bigdec-2 (str a6)))
         (is= 1.11M
           (math/->bigdec-2 a4)
           (math/->bigdec-2 (bigdec a4))
           (math/->bigdec-2 (str a4)))
         (is= 1.11M
           (math/->bigdec-2 a2)
           (math/->bigdec-2 (bigdec a2))
           (math/->bigdec-2 (str a2)))

         (is= 1.67M
           (math/->bigdec-2 b6)
           (math/->bigdec-2 (bigdec b6))
           (math/->bigdec-2 (str b6)))

         (is= 1.67M
           (math/->bigdec-2 b4)
           (math/->bigdec-2 (bigdec b4))
           (math/->bigdec-2 (str b4)))

         (is= 1.66M
           (math/->bigdec-2 b2)
           (math/->bigdec-2 (bigdec b2))
           (math/->bigdec-2 (str b2)))))

     ;---------------------------------------------------------------------------------------------------
     (set! *warn-on-reflection* true)

     ;---------------------------------------------------------------------------------------------------
     (dotest   ; mod works for bigint, always positive
       (is= 1 (mod 1N 2N))
       (is= 0 (mod 2N 2N))
       (is= 1 (mod 3N 2N))
       (is= 0 (mod 0N 2N))
       (is= 1 (mod -1N 2N))
       (is= 0 (mod -2N 2N))
       (is= 1 (mod -3N 2N)))

     (dotest   ; rem works for bigint, sgn(result)==sgn(numerator)
       (is= 1 (rem 1N 2N))
       (is= 0 (rem 2N 2N))
       (is= 1 (rem 3N 2N))
       (is= 0 (rem 0N 2N))
       (is= -1 (rem -1N 2N))
       (is= 0 (rem -2N 2N))
       (is= -1 (rem -3N 2N)))

     ;---------------------------------------------------------------------------------------------------
     (defn mod-biginteger [a b] (mod (BigInteger/valueOf a) (BigInteger/valueOf b)))
     (defn rem-biginteger [a b] (rem (BigInteger/valueOf a) (BigInteger/valueOf b)))
     (dotest   ; mod works for biginteger, always positive
       (is= 1 (mod-biginteger 1 2))
       (is= 0 (mod-biginteger 2 2))
       (is= 1 (mod-biginteger 3 2))
       (is= 0 (mod-biginteger 0 2))
       (is= 1 (mod-biginteger -1 2))
       (is= 0 (mod-biginteger -2 2))
       (is= 1 (mod-biginteger -3 2)))

     (dotest   ; rem works for biginteger, sgn(result)==sgn(numerator)
       (is= 1 (rem-biginteger 1 2))
       (is= 0 (rem-biginteger 2 2))
       (is= 1 (rem-biginteger 3 2))
       (is= 0 (rem-biginteger 0 2))
       (is= -1 (rem-biginteger -1 2))
       (is= 0 (rem-biginteger -2 2))
       (is= -1 (rem-biginteger -3 2)))

     (defn biginteger-equals? [a b] (and (t/biginteger? a) (t/biginteger? b) (= a b)))
     (defn long-equals? [a b] (and (types/long? a) (types/long? b) (= a b)))

     (dotest
       (is (biginteger-equals? (math/pow-BigInteger 2 5) (math/->BigInteger 32)))
       (throws? (math/pow-BigInteger 2 -5) )

       (is (long-equals? (math/pow-long 2 5) 32))
       (throws? (math/pow-long 2 77))
       (throws? (math/pow-long 2 -5)))

     (dotest
       (let [bi-5               (BigInteger/valueOf 5)]
         (is (biginteger-equals? bi-5 bi-5))
         (isnt (biginteger-equals? bi-5 5))
         (isnt (biginteger-equals? bi-5 5.0))
         (isnt (biginteger-equals? bi-5 5N))
         (isnt (biginteger-equals? bi-5 5M))

         (is (biginteger-equals? bi-5 (math/->BigInteger (int 5))))
         (is (biginteger-equals? bi-5 (math/->BigInteger (long 5))))
         (is (biginteger-equals? bi-5 (math/->BigInteger (float 5))))
         (is (biginteger-equals? bi-5 (math/->BigInteger (double 5))))
         (is (biginteger-equals? bi-5 (math/->BigInteger (bigint 5))))
         (is (biginteger-equals? bi-5 (math/->BigInteger (bigdec 5))))
         (is (biginteger-equals? bi-5 (math/->BigInteger bi-5))) ; idempotent

         (throws? (math/->BigInteger "abc"))
         (throws? (math/->BigInteger nil))
         (throws? (math/->BigInteger -666))))

     (dotest   ; BigInteger parsing/formatting
       (is= 42 (s/validate s/Int (BigInteger/valueOf 42.1))) ; truncates! #todo not good!
       (isnt (int? (BigInteger/valueOf 42))) ; works for Schema, but not clojure.core/int?

       (let [bi-13 (BigInteger/valueOf 13)
             s1    (.toString bi-13 2)
             bi2   (BigInteger. s1 2)
             s2    (.toString bi2 16)]
         (is= bi-13 13)
         (is= s1 "1101")
         (is= bi2 13)
         (is= s2 "d")

         (is= "1101" (math/BigInteger->binary-str bi-13))
         (is= bi-13 (math/binary-str->BigInteger "1101"))

         (is= [\1 \1 \0 \1] (math/BigInteger->binary-chars bi-13))
         (is= 13 (math/binary-chars->BigInteger [\1 \1 \0 \1]))

         (let [bi-10   (math/->BigInteger 10)
               bi-cafe (math/->BigInteger 51966)
               bi-babe (math/->BigInteger 47806)]
           (is= "000a" (math/BigInteger->hex-str bi-10 4))
           (is= "cafe" (math/BigInteger->hex-str bi-cafe 4))
           (is= "babe" (math/BigInteger->hex-str bi-babe 2)))))

     ;
     ))

#?(:clj
   ; #todo review - old stuff from clj/tupelo/math.clj
   (dotest
     ; Java Class
     (is= (type 5) (type (long 5.0)) java.lang.Long)
     (is= (type (int 5)) (type (int 5.0)) java.lang.Integer)
     (is= (type 5.0) (type (double 5)) java.lang.Double)
     (is= (type 5M) (type (bigdec 5)) (type (java.math.BigDecimal. "5")) java.math.BigDecimal)
     (is= (type 5N) (type (bigint 5)) (type (bigint 5.0)) clojure.lang.BigInt)
     (is= (type (biginteger 5))
       (type (clojure.core/biginteger (bigint 5)))
       (type (clojure.core/biginteger (bigdec 5)))
       (type (biginteger 5.0)) (type (java.math.BigInteger. "5"))
       java.math.BigInteger) ; clojure.core/BigInt <> java.math.BigInteger

     ; type testing
     (is (t/bigdecimal? (bigdec 5)))
     (is (t/bigdecimal? 5M))
     (is (t/bigint? (bigint 5)))
     (is (t/bigint? 5N))
     (is (t/biginteger? (biginteger 5)))
     (isnt (t/biginteger? 5N))

     ; equivalence of values
     (is= (bigdec 5) 5M)
     (isnt= (bigdec 5) 5)
     (isnt= (bigdec 5) 5.0)

     (is= (bigint 5) 5N)
     (is= (bigint 5) 5)
     (is= (bigint 5) (biginteger 5))
     (isnt= (bigint 5) 5.0)

     (is= (biginteger 5) 5N)
     (is= (biginteger 5) 5)
     (is= (biginteger 5) (bigint 5))
     (isnt= (biginteger 5) 5.0)

     ))
