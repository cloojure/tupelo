;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.math
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             ; [tupelo.core]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.math :as math]
    #?(:clj
       [tupelo.types :as types])
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.testy :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture ]]

    #?(:cljs [goog.string.format] )))

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

  (is= 1 (math/signum 4.4))
  (is= 0 (math/signum 0))
  (is= -1 (math/signum -4.4))

  (is (math/same-sign 1 1))
  (is (math/same-sign -1 -1))
  (isnt (math/same-sign 1 -1))
  (isnt (math/same-sign -1 1)))

#?(:clj
   (do

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

     (dotest   ; math operations with Long result
       ; shift toward +infinity
       (is= 5 (math/ceil-long 4.5))
       (is= -4 (math/ceil-long -4.5))

       ; shift toward -infinity
       (is= 4 (math/floor-long 4.5))
       (is= -5 (math/floor-long -4.5))

       ; truncate toward zero
       (is= 4 (math/trunc-long 4.4))
       (is= -4 (math/trunc-long -4.4))

       ; round toward nearest integer
       (is= 5 (math/round-long 4.6))
       (is= 5 (math/round-long 4.5))
       (is= 4 (math/round-long 4.4))
       (is= -5 (math/round-long -4.6))
       (is= -4 (math/round-long -4.5))
       (is= -4 (math/round-long -4.4))

       ; NOTE: looses precision big-time, but does not throw!!!
       (let [ll (math/round-long 4.4e99)]
         (is= Long (type ll))
         (is (<= 18 (Math/log10 ll) 19))
         (is= 63.0 (math/log2 ll))) ; *** truncated to 63 bits! ***
       )

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
       (is (biginteger-equals? (math/pow->BigInteger 2 5) (biginteger 32)))
       (throws? (math/pow->BigInteger 2 -5) )

       (is (long-equals? (math/pow->Long 2 5) 32))
       (throws? (math/pow->Long 2 77))
       (throws? (math/pow->Long 2 -5)))

     (dotest
       (let [bi-5               (BigInteger/valueOf 5)]
         (is (biginteger-equals? bi-5 bi-5))
         (isnt (biginteger-equals? bi-5 5))
         (isnt (biginteger-equals? bi-5 5.0))
         (isnt (biginteger-equals? bi-5 5N))
         (isnt (biginteger-equals? bi-5 5M))

         (is (biginteger-equals? bi-5 (biginteger (int 5))))
         (is (biginteger-equals? bi-5 (biginteger (long 5))))
         (is (biginteger-equals? bi-5 (biginteger (float 5))))
         (is (biginteger-equals? bi-5 (biginteger (double 5))))
         (is (biginteger-equals? bi-5 (biginteger (bigint 5))))
         (is (biginteger-equals? bi-5 (biginteger (bigdec 5))))
         (is (biginteger-equals? bi-5 (biginteger bi-5))) ; idempotent

         (throws? (biginteger "abc"))
         (throws? (biginteger nil))
         (is= -666 (biginteger -666))))

     )
   )

