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
  #?(:cljs (:require-macros [tupelo.test]))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.math :as math]

    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.test :refer [testing is verify verify-focus
                         is isnt is= isnt= is-set= is-nonblank=
                         throws? throws-not?]]
    )
  #?(:clj  (:require [tupelo.types :as types])
     :cljs (:require [goog.string.format]))
  )

;---------------------------------------------------------------------------------------------------
#?(:cljs (enable-console-print!))
;---------------------------------------------------------------------------------------------------

(verify
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

  (throws? (math/factorial 1.5))
  (throws? (math/factorial -1))

  (is= 1 (math/signum 4.4))
  (is= 0 (math/signum 0))
  (is= -1 (math/signum -4.4))

  (is (math/same-sign 1 1))
  (is (math/same-sign -1 -1))
  (isnt (math/same-sign 1 -1))
  (isnt (math/same-sign -1 1)))

(verify
  (is= (take 0 math/fibonacci-seq) [])
  (is= (take 5 math/fibonacci-seq) [0 1 1 2 3])
  (is= (take 10 math/fibonacci-seq) [0 1 1 2 3 5 8 13 21 34])

  (is= (math/fibo-thru 0) [0])
  (is= (math/fibo-thru 1) [0 1 1])
  (is= (math/fibo-thru 2) [0 1 1 2])
  (is= (math/fibo-thru 3) [0 1 1 2 3])
  (is= (math/fibo-thru 4) [0 1 1 2 3])
  (is= (math/fibo-thru 5) [0 1 1 2 3 5])
  (is= (math/fibo-thru 6) [0 1 1 2 3 5])
  (is= (math/fibo-thru 7) [0 1 1 2 3 5])
  (is= (math/fibo-thru 8) [0 1 1 2 3 5 8])
  (is= (math/fibo-thru 34) [0 1 1 2 3 5 8 13 21 34])

  (is= 0 (math/fibo-nth 0))
  (is= 1 (math/fibo-nth 1))
  (is= 1 (math/fibo-nth 2))
  (is= 2 (math/fibo-nth 3))
  (is= 3 (math/fibo-nth 4))
  (is= 5 (math/fibo-nth 5))
  (is= 8 (math/fibo-nth 6))
  (is= 13 (math/fibo-nth 7))
  (is= 21 (math/fibo-nth 8))
  (is= 34 (math/fibo-nth 9))
  (is (< (math/pow2-nth 62) (math/fibo-nth 91) (math/pow2-nth 63))))

(verify
  (is= (take 0 math/pow2-seq) [])
  (is= (take 5 math/pow2-seq) [1 2 4 8 16])
  (is= (take 10 math/pow2-seq) [1 2 4 8 16 32 64 128 256 512])

  (is= (math/pow2-thru 0) [])
  (is= (math/pow2-thru 1) [1])
  (is= (math/pow2-thru 10) [1 2 4 8])
  (is= (math/pow2-thru 50) [1 2 4 8 16 32])
  (is= (math/pow2-thru 100) [1 2 4 8 16 32 64])
  (is= (math/pow2-thru 200) [1 2 4 8 16 32 64 128])
  (is= (math/pow2-thru 500) [1 2 4 8 16 32 64 128 256])

  (is= 1 (math/pow2-nth 0))
  (is= 2 (math/pow2-nth 1))
  (is= 4 (math/pow2-nth 2))
  (is= 8 (math/pow2-nth 3))
  (is= 16 (math/pow2-nth 4))
  (is= 32 (math/pow2-nth 5))
  (is= 64 (math/pow2-nth 6))
  (is= 128 (math/pow2-nth 7))
  (is= 256 (math/pow2-nth 8))
  (is= 512 (math/pow2-nth 9))

  #?(:clj ; cljs has no biginteger
     (do
       ; Long/MAX_VALUE value is (2^63 - 1), so stop at 62
       (is (= (math/pow2-nth 62) (math/pow->Long 2 62)))
       (is= 633825300114114700748351602688N (math/pow2-nth 99))))
  )

(verify
  (is= (take 0 math/pow2aug-seq) [])
  (is= (take 5 math/pow2aug-seq) [0 1 2 3 4])
  (is= (take 10 math/pow2aug-seq) [0 1 2 3 4 5 7 8 9 15])
  (is= (take 20 math/pow2aug-seq) [0 1 2 3 4 5 7 8 9 15 16 17 31 32 33 63 64 65 127 128])

  (is= (math/pow2aug-thru 0) [0])
  (is= (math/pow2aug-thru 1) [0 1])
  (is= (math/pow2aug-thru 10) [0 1 2 3 4 5 7 8 9])
  (is= (math/pow2aug-thru 50) [0 1 2 3 4 5 7 8 9 15 16 17 31 32 33])
  (is= (math/pow2aug-thru 100) [0 1 2 3 4 5 7 8 9 15 16 17 31 32 33 63 64 65])
  (is= (math/pow2aug-thru 200) [0 1 2 3 4 5 7 8 9 15 16 17 31 32 33 63 64 65 127 128 129])
  (is= (math/pow2aug-thru 500) [0 1 2 3 4 5 7 8 9 15 16 17 31 32 33 63 64 65 127 128 129 255 256 257])

  (is= 0 (math/pow2aug-nth 0))
  (is= 1 (math/pow2aug-nth 1))
  (is= 2 (math/pow2aug-nth 2))
  (is= 3 (math/pow2aug-nth 3))
  (is= 4 (math/pow2aug-nth 4))
  (is= 5 (math/pow2aug-nth 5))
  (is= 7 (math/pow2aug-nth 6))
  (is= 8 (math/pow2aug-nth 7))
  (is= 9 (math/pow2aug-nth 8))
  (is= 15 (math/pow2aug-nth 9))
  (is= 16 (math/pow2aug-nth 10))
  (is= 17 (math/pow2aug-nth 11))
  (is= 31 (math/pow2aug-nth 12))
  (is= 32 (math/pow2aug-nth 13))
  (is= 33 (math/pow2aug-nth 14))

  #?(:clj ; cljs has no biginteger
     (is= 2535301200456458802993406410751N (math/pow2aug-nth 300)))
  )

#?(:clj
   (do

     (verify
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

     (verify ; math operations with Long result
       ; shift toward +infinity
       (is= 5 (math/ceil->Long 4.5))
       (is= -4 (math/ceil->Long -4.5))

       ; shift toward -infinity
       (is= 4 (math/floor->Long 4.5))
       (is= -5 (math/floor->Long -4.5))

       ; truncate toward zero
       (is= 4 (math/trunc->Long 4.4))
       (is= -4 (math/trunc->Long -4.4))

       ; round toward nearest integer
       (is= 5 (math/round->Long 4.6))
       (is= 5 (math/round->Long 4.5))
       (is= 4 (math/round->Long 4.4))
       (is= -5 (math/round->Long -4.6))
       (is= -4 (math/round->Long -4.5))
       (is= -4 (math/round->Long -4.4))

       ; NOTE: looses precision big-time, but does not throw!!!
       (let [ll (math/round->Long 4.4e99)]
         (is= Long (type ll))
         (is (<= 18 (Math/log10 ll) 19))
         (is= 63.0 (math/log2 ll))) ; *** truncated to 63 bits! ***
       )

     ; Works correctly using BigDecimal/setScale & RoundingMode arg
     (verify
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
     (verify
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
     (verify ; mod works for bigint, always positive
       (is= 1 (mod 1N 2N))
       (is= 0 (mod 2N 2N))
       (is= 1 (mod 3N 2N))
       (is= 0 (mod 0N 2N))
       (is= 1 (mod -1N 2N))
       (is= 0 (mod -2N 2N))
       (is= 1 (mod -3N 2N)))

     (verify ; rem works for bigint, sgn(result)==sgn(numerator)
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
     (verify ; mod works for biginteger, always positive
       (is= 1 (mod-biginteger 1 2))
       (is= 0 (mod-biginteger 2 2))
       (is= 1 (mod-biginteger 3 2))
       (is= 0 (mod-biginteger 0 2))
       (is= 1 (mod-biginteger -1 2))
       (is= 0 (mod-biginteger -2 2))
       (is= 1 (mod-biginteger -3 2)))

     (verify ; rem works for biginteger, sgn(result)==sgn(numerator)
       (is= 1 (rem-biginteger 1 2))
       (is= 0 (rem-biginteger 2 2))
       (is= 1 (rem-biginteger 3 2))
       (is= 0 (rem-biginteger 0 2))
       (is= -1 (rem-biginteger -1 2))
       (is= 0 (rem-biginteger -2 2))
       (is= -1 (rem-biginteger -3 2)))

     (defn biginteger-equals? [a b] (and (t/biginteger? a) (t/biginteger? b) (= a b)))
     (defn long-equals? [a b] (and (types/long? a) (types/long? b) (= a b)))

     (verify
       (is (biginteger-equals? (math/pow->BigInteger 2 5) (biginteger 32)))
       (throws? (math/pow->BigInteger 2 -5))

       (is (long-equals? (math/pow->Long 2 5) 32))
       (throws? (math/pow->Long 2 77))
       (throws? (math/pow->Long 2 -5)))

     (verify
       (let [bi-5 (BigInteger/valueOf 5)]
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

     ))

