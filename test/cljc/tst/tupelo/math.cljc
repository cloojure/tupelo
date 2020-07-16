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
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture ]]
    ) )

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
  (newline)
  (println :factorial-fail--start)
  (throws? (math/factorial 1.5))
  (throws? (math/factorial -1))
  (throws? (math/factorial -1))
  (println :factorial-fail--end)
  )


(dotest
  (let [sqrt-2     1.414213562
        sqrt-2-rnd (math/round-decimals sqrt-2 2)
        error      (- 1.414 sqrt-2-rnd)]
    (is (<= 0 (Math/abs error) 0.01))
    (is (t/rel= sqrt-2-rnd sqrt-2 :tol 0.01)))
  (let [val     12345
        val-rnd (math/round-decimals val -2)
        error   (- val val-rnd)]
    (is (<= 0 (Math/abs error) 100))
    (is (t/rel= val-rnd val :tol 100))
    (is (t/rel= val-rnd val :digits 2))
    (isnt (t/rel= val-rnd val :digits 4))))


; #todo review - old stuff from clj/tupelo/math.clj
(dotest
  ; Java Class
  (is= (type 5) (type (long 5.0)) java.lang.Long)
  (is= (type (int 5)) (type (int 5.0)) java.lang.Integer)
  (is= (type 5.0) (type (double 5)) java.lang.Double)
  (is= (type 5M) (type (bigdec 5)) (type (java.math.BigDecimal. "5")) java.math.BigDecimal)
  (is= (type 5N) (type (bigint 5)) (type (bigint 5.0)) clojure.lang.BigInt)
  (is= (type (biginteger 5)) (type (biginteger 5.0)) (type (java.math.BigInteger. "5")) java.math.BigInteger)

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
  (isnt= (biginteger 5) 5.0))


