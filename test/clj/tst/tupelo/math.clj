;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.math
  (:require
    [tupelo.math :as math]
    [schema.core :as s]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]])
  #?(:clj (:import [java.lang Byte Integer])))

#?(:cljs (enable-console-print!))

#?(:clj
   (do

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

     ))
