;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns   tst.tupelo.bits
  (:use tupelo.bits tupelo.core tupelo.test)
  (:require
    [schema.core :as s]
    ))

(verify
  (is= (bitchar->bitval \0) 0)
  (is= (bitchar->bitval \1) 1)
  (throws? (bitchar->bitval \2))

  (is= (bitval->bitchar 0) \0)
  (is= (bitval->bitchar 1) \1)
  (throws? (bitval->bitchar 2))

  ;-----------------------------------------------------------------------------
  ; Byte/valueOf & friends do not accept twos-complement strings, only strings with an optional +/- sign:
  ;                       123456789
  (is= 127 (Byte/valueOf "1111111" 2)) ; 7x1
  (is= 127 (Byte/valueOf "01111111" 2)) ; 8 char,
  (is= 127 (Byte/valueOf "001111111" 2)) ; 9 char
  (is= -127 (Byte/valueOf "-1111111" 2)) ; 7x1 with neg
  (throws? (Byte/valueOf "11111111" 2)) ; 8x1 fails

  (is= 5 (bits-unsigned->byte [1 0 1]))
  (is= 127 (bits-unsigned->byte [1 1 1 1 1 1 1 ]))
  (is= 127 (bits-unsigned->byte [0 1 1 1 1 1 1 1 ])) ; leading zeros are ok
  (is= 127 (bits-unsigned->byte [0 0 1 1 1 1 1 1 1 ])) ;  even if more than 8 bits
  (throws? (bits-unsigned->byte [1 1 1 1 1 1 1 1])) ; twos-complement -1 fails (8x1 bits)

  (is= (take-last 5 (byte->bits-unsigned 5)) [0 0 1 0 1])
  (is= Byte/MAX_VALUE (-> Byte/MAX_VALUE
                        (byte->bits-unsigned)
                        (bits-unsigned->byte)))
  (is= 0 (-> 0
           (byte->bits-unsigned)
           (bits-unsigned->byte)))
  (is= 123 (-> 123
                 (byte->bits-unsigned)
                 (bits-unsigned->byte)))

  (let [invalid-bits (glue [1 1 1] (-> Byte/MAX_VALUE
                                     (byte->bits-unsigned)))]
    (throws? (bits-unsigned->byte invalid-bits)))

  ;-----------------------------------------------------------------------------
  (is= (take-last 5 (long->bits-unsigned 5)) [0 0 1 0 1])
  (is= Long/MAX_VALUE (-> Long/MAX_VALUE
                        (long->bits-unsigned)
                        (bits-unsigned->long)))
  (is= 0 (-> 0
           (long->bits-unsigned)
           (bits-unsigned->long)))
  (is= 1234567 (-> 1234567
                 (long->bits-unsigned)
                 (bits-unsigned->long)))

  (let [invalid-bits (glue [1 1 1] (-> Long/MAX_VALUE
                                     (long->bits-unsigned)))]
    (throws? (bits-unsigned->long invalid-bits)))

  )


; #todo review - old stuff from clj/tupelo/math.clj
(verify
  ; Java Class
  (is= (type 5) (type (long 5.0)) java.lang.Long)
  (is= (type (int 5)) (type (int 5.0)) java.lang.Integer)
  (is= (type 5.0) (type (double 5)) java.lang.Double)
  (is= (type 5M) (type (bigdec 5)) (type (java.math.BigDecimal. "5")) java.math.BigDecimal)
  (is= (type 5N) (type (bigint 5)) (type (bigint 5.0)) clojure.lang.BigInt)
  (is= (type (clojure.core/biginteger 5))
    (type (biginteger (bigint 5)))
    (type (biginteger (bigdec 5)))
    (type (biginteger "5"))
    (type (biginteger 5.0)) (type (java.math.BigInteger. "5"))
    java.math.BigInteger) ; clojure.core/BigInt <> java.math.BigInteger

  ; type testing
  (is (bigdecimal? (bigdec 5)))
  (is (bigdecimal? 5M))
  (is (bigint? (bigint 5)))
  (is (bigint? 5N))
  (is (biginteger? (biginteger 5)))
  (isnt (biginteger? 5N))

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

(verify   ; BigInteger parsing/formatting
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

    ; #todo need examples for negative numbers

    (is= "1101" (intval->binary-str bi-13))
    (is= bi-13 (binary-str->BigInteger "1101"))

    (is= [\1 \1 \0 \1] (intval->binary-chars bi-13))
    (is= 13 (binary-chars->BigInteger [\1 \1 \0 \1]))

    ; verify `bitstr` gives expected result
    (throws? (intval->bitstr 5 2))
    (is= "101" (intval->bitstr 5 3))
    (is= "0101" (intval->bitstr 5 4))
    (is= "00000101" (intval->bitstr 5 8))

    (let [bi-10   (biginteger 10)
          bi-cafe (biginteger 51966)
          bi-babe (biginteger 47806)]
      (is= "000a" (intval->hex-str bi-10 4))
      (is= "cafe" (intval->hex-str bi-cafe 4))
      (is= "babe" (intval->hex-str bi-babe 2)))))
