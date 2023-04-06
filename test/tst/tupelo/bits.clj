;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns   tst.tupelo.bits
  (:use tupelo.bits tupelo.core tupelo.test))

(verify
  (is= (charval->bitval \0) 0)
  (is= (charval->bitval \1) 1)
  (throws? (charval->bitval \2))

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

