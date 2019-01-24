;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.io
  (:use tupelo.io tupelo.core tupelo.test)
  (:refer-clojure :exclude [read-string])
  (:require
    [clojure.java.io :as io]
    [tupelo.types :as types])
  (:import [java.io DataInputStream DataOutputStream FileOutputStream ]))

(def int-val (long (+ 1e9 123456789)))
(def long-val (long 12345e9))
(def pi-float (float Math/PI))
(def pi-double (double Math/PI))

(def dummy-file-name "tst.tupelo.io")
(def dummy-file (create-temp-file dummy-file-name))

(dotest
  (is= types/BYTE_UNSIGNED_MAX_VALUE 255)
  (is= types/SHORT_UNSIGNED_MAX_VALUE 65535)

  (let [in-stream  (io/input-stream dummy-file)
        out-stream (io/output-stream dummy-file)
        dis        (DataInputStream. in-stream)
        dos        (DataOutputStream. out-stream)]
    (isnt (data-input-stream? in-stream))
    (is (input-stream? in-stream))
    (is (input-stream? dis))
    (is (data-input-stream? dis))

    (isnt (data-output-stream? out-stream))
    (is (output-stream? out-stream))
    (is (output-stream? dos))
    (is (data-output-stream? dos))))

(dotest
  (with-open [dos (DataOutputStream.
                    (FileOutputStream. dummy-file))]
    ;-----------------------------------------------------------------------------
    (throws? (write-byte dos (inc Byte/MAX_VALUE)))
    (throws? (write-byte dos (dec Byte/MIN_VALUE)))

    (throws? (write-short dos (inc Short/MAX_VALUE)))
    (throws? (write-short dos (dec Short/MIN_VALUE)))

    (throws? (write-integer dos (inc Integer/MAX_VALUE)))
    (throws? (write-integer dos (dec Integer/MIN_VALUE)))

    (throws? (write-long dos (* 2M (bigdec Long/MAX_VALUE))))
    (throws? (write-long dos (* -2M (bigdec Long/MIN_VALUE))))

    (throws? (write-byte-unsigned dos (inc types/BYTE_UNSIGNED_MAX_VALUE)))
    (throws? (write-byte-unsigned dos (dec types/BYTE_UNSIGNED_MIN_VALUE)))

    (throws? (write-short-unsigned dos (inc types/SHORT_UNSIGNED_MAX_VALUE)))
    (throws? (write-short-unsigned dos (dec types/SHORT_UNSIGNED_MIN_VALUE)))

    ;-----------------------------------------------------------------------------
    (is= (write-byte dos Byte/MIN_VALUE)         Byte/MIN_VALUE)
    (is= (write-byte dos Byte/MAX_VALUE)         Byte/MAX_VALUE)

    (is= (write-short dos Short/MIN_VALUE)       Short/MIN_VALUE)
    (is= (write-short dos Short/MAX_VALUE)       Short/MAX_VALUE)

    (is= (write-integer dos Integer/MIN_VALUE)       Integer/MIN_VALUE)
    (is= (write-integer dos Integer/MAX_VALUE)       Integer/MAX_VALUE)

    (is= (write-long dos Long/MIN_VALUE)         Long/MIN_VALUE)
    (is= (write-long dos Long/MAX_VALUE)         Long/MAX_VALUE)

    (is= (write-byte-unsigned dos types/BYTE_UNSIGNED_MIN_VALUE)       types/BYTE_UNSIGNED_MIN_VALUE)
    (is= (write-byte-unsigned dos types/BYTE_UNSIGNED_MAX_VALUE)       types/BYTE_UNSIGNED_MAX_VALUE)

    (is= (write-short-unsigned dos types/SHORT_UNSIGNED_MIN_VALUE)     types/SHORT_UNSIGNED_MIN_VALUE)
    (is= (write-short-unsigned dos types/SHORT_UNSIGNED_MAX_VALUE)     types/SHORT_UNSIGNED_MAX_VALUE)

    (is= (write-string-bytes dos "hello") "hello")))

(dotest
  (with-open [dos (DataOutputStream.
                    (FileOutputStream. dummy-file))]
    (doto dos
      (write-string-bytes "hello")
      (write-bytes (byte-array [1 2 3 4]))

      (write-byte  42)
      (write-byte -42)
      (write-byte Byte/MIN_VALUE)
      (write-byte Byte/MAX_VALUE)

      (write-byte-unsigned 142)
      (write-byte-unsigned types/BYTE_UNSIGNED_MIN_VALUE)
      (write-byte-unsigned types/BYTE_UNSIGNED_MAX_VALUE)

      (write-short  9999)
      (write-short -9999)
      (write-short Short/MIN_VALUE)
      (write-short Short/MAX_VALUE)

      (write-short-unsigned 55999)
      (write-short-unsigned types/SHORT_UNSIGNED_MIN_VALUE)
      (write-short-unsigned types/SHORT_UNSIGNED_MAX_VALUE)

      (write-integer int-val)
      (write-integer Integer/MIN_VALUE)
      (write-integer Integer/MAX_VALUE)

      (write-integer-unsigned int-val)
      (write-integer-unsigned types/INTEGER_UNSIGNED_MIN_VALUE)
      (write-integer-unsigned types/INTEGER_UNSIGNED_MAX_VALUE)

      (write-long long-val)
      (write-long Long/MIN_VALUE)
      (write-long Long/MAX_VALUE)

      (write-long-unsigned long-val)
      (write-long-unsigned types/LONG_UNSIGNED_MIN_VALUE)
      (write-long-unsigned types/LONG_UNSIGNED_MAX_VALUE)

      (write-float pi-float)
      (write-float Float/MIN_VALUE)
      (write-float Float/MAX_VALUE)

      (write-double pi-double)
      (write-double Double/MIN_VALUE)
      (write-double Double/MAX_VALUE)


      ))

  (with-open [dis (DataInputStream. (io/input-stream dummy-file))]
    (is= (read-string-bytes 5 dis) "hello")
    (is= (vec (read-bytes 4 dis)) [1 2 3 4])

    (is= (read-byte dis)  42)
    (is= (read-byte dis) -42)
    (is= (read-byte dis) Byte/MIN_VALUE)
    (is= (read-byte dis) Byte/MAX_VALUE)

    (is= (read-byte-unsigned dis) 142)
    (is= (read-byte-unsigned dis) types/BYTE_UNSIGNED_MIN_VALUE)
    (is= (read-byte-unsigned dis) types/BYTE_UNSIGNED_MAX_VALUE)

    (is= (read-short dis)  9999)
    (is= (read-short dis) -9999)
    (is= (read-short dis) Short/MIN_VALUE)
    (is= (read-short dis) Short/MAX_VALUE)

    (is= (read-short-unsigned dis) 55999)
    (is= (read-short-unsigned dis) types/SHORT_UNSIGNED_MIN_VALUE)
    (is= (read-short-unsigned dis) types/SHORT_UNSIGNED_MAX_VALUE)

    (is= (read-integer dis) int-val)
    (is= (read-integer dis) Integer/MIN_VALUE)
    (is= (read-integer dis) Integer/MAX_VALUE)

    (is= (read-integer-unsigned dis) int-val)
    (is= (read-integer-unsigned dis) types/INTEGER_UNSIGNED_MIN_VALUE)
    (is= (read-integer-unsigned dis) types/INTEGER_UNSIGNED_MAX_VALUE)

    (is= (read-long dis) long-val)
    (is= (read-long dis) Long/MIN_VALUE)
    (is= (read-long dis) Long/MAX_VALUE)

    (is= (read-long-unsigned dis) long-val)
    (is= (read-long-unsigned dis) types/LONG_UNSIGNED_MIN_VALUE)
    (is= (read-long-unsigned dis) types/LONG_UNSIGNED_MAX_VALUE)

    (is= (read-float dis) pi-float)
    (is= (read-float dis) Float/MIN_VALUE)
    (is= (read-float dis) Float/MAX_VALUE)

    (is= (read-double dis) pi-double)
    (is= (read-double dis) Double/MIN_VALUE)
    (is= (read-double dis) Double/MAX_VALUE))

  )




























