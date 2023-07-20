;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.types
  (:use tupelo.types tupelo.core tupelo.test)
  (:refer-clojure :exclude [float? integer? double?]))

; As of Clojure 1.9.0-alpha5, boolean? is native to clojure
#_(verify
    (is (boolean? true))
    (is (boolean? false))
    (isnt (boolean? :hello))
    (isnt (boolean? "hello"))
    (isnt (boolean? 'hello))
    (isnt (boolean? 1))
    (isnt (boolean? 0))
    (isnt (boolean? 3.14))
    (isnt (boolean? \K)))

(let [boolean-arr (boolean-array [true false])
      char-arr    (char-array [\a \b \c])
      byte-arr    (byte-array (range 3))
      short-arr   (short-array (range 3))
      int-arr     (int-array (range 3))
      long-arr    (long-array (range 3))
      float-arr   (float-array (range 3))
      double-arr  (double-array (range 3))
      object-arr  (object-array ["hello" 5 nil])]

  (verify
    (is (boolean-array? boolean-arr))
    (isnt (boolean-array? char-arr))
    (isnt (boolean-array? byte-arr))
    (isnt (boolean-array? short-arr))
    (isnt (boolean-array? int-arr))
    (isnt (boolean-array? long-arr))
    (isnt (boolean-array? float-arr))
    (isnt (boolean-array? double-arr))
    (isnt (boolean-array? object-arr)))

  (verify
    (isnt (char-array? boolean-arr))
    (is (char-array? char-arr))
    (isnt (char-array? byte-arr))
    (isnt (char-array? short-arr))
    (isnt (char-array? int-arr))
    (isnt (char-array? long-arr))
    (isnt (char-array? float-arr))
    (isnt (char-array? double-arr))
    (isnt (char-array? object-arr)))

  (verify
    (isnt (byte-array? boolean-arr))
    (isnt (byte-array? char-arr))
    (is (byte-array? byte-arr))
    (isnt (byte-array? short-arr))
    (isnt (byte-array? int-arr))
    (isnt (byte-array? long-arr))
    (isnt (byte-array? float-arr))
    (isnt (byte-array? double-arr))
    (isnt (byte-array? object-arr)))

  (verify
    (isnt (short-array? boolean-arr))
    (isnt (short-array? char-arr))
    (isnt (short-array? byte-arr))
    (is (short-array? short-arr))
    (isnt (short-array? int-arr))
    (isnt (short-array? long-arr))
    (isnt (short-array? float-arr))
    (isnt (short-array? double-arr))
    (isnt (short-array? object-arr)))

  (verify
    (isnt (int-array? boolean-arr))
    (isnt (int-array? char-arr))
    (isnt (int-array? byte-arr))
    (isnt (int-array? short-arr))
    (is (int-array? int-arr))
    (isnt (int-array? long-arr))
    (isnt (int-array? float-arr))
    (isnt (int-array? double-arr))
    (isnt (int-array? object-arr)))

  (verify
    (isnt (long-array? boolean-arr))
    (isnt (long-array? char-arr))
    (isnt (long-array? byte-arr))
    (isnt (long-array? short-arr))
    (isnt (long-array? int-arr))
    (is (long-array? long-arr))
    (isnt (long-array? float-arr))
    (isnt (long-array? double-arr))
    (isnt (long-array? object-arr)))

  (verify
    (isnt (float-array? boolean-arr))
    (isnt (float-array? char-arr))
    (isnt (float-array? byte-arr))
    (isnt (float-array? short-arr))
    (isnt (float-array? int-arr))
    (isnt (float-array? long-arr))
    (is (float-array? float-arr))
    (isnt (float-array? double-arr))
    (isnt (float-array? object-arr)))

  (verify
    (isnt (double-array? boolean-arr))
    (isnt (double-array? char-arr))
    (isnt (double-array? byte-arr))
    (isnt (double-array? short-arr))
    (isnt (double-array? int-arr))
    (isnt (double-array? long-arr))
    (isnt (double-array? float-arr))
    (is (double-array? double-arr))
    (isnt (double-array? object-arr)))

  (verify
    (isnt (object-array? boolean-arr))
    (isnt (object-array? char-arr))
    (isnt (object-array? byte-arr))
    (isnt (object-array? short-arr))
    (isnt (object-array? int-arr))
    (isnt (object-array? long-arr))
    (isnt (object-array? float-arr))
    (isnt (object-array? double-arr))
    (is (object-array? object-arr)))

  )

(verify
  (is (byte? (byte 42)))
  (is (short? (short 42)))
  (is (integer? (int 42)))
  (is (long? 42)) ; native EDN
  (is (long? (long (byte 42)))) ; coerce to Long
  (is (float? (float 42)))
  (is (double? 42.0)) ; native EDN
  (is (double? (double 42))) ;coerce to Double
  (is (character? (char 97)))

  (let [float-val (float 42)
        long-val  (long 42)]
    (isnt (character? float-val))
    (isnt (byte? float-val))
    (isnt (short? float-val))
    (isnt (integer? float-val))
    (isnt (long? float-val))
    (isnt (float? long-val))
    (isnt (double? long-val))))

