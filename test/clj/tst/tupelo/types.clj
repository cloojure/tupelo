;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.types
  (:use tupelo.types tupelo.core tupelo.test )
  (:refer-clojure :exclude [float? integer? double?])
  (:require [tupelo.core :as t] ) )

; As of Clojure 1.9.0-alpha5, boolean? is native to clojure
#_(dotest
  (is (boolean? true))
  (is (boolean? false))
  (is (not (boolean? :hello)))
  (is (not (boolean? "hello")))
  (is (not (boolean? 'hello)))
  (is (not (boolean? 1)))
  (is (not (boolean? 0)))
  (is (not (boolean? 3.14)))
  (is (not (boolean? \K)))
)

(dotest
  (is (identity (boolean-array? (boolean-array  [true false]))))
  (is (not      (boolean-array? (byte-array     (range 3)))))
  (is (not      (boolean-array? (char-array     [\a \b \c]))))
  (is (not      (boolean-array? (double-array   (range 3)))))
  (is (not      (boolean-array? (float-array    (range 3)))))
  (is (not      (boolean-array? (int-array      (range 3)))))
  (is (not      (boolean-array? (long-array     (range 3)))))
  (is (not      (boolean-array? (object-array   ["hello" 5 nil]))))
  (is (not      (boolean-array? (short-array    (range 3))))))

(dotest
  (is (not      (byte-array? (boolean-array  [true false]))))
  (is (identity (byte-array? (byte-array     (range 3)))))
  (is (not      (byte-array? (char-array     [\a \b \c]))))
  (is (not      (byte-array? (double-array   (range 3)))))
  (is (not      (byte-array? (float-array    (range 3)))))
  (is (not      (byte-array? (int-array      (range 3)))))
  (is (not      (byte-array? (long-array     (range 3)))))
  (is (not      (byte-array? (object-array   ["hello" 5 nil]))))
  (is (not      (byte-array? (short-array    (range 3))))))

(dotest
  (is (not      (char-array? (boolean-array  [true false]))))
  (is (not      (char-array? (byte-array     (range 3)))))
  (is (identity (char-array? (char-array     [\a \b \c]))))
  (is (not      (char-array? (double-array   (range 3)))))
  (is (not      (char-array? (float-array    (range 3)))))
  (is (not      (char-array? (int-array      (range 3)))))
  (is (not      (char-array? (long-array     (range 3)))))
  (is (not      (char-array? (object-array   ["hello" 5 nil]))))
  (is (not      (char-array? (short-array    (range 3))))))

(dotest
  (is (not      (double-array? (boolean-array  [true false]))))
  (is (not      (double-array? (byte-array     (range 3)))))
  (is (not      (double-array? (char-array     [\a \b \c]))))
  (is (identity (double-array? (double-array   (range 3)))))
  (is (not      (double-array? (float-array    (range 3)))))
  (is (not      (double-array? (int-array      (range 3)))))
  (is (not      (double-array? (long-array     (range 3)))))
  (is (not      (double-array? (object-array   ["hello" 5 nil]))))
  (is (not      (double-array? (short-array    (range 3))))))

(dotest
  (is (not      (float-array? (boolean-array  [true false]))))
  (is (not      (float-array? (byte-array     (range 3)))))
  (is (not      (float-array? (char-array     [\a \b \c]))))
  (is (not      (float-array? (double-array   (range 3)))))
  (is (identity (float-array? (float-array    (range 3)))))
  (is (not      (float-array? (int-array      (range 3)))))
  (is (not      (float-array? (long-array     (range 3)))))
  (is (not      (float-array? (object-array   ["hello" 5 nil]))))
  (is (not      (float-array? (short-array    (range 3))))))

(dotest
  (is (not      (int-array? (boolean-array  [true false]))))
  (is (not      (int-array? (byte-array     (range 3)))))
  (is (not      (int-array? (char-array     [\a \b \c]))))
  (is (not      (int-array? (double-array   (range 3)))))
  (is (not      (int-array? (float-array    (range 3)))))
  (is (identity (int-array? (int-array      (range 3)))))
  (is (not      (int-array? (long-array     (range 3)))))
  (is (not      (int-array? (object-array   ["hello" 5 nil]))))
  (is (not      (int-array? (short-array    (range 3))))))

(dotest
  (is (not      (long-array? (boolean-array  [true false]))))
  (is (not      (long-array? (byte-array     (range 3)))))
  (is (not      (long-array? (char-array     [\a \b \c]))))
  (is (not      (long-array? (double-array   (range 3)))))
  (is (not      (long-array? (float-array    (range 3)))))
  (is (not      (long-array? (int-array      (range 3)))))
  (is (identity (long-array? (long-array     (range 3)))))
  (is (not      (long-array? (object-array   ["hello" 5 nil]))))
  (is (not      (long-array? (short-array    (range 3))))))

(dotest
  (is (not      (object-array? (boolean-array  [true false]))))
  (is (not      (object-array? (byte-array     (range 3)))))
  (is (not      (object-array? (char-array     [\a \b \c]))))
  (is (not      (object-array? (double-array   (range 3)))))
  (is (not      (object-array? (float-array    (range 3)))))
  (is (not      (object-array? (int-array      (range 3)))))
  (is (not      (object-array? (long-array     (range 3)))))
  (is (identity (object-array? (object-array   ["hello" 5 nil]))))
  (is (not      (object-array? (short-array    (range 3))))))

(dotest
  (is (not      (short-array? (boolean-array  [true false]))))
  (is (not      (short-array? (byte-array     (range 3)))))
  (is (not      (short-array? (char-array     [\a \b \c]))))
  (is (not      (short-array? (double-array   (range 3)))))
  (is (not      (short-array? (float-array    (range 3)))))
  (is (not      (short-array? (int-array      (range 3)))))
  (is (not      (short-array? (long-array     (range 3)))))
  (is (not      (short-array? (object-array   ["hello" 5 nil]))))
  (is (identity (short-array? (short-array    (range 3))))))

(dotest
  (do
    (is (byte? (Byte. (byte 42))))
    (is (short? (Short. (short 42))))
    (is (integer? (Integer. 42)))
    (is (long? (Long. 42)))
    (is (float? (Float. 42.0)))
    (is (double? (Double. 42.0)))
    (is (character? (Character. (char 97)))))
  (let [float-val (Float. 42.0)
        long-val  (Long.  42)]
    (isnt (character? float-val))
    (isnt (byte? float-val))
    (isnt (short? float-val))
    (isnt (integer? float-val))
    (isnt (long? float-val))
    (isnt (float? long-val))
    (isnt (double? long-val))))

(dotest
  (is (= [65 66 67] (into [] (str->byte-array "ABC"))))
  (is (= "ABC" (byte-array->str (byte-array [65 66 67]))))
  (is (= "Hello World!" (-> "Hello World!" (str->byte-array) (byte-array->str)))))

