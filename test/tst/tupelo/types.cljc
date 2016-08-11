;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.types
  (:use tupelo.types
        clojure.test)
  (:require [schema.core :as s]
            [tupelo.core :as t] ))

(t/refer-tupelo)
; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

; As of Clojure 1.9.0-alpha5, boolean? is native to clojure
(deftest t-boolean?
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

(deftest t-boolean-array?
  (is (identity (boolean-array? (boolean-array  [true false]))))
  (is (not      (boolean-array? (byte-array     (range 3)))))
  (is (not      (boolean-array? (char-array     [\a \b \c]))))
  (is (not      (boolean-array? (double-array   (range 3)))))
  (is (not      (boolean-array? (float-array    (range 3)))))
  (is (not      (boolean-array? (int-array      (range 3)))))
  (is (not      (boolean-array? (long-array     (range 3)))))
  (is (not      (boolean-array? (object-array   ["hello" 5 nil]))))
  (is (not      (boolean-array? (short-array    (range 3))))))

(deftest t-byte-array?
  (is (not      (byte-array? (boolean-array  [true false]))))
  (is (identity (byte-array? (byte-array     (range 3)))))
  (is (not      (byte-array? (char-array     [\a \b \c]))))
  (is (not      (byte-array? (double-array   (range 3)))))
  (is (not      (byte-array? (float-array    (range 3)))))
  (is (not      (byte-array? (int-array      (range 3)))))
  (is (not      (byte-array? (long-array     (range 3)))))
  (is (not      (byte-array? (object-array   ["hello" 5 nil]))))
  (is (not      (byte-array? (short-array    (range 3))))))

(deftest t-char-array?
  (is (not      (char-array? (boolean-array  [true false]))))
  (is (not      (char-array? (byte-array     (range 3)))))
  (is (identity (char-array? (char-array     [\a \b \c]))))
  (is (not      (char-array? (double-array   (range 3)))))
  (is (not      (char-array? (float-array    (range 3)))))
  (is (not      (char-array? (int-array      (range 3)))))
  (is (not      (char-array? (long-array     (range 3)))))
  (is (not      (char-array? (object-array   ["hello" 5 nil]))))
  (is (not      (char-array? (short-array    (range 3))))))

(deftest t-double-array?
  (is (not      (double-array? (boolean-array  [true false]))))
  (is (not      (double-array? (byte-array     (range 3)))))
  (is (not      (double-array? (char-array     [\a \b \c]))))
  (is (identity (double-array? (double-array   (range 3)))))
  (is (not      (double-array? (float-array    (range 3)))))
  (is (not      (double-array? (int-array      (range 3)))))
  (is (not      (double-array? (long-array     (range 3)))))
  (is (not      (double-array? (object-array   ["hello" 5 nil]))))
  (is (not      (double-array? (short-array    (range 3))))))

(deftest t-float-array?
  (is (not      (float-array? (boolean-array  [true false]))))
  (is (not      (float-array? (byte-array     (range 3)))))
  (is (not      (float-array? (char-array     [\a \b \c]))))
  (is (not      (float-array? (double-array   (range 3)))))
  (is (identity (float-array? (float-array    (range 3)))))
  (is (not      (float-array? (int-array      (range 3)))))
  (is (not      (float-array? (long-array     (range 3)))))
  (is (not      (float-array? (object-array   ["hello" 5 nil]))))
  (is (not      (float-array? (short-array    (range 3))))))

(deftest t-int-array?
  (is (not      (int-array? (boolean-array  [true false]))))
  (is (not      (int-array? (byte-array     (range 3)))))
  (is (not      (int-array? (char-array     [\a \b \c]))))
  (is (not      (int-array? (double-array   (range 3)))))
  (is (not      (int-array? (float-array    (range 3)))))
  (is (identity (int-array? (int-array      (range 3)))))
  (is (not      (int-array? (long-array     (range 3)))))
  (is (not      (int-array? (object-array   ["hello" 5 nil]))))
  (is (not      (int-array? (short-array    (range 3))))))

(deftest t-long-array?
  (is (not      (long-array? (boolean-array  [true false]))))
  (is (not      (long-array? (byte-array     (range 3)))))
  (is (not      (long-array? (char-array     [\a \b \c]))))
  (is (not      (long-array? (double-array   (range 3)))))
  (is (not      (long-array? (float-array    (range 3)))))
  (is (not      (long-array? (int-array      (range 3)))))
  (is (identity (long-array? (long-array     (range 3)))))
  (is (not      (long-array? (object-array   ["hello" 5 nil]))))
  (is (not      (long-array? (short-array    (range 3))))))

(deftest t-object-array?
  (is (not      (object-array? (boolean-array  [true false]))))
  (is (not      (object-array? (byte-array     (range 3)))))
  (is (not      (object-array? (char-array     [\a \b \c]))))
  (is (not      (object-array? (double-array   (range 3)))))
  (is (not      (object-array? (float-array    (range 3)))))
  (is (not      (object-array? (int-array      (range 3)))))
  (is (not      (object-array? (long-array     (range 3)))))
  (is (identity (object-array? (object-array   ["hello" 5 nil]))))
  (is (not      (object-array? (short-array    (range 3))))))

(deftest t-short-array?
  (is (not      (short-array? (boolean-array  [true false]))))
  (is (not      (short-array? (byte-array     (range 3)))))
  (is (not      (short-array? (char-array     [\a \b \c]))))
  (is (not      (short-array? (double-array   (range 3)))))
  (is (not      (short-array? (float-array    (range 3)))))
  (is (not      (short-array? (int-array      (range 3)))))
  (is (not      (short-array? (long-array     (range 3)))))
  (is (not      (short-array? (object-array   ["hello" 5 nil]))))
  (is (identity (short-array? (short-array    (range 3))))))

(deftest t-str->bytes
  (is (= [65 66 67] (into [] (str->bytes "ABC"))))
  (is (= "ABC" (bytes->str (byte-array [65 66 67]))))
  (is (= "Hello World!" (-> "Hello World!" (str->bytes) (bytes->str)))))

