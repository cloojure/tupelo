;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.base64-test
  (:require [clojure.string                         :as str]
            [clojure.test.check                     :as tc]
            [clojure.test.check.generators          :as gen]
            [clojure.test.check.properties          :as prop]
            [clojure.test.check.clojure-test        :as tst]
            [cooljure.base64                        :as b64]
            [cooljure.misc                          :as misc] 
            [cooljure.types                         :as types] 
            [schema.core                            :as s] )
  (:use cooljure.core
        clojure.test)
  (:gen-class))

(def SetOfStr
  #{ s/Str } )

(deftest t1 []
  (println "t1 - enter")
  (s/validate SetOfStr #{ "a" "b" "c"} )
  (println "t1 - exit"))

(def printable-chars
  "A seq of 1-char strings of all printable characters from space (32) to tilde (126)"
  (mapv str (misc/char-seq \space \~)))
; (spy-expr printable-chars)

(defn b64-round-trip-str
  "Transform a string to a base64 string and back."
  [str-in]
  {:pre [(string? str-in)] }
  (let [str-b64    (b64/encode-str     str-in)
        str-out    (b64/decode-str     str-b64) ]
    (assert (every? b64/base64-chars (seq str-b64)))
    (assert (= str-in str-out))
    str-out))

(deftest b64-basic
  (testing "base64 - bytes "
    (doseq [step [50 20 7]]
      (let [orig        (byte-array (mapv #(.byteValue %) (range 0 400 step)))
            b64-str     (b64/encode-bytes->str  orig)
            result      (b64/decode-str->bytes  b64-str) ]
        (assert (= (seq orig) (seq result))))))
  (testing "base64 - string"
    (doseq [num-chars [1 2 3 7 20]]
      (let [orig        (str/join (misc/take-dist num-chars printable-chars))
            b64-str     (b64/encode-str  orig)
            result      (b64/decode-str  b64-str) ]
        (assert (= orig result))))))

; Transform a seq of bytes to a base64 string and back
(tst/defspec ^:slow b64-round-trip-bytes 9999
  (prop/for-all [orig gen/bytes]
    (let [string-b64  (b64/encode-bytes->str  orig)
          result      (b64/decode-str->bytes  string-b64) ]
      (every? b64/base64-chars (seq string-b64))
      (types/byte-array? result)
      (= (seq orig) (seq result)))))

; Transform a string to a base64 string and back
(tst/defspec ^:slow b64-round-trip-string 9999
  (prop/for-all [orig gen/string]
    (let [string-b64  (b64/encode-str  orig)
          result      (b64/decode-str  string-b64) ]
      (every? b64/base64-chars (seq string-b64))
      (types/byte-array? result)
      (= orig result))))

#_(defn -main []
    (println \newline "#9")
    (println "printable-chars" (pr-str printable-chars))
    (doseq [curr-char printable-chars]
      (newline)
      (doseq [prefix ["" "a" "ab" "abc"] ]
        (let [orig-str    (str prefix curr-char)
              enc-str     (b64/encode-str orig-str)
              dec-str     (b64/decode-str enc-str) ]
          (print orig-str enc-str dec-str "        "))))
    (newline)

    (println \newline "#10")
    (println "printable-chars" (pr-str printable-chars))
    (doseq [curr-char printable-chars]
      (newline)
      (doseq [prefix ["" "a" "ab" "abc"] ]
        (let [orig-str    (str prefix curr-char)
              enc-str     (b64/encode-bytes->str (types/str->bytes orig-str))
              dec-str     (str/join (map char (b64/decode-str->bytes enc-str))) ]
          (print orig-str enc-str dec-str "        "))))
    (newline)

    (newline)
    (println "  bye...")
  )
