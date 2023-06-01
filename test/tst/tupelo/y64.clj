;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.y64
  (:use tupelo.core tupelo.test tupelo.test.jvm )
  (:require [clojure.string :as str]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tst]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [tupelo.chars :as char]
            [tupelo.misc :as misc]
            [tupelo.types :as types]
            [tupelo.y64 :as y64]) )

(when (is-java-8-plus?)
  (verify
    (let [orig      (byte-array [(byte \A)])
          y64-bytes (y64/encode-byte-array orig)
          result    (y64/decode-byte-array y64-bytes)]
      (is (every? y64/encoding-char-set (map char y64-bytes)))
      (is (= (seq orig) (seq result)))))

  (verify
    ;byte
    (doseq [step [50 20 7]]
      (let [orig      (byte-array (mapv #(.byteValue %) (range 0 400 step)))
            y64-bytes (y64/encode-byte-array orig)
            result    (y64/decode-byte-array y64-bytes)]
        (is (every? y64/encoding-char-set (map char y64-bytes)))
        (is (= (seq orig) (seq result)))))
    ; string
    (doseq [num-chars [1 2 3 7 20]]
      (let [orig    (str/join (misc/take-dist num-chars char/text))
            y64-str (y64/encode-str orig)
            result  (y64/decode-str y64-str)]
        (is (every? y64/encoding-char-set (seq y64-str)))
        (is (= orig result)))))

  ; Transform a seq of bytes to a y64 string and back
  (dospec 999       ; round-trip-bytes
    (prop/for-all [orig gen/bytes]
      (let [y64-str (y64/encode-byte-array->str orig)
            result  (y64/decode-str->byte-array y64-str)]
        (assert (every? y64/encoding-char-set (seq y64-str)))
        (assert (types/byte-array? result))
        (= (seq orig) (seq result)))))

  ; Transform a string to a y64 string and back
  (dospec 999       ; round-trip-string
    (prop/for-all [orig gen/string]
      (let [y64-str (y64/encode-str orig)
            result  (y64/decode-str y64-str)]
        (assert (every? y64/encoding-char-set (seq y64-str)))
        (assert (string? result))
        (= orig result)))))

(defn -main []
  (newline)
  (println "printable-chars" (pr-str char/text))
  (newline)
  (doseq [curr-char char/text]
    (newline)
    (doseq [prefix ["" "a" "ab" "abc"]]
      (let [orig-str (str prefix curr-char)
            enc-str  (y64/encode-str orig-str)
            dec-str  (y64/decode-str enc-str)]
        (print (format "\"%s\" \"%s\" \"%s\"          " orig-str enc-str dec-str)))))
  (newline))
