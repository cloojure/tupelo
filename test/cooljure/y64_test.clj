;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.y64-test
  (:require [clojure.string                         :as str]
            [clojure.test.check                     :as tc]
            [clojure.test.check.generators          :as gen]
            [clojure.test.check.properties          :as prop]
            [clojure.test.check.clojure-test        :as tst]
            [cooljure.y64                           :as y64]
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
  (s/validate SetOfStr #{ "a" "b" "c"} ))

(def printable-chars
  "A seq of 1-char strings of all printable characters from space (32) to tilde (126)"
  (mapv str (misc/char-seq \space \~)))

(deftest t1
  (let [orig        (byte-array [(byte \A)] )
        y64-bytes   (y64/encode-bytes  orig)
        result      (y64/decode-bytes  y64-bytes) ]
    (is (every? y64/y64-chars (map char y64-bytes)))
    (is (= (seq orig) (seq result)))))

(deftest y64-basic
  (testing "y64 - bytes "
    (doseq [step [50 20 7]]
      (let [orig        (byte-array (mapv #(.byteValue %) (range 0 400 step)))
            y64-bytes   (y64/encode-bytes  orig)
            result      (y64/decode-bytes  y64-bytes) ]
        (is (every? y64/y64-chars (map char y64-bytes)))
        (is (= (seq orig) (seq result))))))
  (testing "y64 - string"
    (doseq [num-chars [1 2 3 7 20]]
      (let [orig        (str/join (misc/take-dist num-chars printable-chars))
            y64-str     (y64/encode-str  orig)
            result      (y64/decode-str  y64-str) ]
        (is (every? y64/y64-chars (seq y64-str)))
        (is (= orig result))))))

; Transform a seq of bytes to a y64 string and back
(tst/defspec ^:slow y64-round-trip-bytes 9999
  (prop/for-all [orig gen/bytes]
    (let [y64-str   (y64/encode-bytes->str  orig)
          result    (y64/decode-str->bytes  y64-str) ]
      (assert (every? y64/y64-chars (seq y64-str)))
      (assert (types/byte-array? result))
      (= (seq orig) (seq result)))))

; Transform a string to a y64 string and back
(tst/defspec ^:slow y64-round-trip-string 9999
  (prop/for-all [orig gen/string]
    (let [y64-str   (y64/encode-str  orig)
          result    (y64/decode-str  y64-str) ]
      (assert (every? y64/y64-chars (seq y64-str)))
      (assert (string? result))
      (= orig result))))

(defn -main []
  (newline)
  (println "printable-chars" (pr-str printable-chars))
  (newline)
  (doseq [curr-char printable-chars]
    (newline)
    (doseq [prefix ["" "a" "ab" "abc"] ]
      (let [orig-str    (str prefix curr-char)
            enc-str     (y64/encode-str orig-str)
            dec-str     (y64/decode-str enc-str) ]
        (print (format "\"%s\" \"%s\" \"%s\"          " orig-str enc-str dec-str)))))
  (newline))

