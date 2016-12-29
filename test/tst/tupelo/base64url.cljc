;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.base64url
  (:require [clojure.string                         :as str]
            [clojure.test.check                     :as tc]
            [clojure.test.check.generators          :as gen]
            [clojure.test.check.properties          :as prop]
            [clojure.test.check.clojure-test        :as tst]
            [tupelo.base64url                       :as b64url]
            [tupelo.core :as t]
            [tupelo.misc                            :as misc]
            [tupelo.types                           :as types]
            [schema.core                            :as s] )
  (:use clojure.test)
  (:gen-class))
(t/refer-tupelo)

(deftest t1
  (if (t/is-java-1-8-plus?)
    (let [orig     (byte-array [(byte \A)])
          code-str (b64url/encode-bytes->str orig)
          result   (b64url/decode-str->bytes code-str)]
      (is (every? b64url/code-chars (seq code-str)))
      (is (= (seq orig) (seq result))))))

(deftest b64-basic
  (testing "base64 - bytes "
    (if (t/is-java-1-8-plus?)
      (doseq [step [50 20 7]]
        (let [orig     (byte-array (mapv #(.byteValue %) (range 0 400 step)))
              code-str (b64url/encode-bytes->str orig)
              result   (b64url/decode-str->bytes code-str)]
          (is (every? b64url/code-chars (seq code-str)))
          (is (= (seq orig) (seq result)))))))
  (testing "base64 - string"
    (if (t/is-java-1-8-plus?)
      (doseq [num-chars [1 2 3 7 20]]
        (let [orig     (str/join (misc/take-dist num-chars misc/printable-chars))
              code-str (b64url/encode-str orig)
              result   (b64url/decode-str code-str)]
          (is (every? b64url/code-chars (seq code-str)))
          (is (= orig result)))))))

(if (t/is-java-1-8-plus?)
  (tst/defspec ^:slow round-trip-bytes 9999
    (prop/for-all [orig gen/bytes]
      (let [string-b64 (b64url/encode-bytes->str orig)
            result     (b64url/decode-str->bytes string-b64)]
        (assert (every? b64url/code-chars (seq string-b64)))
        (assert (types/byte-array? result))
        (= (seq orig) (seq result))))))

; Transform a string to a base64 string and back
(if (t/is-java-1-8-plus?)
  (tst/defspec ^:slow round-trip-string 9999
    (prop/for-all [orig gen/string]
      (let [string-b64 (b64url/encode-str orig)
            result     (b64url/decode-str string-b64)]
        (assert (every? b64url/code-chars (seq string-b64)))
        (assert (string? result))
        (= orig result)))))

(defn -main []
  (newline)
  (println "printable-chars" (pr-str misc/printable-chars))
  (newline)
  (doseq [curr-char misc/printable-chars]
    (newline)
    (doseq [prefix ["" "a" "ab" "abc"] ]
      (let [orig-str    (str prefix curr-char)
            enc-str     (b64url/encode-str orig-str)
            dec-str     (b64url/decode-str enc-str) ]
        (print (format "\"%s\" \"%s\" \"%s\"          " orig-str enc-str dec-str)))))
  (newline))

