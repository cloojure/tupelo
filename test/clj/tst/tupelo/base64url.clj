;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.base64url
  (:use tupelo.test)
  (:require [clojure.string                         :as str]
            [clojure.test.check                     :as tc]
            [clojure.test.check.generators          :as gen]
            [clojure.test.check.properties          :as prop]
            [clojure.test.check.clojure-test        :as tst]
            [tupelo.base64url                       :as b64url]
            [tupelo.chars                            :as char]
            [tupelo.core                            :as t]
            [tupelo.misc                            :as misc]
            [tupelo.types                           :as types] ) )

(when (t/is-java-8-plus?)
  (verify
    (let [orig     (byte-array [(byte \A)])
          code-str (b64url/encode-byte-array->str orig)
          result   (b64url/decode-str->byte-array code-str)]
      (is (every? b64url/encoding-char-set (seq code-str)))
      (is (= (seq orig) (seq result)))))

  (verify
    ;base64 - bytes "
    (doseq [step [50 20 7]]
      (let [orig     (byte-array (mapv #(.byteValue %) (range 0 400 step)))
            code-str (b64url/encode-byte-array->str orig)
            result   (b64url/decode-str->byte-array code-str)]
        (is (every? b64url/encoding-char-set (seq code-str)))
        (is (= (seq orig) (seq result)))))

    ;base64 - string"
    (doseq [num-chars [1 2 3 7 20]]
      (let [orig     (str/join (misc/take-dist num-chars char/text))
            code-str (b64url/encode-str orig)
            result   (b64url/decode-str code-str)]
        (is (every? b64url/encoding-char-set (seq code-str)))
        (is= orig result)))
    (is= "KzEyMy1oZWxsbw==" (b64url/encode-str "+123-hello")))

  (dospec 999       ; round-trip-bytes
    (prop/for-all [orig gen/bytes]
      (let [string-b64 (b64url/encode-byte-array->str orig)
            result     (b64url/decode-str->byte-array string-b64)]
        (assert (every? b64url/encoding-char-set (seq string-b64)))
        (assert (types/byte-array? result))
        (= (seq orig) (seq result)))))
  ; Transform a string to a base64 string and back
  (dospec 999       ; round-trip-string
    (prop/for-all [orig gen/string]
      (let [string-b64 (b64url/encode-str orig)
            result     (b64url/decode-str string-b64)]
        (assert (every? b64url/encoding-char-set (seq string-b64)))
        (assert (string? result))
        (= orig result))))

)

(defn -main []
  (newline)
  (println "char/text" (pr-str char/text))
  (newline)
  (doseq [curr-char char/text]
    (newline)
    (doseq [prefix ["" "a" "ab" "abc"] ]
      (let [orig-str    (str prefix curr-char)
            enc-str     (b64url/encode-str orig-str)
            dec-str     (b64url/decode-str enc-str) ]
        (print (format "\"%s\" \"%s\" \"%s\"          " orig-str enc-str dec-str)))))
  (newline))

