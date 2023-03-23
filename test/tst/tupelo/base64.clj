;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.base64
  (:use tupelo.test)
  (:require
    [clojure.string                    :as str]
    [clojure.test.check                :as check]
    [clojure.test.check.clojure-test   :as tst]
    [clojure.test.check.generators     :as gen]
    [clojure.test.check.properties     :as prop]
    [tupelo.base64                     :as b64]
    [tupelo.base64url                  :as b64url]
    [tupelo.chars                       :as char]
    [tupelo.core                       :as t]
    [tupelo.misc                       :as misc]
    [tupelo.types                      :as types]
    [tupelo.y64                        :as y64]
  ) )

(verify
  (when (t/is-java-8-plus?)
    (let [orig    (byte-array [(byte \A)])
          b64-str (b64/encode-byte-array->str orig)
          result  (b64/decode-str->byte-array b64-str)]
      (is (every? b64/base64-chars (seq b64-str)))
      (is (= (seq orig) (seq result))))))

(verify
  (when (t/is-java-8-plus?)
    ; bytes
    (doseq [step [50 20 7]]
      (let [orig    (byte-array (mapv #(.byteValue %) (range 0 400 step)))
            b64-str (b64/encode-byte-array->str orig)
            result  (b64/decode-str->byte-array b64-str)]
        (is (every? b64/base64-chars (seq b64-str)))
        (is (= (seq orig) (seq result)))))
    ; string
    (doseq [num-chars [1 2 3 7 20]]
      (let [orig    (str/join (misc/take-dist num-chars (vec char/text)))
            b64-str (b64/encode-str orig)
            result  (b64/decode-str b64-str)]
        (is (every? b64/base64-chars (seq b64-str)))
        (is (= orig result))))))

(when (t/is-java-8-plus?)
  (dospec 999 ; round-trip-bytes
    (prop/for-all [orig gen/bytes]
      (let [string-b64 (b64/encode-byte-array->str orig)
            result     (b64/decode-str->byte-array string-b64)]
        (assert (every? b64/base64-chars (seq string-b64)))
        (assert (types/byte-array? result))
        (= (seq orig) (seq result)))))
  ; Transform a string to a base64 string and back
  (dospec 999 ; round-trip-string
    (prop/for-all [orig gen/string]
      (let [string-b64 (b64/encode-str orig)
            result     (b64/decode-str string-b64)]
        (assert (every? b64/base64-chars (seq string-b64)))
        (assert (string? result))
        (= orig result)))) )

(verify
  (is= "jack+base64@ladderlife.com or 6a61636b2b686578406c61646465726c6966652e636f6d206f72206d64353a6330373761363662383137643733623536636130623665373265303239396132"

    (b64/decode-str
      "amFjaytiYXNlNjRAbGFkZGVybGlmZS5jb20gb3IgNmE2MTYzNmIyYjY4NjU3ODQwNmM2MTY0NjQ2NTcyNmM2OTY2NjUyZTYzNmY2ZDIwNmY3MjIwNmQ2NDM1M2E2MzMwMzczNzYxMzYzNjYyMzgzMTM3NjQzNzMzNjIzNTM2NjM2MTMwNjIzNjY1MzczMjY1MzAzMjM5Mzk2MTMy"
      )

    (b64url/decode-str
      "amFjaytiYXNlNjRAbGFkZGVybGlmZS5jb20gb3IgNmE2MTYzNmIyYjY4NjU3ODQwNmM2MTY0NjQ2NTcyNmM2OTY2NjUyZTYzNmY2ZDIwNmY3MjIwNmQ2NDM1M2E2MzMwMzczNzYxMzYzNjYyMzgzMTM3NjQzNzMzNjIzNTM2NjM2MTMwNjIzNjY1MzczMjY1MzAzMjM5Mzk2MTMy"
      )

    (b64url/decode-str
      "amFjaytiYXNlNjRAbGFkZGVybGlmZS5jb20gb3IgNmE2MTYzNmIyYjY4NjU3ODQwNmM2MTY0NjQ2NTcyNmM2OTY2NjUyZTYzNmY2ZDIwNmY3MjIwNmQ2NDM1M2E2MzMwMzczNzYxMzYzNjYyMzgzMTM3NjQzNzMzNjIzNTM2NjM2MTMwNjIzNjY1MzczMjY1MzAzMjM5Mzk2MTMy"
      ))

  ; padding = vs -
  (is=  (b64/encode-str "begin||end") "YmVnaW58fGVuZA==")
  (is=  (y64/encode-str "begin||end") "YmVnaW58fGVuZA--")

  ; / vs _
  (let [ss "Yr?~H1FfGZ}n4!}A([=Wi'k"]
    (is= (b64/encode-str (str "begin|" ss "|end")) "YmVnaW58WXI/fkgxRmZHWn1uNCF9QShbPVdpJ2t8ZW5k" )
    (is= (y64/encode-str (str "begin|" ss "|end")) "YmVnaW58WXI_fkgxRmZHWn1uNCF9QShbPVdpJ2t8ZW5k" ))

  ; + vs -
  (let [ss "ql>Q0cQ~\\O6"]
    (is= (b64/encode-str    (str "begin|" ss "|end")) "YmVnaW58cWw+UTBjUX5cTzZ8ZW5k")
    (is= (b64url/encode-str (str "begin|" ss "|end")) "YmVnaW58cWw-UTBjUX5cTzZ8ZW5k")))

(defn -main []
  (newline)
  (println "printable-chars" (pr-str char/text))
  (newline)
  (doseq [curr-char char/text]
    (newline)
    (doseq [prefix ["" "a" "ab" "abc"] ]
      (let [orig-str    (str prefix curr-char)
            enc-str     (b64/encode-str orig-str)
            dec-str     (b64/decode-str enc-str) ]
        (print (format "\"%s\" \"%s\" \"%s\"          " orig-str enc-str dec-str)))))
  (newline))

; #todo write auto-detector for iso-8859-1, if get neg bytes out?
; If the "plaintext" for a B64 string is iso-8859-1/iso-latin-1, it won't decode right with the default UTF-8 assumption
;(dotest
;  (let [iso-latin-1-charset (java.nio.charset.Charset/forName "ISO-8859-1" ) ; aka ISO-LATIN-1
;        b64-str       "JVBERi0xLjENCiXi48/TDQoxIDAgb2JqDQo8PCAN"
;        bytes-default (vec (.getBytes b64-str))
;        bytes-8859    (vec (.getBytes b64-str iso-latin-1-charset))
;
;        src-byte-array  (b64/decode-bytes (byte-array bytes-default))
;        src-bytes     (vec src-byte-array)
;        src-str-8859  (String. src-byte-array iso-latin-1-charset)
;        ]
;  (spyxx iso-latin-1-charset)
;  (spyx bytes-default)
;  (spyx bytes-8859)
;  (spyx (= bytes-default bytes-8859))
;  (spyx src-bytes)
;  (spyx src-str-8859)
;  ))
