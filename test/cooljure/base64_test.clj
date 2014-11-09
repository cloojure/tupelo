;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.base64-test
  (:require [clojure.string                           :as str]
            [clojure.test.generative                  :as tg]
            [clojure.test.generative.runner           :as tgr]
            [clojure.data.generators                  :as gen]
            [cooljure.base64                          :as b64]
            [cooljure.types                           :as types] 
            [cooljure.misc                            :as misc] 
            [schema.core                              :as s] )
  (:use cooljure.core
        clojure.test)
  (:gen-class))

(def SetOfStr
  #{ s/Str } )

(deftest t1 []
  (println "entering t1")
  (s/validate SetOfStr #{ "a" "b" "c"} )
  )

(def chars-seq
  "A seq of 1-char strings of all printable characters from space (32) to tilde (126)"
  (mapv str (misc/char-seq \space \~)))
(spy-expr chars-seq)

(defn b64-round-trip-bytes
  "Transform a seq of bytes to a base64 string and back."
  [data-in]
; (spy-expr (misc/seq->str data-in))
  (let [bytes-in        (byte-array data-in)
        _ (assert (= (seq bytes-in) (seq data-in)))
        string-b64      (b64/encode-bytes->str      bytes-in)
        bytes-out       (b64/decode-str->bytes      string-b64) ]
;   (spy-expr (misc/seq->str string-b64))
;   (spy-expr (misc/seq->str bytes-out))
    (assert (every? b64/base64-chars (seq string-b64)))
    (assert (= (seq bytes-in) (seq bytes-out)))
    bytes-out))

(deftest sample-t
  (println \newline "base64 - basic test")
  (doseq [step [50 20 7]]
    (let [orig    (byte-array (mapv #(.byteValue %) (range 0 400 step)))
          result  (b64-round-trip-bytes orig) ]
;     (spy-expr (misc/seq->str orig))
      (spy-expr (misc/seq->str result)) 
      (assert (= (seq orig) (seq result))))))

(defn seq-of-bytes
  "Return a list or vec or bytes"
  []
  (gen/one-of   (gen/list gen/byte)
                (gen/vec  gen/byte)))

(tg/defspec bytes-spec
  b64-round-trip-bytes
  [^cooljure.base64-test/seq-of-bytes arg]
  (assert (types/byte-array? %)))

(deftest gen1
  (println \newline "base64 - generative test")
  (try
    (tgr/run 4 2000 #'cooljure.base64-test/bytes-spec)
    (catch Exception ex
      (println "Caught exception: " ex)
      (ex-data ex))))

#_(defn -main []
    (println \newline "#9")
    (println "chars-seq" (pr-str chars-seq))
    (doseq [curr-char chars-seq]
      (newline)
      (doseq [prefix ["" "a" "ab" "abc"] ]
        (let [orig-str    (str prefix curr-char)
              enc-str     (b64/encode-str orig-str)
              dec-str     (b64/decode-str enc-str) ]
          (print orig-str enc-str dec-str "        "))))
    (newline)

    (println \newline "#10")
    (println "chars-seq" (pr-str chars-seq))
    (doseq [curr-char chars-seq]
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
