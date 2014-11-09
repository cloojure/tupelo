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

(def b64-data-chars
  "A set of Base64 data characters a-z, A-A, 0-9 (not including padding characters like =)"
  (let [
  ]
  ))

(def chars-seq
  "A seq of 1-char strings of all printable characters from space (32) to tilde (126)"
  (mapv str 
    (mapv char 
      (range 32 (inc 126)))))

(defn seq-of-bytes
  "Return a list or vec or bytes"
  []
  (gen/one-of   (gen/list gen/byte)
                (gen/vec  gen/byte)))

; (defn bytes-round-trip
;   "Transform a seq of bytes to a base64 string and back."
;   [bytes-in]
;   (let [string-b64      (b64/encode-bytes->str      bytes-in)
;         bytes-out       (b64/decode-str->bytes      string-b64) ]
;     (assert (every? 
; 
; (tg/defspec bytes-t

#_(deftest sample-t
  (testing "basic usage"
    (println \newline "#1")
    (println 
      (b64/encode-bytes->str 
        (byte-array (map #(.byteValue %) (range 0 400 50)))))))

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
