;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.base64
  "Convert to/from traditional base64 encoding."
  (:require [clojure.string :as str]
            [tupelo.misc  :as misc]
            [tupelo.types :as types]
            [criterium.core :as crit])
  (:use tupelo.core )
  (:gen-class))

; #todo: convert :pre/:post to Prismatic Schema

(def base64-chars
  "A set of chars used for traditional base64 encoding (incl. padding char)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) 
                       [\+ \/ \=] ] )))

(def ^:private encoder (java.util.Base64/getEncoder))
(def ^:private decoder (java.util.Base64/getDecoder))

(defn encode-bytes
  "Encodes a byte array into base64, returning a new byte array."
  [data-bytes]
  (.encode encoder data-bytes))

(defn decode-bytes
  "Decodes a byte array from base64, returning a new byte array."
  [code-bytes]
  (.decode decoder code-bytes))

(defn encode-bytes->str
  "Encodes a byte array into base64, returning a String."
  [data-bytes]
  (.encodeToString encoder data-bytes))

(defn decode-str->bytes
  "Decodes a base64 encoded String, returning a byte array"
  [code-str]
  (.decode decoder code-str))

(defn encode-str 
  "Encodes a String into base64, returning a String."
  [data-str]
  (-> data-str types/str->bytes encode-bytes->str))

(defn decode-str 
  "Decodes a base64 encoded String, returning a String."
  [code-str]
  (-> code-str decode-str->bytes types/bytes->str))

(defn ^:private exercise-code []
  (doseq [step [50 20 7]]
    (let [orig        (byte-array (mapv #(.byteValue %) (range 0 400 step)))
          b64-str     (encode-bytes->str  orig)
          result      (decode-str->bytes  b64-str) ] ))
  (doseq [num-chars [1 2 3 7 20]]
    (let [orig        (str/join (misc/take-dist num-chars misc/printable-chars))
          b64-str     (encode-str  orig)
          result      (decode-str  b64-str) ] )))

(defn ^:private quick-bench []
  (crit/quick-bench (exercise-code)))

(defn ^:private bench []
  (crit/bench (exercise-code)))
