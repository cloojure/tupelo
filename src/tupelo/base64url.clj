;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.base64url
  "Convert to/from traditional base64url encoding."
  (:require [clojure.string :as str]
            [tupelo.misc  :as misc]
            [tupelo.types :as types]
            [criterium.core :as crit])
  (:use tupelo.core )
  (:gen-class))

; #todo: convert :pre/:post to Prismatic Schema

; #todo -> code-chars (& other ns's)
(def code-chars
  "A set of chars used for traditional base64url encoding (incl. padding char)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) 
                       [\- \_ \=] ] )))

(def ^:private encoder (java.util.Base64/getUrlEncoder))
(def ^:private decoder (java.util.Base64/getUrlDecoder))

(defn encode-bytes
  "Encodes a byte array into base64url, returning a new byte array."
  [data-bytes]
  (.encode encoder data-bytes))

(defn decode-bytes
  "Decodes a byte array from base64url, returning a new byte array."
  [data-bytes]
  (.decode decoder data-bytes))

(defn encode-bytes->str
  "Encodes a byte array into base64url, returning a String."
  [data-bytes]
  (.encodeToString encoder data-bytes))

(defn decode-str->bytes
  "Decodes a base64url encoded String, returning a byte array"
  [code-str]
  (.decode decoder code-str))

(defn encode-str 
  "Encodes a String into base64url, returning a String."
  [data-str]
  (-> data-str types/str->bytes encode-bytes->str))

(defn decode-str 
  "Decodes a base64url encoded String, returning a String."
  [code-str]
  (-> code-str decode-str->bytes types/bytes->str))

(defn ^:private exercise-code []
  (doseq [step [50 20 7]]
    (let [orig        (byte-array (mapv #(.byteValue %) (range 0 400 step)))
          code-str    (encode-bytes->str  orig)
          result      (decode-str->bytes  code-str) ] ))
  (doseq [num-chars [1 2 3 7 20]]
    (let [orig        (str/join (misc/take-dist num-chars misc/printable-chars))
          code-str    (encode-str  orig)
          result      (decode-str  code-str) ] )))

(defn ^:private quick-bench []
  (crit/quick-bench (exercise-code)))

(defn ^:private bench []
  (crit/bench (exercise-code)))
