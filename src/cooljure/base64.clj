;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.base64
  "Convert to/from traditional Base64 encoding."
  (:require [clojure.string :as str]
            [clojure.data.codec.base64 :as clj-b64]
            [cooljure.misc  :as misc]
            [cooljure.types :as types] )
  (:use cooljure.core)
  (:gen-class))

; #awt TODO: convert :pre/:post to Prismatic Schema

(def base64-chars
  "A set of chars used for traditional Base64 encoding (incl. padding char)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) 
                       [\+ \/ \=] ] )))

(defn encode-bytes
  "Encodes a byte array into Base64, returning a new byte array."
  [bytes-in]
  {:pre [(types/byte-array? bytes-in)] }
  ; clojure.data.codec.base64 does not handle case of zero-length bytes-in, so we must.
  (if (zero? (alength bytes-in))
    (byte-array [])
    (clj-b64/encode bytes-in)))

(defn decode-bytes
  "Decodes a byte array from Base64, returning a new byte array."
  [bytes-in]
  {:pre [(types/byte-array? bytes-in)] }
  ; clojure.data.codec.base64 does not handle case of zero-length bytes-in, so we must.
  ; #awt todo:  submit PR to fix
  (if (zero? (alength bytes-in))
    (byte-array [])
    (clj-b64/decode bytes-in)))

(defn encode-bytes->str
  "Encodes a byte array into base-64, returning a String."
  [bytes-in]
  {:pre  [ (types/byte-array? bytes-in) ] }
  (-> bytes-in encode-bytes types/bytes->str))

(defn decode-str->bytes
  "Decodes a base-64 encoded String, returning a byte array"
  [str-in]
  {:pre  [ (string? str-in) ] }
  (-> str-in types/str->bytes decode-bytes))


(defn encode-str 
  "Encodes a String into base-64, returning a String."
  [str-in]
  {:pre  [ (string? str-in) ] }
  (encode-bytes->str (types/str->bytes str-in)))

(defn decode-str 
  "Decodes a base-64 encoded String, returning a String."
  [str-in]
  {:pre  [ (string? str-in) ] }
  (types/bytes->str (decode-str->bytes str-in)))

