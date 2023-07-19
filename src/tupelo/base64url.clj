;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.base64url
  "Convert to/from traditional base64url encoding."
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.string :as str]
    [tupelo.types :as types]
    ))

; #todo -> code-chars (& other ns's)
(def encoding-char-set
  "A set of chars used for traditional base64url encoding (incl. padding char)"
  (set (glue
         (chars-thru \a \z)
         (chars-thru \A \Z)
         (chars-thru \0 \9)
         [\- \_ \=])))


(def ^:no-doc b64-code-62  (byte \+))
(def ^:no-doc b64-code-63  (byte \/ ))
(def ^:no-doc b64-code-pad (byte \= ))

(def ^:no-doc b64url-code-62  (byte \- ))
(def ^:no-doc b64url-code-63  (byte \_ ))
(def ^:no-doc b64url-code-pad (byte \= ))

(s/defn ^:no-doc b64->b64url :- [s/Int] ; #todo need test
  "Converts a vector of byte values from base64 -> base64url encoding."
  [byte-vec :- [s/Int]]
  (forv [byte-val byte-vec]
    (cond
      (= byte-val b64-code-62) b64url-code-62
      (= byte-val b64-code-63) b64url-code-63
      (= byte-val b64-code-pad) b64url-code-pad
      :default byte-val)))

(s/defn ^:no-doc b64url->b64 :- [s/Int]
  "Converts a vector of byte values from base64url -> base64 encoding."
  [byte-vec :- [s/Int]]
  (forv [byte-val byte-vec]
    (cond
      (= byte-val b64url-code-62) b64-code-62
      (= byte-val b64url-code-63) b64-code-63
      (= byte-val b64url-code-pad) b64-code-pad
      :default byte-val)))

(defn base64url-encoder []
  (if-java-1-8-plus
    (java.util.Base64/getUrlEncoder)
    (throw (RuntimeException. "Unimplemented prior to Java 1.8: "))))

(defn base64url-decoder []
  (if-java-1-8-plus
    (java.util.Base64/getUrlDecoder)
    (throw (RuntimeException. "Unimplemented prior to Java 1.8: "))))

(defn encode-byte-array
  "Encodes a byte array into base64url, returning a new byte array."
  [byte-arr]
  (types/byte-array? byte-arr)
  (.encode (base64url-encoder) byte-arr))

(defn decode-byte-array
  "Decodes a byte array from base64url, returning a new byte array."
  [byte-arr]
  (types/byte-array? byte-arr)
  (.decode (base64url-decoder) byte-arr))

(s/defn encode-byte-array->str :- s/Str
  "Encodes a byte array into base64url, returning a String."
  [byte-arr]
  (types/byte-array? byte-arr)
  (.encodeToString (base64url-encoder) byte-arr))

(s/defn decode-str->byte-array
  "Decodes a base64url encoded String, returning a byte array"
  [code-str :- s/Str]
  (.decode (base64url-decoder) code-str))

(s/defn encode-bytes->str :- s/Str ; #todo need test
  "Encodes a vector of byte values into base64url, returning a String."
  [src-bytes :- [s/Int]]
  (encode-byte-array->str (byte-array src-bytes)))

(s/defn decode-str->bytes :- [s/Int] ; #todo need test
  "Decodes a base64url encoded String, returning a vector of byte values"
  [code-str :- s/Str]
  (vec (decode-str->byte-array code-str)))

(s/defn encode-str :- s/Str
  "Encodes a String into base64url, returning a String."
  [src-str :- s/Str]
  (-> src-str str/str->byte-array encode-byte-array->str))

(s/defn decode-str :- s/Str
  "Decodes a base64url encoded String, returning a String."
  [code-str :- s/Str]
  (-> code-str decode-str->byte-array str/byte-array->str))
