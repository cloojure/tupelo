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
    [tupelo.types :as types]
    [schema.core :as s] ))

; #todo -> code-chars (& other ns's)
(def encoding-char-set
  "A set of chars used for traditional base64url encoding (incl. padding char)"
  (set (glue
         (chars-thru \a \z)
         (chars-thru \A \Z)
         (chars-thru \0 \9)
         [\- \_ \=])))


(def ^:private b64-code-62  (byte \+))
(def ^:private b64-code-63  (byte \/ ))
(def ^:private b64-code-pad (byte \= ))

(def ^:private b64url-code-62  (byte \- ))
(def ^:private b64url-code-63  (byte \_ ))
(def ^:private b64url-code-pad (byte \= ))

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

(defn byte-array-encode-native
  "Encodes a byte array into base64url, returning a new byte array."
  [data-bytes]
  (types/byte-array? data-bytes)
  (.encode (base64url-encoder) data-bytes))

(defn byte-array-decode-native
  "Decodes a byte array from base64url, returning a new byte array."
  [code-bytes]
  (types/byte-array? code-bytes)
  (.decode (base64url-decoder) code-bytes))

(s/defn byte-array-encode :- s/Str
  "Encodes a byte array into base64url, returning a String."
  [data-bytes]
  (types/byte-array? data-bytes)
  (.encodeToString (base64url-encoder) data-bytes))

(s/defn byte-array-decode
  "Decodes a base64url encoded String, returning a byte array"
  [code-str :- s/Str]
  (.decode (base64url-decoder) code-str))

(s/defn bytes-encode :- s/Str ; #todo need test
  "Encodes a vector of byte values into base64url, returning a String."
  [byte-vec :- [s/Int]]
  (byte-array-encode (byte-array byte-vec)))

(s/defn bytes-decode :- [s/Int] ; #todo need test
  "Decodes a base64url encoded String, returning a vector of byte values"
  [code-str :- s/Str]
  (vec (byte-array-decode code-str)))

(s/defn string-encode :- s/Str
  "Encodes a String into base64url, returning a String."
  [data-str :- s/Str]
  (-> data-str types/str->bytes byte-array-encode))

(s/defn string-decode :- s/Str
  "Decodes a base64url encoded String, returning a String."
  [code-str :- s/Str]
  (-> code-str byte-array-decode types/bytes->str))
