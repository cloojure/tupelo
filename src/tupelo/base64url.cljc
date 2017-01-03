;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.base64url
  "Convert to/from traditional base64url encoding."
  (:require [clojure.string :as str]
            [tupelo.core    :as t]
            [tupelo.misc    :as misc]
            [tupelo.types   :as types]
            [schema.core    :as s])
  (:gen-class))
(t/refer-tupelo)

; #todo -> code-chars (& other ns's)
(def code-chars
  "A set of chars used for traditional base64url encoding (incl. padding char)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) 
                       [\- \_ \=] ] )))

(defn base64url-encoder []
  (t/if-java-1-8-plus
    (java.util.Base64/getUrlEncoder)
    (throw (RuntimeException. "Unimplemented prior to Java 1.8: "))))

(defn base64url-decoder []
  (t/if-java-1-8-plus
    (java.util.Base64/getUrlDecoder)
    (throw (RuntimeException. "Unimplemented prior to Java 1.8: "))))

(defn encode-bytes
  "Encodes a byte array into base64url, returning a new byte array."
  [data-bytes]
  (types/byte-array? data-bytes)
  (.encode (base64url-encoder) data-bytes))

(defn decode-bytes
  "Decodes a byte array from base64url, returning a new byte array."
  [code-bytes]
  (types/byte-array? code-bytes)
  (.decode (base64url-decoder) code-bytes))

(s/defn encode-bytes->str :- s/Str
  "Encodes a byte array into base64url, returning a String."
  [data-bytes]
  (types/byte-array? data-bytes)
  (.encodeToString (base64url-encoder) data-bytes))

(s/defn decode-str->bytes
  "Decodes a base64url encoded String, returning a byte array"
  [code-str :- s/Str]
  (.decode (base64url-decoder) code-str))

(s/defn encode-str :- s/Str
  "Encodes a String into base64url, returning a String."
  [data-str :- s/Str]
  (-> data-str types/str->bytes encode-bytes->str))

(s/defn decode-str :- s/Str
  "Decodes a base64url encoded String, returning a String."
  [code-str :- s/Str]
  (-> code-str decode-str->bytes types/bytes->str))

