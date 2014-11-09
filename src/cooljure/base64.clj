(ns cooljure.base64
  (:require [clojure.string :as str]
            [clojure.data.codec.base64 :as clj-b64]
            [cooljure.misc  :as misc]
            [cooljure.types :as types] )
  (:use cooljure.core)
  (:gen-class))

; #awt TODO: convert :pre/:post to Prismatic Schema

(def data-chars
  "A set of Base64 data characters a-z, A-A, 0-9 (not including padding characters like =)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) ] )))

(def base64-chars
  "A set of chars for traditional Base64 symbols, with padding chars [ + \\ = ]"
  (into data-chars [\+ \/ \=] ))
(spy-expr base64-chars)

(def y64-chars
  "A set of chars for the URL-safe Yahoo variant of Base64 symbols, with padding chars [ . _ - ]
   (see YUI library)"
  (into data-chars [\. \_ \-] ))
(spy-expr y64-chars)

(defn encode-bytes->str
  "Encodes a byte array into base-64, returning a String."
  [bytes-in]
  {:pre  [ (types/byte-array? bytes-in) ]
   :post [ (string? %) ] }
  (if (zero? (count bytes-in))
    ""
    (types/bytes->str (clj-b64/encode bytes-in))))

(defn decode-str->bytes
  "Decodes a base-64 encoded String, returning a byte array"
  [^String str-in]
  {:pre  [ (string? str-in) ] 
   :post [ (types/byte-array? %) ] }
  (if (zero? (count str-in))
    (byte-array [])
    (clj-b64/decode (types/str->bytes str-in))))


(defn encode-str 
  "Encodes a String into base-64, returning a String."
  [^String str-in]
  {:pre  [ (string? str-in) ] 
   :post [ (string?   %) ] }
  (encode-bytes->str (types/str->bytes str-in)))

(defn decode-str 
  "Decodes a base-64 encoded String, returning a String."
  [^String str-in]
  {:pre  [ (string? str-in) ] 
   :post [ (string? %) ] }
  (types/bytes->str (decode-str->bytes str-in)))

