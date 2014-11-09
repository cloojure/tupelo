(ns cooljure.base64
  (:require [clojure.string :as str]
            [clojure.data.codec.base64 :as clj-b64]
            [cooljure.types :as types] )
  (:gen-class))

; #awt TODO: convert :pre/:post to Prismatic Schema

(defn encode-bytes->str
  "Encodes a byte array into base-64, returning a String."
  [arg-bytes]
  {:pre  [ (types/byte-array? arg-bytes) ]
   :post [ (string? %) ] }
  (types/bytes->str (clj-b64/encode arg-bytes)))

(defn decode-str->bytes
  "Decodes a base-64 encoded String, returning a byte array"
  [^String arg-str]
  {:pre  [ (string? arg-str) ] 
   :post [ (types/byte-array? %) ] }
  (clj-b64/decode (types/str->bytes arg-str)))


(defn encode-str 
  "Encodes a String into base-64, returning a String."
  [^String arg-str]
  {:pre  [ (string? arg-str) ] 
   :post [ (string?   %) ] }
  (encode-bytes->str (types/str->bytes arg-str)))

(defn decode-str 
  "Decodes a base-64 encoded String, returning a String."
  [^String arg-str]
  {:pre  [ (string? arg-str) ] 
   :post [ (string? %) ] }
  (types/bytes->str (decode-str->bytes arg-str)))

