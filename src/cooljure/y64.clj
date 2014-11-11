;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.y64
  "A set of functions for encoding/decoding strings and byte-arrays into the Y64 encoding.
  Y64 is a URL-safe variant of Base64 encoding created by Yahoo (YUI library) which
  replaces problematic chars like '/' (slash) and '=' (equals) with URL-safe substitues."
  (:require [clojure.string :as str]
            [clojure.data.codec.base64 :as clj-b64]
            [cooljure.misc  :as misc]
            [cooljure.types :as types] )
  (:use cooljure.core)
  (:gen-class))

; #awt TODO: convert :pre/:post to Prismatic Schema

(def y64-chars
  "A set of chars used for the Y64 encoding (incl. padding char)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) 
                       [\. \_ \-] ] )))

(def b64-code-62  (byte \+ ))
(def b64-code-63  (byte \/ ))
(def b64-code-pad (byte \= ))

(def y64-code-62  (byte \. ))
(def y64-code-63  (byte \_ ))
(def y64-code-pad (byte \- ))

(defn b64-bytes->y64-bytes
  "Converts a byte array from Base64 -> Y64 encoding."
  [coded-bytes]
  (byte-array 
    (for [byte-val coded-bytes]
      (cond  
        (= byte-val b64-code-62)    y64-code-62
        (= byte-val b64-code-63)    y64-code-63
        (= byte-val b64-code-pad)   y64-code-pad
        :default byte-val))))

(defn y64-bytes->b64-bytes
  "Converts a byte array from Y64 -> Base64 encoding."
  [coded-bytes]
  (byte-array 
    (for [byte-val coded-bytes]
      (cond  
        (= byte-val y64-code-62)    b64-code-62
        (= byte-val y64-code-63)    b64-code-63
        (= byte-val y64-code-pad)   b64-code-pad
        :default byte-val))))

(defn encode-bytes
  "Encodes a byte array into Y64, returning a new byte array."
  [^bytes input]
  {:pre [(types/byte-array? input)] }
  ; clojure.data.codec.base64 does not handle case of zero-length input, so we must.
  (if (zero? (alength input))
    (byte-array [])
    (let [b64-bytes (clj-b64/encode input)
          result    (b64-bytes->y64-bytes b64-bytes)]
;     (println \newline 
;       (format "encode-bytes, length input: %d  result %d" (count input) (count result) ))
;     (println "  input  =" (misc/seq->str input))
;     (println "  result =" (misc/seq->str result))
;     (println "  chars  =" (misc/seq->str (map char result)))
      result )))

(defn decode-bytes
  "Decodes a byte array from Y64, returning a new byte array."
  [^bytes input]
  {:pre [(types/byte-array? input)] }
  ; clojure.data.codec.base64 does not handle case of zero-length input, so we must.
  ; #awt todo:  submit PR to fix
  (if (zero? (alength input))
    (byte-array [])
    (let [b64-bytes     (y64-bytes->b64-bytes input)
          result        (clj-b64/decode b64-bytes)]
;     (println \newline 
;       (format "decode-bytes, length input: %d  result %d" (count input) (count result) ))
;     (println "  input  =" (misc/seq->str input))
;     (println "  result =" (misc/seq->str result))
      result )))

(defn encode-bytes->str
  "Encodes a byte array into base-64, returning a String."
  [input]
  (-> input encode-bytes types/bytes->str))

(defn decode-str->bytes
  "Decodes a base-64 encoded String, returning a byte array"
  [input]
  (-> input types/str->bytes decode-bytes))

(defn encode-str 
  "Encodes a String into base-64, returning a String."
  [^String str-in]
  {:pre  [ (string? str-in) ] 
   :post [ (string?   %) ] }
  (-> str-in types/str->bytes encode-bytes types/bytes->str))

(defn decode-str 
  "Decodes a base-64 encoded String, returning a String."
  [^String str-in]
  {:pre  [(string? str-in)] 
   :post [(string? %)] }
  (-> str-in types/str->bytes decode-bytes types/bytes->str))

