;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.y64
  "Convert to/from the URL-safe Y64 encoding.  Y64 is a URL-safe variant of Base64
  encoding created by Yahoo (YUI library) which replaces URL-problematic chars 
  with URL-safe substitutes. The specific substitutions are:
      +  ->  .   (plus   -> period)
      /  ->  _   (slash  -> underscore)
      =  ->  -   (equals -> hyphen)
  For more information, please see:
    http://en.wikipedia.org/wiki/Base64  
    http://www.yuiblog.com/blog/2010/07/06/in-the-yui-3-gallery-base64-and-y64-encoding/
  "
  (:use tupelo.core)
  (:require
    [tupelo.base64 :as b64]
    [tupelo.string :as str]
    [tupelo.types :as types]
    [schema.core :as s]))

(def encoding-char-set
  "A set of chars used for the Y64 encoding (incl. padding char)"
  (set (glue
         (chars-thru \a \z)
         (chars-thru \A \Z)
         (chars-thru \0 \9)
         [\. \_ \-])))

(def ^:private b64-code-62  (byte \+))
(def ^:private b64-code-63  (byte \/ ))
(def ^:private b64-code-pad (byte \= ))

(def ^:private y64-code-62  (byte \. ))
(def ^:private y64-code-63  (byte \_ ))
(def ^:private y64-code-pad (byte \- ))


(defn- b64->y64
  "Converts a byte array from base64 -> Y64 encoding."
  [code-bytes]
  (types/byte-array? code-bytes) 
  (byte-array 
    (for [byte-val code-bytes]
      (cond  
        (= byte-val b64-code-62)    y64-code-62
        (= byte-val b64-code-63)    y64-code-63
        (= byte-val b64-code-pad)   y64-code-pad
        :default                    byte-val))))

(defn- y64->b64
  "Converts a byte array from Y64 -> base64 encoding."
  [code-bytes]
  (types/byte-array? code-bytes) 
  (byte-array 
    (for [byte-val code-bytes]
      (cond  
        (= byte-val y64-code-62)    b64-code-62
        (= byte-val y64-code-63)    b64-code-63
        (= byte-val y64-code-pad)   b64-code-pad
        :default byte-val))))


(defn encode-byte-array
  "Encodes a byte array into Y64, returning a new byte array."
  [byte-arr]
  (types/byte-array? byte-arr)
  (-> byte-arr b64/encode-byte-array b64->y64))

(defn decode-byte-array
  "Decodes a byte array from Y64, returning a new byte array."
  [byte-arr]
  (types/byte-array? byte-arr)
  (-> byte-arr y64->b64 b64/decode-byte-array))

(s/defn encode-byte-array->str :- s/Str
  "Encodes a byte array into Y64, returning a String."
  [byte-arr]
  (types/byte-array? byte-arr)
  (-> byte-arr encode-byte-array str/byte-array->str))

(s/defn decode-str->byte-array
  "Decodes a Y64 encoded String, returning a byte array"
  [code-str :- s/Str]
  (-> code-str str/str->byte-array decode-byte-array))

(s/defn encode-bytes->str :- s/Str ; #todo need test
  "Encodes a vector of byte values into Y64, returning a String."
  [src-bytes :- [s/Int]]
  (encode-byte-array->str (byte-array src-bytes)))

(s/defn decode-str->bytes :- [s/Int] ; #todo need test
  "Decodes a Y64 encoded String, returning a vector of byte values"
  [code-str :- s/Str]
  (vec (decode-str->byte-array code-str)))

(s/defn encode-str :- s/Str
  "Encodes a String into Y64, returning a String."
  [src-str :- s/Str]
  (-> src-str str/str->byte-array encode-byte-array->str))

(s/defn decode-str :- s/Str
  "Decodes a Y64 encoded String, returning a String."
  [code-str :- s/Str]
  (-> code-str decode-str->byte-array str/byte-array->str))

