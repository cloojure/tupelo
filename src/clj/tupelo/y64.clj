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


(defn byte-array-encode-native
  "Encodes a byte array into Y64, returning a new byte array."
  [data-bytes]
  (types/byte-array? data-bytes) 
  (-> data-bytes b64/byte-array-encode-native b64->y64))

(defn byte-array-decode-native
  "Decodes a byte array from Y64, returning a new byte array."
  [code-bytes]
  (types/byte-array? code-bytes) 
  (-> code-bytes y64->b64 b64/byte-array-decode-native))

(s/defn byte-array-encode :- s/Str
  "Encodes a byte array into Y64, returning a String."
  [data-bytes]
  (types/byte-array? data-bytes) 
  (-> data-bytes byte-array-encode-native types/bytes->str))

(s/defn byte-array-decode
  "Decodes a Y64 encoded String, returning a byte array"
  [code-str :- s/Str]
  (-> code-str types/str->bytes byte-array-decode-native))

(s/defn bytes-encode :- s/Str ; #todo need test
  "Encodes a vector of byte values into Y64, returning a String."
  [byte-vec :- [s/Int]]
  (byte-array-encode (byte-array byte-vec)))

(s/defn bytes-decode :- [s/Int] ; #todo need test
  "Decodes a Y64 encoded String, returning a vector of byte values"
  [code-str :- s/Str]
  (vec (byte-array-decode code-str)))

(s/defn string-encode :- s/Str
  "Encodes a String into Y64, returning a String."
  [data-str :- s/Str]
  (-> data-str types/str->bytes byte-array-encode))

(s/defn string-decode :- s/Str
  "Decodes a Y64 encoded String, returning a String."
  [code-str :- s/Str]
  (-> code-str byte-array-decode types/bytes->str))

