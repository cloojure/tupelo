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
  (:require [clojure.string     :as str]
            [tupelo.base64    :as b64]
            [tupelo.misc      :as misc]
            [tupelo.types     :as types]
            [schema.core        :as s] )
  (:use tupelo.core)
  (:gen-class))

; #awt TODO: convert :pre/:post to Prismatic Schema

(def y64-chars
  "A set of chars used for the Y64 encoding (incl. padding char)"
  (into #{} (flatten [ (misc/char-seq  \a \z)
                       (misc/char-seq  \A \Z)
                       (misc/char-seq  \0 \9) 
                       [\. \_ \-] ] )))

(def ^:private b64-code-62  (byte \+ ))
(def ^:private b64-code-63  (byte \/ ))
(def ^:private b64-code-pad (byte \= ))

(def ^:private y64-code-62  (byte \. ))
(def ^:private y64-code-63  (byte \_ ))
(def ^:private y64-code-pad (byte \- ))


(defn- b64-bytes->y64-bytes
  "Converts a byte array from Base64 -> Y64 encoding."
  [^bytes coded-bytes]
  (byte-array 
    (for [byte-val coded-bytes]
      (cond  
        (= byte-val b64-code-62)    y64-code-62
        (= byte-val b64-code-63)    y64-code-63
        (= byte-val b64-code-pad)   y64-code-pad
        :default                    byte-val))))

(defn- y64-bytes->b64-bytes
  "Converts a byte array from Y64 -> Base64 encoding."
  [^bytes coded-bytes]
  (byte-array 
    (for [byte-val coded-bytes]
      (cond  
        (= byte-val y64-code-62)    b64-code-62
        (= byte-val y64-code-63)    b64-code-63
        (= byte-val y64-code-pad)   b64-code-pad
        :default byte-val))))


(defn encode-bytes
  "Encodes a byte array into Y64, returning a new byte array."
  [bytes-in]
  {:pre [(types/byte-array? bytes-in)] }
  (-> bytes-in b64/encode-bytes b64-bytes->y64-bytes))

(defn decode-bytes
  "Decodes a byte array from Y64, returning a new byte array."
  [bytes-in]
  {:pre [(types/byte-array? bytes-in)] }
  (-> bytes-in y64-bytes->b64-bytes b64/decode-bytes))


(defn encode-bytes->str
  "Encodes a byte array into base-64, returning a String."
  [bytes-in]
  {:pre [(types/byte-array? bytes-in)] }
  (-> bytes-in encode-bytes types/bytes->str))

(defn decode-str->bytes
  "Decodes a base-64 encoded String, returning a byte array"
  [str-in]
  {:pre  [(string? str-in)] }
  (-> str-in types/str->bytes decode-bytes))


(defn encode-str 
  "Encodes a String into base-64, returning a String."
  [^String str-in]
  {:pre  [ (string? str-in) ] }
  (-> str-in types/str->bytes encode-bytes types/bytes->str))

(defn decode-str 
  "Decodes a base-64 encoded String, returning a String."
  [^String str-in]
  {:pre  [(string? str-in)] }
  (-> str-in types/str->bytes decode-bytes types/bytes->str))

