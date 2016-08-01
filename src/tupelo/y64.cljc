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
  (:require [clojure.string     :as str]
            [tupelo.base64      :as b64]
            [tupelo.misc        :as misc]
            [tupelo.types       :as types]
            [schema.core        :as s] )
  (:gen-class))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs


(def code-chars
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


(defn encode-bytes
  "Encodes a byte array into Y64, returning a new byte array."
  [data-bytes]
  (types/byte-array? data-bytes) 
  (-> data-bytes b64/encode-bytes b64->y64))

(defn decode-bytes
  "Decodes a byte array from Y64, returning a new byte array."
  [code-bytes]
  (types/byte-array? code-bytes) 
  (-> code-bytes y64->b64 b64/decode-bytes))

(s/defn encode-bytes->str :- s/Str
  "Encodes a byte array into Y64, returning a String."
  [data-bytes]
  (types/byte-array? data-bytes) 
  (-> data-bytes encode-bytes types/bytes->str))

(s/defn decode-str->bytes
  "Decodes a Y64 encoded String, returning a byte array"
  [code-str :- s/Str]
  (-> code-str types/str->bytes decode-bytes))

(s/defn encode-str :- s/Str
  "Encodes a String into Y64, returning a String."
  [data-str :- s/Str]
  (-> data-str types/str->bytes encode-bytes->str))

(s/defn decode-str :- s/Str
  "Decodes a Y64 encoded String, returning a String."
  [code-str :- s/Str]
  (-> code-str decode-str->bytes types/bytes->str))

