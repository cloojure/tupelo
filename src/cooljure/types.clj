;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.types
  "Type conversion and detection."
  (:require [clojure.string :as str])
  (:gen-class))

; An instance of the java.lang.Class<Byte[]>.  The representation of a byte array's class.
(defonce Class-byte-array
  (Class/forName "[B"))

(defn byte-array?
  "Returns true is the arg is a byte array, else false."
  [arg]
  (= Class-byte-array (.getClass arg)))

(def ^:const UTF-8-Charset-Name "UTF-8")

(defn str->bytes
  "Converts a String to a byte array using the UTF-8 Charset"
  [^String arg]
  {:pre  [ (string? arg) ] 
   :post [ (byte-array? %) ] }
  [arg]
  (.getBytes arg UTF-8-Charset-Name))

(defn bytes->str
  "Converts a byte array to a String using the UTF-8 Charset"
  [arg]
  {:pre  [ (byte-array? arg) ] 
   :post [ (string? %) ] }
  (String. arg UTF-8-Charset-Name))

