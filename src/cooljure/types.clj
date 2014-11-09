(ns cooljure.types
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

