;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.types
  "Type conversion and detection."
  (:require [clojure.string :as str])
  (:gen-class))

(def ^:const UTF-8-Charset-Name "UTF-8")

; An instance of the java.lang.Class<XXXX[]> (e.g. java.lang.Class<Byte[]>). 
(defonce ^:private Class-byte-array         (.getClass (byte-array      0)))
(defonce ^:private Class-boolean-array      (.getClass (boolean-array   0)))
(defonce ^:private Class-char-array         (.getClass (char-array      0)))
(defonce ^:private Class-double-array       (.getClass (double-array    0)))
(defonce ^:private Class-float-array        (.getClass (float-array     0)))
(defonce ^:private Class-int-array          (.getClass (int-array       0)))
(defonce ^:private Class-long-array         (.getClass (long-array      0)))
(defonce ^:private Class-object-array       (.getClass (object-array    0)))
(defonce ^:private Class-short-array        (.getClass (short-array     0)))


(defn boolean?
  "Returns true is the arg is a Boolean (true or false). Else returns false."
  [arg]
  (or (= true arg) (= false arg)))


(defn byte-array?
  "Returns true is the arg is a byte array, else false."
  [arg]
  (= Class-byte-array (.getClass arg)))

(defn boolean-array?
  "Returns true is the arg is a boolean array, else false."
  [arg]
  (= Class-boolean-array (.getClass arg)))

(defn char-array?
  "Returns true is the arg is a char array, else false."
  [arg]
  (= Class-char-array (.getClass arg)))

(defn double-array?
  "Returns true is the arg is a double array, else false."
  [arg]
  (= Class-double-array (.getClass arg)))

(defn float-array?
  "Returns true is the arg is a float array, else false."
  [arg]
  (= Class-float-array (.getClass arg)))

(defn int-array?
  "Returns true is the arg is a int array, else false."
  [arg]
  (= Class-int-array (.getClass arg)))

(defn long-array?
  "Returns true is the arg is a long array, else false."
  [arg]
  (= Class-long-array (.getClass arg)))

(defn object-array?
  "Returns true is the arg is a object array, else false."
  [arg]
  (= Class-object-array (.getClass arg)))

(defn short-array?
  "Returns true is the arg is a short array, else false."
  [arg]
  (= Class-short-array (.getClass arg)))


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

