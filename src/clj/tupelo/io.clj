;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.io
  "Convert to/from traditional base64 encoding."
  (:use tupelo.core)
  (:require
    [clojure.string :as str]
    [schema.core :as s])
  (:import [java.math BigInteger]
           [java.nio ByteBuffer]
           [java.io File]))

(defn byte-array-glue
  "Reads 4 bytes from the InputStream (big-endian) and parses them (unsigned) into a Long."
  [& byte-arrays]
  (let [total-len   (apply + (mapv count byte-arrays))
        byte-buffer (ByteBuffer/allocate total-len)]
    (doseq [byte-array-curr byte-arrays]
      (.put byte-buffer byte-array-curr))
    (.array byte-buffer)))

(defn take-bytes
  "Reads N bytes from the InputStream and returns them in a byte array."
  [N input-stream]
  (let [bytarr (byte-array N)]
    (.read input-stream bytarr)
    bytarr))

(defn take-str
  "Reads N bytes from the InputStream and returns them as a String."
  [N input-stream]
  (String. (take-bytes N input-stream)))

(defn take-uint8   ; #todo need test
  "Reads 1 byte from the InputStream (big-endian) and parses them (unsigned) into a Integer."
  [input-stream]
  (let [zeros3 (byte-array 3)
        bytes1 (take-bytes 1 input-stream)
        buffer (ByteBuffer/wrap (byte-array-glue zeros3 bytes1))
        result (.getInt buffer)]
    result))

(defn take-int8    ; #todo need test
  "Reads 1 byte from the InputStream (big-endian) and parses them (signed) into an Byte."
  [input-stream]
  (let [bytes1 (take-bytes 1 input-stream)
        result (Byte. (aget bytes1 0))]
    result))

(defn take-uint16   ; #todo need test
  "Reads 2 bytes from the InputStream (big-endian) and parses them (unsigned) into a Integer."
  [input-stream]
  (let [zeros2 (byte-array 2)
        bytes2 (take-bytes 2 input-stream)
        buffer (ByteBuffer/wrap (byte-array-glue zeros2 bytes2))
        result (.getInt buffer)]
    result))

(defn take-int16    ; #todo need test
  "Reads 2 bytes from the InputStream (big-endian) and parses them (signed) into an Short."
  [input-stream]
  (let [bytes2 (take-bytes 2 input-stream)
        buffer (ByteBuffer/wrap bytes2)
        result (.getShort buffer)]
    result))

(defn take-uint32   ; #todo need test
  "Reads 4 bytes from the InputStream (big-endian) and parses them (unsigned) into a Long."
  [input-stream]
  (let [zeros4 (byte-array 4)
        bytes4 (take-bytes 4 input-stream)
        buffer (ByteBuffer/wrap (byte-array-glue zeros4 bytes4))
        result (.getLong buffer)]
    result))

(defn take-int32    ; #todo need test
  "Reads 4 bytes from the InputStream (big-endian) and parses them (signed) into an Integer."
  [input-stream]
  (let [bytes4 (take-bytes 4 input-stream)
        buffer (ByteBuffer/wrap bytes4)
        result (.getInt buffer)]
    result))

(defn take-uint64   ; #todo need test
  "Reads 8 bytes from the InputStream (big-endian) and parses them (unsigned) into a BigInteger."
  [input-stream]
  (let [zeros4 (byte-array 4)
        bytes8 (take-bytes 8 input-stream)
        buffer (ByteBuffer/wrap (byte-array-glue zeros4 bytes8))
        result (BigInteger. (.array buffer))]
    result))

(defn take-int64    ; #todo need test
  "Reads 8 bytes from the InputStream (big-endian) and parses them (signed) into an Long."
  [input-stream]
  (let [bytes8 (take-bytes 8 input-stream)
        buffer (ByteBuffer/wrap bytes8)
        result (.getLong buffer)]
    result))

(defn write-string-bytes
  "Writes the an ASCII string as bytes on output-stream"
  [output-stream str-val]
  (.writeBytes output-stream str-val))

(s/defn create-temp-file :- java.io.File
  "Given a unique ID string (e.g. 'my.dummy.file'), returns a java File object
  for a temporary that will be deleted upon JVM exit."
  [id-str :- s/Str]
  (let [tmp-file (File/createTempFile id-str nil)]
    (.deleteOnExit tmp-file)
    tmp-file ))


































