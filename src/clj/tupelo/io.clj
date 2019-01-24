;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.io
  "Read/Write using DataInputStream/DataOutputStream & InputStream/OutputStream"
  (:use tupelo.core)
  (:refer-clojure :exclude [read-string])
  (:require
    [schema.core :as s]
    [tupelo.types :as types] )
  (:import [java.io File DataInputStream DataOutputStream InputStream OutputStream]))

(def ^:no-doc zeros-4 (byte-array (repeat 4 0)))
(def ^:no-doc zeros-8 (byte-array (repeat 8 0)))

;---------------------------------------------------------------------------------------------------
(s/defn input-stream?
  "Returns true if arg implements java.io.InputStream"
  [arg] (instance? InputStream arg))

(s/defn output-stream?
  "Returns true if arg implements java.io.OutputStream"
  [arg] (instance? OutputStream arg))

(s/defn data-input-stream?
  "Returns true if arg implements java.io.DataInputStream"
  [arg] (instance? DataInputStream arg))

(s/defn data-output-stream?
  "Returns true if arg implements java.io.DataOutputStream"
  [arg] (instance? DataOutputStream arg))

;---------------------------------------------------------------------------------------------------
(s/defn create-temp-file :- java.io.File
  "Given a unique ID string (e.g. 'my.dummy.file'), returns a java File object
  for a temporary that will be deleted upon JVM exit."
  [id-str :- s/Str]
  (let [tmp-file (File/createTempFile id-str nil)]
    (.deleteOnExit tmp-file)
    tmp-file))

(s/defn read-bytes  ; #todo type?
  "Reads N bytes from the DataInputStream and returns them in a byte array."
  [N :- s/Int
   input-stream :- InputStream]
  (let [bytarr (byte-array N)]
    (.read (validate input-stream? input-stream) bytarr)
    bytarr))

(s/defn write-bytes ; #todo type?
  "Writes a byte array to a DataInputStream."
  [out-stream :- OutputStream
   bytarr :- s/Any] ; #todo type
  (.write (validate output-stream? out-stream) bytarr)
  bytarr)

;---------------------------------------------------------------------------------------------------
(s/defn read-string-bytes :- s/Str
  "Reads nchars bytes from the DataInputStream and returns them as a String."
  [nchars :- s/Int
   dis :- DataInputStream]
  (String. (read-bytes nchars (validate data-input-stream? dis))))

(s/defn read-byte :- s/Int    ; #todo need test
  "Reads 1 byte (signed) from the data-input-stream."
  [dis :- DataInputStream]
  (long (.readByte (validate data-input-stream? dis) )))

(s/defn read-byte-unsigned :- s/Int   ; #todo need test
  "Reads 1 byte (unsigned) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readUnsignedByte (validate data-input-stream? dis) )))

(s/defn read-short :- s/Int    ; #todo need test
  "Reads 2 bytes (signed) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readShort (validate data-input-stream? dis))))

(s/defn read-short-unsigned :- s/Int    ; #todo need test
  "Reads 2 bytes (unsigned) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readUnsignedShort (validate data-input-stream? dis))))

(s/defn read-integer :- s/Int    ; #todo need test
  "Reads 4 bytes (signed) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readInt (validate data-input-stream? dis))))

(s/defn read-long :- s/Int    ; #todo need test
  "Reads 8 bytes (signed) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readLong (validate data-input-stream? dis))))

(s/defn read-integer-unsigned :- s/Int ; #todo need test
  "Reads 4 bytes (unsigned) from the data-input-stream"
  [dis :- DataInputStream]
  (long (BigInteger. ^bytes (glue zeros-4
                              (read-bytes 4 (validate data-input-stream? dis))))))

(s/defn read-long-unsigned :- s/Int ; #todo need test
  "Reads 8 bytes (unsigned) from the data-input-stream, returning a BigInteger"
  [dis :- DataInputStream]
  (BigInteger. ^bytes (glue zeros-4
                        (read-bytes 8 (validate data-input-stream? dis)))))

;---------------------------------------------------------------------------------------------------
(s/defn write-string-bytes :- s/Str
  "Writes the an ASCII string as bytes to a DataInputStream."
  [dos :- DataOutputStream
   str-val :- s/Str]
  (.writeBytes (validate data-output-stream? dos) str-val)
  str-val)

(s/defn write-byte :- s/Int    ; #todo need test
  "Writes 1 byte (signed) to a DataOutputStream."
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeByte (validate data-output-stream? dos)
    (validate types/within-bounds-byte? val))
  val)

(s/defn write-byte-unsigned :- s/Int   ; #todo need test
  "Writes 1 byte (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeByte (validate data-output-stream? dos)
    (validate types/within-bounds-byte-unsigned? val))
  val)

(s/defn write-short :- s/Int    ; #todo need test
  "Writes 2 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeShort (validate data-output-stream? dos)
    (validate types/within-bounds-short? val))
  val)

(s/defn write-short-unsigned :- s/Int    ; #todo need test
  "Writes 2 bytes (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeShort (validate data-output-stream? dos)
    (validate types/within-bounds-short-unsigned? val))
  val)

(s/defn write-integer :- s/Int    ; #todo need test
  "Writes 4 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeInt (validate data-output-stream? dos)
    (validate types/within-bounds-integer? val))
  val)

(s/defn write-integer-unsigned :- s/Int    ; #todo need test
  "Writes 4 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeInt (validate data-output-stream? dos)
    (validate types/within-bounds-integer-unsigned? val))
  val)

(s/defn write-long :- s/Int    ; #todo need test
  "Writes 8 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeLong (validate data-output-stream? dos)
    (validate types/within-bounds-long? val))
  val)

(s/defn write-long-unsigned :- s/Int ; #todo need test
  "Writes 4 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (validate types/within-bounds-long-unsigned? val)
  (let [val-biginteger (biginteger val)
        byte-vec       (take-last 8 (glue zeros-8 (.toByteArray ^BigInteger val-biginteger)))
        bytarr         (byte-array byte-vec)]
    (write-bytes dos bytarr))
  val)







