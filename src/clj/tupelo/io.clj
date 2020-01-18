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
    [tupelo.types :as types]
    [clojure.java.io :as io])
  (:import [java.io File DataInputStream DataOutputStream InputStream OutputStream]
           [java.nio.file Files Paths Path]
           [java.nio.file.attribute FileAttribute]
           ))

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
  "Given a Path dir unique ID string (e.g. 'my.dummy.file'), returns a java File object
  for a temporary that will be deleted upon JVM exit."
  ([prefix :- s/Str
    suffix :- (s/maybe s/Str)]
   (let [attrs    (into-array FileAttribute [])
         tmp-file (.toFile (Files/createTempFile (str prefix "-") suffix attrs))]
     (.deleteOnExit tmp-file)
     tmp-file))
  ([dir :- Path
    prefix :- s/Str
    suffix :- (s/maybe s/Str)]
   (let [attrs    (into-array FileAttribute [])
         tmp-file (.toFile (Files/createTempFile dir (str prefix "-") suffix attrs))]
     (.deleteOnExit tmp-file)
     tmp-file)))

(s/defn create-temp-directory :- Path
  ([prefix :- s/Str]
   (let [attrs       (into-array FileAttribute [])]
     (Files/createTempDirectory (str prefix "-") attrs)))
  ([dir :- Path
    prefix :- s/Str]
   (let [ attrs       (into-array FileAttribute [])]
     (Files/createTempDirectory dir (str prefix "-") attrs))))

(s/defn delete-directory
  "Recursively deletes a directory and all its contents. Returns count of objects deleted."
  [dir-name :- s/Str]
  (with-nil-default 0
    (let [file-obj  (io/file dir-name)
          num-files (count (file-seq file-obj)) ]
      (when (.exists file-obj)
        (it-> file-obj
          (file-seq it)
          (reverse it)
          (map io/delete-file it)
          (dorun it))
        num-files))))

;---------------------------------------------------------------------------------------------------

(s/defn read-bytes  ; #todo type?
  "Reads N bytes from a DataInputStream, returning a byte array."
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
  "Reads nchars bytes from a DataInputStream, returning a String."
  [nchars :- s/Int
   dis :- DataInputStream]
  (String. (read-bytes nchars (validate data-input-stream? dis))))

(s/defn read-byte :- s/Int    ; #todo need test
  "Reads 1 byte (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readByte (validate data-input-stream? dis) )))

(s/defn read-byte-unsigned :- s/Int   ; #todo need test
  "Reads 1 byte (unsigned) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readUnsignedByte (validate data-input-stream? dis) )))

(s/defn read-short :- s/Int    ; #todo need test
  "Reads 2 bytes (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readShort (validate data-input-stream? dis))))

(s/defn read-short-unsigned :- s/Int    ; #todo need test
  "Reads 2 bytes (unsigned) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readUnsignedShort (validate data-input-stream? dis))))

(s/defn read-integer :- s/Int    ; #todo need test
  "Reads 4 bytes (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readInt (validate data-input-stream? dis))))

(s/defn read-long :- s/Int    ; #todo need test
  "Reads 8 bytes (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readLong (validate data-input-stream? dis))))

(s/defn read-integer-unsigned :- s/Int ; #todo need test
  "Reads 4 bytes (unsigned) from a DataInputStream"
  [dis :- DataInputStream]
  (long (BigInteger. ^bytes (glue zeros-4
                              (read-bytes 4 (validate data-input-stream? dis))))))

(s/defn read-long-unsigned :- s/Int ; #todo need test
  "Reads 8 bytes (unsigned) from a DataInputStream, returning a BigInteger"
  [dis :- DataInputStream]
  (BigInteger. ^bytes (glue zeros-4
                        (read-bytes 8 (validate data-input-stream? dis)))))


; #todo finish
(s/defn read-float :- s/Num    ; #todo need test
  "Reads a 4 byte single-precision floating-point value from a DataInputStream"
  [dis :- DataInputStream]
  (.readFloat (validate data-input-stream? dis)))

(s/defn read-double :- s/Num    ; #todo need test
  "Reads an 8 byte double-precision floating-point value from a DataInputStream"
  [dis :- DataInputStream]
  (.readDouble (validate data-input-stream? dis)))

;---------------------------------------------------------------------------------------------------
(s/defn write-string-bytes :- s/Str
  "Converts an ASCII String to bytes and writes them to a DataInputStream (w/o length header)."
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
  "Writes 8 bytes (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (validate types/within-bounds-long-unsigned? val)
  (let [val-biginteger (biginteger val)
        byte-vec       (take-last 8 (glue zeros-8 (.toByteArray ^BigInteger val-biginteger)))
        bytarr         (byte-array byte-vec)]
    (write-bytes dos bytarr))
  val)


(s/defn write-float :- s/Num    ; #todo need test
  "Writes a 4 byte single-precision floating-point value to a DataInputStream"
  [dos :- DataOutputStream
   val :- s/Num]
  (.writeFloat ^DataOutputStream (validate data-output-stream? dos) val)
  val)

(s/defn write-double :- s/Num    ; #todo need test
  "Writes an 8 byte double-precision floating-point value to a DataInputStream"
  [dos :- DataOutputStream
   val :- s/Num]
  (.writeDouble ^DataOutputStream  (validate data-output-stream? dos) val)
  val)

