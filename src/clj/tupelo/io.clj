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
    [clojure.java.io :as io]
    [schema.core :as s]
    [tupelo.types :as types]
    )
  (:import
    [java.io File DataInputStream DataOutputStream InputStream OutputStream]
    [java.nio.file Files Path Paths LinkOption]
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
(s/defn Path? :- s/Bool
  [arg :- s/Any]
  (instance? Path arg))

(s/defn File? :- s/Bool
  [arg :- s/Any]
  (instance? File arg))

(s/defn ->Path :- Path
  "Convert a String or File arg to a Path. Idempotent."
  [arg :- (s/cond-pre File s/Str Path)]
  (cond
    (Path? arg) arg
    (string? arg) (Paths/get arg (into-array String []))
    (File? arg) (.toPath arg)
    :else (throw (ex-info "unknown arg type" {:arg arg :type (type arg)}))))

(s/defn ->File :- File
  "Convert a String arg to a File. Idempotent."
  [arg :- (s/cond-pre Path s/Str)]
  (cond
    (File? arg) arg
    (string? arg) (File. arg)
    (Path? arg) (.toFile arg)
    :else (throw (ex-info "unknown arg type" {:arg arg :type (type arg)}))))

(s/defn file-exists? :- s/Bool
  [arg :- (s/cond-pre s/Str File Path)]
  (Files/exists (->Path arg) (into-array LinkOption [])))

(s/defn delete-file-if-exists :- s/Bool
  [arg :- (s/cond-pre s/Str File Path)]
  (Files/deleteIfExists (->Path arg)))

(s/defn mkdirs ; #todo => tupelo.io  &  need test
  "Creates a directory and all parent dirs."
  [arg :- (s/cond-pre s/Str File Path)]
  (.mkdirs (->File arg)))

(s/defn mkdirs-parent ; #todo => tupelo.io   &  need test
  "Creates all parent dirs of a file."
  [arg :- (s/cond-pre s/Str File Path)]
  (it-> arg
    (->File it)
    (.getParentFile it)
    (.mkdirs it)))

;---------------------------------------------------------------------------------------------------
(s/defn create-temp-file :- File
  "Given a Path dir unique ID string (e.g. 'my.dummy.file'), returns a java File object
  for a temporary that will be deleted upon JVM exit."
  ([prefix :- s/Str
    suffix :- (s/maybe s/Str)]
   (let [attrs    (into-array FileAttribute [])
         tmp-file (.toFile (Files/createTempFile (str prefix "-") suffix attrs))]
     (.deleteOnExit tmp-file)
     tmp-file))
  ([dir :- (s/cond-pre File s/Str Path)
    prefix :- s/Str
    suffix :- (s/maybe s/Str)]
   (let [attrs    (into-array FileAttribute [])
         tmp-file (.toFile (Files/createTempFile (->Path dir) (str prefix "-") suffix attrs))]
     (.deleteOnExit tmp-file)
     tmp-file)))

(s/defn create-temp-directory :- File
  ([prefix :- (s/cond-pre File s/Str Path)]
   (let [tmp-file (.toFile (Files/createTempDirectory (str prefix "-")
                             (into-array FileAttribute [])))]
     (.deleteOnExit tmp-file)
     tmp-file))
  ([dir :- (s/cond-pre File s/Str Path)
    prefix :- s/Str]
   (let [tmp-file (.toFile (Files/createTempDirectory (->Path dir) (str prefix "-") (into-array FileAttribute [])))]
     (.deleteOnExit tmp-file)
     tmp-file)))

(s/defn delete-directory-recursive
  "Recursively deletes a directory and all its contents. Returns count of objects deleted.
  Idempotent in case dir is already deleted."
  [dir-name :- s/Str]
  (with-nil-default 0
    (let [file      (->File dir-name)
          num-files (count (file-seq file))]
      (when (.exists file)
        (it-> file
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
    (.read ^InputStream (validate input-stream? input-stream) bytarr)
    bytarr))

(s/defn write-bytes ; #todo type?
  "Writes a byte array to a DataOutputStream."
  [out-stream :- OutputStream
   bytarr :- s/Any] ; #todo type
  (.write ^OutputStream (validate output-stream? out-stream) bytarr)
  bytarr)

;---------------------------------------------------------------------------------------------------
(s/defn read-string-bytes :- s/Str
  "Reads nchars bytes from a DataInputStream, returning a String."
  [nchars :- s/Int
   dis :- DataInputStream]
  (String. (read-bytes nchars ^DataInputStream (validate data-input-stream? dis))))

(s/defn read-byte :- s/Int    ; #todo need test
  "Reads 1 byte (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readByte ^DataInputStream (validate data-input-stream? dis) )))

(s/defn read-byte-unsigned :- s/Int   ; #todo need test
  "Reads 1 byte (unsigned) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readUnsignedByte ^DataInputStream (validate data-input-stream? dis) )))

(s/defn read-short :- s/Int    ; #todo need test
  "Reads 2 bytes (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readShort ^DataInputStream (validate data-input-stream? dis))))

(s/defn read-short-unsigned :- s/Int    ; #todo need test
  "Reads 2 bytes (unsigned) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readUnsignedShort ^DataInputStream (validate data-input-stream? dis))))

(s/defn read-integer :- s/Int    ; #todo need test
  "Reads 4 bytes (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readInt ^DataInputStream (validate data-input-stream? dis))))

(s/defn read-long :- s/Int    ; #todo need test
  "Reads 8 bytes (signed) from a DataInputStream"
  [dis :- DataInputStream]
  (long (.readLong ^DataInputStream (validate data-input-stream? dis))))

(s/defn read-integer-unsigned :- s/Int ; #todo need test
  "Reads 4 bytes (unsigned) from a DataInputStream"
  [dis :- DataInputStream]
  (long (BigInteger. ^bytes (glue zeros-4
                              (read-bytes 4 ^DataInputStream (validate data-input-stream? dis))))))

(s/defn read-long-unsigned :- s/Int ; #todo need test
  "Reads 8 bytes (unsigned) from a DataInputStream, returning a BigInteger"
  [dis :- DataInputStream]
  (BigInteger. ^bytes (glue zeros-4
                        (read-bytes 8 ^DataInputStream (validate data-input-stream? dis)))))

;---------------------------------------------------------------------------------------------------
(s/defn write-string-bytes :- s/Str
  "Converts an ASCII String to bytes and writes them to a DataInputStream (w/o length header)."
  [dos :- DataOutputStream
   str-val :- s/Str]
  (.writeBytes ^DataOutputStream (validate data-output-stream? dos) str-val)
  str-val)

(s/defn write-byte :- s/Int    ; #todo need test
  "Writes 1 byte (signed) to a DataOutputStream."
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeByte ^DataOutputStream (validate data-output-stream? dos)
    (validate types/within-bounds-byte? val))
  val)

(s/defn write-byte-unsigned :- s/Int   ; #todo need test
  "Writes 1 byte (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeByte ^DataOutputStream (validate data-output-stream? dos)
    (validate types/within-bounds-byte-unsigned? val))
  val)

(s/defn write-short :- s/Int    ; #todo need test
  "Writes 2 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeShort ^DataOutputStream (validate data-output-stream? dos)
    (validate types/within-bounds-short? val))
  val)

(s/defn write-short-unsigned :- s/Int    ; #todo need test
  "Writes 2 bytes (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeShort ^DataOutputStream (validate data-output-stream? dos)
    (validate types/within-bounds-short-unsigned? val))
  val)

(s/defn write-integer :- s/Int    ; #todo need test
  "Writes 4 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeInt ^DataOutputStream (validate data-output-stream? dos)
    (validate types/within-bounds-integer? val))
  val)

(s/defn write-integer-unsigned :- s/Int    ; #todo need test
  "Writes 4 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (validate types/within-bounds-integer-unsigned? val)
  (let [val-biginteger (biginteger val)
        byte-vec       (take-last 4 (glue zeros-8 (.toByteArray ^BigInteger val-biginteger)))
        bytarr         (byte-array byte-vec)]
    (write-bytes dos bytarr))
  val)

(s/defn write-long :- s/Int    ; #todo need test
  "Writes 8 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeLong ^DataOutputStream (validate data-output-stream? dos)
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

;---------------------------------------------------------------------------------------------------
(s/defn read-float :- s/Num    ; #todo need test
  "Reads a 4 byte single-precision floating-point value from a DataInputStream"
  [dis :- DataInputStream]
  (.readFloat ^DataInputStream (validate data-input-stream? dis)))

(s/defn read-double :- s/Num    ; #todo need test
  "Reads an 8 byte double-precision floating-point value from a DataInputStream"
  [dis :- DataInputStream]
  (.readDouble ^DataInputStream (validate data-input-stream? dis)))

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

