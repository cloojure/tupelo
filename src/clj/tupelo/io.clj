;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.io
  "Convert to/from traditional base64 encoding."
  (:use tupelo.core)
  (:refer-clojure :exclude [read-string])
  (:require
    [clojure.string :as str]
    [schema.core :as s])
  (:import [java.nio ByteBuffer]
           [java.io File DataInputStream DataOutputStream InputStream OutputStream]))

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


(s/defn create-temp-file :- java.io.File
  "Given a unique ID string (e.g. 'my.dummy.file'), returns a java File object
  for a temporary that will be deleted upon JVM exit."
  [id-str :- s/Str]
  (let [tmp-file (File/createTempFile id-str nil)]
    (.deleteOnExit tmp-file)
    tmp-file ))


(s/defn read-bytes  ; #todo type?
  "Reads N bytes from the DataInputStream and returns them in a byte array."
  [N :- s/Int
   input-stream :- InputStream]
  (let [bytarr (byte-array N)]
    (.read (validate input-stream? input-stream) bytarr)
    bytarr))

(s/defn write-string-bytes
  "Writes the an ASCII string as bytes to a DataInputStream."
  [dos :- DataOutputStream
   str-val :- s/Str]
  (spyx (type dos))
  (.writeBytes (validate data-output-stream? dos) str-val))

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
  (long (.readShort (validate data-input-stream? dis))))

(s/defn read-int :- s/Int    ; #todo need test
  "Reads 4 bytes (signed) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readInt (validate data-input-stream? dis))))

(s/defn read-long :- s/Int    ; #todo need test
  "Reads 8 bytes (signed) from the data-input-stream"
  [dis :- DataInputStream]
  (long (.readLong (validate data-input-stream? dis))))

;---------------------------------------------------------------------------------------------------
; #todo move interval stuff -> misc or math

; "Defines a half-open interval"
(defrecord Interval ; #todo report defrecord "resolve" to Cursive
  [lower-bound upper-bound]) ; #todo report to Cursive

(s/defn interval-contains? :- s/Bool
  "Returns true if val fits within an Interval."
  [itvl :- Interval
   val :- s/Num]
  (and (<= (:lower-bound itvl) val)
    (<  val (:upper-bound itvl))))

(def interval-byte               (->Interval Byte/MIN_VALUE Byte/MAX_VALUE)) ; #todo "resolve" report to Cursive
(def interval-byte-unsigned      (->Interval 0 256))
(def interval-short              (->Interval Short/MIN_VALUE Short/MAX_VALUE))
(def interval-short-unsigned     (->Interval 0 65536))
(def interval-integer            (->Interval Integer/MIN_VALUE Integer/MAX_VALUE))
(def interval-long               (->Interval Long/MIN_VALUE Long/MAX_VALUE))

(s/defn within-interval-byte? :- s/Bool
  "Returns true if val fits within legal range for a byte (signed)."
  [val :- s/Int]
  (interval-contains? interval-byte val))

(s/defn within-interval-byte-unsigned? :- s/Bool
  "Returns true if val fits within legal range for a byte (unsigned)."
  [val :- s/Int]
  (interval-contains? interval-byte-unsigned val))

(s/defn within-interval-short? :- s/Bool
  "Returns true if val fits within legal range for a short (signed)."
  [val :- s/Int]
  (interval-contains? interval-short val))

(s/defn within-interval-short-unsigned? :- s/Bool
  "Returns true if val fits within legal range for a short (unsigned)."
  [val :- s/Int]
  (interval-contains? interval-short-unsigned val))

(s/defn within-interval-integer? :- s/Bool
  "Returns true if val fits within legal range for a integer (signed)."
  [val :- s/Int]
  (interval-contains? interval-integer val))

(s/defn within-interval-long? :- s/Bool
  "Returns true if val fits within legal range for a long (signed)."
  [val :- s/Int]
  (interval-contains? interval-long val))

;---------------------------------------------------------------------------------------------------
(s/defn write-byte :- s/Int    ; #todo need test
  "Writes 1 byte (signed) to a DataOutputStream."
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeByte (validate data-output-stream? dos)
    (validate within-interval-byte? val))
  val)

(s/defn write-byte-unsigned :- s/Int   ; #todo need test
  "Writes 1 byte (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeByte (validate data-output-stream? dos)
    (validate within-interval-byte-unsigned? val))
  val)

(s/defn write-short :- s/Int    ; #todo need test
  "Writes 2 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeShort (validate data-output-stream? dos)
    (validate within-interval-short? val)))

(s/defn write-short-unsigned :- s/Int    ; #todo need test
  "Writes 2 bytes (unsigned) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeShort (validate data-output-stream? dos)
    (validate within-interval-short-unsigned? val)))

(s/defn write-int :- s/Int    ; #todo need test
  "Writes 4 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeInt (validate data-output-stream? dos)
    (validate within-interval-integer? val))
  val)

(s/defn write-long :- s/Int    ; #todo need test
  "Writes 8 bytes (signed) to a DataOutputStream"
  [dos :- DataOutputStream
   val :- s/Int]
  (.writeLong (validate data-output-stream? dos)
    (validate within-interval-long? val))
  val)























