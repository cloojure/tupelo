;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.types
  "Type conversion and detection."
  (:refer-clojure :exclude [integer? float? double?])
  (:require
    [clojure.string :as str]
    [schema.core :as s]))

(def ^:const UTF-8-Charset-Name "UTF-8")

; An instance of the java.lang.Class<XXXX[]> (e.g. java.lang.Class<Byte[]>). 
(def ^:private  class-boolean-array (.getClass (boolean-array   0)))
(def ^:private  class-byte-array    (.getClass (byte-array      0)))
(def ^:private  class-char-array    (.getClass (char-array      0)))
(def ^:private  class-double-array  (.getClass (double-array    0)))
(def ^:private  class-float-array   (.getClass (float-array     0)))
(def ^:private  class-int-array     (.getClass (int-array       0)))
(def ^:private  class-long-array    (.getClass (long-array      0)))
(def ^:private  class-object-array  (.getClass (object-array    0)))
(def ^:private  class-short-array   (.getClass (short-array     0)))


; As of Clojure 1.9.0-alpha5, boolean? is native to clojure
#_(defn ^{:deprecated "1.9.0-alpha5" } boolean?
  "Returns true is the arg is a Boolean (true or false). Else returns false."
  [arg]
  (or (= true arg) (= false arg)))

; #todo change to using `instance?`
(defn boolean-array?
  "Returns true is the arg is a boolean array, else false."
  [arg]
  (= class-boolean-array (.getClass arg)))

(defn byte-array?
  "Returns true is the arg is a byte array, else false."
  [arg]
  (= class-byte-array (.getClass arg)))

(defn char-array?
  "Returns true is the arg is a char array, else false."
  [arg]
  (= class-char-array (.getClass arg)))

(defn double-array?
  "Returns true is the arg is a double array, else false."
  [arg]
  (= class-double-array (.getClass arg)))

(defn float-array?
  "Returns true is the arg is a float array, else false."
  [arg]
  (= class-float-array (.getClass arg)))

(defn int-array?
  "Returns true is the arg is a int array, else false."
  [arg]
  (= class-int-array (.getClass arg)))

(defn long-array?
  "Returns true is the arg is a long array, else false."
  [arg]
  (= class-long-array (.getClass arg)))

(defn object-array?
  "Returns true is the arg is a object array, else false."
  [arg]
  (= class-object-array (.getClass arg)))

(defn short-array?
  "Returns true is the arg is a short array, else false."
  [arg]
  (= class-short-array (.getClass arg)))

;-----------------------------------------------------------------------------
(defn byte?
  "Returns true is the arg is a Byte, else false."
  [arg]
  (instance? java.lang.Byte arg))

(defn short?
  "Returns true is the arg is a Short, else false."
  [arg]
  (instance? java.lang.Short arg))

(defn integer?
  "Returns true is the arg is a Integer, else false."
  [arg]
  (instance? java.lang.Integer arg))

(defn long?
  "Returns true is the arg is a Long, else false."
  [arg]
  (instance? java.lang.Long arg))

(defn float?
  "Returns true is the arg is a Float, else false."
  [arg]
  (instance? java.lang.Float arg))

(defn double?
  "Returns true is the arg is a Double, else false."
  [arg]
  (instance? java.lang.Double arg))

(defn character?
  "Returns true is the arg is a Character, else false."
  [arg]
  (instance? java.lang.Character arg))

;-----------------------------------------------------------------------------
(defn str->byte-array  ; #todo move to tupelo.misc
  "Converts a String to a byte array using the UTF-8 Charset"
  [^String arg]
  {:pre  [ (string? arg) ] 
   :post [ (byte-array? %) ] }
  [arg]
  (.getBytes arg UTF-8-Charset-Name))

(defn byte-array->str  ; #todo move to tupelo.misc
  "Converts a byte array to a String using the UTF-8 Charset"
  [arg]
  {:pre  [ (byte-array? arg) ] 
   :post [ (string? %) ] }
  (String. arg UTF-8-Charset-Name))

;---------------------------------------------------------------------------------------------------

(def BYTE_UNSIGNED_MIN_VALUE 0)
(def BYTE_UNSIGNED_MAX_VALUE (-> (biginteger 2)
                               (.pow 8)
                               (long)
                               (dec)))

(def SHORT_UNSIGNED_MIN_VALUE 0)
(def SHORT_UNSIGNED_MAX_VALUE (-> (biginteger 2)
                                (.pow 16)
                                (long)
                                (dec)))

(def INTEGER_UNSIGNED_MIN_VALUE 0)
(def INTEGER_UNSIGNED_MAX_VALUE (-> (biginteger 2)
                                  (.pow 32)
                                  (long)
                                  (dec)))

(def LONG_UNSIGNED_MIN_VALUE 0)
(def LONG_UNSIGNED_MAX_VALUE (-> (biginteger 2)
                               (.pow 64)
                               (.subtract (biginteger 1))))

(defrecord ^:no-doc IntervalClosed ; #todo report defrecord "resolve" to Cursive
  [lower-bound upper-bound]) ; #todo report to Cursive

(s/defn ^:no-doc within-interval-closed? :- s/Bool
  "Returns true if val fits within an IntervalClosed."
  [ic :- IntervalClosed
   val :- s/Num]
  (<= (:lower-bound ic) val (:upper-bound ic)))

(def ^:no-doc interval-closed-byte               (->IntervalClosed Byte/MIN_VALUE Byte/MAX_VALUE)) ; #todo "resolve" report to Cursive
(def ^:no-doc interval-closed-byte-unsigned      (->IntervalClosed BYTE_UNSIGNED_MIN_VALUE BYTE_UNSIGNED_MAX_VALUE))
(def ^:no-doc interval-closed-short              (->IntervalClosed Short/MIN_VALUE Short/MAX_VALUE))
(def ^:no-doc interval-closed-short-unsigned     (->IntervalClosed SHORT_UNSIGNED_MIN_VALUE SHORT_UNSIGNED_MAX_VALUE))
(def ^:no-doc interval-closed-integer            (->IntervalClosed Integer/MIN_VALUE Integer/MAX_VALUE))
(def ^:no-doc interval-closed-integer-unsigned   (->IntervalClosed INTEGER_UNSIGNED_MIN_VALUE INTEGER_UNSIGNED_MAX_VALUE))
(def ^:no-doc interval-closed-long               (->IntervalClosed Long/MIN_VALUE Long/MAX_VALUE))
(def ^:no-doc interval-closed-long-unsigned     (->IntervalClosed LONG_UNSIGNED_MIN_VALUE LONG_UNSIGNED_MAX_VALUE))

(s/defn within-bounds-byte? :- s/Bool
  "Returns true if val fits within legal range for a byte (signed)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-byte val))

(s/defn within-bounds-byte-unsigned? :- s/Bool
  "Returns true if val fits within legal range for a byte (unsigned)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-byte-unsigned val))

(s/defn within-bounds-short? :- s/Bool
  "Returns true if val fits within legal range for a short (signed)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-short val))

(s/defn within-bounds-short-unsigned? :- s/Bool
  "Returns true if val fits within legal range for a short (unsigned)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-short-unsigned val))

(s/defn within-bounds-integer? :- s/Bool
  "Returns true if val fits within legal range for a integer (signed)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-integer val))

(s/defn within-bounds-integer-unsigned? :- s/Bool
  "Returns true if val fits within legal range for a integer (unsigned)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-integer-unsigned val))

(s/defn within-bounds-long? :- s/Bool
  "Returns true if val fits within legal range for a long (signed)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-long val))

(s/defn within-bounds-long-unsigned? :- s/Bool
  "Returns true if val fits within legal range for a long (unsigned)."
  [val :- s/Int]
  (within-interval-closed? interval-closed-long-unsigned val))



