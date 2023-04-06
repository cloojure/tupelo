;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.bits
  "Tupelo - Making Clojure even sweeter"
  (:use tupelo.core)
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    ))

(s/defn bitval->bitchar :- Character
  "Given an integer bit value 0 or 1, returns a character '0' or '1'"
  [bit-val :- s/Int]
  (cond
    (= bit-val 0) \0
    (= bit-val 1) \1
    :else (throw (ex-info "Illegal bit value" (vals->map bit-val)))))

  (s/defn bitchar->bitval :- s/Int
  "Given a character '0' or '1', returns an integer bit value 0 or 1"
  [bit-char :- Character]
  (cond
    (= bit-char \0) 0
    (= bit-char \1) 1
    :else (throw (ex-info "Illegal bit char" (vals->map bit-char)))))

;-----------------------------------------------------------------------------
(s/defn long->bits-unsigned :- [s/Int]
  "Given a positive Long value, returns a 64-len vector of 0/1 integer values"
  [long-val :- s/Int]
  (when-not (<= 0 long-val Long/MAX_VALUE)
    (throw (ex-info "value out of range [0..Long/MAX_VALUE]" (vals->map long-val))))
  (let [binary-chars  (Long/toBinaryString long-val)
        bits          (mapv bitchar->bitval binary-chars)
        nbits         (count bits)
        leading-zeros (repeat (- 64 nbits) 0)
        result        (glue leading-zeros bits)]
    result))

(s/defn bits-unsigned->long :- s/Int
  "Given a vector of 0/1 integer bit values, returns a signed Long value"
  [bits :- [s/Int]]
  (let [binary-chars (prepend \0 (mapv bitval->bitchar bits))
        binary-str   (str/join binary-chars)
        result       (Long/valueOf binary-str 2)] ; cannot return neg value unless has '-' sign
    result))

;-----------------------------------------------------------------------------
(s/defn byte->bits-unsigned :- [s/Int]
  "Given a positive Byte value, returns an 8-len vector of 0/1 integer values"
  [byte-val :- s/Int]
  (when-not (<= 0 byte-val Byte/MAX_VALUE)
    (throw (ex-info "value out of range [0..Byte/MAX_VALUE]" (vals->map byte-val))))
  (vec (take-last 8 (long->bits-unsigned byte-val))))

(s/defn bits-unsigned->byte :- s/Int
  "Given a vector of 0/1 integer bit values, returns a signed Byte value"
  [bits :- [s/Int]]
  (let [binary-chars (mapv bitval->bitchar bits)
        binary-str   (str/join binary-chars)
        result       (Byte/valueOf binary-str 2)] ; cannot return neg value unless has '-' sign
    result))

;---------------------------------------------------------------------------------------------------
(s/defn intval->binary-str :- s/Str
  "Converts an integer into a binary String"
  [ival :- s/Int]
  (assert (t/nonneg? ival))
  (.toString (biginteger ival) 2))

(s/defn intval->binary-chars :- [Character]
  "Converts a (positive) BigInteger into a binary char sequence"
  [bi :- s/Int] (vec (intval->binary-str bi)))

;-----------------------------------------------------------------------------
(s/defn intval->bitchars :- tsk/Vec ; #todo => tupelo.math
  [ival :- s/Int
   bits-width :- s/Int]
  (let [bitchars-orig     (intval->binary-chars ival) ; does not include leading zeros
        num-bitchars      (count bitchars-orig)
        num-leading-zeros (- bits-width num-bitchars)
        >>                (assert (t/int-nonneg? num-leading-zeros))
        bitchars-final    (glue (repeat num-leading-zeros \0) bitchars-orig)]
    bitchars-final))

(s/defn intval->bitstr :- s/Str ; #todo => tupelo.math
  [ival :- s/Int
   bits-width :- s/Int]
  (str/join (intval->bitchars ival bits-width)))

(s/defn intval->hex-str :- s/Str
  "Converts a (positive) BigInteger into a hex string of `min-width` chars"
  [ival :- s/Int
   min-width :- s/Int] ; #todo test min-width & all
  (assert (t/nonneg? ival))
  (let [hexchars-orig     (vec (.toString (biginteger ival) 16))
        num-hexchars      (count hexchars-orig)
        num-leading-zeros (max 0 (- min-width num-hexchars)) ; soft overflow
        >>                (assert (t/int-nonneg? num-leading-zeros))
        hexchars-final    (glue (repeat num-leading-zeros \0) hexchars-orig)
        hex-str           (str/join hexchars-final)]
    (assert (<= min-width (count hexchars-final)))
    hex-str))

;---------------------------------------------------------------------------------------------------
(s/defn binary-str->BigInteger :- BigInteger
  "Converts a binary char sequence into a (positive) BigInteger"
  [bin-str :- s/Str] (BigInteger. ^String bin-str 2))

(s/defn binary-chars->BigInteger :- BigInteger
  "Converts a binary char sequence into a (positive) BigInteger"
  [bin-chars :- [Character]] (binary-str->BigInteger (str/join bin-chars)))

; #todo add hex-str->BigInteger
