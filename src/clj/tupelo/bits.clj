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
    ))

(s/defn bit->char :- Character
  "Given an integer bit value 0 or 1, returns a character '0' or '1'"
  [bit-val :- s/Int]
  (cond
    (= bit-val 0) \0
    (= bit-val 1) \1
    :else (throw (ex-info "Illegal bit value" (vals->map bit-val)))))

(s/defn char->bit :- s/Int
  "Given a character '0' or '1', returns an integer bit value 0 or 1"
  [bit-char :- Character]
  (cond
    (= bit-char \0) 0
    (= bit-char \1) 1
    :else (throw (ex-info "Illegal bit char" (vals->map bit-char)))))

(s/defn long->bits-unsigned :- [s/Int]
  "Given a positive integer value, returns a 64-len vector of 0/1 integer values"
  [val :- s/Int]
  (when (neg? val)
    (throw (ex-info "value must be non-negative" (vals->map val))))
  (let [binary-chars  (Long/toBinaryString val)
        bits          (mapv char->bit binary-chars)
        nbits         (count bits)
        leading-zeros (repeat (- 64 nbits) 0)
        result        (glue leading-zeros bits)]
    result))

(s/defn bits-unsigned->long :- s/Int
  "Given a vector of 0/1 integer bit values, returns a signed long value"
  [bits :- [s/Int]]
  (let [binary-chars (prepend \0 (mapv bit->char bits))
        binary-str   (str/join binary-chars)
        result       (Long/valueOf binary-str 2)] ; cannot return neg value unless has '-' sign
    result))

(s/defn bits-unsigned->byte :- s/Int
  "Given a vector of 0/1 integer bit values, returns a signed byte value"
  [bits :- [s/Int]]
  (let [binary-chars (mapv bit->char bits)
        binary-str   (str/join binary-chars)
        result       (Byte/valueOf binary-str 2)] ; cannot return neg value unless has '-' sign
    result))

