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
  [bit-val :- s/Int]
  (cond
    (= bit-val 0) \0
    (= bit-val 1) \1
    :else (throw (ex-info "Illegal bit value" (vals->map bit-val)))))

(s/defn char->bit :- s/Int
  [bit-char :- Character]
  (cond
    (= bit-char \0) 0
    (= bit-char \1) 1
    :else (throw (ex-info "Illegal bit char" (vals->map bit-char)))))

(s/defn long->bits-unsigned :- [s/Int]
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
  [bits :- [s/Int]]
  (let [binary-chars (mapv bit->char bits)
        binary-str   (str/join binary-chars)
        result       (Long/valueOf binary-str 2)]
    result))

