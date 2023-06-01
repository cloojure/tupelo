(ns tupelo.iuid
  "Given a user-selected number of bits (currently [4..128]), creates a
   reversible mapping from integer index to a Unique ID (UID). Both
   the index and the UID are in the range of [0..N), where N = 2^num-bits. "
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.iuid.prng :as prng]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    ))

;-----------------------------------------------------------------------------
; new:  IUID - Indexed Unique ID
;
; BitScrambler
;  - idx->int
;  - idx->hex
;  - next-int
;  - next-hex

; #todo add fns:  next-biginteger, next-str-dec, next-str-hex

;-----------------------------------------------------------------------------
(def ^:no-doc min-bits 4) ; NOTE! IMPORTANT! 4 bits minimum due to shuffle step
(def ^:no-doc max-bits 128) ; No real upper limit, but don't need any more for a IUID

;-----------------------------------------------------------------------------

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new Indexed Unique ID (IUID) context map. Usage:

        (new-ctx <params-map>)

   where <params-map> is of the form:

        {:num-bits     <long>  ; REQUIRED:  input/output integers in range [0..2^n) (min=4  max=128):
         :rand-seed    <long>  ; optional:  PRNG seed (default: randomized)
         :num-rounds   <long>  ; optional:  positive int (default: 7)
        } "
  [opts :- tsk/KeyMap]
  (s/validate {:num-bits                    s/Int
               (s/optional-key :rand-seed)  s/Int
               (s/optional-key :num-rounds) s/Int
               s/Any                        s/Any}
    opts)

  (let [num-bits (grab :num-bits opts)]
    (assert-info  (<= min-bits num-bits max-bits)
      "num-bits out of range " (vals->map num-bits min-bits max-bits)))
  (let [params-default {:num-rounds    7
                        :shuffle-bits? false}
        ctx            (prng/new-ctx (glue params-default opts))]
    ctx))

;-----------------------------------------------------------------------------
; Timing {:num-rounds 7  :shuffle-bits? false}
;   32 bits:  12 usec/call
;   64 bits:  12 usec/call
;  128 bits:  12 usec/call
(s/defn idx->uid :- BigInteger
  "Given a context map and an index value [0..2^N), returns the corresponding
  UID value (Unique ID), also in [0..2^N). UID values are guaranteed
  to be unique for each index and pseudo-random within the interval [0..2^N).
  The context map specifies N as the value of the `:num-bits` key. "
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  ; (prof/with-timer-accum :idx->uid)
  (prng/randomize ctx ival))

(s/defn uid->idx :- BigInteger
  "Given a context map and a UID value (Unique ID) in [0..2^N),
   returns the corresponding index value, also in [0..2^N).
  The context map specifies N as the value of the `:num-bits` key.   "
  [ctx :- tsk/KeyMap
   uid :- s/Int]
  ; (prof/with-timer-accum :cuid->idx)
  (prng/derandomize ctx uid))
