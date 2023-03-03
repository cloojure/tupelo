(ns tupelo.cuid
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.cuid.prng :as prng]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk])
  (:import
    [java.util Random]))

;-----------------------------------------------------------------------------
; new:  CUID - Cipher Unique ID
;
; BitScrambler
;  - idx->int
;  - idx->hex
;  - next-int
;  - next-hex

; #todo add fns:  next-biginteger, next-str-dec, next-str-hex

;-----------------------------------------------------------------------------
(def ^:no-doc min-bits 4) ; NOTE! IMPORTANT! 4 bits minimum due to shuffle step
(def ^:no-doc max-bits 128) ; No real upper limit.  Just process in blocks if desired.

;-----------------------------------------------------------------------------

(s/defn new-ctx :- tsk/KeyMap
  "Creates a new Cryptographic Unique ID (CUID) context map. Usage:

        (new-ctx <params-map>)

  where <params-map> is of the form:

        {:num-bits     <long>  ; REQUIRED:  (minimum: 4): input/output integers in [0..2^n)
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
    (when-not (<= min-bits num-bits max-bits)
      (throw (ex-info "num-bits out of range " (vals->map num-bits min-bits max-bits)))))
  (let [params-default {:num-rounds    7
                        :shuffle-bits? false}
        ctx            (prng/new-ctx (glue params-default opts))]
    ctx))

;-----------------------------------------------------------------------------
; Timing {:num-rounds 7  :shuffle-bits? false}
;   32 bits:  12 usec/call
;   64 bits:  12 usec/call
;  128 bits:  12 usec/call
(s/defn idx->cuid :- BigInteger
  "Given a context map and an index value [0..2^N), returns the corresponding
  CUID value (Cryptographic Unique ID), also in [0..2^N). CUID values are guaranteed
  to be unique for each index and pseudo-random within the interval [0..2^N)."
  [ctx :- tsk/KeyMap
   ival :- s/Int]
  ; (prof/with-timer-accum :idx->cuid)
  (prng/randomize ctx ival))

(s/defn cuid->idx :- BigInteger
  "Given a context map and a CUID value (Cryptographic Unique ID) in [0..2^N),
   returns the corresponding index value, also in [0..2^N)."
  [ctx :- tsk/KeyMap
   cuid :- s/Int]
  ; (prof/with-timer-accum :cuid->idx)
  (prng/derandomize ctx cuid))
