(ns tst.tupelo.cuid.prng
  (:use tupelo.cuid.prng tupelo.core tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [criterium.core :as crit]
    [schema.core :as s]
    [tupelo.math :as math]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    ))

(set! *warn-on-reflection* true)

; make sure BigInteger works as expected
(verify
  (let [bi-five (biginteger 5)]
    ; How does it cost us to cast to BigInteger?
    (while false
      (nl) (prn :5)
      (crit/quick-bench (biginteger 5)) ;              Long:  7 nanosec
      (nl) (prn :bi-five)
      (crit/quick-bench (biginteger bi-five))) ; BigINteger:  4 nanosec

    ; ensure s/validate does what we want
    (throws? (s/validate BigInteger 5))
    (s/validate BigInteger bi-five)
    (s/validate s/Int bi-five)
    (s/validate s/Int 5)

    ; verify `bitstr` gives expected result
    (throws? (int->bitstr 5 2))
    (is= "101" (int->bitstr 5 3))
    (is= "0101" (int->bitstr 5 4))
    (is= "00000101" (int->bitstr 5 8))))

(verify   ; simple tests
  (let [times-2 #(* 2 %)]
    (is= [] (take 0 (range 9)))
    (is= 1 (iterate-n 0 times-2 1))
    (is= 2 (iterate-n 1 times-2 1))
    (is= 4 (iterate-n 2 times-2 1))
    (is= 8 (iterate-n 3 times-2 1))
    (is= 256 (iterate-n 8 times-2 1))))

;-----------------------------------------------------------------------------
; simple verification of vec-shuffle
(verify
  (is= [:b :c :d :a] (vec-shuffle [:a :b :c :d] [1 2 3 0]))
  (is= [:d :c :b :a] (vec-shuffle [:a :b :c :d] [3 2 1 0]))
  (is= [:c :b :d :a] (vec-shuffle [:a :b :c :d] [2 1 3 0]))
  (throws? (vec-shuffle [:a :b :c :d] [2 1 0])))

;-----------------------------------------------------------------------------
(verify
  ; fast tests to verify shuffle/unshuffle for BigInteger bits
  (doseq [num-bits (thru 4 12)]
    (let [ctx (new-ctx {:num-bits num-bits})]
      (with-map-vals ctx [num-bits N-max bit-shuffle-idxs-orig bit-shuffle-idxs-prng]
        (let [orig-vals       (range N-max)
              shuffled-vals   (mapv #(shuffle-bits-BigInteger num-bits bit-shuffle-idxs-orig %) orig-vals)
              unshuffled-vals (mapv #(shuffle-bits-BigInteger num-bits bit-shuffle-idxs-prng %) shuffled-vals)]
          (is-set= orig-vals shuffled-vals)
          (is= orig-vals unshuffled-vals))))))

;-----------------------------------------------------------------------------
(verify
  (when false ; ***** ENABLE TO SEE PRINTOUT *****
    (let [ctx (new-ctx {:num-bits   32
                        :num-rounds 5})]
      ; (spyx ctx)
      (with-map-vals ctx [num-bits N-max num-digits-dec num-digits-hex]
        ; arg must be in slice 0..(dec N-max)
        (throws-not? (randomize-frame ctx 1 0))
        (throws-not? (randomize-frame ctx 1 (dec N-max)))
        (throws? (randomize-frame ctx 1 -1))
        (throws? (randomize-frame ctx 1 N-max))

        (let [idx-vals   (take 32 (range N-max))
              cuid-vals  (mapv #(randomize ctx %) idx-vals)
              idx-deprng (mapv #(derandomize ctx %) cuid-vals)]
          (nl)
          (println "    idx      CUID        hex                 binary                    orig")
          (doseq [[i cuid] (indexed cuid-vals)]
            (when (neg? cuid)
              (throw (ex-info "found-negative" (vals->map cuid))))
            (let [fmt-str (str "%7d   %0" num-digits-dec "d   %s   %s  %7d")
                  hex-str (math/BigInteger->hex-str cuid num-digits-hex)
                  bit-str (int->bitstr cuid num-bits)]
              (println (format fmt-str i cuid hex-str bit-str (nth idx-deprng i)))))
          (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-deprng)))))

  ; Fast coverage tests
  (doseq [nbits (thru 4 12)]
    (let [ctx (new-ctx {:num-bits nbits})]
      (with-map-vals ctx [N-max]
        (let [idx-vals   (range N-max)
              cuid-vals  (cp/pmap :builtin #(randomize ctx %) idx-vals)
              idx-deprng (cp/pmap :builtin #(derandomize ctx %) cuid-vals)]
          (is-set= idx-vals cuid-vals) ; all vals present
          (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-deprng) ; derand recovers original vals, in order
          ))))

  ; Slow coverage test (~35 sec).  ***** Enable to run *****
  (when false
    (let [ctx (new-ctx {:num-bits 20})]
      (with-map-vals ctx [num-bits N-max]
        (nl)
        (println (format "Running integer coverage test (num-bits: %d  N-max: %d)" num-bits N-max))
        (prof/with-timer-print :coverage-test
          (let [nums-orig     (range N-max)
                nums-shuffled (cp/pmap :builtin #(randomize ctx %) nums-orig)]
            (is-set= nums-orig nums-shuffled)))))))

(verify
  (when false ; ***** ENABLE TO SEE TIMING PRINTOUTS *****k
    (tsk/with-validation-disabled
      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 32})]
        (prn :timing-1000-32)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (derandomize ctx
                   (randomize ctx i)))))
      (prof/print-profile-stats!)

      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 64})]
        (prn :timing-1000-64)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (derandomize ctx
                   (randomize ctx i)))))
      (prof/print-profile-stats!)

      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 128})]
        (prn :timing-1000-128)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (derandomize ctx
                   (randomize ctx i)))))
      (prof/print-profile-stats!)

      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 256})]
        (prn :timing-1000-256)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (derandomize ctx
                   (randomize ctx i)))))
      (prof/print-profile-stats!))))
