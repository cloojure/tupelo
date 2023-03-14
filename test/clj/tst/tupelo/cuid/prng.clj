(ns tst.tupelo.cuid.prng
  (:use tupelo.cuid.prng
        tupelo.core
        tupelo.test)
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
    ))

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
    (let [ctx (new-ctx {:num-bits 32})]
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
                  hex-str (math/int->hex-str cuid num-digits-hex)
                  bit-str (math/int->bitstr cuid num-bits)]
              (println (format fmt-str i cuid hex-str bit-str (nth idx-deprng i)))))
          (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-deprng))))

    ; sample output
    (comment
      "   idx      CUID        hex                 binary                    orig
            0   1797252525   6b1fe5ad   01101011000111111110010110101101        0
            1   3193035598   be51db4e   10111110010100011101101101001110        1
            2   0411151101   1881aafd   00011000100000011010101011111101        2
            3   0916926540   36a7304c   00110110101001110011000001001100        3
            4   3419910709   cbd7b235   11001011110101111011001000110101        4
            5   1891426540   70bce0ec   01110000101111001110000011101100        5
            6   2757549852   a45cdf1c   10100100010111001101111100011100        6
            7   3880408411   e74a555b   11100111010010100101010101011011        7
            8   3856297955   e5da6fe3   11100101110110100110111111100011        8
            9   4112662128   f5223e70   11110101001000100011111001110000        9
           10   2937493506   af169802   10101111000101101001100000000010       10
           11   2659727793   9e8839b1   10011110100010000011100110110001       11
           12   1596734819   5f2c3d63   01011111001011000011110101100011       12
           13   0220273329   0d211ab1   00001101001000010001101010110001       13
           14   4058072225   f1e144a1   11110001111000010100010010100001       14
           15   2795200814   a69b612e   10100110100110110110000100101110       15
           16   3940061688   ead891f8   11101010110110001001000111111000       16
           17   3770388920   e0bb91b8   11100000101110111001000110111000       17
           18   2689111413   a0489575   10100000010010001001010101110101       18
           19   2444130251   91ae77cb   10010001101011100111011111001011       19
           20   0310946558   1288aafe   00010010100010001010101011111110       20
           21   1881346490   702311ba   01110000001000110001000110111010       21
           22   3863836214   e64d7636   11100110010011010111011000110110       22
           23   3929216666   ea33169a   11101010001100110001011010011010       23
           24   4010904845   ef118d0d   11101111000100011000110100001101       24
           25   2277591884   87c14b4c   10000111110000010100101101001100       25
           26   0583361547   22c5640b   00100010110001010110010000001011       26
           27   0929777628   376b47dc   00110111011010110100011111011100       27
           28   1304460335   4dc07c2f   01001101110000000111110000101111       28
           29   0219986893   0d1cbbcd   00001101000111001011101111001101       29
           30   3445150499   cd58d323   11001101010110001101001100100011       30
           31   3847355345   e551fbd1   11100101010100011111101111010001       31 "
      ))

  ; Exhaustive coverage tests for small N
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

; Sampling tests for larger N
(verify
  (cp/pdoseq :builtin [nbits (thru 32 128)] ; about 300 ms
    (let [ctx   (new-ctx {:num-bits nbits})
          nvals 20]
      (with-map-vals ctx [N-max]
        (let [idx-vals   (range nvals)
              cuid-vals  (mapv #(randomize ctx %) idx-vals)
              idx-deprng (mapv #(derandomize ctx %) cuid-vals)]
          (isnt= idx-vals cuid-vals) ; prob of failure near zero
          (is (every? nonneg? cuid-vals))
          (is (every? #(< % N-max) cuid-vals))
          (is= idx-vals idx-deprng) ; derand recovers original vals, in order
          )))))

(verify
  (when false ; ***** ENABLE TO SEE TIMING PRINTOUTS *****k
    (let [ivals (range 1000)]
      (tsk/with-validation-disabled
        (prof/timer-stats-reset!)
        (let [ctx (new-ctx {:num-bits 32})]
          (prn :timing-1000-32)
          (doseq [i ivals] ; timing for 1000 CRIDX values
            (is= i (derandomize ctx
                     (randomize ctx i)))))
        (prof/print-profile-stats!)

        (prof/timer-stats-reset!)
        (let [ctx (new-ctx {:num-bits 64})]
          (prn :timing-1000-64)
          (doseq [i ivals] ; timing for 1000 CRIDX values
            (is= i (derandomize ctx
                     (randomize ctx i)))))
        (prof/print-profile-stats!)

        (prof/timer-stats-reset!)
        (let [ctx (new-ctx {:num-bits 128})]
          (prn :timing-1000-128)
          (doseq [i ivals] ; timing for 1000 CRIDX values
            (is= i (derandomize ctx
                     (randomize ctx i)))))
        (prof/print-profile-stats!)

        (prof/timer-stats-reset!)
        (let [ctx (new-ctx {:num-bits 256})]
          (prn :timing-1000-256)
          (doseq [i ivals] ; timing for 1000 CRIDX values
            (is= i (derandomize ctx
                     (randomize ctx i)))))
        (prof/print-profile-stats!)))))

