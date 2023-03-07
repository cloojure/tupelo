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
                  hex-str (math/int->hex-str cuid num-digits-hex)
                  bit-str (math/int->bitstr cuid num-bits)]
              (println (format fmt-str i cuid hex-str bit-str (nth idx-deprng i)))))
          (isnt= idx-vals cuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-deprng))))

    ; sample output
    (comment
      "   idx      CUID        hex                 binary                    orig
            0   2938486314   af25be2a   10101111001001011011111000101010        0
            1   0573692738   2231db42   00100010001100011101101101000010        1
            2   2829576266   a8a7e84a   10101000101001111110100001001010        2
            3   1128350082   43414182   01000011010000010100000110000010        3
            4   0730285699   2b874683   00101011100001110100011010000011        4
            5   1443399447   56088717   01010110000010001000011100010111        5
            6   2269242278   8741e3a6   10000111010000011110001110100110        6
            7   2717173679   a1f4c7af   10100001111101001100011110101111        7
            8   3407102468   cb144204   11001011000101000100001000000100        8
            9   2958367173   b05519c5   10110000010101010001100111000101        9
           10   0563780141   219a9a2d   00100001100110101001101000101101       10
           11   0638614259   26107af3   00100110000100000111101011110011       11
           12   0985937285   3ac43585   00111010110001000011010110000101       12
           13   1022018987   3ceac5ab   00111100111010101100010110101011       13
           14   0551653277   20e18f9d   00100000111000011000111110011101       14
           15   4126687089   f5f83f71   11110101111110000011111101110001       15
           16   0473216597   1c34b655   00011100001101001011011001010101       16
           17   2502258414   95256eee   10010101001001010110111011101110       17
           18   2691705856   a0702c00   10100000011100000010110000000000       18
           19   0213499453   0cb9be3d   00001100101110011011111000111101       19
           20   1323737466   4ee6a17a   01001110111001101010000101111010       20
           21   0883324089   34a674b9   00110100101001100111010010111001       21
           22   1043130075   3e2ce6db   00111110001011001110011011011011       22
           23   3310882968   c5581098   11000101010110000001000010011000       23
           24   2448792751   91f59caf   10010001111101011001110010101111       24
           25   4123493087   f5c782df   11110101110001111000001011011111       25
           26   0063742008   03cca038   00000011110011001010000000111000       26
           27   2190235887   828c58ef   10000010100011000101100011101111       27
           28   1285911922   4ca57572   01001100101001010111010101110010       28
           29   2290344855   8883e397   10001000100000111110001110010111       29
           30   3139906799   bb272cef   10111011001001110010110011101111       30
           31   0878482163   345c92f3   00110100010111001001001011110011       31
      "))

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
