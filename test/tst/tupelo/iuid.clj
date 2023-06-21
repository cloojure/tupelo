(ns tst.tupelo.iuid
  (:use tupelo.iuid tupelo.core tupelo.test)
  (:require
    [com.climate.claypoole :as cp]
    [schema.core :as s]
    [tupelo.bits :as bits]
    [tupelo.profile :as prof]
    [tupelo.schema :as tsk]
    ))

(set! *warn-on-reflection* true)

(verify
  ; Fast coverage tests
  (doseq [nbits (thru 4 12)]
    (let [ctx (new-ctx {:num-bits nbits})]
      (with-map-vals ctx [N-max]
        (let [idx-vals   (range N-max)
              iuid-vals  (cp/pmap :builtin #(idx->uid ctx %) idx-vals)
              idx-deprng (cp/pmap :builtin #(uid->idx ctx %) iuid-vals)]
          (is-set= idx-vals iuid-vals) ; all vals present
          (when (< 8 nbits)
            (isnt= idx-vals iuid-vals)) ; but not same order (random chance: 1 in N!)
          (is= idx-vals idx-deprng) ; recovers original vals, in order
          )))))

(verify
  (when false ; ***** ENABLE TO SEE PRINTOUT *****
    (let [ctx (new-ctx {:num-bits 32})]
      ; (spyx ctx)
      (with-map-vals ctx [num-bits N-max num-digits-dec num-digits-hex]
        (let [idx-vals   (take 32 (range N-max))
              iuid-vals  (mapv #(idx->uid ctx %) idx-vals)
              idx-deprng (mapv #(uid->idx ctx %) iuid-vals)]
          (nl)
          (println (strcat "    idx      IUID        hex"
                     (repeat 16 \space) "binary"
                     (repeat 22 \space) "orig"))
          (doseq [[i uid] (indexed iuid-vals)]
            (when (neg? uid)
              (throw (ex-info "found-negative" (vals->map uid))))
            (let [fmt-str (str "%7d   %0" num-digits-dec "d   %s   %s  %7d")
                  hex-str (bits/intval->hex-str uid num-digits-hex)
                  bit-str (bits/intval->bitstr uid num-bits)]
              (println (format fmt-str i uid hex-str bit-str (nth idx-deprng i)))))
          (isnt= idx-vals iuid-vals) ; but not same order (random chance 1 in N!)
          (is= idx-vals idx-deprng))))
  ; sample output
  (comment
    "   idx      IUID        hex                 binary                    orig
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
         31   0878482163   345c92f3   00110100010111001001001011110011       31 "
    )))

(verify
  (when false ; ***** ENABLE TO SEE TIMING PRINTOUTS *****k
    (s/without-fn-validation
      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 32})]
        (prn :timing-1000-32)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (uid->idx ctx
                   (idx->uid ctx i)))))
      (prof/print-profile-stats!)

      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 64})]
        (prn :timing-1000-64)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (uid->idx ctx
                   (idx->uid ctx i)))))
      (prof/print-profile-stats!)

      (prof/timer-stats-reset!)
      (let [ctx (new-ctx {:num-bits 128})]
        (prn :timing-1000-128)
        (dotimes [i 1000] ; timing for 1000 CRIDX values
          (is= i (uid->idx ctx
                   (idx->uid ctx i)))))
      (prof/print-profile-stats!))
    ))
