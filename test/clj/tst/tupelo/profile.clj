(ns tst.tupelo.profile
  (:use tupelo.core tupelo.test)
  (:require
    [overtone.at-at :as at]
    [tupelo.profile :as prof :refer [defnp]]))

(defn sleep [millis] (Thread/sleep millis))

(defnp sleep-02 [] (sleep 2))
(defnp sleep-03 [] (sleep 3))
(defnp sleep-05 [] (sleep 5))
(defnp sleep-07 [] (sleep 7))
(defnp sleep-11 [] (sleep 11))
(defnp sleep-13 [] (sleep 13))
(defnp sleep-17 [] (sleep 17))
(defnp sleep-77 [] (sleep 77))

(dotest
  (prof/timer-stats-reset)
  (let [fns-to-repeat   #{sleep-02 sleep-03 sleep-05 sleep-07
                          sleep-11 sleep-13 sleep-17 sleep-77}
        atat-timer-pool (at/mk-pool)
        jobs            (forv [curr-fn fns-to-repeat]
                          (at/interspaced 20 curr-fn atat-timer-pool))]
    (sleep 300)
    ; stop all jobs
    (doseq [job jobs] (at/stop job))

    (when false
      (nl)
      (spyx-pretty (prof/profile-map))
      (nl)
      (prof/print-profile-stats)
      (nl))
    ;
    ; Sample output:
    ;
    ; (prof/profile-map) =>
    ;    #:tst.tupelo.profile{
    ;         :sleep-02 {:n 13, :total 0.03043, :mean 0.002341, :sigma 1.8963102E-4},
    ;         :sleep-03 {:n 13, :total 0.04619, :mean 0.003553, :sigma 2.1827222E-4},
    ;         :sleep-05 {:n 12, :total 0.06990, :mean 0.005825, :sigma 3.5853187E-4},
    ;         :sleep-07 {:n 11, :total 0.08956, :mean 0.008141, :sigma 3.6538383E-4},
    ;         :sleep-11 {:n 9, :total 0.117637, :mean 0.013070, :sigma 5.0180919E-4},
    ;         :sleep-13 {:n 9, :total 0.138812, :mean 0.015423, :sigma 5.6124241E-4},
    ;         :sleep-17 {:n 8, :total 0.157265, :mean 0.019658, :sigma 0.0013643
    ;         :sleep-77 {:n 3, :total 0.243879, :mean 0.081293, :sigma 1.3316117E-4}}
    ;
    ; (prof/print-profile-stats) =>
    ;    ---------------------------------------------------------------------------------------------------
    ;    Profile Stats:
    ;    Samples       TOTAL        MEAN      SIGMA           ID
    ;    14        0.033     0.002356   0.000190   :tst.tupelo.profile/sleep-02
    ;    13        0.046     0.003554   0.000218   :tst.tupelo.profile/sleep-03
    ;    12        0.070     0.005825   0.000359   :tst.tupelo.profile/sleep-05
    ;    11        0.090     0.008142   0.000365   :tst.tupelo.profile/sleep-07
    ;    9        0.118     0.013071   0.000502   :tst.tupelo.profile/sleep-11
    ;    9        0.139     0.015424   0.000561   :tst.tupelo.profile/sleep-13
    ;    8        0.157     0.019658   0.001364   :tst.tupelo.profile/sleep-17
    ;    3        0.244     0.081293   0.000133   :tst.tupelo.profile/sleep-77
    ;    ---------------------------------------------------------------------------------------------------

    (let [prof-map (prof/profile-map)]
      (let [within-tol (fn [val low] ; add 23 millis + 20% as upper bound
                         (<= low val (+ low (* 1.2 low) 0.023)))]
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-02 :mean]) 0.002))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-03 :mean]) 0.003))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-05 :mean]) 0.005))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-07 :mean]) 0.007))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-11 :mean]) 0.011))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-13 :mean]) 0.013))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-17 :mean]) 0.017))
        (is (within-tol (fetch-in prof-map [:tst.tupelo.profile/sleep-77 :mean]) 0.077)))
      (let [sigma-vals (forv [[tag stats] prof-map]
                         (grab :sigma stats))
            pass-flgs  (mapv #(< % 0.002) sigma-vals)]
        (is (every? truthy? pass-flgs))))
    ))

