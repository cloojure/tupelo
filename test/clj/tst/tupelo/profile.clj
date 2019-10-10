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

(def atat-timer-pool (at/mk-pool))
(dotest
  (prof/timer-stats-reset)
  (let [fns-to-repeat #{sleep-02 sleep-03 sleep-05 sleep-07
                        sleep-11 sleep-13 sleep-17 sleep-77}
        jobs          (forv [curr-fn fns-to-repeat]
                        (at/interspaced 20 curr-fn atat-timer-pool))]
    (sleep 300)
    ; stop all jobs
    (doseq [job jobs] (at/stop job))

    (when false
      (nl)
      (spyx-pretty (prof/profile-map))
      (nl)
      (prof/print-profile-stats)
      (nl)
      )

    ; -----------------------------------------------------------------------------
    ; Sample value for (prof/stats-get-all)
    ;   #:tst.tupelo.profile{:sleep-2 {:n 13, :mean 0.002509983, :sigma 0.000127452537},
    ;                        :sleep-3 {:n 13, :mean 0.003584854, :sigma 0.000274577250},
    ;                        :sleep-7 {:n 11, :mean 0.007849257, :sigma 0.000651772415},
    ;                        :sleep-5 {:n 12, :mean 0.005633091, :sigma 0.000442189072},
    ;                        :sleep-11 {:n 9, :mean 0.012053967, :sigma 0.000799214302},
    ;                        :sleep-13 {:n 9, :mean 0.014495919, :sigma 0.000971787897},
    ;                        :sleep-17 {:n 8, :mean 0.019744416, :sigma 0.001476271221},
    ;                        :sleep-77 {:n 3, :mean 0.079445910, :sigma 0.000089636781}}
    ; -----------------------------------------------------------------------------

    (let [prof-map (prof/profile-map)]
      (let [within-tol (fn [val low] ; add 3 millis + 20% as upper bound
                         (<= low val (+ low (* 1.2 low) 0.003)))]
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

