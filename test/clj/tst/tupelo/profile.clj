(ns tst.tupelo.profile
  (:use tupelo.core tupelo.test)
  (:require
    [overtone.at-at :as at]
    [tupelo.profile :as prof :refer [defnp]]))

(defn sleep [millis] (Thread/sleep millis))

(defnp sleep-2 [] (sleep 2))
(defnp sleep-3 [] (sleep 3))
(defnp sleep-5 [] (sleep 5))
(defnp sleep-7 [] (sleep 7))
(defnp sleep-11 [] (sleep 11))
(defnp sleep-13 [] (sleep 13))
(defnp sleep-17 [] (sleep 17))
(defnp sleep-77 [] (sleep 77))

(def atat-timer-pool (at/mk-pool))
(dotest
  (prof/timer-stats-reset)
  (let [fns-to-repeat #{sleep-2 sleep-3 sleep-5 sleep-7
                        sleep-11 sleep-13 sleep-17 sleep-77}
        jobs          (forv [curr-fn fns-to-repeat]
                        (at/interspaced 20 curr-fn atat-timer-pool))]
    (sleep 300)
    ; stop all jobs
    (doseq [job jobs] (at/stop job))
    (when true
      (nl)
      (println "-----------------------------------------------------------------------------")
      (prof/stats-print-all)
      (println "-----------------------------------------------------------------------------")
      (nl))

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
    ; Sample output for (prof/stats-print-all)
    ;   :tst.tupelo.profile/sleep-2       13   0.00247 0.00015098
    ;   :tst.tupelo.profile/sleep-3       13   0.00364 0.00020310
    ;   :tst.tupelo.profile/sleep-5       12   0.00570 0.00048202
    ;   :tst.tupelo.profile/sleep-7       11   0.00761 0.00029422
    ;   :tst.tupelo.profile/sleep-11       9   0.01250 0.00088938
    ;   :tst.tupelo.profile/sleep-13       9   0.01464 0.00114652
    ;   :tst.tupelo.profile/sleep-17       8   0.01938 0.00127845
    ;   :tst.tupelo.profile/sleep-77       3   0.07960 0.00099748
    ; -----------------------------------------------------------------------------
    (let [stats-all (prof/stats-get-all)]
      (let [within-tol (fn [val low] ; add 3 millis + 20% as upper bound
                         (<= low val (+ low (* 1.2 low) 0.003)))]
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-2 :mean]) 0.002))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-3 :mean]) 0.003))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-5 :mean]) 0.005))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-7 :mean]) 0.007))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-11 :mean]) 0.011))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-13 :mean]) 0.013))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-17 :mean]) 0.017))
        (is (within-tol (fetch-in stats-all [:tst.tupelo.profile/sleep-77 :mean]) 0.077)))
      (let [sigma-vals (forv [[tag stats] stats-all]
                         (grab :sigma stats))
            pass-flgs  (mapv #(< % 0.002) sigma-vals)]
        (is (every? truthy? pass-flgs))))))

