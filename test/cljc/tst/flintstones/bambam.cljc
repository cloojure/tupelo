(ns tst.flintstones.bambam
  (:require
    #?@(:clj [[tupelo.test   :refer [dotest is isnt is= isnt= testing define-fixture throws?]]
              [flintstones.bambam :as bam]])
    #?@(:cljs [[tupelo.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture throws?]]
               [flintstones.bambam :as bam :include-macros true]])
  ))

; #todo fix this
;(define-fixture :once
;     {:enter (fn [ctx] (println "*** TEST ONCE *** - enter ctx=" ctx))
;      :leave (fn [ctx] (println "*** TEST ONCE *** - leave ctx=" ctx))})
;--------------------------------------------------------------------------------------------------

(defn tosser [] (throw (ex-info "It threw!" {:a 1})))

(dotest
  (println "test 1")
  (is= 2 (+ 1 1))

  (throws? (tosser) )

  )

(dotest
  (println "test 2")
  (is= 5 (bam/add2 2 3)) ; this works
  (is= 3 (bam/logr-bambam
           (inc 0)
           (inc 1)
           (inc 2))))
