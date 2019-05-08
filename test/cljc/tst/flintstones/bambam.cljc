(ns tst.flintstones.bambam
  (:require
    #?@(:clj [[tupelo.test   :refer [dotest is isnt is= isnt= testing define-fixture throws?]]
              [flintstones.bambam :as bam]])
    #?@(:cljs [[tupelo.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture throws?]]
               [flintstones.bambam :as bam :include-macros true]])
  ))

(define-fixture :once
     {:enter (fn [ctx]
              ;(println "*** TEST ONCE *** - enter ctx=" ctx)
               )
      :leave (fn [ctx]
              ;(println "*** TEST ONCE *** - leave ctx=" ctx)
               )})
;--------------------------------------------------------------------------------------------------

(defn tosser [] (throw (ex-info "It threw!" {:a 1})))

(dotest
 ;(println "tst.flintstones.bambam - test 1 - enter")
  (is= 2 (+ 1 1))

  (throws? (tosser) )
 ;(println "tst.flintstones.bambam - test 1 - leave")
  )

(dotest
 ;(println "tst.flintstones.bambam - test 2 - enter")
  (is= 5 (bam/add2 2 3)) ; this works
  (is= 3 (bam/logr-bambam
           (inc 0)
           (inc 1)
           (inc 2)))
 ;(println "tst.flintstones.bambam - test 2 - leave")
  )
