(ns tst.flintstones.bambam
  (:require
    #?@(:clj [[flintstones.test-clj   :refer [dotest is isnt is= isnt= testing define-fixture]]
              [flintstones.bambam :as bam]])
    #?@(:cljs [[flintstones.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture]]
               [flintstones.bambam :as bam :include-macros true]])
  ))

(define-fixture :once
     {:enter (fn [ctx] (println "*** TEST ONCE *** - enter ctx=" ctx))
      :leave (fn [ctx] (println "*** TEST ONCE *** - leave ctx=" ctx))})
;--------------------------------------------------------------------------------------------------

(dotest
  (println "test 1")
  (is= 2 (+ 1 1)))

(dotest
  (println "test 2")
  (is= 5 (bam/add2 2 3)) ; this works
  (is= 3 (bam/logr-bambam
           (inc 0)
           (inc 1)
           (inc 2))))
