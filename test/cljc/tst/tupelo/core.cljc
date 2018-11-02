(ns tst.tupelo.core
  (:require
    #?@(:clj [[flintstones.test-clj   :refer [dotest is isnt is= isnt= testing define-fixture]]
              [tupelo.core :as t]])
    #?@(:cljs [[flintstones.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture]]
               [tupelo.core :as t :include-macros true]])
  ))

(define-fixture :once
     {:enter (fn [ctx] (println "*** TEST ONCE *** - enter "))
      :leave (fn [ctx] (println "*** TEST ONCE *** - leave "))})
;--------------------------------------------------------------------------------------------------

(dotest
  (println "tst.tupelo.core test 1")
  (is= 2 (+ 1 1))

  (is (t/truthy? true))
  (is (t/truthy? 5))
  (is (t/falsey? false))
  (is (t/falsey? nil))
  )

