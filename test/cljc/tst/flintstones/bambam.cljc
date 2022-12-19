(ns tst.flintstones.bambam
  (:require
    ; #todo #bug copy  :include-macros true everywhere!!!
    #?(:clj  [tupelo.core :as t]
       :cljs [tupelo.core :as t :include-macros true])

    #?(:clj [clojure.test] :cljs [cljs.test] )
    #?(:clj  [tupelo.test :refer [deftest testing is verify dotest-focus isnt is= isnt= is-set= is-nonblank= throws? define-fixture]]
       :cljs [tupelo.test-cljs :include-macros true
              :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank= throws? define-fixture]])

    #?(:clj  [flintstones.bambam :as bam]
       :cljs [flintstones.bambam :as bam :include-macros true])
     ))

#?(:cljs (enable-console-print!))

(define-fixture :once
  {:enter (fn [ctx]
            ; (newline) (println "*** TEST ONCE *** - bambam enter ctx=" ctx)
            )
   :leave (fn [ctx]
            ; (println "*** TEST ONCE *** - bambam leave ctx=" ctx)
            )})

(defn tosser [] (throw (ex-info "It threw!" {:a 1})))

(verify
  (println "tst.flintstones.bambam - test 1 - enter")
  (is (= 2 (+ 1 1)))

  (throws? (tosser)) ; #todo fix this!
  (println "tst.flintstones.bambam - test 1 - leave")
  )

(verify
  (println "tst.flintstones.bambam - test 2 - enter")
  (is (= 5 (bam/add2 2 3))) ; this works
  (is (= 3 (bam/logr-bambam
             (inc 0)
             (inc 1)
             (inc 2))))
  (println "tst.flintstones.bambam - test 2 - leave")
  )

