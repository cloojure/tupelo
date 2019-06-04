(ns tst.flintstones.bambam
  #?(:clj (:require
            [tupelo.test :as tt
           ; :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]
             ]
            [tupelo.core :as t :refer [spy spyx spyxx]]
            [tupelo.types :as types]
            [flintstones.bambam :as bam]
            ))
  #?(:cljs (:require
             [cljs.test :refer [deftest is ] ]
             [tupelo.test-cljs :as tt :include-macros true ; #todo #bug copy  :include-macros true everywhere!!!
            ; :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]
              ]
             [tupelo.core :include-macros true :as t :refer [spy spyx spyxx]]
             [flintstones.bambam :as bam :include-macros true]
             )))

#?(:cljs (enable-console-print!))

(tt/define-fixture :once
  {:enter (fn [ctx]
             (println "*** TEST ONCE *** - bambam enter ctx=" ctx)
            )
   :leave (fn [ctx]
             (println "*** TEST ONCE *** - bambam leave ctx=" ctx)
            )})

(defn tosser [] (throw (ex-info "It threw!" {:a 1})))

(tt/dotest         ; deftest t-1         ;
  (println "tst.flintstones.bambam - test 1 - enter")
  (is (= 2 (+ 1 1)))

  ; (tt/throws? (tosser)) ; #todo fix this!
  (println "tst.flintstones.bambam - test 1 - leave")
  )

(tt/dotest         ; deftest t-2         ;
  (println "tst.flintstones.bambam - test 2 - enter")
  (is (= 5 (bam/add2 2 3))) ; this works
  (is (= 3 (bam/logr-bambam
             (inc 0)
             (inc 1)
             (inc 2))))
  (println "tst.flintstones.bambam - test 2 - leave")
  )
