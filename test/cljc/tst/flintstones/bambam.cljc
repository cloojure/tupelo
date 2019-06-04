(ns tst.flintstones.bambam
  #?(:clj (:require
            [tupelo.test :as ttst
             :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx]]
            [tupelo.types :as types]
            [flintstones.bambam :as bam]
            ))
  #?(:cljs (:require
             [tupelo.test-cljs :include-macros true ; #todo #bug copy  :include-macros true everywhere!!!
              :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]]
             [tupelo.core :include-macros true :as t :refer [spy spyx spyxx]]
             [tupelo.string :include-macros true :as ts]
             [clojure.string :as str]
             [flintstones.bambam :as bam :include-macros true]
             )))

#?(:cljs (enable-console-print!))

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

  (throws? (tosser))
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
