(ns tst.flintstones.slate
  (:require
    [clojure.string :as str]

    #?(:clj  [clojure.test :refer [deftest testing is]]
       :cljs [cljs.test :refer-macros [deftest testing is]])

    ; ; #todo #bug copy  :include-macros true everywhere!!!
    #?(:clj  [tupelo.test      :refer [define-fixture dotest isnt is= isnt= is-set= is-nonblank= throws?]]
       :cljs [tupelo.test-cljs :refer [define-fixture dotest isnt is= isnt= is-set= is-nonblank= throws?]
              ;:as tt
              :include-macros true ])

    #?(:clj  [tupelo.core :as t :refer [spy spyx spyxx]]
       :cljs [tupelo.core :as t :refer [spy spyx spyxx]] :include-macros true )

    #?(:clj  [flintstones.slate :as slate]
       :cljs [flintstones.slate :as slate :include-macros true])
    ))


#?(:cljs (enable-console-print!))


(define-fixture :once
  {:enter (fn [ctx]
            (println "*** TEST ONCE *** - slate enter ctx=" ctx)
            )
   :leave (fn [ctx]
            (println "*** TEST ONCE *** - slate leave ctx=" ctx)
            )})

(dotest
  (is= 2 (+ 1 1))   ; this works
  (is= 5 (slate/add2 2 3)) ; this works

  (is= 3 (slate/logr-slate
           (inc 0)
           (inc 1)
           (inc 2)))

  (is true)
  (isnt false)
  (is= 42 (* 6 7)))
