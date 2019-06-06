(ns tst.flintstones.slate
  (:require
    [clojure.string :as str]
    ; ; #todo #bug copy  :include-macros true everywhere!!!

    #?(:clj  [tupelo.core :as t]
       :cljs [tupelo.core :as t :include-macros true])

    #?(:clj [clojure.test] :cljs [cljs.test] )
    #?(:clj  [tupelo.test :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank= throws? define-fixture]]
       :cljs [tupelo.test-cljs ; :include-macros true
              :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank= throws? define-fixture]])

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
