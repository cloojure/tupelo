(ns tst.flintstones.slate
  #?(:clj (:require
            [tupelo.test :as ttst
             :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx]]
            [tupelo.types :as types]

            [flintstones.slate :as slate]
            ))
  #?(:cljs (:require
             [tupelo.test-cljs :include-macros true ; #todo #bug copy  :include-macros true everywhere!!!
              :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]]
             [tupelo.core :include-macros true :as t :refer [spy spyx spyxx]]
             [tupelo.string :include-macros true :as ts]
             [clojure.string :as str]

             [flintstones.slate :as slate :include-macros true]
             )))

  #?(:cljs (enable-console-print!))


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
