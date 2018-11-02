(ns tst.flintstones.slate
  (:require
    #?@(:clj [[flintstones.test-clj :refer [dotest is isnt is= isnt=]]
              [flintstones.slate :as slate]])

    #?@(:cljs [[flintstones.test-cljs :refer [dotest is isnt is= isnt=]]
               [flintstones.slate :as slate :include-macros true]])
  ))

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
