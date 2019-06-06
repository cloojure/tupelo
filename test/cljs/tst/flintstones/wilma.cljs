(ns tst.flintstones.wilma
  (:require
    [tupelo.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture]]
    [wilma] ; new for figwheel-main

    ; #todo figure out how to make wilma tests work with externs

  ))

(define-fixture :each
  {:enter (fn [ctx] (println "*** TEST EACH *** - enter ctx=" ctx))
   :leave (fn [ctx] (println "*** TEST EACH *** - leave ctx=" ctx))})

(dotest
  (println "wilma: testing add" )
  (is= 3 (+ 2 1)))

(dotest
  (println "wilma/stats:   " wilma/stats)
  (isnt= wilma/stats wilma/stats2) ; JS objs are not=

  (is= (js->clj wilma/stats) ; must convert to clojure maps for value equality to work
    (js->clj wilma/stats2))

  (let [wilma (wilma/makeWilma)]
    (println "wilma =>" wilma)
    (is= (.-desc wilma) "patient housewife")
    (is= (.says wilma "Fred") "Hello, Fred")))

