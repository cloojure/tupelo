(ns tst.flintstones.wilma
  (:require
    [clojure.string :as str]
    [flintstones.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture]]
    [wilmaPhony]    ; This one must be first or get;
         ; "WARNING: Use of undeclared Var cljs.test/test-var at line 15 test/cljs/tst/flintstones/wilma.cljs"
  ))

(define-fixture :each
  {:enter (fn [ctx] (println "*** TEST EACH *** - enter ctx=" ctx))
   :leave (fn [ctx] (println "*** TEST EACH *** - leave ctx=" ctx))})

(dotest
  (println "wilma: testing add" )
  (is= 3 (+ 2 1)))

(dotest
  (println "wilmaPhony/stats:   " wilmaPhony/stats)
  (isnt= wilmaPhony/stats wilmaPhony/stats2) ; JS objs are not=

  (is= (js->clj wilmaPhony/stats) ; must convert to clojure maps for value equality to work
    (js->clj wilmaPhony/stats2))

  (let [wilma (wilmaPhony/makeWilma)]
    (println "wilma =>" wilma)
    (is= (.-desc wilma) "patient housewife")
    (is= (.says wilma "Fred") "Hello, Fred")))

