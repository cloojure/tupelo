(ns tst.runner
  (:require
    [cljs.test :refer [report] :refer-macros [run-tests]]
    [figwheel.main.async-result :as async-result]

    ; ********** REQUIRE ALL TESTING NS **********
    ;-----------------------------------------------------------------------------
    [tst._bootstrap]

    [tst.flintstones.dino]
    [tst.flintstones.slate]
    ;[tst.flintstones.wilma]
    ;[tst.flintstones.pebbles]
    [tst.flintstones.bambam]

    [tst.tupelo.array]
    [tst.tupelo.array.mutable]
    [tst.tupelo.chars]
    [tst.tupelo.core]
    [tst.tupelo.gotchas]
    [tst.tupelo.lazy]
    [tst.tupelo.lexical]
    [tst.tupelo.misc]
    [tst.tupelo.math]
    [tst.tupelo.parse]
    [tst.tupelo.schema]
    [tst.tupelo.set]
    [tst.tupelo.string]
    [tst.tupelo.vec]
    ;-----------------------------------------------------------------------------

    ))

; tests can be asynchronous, we must hook test end
(defmethod report [:cljs.test/default :end-run-tests] [test-data]
  (if (cljs.test/successful? test-data)
    (async-result/send "Tests passed!!")
    (async-result/throw-ex (ex-info "Tests Failed" test-data))))


(defn -main [& args]
  (run-tests

    ; ********** MUST REPEAT HERE ALL TEST NS FROM ABOVE `(:require ...)` **********
    ;-----------------------------------------------------------------------------
    'tst._bootstrap

    'tst.flintstones.dino
    'tst.flintstones.slate
    ;'tst.flintstones.wilma
    ;'tst.flintstones.pebbles
    'tst.flintstones.bambam

    'tst.tupelo.array
    'tst.tupelo.array.mutable
    'tst.tupelo.chars
    'tst.tupelo.core
    'tst.tupelo.gotchas
    'tst.tupelo.lazy
    'tst.tupelo.lexical
    'tst.tupelo.misc
    'tst.tupelo.math
    'tst.tupelo.parse
    'tst.tupelo.schema
    'tst.tupelo.set
    'tst.tupelo.string
    'tst.tupelo.vec
    ;-----------------------------------------------------------------------------

    )
  ; return a message to the figwheel process that tells it to wait
  [:figwheel.main.async-result/wait 22000] ; millis
  )
