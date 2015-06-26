
; Another benefit of test-all:  don't need "-test" suffix like in lein test:
  ; ~/tupelo > lein test :only tupelo.core
  ; lein test user
  ; Ran 0 tests containing 0 assertions.     ***** Nearly silent failure *****
  ; 0 failures, 0 errors.
  ;
  ; ~/tupelo > lein test :only tupelo.core-test
  ; lein test tupelo.core-test
  ; Ran 8 tests containing 44 assertions.     ***** Runs correctly *****
  ; 0 failures, 0 errors.
  ;
  ; ~/tupelo > lein test :only tupelo.core-test/convj-test
  ; lein test tupelo.core-test
  ; Ran 1 tests containing 3 assertions.
  ; 0 failures, 0 errors.
  ; 
  ; #awt TODO:  add run-tests with syntax like lein test :only
  ;   (run-tests 'tupelo.core-test)
  ;   (run-tests 'tupelo.core-test/convj-test)

(defn test-all 
  "Convenience fn to reload a namespace & the corresponding test namespace from disk and
  execute tests in the REPL.  Assumes canonical project test file organization with
  parallel src/... & test/... directories, where a '-test' suffix is added to all src
  namespaces to generate the cooresponding test namespace.  Example:

    (test-all 'tupelo.core 'tupelo.csv)

  This will reload tupelo.core, tupelo.core-test, tupelo.csv, tupelo.csv-test and
  then execute clojure.test/run-tests on both of the test namespaces."
  [& ns-list]
  (use 'clojure.test)
  (let [
    test-ns-list    (for [curr-ns ns-list]
                      (let [curr-ns-test (symbol (str curr-ns "-test")) ]
                        (require curr-ns curr-ns-test :reload-all)
                        curr-ns-test )) 
    _ (println "------------------------------------------------------------")
    test-result     (apply clojure.test/run-tests test-ns-list)
    _ (println "------------------------------------------------------------")
    _ (newline)
  ]
  nil ))

