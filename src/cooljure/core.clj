;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure"
      :author "Alan Thompson"}
  cooljure.core
    (:require [clojure.string       :as str]
              [clojure.test         :as test] ) )

(defn truthy?
 "[arg]
  Returns true if arg is logical true (neither nil nor false);
  otherwise returns false."
  [arg]
  (if arg true false) )

(defn falsey?
 "[arg]
  Returns true if arg is logical false (either nil or false);
  otherwise returns false. Equivalent to (not (truthy? arg))."
  [arg]
  (if arg false true) )

(defn any?
 "[pred coll]
  For any predicate & collection, returns true if (pred x) is logical true for any x in
  coll; otherwise returns false.  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (truthy? (some pred coll)) )

(defn not-empty?
 "[coll]
  For any collection, returns true if coll contains any items; otherwise returns false
  Equivalent to (not (empty? coll))."
  [coll]
  (truthy? (seq coll)) )

(defn conjv 
 "( [coll x]
    [coll x & xs] )
  For any collection coll and list of values x, appends the x's to collection, always
  returning the result as a vector."
  ; From Stuart Sierra post 2014-2-10
  ( [coll x]
      (conj (vec coll) x) )
  ( [coll x & xs]
      (apply conj (vec coll) x xs) ))

(defn keyvals 
 "[m]
  For any map m, returns the keys & values of m as a vector, suitable for reconstructing m
  via (apply hashmap (keyvals m))."
  [m]
  {:pre  [ (map? m) ] 
   :post [ (vector? %) ] }
  (reduce   #(conj %1 (first %2) (second %2))
            [] (seq m) ))

(defmacro with-exception-default
 "[default & body]
  Evaluates body & returns its result.  In the event of an exception the specified default
  value is returned instead."
  [default & body]
  `(try
     ~@body
     (catch Exception e# ~default) ))

(defmacro spy-first
  "[expr msg]
  Evaluates the expression (the first arg) and prints both msg and the result to stdout;
  returns the result of expr."
  [expr msg]
  `(let [out-val# ~expr]
      (println (str ~msg " => " out-val#))
      out-val# ))

(defmacro spy-last
  "[msg expr]
  Evaluates the expression (the last arg) and prints both msg and the result to stdout;
  returns the result of expr."
  [msg expr]
  `(let [out-val# ~expr]
      (println (str ~msg " => " out-val#))
      out-val# ))

(defmacro spy-expr
  "[expr]
  Evaluates the expression and prints both expr and its result to stdout; 
  returns the result of expr."
  [expr]
  `(let [out-val# ~expr] 
      (println (str '~expr " => " out-val#)) 
      out-val#) )

(defmacro spy-val
  "[expr]
  Evaluates the expression and prints its result to stdout; 
  returns the result of expr."
  [expr]
  `(let [out-val# ~expr]
      (println out-val#)
      out-val# ))

; add eager (forall  ...) -> (doall (for ...))      ; awtawt TODO:  
;           (for-all ...)
;           (for-now ...)

; Another benefitof test-all:  don't need "-test" suffix like in lein test:
  ; ~/cooljure > lein test :only cooljure.core
  ; lein test user
  ; Ran 0 tests containing 0 assertions.
  ; 0 failures, 0 errors.
  ;
  ; ~/cooljure > lein test :only cooljure.core-test
  ; lein test cooljure.core-test
  ; Ran 8 tests containing 44 assertions.
  ; 0 failures, 0 errors.
  ;
  ; ~/cooljure > lein test :only cooljure.core-test/grab-test
  ; lein test cooljure.core-test
  ; Ran 1 tests containing 3 assertions.
  ; 0 failures, 0 errors.
  ; 
  ; awtawt TODO:  add run-tests with syntax like lein test :only
  ;   (run-tests 'cooljure.core-test)
  ;   (run-tests 'cooljure.core-test/grab-test)

(defn test-all 
  "[& ns-list]
  Convenience fn to reload a namespace & the corresponding test namespace from disk and
  execute tests i the REPL.  Assumes canonical project test file organization with
  parallel src/... & test/... directories, where a '-test' suffix is added to all src
  namespaces to generate the cooresponding test namespace.  Example:

    (test-all 'cooljure.core 'cooljure.csv)

  This will reload cooljure.core, cooljure.core-test, cooljure.csv, cooljure.csv-test and
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

; awtawt TODO:  maybe allow (grab m k :or alt-value) syntax???
(defn grab 
  "A fail-fast version of get. For map m & key k, returns the value v associated with k in
  m.  Throws an exception if k is not present in m."
  [m k]
  (if (contains? m k)
    (get m k)
    (throw (IllegalArgumentException.    
              (str  "Key not present in map:" \newline
                    "  map: " m  \newline
                    "  key: " k  \newline )))))


;************************************************************
; awtawt TODO:  broken, needs fix!
;
(defn f1 [] (println "*** f1 ***"))
(defn f2 [] (println "*** f2 ***"))

(defn run-all 
  "[& fn-list]
  Convenience fn to run a function in the REPL after first reloading its namespace from
  disk.  Functions must be fully namespace-qualified.  Example:

    (run-all 'cooljure.core/main 'cooljure.csv/demo)

  This will reload cooljure.core, and cooljure.csv, then execute the corresponging main
  and test functions."
  [& fn-list]
  (spy-expr fn-list)
  (doseq [curr-fn fn-list]
    (let [  _ (spy-expr curr-fn)
          ns-str      (str/replace (str curr-fn) #"/.*$" "")
          curr-ns     (symbol ns-str) ]
      (println "reloading:" ns-str)
      (require curr-ns :reload-all)
      (println "running" curr-fn)
      (eval curr-fn)
    )))

