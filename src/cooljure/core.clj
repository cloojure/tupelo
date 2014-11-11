;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.core
  "Cooljure - Cool stuff you wish was in Clojure"
  (:require [clojure.string               :as str]
            [clojure.test                 :as test]
            [clojure.core.incubator       :as cci] ))

(defn truthy?
 "Returns true if arg is logical true (neither nil nor false);
  otherwise returns false."
  [arg]
  (if arg true false) )

(defn falsey?
 "Returns true if arg is logical false (either nil or false);
  otherwise returns false. Equivalent to (not (truthy? arg))."
  [arg]
  (if arg false true) )

(defn any?
 "For any predicate & collection, returns true if (pred x) is logical true for any x in
  coll; otherwise returns false.  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (truthy? (some pred coll)) )

(defn not-empty?
 "For any collection, returns true if coll contains any items; otherwise returns false
  Equivalent to (not (empty? coll))."
  [coll]
  (truthy? (seq coll)) )

(defn conjv 
 "For any collection coll and list of values x, appends the x's to collection, always
  returning the result as a vector."
  ; From Stuart Sierra post 2014-2-10
  ( [coll x]
      (conj (vec coll) x) )
  ( [coll x & xs]
      (apply conj (vec coll) x xs) ))

(defn keyvals 
 "For any map m, returns the keys & values of m as a vector, suitable for reconstructing m
  via (apply hash-map (keyvals m))."
  [m]
  {:pre  [ (map? m) ] 
   :post [ (vector? %) ] }
  (reduce   #(conj %1 (first %2) (second %2))
            [] (seq m) ))

(defmacro with-exception-default
 "Evaluates body & returns its result.  In the event of an exception the specified default
  value is returned instead of the exception."
  [default-val & body]
  `(try
     ~@body
     (catch Exception e# ~default-val) ))

(defn spy
  "A (println ...) for use in threading forms. Usage:  (spy :msg \"#dbg301\")
  Prints both the message (the string after the :msg keyword) and the value inserted by the
  threading form (either -> or ->>) to stdout, then returns the value. For example, both
  of the following 
        (->   2 
              (+ 3) 
              (spy :msg \"sum\" )
              (* 4))
        (->>  2 
              (+ 3) 
              (spy :msg \"sum\" )
              (* 4))
  will print 'sum => 5'.  "
  [arg1 arg2 arg3]
  (cond (= :msg arg1) (do (println (str arg2 " => " (pr-str arg3))) arg3)  ; for ->>
        (= :msg arg2) (do (println (str arg3 " => " (pr-str arg1))) arg1)  ; for ->
        :else (throw (IllegalArgumentException.  (str 
                         "spy: either first or 2nd arg must be :msg \n   args:"
                         (pr-str [arg1 arg2 arg3]))))))

(defmacro spyx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the
  supplied expression, printing both the expression and its value to stdout, then returns
  the value."
  [expr]
  `(let [spy-val# ~expr] 
      (println (str '~expr " => " (pr-str spy-val#)))
      spy-val#))


; add eager (forall  ...) -> (doall (for ...))      ; #awt TODO:  
;           (for-all ...)
;           (for-now ...)

; Another benefit of test-all:  don't need "-test" suffix like in lein test:
  ; ~/cooljure > lein test :only cooljure.core
  ; lein test user
  ; Ran 0 tests containing 0 assertions.     ***** Nearly silent failure *****
  ; 0 failures, 0 errors.
  ;
  ; ~/cooljure > lein test :only cooljure.core-test
  ; lein test cooljure.core-test
  ; Ran 8 tests containing 44 assertions.     ***** Runs correctly *****
  ; 0 failures, 0 errors.
  ;
  ; ~/cooljure > lein test :only cooljure.core-test/convj-test
  ; lein test cooljure.core-test
  ; Ran 1 tests containing 3 assertions.
  ; 0 failures, 0 errors.
  ; 
  ; #awt TODO:  add run-tests with syntax like lein test :only
  ;   (run-tests 'cooljure.core-test)
  ;   (run-tests 'cooljure.core-test/convj-test)

(defn test-all 
  "[& ns-list]
  Convenience fn to reload a namespace & the corresponding test namespace from disk and
  execute tests in the REPL.  Assumes canonical project test file organization with
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

(defn rel=
 "Returns true if 2 double-precision numbers are relatively equal, else false.  Relative
 equality is specified as either (1) the N most significant digits are equal, or (2) the
 absolute difference is less than a tolerance value.  Input values are coerced to double
 before comparison.  Example:

    (rel= 123450000 123456789   :digits 4)      ; true
    (rel= 1         1.001       :tol 0.01 )     ; true
  "
  [val1 val2 & {:as opts} ]
  { :pre [  (number? val1) (number? val2) ]
    :post [ (contains? #{true false} %) ] }
  (let [ {:keys [digits tol] }  opts ]
    (when-not (or digits tol)
      (throw (IllegalArgumentException.
                (str  "Must specify either :digits or :tol" \newline
                      "opts: " opts ))))
    (when tol 
      (when-not (number? tol)
        (throw (IllegalArgumentException.
                  (str  ":tol must be a number" \newline
                        "opts: " opts ))))
      (when-not (pos? tol)
        (throw (IllegalArgumentException.
                  (str  ":tol must be positive" \newline
                        "opts: " opts )))))
    (when digits 
      (when-not (integer? digits)
        (throw (IllegalArgumentException.
                  (str  ":digits must be an integer" \newline
                        "opts: " opts ))))
      (when-not (pos? digits)
        (throw (IllegalArgumentException.
                  (str  ":digits must positive" \newline
                        "opts: " opts )))))
    ; At this point, there were no invalid args and at least one of either :tol and
    ; :digits was specified.  So, return the answer.
    (let [val1      (double val1)
          val2      (double val2)
          delta-abs (Math/abs (- val1 val2)) 
          or-result  
            (truthy?  (or (zero? delta-abs)
                          (and tol
                            (let [tol-result (< delta-abs tol)
                            ] tol-result ))
                          (and digits
                            (let [abs1            (Math/abs val1)
                                  abs2            (Math/abs val2)
                                  max-abs         (Math/max abs1 abs2)
                                  delta-rel-abs   (/ delta-abs max-abs) 
                                  rel-tol         (Math/pow 10 (- digits))
                                  dig-result      (< delta-rel-abs rel-tol)
                            ] dig-result ))))
    ] 
      or-result )))

;************************************************************
; #awt TODO:  broken, needs fix!
;
(defn run-all 
  "[& fn-list]
  Convenience fn to run a function in the REPL after first reloading its namespace from
  disk.  Functions must be fully namespace-qualified.  Example:

    (run-all 'cooljure.core/main 'cooljure.csv/demo)

  This will reload cooljure.core, and cooljure.csv, then execute the corresponging main
  and test functions."
  [& fn-list]
  (spyx fn-list)
  (doseq [curr-fn fn-list]
    (let [  _ (spyx curr-fn)
          ns-str      (str/replace (str curr-fn) #"/.*$" "")
          curr-ns     (symbol ns-str) ]
      (println "reloading:" ns-str)
      (require curr-ns :reload-all)
      (println "running" curr-fn)
      (eval curr-fn)
    )))

