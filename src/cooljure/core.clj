;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns cooljure.core
  "Cooljure - Cool stuff you wish was in Clojure"
  (:require [clojure.string               :as str]
            [clojure.pprint               :as pprint]
            [clojure.test                 :as test]
            [cooljure.types               :as types]
  ))

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
 "For any predicate pred & collection coll, returns true if (pred x) is logical true for
 any x in coll; otherwise returns false.  Like clojure.core/some, but returns only true or
 false."
  [pred coll]
  (truthy? (some pred coll)) )

(defn not-empty?
 "For any collection coll, returns true if coll contains any items; otherwise returns
 false. Equivalent to (not (empty? coll))."
  [coll]
  (truthy? (seq coll)) )

(defn conjv 
 "For any collection coll and seq x, appends the x's to coll, always returning the result
 as a vector."
  ; From Stuart Sierra post 2014-2-10
  ( [coll x]
      (conj (vec coll) x) )
  ( [coll x & xs]
      (apply conj (vec coll) x xs) ))

(defn strcat
  "Concat all arguments into a single string result."
  [& args]
  (let [
    ; We need to use flatten twice since the inner one doesn't changes a string into a
    ; sequence of chars, nor does it affect byte-array, et al.  We eventually get
    ; seq-of-scalars which can look like [ \a \b 77 78 \66 \z ]
    seq-of-scalars  (flatten 
                      (for [it (flatten [args])] 
                        ; Note that "sequential?" returns false for sets, strings, and the various
                        ; array types.
                        (if (or (sequential? it)
                                (set?                   it)
                                (string?                it)
                                (types/byte-array?      it)
                                (types/char-array?      it)
                                (types/int-array?       it)
                                (types/long-array?      it)
                                (types/short-array?     it)
                                (types/object-array?    it))
                          (seq it)
                          it )))
    ; Coerce any integer values into character equivalents (e.g. 65 -> \A), then concat
    ; into a single string.
    result  (apply str 
              (map char seq-of-scalars))
  ]
    result ))

(defn pp-str
  "Returns a string that is the result of clojure.pprint/pprint"
  [arg]
  (with-out-str (pprint/pprint arg)))

(defn seqable?      ; from clojure.contrib.core/seqable
  "Returns true if (seq x) will succeed, false otherwise."
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))

(defn keyvals 
 "For any map m, returns the keys & values of m as a vector, suitable for reconstructing m
  via (apply hash-map (keyvals m))."
  [m]
  {:pre  [ (map? m) ] 
   :post [ (vector? %) ] }
  (reduce   #(conj %1 (first %2) (second %2))
            [] (seq m) ))

(defmacro with-exception-default
 "Evaluates body & returns its result.  In the event of an exception, default-val is
 returned instead of the exception."
  [default-val & body]
  `(try
     ~@body
     (catch Exception e# ~default-val) ))

(defn spy
  "A form of (println ...) to ease debugging display of either intermediate values in
  threading forms or function return values. There are three variants.  Usage:  

    (spy :msg <msg-string>)
        This variant is intended for use in either thread-first (->) or thread-last (->>)
        forms.  The keyword :msg is used to identify the message string and works equally
        well for both the -> and ->> operators. Spy prints both <msg-string>  and the
        threading value to stdout, then returns the value for further propogation in the
        threading form. For example, both of the following:
            (->   2 
                  (+ 3) 
                  (spy :msg \"sum\" )
                  (* 4))
            (->>  2 
                  (+ 3) 
                  (spy :msg \"sum\" )
                  (* 4))
        will print 'sum => 5' to stdout.

    (spy <msg-string> <value>)
        This variant is intended for simpler use cases such as function return values.
        Function return value expressions often invoke other functions and cannot be
        easily displayed since (println ...) swallows the return value and returns nil
        itself.  Spy will output both <msg-string> and the value, then return the value
        for use by further processing.  For example, the following:
            (println (* 2
                       (spy \"sum\" (+ 3 4))))
      will print:
            sum => 7
            14
      to stdout.

    (spy <value>)
        This variant is intended for use in very simple situations and is the same as the
        2-argument arity where <msg-string> defaults to 'spy'.  For example (spy (+ 2 3))
        prints 'spy => 5' to stdout.  "
  ( [arg1 arg2 arg3]
    (cond (= :msg arg1) (do (println (str arg2 " => " (pr-str arg3))) arg3)  ; for ->>
          (= :msg arg2) (do (println (str arg3 " => " (pr-str arg1))) arg1)  ; for ->
          :else (throw (IllegalArgumentException.  (str 
                           "spy: either first or 2nd arg must be :msg \n   args:"
                           (pr-str [arg1 arg2 arg3]))))))

  ( [msg value] ; 2-arg arity assumes value is last arg
    (do (println (str msg " => " (pr-str value))) value))

  ( [value] ; 1-arg arity uses a generic "spy" message
    (spy "spy" value)))

(defmacro spyx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the
  supplied expression, printing both the expression and its value to stdout, then returns
  the value."
  [expr]
  `(let [spy-val# ~expr] 
      (println (str '~expr " => " (pr-str spy-val#)))
      spy-val#))

(defmacro forv
  "Like clojure.core/for but returns results in a vector.  Equivalent to 
  (into [] (for ...)). Not lazy."
  [& body]
  `(into [] (for ~@body)))

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
  "Convenience fn to reload a namespace & the corresponding test namespace from disk and
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
#_(defn run-all 
  "Convenience fn to run a function in the REPL after first reloading its namespace from
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

