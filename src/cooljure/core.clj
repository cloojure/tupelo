;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure"
      :author "Alan Thompson"}
  cooljure.core
    (:require [clojure.test      :as test] ) )

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
  Returns true if (pred x) is logical true for any x in collection; otherwise returns
  false.  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (truthy? (some pred coll)) )

(defn not-empty?
 "[coll]
  Returns true if collection contains any items; otherwise returns false
  Equivalent to (not (empty? coll))."
  [coll]
  (truthy? (seq coll)) )

(defn conjv 
 "( [coll x]
    [coll x & xs] )
  Appends items to collection, always returning the result as a vector."
  ; From Stuart Sierra post 2014-2-10
  ( [coll x]
      (conj (vec coll) x) )
  ( [coll x & xs]
      (apply conj (vec coll) x xs) ))

(defn keyvals 
 "[m]
  Returns a map's keys & values as a vector, suitable for reconstructing the map via
  (apply hashmap (keyvals m))."
  [m]
  {:pre [ (map? m) ] }
  (vec (flatten (seq m))) )

(defmacro with-exception-default
 "[default & body]
  Evaluates body, returning specified default value in the event of an exception."
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

; add eager (forall  ...) -> (doall (for ...))      ; AWTAWT TODO:  
;           (for-all ...)
;           (for-now ...)

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

