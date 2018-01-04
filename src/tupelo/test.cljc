;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.test
  "Testing functions."
  (:require [clojure.test.check :as tc]
            [clojure.test :as ct]
            [tupelo.impl :as i] ))

(defn use-fixtures [& args] (apply ct/use-fixtures args))
(defmacro deftest [& forms] `(ct/deftest ~@forms))
(defmacro is [& forms] `(ct/is ~@forms))
(defmacro testing [& forms] `(ct/testing ~@forms))


(defmacro isnt      ; #todo readme/test
  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
  [& body]
  `(clojure.test/is (not ~@body)))

(defmacro is=  ; #todo readme/test
  "Use (is= ...) instead of (is (= ...)) for clojure.test"
  [& body]
  `(clojure.test/is (= ~@body)))

(defmacro isnt=  ; #todo readme/test
  "Use (isnt= ...) instead of (is (not= ...)) for clojure.test"
  [& body]
  `(clojure.test/is (not (= ~@body))))

(defn throws?-impl
  [& forms]
  (if (= clojure.lang.Symbol (class (first forms)))
    ; symbol 1st arg => expected Throwable provided
    (do
      ; (println "symbol found")
      `(clojure.test/is
         (try
           ~@(rest forms)
           false        ; fail if no exception thrown
           (catch ~(first forms) t1#
             true) ; if catch expected type, test succeeds
           (catch Throwable t2#
             false))) ; if thrown type is unexpected, test fails
      )
    (do ; expected Throwable not provided
      ; (println "symbol not found")
      `(clojure.test/is
         (try
           ~@forms
           false        ; fail if no exception thrown
           (catch Throwable t3#
             true))) ; if anything is thrown, test succeeds
      )))

(defmacro throws?  ; #todo document in readme
  "Use (throws? ...) instead of (is (thrown? ...)) for clojure.test. Usage:

     (throws? (/ 1 0))                      ; catches any Throwable
     (throws? ArithmeticException (/ 1 0))  ; catches specified Throwable (or subclass) "
  [& forms]
  (apply throws?-impl forms))

; #todo maybe def-anon-test
(defmacro dotest [& body] ; #todo README & tests
  (let [test-name-sym (symbol (str "dotest-line-" (:line (meta &form))))]
  `(clojure.test/deftest ~test-name-sym ~@body)))

; #todo maybe def-anon-spec
(defmacro dospec [& body] ; #todo README & tests
  (let [test-name-sym (symbol (str "dospec-line-" (:line (meta &form))))]
  `(clojure.test.check.clojure-test/defspec ^:slow ~test-name-sym ~@body)))

(defmacro check-is [& body] ; #todo README & tests
  `(clojure.test/is (i/grab :result (tc/quick-check ~@body))))

(defmacro check-isnt [& body] ; #todo README & tests
  `(clojure.test/is (not (i/grab :result (tc/quick-check ~@body)))))

; #todo: gen/elements -> clojure.check/rand-nth