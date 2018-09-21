;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.test
  "Testing functions."
  #?@(:clj [
       (:require
         [clojure.test.check :as ctc]
         [clojure.test :as ct]
         [schema.core :as s]
         [tupelo.impl :as i]
         [tupelo.string :as ts])
       ]))

(defn use-fixtures [& args] (apply ct/use-fixtures args))
(defmacro deftest [& forms] `(ct/deftest ~@forms))
(defmacro testing [& forms] `(ct/testing ~@forms))

;(defmacro is
;  "Equivalent to clojure.test/is."
;  [arg]
;  `(ct/is ~arg))

(defmacro is
  "Equivalent to clojure.test/is."
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/is requires exactly 1 form " ~line-str))))
    `(ct/is ~@forms)))

;(defmacro isnt      ; #todo readme/test
;  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
;  [arg]
;  `(ct/is (not ~arg)))
(defmacro isnt      ; #todo readme/test
  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/isnt requires exactly 1 form " ~line-str))))
    `(ct/is (not ~@forms))))


(defmacro is=  ; #todo readme/test
  "Use (is= ...) instead of (is (= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
     `(throw (IllegalArgumentException.
               (str "tupelo.test/is= requires at least 2 forms " ~line-str))))
     `(is (= ~@forms))))

(defmacro isnt=         ; #todo readme/test
  "Use (isnt= ...) instead of (is (not= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/isnt= requires at least 2 forms " ~line-str))))
    `(isnt (= ~@forms))))

;(defmacro sets=
;  "Converts each input collection to a set, then tests for equality."
;  [& forms]
;  `(is= ~@(mapv #(list 'set %) forms)))

(defmacro set=  ; #todo readme/test
  "Converts each input collection to a set, then tests for equality."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/set= requires at least 2 forms " ~line-str))))
    `(is= ~@(mapv #(list 'set %) forms))))

; #todo need test
;(defmacro nonblank=
;  "Returns true if each input string is equal treating all whitespace as equivalent."
;  [& strings ]
;  (is (apply ts/equals-ignore-spacing? strings)))

(defmacro nonblank=  ; #todo readme/test
  "Returns true if each input string is equal treating all whitespace as equivalent."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/set= requires at least 2 forms " ~line-str))))
    `(is (ts/equals-ignore-spacing? ~@forms) )))

#?(:clj (do

(defn throws?-impl
  [& forms]
  (if (= clojure.lang.Symbol (class (first forms)))
    ; symbol 1st arg => expected Throwable provided
    (do
      ; (println "symbol found")
      `(ct/is
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
      `(ct/is
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

; #todo maybe def-anon-test, or anon-test
(defmacro dotest ; #todo README & tests
  "Like clojure.test/deftest, but doesn't require a test name. Usage:

      (ns xyz..
        (:use tupelo.test))

      (dotest
        (is= 5 (+ 2 3))          ; contraction of (is (= ...))
        (isnt false)             ; contraction of (is (not ...))
        (set= [1 2 3] [3 2 1])   ; set equality semantics
        (throws? (/ 1 0)))
  "
  [& body]
  (let [name (symbol (str "dotest-line-" (:line (meta &form))))]
    `(def ~(vary-meta name assoc
             :test `(fn [] ~@body) )
       (fn [] (clojure.test/test-var (var ~name))))))

(defmacro dotest-focus ; #todo README & tests
  "Like `dotest`, but includes metadata  ^:test-refresh/focus  to put lein-test-refresh into 'focus' mode"
  [& body]
  (let [name (symbol (str "dotest-line-" (:line (meta &form))))]
    `(def ~(vary-meta name assoc
             :test `(fn [] ~@body)
             :test-refresh/focus true )
       (fn [] (clojure.test/test-var (var ~name))))))

; #todo ^:slow not working (always executed); need to fix
; #todo maybe def-anon-spec or anon-spec; maybe (gen-spec 999 ...) or (gen-test 999 ...)
; #todo maybe integrate with `dotest` like:   (dotest 999 ...)  ; 999 1st item implies generative test
(defmacro dospec [& body] ; #todo README & tests
  (let [test-name-sym (symbol (str "dospec-line-" (:line (meta &form))))]
  `(clojure.test.check.clojure-test/defspec ^:slow ~test-name-sym ~@body)))

(defmacro check-is [& body] ; #todo README & tests
  `(ct/is (i/grab :result (ctc/quick-check ~@body))))

(defmacro check-isnt [& body] ; #todo README & tests
  `(ct/is (not (i/grab :result (ctc/quick-check ~@body)))))

; #todo: gen/elements -> clojure.check/rand-nth

))
