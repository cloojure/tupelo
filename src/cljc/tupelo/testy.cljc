;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.testy
  "Testing functions."
  #?(:cljs (:require-macros [tupelo.testy]))
  (:require
    [tupelo.core :as t]
    [tupelo.string :as ts]
    [clojure.test :as test]))

(defmacro deftest [& forms] `(test/deftest ~@forms))
(defmacro testing [& forms] `(test/testing ~@forms))

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
  (let [test-name-sym (symbol (str "dotest-line-" (:line (meta &form))))]
    `(def ~(vary-meta test-name-sym assoc
             :test `(fn [] ~@body))
       (fn [] (test/test-var (var ~test-name-sym))))))

(defmacro dotest-focus ; #todo README & tests
  "Alias for tupelo.test/deftest-focus "
  [& body]
  (let [test-name-sym (symbol (str "dotest-line-" (:line (meta &form))))]
    `(def ~(vary-meta test-name-sym assoc
             :test `(fn [] ~@body)
             :test-refresh/focus true)
       (fn [] (test/test-var (var ~test-name-sym))))))

; For all the following arity tests, we use an `if` statement so the exception is thrown during
; the test execution, not during compilation.
(defmacro is
  "Equivalent to clojure.test/is."
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info "tupelo.test/is requires exactly 1 form " {:line-str ~line-str })))
    `(test/is ~@forms)))

(defmacro isnt      ; #todo readme/test
  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info "tupelo.test/isnt requires exactly 1 form " {:line-str ~line-str })))
    `(test/is (not ~@forms))))

(defmacro is=  ; #todo readme/test
  "Use (is= ...) instead of (is (= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
     `(throw (ex-info "tupelo.test/is= requires at least 2 forms " {:line-str ~line-str })))
     `(test/is (= ~@forms))))

(defmacro isnt=         ; #todo readme/test
  "Use (isnt= ...) instead of (is (not= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info "tupelo.test/isnt= requires at least 2 forms " {:line-str ~line-str })))
    `(test/is (not (= ~@forms)))))

; #todo use t/set=
(defmacro is-set=  ; #todo readme/test
  "Converts each input collection to a set, then tests for equality."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info  "tupelo is-set= requires at least 2 forms " ~line-str)))
    `(test/is (= ~@(mapv #(list 'set %) forms)))))

; #todo use tstr/nonblank=
(defmacro is-nonblank=  ; #todo readme/test
  "Returns true if each input string is equal treating all whitespace as equivalent."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo is-nonblank= requires at least 2 forms " ~line-str))))
    `(test/is (ts/nonblank= ~@forms) )))

; #todo use tstr/nonblank=
(defmacro is-nonblank-lines=  ; #todo readme/test
  "Returns true if each line of each input string is equal treating all whitespace as equivalent."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo is-nonblank-lines= requires at least 2 forms " ~line-str))))
    `(test/is (ts/nonblank-lines= ~@forms) )))

(defmacro throws-2019? ; #todo document in readme
  "Use (throws? ...) instead of (is (thrown? ...)) for clojure.test. Usage:
     (throws? (/ 1 0))                      ; catches any Throwable"
  [& forms]
  `(test/is
     (try
       ~@forms
       false ; fail if no exception thrown

       ; if anything is thrown, test succeeds
       (catch
         #?(:clj  Throwable
            :cljs :default) ; NOTE:  cannot catch java.lang.Throwable
         dummy# true))))

(defmacro throws? ; #todo document in readme
  "Use (throws? ...) instead of (is (thrown? ...)) for clojure.test. Usage:
     (throws? (/ 1 0))                      ; catches any Throwable"
  [& forms]
  `(test/is
     (tupelo.core/try-catchall
       ~@forms
       false
       (catch e# true))))

(defmacro throws-not? ; #todo document in readme
  "The opposite of (throws? ...)"
  [& forms]
  `(test/is
     (try
       ~@forms
       true ; succeed if no exception thrown

       ; if anything is thrown, test fails
       #?(:clj (catch
                 #?(:clj  Throwable
                    :cljs :default) ; NOTE:  cannot catch java.lang.Throwable
                 dummy# false)))))

;---------------------------------------------------------------------------------------------------
; non-CLJS follows ;
#?(:clj
   (do

     ; (defn use-fixtures-all [& args] (apply test/use-fixtures args)) #todo why is this here???
     (defn ^:no-doc define-fixture-impl
       [ctx mode interceptor-map]
       (let [enter-fn (or (:enter interceptor-map) `identity)
             leave-fn (or (:leave interceptor-map) `identity)]
         `(test/use-fixtures ~mode
            (fn ~'fixture-fn [tgt-fn#] ; #todo
              (~enter-fn ~ctx)
              (tgt-fn#)
              (~leave-fn ~ctx))))
       )

     (defmacro define-fixture
       [mode interceptor-map]
       (assert (contains? #{:each :once} mode))
       (assert (map? interceptor-map))
       (let [ctx (meta &form)]
         (define-fixture-impl ctx mode interceptor-map)))

     ; #todo ^:slow not working (always executed); need to fix
     ; #todo maybe def-anon-spec or anon-spec; maybe (gen-spec 999 ...) or (gen-test 999 ...)
     ; #todo maybe integrate with `dotest` like:   (dotest 999 ...)  ; 999 1st item implies generative test
     (defmacro dospec [& body] ; #todo README & tests
       (let [test-name-sym (symbol (str "dospec-line-" (:line (meta &form))))]
         `(clojure.test.check.clojure-test/defspec ^:slow ~test-name-sym ~@body)))

     (defmacro check-is [& body] ; #todo README & tests
       `(test/is (t/grab :result (clojure.test.check/quick-check ~@body))))

     (defmacro check-isnt [& body] ; #todo README & tests
       `(test/is (not (t/grab :result (clojure.test.check/quick-check ~@body)))))

     ; #todo: gen/elements -> clojure.check/rand-nth

))
