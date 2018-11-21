;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.test
  "Testing functions."
  (:require
     [clojure.test :as ct]
     [clojure.test.check :as ctc]
     [tupelo.core :as t ]
     [tupelo.string :as tstr]
  ))

(defn use-fixtures [& args] (apply ct/use-fixtures args))
(defmacro testing [& forms] `(ct/testing ~@forms))

(defn define-fixture-impl
  [ctx mode interceptor-map]
  (let [enter-fn (or (:enter interceptor-map) `identity)
        leave-fn (or (:leave interceptor-map) `identity) ]
    `(ct/use-fixtures ~mode
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

; #todo maybe def-anon-test, or anon-test
(defmacro deftest ; #todo README & tests
  "Like clojure.test/deftest, but doesn't require a test name. Usage:

      (ns xyz..
        (:use tupelo.test))

      (dotest
        (is= 5 (+ 2 3))          ; contraction of (is (= ...))
        (isnt false)             ; contraction of (is (not ...))
        (set= [1 2 3] [3 2 1])   ; set equality semantics
        (throws? (/ 1 0)))
  "
  [& items]
  (let [item-1 (clojure.core/first items)
        suffix (str "-line-" (:line (meta &form)))
        [label forms] (cond
                        (symbol? item-1) [(symbol (str (clojure.core/name           item-1) suffix)) (vec (clojure.core/rest items))]
                        (string? item-1) [(symbol (str (tupelo.string/normalize-str item-1) suffix)) (vec (clojure.core/rest items))]
                        :else [(symbol (str "deftest-block" suffix)) (vec items)]) ]
    `(def ~(vary-meta label assoc
             :test `(fn [] ~@forms))
       (fn [] (clojure.test/test-var (var ~label))))))

(defmacro dotest ; #todo README & tests
  "Alias for tupelo.test/deftest "
  [& items]
  (let [item-1 (clojure.core/first items)
        suffix (str "-line-" (:line (meta &form)))
        [label forms] (cond
                        (symbol? item-1) [(symbol (str (clojure.core/name           item-1) suffix)) (vec (clojure.core/rest items))]
                        (string? item-1) [(symbol (str (tupelo.string/normalize-str item-1) suffix)) (vec (clojure.core/rest items))]
                        :else [(symbol (str "dotest-block" suffix)) (vec items)]) ]
    `(def ~(vary-meta label assoc
             :test `(fn [] ~@forms))
       (fn [] (clojure.test/test-var (var ~label))))))


(defmacro is
  "Equivalent to clojure.test/is."
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo.test/is requires exactly 1 form " ~line-str))))
    `(ct/is ~@forms)))

(defmacro isnt      ; #todo readme/test
  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo.test/isnt requires exactly 1 form " ~line-str))))
    `(ct/is (not ~@forms))))

(defmacro is=  ; #todo readme/test
  "Use (is= ...) instead of (is (= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
     `(throw (ex-info (str "tupelo.test/is= requires at least 2 forms " ~line-str))))
     `(is (= ~@forms))))

(defmacro isnt=         ; #todo readme/test
  "Use (isnt= ...) instead of (is (not= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo.test/isnt= requires at least 2 forms " ~line-str))))
    `(isnt (= ~@forms))))

(defmacro set=  ; #todo readme/test
  "Converts each input collection to a set, then tests for equality."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo.test/set= requires at least 2 forms " ~line-str))))
    `(is= ~@(mapv #(list 'set %) forms))))

; #todo need test
(defmacro nonblank=  ; #todo readme/test
  "Returns true if each input string is equal treating all whitespace as equivalent."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo.test/set= requires at least 2 forms " ~line-str))))
    `(is (tstr/equals-ignore-spacing? ~@forms) )))

;---------------------------------------------------------------------------------------------------
; non-CLJS follows ; #?(:clj (do ))

(defn throws?-impl
  [& forms]
  (if (= clojure.lang.Symbol (class (first forms)))
    ; symbol 1st arg => expected Throwable provided
    (do
      ; (println "symbol found")
      `(ct/is
         (try
           ~@(rest forms)
           false    ; fail if no exception thrown
           (catch ~(first forms) t1#
             true)  ; if catch expected type, test succeeds
           (catch Throwable t2#
             false))) ; if thrown type is unexpected, test fails
      )
    (do             ; expected Throwable not provided
      ; (println "symbol not found")
      `(ct/is
         (try
           ~@forms
           false    ; fail if no exception thrown
           (catch Throwable t3#
             true))) ; if anything is thrown, test succeeds
      )))

(defmacro throws?   ; #todo document in readme
  "Use (throws? ...) instead of (is (thrown? ...)) for clojure.test. Usage:

     (throws? (/ 1 0))                      ; catches any Throwable
     (throws? ArithmeticException (/ 1 0))  ; catches specified Throwable (or subclass) "
  [& forms]
  (apply throws?-impl forms))

(defmacro deftest-focus ; #todo README & tests
  "Like `deftest`, but invokes lein-test-refresh focus mode; i.e. applies metadata {:test-refresh/focus true}"
  [& items]
  (let [item-1 (clojure.core/first items)
        suffix (str "-line-" (:line (meta &form)))
        [label forms] (cond
                        (symbol? item-1) [(symbol (str (clojure.core/name item-1) suffix)) (vec (clojure.core/rest items))]
                        (string? item-1) [(symbol (str (tupelo.string/normalize-str item-1) suffix)) (vec (clojure.core/rest items))]
                        :else [(symbol (str "deftest-focus-block" suffix)) (vec items)])]
    `(def ~(vary-meta label assoc
             :test `(fn [] ~@forms)
             :test-refresh/focus true)
       (fn [] (clojure.test/test-var (var ~label))))))

(defmacro dotest-focus ; #todo README & tests
  "Alias for tupelo.test/deftest-focus "
  [& items]
  (let [item-1 (clojure.core/first items)
        suffix (str "-line-" (:line (meta &form)))
        [label forms] (cond
                        (symbol? item-1) [(symbol (str (clojure.core/name item-1) suffix)) (vec (clojure.core/rest items))]
                        (string? item-1) [(symbol (str (tupelo.string/normalize-str item-1) suffix)) (vec (clojure.core/rest items))]
                        :else [(symbol (str "dotest-focus-block" suffix)) (vec items)])]
    `(def ~(vary-meta label assoc
             :test `(fn [] ~@forms)
             :test-refresh/focus true)
       (fn [] (clojure.test/test-var (var ~label))))))

; #todo ^:slow not working (always executed); need to fix
; #todo maybe def-anon-spec or anon-spec; maybe (gen-spec 999 ...) or (gen-test 999 ...)
; #todo maybe integrate with `dotest` like:   (dotest 999 ...)  ; 999 1st item implies generative test
(defmacro dospec [& body] ; #todo README & tests
  (let [test-name-sym (symbol (str "dospec-line-" (:line (meta &form))))]
    `(clojure.test.check.clojure-test/defspec ^:slow ~test-name-sym ~@body)))

(defmacro check-is [& body] ; #todo README & tests
  `(ct/is (t/grab :result (ctc/quick-check ~@body))))

(defmacro check-isnt [& body] ; #todo README & tests
  `(ct/is (not (t/grab :result (ctc/quick-check ~@body)))))

; #todo: gen/elements -> clojure.check/rand-nth

