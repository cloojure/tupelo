;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.test-jvm
  "Testing functions for JVM usage only (not CLJS)."
  (:require
    [tupelo.core :as t]
    [tupelo.string :as ts]
    [clojure.test :as test]
    ))

;---------------------------------------------------------------------------------------------------
; non-CLJS follows

(def ^:dynamic *equality-digits-float=*
  "Default number of digits that must match for 2 Float values to be considered 'equal'.
  A 32-bit float has about 7 decimal digits of precision."
  4)

(defmacro is-float= ; #todo readme/test
  "Use (is-float= ...) instead of (is (rel= ... :digits *digits-float=*))"
  [& forms]
  (if (<= (count forms) 1)
    ; #todo make `throw-macro-error` helper function...???
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (ex-info "tupelo.test/is-float= requires at least 2 forms " {:line-str ~line-str})))
    `(test/is (tupelo.core/rel= ~@forms :digits *equality-digits-float=*))))
(defmacro isnt-float= ; #todo readme/test
  "Use (isnt-float= ...) instead of (is (rel= ... :digits *digits-float=*))"
  [& forms]
  (if (<= (count forms) 1)
    ; #todo make `throw-macro-error` helper function...???
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (ex-info "tupelo.test/isnt-float= requires at least 2 forms " {:line-str ~line-str})))
    `(test/is (not (tupelo.core/rel= ~@forms :digits *equality-digits-float=*)))))

(def ^:dynamic *equality-digits-double=*
  "Default number of digits that must match for 2 Double values to be considerd 'equal'.
    A 64-bit float has about 15 decimal digits of precision."
  10)

(defmacro is-double= ; #todo readme/test
  "Use (is-double= ...) instead of (is (rel= ... :digits *digits-double=*))"
  [& forms]
  (if (<= (count forms) 1)
    ; #todo make `throw-macro-error` helper function...???
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (ex-info "tupelo.test/is-double= requires at least 2 forms " {:line-str ~line-str})))
    `(test/is (tupelo.core/rel= ~@forms :digits *equality-digits-double=*))))
(defmacro isnt-double= ; #todo readme/test
  "Use (isnt-double= ...) instead of (is (rel= ... :digits *digits-double=*))"
  [& forms]
  (if (<= (count forms) 1)
    ; #todo make `throw-macro-error` helper function...???
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (ex-info "tupelo.test/isnt-double= requires at least 2 forms " {:line-str ~line-str})))
    `(test/is (not (tupelo.core/rel= ~@forms :digits *equality-digits-double=*)))))

;-----------------------------------------------------------------------------
; #todo what to do with this?
(defmacro use-fixtures
  "Alias for clojure.test/use-fixtures"
  [& forms] `(test/use-fixtures ~@forms))

; (defn use-fixtures-all [& args] (apply test/use-fixtures args)) #todo why is this here???
(defn ^:no-doc define-fixture-impl
  [ctx mode interceptor-map]
  (let [enter-fn (or (:enter interceptor-map) `identity)
        leave-fn (or (:leave interceptor-map) `identity)]
    `(test/use-fixtures ~mode
       (fn ~'fixture-fn [tgt-fn#] ; #todo
         (~enter-fn ~ctx)
         (tgt-fn#)
         (~leave-fn ~ctx)))))

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
