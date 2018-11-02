(ns tupelo.test-cljs ; this file defines macros
  (:require [cljs.test :as ct]))

(comment ; #todo  new format?
  (define-fixtures  ; #todo cljs allows only one choice of :each of :once   :(
    {:once {:enter (fn [] (println "*** TEST ONCE *** - enter"))
            :leave (fn [] (println "*** TEST ONCE *** - leave"))}}
    {:each {:enter (fn [] (println "*** TEST EACH *** - enter"))
            :leave (fn [] (println "*** TEST EACH *** - leave"))}})
  ;#todo maybe define
  ;#todo   (def-fixture-global {intc-fixture-map} ...)  as global `use-fixtures`
  ;#todo   (def-fixture-local abc {abc-fixture-intc} ...)   defines entry in ns-local fixture map for (dotest-with abc ...)
)

(defmacro define-fixture ; #todo maybe (define-fixture ...)
  [mode interceptor-map]
  (assert (contains? #{:each :once} mode))
  (assert (map? interceptor-map))
  (let [enter-fn  (:enter interceptor-map) ; #todo grab
        leave-fn  (:leave interceptor-map) ; #todo grab
        ctx       (meta &form)]
    `(ct/use-fixtures ~mode
       {:before #(~enter-fn ~ctx)
        :after  #(~leave-fn ~ctx)})))

(defmacro deftest       [& forms] `(ct/deftest ~@forms))
(defmacro testing       [& forms] `(ct/testing ~@forms))
(defmacro is            [& forms] `(ct/is ~@forms))

(defmacro dotest [& body] ; #todo README & tests
  (let [test-name-sym (symbol (str "dotest-line-" (:line (meta &form))))]
    `(ct/deftest ~test-name-sym ~@body)))

(defmacro isnt      ; #todo readme/test
  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
  [& body]
  `(ct/is (not ~@body)))

(defmacro is=  ; #todo readme/test
  "Use (is= ...) instead of (is (= ...)) for clojure.test"
  [& forms]
  `(ct/is (= ~@forms)))

(defmacro isnt=  ; #todo readme/test
  "Use (isnt= ...) instead of (is (not= ...)) for clojure.test"
  [& body]
  `(ct/is (not (= ~@body))))

; #?(:cljs (do ))

  (defn throws?-impl
    [& forms]
    (if (= clojure.lang.Symbol (class (first forms)))
      ; symbol 1st arg => expected Throwable provided
      (do
        (println "symbol found")
        (println "Error - CLJS impl not allow specific exception type to be specified" (first forms))
        (throw (ex-info "Error - CLJS impl not allow specific exception type to be specified" (first forms))) )
      (do  ; expected Throwable not provided
        ; (println "symbol not found")
        `(ct/is
           (try
             ~@forms
             false    ; fail if no exception thrown
             (catch :default ex#    ; NOTE:  cannot catch java.lang.Throwable
               true)))))) ; if anything is thrown, test succeeds

  (defmacro throws?   ; #todo document in readme
    "Use (t/throws? ...) instead of (is (thrown? ...)) from clojure.test. Usage:
       (throws? (/ 1 0))  ; catches any Throwable "
    [& forms]
    (apply throws?-impl forms))
