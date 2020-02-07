;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.test-cljs ; this file defines macros
  (:require
;  #?(:clj
; [clojure.test :as test]
;     :cljs
  [cljs.test :as test]
;     )
    [tupelo.string :as ts]
    ))

; #todo merge into a single namespace using `is-cljs` macro when necessary

(defmacro deftest [& forms] `(test/deftest ~@forms))
(defmacro testing [& forms] `(test/testing ~@forms))

(defmacro dotest [& body] ; #todo README & tests
  (let [test-name-sym (symbol (str "dotest-line-" (:line (meta &form))))]
    `(test/deftest ~test-name-sym ~@body)))

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
(defmacro is-nonblank= ; #todo readme/test
  "Returns true if each input string is equal treating all whitespace as equivalent."
  [& forms]
  (if (<= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (ex-info (str "tupelo is-nonblank= requires at least 2 forms " ~line-str))))
    `(test/is (ts/nonblank= ~@forms))))

; #todo use tstr/nonblank=
(defmacro is-nonblank-lines=  ; #todo readme/test
  "Returns true if each line of each input string is equal treating all whitespace as equivalent."
  [& forms]
  (if (<= (count forms) 1 )
    (let [line-str (str "[source line=" (:line (meta &form))  "]")]
      `(throw (ex-info (str "tupelo is-nonblank-lines= requires at least 2 forms " ~line-str))))
    `(test/is (ts/nonblank-lines= ~@forms) )))

(defmacro throws? ; #todo document in readme
  "Use (throws? ...) instead of (is (thrown? ...)) for clojure.test. Usage:
     (throws? (/ 1 0))                      ; catches any Throwable"
  [& forms]
  `(test/is
     (try
       ~@forms
       false ; fail if no exception thrown
       (catch :default dummy# ; NOTE:  cannot catch java.lang.Throwable
         true)))) ; if anything is thrown, test succeeds

(defmacro throws-not?   ; #todo document in readme
  "The opposite of (throws? ...)"
  [& forms]
  `(test/is
     (try
       ~@forms
       true    ; succeed if no exception thrown
       (catch :default dummy# ; NOTE:  cannot catch java.lang.Throwable
         false)))) ; if anything is thrown, test fails

(comment            ; #todo  new format?
  (define-fixtures  ; #todo cljs allows only one choice of :each of :once   :(
    {:once {:enter (fn [] (println "*** TEST ONCE *** - enter"))
            :leave (fn [] (println "*** TEST ONCE *** - leave"))}}
    {:each {:enter (fn [] (println "*** TEST EACH *** - enter"))
            :leave (fn [] (println "*** TEST EACH *** - leave"))}})
  ;#todo maybe define
  ;#todo   (def-fixture-global {intc-fixture-map} ...)  as global `use-fixtures`
  ;#todo   (def-fixture-local abc {abc-fixture-intc} ...)   defines entry in ns-local fixture map for (dotest-with abc ...)
  )

; ********** WARNING: different than clojure.test/use-fixtures **********
(defmacro define-fixture
  [mode interceptor-map]
  (assert (contains? #{:each :once} mode))
  (assert (map? interceptor-map))
  (let [enter-fn (or (:enter interceptor-map) `identity)
        leave-fn (or (:leave interceptor-map) `identity)
        ctx      (meta &form)]
    `(test/use-fixtures ~mode
       {:before #(~enter-fn ~ctx)
        :after  #(~leave-fn ~ctx)})))

;---------------------------------------------------------------------------------------------------
; #todo incorporate this example & ritual for tupelo and enflame and cljs-template

;(comment
;
;  (ns minerva.core-test
;    (:require
;      [tupelo.core :include-macros true :as t :refer [spy spyx spyxx try-catchall]]
;      [tupelo.test-cljs :include-macros true :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]]
;      [tupelo.schema :as tsk]
;      ;-----------------------------------------------------------------------------
;      [clojure.string :as str]
;      [clojure.test :as ct]
;      [minerva.core :as core]
;      [schema.core :as s]
;      ))
;
;  (enable-console-print!)
;
;  (s/defn concat-str :- s/Str
;          [ctx :- tsk/KeyMap]
;          (let [{:keys [a b]} ctx]
;            (str
;              (s/validate s/Str a)
;              (s/validate s/Str b))))
;
;  (dotest
;    (println "-----------------------------------------------------------------------------")
;    (println "minerva.core-test enter")
;    (is= (s/validate s/Str "hello!") "hello!")
;    (is= (s/validate s/Int (* 2 3 7)) 42)
;    (is= 5 (+ 2 3))
;    (is= 11 (+ 5 6))
;    (is= "aabbb" (concat-str {:a "aa" :b "bbb"}))
;    (throws? (throw (ex-info "Some Exception" {:data 42})))
;
;    (try
;      (throw (ex-info "Thrower!" {:waste 86}))
;      (catch js/Object ex
;        (println "Exception=" ex)))
;
;    (try-catchall
;      (throw (ex-info "Tosser!" {:bad 666}))
;      (catch problem
;             (println "Exception=" problem)))
;
;    (isnt false)
;    (isnt= 3 4)
;
;    ;(throw (ex-info "Failer!" {:result false}))   ; works
;
;    (println "minerva.core-test leave")
;    (println "-----------------------------------------------------------------------------")
;    )
;  )

