(ns flintstones.test-clj
  (:use tupelo.core)
  (:require
    [clojure.string :as str]
    [clojure.test :as ct] ))

(defn define-fixture-impl
  [mode interceptor-map meta-form]
  (assert (contains? #{:each :once} mode))
  (assert (map? interceptor-map))
  (let [enter-fn (grab :enter interceptor-map) ; #todo grab
        leave-fn   (:leave interceptor-map) ; #todo grab
        impl-out `(let [~'ctx      ~meta-form
                        ~'fixture-fn (fn [~'tst-fn]
                                     (~enter-fn ~'ctx) ; #todo must pass ctx output from :enter-fn to :leave-fn
                                     (~'tst-fn)
                                     (~leave-fn ~'ctx))]
       (ct/use-fixtures ~mode ~'fixture-fn))]
    impl-out))

(defmacro define-fixture
  [mode interceptor-map]
  (let [meta-form (meta &form) ]
    (define-fixture-impl mode interceptor-map meta-form)))

(defmacro with-interceptor ; #todo => tupelo.core ;  and also (with-interceptors [intc-1 intc-2 ...]  & forms)
  "Generic wrapper functionality"
  [interceptor-map & forms]
  (assert (map? interceptor-map)) ; #todo (validate map? interceptor-map)
  (let [enter-fn  (:enter interceptor-map) ; #todo grab
        leave-fn  (:leave interceptor-map) ; #todo grab
        ctx       (meta &form) ]
    `(do
       (enter-fn ctx)
       (let [result (do ~@forms)]
         (leave-fn ctx)
         result))))

(defn normalize-str
  "Returns a 'normalized' version of str-in, stripped of leading/trailing
   blanks, and with all non-alphanumeric chars converted to hyphens."
  [str-in]
  (-> str-in
    str/trim
    (str/replace #"[^a-zA-Z0-9]" "-")))

(defmacro testing [& forms] `(ct/testing ~@forms))

(defmacro is
  "Equivalent to clojure.test/is."
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/is requires exactly 1 form " ~line-str))))
    `(ct/is ~@forms)))

(defmacro isnt      ; #todo readme/test
  "Use (isnt ...) instead of (is (not ...)) for clojure.test"
  [& forms]
  (if (not= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/isnt requires exactly 1 form " ~line-str))))
    `(ct/is (not ~@forms))))

(defmacro is=       ; #todo readme/test
  "Use (is= ...) instead of (is (= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/is= requires at least 2 forms " ~line-str))))
    `(is (= ~@forms))))

(defmacro isnt=     ; #todo readme/test
  "Use (isnt= ...) instead of (is (not= ...)) for clojure.test"
  [& forms]
  (if (<= (count forms) 1)
    (let [line-str (str "[source line=" (:line (meta &form)) "]")]
      `(throw (IllegalArgumentException.
                (str "tupelo.test/isnt= requires at least 2 forms " ~line-str))))
    `(isnt (= ~@forms))))

(defmacro dotest    ; #todo README & tests
  "Alias for tupelo.test/deftest "
  [& items]
  (let [item-1 (clojure.core/first items)
        suffix (str "-line-" (:line (meta &form)))
        [label forms] (cond
                        (symbol? item-1) [(symbol (str (clojure.core/name item-1) suffix)) (vec (clojure.core/rest items))]
                        (string? item-1) [(symbol (str (normalize-str item-1) suffix)) (vec (clojure.core/rest items))]
                        :else [(symbol (str "dotest-block" suffix)) (vec items)])]
    `(def ~(vary-meta label assoc
             :test `(fn [] ~@forms))
       (fn [] (clojure.test/test-var (var ~label))))))

