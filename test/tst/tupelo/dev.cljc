;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.dev
  (:require
    [clojure.string :as str]
    #?@(:clj [[criterium.core :as crit]
              [schema.core :as s]
              [tupelo.core :refer :all]
              [tupelo.dev :refer :all]
              [tupelo.impl :as i]
              [tupelo.test :refer :all]]))
  #?(:clj
     (:import [java.io ByteArrayOutputStream PrintStream])))

(s/defn sequential->idx-map :- {s/Int s/Any}
  [data :- [s/Any]]
  (into (sorted-map)
    (map-indexed (fn [idx val] [idx val])
      data)))

;(defmacro destruct
;  [bindings & forms]
;  )


(defn ch->sym [ch] (symbol (str ch)))
(comment)

(defn dstr-analyze
  [{:keys [result path tmpl] :as arg}]
  (cond
    (map? tmpl) (doseq [entry tmpl]
                  (let [[curr-key curr-val] entry]
                    ;(spyx [curr-key curr-val])
                    (let [path-new (append path curr-key)]
                      ;(spyx path-new)
                      (if (symbol? curr-val)
                        (let [var-sym (if (= curr-val (ch->sym \?))
                                        (i/kw->sym curr-key)
                                        curr-val)]
                          (swap! result append {:path path-new :name var-sym}))
                        (dstr-analyze {:result result :path path-new :tmpl curr-val})))))

    (sequential? tmpl)
    (dstr-analyze {:result result :path path :tmpl (sequential->idx-map tmpl)})

    :else (println :oops-44)))

(defn get-in-strict [data path]
  (let [result (get-in data path ::not-found)]
    (when (= result ::not-found)
      (throw (ex-info "destruct(get-in-strict): value not found" {:data data :path path})))
    result))

(defn dstr-fn
  [data tmpl forms]
  (let [result (atom [])]
    (dstr-analyze {:result result :path [] :tmpl tmpl})
    ;(spyx-pretty @result)
    (let [extraction-pairs (apply glue
                             (for [{:keys [name path]} @result]
                               [name `(get-in-strict ~data ~path) ])) ]
      `(let [~@extraction-pairs]
         ~@forms))))

(defmacro destruct
  [value0 tmpl0 & forms0]
  (dstr-fn value0 tmpl0 forms0))

(dotest
  (is= {0 :a 1 :b 2 :c} (sequential->idx-map [:a :b :c]))
  (is= {0 :x 1 :y 2 :z} (sequential->idx-map [:x :y :z]))
  (is= 'a (ch->sym \a))
  (is= '? (ch->sym \?)))

(dotest-focus
  (let [data {:a 1
              :b {:c 3}}]
    (destruct data {:a ?
                    :b {:c ?}}
      (is= [1 3] [a c])))
  (let [data [:a :b :c]]
    (destruct data [v1 v2 v3]
      (is= [:a :b :c] [v1 v2 v3])))

  ; bad data examples
  (throws?
    (let [data {:a 1
                :b {:z 3}}]
      (destruct data {:a ?
                      :b {:c ?}}
        (is= [1 3] [a c]))))
  (throws?
    (let [data [:a :b]]
      (destruct data [v1 v2 v3]
        (is= [:a :b :c] [v1 v2 v3]))))
  )

;-----------------------------------------------------------------------------
(dotest
  (let [data-1  [1 2 3]
        data-2  [[1 2 3]
                 [10 11]
                 []]
        data-2b '((1 2 3)
                   (10 11)
                   ())
        data-2c [[1 2 3]
                 [10 11]
                 [9 2 8]]
        data-3  [[[1 2 3]
                  [4 5 6]
                  [7 8 9]]
                 [[10 11]
                  [12 13]]
                 [[20]
                  [21]]
                 [[30]]
                 [[]]]
        data-4  [[[1 2 3]
                  [4 5 6]
                  [7 8 9]]
                 [[10 11]
                  [12 2]]
                 [[20]
                  [21]]
                 [[30]]
                 [[2]]]
        ]
    (is= (find-idxs data-1 2) [{:idxs [1], :val 2}])

    (is= (find-idxs data-2 10) [{:idxs [1 0], :val 10}])
    (is= (find-idxs data-2 odd?) [{:idxs [0 0], :val 1}
                                  {:idxs [0 2], :val 3}
                                  {:idxs [1 1], :val 11} ])

    (is= (find-idxs data-2b 10) [{:idxs [1 0], :val 10}])

    (is= (find-idxs data-2c 2)
      [{:idxs [0 1], :val 2}
       {:idxs [2 1], :val 2}])

    (is= (find-idxs data-3 13) [{:idxs [1 1 1], :val 13}])
    (is= (find-idxs data-3 21) [{:idxs [2 1 0], :val 21}])
    (is= (find-idxs data-3 99) [])
    (is= (find-idxs data-4 2) [{:idxs [0 0 1], :val 2}
                               {:idxs [1 1 1], :val 2}
                               {:idxs [4 0 0], :val 2}])
    ))

(dotest
  (is= (combinations-duplicate [1 1 2] 2)
    [[1 1] [1 2] [1 2]])
  (is= (combinations-duplicate [1 1 1 2 2] 3)
    [[1 1 1] [1 1 2] [1 1 2] [1 1 2] [1 1 2]
     [1 2 2] [1 1 2] [1 1 2] [1 2 2] [1 2 2]]))

(dotest
  (is= [2 3 :x] (parse-string "2 3 :x"))
  (is= [2 3 :x] (with-in-str "2 3 :x"
                  (parse-string (read-line)))))

(defn vrange [n]    ; 1e6 => 30 ms
  (loop [i 0
         v []]
    (if (< i n)
      (recur (inc i) (conj v i))
      v)))

(defn vrange2 [n]   ; 1e6 => 16 ms
  (loop [i 0
         v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

;; benchmarked (Java 1.8, Clojure 1.7)
(when false
  (dotest
    (nl) (println :v1) (crit/quick-bench (vrange 1000000))
    (nl) (println :v2) (crit/quick-bench (vrange2 1000000))))

(dotest
  (try
    (throw (ex-info "something bad happened" {:a 1 :b 2}))
    (catch Exception ex
      (is= "something bad happened" (ex-msg ex))
      (is= {:a 1 :b 2} (ex-data ex))
      (is (str/includes? (ex-stacktrace ex)
            "clojure.lang.ExceptionInfo: something bad happened {:a 1, :b 2}"))
      (is (str/includes? (ex-stacktrace ex)
            "at tst.tupelo.")))))



