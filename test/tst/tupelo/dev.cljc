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

(defn char->sym [ch] (symbol (str ch)))

(defn get-in-strict [data path]
  (let [result (get-in data path ::not-found)]
    (when (= result ::not-found)
      (throw (ex-info "destruct(get-in-strict): value not found" {:data data :path path})))
    result))

(defn tmpl-analyze
  [ctx]
  (with-map-vals ctx [parsed path tmpl]
    (spyx path)
    (cond
      (map? tmpl)
      (doseq [entry tmpl]
        (spyx entry)
        (let [[curr-key curr-val] entry]
          (spyx [curr-key curr-val])
          (let [path-new (append path curr-key)]
            (spyx path-new)
            (if (symbol? curr-val)
              (let [var-sym (if (= curr-val (char->sym \?))
                              (i/kw->sym curr-key)
                              curr-val)]
                (swap! parsed append {:path path-new :name var-sym}))
              (tmpl-analyze {:parsed parsed :path path-new :tmpl curr-val})))))

      (sequential? tmpl)
      (do
        (spy :tmpl-51 tmpl)
        (tmpl-analyze {:parsed parsed :path path :tmpl (sequential->idx-map tmpl)}))

      :else (println :oops-44))))

(defn dstr-fn
  [bindings forms]
  (spyx bindings)
  (spyx forms)
  (when (not (even? (spyx (count bindings))))
    (throw (ex-info "destruct: uneven number of bindings:" bindings)))
  (when (empty? bindings)
    (throw (ex-info "destruct: bindings empty:" bindings)))
  (let [binding-pairs (partition 2 bindings)
        datas         (mapv first binding-pairs)
        tmpls         (mapv second binding-pairs)
        tmpls-parsed  (vec (for [tmpl tmpls]
                             (let [parsed (atom [])]
                               (tmpl-analyze {:parsed parsed :path [] :tmpl tmpl})
                               (spyx-pretty @parsed))))]
    (spyx tmpls-parsed)
    ; look for duplicate variable names
    (let [var-names (vec (for [tmpl-parsed   tmpls-parsed
                               path-name-map tmpl-parsed]
                           (grab :name path-name-map)))]
     ;(spyx var-names)
      (when (not= var-names (distinct var-names))
        (throw (ex-info "destruct: var-names not unique" var-names))))

    (let [data-parsed-pairs (zip datas tmpls-parsed)]
      (spyx data-parsed-pairs)
      (vec (for [[data parsed] data-parsed-pairs]
             (do
               (spyx data)
               (spyx parsed)
               (spyx-pretty :result
                 (let [extraction-pairs (apply glue
                                          (for [{:keys [name path]} parsed]
                                            [name `(get-in-strict ~data ~path)]))]
                   `(let [~@extraction-pairs]
                      ~@forms)))))))))

(defmacro destruct
  [bindings & forms0]
  (dstr-fn bindings forms0))

(dotest
  (is= {0 :a 1 :b 2 :c} (sequential->idx-map [:a :b :c]))
  (is= {0 :x 1 :y 2 :z} (sequential->idx-map [:x :y :z]))
  (is= 'a (char->sym \a))
  (is= '? (char->sym \?)))

(dotest-focus
  (dstr-fn '[
             {:a 1 :b {:c 3}}
             {:a ? :b {:c ?}}

             {:x 1 :y {:z 3}}
             {:x ? :y {:z ?}}

             ]
    '(println [1 3] [a c])))

;(dotest             ; -focus
;  (let [data {:a 1
;              :b {:c 3}}]
;    (destruct [data {:a ?
;                     :b {:c ?}}]
;      (is= [1 3] [a c])))
;  (let [data [:a :b :c]]
;    (destruct [data [v1 v2 v3]]
;      (is= [:a :b :c] [v1 v2 v3])))
;
;  ; bad data examples
;  (throws?
;    (let [data {:a 1
;                :b {:z 3}}]
;      (destruct [data {:a ?
;                       :b {:c ?}}]
;        (spyx [a c]))))
;  (throws?
;    (let [data [:a :b]]
;      (destruct [data [v1 v2 v3]]
;        (spyx [v1 v2 v3]))))
;  )

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



