;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.dev
  #?@(:clj [
            (:use tupelo.dev tupelo.core tupelo.test)
            (:require
              [criterium.core :as crit]
              [clojure.string :as str]
              [tupelo.impl :as i]
              [schema.core :as s])
            ])
  (:import [java.io ByteArrayOutputStream PrintStream]))

(s/defn sequential->idx-map :- {s/Int s/Any}
  [data :- [s/Any]]
  (into (sorted-map)
    (map-indexed (fn [idx val] [idx val])
      data)))

;(defmacro destruct
;  [bindings & forms]
;  )


(comment )

(defn dstr-impl
  [{:keys [result path tmpl] :as arg}]
  (spyx-pretty arg)
  (cond
    (map? tmpl) (doseq [entry tmpl]
                  (let [[key val] entry]
                    (i/spyx [key val])
                    (let [path-new (append path key)]
                      (if (= :? val)
                        (swap! result append {:path path-new :name (i/kw->sym key)})
                        (dstr-impl {:result result :path path-new :tmpl val})))))

    (sequential? tmpl) (dstr-impl {:result result :path path :tmpl (sequential->idx-map val)})

    :else (println :oops-44)))

(defn dstr
  [value tmpl & forms]
  (spyx forms)
  (let [result (atom [])]
    (dstr-impl {:result result :path [] :tmpl tmpl})
    (spyx-pretty @result)
    (spy :res-52
      `[:let [~@(apply glue
                  (for [{:keys [name path]} @result]
                    [name [:fetch-in :value path]]))]
        ~@forms ]
      )))

(dotest-focus
  (is= {0 :a 1 :b 2 :c} (sequential->idx-map [:a :b :c]))
  (is= {0 :x 1 :y 2 :z} (sequential->idx-map [:x :y :z]))
  (let [data {:a 1
              :b {:c 3}}
        tmpl {:a :?
              :b {:c :?}}]
    (spyx-pretty (dstr data tmpl
                   [:is= [1 3] [:spyx [:a :c]]]
                   [:println [:a :c]]))
    ;(destruct [data [:a :?
    ;                 :b {:c :?}] ]
    ;  (is= [1 3] (spyx [a c])))
  ))

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



