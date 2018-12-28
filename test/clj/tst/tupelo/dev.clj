;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.dev
  (:use tupelo.dev tupelo.core tupelo.test)
  (:require
    [clojure.string :as str] 
  ))

(dotest ; #todo move
  (is= {0 :a 1 :b 2 :c} (sequential->idx-map [:a :b :c]))
  (is= {0 :x 1 :y 2 :z} (sequential->idx-map [:x :y :z]))
  (is= 'a (char->sym \a))
  (is= '? (char->sym \?)))

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

(defn vrange-1 [n]    ; 1e6 => 30 ms
  (loop [i 0
         v []]
    (if (< i n)
      (recur (inc i) (conj v i))
      v)))

(defn vrange-2 [n]  ; 1e6 => 16 ms
  (loop [i 0
         v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

(dotest
  (when false
    (with-timer-x (vrange-1 100000))
    (with-timer-x (vrange-2 100000))))

(comment            ; #todo fixme broken 2018-11-10 during impl merge

  ; (dotest
  ;   (try
  ;     (throw (ex-info "something bad happened" {:a 1 :b 2}))
  ;     (catch Exception ex
  ;       (is= "something bad happened" (ex-msg ex))
  ;       (is= {:a 1 :b 2} (ex-data ex))
  ;       (is (str/includes? (ex-stacktrace ex)
  ;             "clojure.lang.ExceptionInfo: something bad happened {:a 1, :b 2}"))
  ;       (is (str/includes? (ex-stacktrace ex)
  ;             "at tst.tupelo.")))))

)


