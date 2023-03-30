(ns tst.tupelo.core.snip
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.pprint :as pprint]
    [flatland.ordered.map :as omap]
    [flatland.ordered.set :as oset]
    [tupelo.core :as t]
    ))

(verify
  (let [r10     (range 10)
        m10     {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10}
        m10-seq (seq (t/->sorted-map-generic m10))
        s10     #{:a :b :c :d :e :f :g :h :i :j}]
    (throws? (snip-seq-heads [] r10))

    (is= (snip-seq-heads [0] r10) [t/SNIP-TOKEN])
    (is= (snip-seq-heads [1] r10) [0 t/SNIP-TOKEN])
    (is= (snip-seq-heads [2] r10) [0 1 t/SNIP-TOKEN])
    (is= (snip-seq-heads [1 1] r10) [0 t/SNIP-TOKEN 5 t/SNIP-TOKEN])
    (is= (snip-seq-heads [2 3] r10) [0 1 t/SNIP-TOKEN 5 6 7 t/SNIP-TOKEN])
    (is= (snip-seq-heads [3 3 3] r10) [0 1 2 t/SNIP-TOKEN 3 4 5 t/SNIP-TOKEN 6 7 8 t/SNIP-TOKEN])
    (is= (snip-seq-heads [4 9] r10) [0 1 2 3 4 5 6 7 8 9])

    (is= (snip-seq-tail 0 r10) [])
    (is= (snip-seq-tail 1 r10) [9])
    (is= (snip-seq-tail 2 r10) [8 9])

    (throws? (snip-seq [0] r10))
    (is= (snip-seq [1] r10) [0 t/SNIP-TOKEN])
    (is= (snip-seq [1 1] r10) [0 t/SNIP-TOKEN 9])
    (is= (snip-seq [1 1 1] r10) [0 t/SNIP-TOKEN 4 t/SNIP-TOKEN 9])
    (is= (snip-seq [2 3 2] r10) [0 1 t/SNIP-TOKEN 4 5 6 t/SNIP-TOKEN 8 9])
    (is= (snip-seq [3 3 3] r10) [0 1 2 t/SNIP-TOKEN 3 4 5 t/SNIP-TOKEN 7 8 9])

    ;---------------------------------------------------------------------------------------------------
    (throws? (snip-seq-heads [] m10-seq))

    (is= (snip-seq-heads [0] m10-seq) [t/SNIP-TOKEN])
    (is= (snip-seq-heads [1] m10-seq) [[:a 1] t/SNIP-TOKEN])
    (is= (snip-seq-heads [2] m10-seq) [[:a 1] [:b 2] t/SNIP-TOKEN])
    (is= (snip-seq-heads [1 1] m10-seq) [[:a 1] t/SNIP-TOKEN [:f 6] t/SNIP-TOKEN])
    (is= (snip-seq-heads [2 3] m10-seq) [[:a 1] [:b 2] t/SNIP-TOKEN [:f 6] [:g 7] [:h 8] t/SNIP-TOKEN])
    (is= (snip-seq-heads [3 3 3] m10-seq) [[:a 1] [:b 2] [:c 3] t/SNIP-TOKEN [:d 4] [:e 5] [:f 6] t/SNIP-TOKEN [:g 7] [:h 8] [:i 9] t/SNIP-TOKEN])
    (is= (snip-seq-heads [4 9] m10-seq) [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] [:g 7] [:h 8] [:i 9] [:j 10]])

    ;---------------------------------------------------------------------------------------------------
    (is= (snip-impl {:snip-sizes [1 1 1] :data r10}) [0 t/SNIP-TOKEN 4 t/SNIP-TOKEN 9])
    (is= (snip-impl {:snip-sizes [2 3 2] :data r10}) [0 1 t/SNIP-TOKEN 4 5 6 t/SNIP-TOKEN 8 9])

    (is= (snip-impl {:snip-sizes [1 1] :data m10})
      {:a 1, :<snip-key-0> :<snip-val-0>, :j 10})
    (is= (snip-impl {:snip-sizes [2 3] :data m10})
      {:a            1, :b 2,
       :<snip-key-0> :<snip-val-0>,
       :h            8, :i 9, :j 10})
    (is= (snip-impl {:snip-sizes [2 1 2] :data m10})
      {:a            1,
       :b            2,
       :<snip-key-0> :<snip-val-0>,
       :e            5,
       :<snip-key-1> :<snip-val-1>,
       :i            9,
       :j            10})

    ; ***** must use `spyx-pretty` or `pprint` to avoid `#ordered-map (...)` syntax *****
    (is= (snip-impl {:snip-sizes [1 1], :data s10}) #{:a :<snip-0> :j})
    (is= (snip-impl {:snip-sizes [2 3], :data s10}) #{:a :b :<snip-0> :h :i :j})

    ; ***** Enable debug code to see default printout put like `#ordered-map (...)`
    (when false
      (nl)
      (spyx "hello")
      (spyx-pretty "hello")
      (nl)
      (let [om (omap/ordered-map :a 1 :b 2)]
        (spyx-pretty om)
        (println :println om)
        (println :pr-str (pr-str om))
        (prn :prn om)
        (pprint/pprint [:pprint om]))
      (nl)
      (let [os (oset/ordered-set 1 2 3)]
        (spyx os)
        (spyx-pretty os)
        (println :println os)
        (println :pr-str (pr-str os))
        (prn :prn os)
        (pprint/pprint [:pprint os]))
      (nl)
      (let [os10 (into (oset/ordered-set) (range 20))]
        (spyx :coerce-before os10)
        (coerce-flatland-ordered->normal-print!)
        (spyx :coerce-after os10)
        (spyx :plain-set (into #{} os10))))
    ))

(verify
  (let [e1  [{:date "2020-10-01", :value 116.5888}
             {:date "2020-10-02", :value 112.8253}
             {:date "2020-10-05", :value 116.2993}
             {:date "2020-10-06", :value 112.9651}
             {:date "2020-10-07", :value 114.8818}
             {:date "2020-10-08", :value 114.7719}
             {:date "2020-10-09", :value 116.7685}
             {:date "2020-10-12", :value 124.1857}
             {:date "2020-10-13", :value 120.8914}
             {:date "2020-10-14", :value 120.9812}
             {:date "2020-10-15", :value 120.5021}]
        e2  [{:date "2020-10-01", :value 116.5888}
             {:date "2020-10-02", :value 112.8253}
             {:date "2020-10-05", :value 116.2993}
             {:date "2020-10-06", :value 112.9651}
             {:date "2020-10-07", :value 114.8818}
             {:date "2020-10-08", :value 114.7719}
             {:date "2020-10-09", :value 116.7685}
             {:date "2020-10-12", :value 124.1857}
             {:date "2020-10-13", :value 120.8914}
             {:date "2020-10-14", :value 120.9812}
             {:date "2020-10-15", :value 120.5021}]
        sn1 (snip e1)
        sn2 (snip e2)]
    (is= sn1
      [{:date "2020-10-01", :value 116.5888}
       {:date "2020-10-02", :value 112.8253}
       {:date "2020-10-05", :value 116.2993}
       {:date "2020-10-06", :value 112.9651}
       t/SNIP-TOKEN
       {:date "2020-10-13", :value 120.8914}
       {:date "2020-10-14", :value 120.9812}
       {:date "2020-10-15", :value 120.5021}])

    (is (deep-rel= e1 e2))
    (is (deep-rel= sn1 sn2))))

(verify
  (let [v312      (forv [i (thru 1 9)]
                    (forv [j (thru 1 12)]
                      (->kw (format "%d-%d" i j))))
        v312-snip (snip v312)]
    (is= v312
      [[:1-1 :1-2 :1-3 :1-4 :1-5 :1-6 :1-7 :1-8 :1-9 :1-10 :1-11 :1-12]
       [:2-1 :2-2 :2-3 :2-4 :2-5 :2-6 :2-7 :2-8 :2-9 :2-10 :2-11 :2-12]
       [:3-1 :3-2 :3-3 :3-4 :3-5 :3-6 :3-7 :3-8 :3-9 :3-10 :3-11 :3-12]
       [:4-1 :4-2 :4-3 :4-4 :4-5 :4-6 :4-7 :4-8 :4-9 :4-10 :4-11 :4-12]
       [:5-1 :5-2 :5-3 :5-4 :5-5 :5-6 :5-7 :5-8 :5-9 :5-10 :5-11 :5-12]
       [:6-1 :6-2 :6-3 :6-4 :6-5 :6-6 :6-7 :6-8 :6-9 :6-10 :6-11 :6-12]
       [:7-1 :7-2 :7-3 :7-4 :7-5 :7-6 :7-7 :7-8 :7-9 :7-10 :7-11 :7-12]
       [:8-1 :8-2 :8-3 :8-4 :8-5 :8-6 :8-7 :8-8 :8-9 :8-10 :8-11 :8-12]
       [:9-1 :9-2 :9-3 :9-4 :9-5 :9-6 :9-7 :9-8 :9-9 :9-10 :9-11 :9-12]])

    (is= v312-snip
      [[:1-1 :1-2 :1-3 :1-4 t/SNIP-TOKEN :1-10 :1-11 :1-12]
       [:2-1 :2-2 :2-3 :2-4 t/SNIP-TOKEN :2-10 :2-11 :2-12]
       [:3-1 :3-2 :3-3 :3-4 t/SNIP-TOKEN :3-10 :3-11 :3-12]
       [:4-1 :4-2 :4-3 :4-4 t/SNIP-TOKEN :4-10 :4-11 :4-12]
       t/SNIP-TOKEN
       [:7-1 :7-2 :7-3 :7-4 t/SNIP-TOKEN :7-10 :7-11 :7-12]
       [:8-1 :8-2 :8-3 :8-4 t/SNIP-TOKEN :8-10 :8-11 :8-12]
       [:9-1 :9-2 :9-3 :9-4 t/SNIP-TOKEN :9-10 :9-11 :9-12]])))

(verify
  (let [data      (apply glue
                    (forv [x (chars-thru \a \k)]
                      {(->kw (str x))
                       (forv [j (thru 1 12)]
                         (->kw (format "%s-%d" x j)))}))
        data-snip (snip data)]
    (is= data
      {:e [:e-1 :e-2 :e-3 :e-4 :e-5 :e-6 :e-7 :e-8 :e-9 :e-10 :e-11 :e-12],
       :k [:k-1 :k-2 :k-3 :k-4 :k-5 :k-6 :k-7 :k-8 :k-9 :k-10 :k-11 :k-12],
       :g [:g-1 :g-2 :g-3 :g-4 :g-5 :g-6 :g-7 :g-8 :g-9 :g-10 :g-11 :g-12],
       :c [:c-1 :c-2 :c-3 :c-4 :c-5 :c-6 :c-7 :c-8 :c-9 :c-10 :c-11 :c-12],
       :j [:j-1 :j-2 :j-3 :j-4 :j-5 :j-6 :j-7 :j-8 :j-9 :j-10 :j-11 :j-12],
       :h [:h-1 :h-2 :h-3 :h-4 :h-5 :h-6 :h-7 :h-8 :h-9 :h-10 :h-11 :h-12],
       :b [:b-1 :b-2 :b-3 :b-4 :b-5 :b-6 :b-7 :b-8 :b-9 :b-10 :b-11 :b-12],
       :d [:d-1 :d-2 :d-3 :d-4 :d-5 :d-6 :d-7 :d-8 :d-9 :d-10 :d-11 :d-12],
       :f [:f-1 :f-2 :f-3 :f-4 :f-5 :f-6 :f-7 :f-8 :f-9 :f-10 :f-11 :f-12],
       :i [:i-1 :i-2 :i-3 :i-4 :i-5 :i-6 :i-7 :i-8 :i-9 :i-10 :i-11 :i-12],
       :a [:a-1 :a-2 :a-3 :a-4 :a-5 :a-6 :a-7 :a-8 :a-9 :a-10 :a-11 :a-12]})
    (is= data-snip
      {:a            [:a-1 :a-2 :a-3 :a-4 t/SNIP-TOKEN :a-10 :a-11 :a-12],
       :b            [:b-1 :b-2 :b-3 :b-4 t/SNIP-TOKEN :b-10 :b-11 :b-12],
       :c            [:c-1 :c-2 :c-3 :c-4 t/SNIP-TOKEN :c-10 :c-11 :c-12],
       :d            [:d-1 :d-2 :d-3 :d-4 t/SNIP-TOKEN :d-10 :d-11 :d-12],
       :<snip-key-0> :<snip-val-0>,
       :i            [:i-1 :i-2 :i-3 :i-4 t/SNIP-TOKEN :i-10 :i-11 :i-12],
       :j            [:j-1 :j-2 :j-3 :j-4 t/SNIP-TOKEN :j-10 :j-11 :j-12],
       :k            [:k-1 :k-2 :k-3 :k-4 t/SNIP-TOKEN :k-10 :k-11 :k-12]})))

(verify
  (let [data      (apply glue
                    (forv [x (chars-thru \a \c)]
                      {(->kw (str x))
                       (apply glue
                         (forv [j (thru 1 12)]
                           {j (->kw (format "%s-%d" x j))}))}))
        data-snip (snip data)]
    (is= data-snip
      {:a {1             :a-1,
           2             :a-2,
           3             :a-3,
           4             :a-4,
           :<snip-key-0> :<snip-val-0>,
           10            :a-10,
           11            :a-11,
           12            :a-12},
       :b {1             :b-1,
           2             :b-2,
           3             :b-3,
           4             :b-4,
           :<snip-key-0> :<snip-val-0>,
           10            :b-10,
           11            :b-11,
           12            :b-12},
       :c {1             :c-1,
           2             :c-2,
           3             :c-3,
           4             :c-4,
           :<snip-key-0> :<snip-val-0>,
           10            :c-10,
           11            :c-11,
           12            :c-12}})))

