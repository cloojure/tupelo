(ns       ; ^:test-refresh/focus
  tst.tupelo.x.splat-1
  (:use tupelo.x.splat-1
        tupelo.core
        tupelo.test)
  (:require
    [tupelo.core :as t]
    [clojure.walk :as walk]))

(defn splatter-stub
  [arg] {:splatter-stub arg})

(verify
  (with-redefs [splatter splatter-stub]
    (is= (splatter-impl 1) {:type :prim :data 1})
    (is= (splatter-impl [1])
      {:entries #{{:type :entry/list
                   :idx  0
                   :val  {:splatter-stub 1}}}
       :type    :coll/list})
    (is= (splatter-impl {:a 1})
      {:entries #{
                  {:type :entry/map
                   :key  {:splatter-stub :a}
                   :val  {:splatter-stub 1}}}
       :type    :coll/map})
    (is= (splatter-impl #{1 2})
      {:entries #{
                  {:type :entry/set :val {:splatter-stub 1}}
                  {:type :entry/set :val {:splatter-stub 2}}}
       :type    :coll/set})))

;---------------------------------------------------------------------------------------------------
(verify
  (is= (splatter 1)
    {:data 1 :type :prim})
  (is= (splatter :a)
    {:data :a :type :prim})
  (is= (splatter "abc")
    {:data "abc" :type :prim})

  (is= (splatter [1])
    {:type    :coll/list
     :entries #{{:type :entry/list
                 :idx  0
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter [1 2])
    {:type    :coll/list
     :entries #{
                {:type :entry/list
                 :idx  0
                 :val  {:data 1 :type :prim}}
                {:type :entry/list
                 :idx  1
                 :val  {:data 2 :type :prim}}}})

  (is= (splatter {:a 1})
    {:type    :coll/map
     :entries #{{:key  {:data :a :type :prim}
                 :type :entry/map
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter {:a 1 :b 2})
    {:type    :coll/map
     :entries #{
                {:type :entry/map
                 :key  {:data :a :type :prim}
                 :val  {:data 1 :type :prim}}
                {:type :entry/map
                 :key  {:data :b :type :prim}
                 :val  {:data 2 :type :prim}}}})

  (is= (splatter #{1})
    {:type    :coll/set
     :entries #{
                {:type :entry/set
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter #{1 2})
    {:type    :coll/set
     :entries #{
                {:type :entry/set
                 :val  {:data 1 :type :prim}}
                {:type :entry/set
                 :val  {:data 2 :type :prim}}}}))

;---------------------------------------------------------------------------------------------------
(verify
  (let [data  {:a 1 :b [2 3]}
        splat (splatter data)]
    (is= splat
      {:type    :coll/map
       :entries #{
                  {:type :entry/map
                   :key  {:type :prim :data :a}
                   :val  {:type :prim :data 1}}
                  {:type :entry/map
                   :key  {:type :prim :data :b}
                   :val  {:type    :coll/list
                          :entries #{
                                     {:type :entry/list
                                      :idx  0
                                      :val  {:type :prim :data 2}}
                                     {:type :entry/list
                                      :idx  1
                                      :val  {:type :prim :data 3}}
                                     }}}}})

    ; delete some entries (replace with "tombstone" `nil` and then reconstruct the remainder)
    (let [trimmed (walk/postwalk (fn [item]
                                   (if (and (map? item)
                                         (or (submap? {:val {:type :prim :data 1}} item)
                                           (submap? {:idx 1} item)))
                                     nil
                                     item))
                    splat)
          result  (unsplatter trimmed)]
      (is= trimmed {:entries #{nil ; tombstone for {:a 1} map-entry
                               {:key  {:data :b, :type :prim},
                                :type :entry/map,
                                :val  {:entries
                                             #{nil ; tombstone for {:idx 1} list-entry
                                               {:idx 0, :type :entry/list, :val {:data 2, :type :prim}}},
                                       :type :coll/list}}},
                    :type    :coll/map})
      (is= result {:b [2]})))

  (let [data     {:a 1 :b #{4 5 "six"}}
        splat    (splatter data)
        expected {:type    :coll/map
                  :entries #{
                             {:type :entry/map
                              :key  {:type :prim :data :a}
                              :val  {:type :prim :data 1}}
                             {:type :entry/map
                              :key  {:type :prim :data :b}
                              :val  {:type    :coll/set
                                     :entries #{
                                                {:type :entry/set
                                                 :val  {:type :prim :data 4}}
                                                {:type :entry/set
                                                 :val  {:type :prim :data 5}}
                                                {:type :entry/set
                                                 :val  {:type :prim :data "six"}}}}}}}]
    (is= splat expected)
    (is= data (unsplatter splat))))

;---------------------------------------------------------------------------------------------------
(defn inc-prim-odd
  [arg]
  (let [type (:type arg)
        data (:data arg)]
    ; (spyx arg)
    (let [arg-out (cond-it-> arg
                    (and (= :prim type) (number? data) (odd? data))
                    (update arg :data inc))]
      ; (spyx arg-out)
      arg-out)))
(verify
  (is= (inc-prim-odd {:type :prim :data :a})
    {:type :prim :data :a})
  (is= (inc-prim-odd {:type :prim :data 1})
    {:type :prim :data 2}))

(verify
  (let    ; -spy-pretty
    [data       [1 2]
     splat-orig (splatter data)
     intc       {:enter identity
                 :leave identity}
     splat-out  (walk-interceptor intc splat-orig)
     data-out   (unsplatter splat-out)]
    (is= data data-out)))

(verify
  (let [intc     {:enter inc-prim-odd
                  :leave identity}
        data-out (it-> [1 2]
                   (splatter it)
                   (walk-interceptor intc it)
                   (unsplatter it))]
    (is= [2 2] data-out)))

(verify
  (let [intc     {:enter inc-prim-odd
                  :leave identity}
        data-out (it-> {:a 1 :b 2}
                   (splatter it)
                   (walk-interceptor intc it)
                   (unsplatter it))]
    (is= {:a 2 :b 2} data-out)))

(verify
  (let [intc     {:enter inc-prim-odd
                  :leave identity}
        data-out (it-> #{:a 1 22}
                   (splatter it)
                   (walk-interceptor intc it)
                   (unsplatter it))]
    (is= #{:a 2 22} data-out)))

; enable this to see how the recursion proceeds through a splattered piece of data
(when true
  (verify
    (let [data  {:a 1 :b #{2 3}}
          splat (splatter data)

          ; :leave will default to `identity`
          intc  {:enter (fn [data]
                          (newline)
                          (prn :-----------------------------------------------------------------------------)
                          (spy-pretty :enter data))}]
      (walk-interceptor intc splat))))

