(ns tst.tupelo.splat
  (:use tupelo.splat tupelo.core tupelo.test)
  (:require
    [tupelo.splat :as splat]
    [clojure.walk :as walk]))

(let [splatter-stub (fn [arg]
                      {:splatter-stub arg})]
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
         :type    :coll/set}))))

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
(let [inc-prim-odd (fn [stack arg]
                     (with-spy-indent
                       ;(nl)
                       ;(spyq :inc-prim-odd--enter )
                       ;(spyx-pretty arg )
                       ;(spyx-pretty stack)
                       (let [type (:type arg)
                             data (:data arg)]
                         (let [arg-out (cond-it-> arg
                                         (and (= :prim type) (number? data) (odd? data))
                                         (update arg :data inc))]
                           arg-out))))]

  (verify
    (is= (inc-prim-odd [] {:type :prim :data :a})
      {:type :prim :data :a})
    (is= (inc-prim-odd [] {:type :prim :data 1})
      {:type :prim :data 2}))

  (verify
    (let [intc     {:enter stack-walk-identity
                    :leave stack-walk-identity}
          data-out (it-> [1 2]
                     (splatter it)
                     (stack-walk intc it)
                     (unsplatter it))]
      (is= [1 2] data-out)))

  (let [intc {:enter inc-prim-odd
              :leave stack-walk-identity}]
    (verify
      (is= (splat/splatter-walk intc [1 2])
        [2 2])

      (is= (splat/splatter-walk intc {:a 1 :b 2})
        {:a 2 :b 2})

      (is= (splat/splatter-walk intc #{:a 1 22})
        #{:a 2 22}))))

; Uncomment one of these lines to see how the recursion proceeds through a splattered piece of data
(verify
  ; (splatter-walk-spy [1 2])
  ; (splatter-walk-spy {:a 1 :b #{2 3}})
  )



