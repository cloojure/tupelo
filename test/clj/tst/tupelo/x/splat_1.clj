(ns tst.tupelo.x.splat-1
  (:use tupelo.x.splat-1 tupelo.core tupelo.test)
  (:require
    [tupelo.core :as t]
    ))


(defn  splatter-dispatch-stub
  [arg] {:splatter-dispatch-stub arg})

(verify
  (with-redefs [splatter-dispatch  splatter-dispatch-stub]
    (is= (splat-primative 1)  {:type :prim :data 1})
    (is= (splat-list [1])
      {:entries #{{:type :list-entry
                   :idx  0
                   :val  {:splatter-dispatch-stub 1}}}
       :type    :list})
    (is= (splat-map {:a 1})
      {:entries #{
                  {:type :map-entry
                   :key  {:splatter-dispatch-stub :a}
                   :val  {:splatter-dispatch-stub 1}}}
       :type    :map})
    (is= (splat-set #{1 2})
      {:entries #{
                  {:type :set-entry :val {:splatter-dispatch-stub 1}}
                  {:type :set-entry :val {:splatter-dispatch-stub 2}}}
       :type    :set})))

;---------------------------------------------------------------------------------------------------
(verify
  (is= (splatter 1)
    {:data 1 :type :prim})
  (is= (splatter :a)
    {:data :a :type :prim})
  (is= (splatter "abc")
    {:data "abc" :type :prim})

  (is= (splatter [1])
    {:type :list
     :entries #{{:type :list-entry
                 :idx 0
                 :val {:data 1 :type :prim}}}})
  (is= (splatter [1 2])
    {:type :list
     :entries #{
                {:type :list-entry
                 :idx 0
                 :val {:data 1 :type :prim}}
                {:type :list-entry
                 :idx 1
                 :val {:data 2 :type :prim}}}})

  (is= (splatter {:a 1})
    {:type   :map
     :entries #{{:key  {:data :a :type :prim}
                 :type :map-entry
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter {:a 1 :b 2})
    {:type    :map
     :entries #{
                {:type :map-entry
                 :key  {:data :a :type :prim}
                 :val  {:data 1 :type :prim}}
                {:type :map-entry
                 :key  {:data :b :type :prim}
                 :val  {:data 2 :type :prim}}}})

  (is= (splatter #{1 })
    {:type    :set
     :entries #{
                {:type :set-entry
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter #{1 2})
    {:type    :set
     :entries #{
                {:type :set-entry
                 :val  {:data 1 :type :prim}}
                {:type :set-entry
                 :val  {:data 2 :type :prim}}}}))

;---------------------------------------------------------------------------------------------------
(verify
  (let [data  {:a 1 :b [2 3]}
        splat (splatter data)]
    (is= splat
      {:type    :map
       :entries #{
                  {:type :map-entry
                   :key  {:type :prim :data :a}
                   :val  {:type :prim :data 1}}
                  {:type :map-entry
                   :key  {:type :prim :data :b}
                   :val  {:type    :list
                          :entries #{
                                     {:type :list-entry
                                      :idx  0
                                      :val  {:type :prim :data 2}}
                                     {:type :list-entry
                                      :idx  1
                                      :val  {:type :prim :data 3}}
                                     }}}}})
    (is= data (unsplatter splat)))

  (let [data     {:a 1 :b #{4 5 "six"}}
        splat    (splatter data)
        expected {:type    :map
                  :entries #{
                             {:type :map-entry
                              :key  {:type :prim :data :a}
                              :val  {:type :prim :data 1}}
                             {:type :map-entry
                              :key  {:type :prim :data :b}
                              :val  {:type    :set
                                     :entries #{
                                                {:type :set-entry
                                                 :val  {:type :prim :data 4}}
                                                {:type :set-entry
                                                 :val  {:type :prim :data 5}}
                                                {:type :set-entry
                                                 :val  {:type :prim :data "six"}}
                                                }}}}}]
    (is= splat expected)
    (is= data (unsplatter splat))))

;---------------------------------------------------------------------------------------------------
(comment  ; #todo finish this or replace with tupelo.data ???
  (verify
    (let [intc {:enter (fn [ctx]
                         (spy-pretty :enter ctx)
                         ;(cond-it-> ctx
                         ;  (and (= :set-entry/elem (grab :branch it))
                         ;    (int? (grab :data it)))
                         ;  (update-in it [:data] #(* % 10)))
                         )
                :leave (fn [ctx]
                         (spy-pretty :leave ctx)
                         ;(cond-it-> ctx
                         ;  (and (= :set-entry/elem (grab :branch it))
                         ;    (int? (grab :data it)))
                         ;  (update-in it [:data] #(inc %)))
                         )}]
      (spyx-pretty (walk-splatter #{2 3} intc))
      )))

