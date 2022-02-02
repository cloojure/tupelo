(ns tst.tupelo.x.splat-1
  (:use tupelo.x.splat-1 tupelo.core tupelo.test)
  (:require
    [tupelo.core :as t]
    ))
(dotest (prn ::enter))

(dotest
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
    (is= data (unsplatter splat)))
  )

(dotest   ; -focus
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
    ))

