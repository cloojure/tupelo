(ns tst.tupelo.x.splat-1
  (:use tupelo.x.splat-1 tupelo.core tupelo.test)
  (:require
    [tupelo.core :as t]
    ))

(dotest-focus
  (is= (splatter {:a 1 :b [2 3]})
    {:type    :map
     :entries [{:type :map-entry
                :key  {:type :prim :data :a}
                :val  {:type :prim :data 1}}
               {:type :map-entry
                :key  {:type :prim :data :b}
                :val  {:type    :list
                       :entries [{:type :list-entry
                                  :idx  0
                                  :val  {:type :prim :data 2}}
                                 {:type :list-entry
                                  :idx  1
                                  :val  {:type :prim :data 3}}]}}]})
  (is= (splatter {:a 1 :b #{4 5}})
    {:type    :map
     :entries [{:type :map-entry
                :key  {:type :prim :data :a}
                :val  {:type :prim :data 1}}
               {:type :map-entry
                :key  {:type :prim :data :b}
                :val  {:type    :list
                       :entries [{:type :set-entry
                                  :val  {:type :prim :data 4}}
                                 {:type :set-entry
                                  :val  {:type :prim :data 5}}]}}]})

  )


