(ns ^:test-refresh/focus tst.tupelo.x.splat-1
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
      {:entries #{{:type :list-entry
                   :idx  0
                   :val  {:splatter-stub 1}}}
       :type    :list})
    (is= (splatter-impl {:a 1})
      {:entries #{
                  {:type :map-entry
                   :key  {:splatter-stub :a}
                   :val  {:splatter-stub 1}}}
       :type    :map})
    (is= (splatter-impl #{1 2})
      {:entries #{
                  {:type :set-entry :val {:splatter-stub 1}}
                  {:type :set-entry :val {:splatter-stub 2}}}
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
    {:type    :list
     :entries #{{:type :list-entry
                 :idx  0
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter [1 2])
    {:type    :list
     :entries #{
                {:type :list-entry
                 :idx  0
                 :val  {:data 1 :type :prim}}
                {:type :list-entry
                 :idx  1
                 :val  {:data 2 :type :prim}}}})

  (is= (splatter {:a 1})
    {:type    :map
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

  (is= (splatter #{1})
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
                                :type :map-entry,
                                :val  {:entries
                                             #{nil ; tombstone for {:idx 1} list-entry
                                               {:idx 0, :type :list-entry, :val {:data 2, :type :prim}}},
                                       :type :list}}},
                    :type    :map})
      (is= result {:b [2]})))

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
    (let  ; -spy-pretty
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

(comment  ; #todo finish this or replace with tupelo.data ???
  (verify
    (let [intc {:enter (fn [data]
                         (spy-pretty :enter data)
                         ;(cond-it-> data
                         ;  (and (= :set-entry/elem (grab :branch it))
                         ;    (int? (grab :data it)))
                         ;  (update-in it [:data] #(* % 10)))
                         )
                :leave (fn [data]
                         (spy-pretty :leave data)
                         ;(cond-it-> data
                         ;  (and (= :set-entry/elem (grab :branch it))
                         ;    (int? (grab :data it)))
                         ;  (update-in it [:data] #(inc %)))
                         )}]
      (spyx-pretty (walk-interceptor intc (splatter {:a 1 :b #{2 3}})))
      )))

