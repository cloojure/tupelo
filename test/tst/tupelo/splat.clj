(ns tst.tupelo.splat
  (:use tupelo.splat
        tupelo.core
        tupelo.test)
  (:require
    [tupelo.splat :as splat]
    [clojure.walk :as walk]))

;---------------------------------------------------------------------------------------------------
; test the dispatch process using a stub to stop recursion but return the dispatch result as data
(let [splatter-stub (fn [data]
                      {:splatter-stub data})]
  (verify
    (with-redefs [splatter splatter-stub]
      (is= (splatter-impl 1) {:type :prim :data 1})
      (is= (splatter-impl [1])
        {:entries #{{:type :list/entry
                     :idx  0
                     :val  {:splatter-stub 1}}}
         :type    :coll/list})
      (is= (splatter-impl {:a 1})
        {:entries #{
                    {:type :map/entry
                     :key  {:splatter-stub :a}
                     :val  {:splatter-stub 1}}}
         :type    :coll/map})
      (is= (splatter-impl #{1 2})
        {:entries #{
                    {:type :set/entry :val {:splatter-stub 1}}
                    {:type :set/entry :val {:splatter-stub 2}}}
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
     :entries #{{:type :list/entry
                 :idx  0
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter [1 2])
    {:type    :coll/list
     :entries #{
                {:type :list/entry
                 :idx  0
                 :val  {:data 1 :type :prim}}
                {:type :list/entry
                 :idx  1
                 :val  {:data 2 :type :prim}}}})

  (is= (splatter {:a 1})
    {:type    :coll/map
     :entries #{{:key  {:data :a :type :prim}
                 :type :map/entry
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter {:a 1 :b 2})
    {:type    :coll/map
     :entries #{
                {:type :map/entry
                 :key  {:data :a :type :prim}
                 :val  {:data 1 :type :prim}}
                {:type :map/entry
                 :key  {:data :b :type :prim}
                 :val  {:data 2 :type :prim}}}})

  (is= (splatter #{1})
    {:type    :coll/set
     :entries #{
                {:type :set/entry
                 :val  {:data 1 :type :prim}}}})
  (is= (splatter #{1 2})
    {:type    :coll/set
     :entries #{
                {:type :set/entry
                 :val  {:data 1 :type :prim}}
                {:type :set/entry
                 :val  {:data 2 :type :prim}}}}))

(verify
  (let [orig  {:type    :coll/map
               :entries #{{:type :map/entry
                           :key  {:data :a :type :prim}
                           :val  {:data 1 :type :prim}}
                          {:type :map/entry
                           :key  {:data :b :type :prim}
                           :val  {:data 2 :type :prim}}}}
        map-a {:type    :coll/map
               :entries #{{:type :map/entry
                           :key  {:data :a :type :prim}
                           :val  {:data 1 :type :prim}}}}]
    (is= orig (remove-nils-map orig))

    (is= map-a
      (remove-nils-map {:type    :coll/map
                        :entries #{{:type :map/entry
                                    :key  {:data :a :type :prim}
                                    :val  {:data 1 :type :prim}}
                                   {:type :map/entry
                                    :key  {:data :b :type :prim}
                                    :val  nil}}})
      (remove-nils-map {:type    :coll/map
                        :entries #{{:type :map/entry
                                    :key  {:data :a :type :prim}
                                    :val  {:data 1 :type :prim}}
                                   {:type :map/entry
                                    :key  nil
                                    :val  {:data 2 :type :prim}}}})
      (remove-nils-map {:type    :coll/map
                        :entries #{{:type :map/entry
                                    :key  {:data :a :type :prim}
                                    :val  {:data 1 :type :prim}}
                                   nil}}))
    (is= {:a 1 :b 2} (unsplatter-map orig))
    (is= {:a 1} (unsplatter-map map-a))))

; A map with no valid entries will be empty
(verify
  (is= {} (unsplatter-map {:type    :coll/map
                           :entries #{}}))

  (is= (splatter {:a {:b 9}})
    {:entries
     #{{:key  {:data :a :type :prim}
        :type :map/entry
        :val  {:entries #{{:key  {:data :b :type :prim}
                           :type :map/entry
                           :val  {:data 9 :type :prim}}}
               :type    :coll/map}}}
     :type :coll/map})

  (is= {:a {}} (unsplatter-map {:entries #{{:key  {:data :a :type :prim}
                                            :type :map/entry
                                            :val  {:entries #{{:key  {:data :b :type :prim}
                                                               :type :map/entry
                                                               :val  nil}}
                                                   :type    :coll/map}}}
                                :type    :coll/map}))


  ; Deleting the `9` via `nil` will cause that map to become empty
  (is= (splatter {:a 1 :b {:c 9}})
    {:entries #{{:key  {:data :a :type :prim}
                 :type :map/entry
                 :val  {:data 1 :type :prim}}
                {:key  {:data :b :type :prim}
                 :type :map/entry
                 :val  {:entries #{{:key  {:data :c :type :prim}
                                    :type :map/entry
                                    :val  {:data 9 :type :prim}}}
                        :type    :coll/map}}}
     :type    :coll/map})
  (is= {:a 1 :b {}}
    (unsplatter-map {:entries #{{:key  {:data :a :type :prim}
                                 :type :map/entry
                                 :val  {:data 1 :type :prim}}
                                {:key  {:data :b :type :prim}
                                 :type :map/entry
                                 :val  {:entries #{{:key  {:data :c :type :prim}
                                                    :type :map/entry
                                                    :val  nil}}
                                        :type    :coll/map}}}
                     :type    :coll/map})))

(verify
  (let [
        orig   {:type    :coll/list
                :entries #{
                           {:type :list/entry
                            :idx  0
                            :val  {:data 1 :type :prim}}
                           {:type :list/entry
                            :idx  1
                            :val  {:data 2 :type :prim}}}}
        list-1 {:type    :coll/list
                :entries #{
                           {:type :list/entry
                            :idx  0
                            :val  {:data 1 :type :prim}}}}]
    (is= orig (remove-nils-list orig))
    (is= list-1 (remove-nils-list {:type    :coll/list
                                   :entries #{
                                              {:type :list/entry
                                               :idx  0
                                               :val  {:data 1 :type :prim}}
                                              {:type :list/entry
                                               :idx  1
                                               :val  nil}}}))
    (is= list-1 (remove-nils-list {:type    :coll/list
                                   :entries #{
                                              {:type :list/entry
                                               :idx  0
                                               :val  {:data 1 :type :prim}}
                                              {:type :list/entry
                                               :idx  nil
                                               :val  {:data 2 :type :prim}}}}))
    (is= list-1 (remove-nils-list {:type    :coll/list
                                   :entries #{
                                              {:type :list/entry
                                               :idx  0
                                               :val  {:data 1 :type :prim}}
                                              nil}}))
    (is= [1 2] (unsplatter-list orig))
    (is= [1 2] (unsplatter-list {:type    :coll/list
                                 :entries #{{:type :list/entry
                                             :idx  0
                                             :val  {:data 1 :type :prim}}
                                            {:type :list/entry
                                             :idx  9
                                             :val  {:data 2 :type :prim}}}}))
    (is= [1 2] (unsplatter-list {:type    :coll/list
                                 :entries #{{:type :list/entry
                                             :idx  -9
                                             :val  {:data 1 :type :prim}}
                                            {:type :list/entry
                                             :idx  9
                                             :val  {:data 2 :type :prim}}}}))
    (is= [1] (unsplatter-list {:type    :coll/list
                               :entries #{{:type :list/entry
                                           :idx  0
                                           :val  {:data 1 :type :prim}}
                                          {:type :list/entry
                                           :idx  nil
                                           :val  {:data 2 :type :prim}}}}))
    (is= [nil 2] (unsplatter-list {:type    :coll/list
                                   :entries #{{:type :list/entry
                                               :idx  0
                                               :val  {:data nil :type :prim}}
                                              {:type :list/entry
                                               :idx  9
                                               :val  {:data 2 :type :prim}}}}))))

(verify
  (let [data {:entries [{:idx 1, :type :list/entry, :val nil}
                        {:idx 2, :type :list/entry, :val nil}
                        {:idx 3, :type :list/entry, :val nil}
                        {:idx  0,
                         :type :list/entry,
                         :val  {:branch :list/val, :data :school, :type :prim}}],
              :type    :coll/list}
        ]
    (is= (unsplatter-list data)
      [:school])))

(verify
  (let [data [1 2 [3 4]]]
    (is= data (unsplatter (splatter data))))
  (let [data {:a 1 :b [2 3 [4 5]]}]
    (is= data (unsplatter (splatter data))))
  (let [data #{:a :b [2 3 [4 {:x 1 :y 2}]]}]
    (is= data (unsplatter (splatter data)))))

(verify
  (let [orig  {:type    :coll/set
               :entries #{{:type :set/entry
                           :val  {:data 1 :type :prim}}
                          {:type :set/entry
                           :val  {:data 2 :type :prim}}}}
        set-1 {:type    :coll/set
               :entries #{{:type :set/entry
                           :val  {:data 1 :type :prim}}}}]
    (is= orig (remove-nils-set orig))
    (is= set-1 (remove-nils-set {:type    :coll/set
                                 :entries #{{:type :set/entry
                                             :val  {:data 1 :type :prim}}
                                            {:type :set/entry
                                             :val  nil}}}))
    (is= set-1 (remove-nils-set {:type    :coll/set
                                 :entries #{{:type :set/entry
                                             :val  {:data 1 :type :prim}}
                                            nil}}))
    (is= #{1 2} (unsplatter-set orig))
    (is= #{1} (unsplatter-set {:type    :coll/set
                               :entries #{{:type :set/entry
                                           :val  {:data 1 :type :prim}}}}))))

;---------------------------------------------------------------------------------------------------
(verify
  (let [data  {:a 1 :b [2 3]}
        splat (splatter data)]
    (is= splat
      {:type    :coll/map
       :entries #{
                  {:type :map/entry
                   :key  {:type :prim :data :a}
                   :val  {:type :prim :data 1}}
                  {:type :map/entry
                   :key  {:type :prim :data :b}
                   :val  {:type    :coll/list
                          :entries #{
                                     {:type :list/entry
                                      :idx  0
                                      :val  {:type :prim :data 2}}
                                     {:type :list/entry
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
                               {:key  {:data :b :type :prim}
                                :type :map/entry
                                :val  {:entries
                                       #{nil ; tombstone for {:idx 1} list-entry
                                         {:idx 0 :type :list/entry :val {:data 2 :type :prim}}}
                                       :type :coll/list}}}
                    :type    :coll/map})
      (is= result {:b [2]})))

  (let [data     {:a 1 :b #{4 5 "six"}}
        splat    (splatter data)
        expected {:type    :coll/map
                  :entries #{
                             {:type :map/entry
                              :key  {:type :prim :data :a}
                              :val  {:type :prim :data 1}}
                             {:type :map/entry
                              :key  {:type :prim :data :b}
                              :val  {:type    :coll/set
                                     :entries #{
                                                {:type :set/entry
                                                 :val  {:type :prim :data 4}}
                                                {:type :set/entry
                                                 :val  {:type :prim :data 5}}
                                                {:type :set/entry
                                                 :val  {:type :prim :data "six"}}}}}}}]
    (is= splat expected)
    (is= data (unsplatter splat))))

;---------------------------------------------------------------------------------------------------
(let [inc-prim-odd (fn [stack arg]
                     (with-spy-indent
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
    (let [intc     {:enter stack-identity
                    :leave stack-identity}
          data-out (it-> [1 2]
                     (splatter it)
                     (stack-walk intc it)
                     (unsplatter it))]
      (is= [1 2] data-out)))

  (let [intc {:enter inc-prim-odd
              :leave stack-identity}]
    (verify
      (is= (splat/splatter-walk intc [1 2])
        [2 2])

      (is= (splat/splatter-walk intc {:a 1 :b 2})
        {:a 2 :b 2})

      (is= (splat/splatter-walk intc #{:a 1 22})
        #{:a 2 22}))))

;---------------------------------------------------------------------------------------------------
; remove empty leaves like `state`, etc
(verify
  (let [data-hiccup [:foo
                     [:name "John"]
                     [:address "1 hacker way"]
                     [:phone]
                     [:school [:name] [:state] [:type]]
                     [:college [:name "mit"] [:address] [:state]]]]
    (when false
      (spyx-pretty (splat/splatter data-hiccup))
      (comment ; sample subtree for `state`
        {:idx 2 :type :list/entry :val
         {:entries #{{:idx  0
                      :type :list/entry
                      :val  {:data :state :type :prim}}}
          :type    :coll/list}}))

    (let [intc   {:leave (fn [stack node]
                           (cond-it-> node
                             ; If a node is of type `:coll/list` has only the initial keyword and
                             ; nothing else, it is "empty" and we remove it by setting to `nil`
                             (= :coll/list (:type it)) (if (= 1 (count (:entries node)))
                                                         nil
                                                         node)))}
          result (splat/splatter-walk intc data-hiccup)]
      (is= result
        [:foo
         [:name "John"]
         [:address "1 hacker way"]
         [:college
          [:name "mit"]]]))))

;-----------------------------------------------------------------------------
; Uncomment one of these lines to see how the recursion proceeds through a splattered piece of data
(verify   ; -focus
  ; (splatter-walk-spy [1 2])
  ; (splatter-walk-spy {:a 1 :b #{2 3}})
  )


