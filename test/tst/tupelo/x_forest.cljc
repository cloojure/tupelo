;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x-forest
  (:use tupelo.x-forest tupelo.test )
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

(dotest
  (let [tree-2 [:a
                [:b 2]
                [:c 3]]
        tree-3 [:a {:k1 "v1"}
                [:b 2]
                [:c 3]]
        tree-4 [:a {:k1 "v1"}
                [:b 2]
                [:c {:k3 "v3" :k4 4}
                 [:d 4]]] ]
    (is= (hiccup->enlive tree-2)
      {:tag   :a,
       :attrs {},
       :content
              [{:tag :b, :attrs {}, :content [2]}
               {:tag :c, :attrs {}, :content [3]}]})

    (is= (hiccup->enlive tree-3)
      {:tag   :a,
       :attrs {:k1 "v1"},
       :content
              [{:tag :b, :attrs {}, :content [2]}
               {:tag :c, :attrs {}, :content [3]}]})

    (is= (hiccup->enlive tree-4)
      {:tag   :a,
       :attrs {:k1 "v1"},
       :content
              [{:tag :b, :attrs {}, :content [2]}
               {:tag :c, :attrs {:k3 "v3" :k4 4}, :content [
                                                            {:tag :d :attrs {} :content [4]}]}]})
    ; #todo add generative testing
    (is=
      (-> tree-2 hiccup->enlive)
      (-> tree-2 hiccup->enlive enlive->hiccup hiccup->enlive))
    (is=
      (-> tree-3 hiccup->enlive)
      (-> tree-3 hiccup->enlive enlive->hiccup hiccup->enlive))
    (is=
      (-> tree-4 hiccup->enlive)
      (-> tree-4 hiccup->enlive enlive->hiccup hiccup->enlive))
    ))

(dotest
  (with-forest (new-forest)
    (let [x      (add-leaf {:tag :char :color :red} "x")
          y      (add-leaf {:tag :char :color :red} "y")
          z      (add-leaf {:tag :char :color :red} "z")
          r      (add-node {:tag :root :color :white} [x y z])
          x-tree (hid->tree x)
          y-tree (hid->tree y)
          z-tree (hid->tree z)
          r-tree (hid->tree r)
          x-elem (hid->elem x)
          y-elem (hid->elem y)
          z-elem (hid->elem z)
          r-elem (hid->elem r)

          roots (root-hids)]
      (is (and (hid? x) (hid? y) (hid? z) (hid? r)))

      (is (and (leaf-hid? x) (leaf-hid? y) (leaf-hid? z)))
      (is (and (tree-leaf? x-tree) (tree-leaf? y-tree) (tree-leaf? z-tree)))
      (is (and (tree-leaf? x-elem) (tree-leaf? y-elem) (tree-leaf? z-elem)))

      (is (node-hid? r))
      (is (tree-node? r-tree))
      (is (tree-node? r-elem))

      (is= #{r} roots)
      (not= #{x} roots)

      (is= x-tree {:attrs {:tag :char, :color :red}, :value "x"})
      (is= r-tree
        {:attrs {:tag :root, :color :white},
         :kids  [{:attrs {:tag :char, :color :red}, :value "x"}
                 {:attrs {:tag :char, :color :red}, :value "y"}
                 {:attrs {:tag :char, :color :red}, :value "z"}]})
      (is (wild-match?
            {:attrs {:tag :root, :color :white},
             :kids  [:* :* :*]}
            (hid->node r)))
      (is (wild-match?
            {:attrs {:tag :root :color :*},
             :kids  [:* :* :*]}
            (hid->node r)))
      (isnt (wild-match?
              {:attrs {:tag :root :color :*},
               :kids  [:* :*]}
              (hid->node r)))
      (is (wild-match?
            {:attrs {:tag :root :color :*},
             :kids  :*}
            (hid->node r)))

      (attrs-merge x {:color :green})
      (is= (hid->tree x) (into {} (hid->leaf x))
        {:attrs {:tag :char, :color :green}, :value "x"})

      (is= (hid->attrs r) {:tag :root, :color :white})
      (is= (hid->attrs z) {:tag :char, :color :red})
      (is= (hid->kids r) [x y z])
      (is= (hid->value z) "z")

      (attrs-reset z {:type :tuna, :name :charlie})
      (is= (hid->attrs z) {:type :tuna, :name :charlie}))))

(dotest
  (with-forest (new-forest)
    (let [x (add-leaf {:tag :char :color :red :cnt 0} "x")
          r (add-node {:tag :root :color :white :cnt 0} [x])
          x-tree (hid->tree x)
          r-tree (hid->tree r)]
      (is= r-tree
        {:attrs {:tag :root, :color :white :cnt 0},
         :kids  [{:attrs {:tag :char, :color :red :cnt 0}, :value "x"}]})

      (attr-update x :cnt inc)
      (attr-update x :cnt inc)
      (attr-update r :cnt #(+ 2 %))
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white, :cnt 2},
         :kids  [{:attrs {:tag :char, :color :red, :cnt 2}, :value "x"}]})

      (attr-update r :cnt * 3)
      (attr-update r :cnt + 7)
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white, :cnt 13},
         :kids  [{:attrs {:tag :char, :color :red, :cnt 2}, :value "x"}]}))))

(dotest
  (let [state    (atom {})
        forest-1 (with-forest-result (new-forest)
                   (let [x (add-leaf {:tag :char :color :red} "x")
                         y (add-leaf {:tag :char :color :red} "y")
                         z (add-leaf {:tag :char :color :red} "z")
                         r (add-node {:tag :root :color :white} [x y z])]
                     (reset! state (vals->map x y z r))
                     (is= (hid->kids r) [x y z])
                     (is= (hid->value z) "z")

                     (attrs-reset z {:type :tuna, :name :charlie})
                     (is= (hid->attrs z) {:type :tuna, :name :charlie})

                     (is= (hid->leaf y) (->Leaf {:tag :char, :color :red} "y"))
                     (is= (attr-remove y :color) (->Leaf {:tag :char} "y"))))

        forest-2 (with-forest-result forest-1
                   (let [{:keys [x y z r]} @state]
                     (is= (hid->elem y) (->Leaf {:tag :char} "y"))
                     (is= (value-set y "YYY") (->Leaf {:tag :char} "YYY"))
                     (is= (value-set y 0) (->Leaf {:tag :char} 0))
                     (value-update y + 7)
                     (value-update y * 6)
                     (is= (hid->leaf y) (->Leaf {:tag :char} 42))))

        ; forest-1 is unaffected by changes that created forest-2
        forest-3 (with-forest-result forest-1
                   (let [{:keys [x y z r]} @state]
                     (is= (hid->elem y) (->Leaf {:tag :char} "y")))) ; still has forest-1 value

        forest-4 (with-forest-result forest-2
                   (let [{:keys [x y z r]} @state
                         _ (is= (hid->leaf y) (->Leaf {:tag :char} 42)) ; still has forest-2 value
                         a (add-leaf {:name :michael} "do")
                         b (add-leaf {:name :tito} "re")
                         c (add-leaf {:name :germain} "mi")]
                     (kids-set r [a b c])
                     (is= (hid->tree r)
                       {:attrs {:tag :root, :color :white},
                        :kids  [{:attrs {:name :michael}, :value "do"}
                                {:attrs {:name :tito}, :value "re"}
                                {:attrs {:name :germain}, :value "mi"}]})
                     (kids-update r
                       (fn sort-kids [kids]
                         (sort-by #(grab :name (hid->attrs %)) kids)))
                     (is= (hid->tree r)
                       {:attrs {:tag :root, :color :white},
                        :kids  [{:attrs {:name :germain}, :value "mi"}
                                {:attrs {:name :michael}, :value "do"}
                                {:attrs {:name :tito}, :value "re"}]}
                       )
                     (kids-update r
                       (fn sort-kids [kids]
                         (sort-by #(hid->value %) kids)))
                     (is= (hid->tree r)
                       {:attrs {:tag :root, :color :white},
                        :kids  [{:attrs {:name :michael}, :value "do"}
                                {:attrs {:name :germain}, :value "mi"}
                                {:attrs {:name :tito}, :value "re"}]})))])

  (with-forest (new-forest)
    (let [x (add-leaf {:tag :char :color :red} "x")
          y (add-leaf {:tag :char :color :green} "y")
          z (add-leaf {:tag :char :color :blue} "z")
          r (add-node {:tag :root :color :white} [])]
      (is= (hid->kids r) [])
      (kids-append r [x]) (is= (hid->kids r) [x])
      (kids-append r [y]) (is= (hid->kids r) [x y])
      (kids-append r [z]) (is= (hid->kids r) [x y z])
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white},
         :kids
                [{:attrs {:tag :char, :color :red}, :value "x"}
                 {:attrs {:tag :char, :color :green}, :value "y"}
                 {:attrs {:tag :char, :color :blue}, :value "z"}]})

      (remove-kid r #{z x})
      (is= (hid->kids r) [y])
      (throws? (remove-kid r #{y x}))
      (remove-kid r #{y})
      (is= (hid->kids r) [])
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white},
         :kids  []})))

  (with-forest (new-forest)
    (let [x (add-leaf {:tag :char :color :red} "x")
          y (add-leaf {:tag :char :color :green} "y")
          z (add-leaf {:tag :char :color :blue} "z")

          a (add-node {:tag :r1 :color :white} [x y z])
          b (add-node {:tag :r2 :color :grey} [x y z])
          c (add-node {:tag :r3 :color :black} [x y z])
          ]
      (is= (hid->kids a) [x y z])
      (is= (hid->kids b) [x y z])
      (is= (hid->kids c) [x y z])
      (is= (hid->tree a) {:attrs {:tag :r1, :color :white},
                          :kids
                                 [{:attrs {:tag :char, :color :red},   :value "x"}
                                  {:attrs {:tag :char, :color :green}, :value "y"}
                                  {:attrs {:tag :char, :color :blue},  :value "z"}]})
      (remove-hid y z)
      (is= (hid->kids a) [x])
      (is= (hid->kids b) [x])
      (is= (hid->kids c) [x])
      (is= (hid->tree c) {:attrs {:tag :r3, :color :black},
                          :kids  [{:attrs {:tag :char, :color :red}, :value "x"}]})
      (throws? (remove-hid x y))

      (remove-hid x)
      (is= (hid->kids a) [])
      (is= (hid->kids b) [])
      (is= (hid->kids c) [])
      (is= (hid->tree c) {:attrs {:tag :r3, :color :black}, :kids []}))))

(dotest
  (with-forest (new-forest)
    (let [x (add-leaf {:a 1 :b 2} "x")]
      (is= #{x} (root-hids))
      (is (hid-matches? x {:a 1 :b 2}))
      (is (hid-matches? x {:a nil :b 2}))
      (is (hid-matches? x {:a :* :b 2}))
      (is (hid-matches? x {:a 1}))
      (is (hid-matches? x {:a nil}))
      (is (hid-matches? x {:a :*}))
      (is (hid-matches? x {:a nil :b :*}))
      (is (hid-matches? x {:a :* :b nil}))
      (is (hid-matches? x {}))
      (isnt (hid-matches? x {:a 9}))
      (isnt (hid-matches? x {:a 1 :c nil}))
      (isnt (hid-matches? x {:c nil}))
      (isnt (hid-matches? x {:c :*}))

      (is (hid-matches? x [:a :b]))
      (is (hid-matches? x [:a]))
      (is (hid-matches? x [:b]))
      (is (hid-matches? x []))
      (isnt (hid-matches? x [:a :b :c]))
      (isnt (hid-matches? x [:a :c]))
      (isnt (hid-matches? x [:c]))

      (isnt (hid-matches? x :a))
      (isnt (hid-matches? x :b))
      (isnt (hid-matches? x :c))))
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup
                     [:a
                      [:b 1]
                      [:b 2]
                      [:b
                       [:c 4]
                       [:c 5]]
                      [:c 9]])]
      (is (hid-matches? root-hid :a))
      (is (hid-matches? root-hid {:tag :a})) )))

(dotest
  (with-forest (new-forest)
    (let [hiccup-1 [:a
                  [:b 1]
                  [:b 2]
                  [:b
                   [:c 4]
                   [:c 5]]
                  [:c 9]]
          root-1 (add-tree-hiccup hiccup-1)

          b1     (add-leaf {:tag :b} 1)
          b2     (add-leaf {:tag :b} 2)
          c4     (add-leaf {:tag :c} 4)
          c5     (add-leaf {:tag :c} 5)
          c9     (add-leaf {:tag :c} 9)
          b3     (add-node {:tag :b} [c4 c5])
          aa     (add-node {:tag :a} [b1 b2 b3 c9])

          root-2 (add-node {:tag :a}
                   [(add-leaf {:tag :b} 1)
                    (add-leaf {:tag :b} 2)
                    (add-node {:tag :b}
                      [(add-leaf {:tag :c} 4)
                       (add-leaf {:tag :c} 5)])
                    (add-leaf {:tag :c} 9)])

          root-3 (add-node :a
                   [(add-leaf :b 1)
                    (add-leaf :b 2)
                    (add-node :b
                      [(add-leaf :c 4)
                       (add-leaf :c 5)])
                    (add-leaf :c 9)])
          ]
      (is= (hid->tree b1) {:attrs {:tag :b}, :value 1})
      (is= (hid->tree b2) {:attrs {:tag :b}, :value 2})
      (is= (hid->tree c4) {:attrs {:tag :c}, :value 4})
      (is= (hid->tree c5) {:attrs {:tag :c}, :value 5})
      (is= (hid->tree c9) {:attrs {:tag :c}, :value 9})
      (is= (hid->tree b3) {:attrs {:tag :b},
                           :kids  [{:attrs {:tag :c}, :value 4}
                                   {:attrs {:tag :c}, :value 5}]})
      (is= (hid->tree aa) {:attrs {:tag :a},
                           :kids  [{:attrs {:tag :b}, :value 1}
                                   {:attrs {:tag :b}, :value 2}
                                   {:attrs {:tag :b},
                                    :kids  [{:attrs {:tag :c}, :value 4}
                                            {:attrs {:tag :c}, :value 5}]}
                                   {:attrs {:tag :c}, :value 9}]})

      (is (validate-hid root-1))
      (is=
        (hid->tree aa)
        (hid->tree root-1)
        (hid->tree root-2)
        (hid->tree root-3)
        {:attrs {:tag :a},
         :kids
                [{:attrs {:tag :b}, :value 1}
                 {:attrs {:tag :b}, :value 2}
                 {:attrs {:tag :b},
                  :kids [{:attrs {:tag :c}, :value 4}
                         {:attrs {:tag :c}, :value 5}]}
                 {:attrs {:tag :c}, :value 9}]})
    )))

(dotest
  (with-forest (new-forest)
    (let [hiccup-1 [:a
                    [:b 1]
                    [:b 2]
                    [:b
                     [:c 4]
                     [:c 5]]
                    [:c 9]]
          enlive-1 (hiccup->enlive hiccup-1)
          tree-1    (hiccup->tree hiccup-1)
          tree-2    (enlive->tree enlive-1)
          bush-1    (hiccup->bush hiccup-1)
          bush-2    (tree->bush tree-1)
          tree-3    (bush->tree bush-1)

          hiccup-2a (enlive->hiccup enlive-1)
          hiccup-2b (tree->hiccup tree-1)
          hiccup-2c (tree->hiccup tree-2)
          hiccup-2d (bush->hiccup bush-1)

          ;hid-2    (add-tree-hiccup hiccup-1)
          ;tree-3   (hid->tree hid-2)
          ;bush-2   (hid->bush hid-2)
          ;enlive-2 (tree->enlive tree-2)
          ]
      (is= enlive-1
        {:tag   :a,
         :attrs {},
         :content
                [{:tag :b, :attrs {}, :content [1]}
                 {:tag :b, :attrs {}, :content [2]}
                 {:tag   :b,
                  :attrs {},
                  :content
                         [{:tag :c, :attrs {}, :content [4]}
                          {:tag :c, :attrs {}, :content [5]}]}
                 {:tag :c, :attrs {}, :content [9]}]})
      (is= tree-1
        {:attrs {:tag :a},
         :kids
                [{:attrs {:tag :b}, :value 1}
                 {:attrs {:tag :b}, :value 2}
                 {:attrs {:tag :b},
                  :kids  [{:attrs {:tag :c}, :value 4}
                          {:attrs {:tag :c}, :value 5}]}
                 {:attrs {:tag :c}, :value 9}]})
      (is= tree-2 tree-1)
      (is= tree-3 tree-1)
      (is= bush-1
        [{:tag :a}
         [{:tag :b} 1]
         [{:tag :b} 2]
         [{:tag :b}
          [{:tag :c} 4]
          [{:tag :c} 5]]
         [{:tag :c} 9]])
      (is= bush-2 bush-1)

      (is= hiccup-1
        hiccup-2a
        hiccup-2b
        hiccup-2c
        hiccup-2d)
      (is= hiccup-1 (enlive->hiccup (hiccup->enlive hiccup-1)))
      (is= enlive-1 (hiccup->enlive (enlive->hiccup enlive-1)))

      (is= enlive-1 (tree->enlive tree-2))

      (is= hiccup-1 (-> hiccup-1 hiccup->tree tree->hiccup))
      (is= enlive-1 (-> enlive-1 enlive->tree tree->enlive))

      (is= tree-1 (-> tree-1 tree->bush bush->tree))

      (is= tree-1 (-> tree-1 tree->hiccup hiccup->tree))
      (is= tree-1 (-> tree-1 tree->enlive enlive->tree))
      (is= bush-1 (-> bush-1 bush->tree tree->bush))
      (is= bush-1 (-> bush-1 bush->enlive enlive->bush))

    )))

(dotest
  (with-forest (new-forest)
    (let [root-hid (add-node :a
                     [(add-leaf :b 1)
                      (add-leaf :b 2)
                      (add-node :b
                        [(add-leaf :c 4)
                         (add-leaf :c 5)])
                      (add-leaf :c 9)])]

      (is (empty? (find-paths root-hid [:z])))
      (is (empty? (find-paths root-hid [:z :b])))
      (is (empty? (find-paths root-hid [:z :b :c])))
      (is (empty? (find-paths root-hid [:a :z])))
      (is (empty? (find-paths root-hid [:a :z :c])))
      (is (empty? (find-paths root-hid [:a :b :z])))

      (is= (format-paths (find-paths root-hid [:a]))
        [[{:tag :a}
          [{:tag :b} 1]
          [{:tag :b} 2]
          [{:tag :b}
           [{:tag :c} 4]
           [{:tag :c} 5]]
          [{:tag :c} 9]]])


      (is= (format-paths (find-paths root-hid [:a :b]))
        [[{:tag :a} [{:tag :b} 1]]
         [{:tag :a} [{:tag :b} 2]]
         [{:tag :a} [{:tag :b}
                     [{:tag :c} 4]
                     [{:tag :c} 5]]]])

      (is (wild-match? [[:* :*]
                        [:* :*]
                        [:* :*]]
            (find-paths root-hid [:a :b])))
      ; Actual results: (find-paths aa [:a :b]) =>
      ;    [ [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :b40b6f37e6a746f815b092a8590cefe5cf37121a]
      ;      [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :76859beedd81468b4ee3cc5f17a5fdcf7a34a787]
      ;      [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :5c0cb1ba6657ba0ac40cc5099f2be091b5637a3b] ]

      (is= (format-paths (find-paths root-hid [:a :c]))
        [[{:tag :a}
          [{:tag :c} 9]]])

      (is= (format-paths (find-paths root-hid [:a :b :c]))
        [[{:tag :a}
          [{:tag :b}
           [{:tag :c} 4]]]
         [{:tag :a}
          [{:tag :b}
           [{:tag :c} 5]]]])

      (is= (set (format-paths (find-paths root-hid [:* :b])))
        #{[{:tag :a}
           [{:tag :b}
            [{:tag :c} 4]
            [{:tag :c} 5]]]
          [{:tag :a}
           [{:tag :b} 2]]
          [{:tag :a}
           [{:tag :b} 1]]})

      (is= (format-paths (find-paths root-hid [:a :*]))
        [[{:tag :a}
          [{:tag :b} 1]]
         [{:tag :a}
          [{:tag :b} 2]]
         [{:tag :a}
          [{:tag :b}
           [{:tag :c} 4]
           [{:tag :c} 5]]]
         [{:tag :a}
          [{:tag :c} 9]]])

      (is= (format-paths (find-paths root-hid [:a :* :c]))
        [[{:tag :a} [{:tag :b} [{:tag :c} 4]]]
         [{:tag :a} [{:tag :b} [{:tag :c} 5]]]])
    )))

(dotest
  (with-forest (new-forest)
    (let [aa (add-node :a
               [(add-leaf {:b :b1} 1)
                (add-leaf {:b :b2} 2)
                (add-node {:b :b3}
                  [(add-leaf {:c :c4} 4)
                   (add-leaf {:c :c5} 5)])
                (add-leaf {:c :c9} 9)])]
      (is= (set (format-paths (find-paths aa [:a :** :*])))
        #{[{:tag :a} [{:b :b1} 1]]
          [{:tag :a} [{:b :b2} 2]]
          [{:tag :a} [{:c :c9} 9]]
          [{:tag :a} [{:b :b3}
                      [{:c :c4} 4]
                      [{:c :c5} 5]]]
          [{:tag :a} [{:b :b3} [{:c :c4} 4]]]
          [{:tag :a} [{:b :b3} [{:c :c5} 5]]]})

      (is= (set (format-paths (find-paths aa [:** {:c :*}])))
        #{[{:tag :a} [{:b :b3} [{:c :c5} 5]]]
          [{:tag :a} [{:b :b3} [{:c :c4} 4]]]
          [{:tag :a} [{:c :c9} 9]]})

      (is= (set (format-paths (find-paths aa [:a :** {:c :*}])))
        #{[{:tag :a} [{:b :b3} [{:c :c5} 5]]]
          [{:tag :a} [{:b :b3} [{:c :c4} 4]]]
          [{:tag :a} [{:c :c9} 9]]})))

  (with-forest (new-forest)
    (let [aa (add-node {:tag :a}
               [(add-leaf {:tag :b} 2)
                (add-leaf {:tag :c} 3) ])]
      (throws? (find-paths aa [:**]))
      (throws? (find-paths aa [:a :**]))
      (is=
        (format-paths (find-paths aa [:a :** :c]))
        (format-paths (find-paths aa [:a :** :** :c]))
        (format-paths (find-paths aa [:** :c]))
        [[{:tag :a} [{:tag :c} 3]]])
      (is= (format-paths (find-paths aa [:** :*]))
        [[{:tag :a} [{:tag :b} 2] [{:tag :c} 3]]
         [{:tag :a} [{:tag :b} 2]]
         [{:tag :a} [{:tag :c} 3]]])
      (is= (hid->tree aa)
        {:attrs {:tag :a},
         :kids  [{:attrs {:tag :b}, :value 2}
                 {:attrs {:tag :c}, :value 3}]})

      (throws? (find-paths-leaf aa [:**   ] 13))
      (throws? (find-paths-leaf aa [:a :**] 13))

      (is= (format-paths (find-paths-leaf aa [:a :b] 2))
        [[{:tag :a} [{:tag :b} 2]]])

      (is= (format-paths (find-paths-leaf aa [:a :** :b] 2))
        [ [{:tag :a} [{:tag :b} 2]] ])

      (is= (format-paths (find-paths-leaf aa [:a :** :** :b] 2))
        [[{:tag :a} [{:tag :b} 2]]])

      (is= (format-paths (find-paths-leaf aa [:** :b] 2))
        [[{:tag :a} [{:tag :b} 2]]])

      (is= (format-paths (find-paths-leaf aa [:a :c] 3))
        [[{:tag :a} [{:tag :c} 3]]])

      (is= (format-paths (find-paths-leaf aa [:* :c] 3))
        [[{:tag :a} [{:tag :c} 3]]])

      (is= (format-paths (find-paths-leaf aa [:a :*] 3))
        [[{:tag :a} [{:tag :c} 3]]])

      (is= (format-paths (find-paths-leaf aa [:** :*] 3))
        [[{:tag :a} [{:tag :c} 3]]])

      (is= (format-paths (find-paths-leaf aa [:a :** :c] 3))
        [[{:tag :a} [{:tag :c} 3]]])

      (is= (format-paths (find-paths-leaf aa [:a :** :** :c] 3))
        [[{:tag :a} [{:tag :c} 3]]])

      (is= (format-paths (find-paths-leaf aa [:** :*] :*))
        [[{:tag :a} [{:tag :b} 2]]
         [{:tag :a} [{:tag :c} 3]]])

      (throws? (find-paths aa [:**]))
      (throws? (find-paths aa [:a :**]))))

  (with-forest (new-forest)
    (throws? (add-node {:a :a1}
               [(add-leaf {:* :b2} 2)
                (add-leaf {:c :c3} 3)]))
    (throws? (add-node {:a :a1}
               [(add-leaf {:b :*} 2)
                (add-leaf {:c :c3} 3)]))
    (throws? (add-node {:a :a1}
               [(add-leaf {:** :b2} 2)
                (add-leaf {:c :c3} 3)]))
    (throws? (add-node {:a :a1}
               [(add-leaf {:b :**} 2)
                (add-leaf {:c :c3} 3)])))
)

(dotest
  (with-forest (new-forest)
    (let [x (add-node {:a :a1}
              [(add-leaf {:b :b1 :color :red} 2)
               (add-leaf {:b :b2 :color :red} 3)])
          y (add-node {:a :a2}
              [(add-leaf {:b :b1 :color :green} 2)
               (add-leaf {:b :b2 :color :green} 3)])
          z (add-node {:a :a3}
              [(add-leaf {:c :b1 :color :blue} 2)
               (add-leaf {:c :b2 :color :blue} 3)])]
      (is= (set (mapv hid->tree (root-hids)))
        #{{:attrs {:a :a2},
           :kids  [{:attrs {:b :b1, :color :green}, :value 2}
                   {:attrs {:b :b2, :color :green}, :value 3}]}
          {:attrs {:a :a1},
           :kids  [{:attrs {:b :b1, :color :red}, :value 2}
                   {:attrs {:b :b2, :color :red}, :value 3}]}
          {:attrs {:a :a3},
           :kids  [{:attrs {:c :b1, :color :blue}, :value 2}
                   {:attrs {:c :b2, :color :blue}, :value 3}]}})

      (is= (set (format-paths (find-paths (root-hids) [{:a :*}])))
        #{[{:a :a1}
           [{:b :b1, :color :red} 2]
           [{:b :b2, :color :red} 3]]
          [{:a :a2}
           [{:b :b1, :color :green} 2]
           [{:b :b2, :color :green} 3]]
          [{:a :a3}
           [{:c :b1, :color :blue} 2]
           [{:c :b2, :color :blue} 3]]})

      (is= (set (format-paths (find-paths (root-hids) [{:a :*} {:b :*}])))
        #{[{:a :a1} [{:b :b1, :color :red} 2]]
          [{:a :a1} [{:b :b2, :color :red} 3]]
          [{:a :a2} [{:b :b1, :color :green} 2]]
          [{:a :a2} [{:b :b2, :color :green} 3]]})

      (is= (set (format-paths (find-paths-leaf (root-hids) [{:a :*} {:b :*}] 3)))
        #{[{:a :a1} [{:b :b2, :color :red} 3]]
          [{:a :a2} [{:b :b2, :color :green} 3]]})

      (is= (format-paths (find-paths-leaf (root-hids) [{:a :*} {:c :*}] 3))
        [[{:a :a3} [{:c :b2, :color :blue} 3]]])

      (is= (set (format-paths (find-paths-leaf (root-hids) [:** :*] 3)))
        #{[{:a :a1} [{:b :b2, :color :red} 3]]
          [{:a :a2} [{:b :b2, :color :green} 3]]
          [{:a :a3} [{:c :b2, :color :blue} 3]]}))))

; #todo need to test find-paths using attrs
(dotest
  (with-forest (new-forest)
    (let [aa (add-node {:color :red}
               [(add-leaf {:color :green} 2)
                (add-leaf {:color :blue} 3)])]
      (is= (format-paths (find-paths aa [{:color :red}]))
        [[{:color :red}
          [{:color :green} 2]
          [{:color :blue} 3]]])

      (is= (format-paths (find-paths aa [{:color :red} {:color :green}]))
        [[{:color :red}
          [{:color :green} 2]]])

      (is= (format-paths (find-paths aa [:** {:color :green}]))
        [[{:color :red}
          [{:color :green} 2]]]))))

(dotest
  (with-forest (new-forest)
    (let [x (add-node {:tag :a :id :a1}
              [(add-leaf {:tag :b :color :red} 2)
               (add-leaf {:tag :b :color :red} 3)])
          y (add-node {:tag :a :id :a2}
              [(add-leaf {:tag :b :color :green} 2)
               (add-leaf {:tag :b :color :green} 3)])
          z (add-node {:tag :a :id :a3}
              [(add-leaf {:tag :c :color :blue} 2)
               (add-leaf {:tag :c :color :blue} 3)]) ]

      (is= (set (format-paths (find-paths (root-hids) [{:tag :a}])))
        #{[{:tag :a, :id :a1}
           [{:tag :b, :color :red} 2]
           [{:tag :b, :color :red} 3]]
          [{:tag :a, :id :a2}
           [{:tag :b, :color :green} 2]
           [{:tag :b, :color :green} 3]]
          [{:tag :a, :id :a3}
           [{:tag :c, :color :blue} 2]
           [{:tag :c, :color :blue} 3]]}
        )

      (is= (format-paths (find-paths (root-hids) [{:id :a2}]))
        [[{:tag :a, :id :a2}
          [{:tag :b, :color :green} 2]
          [{:tag :b, :color :green} 3]]])

      (is= (format-paths (find-paths (root-hids) [:** {:color :green}]))
        [[{:tag :a, :id :a2}
          [{:tag :b, :color :green} 2]]
         [{:tag :a, :id :a2}
          [{:tag :b, :color :green} 3]]])

      ; Actual return value looks like this:
      ; (find-paths (root-hids) [:** {:color :green}]) =>
      ;   #{[:e41495fcd783b2b33bf68df959b53d2471d8043f :13893f7cf114a456bc286ffb6536ab076c5a3272]
      ;     [:e41495fcd783b2b33bf68df959b53d2471d8043f :a825b2abfd07a9db00ab70a3d24a61349d4d0082]})
      (is (wild-match? [[:* :*]
                        [:* :*]]
            (vec
              (into (sorted-set)
                (find-paths (root-hids) [:** {:color :green}])))))

      (is= (set (format-paths (find-paths-leaf (root-hids) [:** {:tag :b}] 2)))
        #{[{:tag :a, :id :a2}
           [{:tag :b, :color :green} 2]]
          [{:tag :a, :id :a1}
           [{:tag :b, :color :red} 2]]})

      (is= (format-paths (find-paths-leaf x [:** {:tag :b}] 2))
        [[{:tag :a, :id :a1}
          [{:tag :b, :color :red} 2]]])

      (is= (find-leaf x [:** {:tag :b}] 2)
        (map->Leaf {:attrs {:tag :b, :color :red}, :value 2}))

      (is= (find-leaf (root-hids) [:** {:color :blue}] 2)
        (map->Leaf {:attrs {:tag :c, :color :blue}, :value 2}))


      (is= (set (format-paths (find-paths-leaf #{z y} [{:tag :a} :*] 2)))
        #{[{:tag :a, :id :a2} [{:tag :b, :color :green} 2]]
          [{:tag :a, :id :a3} [{:tag :c, :color :blue} 2]]})
      (is= (set (format-paths (find-paths-leaf (root-hids) [{:tag :a} :*] 2)))
        #{[{:tag :a, :id :a2} [{:tag :b, :color :green} 2]]
          [{:tag :a, :id :a1} [{:tag :b, :color :red} 2]]
          [{:tag :a, :id :a3} [{:tag :c, :color :blue} 2]]})
      (is= (set (format-paths (find-paths-leaf (root-hids) [{:tag :a} {:tag :c}] :*)))
        #{[{:tag :a, :id :a3} [{:tag :c, :color :blue} 3]]
          [{:tag :a, :id :a3} [{:tag :c, :color :blue} 2]]})

      ; Actual return value looks like this:
      ; (find-leaves (root-hids) [{:tag :a} {:tag :c}] :*) =>
      ;   #{[:bfdb71187fc7fc1182e1776d9d50f6f6e1f72646 :dca3a96d1811b28f8cbc8a0e15ddd7670c9ed654]
      ;     [:bfdb71187fc7fc1182e1776d9d50f6f6e1f72646 :0288e8f77e17e289f02d62229304960f1ddac39b]})
      (is (wild-match? [[:* :*]
                        [:* :*]]
            (find-paths-leaf (root-hids) [{:tag :a} {:tag :c}] :*))))))

(dotest
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup
                     [:a
                      [:b 1]
                      [:b 2]
                      [:b
                       [:c 4]
                       [:c 5]]
                      [:c 9]])
          tree     (hid->tree root-hid)
          bush     (hid->bush root-hid)]
      (is= tree
        {:attrs {:tag :a},
         :kids
                [{:attrs {:tag :b}, :value 1}
                 {:attrs {:tag :b}, :value 2}
                 {:attrs {:tag :b},
                  :kids  [{:attrs {:tag :c}, :value 4}
                          {:attrs {:tag :c}, :value 5}]}
                 {:attrs {:tag :c}, :value 9}]})
      (is= bush
        [{:tag :a}
          [{:tag :b} 1]
          [{:tag :b} 2]
          [{:tag :b}
            [{:tag :c} 4]
            [{:tag :c} 5]]
          [{:tag :c} 9]]  ))))

