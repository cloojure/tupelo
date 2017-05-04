;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x
  "Experimental new code"
  (:use clojure.test tupelo.test tupelo.x)
  (:require
    [clojure.string :as str]
    [clj-uuid :as uuid]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
  )
  (:import
    [java.nio ByteBuffer]
    [java.util UUID ]
  ))
(t/refer-tupelo)

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
      (is (and (leaf-elem? x-tree) (leaf-elem? y-tree) (leaf-elem? z-tree)))
      (is (and (leaf-elem? x-elem) (leaf-elem? y-elem) (leaf-elem? z-elem)))

      (is (node-hid? r))
      (is (node-elem? r-tree))
      (is (node-elem? r-elem))

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

      (merge-attrs x {:color :green})
      (is= (hid->tree x) (into {} (hid->leaf x))
        {:attrs {:tag :char, :color :green}, :value "x"})

      (is= (hid->attrs r) {:tag :root, :color :white})
      (is= (hid->attrs z) {:tag :char, :color :red})
      (is= (hid->kids r) [x y z])
      (is= (hid->value z) "z")

      (set-attrs z {:type :tuna, :name :charlie})
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

      (update-attrs x #(update % :cnt inc))
      (update-attrs x #(update % :cnt inc))
      (update-attrs r #(update % :cnt inc))
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white, :cnt 1},
         :kids  [{:attrs {:tag :char, :color :red, :cnt 2}, :value "x"}]})

      (update-attr x :cnt inc)
      (update-attr x :cnt inc)
      (update-attr r :cnt inc)
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white, :cnt 2},
         :kids  [{:attrs {:tag :char, :color :red, :cnt 4}, :value "x"}]})

      (update-attr r :cnt * 3)
      (update-attr r :cnt + 7)
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white, :cnt 13},
         :kids  [{:attrs {:tag :char, :color :red, :cnt 4}, :value "x"}]}))))

(dotest
  (let [state    (atom {})
        forest-1 (with-forest (new-forest)
                   (let [x (add-leaf {:tag :char :color :red} "x")
                         y (add-leaf {:tag :char :color :red} "y")
                         z (add-leaf {:tag :char :color :red} "z")
                         r (add-node {:tag :root :color :white} [x y z])]
                     (reset! state {:x x :y y :z z :r r})
                     (is= (hid->kids r) [x y z])
                     (is= (hid->value z) "z")

                     (set-attrs z {:type :tuna, :name :charlie})
                     (is= (hid->attrs z) {:type :tuna, :name :charlie})

                     (is= (hid->leaf y) (->Leaf {:tag :char, :color :red} "y"))
                     (is= (remove-attr y :color) (->Leaf {:tag :char} "y"))))

        forest-2 (with-forest forest-1
                   (let [{:keys [x y z r]} @state]
                     (is= (hid->elem y) (->Leaf {:tag :char} "y"))
                     (is= (set-value y "YYY") (->Leaf {:tag :char} "YYY"))
                     (is= (set-value y 0) (->Leaf {:tag :char} 0))
                     (update-value y + 7)
                     (update-value y * 6)
                     (is= (hid->leaf y) (->Leaf {:tag :char} 42))))

        ; forest-1 is unaffected by changes that created forest-2
        forest-3 (with-forest forest-1
                   (let [{:keys [x y z r]} @state]
                     (is= (hid->elem y) (->Leaf {:tag :char} "y")))) ; still has forest-1 value

        forest-4 (with-forest forest-2
                   (let [{:keys [x y z r]} @state
                         _ (is= (hid->leaf y) (->Leaf {:tag :char} 42)) ; still has forest-2 value
                         a (add-leaf {:name :michael} "do")
                         b (add-leaf {:name :tito} "re")
                         c (add-leaf {:name :germain} "mi")]
                     (set-kids r [a b c])
                     (is= (hid->tree r)
                       {:attrs {:tag :root, :color :white},
                        :kids  [{:attrs {:name :michael}, :value "do"}
                                {:attrs {:name :tito}, :value "re"}
                                {:attrs {:name :germain}, :value "mi"}]})
                     (update-kids r
                       (fn sort-kids [kids]
                         (sort-by #(grab :name (hid->attrs %)) kids)))
                     (is= (hid->tree r)
                       {:attrs {:tag :root, :color :white},
                        :kids  [{:attrs {:name :germain}, :value "mi"}
                                {:attrs {:name :michael}, :value "do"}
                                {:attrs {:name :tito}, :value "re"}]}
                       )
                     (update-kids r
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
      (add-kids r [x]) (is= (hid->kids r) [x])
      (add-kids r [y]) (is= (hid->kids r) [x y])
      (add-kids r [z]) (is= (hid->kids r) [x y z])
      (is= (hid->tree r)
        {:attrs {:tag :root, :color :white},
         :kids
                [{:attrs {:tag :char, :color :red}, :value "x"}
                 {:attrs {:tag :char, :color :green}, :value "y"}
                 {:attrs {:tag :char, :color :blue}, :value "z"}]})

      (remove-kids r #{z x})
      (is= (hid->kids r) [y])
      (throws? (remove-kids r #{y x}))
      (remove-kids r #{y})
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
                                 [{:attrs {:tag :char, :color :red}, :value "x"}
                                  {:attrs {:tag :char, :color :green}, :value "y"}
                                  {:attrs {:tag :char, :color :blue}, :value "z"}]})
      (remove-elems #{y z})
      (is= (hid->kids a) [x])
      (is= (hid->kids b) [x])
      (is= (hid->kids c) [x])
      (is= (hid->tree c) {:attrs {:tag :r3, :color :black},
                          :kids  [{:attrs {:tag :char, :color :red}, :value "x"}]})
      (throws? (remove-elems #{x y}))

      (remove-elems #{x})
      (is= (hid->kids a) [])
      (is= (hid->kids b) [])
      (is= (hid->kids c) [])
      (is= (hid->tree c) {:attrs {:tag :r3, :color :black}, :kids []}))))

(dotest
  (with-forest (new-forest)
    (let [x (add-leaf {:a 1 :b 2} "x")]
      (is= #{x} (root-hids))
      (is (elem-matches? x {:a 1 :b 2}))
      (is (elem-matches? x {:a nil :b 2}))
      (is (elem-matches? x {:a :* :b 2}))
      (is (elem-matches? x {:a 1}))
      (is (elem-matches? x {:a nil}))
      (is (elem-matches? x {:a :*}))
      (is (elem-matches? x {:a nil :b :*}))
      (is (elem-matches? x {:a :* :b nil}))
      (is (elem-matches? x {}))
      (isnt (elem-matches? x {:a 9}))
      (isnt (elem-matches? x {:a 1 :c nil}))
      (isnt (elem-matches? x {:c nil}))
      (isnt (elem-matches? x {:c :*}))

      (is (elem-matches? x [:a :b]))
      (is (elem-matches? x [:a]))
      (is (elem-matches? x [:b]))
      (is (elem-matches? x []))
      (isnt (elem-matches? x [:a :b :c]))
      (isnt (elem-matches? x [:a :c]))
      (isnt (elem-matches? x [:c]))

      (is (elem-matches? x :a))
      (is (elem-matches? x :b))
      (isnt (elem-matches? x :c)))))

(dotest
  (with-forest (new-forest)
    (let [tree-1 [:a
                  [:b 1]
                  [:b 2]
                  [:b
                   [:c 4]
                   [:c 5]]
                  [:c 9]]
          root-1 (add-tree-hiccup tree-1)

          b1     (add-leaf {:b nil} 1)
          b2     (add-leaf {:b nil} 2)
          c4     (add-leaf {:c nil} 4)
          c5     (add-leaf {:c nil} 5)
          c9     (add-leaf {:c nil} 9)
          b3     (add-node {:b nil} [c4 c5])
          aa     (add-node {:a nil} [b1 b2 b3 c9])

          root-2 (add-node {:a nil}
                   [(add-leaf {:b nil} 1)
                    (add-leaf {:b nil} 2)
                    (add-node {:b nil}
                      [(add-leaf {:c nil} 4)
                       (add-leaf {:c nil} 5)])
                    (add-leaf {:c nil} 9)])

          root-3 (add-node :a
                   [(add-leaf :b 1)
                    (add-leaf :b 2)
                    (add-node :b
                      [(add-leaf :c 4)
                       (add-leaf :c 5)])
                    (add-leaf :c 9)])
          ]
      (is= (hid->tree root-1)
        {:attrs {:a nil},
         :kids  [{:attrs {:b nil}, :value [1]}
                 {:attrs {:b nil}, :value [2]}
                 {:attrs {:b nil},
                  :kids  [{:attrs {:c nil}, :value [4]}
                          {:attrs {:c nil}, :value [5]}]}
                 {:attrs {:c nil}, :value [9]}]})
      (is=
        (hid->tree aa)
        (hid->tree root-2)
        (hid->tree root-3)
        {:attrs {:a nil},
         :kids
                [{:attrs {:b nil}, :value 1}
                 {:attrs {:b nil}, :value 2}
                 {:attrs {:b nil},
                  :kids  [{:attrs {:c nil}, :value 4}
                          {:attrs {:c nil}, :value 5}]}
                 {:attrs {:c nil}, :value 9}]}))))

(dotest
  (with-forest (new-forest)
    (let [tree-1 [:a
                  [:b 1 2 3]
                  [:b 2]
                  [:b
                   [:c 4]
                   [:c 5]]
                  [:c 9]]
          root-1 (add-tree-hiccup tree-1)
          ]
      (is= (hid->tree root-1)
        {:attrs {:a nil},
         :kids  [{:attrs {:b nil}, :value [1 2 3]}
                 {:attrs {:b nil}, :value [2]}
                 {:attrs {:b nil},
                  :kids  [{:attrs {:c nil}, :value [4]}
                          {:attrs {:c nil}, :value [5]}]}
                 {:attrs {:c nil}, :value [9]}]}))))

(dotest
  (with-forest (new-forest)
    (let [aa (add-node :a
               [(add-leaf :b 1)
                (add-leaf :b 2)
                (add-node :b
                  [(add-leaf :c 4)
                   (add-leaf :c 5)])
                (add-leaf :c 9)])]

      (is (empty? (find-paths aa [:z])))
      (is (empty? (find-paths aa [:z :b])))
      (is (empty? (find-paths aa [:z :b :c])))
      (is (empty? (find-paths aa [:a :z])))
      (is (empty? (find-paths aa [:a :z :c])))
      (is (empty? (find-paths aa [:a :b :z])))

      (is= (format-solns (find-paths aa [:a]))
        #{[{:attrs {:a nil},
            :kids
                   [{:attrs {:b nil}, :value 1}
                    {:attrs {:b nil}, :value 2}
                    {:attrs {:b nil},
                     :kids  [{:attrs {:c nil}, :value 4}
                             {:attrs {:c nil}, :value 5}]}
                    {:attrs {:c nil}, :value 9}]}]})
      (is= (format-solns (find-paths aa [:a :b]))
        #{[{:a nil}
           {:attrs {:b nil}, :value 1}]
          [{:a nil}
           {:attrs {:b nil}, :value 2}]
          [{:a nil}
           {:attrs {:b nil},
            :kids  [{:attrs {:c nil}, :value 4}
                    {:attrs {:c nil}, :value 5}]}]})
      (is= (format-solns (find-paths aa [:a :c]))
        #{[{:a nil}
           {:attrs {:c nil}, :value 9}]})
      (is= (format-solns (find-paths aa [:a :b :c]))
        #{[{:a nil} {:b nil} {:attrs {:c nil}, :value 5}]
          [{:a nil} {:b nil} {:attrs {:c nil}, :value 4}]})
      (is= (format-solns (find-paths aa [:* :b]))
        #{[{:a nil} {:attrs {:b nil}, :value 1}]
          [{:a nil} {:attrs {:b nil}, :value 2}]
          [{:a nil}
           {:attrs {:b nil},
            :kids  [{:attrs {:c nil}, :value 4}
                    {:attrs {:c nil}, :value 5}]}]})
      (is= (format-solns (find-paths aa [:a :*]))
        #{[{:a nil} {:attrs {:b nil}, :value 1}]
          [{:a nil} {:attrs {:b nil}, :value 2}]
          [{:a nil}
           {:attrs {:b nil},
            :kids  [{:attrs {:c nil}, :value 4}
                    {:attrs {:c nil}, :value 5}]}]
          [{:a nil} {:attrs {:c nil}, :value 9}]})
      (is= (format-solns (find-paths aa [:a :* :c]))
        #{[{:a nil} {:b nil} {:attrs {:c nil}, :value 5}]
          [{:a nil} {:b nil} {:attrs {:c nil}, :value 4}]}))))

(dotest
  (with-forest (new-forest)
    (let [aa (add-node :a
               [(add-leaf {:b :b1} 1)
                (add-leaf {:b :b2} 2)
                (add-node {:b :b3}
                  [(add-leaf {:c :c4} 4)
                   (add-leaf {:c :c5} 5)])
                (add-leaf {:c :c9} 9)])]
      (is= (format-solns (find-paths aa [:a :** :*]))
        #{[{:a nil} {:attrs {:b :b1}, :value 1}]
          [{:a nil} {:attrs {:b :b2}, :value 2}]
          [{:a nil} {:b :b3} {:attrs {:c :c4}, :value 4}]
          [{:a nil} {:b :b3} {:attrs {:c :c5}, :value 5}]
          [{:a nil}
           {:attrs {:b :b3},
            :kids  [{:attrs {:c :c4}, :value 4}
                    {:attrs {:c :c5}, :value 5}]}]
          [{:a nil} {:attrs {:c :c9}, :value 9}]})

      (is= (format-solns (find-paths aa [:** :c]))
        #{[{:a nil} {:b :b3} {:attrs {:c :c4}, :value 4}]
          [{:a nil} {:b :b3} {:attrs {:c :c5}, :value 5}]
          [{:a nil} {:attrs {:c :c9}, :value 9}]})

      (is= (format-solns (find-paths aa [:a :** :c]))
        #{[{:a nil} {:b :b3} {:attrs {:c :c4}, :value 4}]
          [{:a nil} {:b :b3} {:attrs {:c :c5}, :value 5}]
          [{:a nil} {:attrs {:c :c9}, :value 9}]})))

  (with-forest (new-forest)
    (let [aa (add-node {:a :a1}
               [(add-leaf {:b :b2} 2)
                (add-leaf {:c :c3} 3)
                ])]
      (throws? (format-solns (find-paths aa [:**])))
      (throws? (format-solns (find-paths aa [:a :**])))
      (is= (format-solns (find-paths aa [:a :** :c]))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-paths aa [:a :** :** :c]))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-paths aa [:** :c]))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-paths aa [:** :*]))
        #{[{:attrs {:a :a1},
            :kids  [{:attrs {:b :b2}, :value 2}
                    {:attrs {:c :c3}, :value 3}]}]
          [{:a :a1} {:attrs {:b :b2}, :value 2}]
          [{:a :a1} {:attrs {:c :c3}, :value 3}]})

      (throws? (format-solns (find-leaves aa [:**] 13)))
      (throws? (format-solns (find-leaves aa [:a :**] 13)))
      (is= (format-solns (find-leaves aa [:a :b] 2))
        #{[{:a :a1} {:attrs {:b :b2}, :value 2}]})
      (is= (format-solns (find-leaves aa [:a :** :b] 2))
        #{[{:a :a1} {:attrs {:b :b2}, :value 2}]})
      (is= (format-solns (find-leaves aa [:a :** :** :b] 2))
        #{[{:a :a1} {:attrs {:b :b2}, :value 2}]})
      (is= (format-solns (find-leaves aa [:** :b] 2))
        #{[{:a :a1} {:attrs {:b :b2}, :value 2}]})
      (is= (format-solns (find-leaves aa [:a :c] 3))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-leaves aa [:* :c] 3))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-leaves aa [:a :*] 3))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-leaves aa [:** :*] 3))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-leaves aa [:a :** :c] 3))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-leaves aa [:a :** :** :c] 3))
        #{[{:a :a1} {:attrs {:c :c3}, :value 3}]})
      (is= (format-solns (find-leaves aa [:** :*] :*))
        #{[{:a :a1} {:attrs {:b :b2}, :value 2}]
          [{:a :a1} {:attrs {:c :c3}, :value 3}]})

      (throws? (format-solns (find-paths aa [:**])))
      (throws? (format-solns (find-paths aa [:a :**])))))

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
               (add-leaf {:c :b2 :color :blue} 3)])

          ]
      (is= (set (mapv hid->tree (root-hids)))
        #{{:attrs {:a :a1},
           :kids  [{:attrs {:b :b1, :color :red}, :value 2}
                   {:attrs {:b :b2, :color :red}, :value 3}]}
          {:attrs {:a :a2},
           :kids  [{:attrs {:b :b1, :color :green}, :value 2}
                   {:attrs {:b :b2, :color :green}, :value 3}]}
          {:attrs {:a :a3},
           :kids  [{:attrs {:c :b1, :color :blue}, :value 2}
                   {:attrs {:c :b2, :color :blue}, :value 3}]}})

      (is= (format-solns (find-paths (root-hids) [:a]))
        #{[{:attrs {:a :a1},
            :kids
                   [{:attrs {:b :b1, :color :red}, :value 2}
                    {:attrs {:b :b2, :color :red}, :value 3}]}]
          [{:attrs {:a :a2},
            :kids
                   [{:attrs {:b :b1, :color :green}, :value 2}
                    {:attrs {:b :b2, :color :green}, :value 3}]}]
          [{:attrs {:a :a3},
            :kids
                   [{:attrs {:c :b1, :color :blue}, :value 2}
                    {:attrs {:c :b2, :color :blue}, :value 3}]}]})
      (is= (format-solns (find-paths (root-hids) [:a :b]))
        #{[{:a :a1} {:attrs {:b :b1, :color :red}, :value 2}]
          [{:a :a1} {:attrs {:b :b2, :color :red}, :value 3}]
          [{:a :a2} {:attrs {:b :b1, :color :green}, :value 2}]
          [{:a :a2} {:attrs {:b :b2, :color :green}, :value 3}]})

      (is= (format-solns (find-leaves (root-hids) [:a :b] 3))
        #{[{:a :a1} {:attrs {:b :b2, :color :red}, :value 3}]
          [{:a :a2} {:attrs {:b :b2, :color :green}, :value 3}]})

      (is= (format-solns (find-leaves (root-hids) [:a :c] 3))
        #{ [{:a :a3} {:attrs {:c :b2, :color :blue}, :value 3}] })

      (is= (format-solns (find-leaves (root-hids) [:** :*] 3))
        #{[{:a :a1} {:attrs {:b :b2, :color :red}, :value 3}]
          [{:a :a2} {:attrs {:b :b2, :color :green}, :value 3}]
          [{:a :a3} {:attrs {:c :b2, :color :blue}, :value 3}] }) )))
