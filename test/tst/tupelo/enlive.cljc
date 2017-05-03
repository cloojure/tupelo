;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.enlive
  "Experimental new code"
  (:use clojure.test tupelo.test tupelo.enlive )
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
  (let [map-subtree->hiccup (fn fn-map-subtree->hiccup [soln-set]
                              (into #{}
                                (for [soln soln-set]
                                  (update-in soln [:subtree] enlive->hiccup))))]
    (let [tree-1        [:a
                         [:b 1]
                         [:b 2]
                         [:b
                          [:c 1]
                          [:c 2]]
                         [:c 9]]
          tree-1-enlive (hiccup->enlive tree-1)]
      ;---------------------------------------------------------------------------------------------------
      (is (empty? (find-tree-hiccup tree-1 [:z])))
      (is (empty? (find-tree-hiccup tree-1 [:z :b])))
      (is (empty? (find-tree-hiccup tree-1 [:z :b :c])))
      (is (empty? (find-tree-hiccup tree-1 [:a :z])))
      (is (empty? (find-tree-hiccup tree-1 [:a :z :c])))
      (is (empty? (find-tree-hiccup tree-1 [:a :b :z])))

      (is= (find-tree-hiccup tree-1 [:a])
        #{{:parent-path [] :subtree [:a [:b 1] [:b 2] [:b [:c 1] [:c 2]]
                                     [:c 9]]}})
      (is= (find-tree-hiccup tree-1 [:a :b])
        #{{:parent-path [:a] :subtree [:b 1]}
          {:parent-path [:a] :subtree [:b 2]}
          {:parent-path [:a] :subtree [:b [:c 1] [:c 2]]}})
      (is= (find-tree-hiccup tree-1 [:a :b :c])
        #{{:parent-path [:a :b] :subtree [:c 1]}
          {:parent-path [:a :b] :subtree [:c 2]}})

      (is= (find-tree-hiccup tree-1 [:* :b])
        #{{:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:b 2]}
          {:parent-path [:a], :subtree [:b [:c 1] [:c 2]]}})
      (is= (find-tree-hiccup tree-1 [:a :*])
        #{{:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:b 2]}
          {:parent-path [:a], :subtree [:c 9]}
          {:parent-path [:a], :subtree [:b [:c 1] [:c 2]]}})
      (is= (find-tree-hiccup tree-1 [:a :* :c])
        #{{:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}})

      (is= (find-tree-hiccup tree-1 [:a :** :*])
        #{{:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:b 2]}
          {:parent-path [:a], :subtree [:c 9]}
          {:parent-path [:a], :subtree [:b [:c 1] [:c 2]]}
          {:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}})
      (is= (find-tree-hiccup tree-1 [:** :c])
        #{{:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}
          {:parent-path [:a], :subtree [:c 9]}})

      (is= (find-tree-hiccup tree-1 [:a :** :c])
        #{{:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}
          {:parent-path [:a], :subtree [:c 9]}})
      ;---------------------------------------------------------------------------------------------------
      (is (empty? (find-tree tree-1-enlive [:z])))
      (is (empty? (find-tree tree-1-enlive [:z :b])))
      (is (empty? (find-tree tree-1-enlive [:z :b :c])))
      (is (empty? (find-tree tree-1-enlive [:a :z])))
      (is (empty? (find-tree tree-1-enlive [:a :z :c])))
      (is (empty? (find-tree tree-1-enlive [:a :b :z])))

      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a]))
        #{{:parent-path [] :subtree [:a [:b 1] [:b 2] [:b [:c 1] [:c 2]]
                                     [:c 9]]}})
      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a :b]))
        #{{:parent-path [:a] :subtree [:b 1]}
          {:parent-path [:a] :subtree [:b 2]}
          {:parent-path [:a] :subtree [:b [:c 1] [:c 2]]}})
      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a :b :c]))
        #{{:parent-path [:a :b] :subtree [:c 1]}
          {:parent-path [:a :b] :subtree [:c 2]}})

      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:* :b]))
        #{{:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:b 2]}
          {:parent-path [:a], :subtree [:b [:c 1] [:c 2]]}})
      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a :*]))
        #{{:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:b 2]}
          {:parent-path [:a], :subtree [:c 9]}
          {:parent-path [:a], :subtree [:b [:c 1] [:c 2]]}})
      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a :* :c]))
        #{{:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}})

      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a :** :*]))
        #{{:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:b 2]}
          {:parent-path [:a], :subtree [:c 9]}
          {:parent-path [:a], :subtree [:b [:c 1] [:c 2]]}
          {:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}})
      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:** :c]))
        #{{:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}
          {:parent-path [:a], :subtree [:c 9]}})

      (is= (map-subtree->hiccup (find-tree tree-1-enlive [:a :** :c]))
        #{{:parent-path [:a :b], :subtree [:c 1]}
          {:parent-path [:a :b], :subtree [:c 2]}
          {:parent-path [:a], :subtree [:c 9]}})
      )

    (let [tree-2        [:a
                         [:b 1]
                         [:c 3]]
          tree-2-enlive (hiccup->enlive tree-2)]
      ;---------------------------------------------------------------------------------------------------
      (throws? (find-tree-hiccup tree-2 [:**]))
      (throws? (find-tree-hiccup tree-2 [:a :**]))
      (is= (find-tree-hiccup tree-2 [:a :** :c])
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (find-tree-hiccup tree-2 [:a :** :** :c])
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (find-tree-hiccup tree-2 [:** :c])
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (find-tree-hiccup tree-2 [:** :*])
        #{{:parent-path [], :subtree [:a [:b 1] [:c 3]]}
          {:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:c 3]}})

      (throws? (find-leaf-hiccup tree-2 [:**] 13))
      (throws? (find-leaf-hiccup tree-2 [:a :**] 13))
      (is= (find-leaf-hiccup tree-2 [:a :** :b] 1)
        #{{:parent-path [:a], :subtree [:b 1]}})
      (is= (find-leaf-hiccup tree-2 [:a :** :** :b] 1)
        #{{:parent-path [:a], :subtree [:b 1]}})
      (is= (find-leaf-hiccup tree-2 [:** :b] 1)
        #{{:parent-path [:a], :subtree [:b 1]}})
      (is= (find-leaf-hiccup tree-2 [:a :** :c] 3)
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (find-leaf-hiccup tree-2 [:a :** :** :c] 3)
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (find-leaf-hiccup tree-2 [:** :c] 3)
        #{{:parent-path [:a], :subtree [:c 3]}})

      (is= (find-leaf-hiccup tree-2 [:** :*] :*)
        #{{:parent-path [], :subtree [:a [:b 1] [:c 3]]}
          {:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:c 3]}})
      ;---------------------------------------------------------------------------------------------------
      (throws? (find-tree tree-2-enlive [:**]))
      (throws? (find-tree tree-2-enlive [:a :**]))
      (is= (map-subtree->hiccup (find-tree tree-2-enlive [:a :** :c]))
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (map-subtree->hiccup (find-tree tree-2-enlive [:a :** :** :c]))
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (map-subtree->hiccup (find-tree tree-2-enlive [:** :c]))
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (map-subtree->hiccup (find-tree tree-2-enlive [:** :*]))
        #{{:parent-path [], :subtree [:a [:b 1] [:c 3]]}
          {:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:c 3]}})

      (throws? (find-leaf tree-2-enlive [:**] 13))
      (throws? (find-leaf tree-2-enlive [:a :**] 13))
      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:a :** :b] 1))
        #{{:parent-path [:a], :subtree [:b 1]}})
      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:a :** :** :b] 1))
        #{{:parent-path [:a], :subtree [:b 1]}})
      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:** :b] 1))
        #{{:parent-path [:a], :subtree [:b 1]}})
      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:a :** :c] 3))
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:a :** :** :c] 3))
        #{{:parent-path [:a], :subtree [:c 3]}})
      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:** :c] 3))
        #{{:parent-path [:a], :subtree [:c 3]}})

      (is= (map-subtree->hiccup (find-leaf tree-2-enlive [:** :*] :*))
        #{{:parent-path [], :subtree [:a [:b 1] [:c 3]]}
          {:parent-path [:a], :subtree [:b 1]}
          {:parent-path [:a], :subtree [:c 3]}})
      )
    )
  ; #todo need test for get-leaf & get-tree
  ;---------------------------------------------------------------------------------------------------
  (let [bad-tree [:a
                  [:* 1]
                  [:b 2]]]
    (throws? (find-tree-hiccup bad-tree [:a :b]))
    (throws? (find-tree (hiccup->enlive bad-tree) [:a :b])))
  (let [bad-tree [:a
                  [:** 1]
                  [:b 2]]]
    (throws? (find-tree-hiccup bad-tree [:a :b]))
    (throws? (find-tree (hiccup->enlive bad-tree) [:a :b])))
  )

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
                 [:d 4]]]
        ]
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

