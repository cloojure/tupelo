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
  (let [db (new-db)
        x (add-leaf db {:tag :char :color :red} "x")
        y (add-leaf db {:tag :char :color :red} "y")
        z (add-leaf db {:tag :char :color :red} "z")
        r (add-node db {:tag :root :color :white} [x y z])
        x-tree (hid->tree db x)
        y-tree (hid->tree db y)
        z-tree (hid->tree db z)
        r-tree (hid->tree db r) ]
    (is (and (hid? x) (hid? y) (hid? z) (hid? r)))
    (is (and (leaf? x-tree) (leaf? y-tree) (leaf? z-tree)))
    (is (node? r-tree))
    (is= x-tree {:attrs {:tag :char, :color :red}, :value "x"} )
    (is= r-tree
      {:attrs {:tag :root, :color :white},
       :kids  [{:attrs {:tag :char, :color :red}, :value "x"}
               {:attrs {:tag :char, :color :red}, :value "y"}
               {:attrs {:tag :char, :color :red}, :value "z"}]})
    (is (wild-match?
          {:attrs {:tag :root, :color :white},
           :kids  [:* :* :*]}
          (hid->node db r)))

    (merge-attrs db x {:color :green})
    (is= (hid->tree db x) (into {} (hid->leaf db x))
      {:attrs {:tag :char, :color :green}, :value "x"} )

    (is= (hid->attrs db r) {:tag :root, :color :white})
    (is= (hid->attrs db z) {:tag :char, :color :red})
    (is= (hid->kids db r) [x y z])
    (is= (hid->value db z) "z")

    (set-attrs db z {:type :tuna, :name :charlie})
    (is= (hid->attrs db z) {:type :tuna, :name :charlie})
  ))

(dotest
  (let [db (new-db)
        x (add-leaf db {:tag :char :color :red :cnt 0} "x")
        r (add-node db {:tag :root :color :white :cnt 0} [x])
        x-tree (hid->tree db x)
        r-tree (hid->tree db r) ]
    (is= r-tree
      {:attrs {:tag :root, :color :white :cnt 0},
       :kids  [{:attrs {:tag :char, :color :red :cnt 0}, :value "x"} ]})

    (update-attrs db x #(update % :cnt inc))
    (update-attrs db x #(update % :cnt inc))
    (update-attrs db r #(update % :cnt inc))
    (is= (hid->tree db r)
      {:attrs {:tag :root, :color :white, :cnt 1},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 2}, :value "x"}]})

    (update-attr db x :cnt  inc)
    (update-attr db x :cnt  inc)
    (update-attr db r :cnt  inc)
    (is= (hid->tree db r)
      {:attrs {:tag :root, :color :white, :cnt 2},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 4}, :value "x"}]})

    (update-attr db r :cnt * 3)
    (update-attr db r :cnt + 7)
    (is= (hid->tree db r)
      {:attrs {:tag :root, :color :white, :cnt 13},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 4}, :value "x"}]})))

(dotest
  (let [db (new-db)
        x (add-leaf db {:tag :char :color :red} "x")
        y (add-leaf db {:tag :char :color :red} "y")
        z (add-leaf db {:tag :char :color :red} "z")
        r (add-node db {:tag :root :color :white} [x y z]) ]
    (is= (hid->kids db r) [x y z])
    (is= (hid->value db z) "z")

    (set-attrs db z {:type :tuna, :name :charlie})
    (is= (hid->attrs db z) {:type :tuna, :name :charlie})

    (is= (hid->leaf db y) (->Leaf {:tag :char, :color :red} "y"))
    (is= (remove-attr db y :color) (->Leaf {:tag :char} "y") )

    (is= (set-value db y "YYY") (->Leaf {:tag :char} "YYY"))
    (is= (set-value db y 0) (->Leaf {:tag :char} 0))
    (update-value db y + 7)
    (update-value db y * 6)
    (is= (hid->leaf db y) (->Leaf {:tag :char} 42))

    (let [a (add-leaf db {:name :michael} "do")
          b (add-leaf db {:name :tito} "re")
          c (add-leaf db {:name :germain} "mi")]
      (set-kids db r [a b c])
      (is= (hid->tree db r)
        {:attrs {:tag :root, :color :white},
         :kids  [{:attrs {:name :michael}, :value "do"}
                 {:attrs {:name :tito}, :value "re"}
                 {:attrs {:name :germain}, :value "mi"}]})
      (update-kids db r
        (fn sort-kids [kids]
          (sort-by #(grab :name (hid->attrs db %)) kids)))
      (is= (hid->tree db r)
        {:attrs {:tag :root, :color :white},
         :kids
                [{:attrs {:name :germain}, :value "mi"}
                 {:attrs {:name :michael}, :value "do"}
                 {:attrs {:name :tito}, :value "re"}]}
        )
      (update-kids db r
        (fn sort-kids [kids]
          (sort-by #(hid->value db %) kids)))
      (is= (hid->tree db r)
        {:attrs {:tag :root, :color :white},
         :kids  [{:attrs {:name :michael}, :value "do"}
                 {:attrs {:name :germain}, :value "mi"}
                 {:attrs {:name :tito}, :value "re"}]} )))

  (let [db (new-db)
        x (add-leaf db {:tag :char :color :red} "x")
        y (add-leaf db {:tag :char :color :green} "y")
        z (add-leaf db {:tag :char :color :blue} "z")
        r (add-node db {:tag :root :color :white} []) ]
    (is= (hid->kids db r) [])
    (add-kids db r [x]) (is= (hid->kids db r) [x])
    (add-kids db r [y]) (is= (hid->kids db r) [x y])
    (add-kids db r [z]) (is= (hid->kids db r) [x y z])
    (is= (hid->tree db r)
      {:attrs {:tag :root, :color :white},
       :kids
              [{:attrs {:tag :char, :color :red}, :value "x"}
               {:attrs {:tag :char, :color :green}, :value "y"}
               {:attrs {:tag :char, :color :blue}, :value "z"}]} )

    (remove-kids db r #{z x})
    (is= (hid->kids db r) [y])
    (throws? (remove-kids db r #{y x}))
    (remove-kids db r #{y})
    (is= (hid->kids db r) [])
    (is= (hid->tree db r)
      {:attrs {:tag :root, :color :white},
       :kids  []}))

  (let [db (new-db)
        x (add-leaf db {:tag :char :color :red} "x")
        y (add-leaf db {:tag :char :color :green} "y")
        z (add-leaf db {:tag :char :color :blue} "z")

        a (add-node db {:tag :r1 :color :white} [x y z])
        b (add-node db {:tag :r2 :color :grey } [x y z])
        c (add-node db {:tag :r3 :color :black} [x y z])
  ]
    (is= (hid->kids db a) [x y z])
    (is= (hid->kids db b) [x y z])
    (is= (hid->kids db c) [x y z])
    (is= (hid->tree db a) {:attrs {:tag :r1, :color :white},
                           :kids
                               [{:attrs {:tag :char, :color :red}, :value "x"}
                                {:attrs {:tag :char, :color :green}, :value "y"}
                                {:attrs {:tag :char, :color :blue}, :value "z"} ]} )
    (remove-elems db #{y z})
    (is= (hid->kids db a) [x])
    (is= (hid->kids db b) [x])
    (is= (hid->kids db c) [x])
    (is= (hid->tree db c) {:attrs {:tag :r3, :color :black},
                           :kids [{:attrs {:tag :char, :color :red}, :value "x"} ]} )
    (throws? (remove-elems db #{x y}))

    (remove-elems db #{x})
    (is= (hid->kids db a) [])
    (is= (hid->kids db b) [])
    (is= (hid->kids db c) [])
    (is= (hid->tree db c) {:attrs {:tag :r3, :color :black}, :kids []} )))

(dotest
  (let [db (new-db)
        x (add-leaf db {:a 1 :b 2} "x") ]
    (is   (elem-matches? db x {:a 1    :b 2  } ))
    (is   (elem-matches? db x {:a nil  :b 2  } ))
    (is   (elem-matches? db x {:a :*   :b 2  } ))
    (is   (elem-matches? db x {:a 1          } ))
    (is   (elem-matches? db x {:a nil        } ))
    (is   (elem-matches? db x {:a :*         } ))
    (is   (elem-matches? db x {:a nil  :b :* } ))
    (is   (elem-matches? db x {:a :*   :b nil} ))
    (is   (elem-matches? db x {              } ))
    (isnt (elem-matches? db x {:a 9        } ))
    (isnt (elem-matches? db x {:a 1  :c nil} ))
    (isnt (elem-matches? db x {      :c nil} ))
    (isnt (elem-matches? db x {      :c :* } ))

    (is   (elem-matches? db x [:a :b   ] ))
    (is   (elem-matches? db x [:a      ] ))
    (is   (elem-matches? db x [   :b   ] ))
    (is   (elem-matches? db x [        ] ))
    (isnt (elem-matches? db x [:a :b :c] ))
    (isnt (elem-matches? db x [:a    :c] ))
    (isnt (elem-matches? db x [      :c] ))

    (is   (elem-matches? db x :a ))
    (is   (elem-matches? db x :b ))
    (isnt (elem-matches? db x :c )) ))

(dotest
  (let [db (new-db)
        tree-1 [:a
                [:b 1]
                [:b 2]
                [:b
                 [:c 4]
                 [:c 5]]
                [:c 9]]
        root-1 (add-tree-hiccup db tree-1)

        b1     (add-leaf db {:b nil} 1)
        b2     (add-leaf db {:b nil} 2)
        c4     (add-leaf db {:c nil} 4)
        c5     (add-leaf db {:c nil} 5)
        c9     (add-leaf db {:c nil} 9)
        b3     (add-node db {:b nil} [c4 c5])
        aa     (add-node db {:a nil} [b1 b2 b3 c9])

        root-2 (add-node db {:a nil}
                 [(add-leaf db {:b nil} 1)
                  (add-leaf db {:b nil} 2)
                  (add-node db {:b nil}
                    [(add-leaf db {:c nil} 4)
                     (add-leaf db {:c nil} 5)])
                  (add-leaf db {:c nil} 9)])

        root-3 (add-node db :a
                 [(add-leaf db :b 1)
                  (add-leaf db :b 2)
                  (add-node db :b
                    [(add-leaf db :c 4)
                     (add-leaf db :c 5)])
                  (add-leaf db :c 9)])
  ]
    (is= (hid->tree db root-1)
      {:attrs {:a nil},
       :kids  [{:attrs {:b nil}, :value [1]}
               {:attrs {:b nil}, :value [2]}
               {:attrs {:b nil},
                :kids  [{:attrs {:c nil}, :value [4]}
                        {:attrs {:c nil}, :value [5]}]}
               {:attrs {:c nil}, :value [9]}]} )
    (is=
      (hid->tree db aa)
      (hid->tree db root-2)
      (hid->tree db root-3)
      {:attrs {:a nil},
       :kids
              [{:attrs {:b nil}, :value 1}
               {:attrs {:b nil}, :value 2}
               {:attrs {:b nil},
                :kids  [{:attrs {:c nil}, :value 4}
                        {:attrs {:c nil}, :value 5}]}
               {:attrs {:c nil}, :value 9}]}) ))

(dotest
  (let [db (new-db)
        tree-1 [:a
                [:b 1 2 3]
                [:b 2]
                [:b
                 [:c 4]
                 [:c 5]]
                [:c 9]]
        root-1 (add-tree-hiccup db tree-1)
  ]
    (is= (hid->tree db root-1)
      {:attrs {:a nil},
       :kids  [{:attrs {:b nil}, :value [1 2 3]}
               {:attrs {:b nil}, :value [2]}
               {:attrs {:b nil},
                :kids  [{:attrs {:c nil}, :value [4]}
                        {:attrs {:c nil}, :value [5]}]}
               {:attrs {:c nil}, :value [9]}]} )))
