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
  (clear-db!)
  (let [x (add-leaf {:tag :char :color :red} "x")
        y (add-leaf {:tag :char :color :red} "y")
        z (add-leaf {:tag :char :color :red} "z")
        r (add-node {:tag :root :color :white} [x y z])
        x-tree (hid->tree x)
        y-tree (hid->tree y)
        z-tree (hid->tree z)
        r-tree (hid->tree r) ]
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
          (hid->node r)))

    (merge-attrs x {:color :green})
    (is= (hid->tree x) (into {} (hid->leaf x))
      {:attrs {:tag :char, :color :green}, :value "x"} )

    (is= (hid->attrs r) {:tag :root, :color :white})
    (is= (hid->attrs z) {:tag :char, :color :red})
    (is= (hid->kids r) [x y z])
    (is= (hid->value z) "z")

    (set-attrs z {:type :tuna, :name :charlie})
    (is= (hid->attrs z) {:type :tuna, :name :charlie})
  ))

(dotest
  (clear-db!)
  (let [x (add-leaf {:tag :char :color :red :cnt 0} "x")
        r (add-node {:tag :root :color :white :cnt 0} [x])
        x-tree (hid->tree x)
        r-tree (hid->tree r) ]
    (is= r-tree
      {:attrs {:tag :root, :color :white :cnt 0},
       :kids  [{:attrs {:tag :char, :color :red :cnt 0}, :value "x"} ]})

    (update-attrs x #(update % :cnt inc))
    (update-attrs x #(update % :cnt inc))
    (update-attrs r #(update % :cnt inc))
    (is= (hid->tree r)
      {:attrs {:tag :root, :color :white, :cnt 1},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 2}, :value "x"}]})

    (update-attr x :cnt  inc)
    (update-attr x :cnt  inc)
    (update-attr r :cnt  inc)
    (is= (hid->tree r)
      {:attrs {:tag :root, :color :white, :cnt 2},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 4}, :value "x"}]})

    (update-attr r :cnt * 3)
    (update-attr r :cnt + 7)
    (is= (hid->tree r)
      {:attrs {:tag :root, :color :white, :cnt 13},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 4}, :value "x"}]})))

(dotest
  (clear-db!)
  (let [x (add-leaf {:tag :char :color :red} "x")
        y (add-leaf {:tag :char :color :red} "y")
        z (add-leaf {:tag :char :color :red} "z")
        r (add-node {:tag :root :color :white} [x y z]) ]
    (is= (hid->kids r) [x y z])
    (is= (hid->value z) "z")

    (set-attrs z {:type :tuna, :name :charlie})
    (is= (hid->attrs z) {:type :tuna, :name :charlie})

    (is= (hid->leaf y) (->Leaf {:tag :char, :color :red} "y"))
    (is= (remove-attr y :color) (->Leaf {:tag :char} "y") )

    (is= (set-value y "YYY") (->Leaf {:tag :char} "YYY"))
    (is= (set-value y 0) (->Leaf {:tag :char} 0))
    (update-value y + 7)
    (update-value y * 6)
    (is= (hid->leaf y) (->Leaf {:tag :char} 42))

    (let [a (add-leaf {:name :michael} "do")
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
         :kids
                [{:attrs {:name :germain}, :value "mi"}
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
                 {:attrs {:name :tito}, :value "re"}]} )))

  (clear-db!)
  (let [x (add-leaf {:tag :char :color :red} "x")
        y (add-leaf {:tag :char :color :green} "y")
        z (add-leaf {:tag :char :color :blue} "z")
        r (add-node {:tag :root :color :white} []) ]
    (is= (hid->kids r) [])
    (add-kids r [x]) (is= (hid->kids r) [x])
    (add-kids r [y]) (is= (hid->kids r) [x y])
    (add-kids r [z]) (is= (hid->kids r) [x y z])
    (is= (hid->tree r)
      {:attrs {:tag :root, :color :white},
       :kids
              [{:attrs {:tag :char, :color :red}, :value "x"}
               {:attrs {:tag :char, :color :green}, :value "y"}
               {:attrs {:tag :char, :color :blue}, :value "z"}]} ))
)
