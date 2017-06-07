;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.forest-examples
  (:use tupelo.x-forest tupelo.test )
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

; Examples from:
;   http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/
;
;

(def t0
  [1 [:a :b] 2 3 [40 50 60]] )

(def t0-hiccup
  [:item
   [:item 1]
   [:item
    [:item :a]
    [:item :b]]
   [:item 2]
   [:item 3]
   [:item
    [:item 40]
    [:item 50]
    [:item 60]]] )

(def t1-hiccup
  [:vec 1
   [:vec :a :b]
   2
   3
   [:vec 40 50 60]] )

(def t2-hiccup
  [:vec
   [:int 1]
   [:vec
    [:kw :a]
    [:kw :b]]
   [:int 2]
   [:int 3]
   [:vec
    [:int 40]
    [:int 50]
    [:int 60]]] )

(def z2-hiccup
  [:item
   [:item 1]
   [:item 2]
   [:item :a]
   [:item :b]
   [:item
    [:item 3]
    [:item 4]
    [:item :c]
    [:item :d]
    [:item 5]]
   [:item :e]] )

(def z3-hiccup
  [:item
   [:item 1]
   [:item 2]
   [:item :a]
   [:item :b]
   [:item :c]
   [:item :d]
   [:item :e]
   [:item 3]])

;-----------------------------------------------------------------------------
; t0-hiccup
(dotest
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup t0-hiccup)]
       (let [tree (hid->tree root-hid)
             bush (hid->bush root-hid)]
            (is= tree
              {:attrs {:tag :item},
               :kids
                      [{:attrs {:tag :item}, :value 1}
                       {:attrs {:tag :item},
                        :kids  [{:attrs {:tag :item}, :value :a}
                                {:attrs {:tag :item}, :value :b}]}
                       {:attrs {:tag :item}, :value 2}
                       {:attrs {:tag :item}, :value 3}
                       {:attrs {:tag :item},
                        :kids  [{:attrs {:tag :item}, :value 40}
                                {:attrs {:tag :item}, :value 50}
                                {:attrs {:tag :item}, :value 60}]}]})
         (is= bush
           [{:tag :item}
            [{:tag :item} 1]
            [{:tag :item}
             [{:tag :item} :a]
             [{:tag :item} :b]]
            [{:tag :item} 2]
            [{:tag :item} 3]
            [{:tag :item}
             [{:tag :item} 40]
             [{:tag :item} 50]
             [{:tag :item} 60]]]) )
      ; find all keyword leaves in order
      (let [leaf-hids-1  (find-leaf-hids root-hid [:** :*] :*)
            leaf-hids-2  (all-leaf-hids)
            >>           (is= (set leaf-hids-1) leaf-hids-2)
            kw-leaf-hids (keep-if #(keyword? (hid->value %)) leaf-hids-1) ; could keep only first one here
            leaves       (mapv hid->leaf kw-leaf-hids)]
           ; must use `val=` since (not= {:attrs {:tag :item}, :value :a}
           ;                  (map->Leaf {:attrs {:tag :item}, :value :a} ))
           (is (val= leaves
                 [{:attrs {:tag :item}, :value :a}
                  {:attrs {:tag :item}, :value :b}]))))))

; update the first child of the root using `inc`
(dotest
  (with-forest (new-forest)
    (let [root-hid    (add-tree-hiccup t0-hiccup)
          child-1-hid (first (hid->kids root-hid))
          >>          (value-update child-1-hid inc)
          result      (hid->leaf child-1-hid)]
         (is= result #tupelo.x_forest.Leaf{:attrs {:tag :item}, :value 2})
      (is= (hid->bush root-hid)
        [{:tag :item}
         [{:tag :item} 2]
         [{:tag :item}
          [{:tag :item} :a]
          [{:tag :item} :b]]
         [{:tag :item} 2]
         [{:tag :item} 3]
         [{:tag :item}
          [{:tag :item} 40]
          [{:tag :item} 50]
          [{:tag :item} 60]]]))))

; update the 2nd child of the root by appending :c
(dotest
  (with-forest (new-forest)
    (let [root-hid  (add-tree-hiccup t0-hiccup)
          kid-2-hid (xsecond (hid->kids root-hid))
          >>        (kids-append kid-2-hid [(add-leaf :item :c)])]
      (is= (hid->bush root-hid)
        [{:tag :item}
         [{:tag :item} 1]
         [{:tag :item}
          [{:tag :item} :a]
          [{:tag :item} :b]
          [{:tag :item} :c]]
         [{:tag :item} 2]
         [{:tag :item} 3]
         [{:tag :item}
          [{:tag :item} 40]
          [{:tag :item} 50]
          [{:tag :item} 60]]]))))

; update the 2nd child of the root by pre-pending :aa
(dotest
  (with-forest (new-forest)
    (let [root-hid  (add-tree-hiccup t0-hiccup)
          kid-2-hid (xsecond (hid->kids root-hid))
          >>        (kids-prepend kid-2-hid [(add-leaf :item :aa)])]
         (is= (hid->bush root-hid)
           [{:tag :item}
            [{:tag :item} 1]
            [{:tag :item}
             [{:tag :item} :aa]
             [{:tag :item} :a]
             [{:tag :item} :b] ]
            [{:tag :item} 2]
            [{:tag :item} 3]
            [{:tag :item}
             [{:tag :item} 40]
             [{:tag :item} 50]
             [{:tag :item} 60]]]))))

(defn leaf-gt-10?
  [path]
  (let [hid     (last path)
        keeper? (and (leaf-hid? hid)
                  (let [leaf-val (hid->value hid)]
                       (and (integer? leaf-val) (< 10 leaf-val))))]
     keeper?))

; delete any numbers (< 10 n)
(dotest
  (with-forest (new-forest)
    (let [root-hid  (add-tree-hiccup t0-hiccup)
          big-paths   (find-paths-with root-hid [:** :*] leaf-gt-10?)
          big-hids   (mapv last big-paths)
          big-leaves (mapv hid->leaf big-hids) ]
      (apply remove-hid big-hids)
      (is= (hid->bush root-hid)
        [{:tag :item}
         [{:tag :item} 1]
         [{:tag :item}
          [{:tag :item} :a]
          [{:tag :item} :b]]
         [{:tag :item} 2]
         [{:tag :item} 3]
         [{:tag :item}]])))) ; they're gone!


; delete any numbers (< 10 n)

(defn leaf-kw-hid? [hid]
  (and (leaf-hid? hid)
    (keyword? (hid->value hid))))

(s/defn kw-partition? :- s/Bool
  [partition :- [HID]]
  (leaf-kw-hid? (xfirst partition)))

(s/defn wrap-adjacent-kw-kids [hid]
  (let [kid-hids            (hid->kids hid)
        kid-elems           (mapv hid->elem kid-hids)
        kid-partitions      (partition-by leaf-kw-hid? kid-hids)
        kid-partitions-flgs (mapv kw-partition? kid-partitions)
        kid-partitions-new  (map-let [partition kid-partitions
                                      kw-part-flag kid-partitions-flgs]
                              (if kw-part-flag
                                [(add-node :item partition)]
                                partition))
        kids-new            (apply glue kid-partitions-new)
        ]
       (kids-set hid kids-new)))

(dotest
  (with-forest (new-forest)
    (let [root-hid        (add-tree-hiccup z2-hiccup) ]
         (is= (hid->hiccup root-hid)
           [:item
            [:item 1]
            [:item 2]
            [:item :a]
            [:item :b]
            [:item
             [:item 3]
             [:item 4]
             [:item :c]
             [:item :d]
             [:item 5]]
            [:item :e]])
      (mapv wrap-adjacent-kw-kids (all-node-hids))
      (is= (hid->hiccup root-hid)
        [:item
         [:item 1]
         [:item 2]
         [:item
          [:item :a]
          [:item :b]]
         [:item
          [:item 3]
          [:item 4]
          [:item
           [:item :c]
           [:item :d]]
          [:item 5]]
         [:item
          [:item :e]]]))))
(dotest
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup z3-hiccup)]
         (is= (hid->hiccup root-hid)
           [:item
            [:item 1]
            [:item 2]
            [:item :a]
            [:item :b]
            [:item :c]
            [:item :d]
            [:item :e]
            [:item 3]])
      (mapv wrap-adjacent-kw-kids (all-node-hids))
      (is= (hid->hiccup root-hid)
        [:item
         [:item 1]
         [:item 2]
         [:item [:item :a] [:item :b] [:item :c] [:item :d] [:item :e]]
         [:item 3]]))))


