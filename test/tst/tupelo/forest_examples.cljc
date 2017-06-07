;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.forest-examples
  (:use tupelo.x-forest tupelo.test )
  (:require
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

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

;-----------------------------------------------------------------------------
; t0-hiccup
(dotest
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup t0-hiccup)
          tree     (hid->tree root-hid)
          bush     (hid->bush root-hid)]
         (is= tree
           {:attrs {:tag :item},
            :kids
                   [{:attrs {:tag :item}, :value 1}
                    {:attrs {:tag :item},
                     :kids
                            [{:attrs {:tag :item}, :value :a} {:attrs {:tag :item}, :value :b}]}
                    {:attrs {:tag :item}, :value 2}
                    {:attrs {:tag :item}, :value 3}
                    {:attrs {:tag :item},
                     :kids
                            [{:attrs {:tag :item}, :value 40}
                             {:attrs {:tag :item}, :value 50}
                             {:attrs {:tag :item}, :value 60}]}]})
      (is= bush
        [{:tag :item}
         [{:tag :item} 1]
         [{:tag :item} [{:tag :item} :a] [{:tag :item} :b]]
         [{:tag :item} 2]
         [{:tag :item} 3]
         [{:tag :item} [{:tag :item} 40] [{:tag :item} 50] [{:tag :item} 60]]])
      )))