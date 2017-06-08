;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.forest-examples
  (:use tupelo.forest tupelo.test )
  (:require
    [clojure.data.xml :as dx]
    [clojure.java.io :as io]
    [clojure.set :as cs]
    [net.cgrand.enlive-html :as en-html]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.string :as ts]))
(t/refer-tupelo)

; Examples from:
;   http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/
;   http://josf.info/blog/2014/03/28/clojure-zippers-structure-editing-with-your-mind/
;   http://josf.info/blog/2014/04/14/seqs-of-clojure-zippers/
;   http://josf.info/blog/2014/10/02/practical-zippers-extracting-text-with-enlive/

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
         (is= result #tupelo.forest.Leaf{:attrs {:tag :item}, :value 2})
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

;-----------------------------------------------------------------------------
(defn zappy
  "grab a webpage we can trust, and parse it into a zipper"
  []
  (dx/parse
    (io/input-stream
      (io/resource "clojure.zip-api.html"))))

(dotest
  (when false ; manually enable to grab a new copy of the webpage
    (spit "clojure-sample.html"
      (slurp "http://clojure.github.io/clojure/clojure.zip-api.html")))
  (with-forest (new-forest)
    (let [root-hid          (add-tree-enlive (zappy))
          a-node-paths      (find-paths root-hid [:** :a])
          extract-href-info (fn fn-extract-href-info [path]
                              (let [hid       (last path)
                                    href      (hid->attr hid :href)
                                    depth     (count path)
                                    curr-tree (hid->tree hid)
                                    num-hids  (with-forest (new-forest)
                                                (let [tmp-root (add-tree curr-tree)]
                                                     (count (all-hids))))]
                                   (vals->map href depth num-hids)))
          result-data       (mapv extract-href-info a-node-paths)]
       ; (spyx-pretty (hid->bush root-hid))
         (is (cs/subset?
               (set [{:href "index.html", :depth 5, :num-hids 2}
                     {:href "index.html", :depth 6, :num-hids 1}
                     {:href "index.html", :depth 9, :num-hids 1}
                     {:href "api-index.html", :depth 9, :num-hids 1}
                     {:href "clojure.core-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.data-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.edn-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.inspector-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.instant-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.java.browse-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.java.io-api.html", :depth 10, :num-hids 1}
                     {:href "clojure.java.javadoc-api.html", :depth 10, :num-hids 1}
                     {:href "http://clojure.org", :depth 7, :num-hids 1}
                     {:href "#toc0", :depth 12, :num-hids 1}
                     {:href "#", :depth 12, :num-hids 1}
                     {:href "#var-section", :depth 12, :num-hids 1}
                     {:href "#clojure.zip/append-child", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/branch?", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/children", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/down", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/edit", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/end?", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/root", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/seq-zip", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/up", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/vector-zip", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/xml-zip", :depth 13, :num-hids 1}
                     {:href "#clojure.zip/zipper", :depth 13, :num-hids 1}])
               (set result-data))))))

;-----------------------------------------------------------------------------
(dotest
  (with-forest (new-forest)
    (let [enlive-tree (->> "<p>sample <em>text</em> with words.</p>"
                        clojure.string/lower-case
                        java.io.StringReader.
                        en-html/html-resource
                        first)
          root-hid    (add-tree-enlive enlive-tree)
          leaf-hids   (find-leaf-hids root-hid [:** :*] :*)
          leaf-values (mapv hid->value leaf-hids)
          result      (apply glue leaf-values)]
         (is= enlive-tree
           '{:tag     :html
             :attrs   nil,
             :content ({:tag     :body,
                        :attrs   nil,
                        :content ({:tag     :p
                                   :attrs   nil,
                                   :content ("sample "
                                              {:tag :em, :attrs nil, :content ("text")}
                                              " with words.")})})})
      (is= (hid->tree root-hid)
        {:attrs {:tag :html},
         :kids  [{:attrs {:tag :body},
                  :kids  [{:attrs {:tag :p},
                           :kids  [{:attrs {:tag :tupelo.forest/raw}, :value "sample "}
                                   {:attrs {:tag :em}, :value "text"}
                                   {:attrs {:tag :tupelo.forest/raw}, :value " with words."}]}]}]})
      (is= (hid->hiccup root-hid)
        [:html
         [:body
          [:p
           [:tupelo.forest/raw "sample "]
           [:em "text"]
           [:tupelo.forest/raw " with words."]]]])

      (is= result "sample text with words."))))

;-----------------------------------------------------------------------------
; Discard any xml nodes of Type="A" or Type="B" (plus blank string nodes)
(dotest
  (with-forest (new-forest)
    (let [xml-str         "<ROOT>
                            <Items>
                              <Item><Type>A</Type><Note>AA1</Note></Item>
                              <Item><Type>B</Type><Note>BB1</Note></Item>
                              <Item><Type>C</Type><Note>CC1</Note></Item>
                              <Item><Type>A</Type><Note>AA2</Note></Item>
                            </Items>
                          </ROOT>"
          enlive-tree     (->> xml-str
                            java.io.StringReader.
                            en-html/html-resource
                            first)
          root-hid        (add-tree-enlive enlive-tree)
          tree-1          (hid->tree root-hid)

          blank-leaf-hid? (fn [hid] (and (leaf-hid? hid) ; ensure it is a leaf node
                                      (let [value (hid->value hid)]
                                           (and (string? value)
                                             (or (zero? (count value)) ; empty string
                                               (ts/whitespace? value)))))) ; all whitespace string

          type-bc-hid?    (fn [hid] (pos? (count (glue
                                                   (find-leaf-hids hid [:** :Type] "B")
                                                   (find-leaf-hids hid [:** :Type] "C")))))

          blank-leaf-hids (keep-if blank-leaf-hid? (all-hids))
          >>              (apply remove-hid blank-leaf-hids)
          tree-2          (hid->tree root-hid)

          type-bc-hids    (find-hids-with root-hid [:** :Item] type-bc-hid?)
          >>              (apply remove-hid type-bc-hids)
          tree-3          (hid->tree root-hid)
          tree-3-hiccup   (hid->hiccup root-hid)]
     (is= tree-1
       {:attrs {:tag :ROOT},
        :kids  [{:attrs {:tag :tupelo.forest/raw},
                 :value "\n                            "}
                {:attrs {:tag :Items},
                 :kids  [{:attrs {:tag :tupelo.forest/raw},
                          :value "\n                              "}
                         {:attrs {:tag :Item},
                          :kids  [{:attrs {:tag :Type}, :value "A"}
                                  {:attrs {:tag :Note}, :value "AA1"}]}
                         {:attrs {:tag :tupelo.forest/raw},
                          :value "\n                              "}
                         {:attrs {:tag :Item},
                          :kids  [{:attrs {:tag :Type}, :value "B"}
                                  {:attrs {:tag :Note}, :value "BB1"}]}
                         {:attrs {:tag :tupelo.forest/raw},
                          :value "\n                              "}
                         {:attrs {:tag :Item},
                          :kids  [{:attrs {:tag :Type}, :value "C"}
                                  {:attrs {:tag :Note}, :value "CC1"}]}
                         {:attrs {:tag :tupelo.forest/raw},
                          :value "\n                              "}
                         {:attrs {:tag :Item},
                          :kids  [{:attrs {:tag :Type}, :value "A"}
                                  {:attrs {:tag :Note}, :value "AA2"}]}
                         {:attrs {:tag :tupelo.forest/raw},
                          :value "\n                            "}]}
                {:attrs {:tag :tupelo.forest/raw},
                 :value "\n                          "}]})

    (is= tree-2
      {:attrs {:tag :ROOT},
       :kids
              [{:attrs {:tag :Items},
                :kids
                       [{:attrs {:tag :Item},
                         :kids
                                [{:attrs {:tag :Type}, :value "A"}
                                 {:attrs {:tag :Note}, :value "AA1"}]}
                        {:attrs {:tag :Item},
                         :kids
                                [{:attrs {:tag :Type}, :value "B"}
                                 {:attrs {:tag :Note}, :value "BB1"}]}
                        {:attrs {:tag :Item},
                         :kids
                                [{:attrs {:tag :Type}, :value "C"}
                                 {:attrs {:tag :Note}, :value "CC1"}]}
                        {:attrs {:tag :Item},
                         :kids
                                [{:attrs {:tag :Type}, :value "A"}
                                 {:attrs {:tag :Note}, :value "AA2"}]}]}]})
    (is= tree-3
      {:attrs {:tag :ROOT},
       :kids
              [{:attrs {:tag :Items},
                :kids  [{:attrs {:tag :Item},
                         :kids  [{:attrs {:tag :Type}, :value "A"}
                                 {:attrs {:tag :Note}, :value "AA1"}]}
                        {:attrs {:tag :Item},
                         :kids  [{:attrs {:tag :Type}, :value "A"}
                                 {:attrs {:tag :Note}, :value "AA2"}]}]}]})
    (is= tree-3-hiccup
      [:ROOT
       [:Items
        [:Item [:Type "A"] [:Note "AA1"]]
        [:Item [:Type "A"] [:Note "AA2"]]]]))))

;-----------------------------------------------------------------------------
; xml searching example
(def xml-str-prod "<data>
                    <products>
                      <product>
                        <section>Red Section</section>
                        <images>
                          <image>img.jpg</image>
                          <image>img2.jpg</image>
                        </images>
                      </product>
                      <product>
                        <section>Blue Section</section>
                        <images>
                          <image>img.jpg</image>
                          <image>img3.jpg</image>
                        </images>
                      </product>
                      <product>
                        <section>Green Section</section>
                        <images>
                          <image>img.jpg</image>
                          <image>img2.jpg</image>
                        </images>
                      </product>
                    </products>
                  </data> " )
(dotest
  (with-forest (new-forest)
    (let [enlive-tree          (->> xml-str-prod
                                 java.io.StringReader.
                                 en-html/html-resource
                                 first)
          root-hid             (add-tree-enlive enlive-tree)
          tree-1               (hid->hiccup root-hid)

          blank-leaf-hid?      (fn [hid] (and (leaf-hid? hid) ; ensure it is a leaf node
                                           (let [value (hid->value hid)]
                                                (and (string? value)
                                                  (or (zero? (count value)) ; empty string
                                                    (ts/whitespace? value)))))) ; all whitespace string

          blank-leaf-hids      (keep-if blank-leaf-hid? (all-hids))
          >>                   (apply remove-hid blank-leaf-hids)
          tree-2               (hid->hiccup root-hid)

          product-hids         (find-hids root-hid [:** :product])
          product-trees-hiccup (mapv hid->hiccup product-hids)

          img2-paths           (find-paths-leaf root-hid [:data :products :product :images :image] "img2.jpg")
          img2-prod-paths      (mapv #(drop-last 2 %) img2-paths)
          img2-prod-hids       (mapv last img2-prod-paths)
          img2-trees-hiccup    (mapv hid->hiccup img2-prod-hids)

          red-sect-paths       (find-paths-leaf root-hid [:data :products :product :section] "Red Section")
          red-prod-paths       (mapv #(drop-last 1 %) red-sect-paths)
          red-prod-hids        (mapv last red-prod-paths)
          red-trees-hiccup     (mapv hid->hiccup red-prod-hids)]
     ;(spyx-pretty tree-1)
     ;(spyx-pretty tree-2)
     (is= product-trees-hiccup
       [[:product
         [:section "Red Section"]
         [:images
          [:image "img.jpg"]
          [:image "img2.jpg"]]]
        [:product
         [:section "Blue Section"]
         [:images
          [:image "img.jpg"]
          [:image "img3.jpg"]]]
        [:product
         [:section "Green Section"]
         [:images
          [:image "img.jpg"]
          [:image "img2.jpg"]]]])

      (is= img2-trees-hiccup
        [[:product
          [:section "Red Section"]
          [:images
           [:image "img.jpg"]
           [:image "img2.jpg"]]]
         [:product
          [:section "Green Section"]
          [:images
           [:image "img.jpg"]
           [:image "img2.jpg"]]]])

      (is= red-trees-hiccup
        [[:product
          [:section "Red Section"]
          [:images
           [:image "img.jpg"]
           [:image "img2.jpg"]]]]))))
