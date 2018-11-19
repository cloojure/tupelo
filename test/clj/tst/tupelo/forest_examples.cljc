;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.forest-examples
  #?(:clj (:use tupelo.core tupelo.forest tupelo.test))
  (:require
    [clojure.set :as cs]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.string :as ts]
    [tupelo.schema :as tsk]
    #?@(:clj [[clojure.data.xml :as xml]
              [clojure.java.io :as io]
              [net.cgrand.tagsoup :as enlive-tagsoup]
              [tupelo.forest :as tf]
              [tupelo.misc :as tm :refer [HID]]
              ])
  )
    #?(:clj (:import [java.io StringReader])  )
)

#?(:clj (do

(dotest
  (with-debug-hid
    (with-forest (new-forest)
      (let [root-hid  (add-tree-hiccup [:a
                                        [:b 1]
                                        [:b 2]
                                        [:b
                                         [:c 4]
                                         [:c 5]]
                                        [:c 9]])
            c-paths   (find-paths root-hid [:** :c])
            c4-paths  (find-paths root-hid [:** {:tag :c :value 4}])
            c4-hid    (-> c4-paths only last)
            c4-parent (-> c4-paths only reverse second)]
        (is= c-paths [[:0006 :0004 :0002]
                      [:0006 :0004 :0003]
                      [:0006 :0005]])
        (is= (hid->hiccup :0005) [:c 9])
        (is= c4-paths [[:0006 :0004 :0002]])
        (is= :0004 c4-parent)
        (is= (hid->hiccup c4-parent) [:b [:c 4] [:c 5]])
        (is= (hid->node c4-parent) {:tag :b, :tupelo.forest/khids [:0002 :0003]})
        (is= c4-hid :0002)
        (value-update c4-hid inc)
        (is= (hid->node c4-hid) {:tupelo.forest/khids [], :tag :c, :value 5})))))

(dotest
  (with-debug-hid
    (with-forest (new-forest)
      (let [root-hid (add-tree-hiccup [:a
                                       [:b 1]
                                       [:b 2]
                                       [:b
                                        [:c 4]
                                        [:c 5]]
                                       [:c 9]])]
        (is= (format-paths (find-paths root-hid [:a]))
          [[{:tag :a}
            [{:tag :b, :value 1}]
            [{:tag :b, :value 2}]
            [{:tag :b}
             [{:tag :c, :value 4}]
             [{:tag :c, :value 5}]]
            [{:tag :c, :value 9}]]])
        (is= (find-paths root-hid [:a :b])
          [[:0006 :0000] ; with-dbg-hid => 4-digit hex HID values
           [:0006 :0001]
           [:0006 :0004]] )
        ; Actual results: (find-paths aa [:a :b]) =>
        ;    [ [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :b40b6f37e6a746f815b092a8590cefe5cf37121a]
        ;      [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :76859beedd81468b4ee3cc5f17a5fdcf7a34a787]
        ;      [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :5c0cb1ba6657ba0ac40cc5099f2be091b5637a3b] ]
        (is= (format-paths (find-paths root-hid [:a :b]))
          [[{:tag :a}
            [{:tag :b, :value 1}]]
           [{:tag :a}
            [{:tag :b, :value 2}]]
           [{:tag :a}
            [{:tag :b}
             [{:tag :c, :value 4}]
             [{:tag :c, :value 5}]]]])
        (is= (format-paths (find-paths root-hid [:a :c]))
          [[{:tag :a} [{:tag :c, :value 9}]]])
        (is= (format-paths (find-paths root-hid [:a :b :c]))
          [[{:tag :a}
            [{:tag :b}
             [{:tag :c, :value 4}]]]
           [{:tag :a}
            [{:tag :b}
             [{:tag :c, :value 5}]]]])
        (is= (set (format-paths (find-paths root-hid [:* :b])))
          #{[{:tag :a}
             [{:tag :b :value 2}]]
            [{:tag :a}
             [{:tag :b :value 1}]]
            [{:tag :a}
             [{:tag :b}
              [{:tag :c :value 4}]
              [{:tag :c :value 5}]]]})
        (is= (format-paths (find-paths root-hid [:a :* :c]))
          [[{:tag :a} [{:tag :b} [{:tag :c :value 4}]]]
           [{:tag :a} [{:tag :b} [{:tag :c :value 5}]]]])
        (is= (format-paths (find-paths root-hid [:** :c]))
          [[{:tag :a} [{:tag :b} [{:tag :c, :value 4}]]]
           [{:tag :a} [{:tag :b} [{:tag :c, :value 5}]]]
           [{:tag :a} [{:tag :c, :value 9}]]])
      )))

  (with-forest (new-forest)
               (let [root-hid (add-tree-hiccup [:a
                                                [:b 1]
                                                [:b 2]
                                                [:b
                                                 [:c 4]
                                                 [:c 5]]
                                                [:c 9]])
                     ab1-hid (last (only (find-paths root-hid [:** {:tag :b :value 1}])))]
                 (is= (hid->bush ab1-hid) [{:tag :b :value 1}])
                 (value-update ab1-hid inc)
                 (is= (hid->bush ab1-hid) [{:tag :b :value 2}])
                 (is= (hid->bush root-hid)
                      [{:tag :a}
                       [{:tag :b, :value 2}]
                       [{:tag :b, :value 2}]
                       [{:tag :b} [{:tag :c, :value 4}] [{:tag :c, :value 5}]]
                       [{:tag :c, :value 9}]]))))

;-----------------------------------------------------------------------------

; Examples from:
;   http://josf.info/blog/2014/03/21/getting-acquainted-with-clojure-zippers/
;   http://josf.info/blog/2014/03/28/clojure-zippers-structure-editing-with-your-mind/
;   http://josf.info/blog/2014/04/14/seqs-of-clojure-zippers/
;   http://josf.info/blog/2014/10/02/practical-zippers-extracting-text-with-enlive/

;-----------------------------------------------------------------------------
; t0-data
(def t0-data
  [1 [:a :b] 2 3 [40 50 60]] )

;(dotest
;  (with-forest (new-forest)
;    (let [root-hid (add-tree (data->tree t0-data))]
;      (is= (hid->bush root-hid)
;        [{::tf/tag :root}
;         [{::tf/tag :data, ::tf/idx 0, :value 1}]
;         [{::tf/tag :data, ::tf/idx 1}
;          [{::tf/tag :data, ::tf/idx 0, :value :a}]
;          [{::tf/tag :data, ::tf/idx 1, :value :b}]]
;         [{::tf/tag :data, ::tf/idx 2, :value 2}]
;         [{::tf/tag :data, ::tf/idx 3, :value 3}]
;         [{::tf/tag :data, ::tf/idx 4}
;          [{::tf/tag :data, ::tf/idx 0, :value 40}]
;          [{::tf/tag :data, ::tf/idx 1, :value 50}]
;          [{::tf/tag :data, ::tf/idx 2, :value 60}]]])
;      (is= (hid->tree root-hid)
;        {::tf/tag  :root,
;         ::tf/kids [{::tf/kids [], ::tf/tag :data, ::tf/idx 0, :value 1}
;                    {::tf/tag  :data,
;                     ::tf/idx  1,
;                     ::tf/kids [{::tf/kids [], ::tf/tag :data, ::tf/idx 0, :value :a}
;                                {::tf/kids [], ::tf/tag :data, ::tf/idx 1, :value :b}]}
;                    {::tf/kids [], ::tf/tag :data, ::tf/idx 2, :value 2}
;                    {::tf/kids [], ::tf/tag :data, ::tf/idx 3, :value 3}
;                    {::tf/tag  :data,
;                     ::tf/idx  4,
;                     ::tf/kids [{::tf/kids [], ::tf/tag :data, ::tf/idx 0, :value 40}
;                                {::tf/kids [], ::tf/tag :data, ::tf/idx 1, :value 50}
;                                {::tf/kids [], ::tf/tag :data, ::tf/idx 2, :value 60}]}]} ) )))

;-----------------------------------------------------------------------------
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

(dotest
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup t0-hiccup)
          tree (hid->tree root-hid)
          bush (hid->bush root-hid)]
       (is= tree
         {:tag :item,
          ::tf/kids
               [{::tf/kids [], :tag :item, :value 1}
                {:tag :item,
                 ::tf/kids
                      [{::tf/kids [], :tag :item, :value :a}
                       {::tf/kids [], :tag :item, :value :b}]}
                {::tf/kids [], :tag :item, :value 2}
                {::tf/kids [], :tag :item, :value 3}
                {:tag :item,
                 ::tf/kids
                      [{::tf/kids [], :tag :item, :value 40}
                       {::tf/kids [], :tag :item, :value 50}
                       {::tf/kids [], :tag :item, :value 60}]}]})
      (is= bush
        [{:tag :item}
         [{:tag :item, :value 1}]
         [{:tag :item}
          [{:tag :item, :value :a}]
          [{:tag :item, :value :b}]]
         [{:tag :item, :value 2}]
         [{:tag :item, :value 3}]
         [{:tag :item}
          [{:tag :item, :value 40}]
          [{:tag :item, :value 50}]
          [{:tag :item, :value 60}]]])
      ; find all keyword leaves in order
      (let [leaf-hids-1  (find-leaf-hids root-hid [:** :*])
            leaf-hids-2  (all-leaf-hids)
            kw-leaf-hids (keep-if #(keyword? (grab :value (hid->node %))) leaf-hids-1) ; could keep only first one here
            leaves       (mapv hid->leaf kw-leaf-hids)]
        (is= (set leaf-hids-1) leaf-hids-2)
        ; must use `val=` since (not= {:attrs {:tag :item}, ::value :a}
        ;                  (map->Node {:attrs {:tag :item}, ::value :a} ))
        (is= leaves
          [{::tf/khids [], :tag :item, :value :a}
           {::tf/khids [], :tag :item, :value :b}]))
      )))


; update the first child of the root using `inc`
(dotest
  (with-forest (new-forest)
    (let [root-hid    (add-tree-hiccup t0-hiccup)
          child-1-hid (first (hid->kids root-hid))
          >>          (value-update child-1-hid inc)
          result      (hid->leaf child-1-hid)]
         (is= result {::tf/khids [], :tag :item, :value 2} )
      (is= (hid->hiccup root-hid)
        [:item
         [:item 2]
         [:item [:item :a] [:item :b]]
         [:item 2]
         [:item 3]
         [:item [:item 40] [:item 50] [:item 60]]] ))))

; update the 2nd child of the root by appending :c
(dotest
  (with-forest (new-forest)
    (let [root-hid  (add-tree-hiccup t0-hiccup)
          kid-2-hid (xsecond (hid->kids root-hid))
          >>        (kids-append kid-2-hid [(add-leaf :item :c)])]
         (is= (hid->hiccup root-hid)
           [:item
            [:item 1]
            [:item [:item :a] [:item :b] [:item :c]]
            [:item 2]
            [:item 3]
            [:item [:item 40] [:item 50] [:item 60]]] ))))

; update the 2nd child of the root by pre-pending :aa
(dotest
  (with-forest (new-forest)
    (let [root-hid  (add-tree-hiccup t0-hiccup)
          kid-2-hid (xsecond (hid->kids root-hid))
          >>        (kids-prepend kid-2-hid [(add-leaf :item :aa)])]
         (is= (hid->hiccup root-hid)
           [:item
            [:item 1]
            [:item [:item :aa] [:item :a] [:item :b]]
            [:item 2]
            [:item 3]
            [:item [:item 40] [:item 50] [:item 60]]] ))))

(defn leaf-gt-10?
  [path]
  (let [hid     (last path)
        keeper? (and (leaf-hid? hid)
                  (let [leaf-val (grab :value (hid->node hid))]
                       (and (integer? leaf-val) (< 10 leaf-val))))]
     keeper?))

; delete any numbers (< 10 n)
(dotest
  (with-forest (new-forest)
    (let [root-hid  (add-tree-hiccup t0-hiccup)
          big-paths (find-paths-with root-hid [:** :*] leaf-gt-10?)
          big-hids  (mapv last big-paths)]
         (apply remove-hid big-hids)
      (is= (hid->hiccup root-hid)
        [:item
         [:item 1]
         [:item [:item :a] [:item :b]]
         [:item 2]
         [:item 3]
         [:item]])))) ; they're gone!


;-----------------------------------------------------------------------------
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

(defn leaf-kw-hid? [hid]
  (and (leaf-hid? hid)
    (keyword? (grab :value (hid->node hid)))))

(s/defn kw-partition? :- s/Bool
  [partition :- [tm/HID]]
  (leaf-kw-hid? (xfirst partition)))

(s/defn wrap-adjacent-kw-kids [hid]
  (let [kid-hids            (hid->kids hid)
        kid-elems           (mapv hid->node kid-hids)
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
    (let [root-hid  (add-tree-hiccup z2-hiccup) ]
       (is= (hid->hiccup root-hid)
         [:item
          [:item 1]
          [:item 2]
          [:item :a]
          [:item :b]
          [:item [:item 3] [:item 4] [:item :c] [:item :d] [:item 5]]
          [:item :e]] )
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

;-----------------------------------------------------------------------------
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
  (let [; >> (println "io/resource - BEFORE")
        v1 (io/resource "clojure.zip-api.html")
        ; >> (println "io/resource - AFTER")
        ; >> (println "io/input-stream - BEFORE")
        v2 (io/input-stream v1)
        ; >> (println "io/input-stream - AFTER")
        ;>> (do
        ;     (nl)
        ;     (println (class v2))
        ;     (println "*****************************************************************************")
        ;     (println v2)
        ;     (println "*****************************************************************************")
        ;     (nl))

       ;>> (println "xml/parse - BEFORE")
        v3 (xml/parse v2)
       ;>> (println "xml/parse - AFTER")
      ]
    v3))

;(dotest
;  (println "zappy - BEFORE")
;  (zappy)
;  (println "zappy - AFTER"))

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
    (let [enlive-tree (xml->enlive "<p>sample <em>text</em> with words.</p>" )
          root-hid    (add-tree-enlive enlive-tree)
          leaf-hids   (find-leaf-hids root-hid [:** :*])
          leaf-values (mapv #(grab :value (hid->node %)) leaf-hids)
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
        {:tag      :html,
         ::tf/kids [{:tag      :body,
                     ::tf/kids [{:tag      :p,
                                 ::tf/kids [{:tag ::tf/raw, :value "sample " ::tf/kids []}
                                            {:tag :em, :value "text" ::tf/kids []}
                                            {:tag ::tf/raw, :value " with words." ::tf/kids []}]}]}]})
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
          root-hid        (add-tree-xml xml-str )
          tree-1          (hid->tree root-hid)

          type-bc-hid?    (fn [hid] (or (has-child-leaf? hid [:** {:tag :Type :value "B"}])
                                        (has-child-leaf? hid [:** {:tag :Type :value "C"}])))

          blank-leaf-hids (keep-if whitespace-leaf-hid? (all-hids))
          >>              (apply remove-hid blank-leaf-hids)
          tree-2          (hid->tree root-hid)

          type-bc-hids    (find-hids-with root-hid [:** :Item] type-bc-hid?)
          >>              (apply remove-hid type-bc-hids)
          tree-3          (hid->tree root-hid)
          tree-3-hiccup   (hid->hiccup root-hid)]
     (is= tree-1
       {:tag :ROOT,
        ::tf/kids
             [{::tf/kids [], :tag ::tf/raw, :value "\n                            "}
              {:tag :Items,
               ::tf/kids
                    [{::tf/kids [], :tag ::tf/raw, :value "\n                              "}
                     {:tag :Item,
                      ::tf/kids
                           [{::tf/kids [], :tag :Type, :value "A"}
                            {::tf/kids [], :tag :Note, :value "AA1"}]}
                     {::tf/kids [], :tag ::tf/raw, :value "\n                              "}
                     {:tag :Item,
                      ::tf/kids
                           [{::tf/kids [], :tag :Type, :value "B"}
                            {::tf/kids [], :tag :Note, :value "BB1"}]}
                     {::tf/kids [], :tag ::tf/raw, :value "\n                              "}
                     {:tag :Item,
                      ::tf/kids
                           [{::tf/kids [], :tag :Type, :value "C"}
                            {::tf/kids [], :tag :Note, :value "CC1"}]}
                     {::tf/kids [], :tag ::tf/raw, :value "\n                              "}
                     {:tag :Item,
                      ::tf/kids
                           [{::tf/kids [], :tag :Type, :value "A"}
                            {::tf/kids [], :tag :Note, :value "AA2"}]}
                     {::tf/kids [], :tag ::tf/raw, :value "\n                            "}]}
              {::tf/kids [], :tag ::tf/raw, :value "\n                          "}]} )

      (is= tree-2
        {:tag      :ROOT,
         ::tf/kids [{:tag :Items,
                     ::tf/kids
                          [{:tag :Item,
                            ::tf/kids
                                 [{::tf/kids [], :tag :Type, :value "A"}
                                  {::tf/kids [], :tag :Note, :value "AA1"}]}
                           {:tag :Item,
                            ::tf/kids
                                 [{::tf/kids [], :tag :Type, :value "B"}
                                  {::tf/kids [], :tag :Note, :value "BB1"}]}
                           {:tag :Item,
                            ::tf/kids
                                 [{::tf/kids [], :tag :Type, :value "C"}
                                  {::tf/kids [], :tag :Note, :value "CC1"}]}
                           {:tag :Item,
                            ::tf/kids
                                 [{::tf/kids [], :tag :Type, :value "A"}
                                  {::tf/kids [], :tag :Note, :value "AA2"}]}]}]})
    (is= tree-3
      {:tag      :ROOT,
       ::tf/kids [{:tag      :Items,
                   ::tf/kids [{:tag :Item,
                               ::tf/kids
                                    [{::tf/kids [], :tag :Type, :value "A"}
                                     {::tf/kids [], :tag :Note, :value "AA1"}]}
                              {:tag :Item,
                               ::tf/kids
                                    [{::tf/kids [], :tag :Type, :value "A"}
                                     {::tf/kids [], :tag :Note, :value "AA2"}]}]}]} )
    (is= tree-3-hiccup
      [:ROOT
       [:Items
        [:Item [:Type "A"] [:Note "AA1"]]
        [:Item [:Type "A"] [:Note "AA2"]]]]))))

;-----------------------------------------------------------------------------
; shorter version w/o extra features
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
          root-hid        (add-tree-xml xml-str)
          has-bc-leaf?    (fn [hid] (or (has-child-leaf? hid [:** {:tag :Type :value "B"}])
                                      (has-child-leaf? hid [:** {:tag :Type :value "C"}])))
          >>              (remove-whitespace-leaves)
          bc-item-hids    (find-hids-with root-hid [:** :Item] has-bc-leaf?)]
      (apply remove-hid bc-item-hids)
      (is= (hid->hiccup root-hid)
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
    (let [root-hid             (add-tree-xml xml-str-prod)
          >>                   (remove-whitespace-leaves)

          product-hids         (find-hids root-hid [:** :product])
          product-trees-hiccup (mapv hid->hiccup product-hids)

          has-img2-leaf?       (fn [hid] (has-child-leaf? hid [:product :images {:tag :image :value "img2.jpg"}]))

          img2-prod-hids       (find-hids-with root-hid [:data :products :product] has-img2-leaf?)
          img2-trees-hiccup    (mapv hid->hiccup img2-prod-hids)

          red-sect-paths       (find-leaf-paths root-hid [:** {:tag :section :value "Red Section"}])
          red-prod-paths       (mapv #(drop-last 1 %) red-sect-paths)
          red-prod-hids        (mapv last red-prod-paths)
          red-trees-hiccup     (mapv hid->hiccup red-prod-hids)]
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

;-----------------------------------------------------------------------------
(dotest
  (with-forest (new-forest)
    (let [xml-str  "<html>
                     <body>
                       <div class='one'>
                         <div class='two'></div>
                       </div>
                     </body>
                   </html>"
          root-hid (add-tree-xml xml-str)

          ; Removing whitespace nodes is optional; just done to keep things neat
          >>       (remove-whitespace-leaves)

          ; Can search for inner `div` 2 ways
          result-1 (find-paths root-hid [:html :body :div :div]) ; explicit path from root
          result-2 (find-paths root-hid [:** {:class "two"}]) ; wildcard path that ends in :class "two"
          ]
      (is= result-1 result-2) ; both searches return the same path
      (is= (hid->bush root-hid)
        [{:tag :html}
         [{:tag :body}
          [{:class "one", :tag :div}
           [{:class "two", :tag :div}]]]])
      (is=
        (format-paths result-1)
        (format-paths result-2)
        [[{:tag :html}
          [{:tag :body}
           [{:class "one", :tag :div}
            [{:class "two", :tag :div}]]]]])

      (is (= (hid->node (last (only result-1)))
            {::tf/khids [], :class "two", :tag :div})) )))

;-----------------------------------------------------------------------------

(dotest
  (with-forest (new-forest)
    ; #todo re-work to fix "special" double-quotes
    (let [html-str        "<div class=“group”>
                              <h2>title1</h2>
                              <div class=“subgroup”>
                                <p>unused</p>
                                <h3>subheading1</h3>
                                <a href=“path1” />
                              </div>
                              <div class=“subgroup”>
                                <p>unused</p>
                                <h3>subheading2</h3>
                                <a href=“path2” />
                              </div>
                            </div>
                            <div class=“group”>
                              <h2>title2</h2>
                              <div class=“subgroup”>
                                <p>unused</p>
                                <h3>subheading3</h3>
                                <a href=“path3” />
                              </div>
                            </div>"
          root-hid        (add-tree-xml html-str) ; html is a subset of xml

          ; Removing whitespace nodes is optional; just done to keep things neat
          >>              (remove-whitespace-leaves)

          tree-2          (hid->hiccup root-hid)
          >>              (is= tree-2 [:html
                                       [:body
                                        [:div {:class "“group”"}
                                         [:h2 "title1"]
                                         [:div {:class "“subgroup”"}
                                          [:p "unused"]
                                          [:h3 "subheading1"]
                                          [:a {:href "“path1”"}]]
                                         [:div {:class "“subgroup”"}
                                          [:p "unused"]
                                          [:h3 "subheading2"]
                                          [:a {:href "“path2”"}]]]
                                        [:div {:class "“group”"}
                                         [:h2 "title2"]
                                         [:div {:class "“subgroup”"}
                                          [:p "unused"]
                                          [:h3 "subheading3"]
                                          [:a {:href "“path3”"}]]]]])

          ; find consectutive nested [:div :h2] pairs at any depth in the tree
          div-h2-paths    (find-paths root-hid [:** :div :h2])
          >>              (is= (format-paths div-h2-paths)
                            [[{:tag :html}
                              [{:tag :body}
                               [{:class "“group”", :tag :div}
                                [{:tag :h2, :value "title1"}]]]]
                             [{:tag :html}
                              [{:tag :body}
                               [{:class "“group”", :tag :div}
                                [{:tag :h2, :value "title2"}]]]]])

          ; find the hid for each top-level :div (i.e. "group"); the next-to-last (-2) hid in each vector
          div-hids        (mapv #(idx % -2) div-h2-paths)
          ; for each of div-hids, find and collect nested :h3 values
          dif-h3-paths    (glue-rows
                            (forv [div-hid div-hids]
                              (let [h2-value (grab :value (hid->node (find-hid div-hid [:div :h2])))
                                    h3-paths  (find-paths div-hid [:** :h3])
                                    h3-values (it-> h3-paths
                                                (mapv last it)
                                                (mapv hid->node it)
                                                (mapv #(grab :value %) it))]
                                (forv [h3-value h3-values]
                                  [h2-value h3-value]))))
          ]
      (is= dif-h3-paths
        [["title1" "subheading1"]
         ["title1" "subheading2"]
         ["title2" "subheading3"]]) )))

;-----------------------------------------------------------------------------
(dotest
  (with-debug-hid
    (with-forest (new-forest)
      (let [xml-str  "<top>
                        <group>
                            <group>
                                <item>
                                    <number>1</number>
                                </item>
                                <item>
                                    <number>2</number>
                                </item>
                                <item>
                                    <number>3</number>
                                </item>
                            </group>
                            <item>
                                <number>0</number>
                            </item>
                        </group>
                    </top>"
            root-hid (add-tree-xml xml-str)
            >>       (remove-whitespace-leaves)

            ; Can search for inner `div` 2 ways
            result-1 (find-paths root-hid [:top :group :group])  ; explicit path from root
            result-2 (find-paths root-hid [:** :item :number]) ] ; wildcard path that ends in [:item :number]
        (is= (hid->bush root-hid)
          [{:tag :top}
           [{:tag :group}
            [{:tag :group}
             [{:tag :item} [{:tag :number, :value "1"}]]
             [{:tag :item} [{:tag :number, :value "2"}]]
             [{:tag :item} [{:tag :number, :value "3"}]]]
            [{:tag :item} [{:tag :number, :value "0"}]]]])

        ; Here we see only the double-nested items 1, 2, 3
        ; sample result-1 (with-debug-hid) => [[:001b :0019 :0012]]
        (is= (format-paths result-1)
          [[{:tag :top}
            [{:tag :group}
             [{:tag :group}
              [{:tag :item} [{:tag :number, :value "1"}]]
              [{:tag :item} [{:tag :number, :value "2"}]]
              [{:tag :item} [{:tag :number, :value "3"}]]]]]])

        ; Here we see both the double-nested items & the single-nested item 0
        ; sample result-2 (with-debug-hid) =>
        ;   [[:001b :0019 :0012 :0006 :0004]
        ;    [:001b :0019 :0012 :000b :0009]
        ;    [:001b :0019 :0012 :0010 :000e]
        ;    [:001b :0019 :0017 :0015]]
        (is= (set (format-paths result-2)) ; need `set` since order is non-deterministic
          (set
            [[{:tag :top}
              [{:tag :group} [{:tag :item} [{:tag :number, :value "0"}]]]]
             [{:tag :top}
              [{:tag :group}
               [{:tag :group} [{:tag :item} [{:tag :number, :value "1"}]]]]]
             [{:tag :top}
              [{:tag :group}
               [{:tag :group} [{:tag :item} [{:tag :number, :value "2"}]]]]]
             [{:tag :top}
              [{:tag :group}
               [{:tag :group} [{:tag :item} [{:tag :number, :value "3"}]]]]]])) ))))

(dotest
  (with-forest (new-forest)
    (let [root-hid   (add-tree-enlive
                       {:tag     :eSearchResult,
                        :attrs   {},
                        :content [
                            {:tag :Count, :attrs {}, :content ["16"]}
                            {:tag :RetMax, :attrs {}, :content ["16"]}
                            {:tag :RetStart, :attrs {}, :content ["0"]}
                            {:tag     :IdList,
                             :attrs   {},
                             :content [
                                 {:tag :Id, :attrs {}, :content ["28911150"]}
                                 {:tag :Id, :attrs {}, :content ["28899394"]}
                                 {:tag :Id, :attrs {}, :content ["28597238"]}
                                 {:tag :Id, :attrs {}, :content ["28263281"]}
                                 {:tag :Id, :attrs {}, :content ["28125459"]}
                                 {:tag :Id, :attrs {}, :content ["26911135"]}
                                 {:tag :Id, :attrs {}, :content ["26699345"]}
                                 {:tag :Id, :attrs {}, :content ["26297102"]}
                                 {:tag :Id, :attrs {}, :content ["26004019"]}
                                 {:tag :Id, :attrs {}, :content ["25995331"]}
                                 {:tag :Id, :attrs {}, :content ["25429093"]}
                                 {:tag :Id, :attrs {}, :content ["25355095"]}
                                 {:tag :Id, :attrs {}, :content ["25224593"]}
                                 {:tag :Id, :attrs {}, :content ["24816246"]}
                                 {:tag :Id, :attrs {}, :content ["24779721"]}
                                 {:tag :Id, :attrs {}, :content ["24740865"]} ]}]})
          id-content-paths (find-paths root-hid [:eSearchResult :IdList :Id])
          id-strings       (forv [path id-content-paths]
                             (grab :value (hid->leaf (last path))))]
      (is= (hid->bush root-hid)
        [{:tag :eSearchResult}
         [{:tag :Count, :value "16"}]
         [{:tag :RetMax, :value "16"}]
         [{:tag :RetStart, :value "0"}]
         [{:tag :IdList}
          [{:tag :Id, :value "28911150"}]
          [{:tag :Id, :value "28899394"}]
          [{:tag :Id, :value "28597238"}]
          [{:tag :Id, :value "28263281"}]
          [{:tag :Id, :value "28125459"}]
          [{:tag :Id, :value "26911135"}]
          [{:tag :Id, :value "26699345"}]
          [{:tag :Id, :value "26297102"}]
          [{:tag :Id, :value "26004019"}]
          [{:tag :Id, :value "25995331"}]
          [{:tag :Id, :value "25429093"}]
          [{:tag :Id, :value "25355095"}]
          [{:tag :Id, :value "25224593"}]
          [{:tag :Id, :value "24816246"}]
          [{:tag :Id, :value "24779721"}]
          [{:tag :Id, :value "24740865"}]]])
      (is= (format-paths id-content-paths)
        [[{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "28911150"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "28899394"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "28597238"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "28263281"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "28125459"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "26911135"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "26699345"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "26297102"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "26004019"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "25995331"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "25429093"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "25355095"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "25224593"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "24816246"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "24779721"}]]]
         [{:tag :eSearchResult} [{:tag :IdList} [{:tag :Id, :value "24740865"}]]]])
      (is= id-strings
        ["28911150"
         "28899394"
         "28597238"
         "28263281"
         "28125459"
         "26911135"
         "26699345"
         "26297102"
         "26004019"
         "25995331"
         "25429093"
         "25355095"
         "25224593"
         "24816246"
         "24779721"
         "24740865"]) )))

;---------------------------------------------------------------------------
(dotest
  (with-forest (new-forest)
    (let [root-hid (add-tree
                     (data->tree
                       {:bucket-aggregation
                        {:buckets
                         [{:key "outer_bucket"
                           :bucket-aggregation
                                {:buckets
                                 [{:key "inner_bucket_1"
                                   :bucket-aggregation
                                        {:buckets
                                         [{:key 1510657200000, :sum {:value 25}}
                                          {:key 1510660800000, :sum {:value 50}}]}}
                                  {:key "inner_bucket_2"
                                   :bucket-aggregation
                                        {:buckets
                                         [{:key 1510657200000, :sum {:value 30}}
                                          {:key 1510660800000, :sum {:value 35}}]}}
                                  {:key "inner_bucket_3"
                                   :bucket-aggregation
                                        {:buckets
                                         [{:key 1510657200000, :sum {:value 40}}
                                          {:key 1510660800000, :sum {:value 45}}]}}]}}]}}
                         )
                       )
          value-paths (find-paths root-hid [:** {::tf/key :value} {::tf/value :*}])
          tail-hids (mapv last value-paths)
          value-nodes (mapv #(grab ::tf/value (hid->node %)) tail-hids) ]

      (is= value-nodes [25 50 30 35 40 45])
      ; #todo  Want output like so (better than DataScript):
      ; #todo  RE:  https://stackoverflow.com/questions/47438985/clojure-parsing-elasticsearch-query-response-and-extracting-values
      (def desired-result
        [{:key ["outer_bucket" "inner_bucket_1" 1510657200000], :value 25}
         {:key ["outer_bucket" "inner_bucket_1" 1510660800000], :value 50}
         {:key ["outer_bucket" "inner_bucket_2" 1510657200000], :value 30}
         {:key ["outer_bucket" "inner_bucket_2" 1510660800000], :value 35}
         {:key ["outer_bucket" "inner_bucket_3" 1510657200000], :value 40}
         {:key ["outer_bucket" "inner_bucket_3" 1510660800000], :value 45}]
        ))))

;-----------------------------------------------------------------------------
(dotest
  (with-forest (new-forest)
    (let [xml-str         "<?xml version=\"1.0\"?>
                            <root>
                              <a>1</a>
                              <b>2</b>
                           </root>"
          root-hid        (add-tree-xml xml-str)
          bush-blanks     (hid->bush root-hid)
          >>              (remove-whitespace-leaves)

          bush-no-blanks  (hid->bush root-hid)
          leaf-hids       (find-leaf-hids root-hid [:** :*])]
      (is= bush-blanks [{:tag :root}
                        [{:tag :tupelo.forest/raw, :value "\n                              "}]
                        [{:tag :a, :value "1"}]
                        [{:tag :tupelo.forest/raw, :value "\n                              "}]
                        [{:tag :b, :value "2"}]
                        [{:tag :tupelo.forest/raw, :value "\n                           "}]])
      (is= bush-no-blanks [{:tag :root}
                           [{:tag :a, :value "1"}]
                           [{:tag :b, :value "2"}]])
      (is= (mapv hid->node leaf-hids)
        [{:tupelo.forest/khids [], :tag :a, :value "1"}
         {:tupelo.forest/khids [], :tag :b, :value "2"}]))))
;-----------------------------------------------------------------------------
(dotest
  (let [xml-str (ts/quotes->double
                  "<document>
                     <sentence id='1'>
                       <word id='1.1'>foo</word>
                       <word id='1.2'>bar</word>
                     </sentence>
                     <sentence id='2'>
                       <word id='2.1'>beyond</word>
                       <word id='2.2'>all</word>
                       <word id='2.3'>recognition</word>
                     </sentence>
                   </document>")]
    (with-forest (new-forest)
      (let [root-hid       (add-tree-xml xml-str)
            >>             (remove-whitespace-leaves)
            bush-no-blanks (hid->bush root-hid)
            sentence-hids  (find-hids root-hid [:document :sentence])
            sentences      (forv [sentence-hid sentence-hids]
                             (let [word-hids     (hid->kids sentence-hid)
                                   words         (mapv #(grab :value (hid->leaf %)) word-hids)
                                   sentence-text (str/join \space words)]
                               sentence-text))]
        (is= bush-no-blanks
          [{:tag :document}
           [{:id "1", :tag :sentence}
            [{:id "1.1", :tag :word, :value "foo"}]
            [{:id "1.2", :tag :word, :value "bar"}]]
           [{:id "2", :tag :sentence}
            [{:id "2.1", :tag :word, :value "beyond"}]
            [{:id "2.2", :tag :word, :value "all"}]
            [{:id "2.3", :tag :word, :value "recognition"}]]])
        (set= sentences ["foo bar" "beyond all recognition"])))
    (with-forest (new-forest)
      (let [enlive-tree-lazy (xml/parse (StringReader. xml-str))
            enlive-words     (filter-enlive-subtrees enlive-tree-lazy [:document :sentence :word])
            root-hids        (forv [word enlive-words] (add-tree-enlive word))
            >>               (remove-whitespace-leaves)
            bush-words       (forv [root-hid root-hids] (hid->bush root-hid))]
        (is= bush-words
          [[{:tag :document}
            [{:id "1", :tag :sentence}
             [{:id "1.1", :tag :word, :value "foo"}]]]
           [{:tag :document}
            [{:id "1", :tag :sentence}
             [{:id "1.2", :tag :word, :value "bar"}]]]
           [{:tag :document}
            [{:id "2", :tag :sentence}
             [{:id "2.1", :tag :word, :value "beyond"}]]]
           [{:tag :document}
            [{:id "2", :tag :sentence}
             [{:id "2.2", :tag :word, :value "all"}]]]
           [{:tag :document}
            [{:id "2", :tag :sentence}
             [{:id "2.3", :tag :word, :value "recognition"}]]]])))
    (with-forest (new-forest)
      (let [enlive-tree-lazy    (xml/parse (StringReader. xml-str))
            enlive-sentences    (filter-enlive-subtrees enlive-tree-lazy [:document :sentence])
            root-hids           (forv [sentence enlive-sentences] (add-tree-enlive sentence))
            >>                  (remove-whitespace-leaves)
            bush-sentences      (forv [root-hid root-hids] (hid->bush root-hid))
            sentence-hids       (find-hids root-hids [:document :sentence])
            sentence-extract-fn (fn [sentence-hid]
                                  (let [word-hids     (hid->kids sentence-hid)
                                        words         (mapv #(grab :value (hid->leaf %)) word-hids)
                                        sentence-text (str/join \space words)]
                                    sentence-text))
            result-sentences    (mapv sentence-extract-fn sentence-hids)]
        (is= bush-sentences
          [[{:tag :document}
            [{:id "1", :tag :sentence}
             [{:id "1.1", :tag :word, :value "foo"}]
             [{:id "1.2", :tag :word, :value "bar"}]]]
           [{:tag :document}
            [{:id "2", :tag :sentence}
             [{:id "2.1", :tag :word, :value "beyond"}]
             [{:id "2.2", :tag :word, :value "all"}]
             [{:id "2.3", :tag :word, :value "recognition"}]]]])
        (set= result-sentences ["foo bar" "beyond all recognition"])))
    (with-forest (new-forest)
      (let [enlive-tree-lazy (xml/parse (StringReader. xml-str))
            enlive-document  (only (filter-enlive-subtrees enlive-tree-lazy [:document]))
            root-hid         (add-tree-enlive enlive-document)
            >>               (remove-whitespace-leaves)
            bush-document    (hid->bush root-hid)]
        (is= bush-document
          [{:tag :document}
           [{:id "1", :tag :sentence}
            [{:id "1.1", :tag :word, :value "foo"}]
            [{:id "1.2", :tag :word, :value "bar"}]]
           [{:id "2", :tag :sentence}
            [{:id "2.1", :tag :word, :value "beyond"}]
            [{:id "2.2", :tag :word, :value "all"}]
            [{:id "2.3", :tag :word, :value "recognition"}]]])))))

;---------------------------------------------------------------------------------------------------
(defn get-xkcd-enlive
  "Load a sample webpage from disk"
  []
  (it-> "xkcd-sample.html"
    (io/resource it)
    (io/input-stream it)
    (enlive-tagsoup/parser it)
    (drop-if #(= :dtd (:type %)) it)
    (only it)))

(dotest
  (when false       ; manually enable to grab a new copy of the webpage
    (spit "xkcd-sample.html"
      (slurp "https://xkcd.com")))
  (with-forest (new-forest)
    (let [root-hid    (add-tree-enlive (get-xkcd-enlive))
          >>          (remove-whitespace-leaves)
          hid-keep-fn (fn [hid]
                        (let [node       (hid->node hid)
                              value      (when (contains? node :value) (grab :value node))
                              perm-link? (when (string? value)
                                           (re-find #"Permanent link to this comic" value))]
                          perm-link?))
          found-hids  (find-hids-with root-hid [:** :*] hid-keep-fn)
          link-node   (hid->node (only found-hids)) ; assume there is only 1 link node
          value-str   (grab :value link-node)
          result      (re-find #"http.*$" value-str)]
      (is= value-str "\nPermanent link to this comic: https://xkcd.com/1988/")
      (is= "https://xkcd.com/1988/" result))))

;-----------------------------------------------------------------------------
; Random AST generation of specified size
;   https://stackoverflow.com/questions/52125331/why-is-a-successful-update-returning-1-rows-affected
;   https://cs.gmu.edu/~sean/book/metaheuristics/

(def op->arity {:add 2
                :sub 2
                :mul 2
                :div 2
                :pow 2})
(def op-set (set (keys op->arity)))
(defn choose-rand-op [] (rand-elem op-set))

(def arg-set #{:x :y})
(defn choose-rand-arg [] (rand-elem arg-set))

(defn num-hids [] (count (all-hids)))

(s/defn hid->empty-kids :- s/Int
  [hid :- HID]
  (let [op             (hid->attr hid :op)
        arity          (grab op op->arity)
        kid-slots-used (count (hid->kids hid))
        num-empties    (- arity kid-slots-used)]
    (verify (= 2 arity))
    (verify (not (neg? num-empties)))
    num-empties))

(s/defn node-has-empty-slot? :- s/Bool
  [hid :- HID]
  (pos? (hid->empty-kids hid)))

(s/defn total-empty-kids :- s/Int
  []
  (reduce +
    (mapv hid->empty-kids (all-hids))))

(s/defn add-op-node :- HID
  [op :- s/Keyword]
  (add-node {:tag :op :op op})) ; add node w no kids

(s/defn add-leaf-node :- tsk/KeyMap
  [parent-hid :- HID
   arg :- s/Keyword]
  (kids-append parent-hid [(add-leaf {:tag :arg :arg arg})]))

(s/defn need-more-op? :- s/Bool
  [tgt-size :- s/Int]
  (let [num-op            (num-hids)
        total-size-so-far (+ num-op (total-empty-kids))
        need-more?        (< total-size-so-far tgt-size)]
    need-more?))

(s/defn build-rand-ast :- tsk/Vec ; bush result
  [ast-size]
  (verify (<= 3 ast-size)) ; 1 op & 2 args minimum;  #todo refine this
  (with-debug-hid
    (with-forest (new-forest)
      (let [root-hid (add-op-node (choose-rand-op))] ; root of AST
        ; Fill in random op nodes into the tree
        (while (need-more-op? ast-size)
          (let [node-hid (rand-elem (all-hids))]
            (when (node-has-empty-slot? node-hid)
              (kids-append node-hid
                [(add-op-node (choose-rand-op))]))))
        ; Fill in random arg nodes in empty leaf slots
        (doseq [node-hid (all-hids)]
          (while (node-has-empty-slot? node-hid)
            (add-leaf-node node-hid (choose-rand-arg))))
        (hid->bush root-hid)))))

(defn bush->form [it]
  (let [head (xfirst it)
        tag  (grab :tag head)]
    (if (= :op tag)
      (list (kw->sym (grab :op head))
        (bush->form (xsecond it))
        (bush->form (xthird it)))
      (kw->sym (grab :arg head)))))

(dotest
  (let [tgt-size 13]
    (dotimes [i 3]
      (let [ast (build-rand-ast tgt-size)]
        (when false ; set true to print demo
          (nl)
          (println (pretty-str ast))
          (println (pretty-str (bush->form ast))))))))

;-----------------------------------------------------------------------------
(dotest
  (let [data-enlive
        {:tag   :root
         :attrs nil
         :content
            [{:tag     :SoapObject, :attrs nil,
              :content
                 [{:tag     :ObjectData, :attrs nil,
                   :content [{:tag :FieldName, :attrs nil, :content ["ID"]}
                             {:tag :FieldValue, :attrs nil, :content ["8d8edbb6-cb0f-11e8-a8d5-f2801f1b9fd1"]}]}
                  {:tag     :ObjectData, :attrs nil,
                   :content [{:tag :FieldName, :attrs nil, :content ["Attribute_1"]}
                             {:tag :FieldValue, :attrs nil, :content ["Value_1a"]}]}
                  {:tag     :ObjectData, :attrs nil,
                   :content [{:tag :FieldName, :attrs nil, :content ["Attribute_2"]}
                             {:tag :FieldValue, :attrs nil, :content ["Value_2a"]}]}]}
             {:tag     :SoapObject, :attrs nil,
              :content
                 [{:tag     :ObjectData, :attrs nil,
                   :content [{:tag :FieldName, :attrs nil, :content ["ID"]}
                             {:tag :FieldValue, :attrs nil, :content ["90e39036-cb0f-11e8-a8d5-f2801f1b9fd1"]}]}
                  {:tag     :ObjectData, :attrs nil,
                   :content [{:tag :FieldName, :attrs nil, :content ["Attribute_1"]}
                             {:tag :FieldValue, :attrs nil, :content ["Value_1b"]}]}
                  {:tag     :ObjectData, :attrs nil,
                   :content [{:tag :FieldName, :attrs nil, :content ["Attribute_2"]}
                             {:tag :FieldValue, :attrs nil, :content ["Value_2b"]}]}]}]}]
    (with-debug-hid
      (with-forest (new-forest)
        (let [root-hid     (add-tree-enlive data-enlive)
              soapobj-hids (find-hids root-hid [:root :SoapObject])
              objdata->map (fn [objdata-hid]
                             (let [fieldname-node  (hid->node (find-hid objdata-hid [:ObjectData :FieldName]))
                                   fieldvalue-node (hid->node (find-hid objdata-hid [:ObjectData :FieldValue]))]
                               { (grab :value fieldname-node) (grab :value fieldvalue-node) }))
              soapobj->map (fn [soapobj-hid]
                             (apply glue
                               (for [objdata-hid (hid->kids soapobj-hid)]
                                 (objdata->map objdata-hid))))
              results      (mapv soapobj->map soapobj-hids)]
          (is= (hid->bush root-hid)
            [{:tag :root}
             [{:tag :SoapObject}
              [{:tag :ObjectData}
               [{:tag :FieldName, :value "ID"}]
               [{:tag :FieldValue, :value "8d8edbb6-cb0f-11e8-a8d5-f2801f1b9fd1"}]]
              [{:tag :ObjectData}
               [{:tag :FieldName, :value "Attribute_1"}]
               [{:tag :FieldValue, :value "Value_1a"}]]
              [{:tag :ObjectData}
               [{:tag :FieldName, :value "Attribute_2"}]
               [{:tag :FieldValue, :value "Value_2a"}]]]
             [{:tag :SoapObject}
              [{:tag :ObjectData}
               [{:tag :FieldName, :value "ID"}]
               [{:tag :FieldValue, :value "90e39036-cb0f-11e8-a8d5-f2801f1b9fd1"}]]
              [{:tag :ObjectData}
               [{:tag :FieldName, :value "Attribute_1"}]
               [{:tag :FieldValue, :value "Value_1b"}]]
              [{:tag :ObjectData}
               [{:tag :FieldName, :value "Attribute_2"}]
               [{:tag :FieldValue, :value "Value_2b"}]]]])
          (is= soapobj-hids [:0009 :0013])
          (is= results
            [{"ID"          "8d8edbb6-cb0f-11e8-a8d5-f2801f1b9fd1",
              "Attribute_1" "Value_1a",
              "Attribute_2" "Value_2a"}
             {"ID"          "90e39036-cb0f-11e8-a8d5-f2801f1b9fd1",
              "Attribute_1" "Value_1b",
              "Attribute_2" "Value_2b"}]))))))

(dotest
  (with-debug-hid
    (with-forest (new-forest)
      (let [root-hid                   (add-tree-hiccup
                                         [:div {:class :some-div-1}
                                          [:div {:class :some-div-2}
                                           [:label "Some Junk"]
                                           [:div {:class :some-div-3}
                                            [:label "Specify your shipping address"]
                                            [:div {:class :some-div-4}
                                             [:input {:type        "text" :autocomplete "off" :required "required"
                                                      :placeholder "" :class "el-input__inner"}]]]]])
            label-path                 (only (find-paths root-hid [:** {:tag :label :value "Specify your shipping address"}]))
            parent-div-hid             (-> label-path reverse second)
            shipping-address-input-hid (find-hid parent-div-hid [:div :div :input])
            ]
        (is= label-path [:0006 :0005 :0004 :0001])
        (is= parent-div-hid :0004)
        (is= (hid->hiccup shipping-address-input-hid)
          [:input {:type        "text", :autocomplete "off", :required "required",
                   :placeholder "", :class "el-input__inner"}])
        (value-set shipping-address-input-hid "1234 Main St")
        (is= (hid->hiccup shipping-address-input-hid)
          [:input {:type        "text", :autocomplete "off", :required "required",
                   :placeholder "", :class "el-input__inner"}
           "1234 Main St"])
        (is= (hid->hiccup root-hid)
          [:div
           {:class :some-div-1}
           [:div
            {:class :some-div-2}
            [:label "Some Junk"]
            [:div
             {:class :some-div-3}
             [:label "Specify your shipping address"]
             [:div
              {:class :some-div-4}
              [:input
               {:type         "text",
                :autocomplete "off",
                :required     "required",
                :placeholder  "",
                :class        "el-input__inner"}
               "1234 Main St"]]]]])))))

;---------------------------------------------------------------------------------------------------
; Find dependencies (children) in a tree. Given this data:
;[{:value "A"}
;  [{:value "B"}
;    [{:value "C"} {:value "D"}]
;  [{:value "E"} [{:value "F"}]]]]
;
;we want output like:
; {:A [:B :E]
;  :B [:C :D]
;  :C []
;  :D []
;  :E [:F]
;  :F}

(dotest
  (let [relationhip-data-hiccup [:A
                                 [:B
                                  [:C]
                                  [:D]]
                                 [:E
                                  [:F]]]
        expected-result         {:A [:B :E]
                                 :B [:C :D]
                                 :C []
                                 :D []
                                 :E [:F]
                                 :F []} ]
    (with-debug-hid
      (with-forest (new-forest)
        (let [root-hid (tf/add-tree-hiccup relationhip-data-hiccup)
              result   (apply glue (sorted-map)
                         (forv [hid (all-hids)]
                           (let [parent-tag (grab :tag (hid->node hid))
                                 kid-tags   (forv [kid-hid (hid->kids hid)]
                                              (let [kid-tag (grab :tag (hid->node kid-hid))]
                                                kid-tag))]
                             {parent-tag kid-tags})))]
          (is= (format-paths (find-paths root-hid [:A]))
            [[{:tag :A}
              [{:tag :B} [{:tag :C}] [{:tag :D}]]
              [{:tag :E} [{:tag :F}]]]])
          (is= result  expected-result ))))))















))

