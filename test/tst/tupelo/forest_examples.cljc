;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.forest-examples
  #?@(:clj [
  (:use tupelo.forest tupelo.test )
  (:require
    [clojure.data.xml :as cdx]
    [clojure.java.io :as io]
    [clojure.set :as cs]
    [clojure.string :as str]
    [net.cgrand.enlive-html :as enlive-html]
    [net.cgrand.jsoup :as enlive-jsoup]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.string :as ts]
    [tupelo.schema :as tsk]
    [tupelo.forest :as tf] )
  (:import [java.io StringReader])
    ]) )

(t/refer-tupelo :dev)

#?(:clj (do
(dotest
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
         (is= (format-paths (find-paths root-hid [:a :b]))
              [[{:tag :a} [{:tag :b, :value 1}]]
               [{:tag :a} [{:tag :b, :value 2}]]
               [{:tag :a} [{:tag :b}
                           [{:tag :c, :value 4}]
                           [{:tag :c, :value 5}]]]])
         ; Actual results: (find-paths aa [:a :b]) =>
         ;    [ [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :b40b6f37e6a746f815b092a8590cefe5cf37121a]
         ;      [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :76859beedd81468b4ee3cc5f17a5fdcf7a34a787]
         ;      [:c3b0dccd4d344ac765183f49940f4d685de7a3f5 :5c0cb1ba6657ba0ac40cc5099f2be091b5637a3b] ]
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
               [{:tag :a} [{:tag :c, :value 9}]]])))

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

       ;>> (println "clojure.data.xml/parse - BEFORE")
        v3 (clojure.data.xml/parse v2)
       ;>> (println "clojure.data.xml/parse - AFTER")
      ]
    v3))

;(dotest
;  (println "zappy - BEFORE")
;  (zappy)
;  (println "zappy - AFTER"))

; #todo #bug clojure.lang.Reflector
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
    (let [enlive-tree (->> "<p>sample <em>text</em> with words.</p>"
                        clojure.string/lower-case
                        java.io.StringReader.
                        enlive-html/html-resource
                        first)
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
          enlive-tree     (->> xml-str
                            java.io.StringReader.
                            enlive-html/html-resource
                            first)
          root-hid        (add-tree-enlive enlive-tree)
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
          enlive-tree     (->> xml-str
                            java.io.StringReader.
                            enlive-html/xml-resource
                            only)
          root-hid        (add-tree-enlive enlive-tree)
          has-bc-leaf?    (fn [hid] (or (has-child-leaf? hid [:** {:tag :Type :value "B"}])
                                      (has-child-leaf? hid [:** {:tag :Type :value "C"}])))
          blank-leaf-hids (keep-if whitespace-leaf-hid? (all-leaf-hids))
          >>              (apply remove-hid blank-leaf-hids)
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
    (let [enlive-tree          (->> xml-str-prod
                                 java.io.StringReader.
                                 enlive-html/xml-resource
                                 first)
          root-hid             (add-tree-enlive enlive-tree)
          tree-1               (hid->hiccup root-hid)

          blank-leaf-hids      (keep-if whitespace-leaf-hid? (all-hids))
          >>                   (apply remove-hid blank-leaf-hids)
          tree-2               (hid->hiccup root-hid)

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
    (let [xml-str         "<html>
                             <body>
                               <div class='one'>
                                 <div class='two'></div>
                               </div>
                             </body>
                           </html>"

          enlive-tree     (->> xml-str
                            java.io.StringReader.
                            enlive-html/xml-resource
                            only)
          root-hid        (add-tree-enlive enlive-tree)

          ; Removing whitespace nodes is optional; just done to keep things neat
          blank-leaf-hids (keep-if whitespace-leaf-hid? (all-leaf-hids)) ; find whitespace nodes
          >>              (apply remove-hid blank-leaf-hids) ; delete whitespace nodes found

          ; Can search for inner `div` 2 ways
          result-1        (find-paths root-hid [:html :body :div :div]) ; explicit path from root
          result-2        (find-paths root-hid [:** {:class "two"}]) ; wildcard path that ends in :class "two"
    ]
       (is= result-1 result-2) ; both searches return the same path
       (is= (hid->bush root-hid)
         [{:tag :html}
          [{:tag :body}
           [{:class "one", :tag :div}
            [{:class "two", :tag :div}]]]] )
      (is=
        (format-paths result-1)
        (format-paths result-2)
        [[{:tag :html}
          [{:tag :body}
           [{:class "one", :tag :div}
            [{:class "two", :tag :div}]]]]] )

       (is (= (hid->node (last (only result-1)))
             {::tf/khids [], :class "two", :tag :div}))
   )))

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

          enlive-tree     (->> html-str
                            java.io.StringReader.
                            enlive-html/html-resource
                            first)
          root-hid        (add-tree-enlive enlive-tree)
          tree-1          (hid->hiccup root-hid) ; orig tree with lots of whitespace leaves

          ; Removing whitespace nodes is optional; just done to keep things neat
          blank-leaf-hids (keep-if whitespace-leaf-hid? (all-leaf-hids)) ; find whitespace nodes
          >>              (apply remove-hid blank-leaf-hids) ; delete whitespace nodes found
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
  (with-forest (new-forest)
    (let [xml-str         "<top>
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
          enlive-tree     (->> xml-str
                            java.io.StringReader.
                            enlive-html/xml-resource
                            only)
          root-hid        (add-tree-enlive enlive-tree)

          ; Removing whitespace nodes is optional; just done to keep things neat
          blank-leaf-hids (keep-if whitespace-leaf-hid? (all-leaf-hids)) ; find whitespace nodes
          >>              (apply remove-hid blank-leaf-hids) ; delete whitespace nodes found

          ; Can search for inner `div` 2 ways
          result-1        (find-paths root-hid [:top :group :group]) ; explicit path from root
          result-2        (find-paths root-hid [:** :item :number]) ; wildcard path that ends in [:item :number]
          ]
      (is= 17 (count blank-leaf-hids))
      (is= (hid->bush root-hid)
        [{:tag :top}
         [{:tag :group}
          [{:tag :group}
           [{:tag :item} [{:tag :number, :value "1"}]]
           [{:tag :item} [{:tag :number, :value "2"}]]
           [{:tag :item} [{:tag :number, :value "3"}]]]
          [{:tag :item} [{:tag :number, :value "0"}]]]])

      ; Here we see only the double-nested items 1, 2, 3
      ; sample result-1 =>
      ;   [[:af35e171233589ed68703cc14b27f3bd5bce7a76 :3452b109d27f00b5a62bb7d4cca0deb9204e37f2
      ;     :332321b28a12e07548ac15aa92cb6f891c71a187]]
      (is= (format-paths result-1)
        [[{:tag :top}
          [{:tag :group}
           [{:tag :group}
            [{:tag :item} [{:tag :number, :value "1"}]]
            [{:tag :item} [{:tag :number, :value "2"}]]
            [{:tag :item} [{:tag :number, :value "3"}]]]]]])

      ; Here we see both the double-nested items & the single-nested item 0
      ; sample result-2 =>
      ;   [[:af35e171233589ed68703cc14b27f3bd5bce7a76   :3452b109d27f00b5a62bb7d4cca0deb9204e37f2
      ;     :332321b28a12e07548ac15aa92cb6f891c71a187   :a37b45f31cf4a31553fddbc9491c3344671a4f3d
      ;     :ece2b0ee3a9dfa0ec3d4c96a5fcab255f5ad6518]
      ;    [:af35e171233589ed68703cc14b27f3bd5bce7a76   :3452b109d27f00b5a62bb7d4cca0deb9204e37f2
      ;     :332321b28a12e07548ac15aa92cb6f891c71a187   :6d4d0c67048289250a69726295136879100c78d6
      ;     :78e278d5015e38e3db4e967dbef824d6bcecc058]
      ;    [:af35e171233589ed68703cc14b27f3bd5bce7a76   :3452b109d27f00b5a62bb7d4cca0deb9204e37f2
      ;     :332321b28a12e07548ac15aa92cb6f891c71a187   :ee5801b2373cf68f036b1368c287db964cbfb7ec
      ;     :b853a8ac12f1f70b54b49f68ecb6100d2718f1ba]
      ;    [:af35e171233589ed68703cc14b27f3bd5bce7a76   :3452b109d27f00b5a62bb7d4cca0deb9204e37f2
      ;     :ee514dc4be92cb02728335d7b628807a65915f7c   :240b7125bfb2d33a1a2ada5cd57c18d9ff88b207]]
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
             [{:tag :group} [{:tag :item} [{:tag :number, :value "3"}]]]]]]))

      )))

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
          value-nodes (mapv #(grab ::tf/value (hid->node %)) tail-hids)
          ]
      ;(spyx-pretty (hid->bush root-hid))
      ;(spyx-pretty (format-paths value-paths))

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
          enlive-tree     (->> xml-str
                            java.io.StringReader.
                            enlive-html/xml-resource
                            only)
          root-hid        (add-tree-enlive enlive-tree)
          bush-blanks     (hid->bush root-hid)
          blank-leaf-hids (keep-if whitespace-leaf-hid? (all-leaf-hids))
          >>              (apply remove-hid blank-leaf-hids)
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
        (is= sentences
          ["foo bar"
           "beyond all recognition"])))
    (let [handler          (fn [root-hid]
                             (remove-whitespace-leaves)
                             (tf/hid->bush root-hid))
          enlive-tree-lazy (clojure.data.xml/parse (StringReader. xml-str))
          result-word      (proc-tree-enlive-lazy enlive-tree-lazy [:document :sentence :word] handler)
          result-sentence  (proc-tree-enlive-lazy enlive-tree-lazy [:document :sentence] handler)
          result-document  (proc-tree-enlive-lazy enlive-tree-lazy [:document] handler)]
      (is= result-word
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
           [{:id "2.3", :tag :word, :value "recognition"}]]]])
      (is= result-sentence
        [[{:tag :document}
          [{:id "1", :tag :sentence}
           [{:id "1.1", :tag :word, :value "foo"}]
           [{:id "1.2", :tag :word, :value "bar"}]]]
         [{:tag :document}
          [{:id "2", :tag :sentence}
           [{:id "2.1", :tag :word, :value "beyond"}]
           [{:id "2.2", :tag :word, :value "all"}]
           [{:id "2.3", :tag :word, :value "recognition"}]]]])
      (is=
        (spyx-pretty result-document)
        [[{:tag :document}
          [{:id "1", :tag :sentence}
           [{:id "1.1", :tag :word, :value "foo"}]
           [{:id "1.2", :tag :word, :value "bar"}]]
          [{:id "2", :tag :sentence}
           [{:id "2.1", :tag :word, :value "beyond"}]
           [{:id "2.2", :tag :word, :value "all"}]
           [{:id "2.3", :tag :word, :value "recognition"}]]]]))
    (let [enlive-tree-lazy     (clojure.data.xml/parse (StringReader. xml-str))
          doc-sentence-handler (fn [root-hid]
                                 (remove-whitespace-leaves)
                                 (let [sentence-hid  (only (find-hids root-hid [:document :sentence]))
                                       word-hids     (hid->kids sentence-hid)
                                       words         (mapv #(grab :value (hid->leaf %)) word-hids)
                                       sentence-text (str/join \space words)]
                                   sentence-text))
          result-sentences     (proc-tree-enlive-lazy enlive-tree-lazy
                                 [:document :sentence] doc-sentence-handler)]
      (is= result-sentences ["foo bar" "beyond all recognition"])) ))

;---------------------------------------------------------------------------------------------------
(defn xkcd
  "Load a sample webpage from disk"
  []
  (-> "xkcd-sample.html"
    (io/resource)
    (io/input-stream)
    ;(clojure.data.xml/parse)
    (enlive-jsoup/parser)
  ))
(dotest
  (when false ; manually enable to grab a new copy of the webpage
    (spit "xkcd-sample.html"
      (slurp "https://xkcd.com")))
  (with-forest (new-forest)
    (let [doc         (it-> (xkcd)
                        (drop-if #(= :dtd (:type %)) it)
                        (only it))
          root-hid    (add-tree-enlive doc)
          >>          (remove-whitespace-leaves)
          ;>>          (spyx-pretty (hid->bush root-hid))
          hid-keep-fn (fn [hid]
                        (let [node       (hid->node hid)
                              value      (when (contains? node :value) (grab :value node))
                              perm-link? (when (string? value)
                                           (re-find #"Permanent link to this comic" value))]
                          perm-link?))
          found-hids  (find-hids-with root-hid [:** :*] hid-keep-fn)
          link-node   (hid->node (only found-hids)) ; assume there is only 1 link node
          value-str   (grab :value link-node) ; "\nPermanent link to this comic: https://xkcd.com/1988/"
          result      (re-find #"http.*$" value-str)]
     ;(spyx-pretty link-node)  ;=> {:tupelo.forest/khids [],
                                  ; :tag :tupelo.forest/raw,
                                  ; :value "\nPermanent link to this comic: https://xkcd.com/1988/"}
     ;(spyx result) ; => "https://xkcd.com/1988/"
    )))

))
