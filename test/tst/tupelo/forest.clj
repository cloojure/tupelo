;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.forest
  (:use tupelo.core tupelo.forest tupelo.test )
  (:require
    [tupelo.forest :as tf]
    [tupelo.string :as ts]
  ))

(verify
  (is= "0013" (format "%04x" 19))
  (is= "000a" (format "%04x" 10))
  (is= "ffff" (format "%04x" (dec (long (Math/pow 2 16)))))

  (hid-count-reset)
  (is= 1001 (new-hid))
  (is= 1002 (new-hid))
  (is= 1003 (new-hid))
  (is= 1004 (new-hid))
  (is= 1005 (new-hid))
  (is= 1006 (new-hid))
  (is= 1007 (new-hid))
  (is= 1008 (new-hid))
  (is= 1009 (new-hid))
  (is= 1010 (new-hid))
  (is= 1011 (new-hid)))

(verify
  (let [hiccup-data [:a
                     [:b 2]
                     [:c 3]]]
    (throws? (add-tree-hiccup hiccup-data)) ; should throw since no forest
    (try
      (add-tree-hiccup hiccup-data)
      (catch Exception ex
        (is (ts/contains-str? (.toString ex) "Possibly you forgot"))
        (is= {:nil-forest true} (ex-data ex)) ))

    (with-forest (new-forest) ; should work fine
      (let [root-hid (add-tree-hiccup hiccup-data)]
        (is= hiccup-data (hid->hiccup root-hid))))))

(verify
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
        tree-5 [:a {:k1 "v1"}
                [:b 2]
                [:c 3 4 5]]
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

    (is= (hiccup->enlive tree-5)
      {:tag   :a,
       :attrs {:k1 "v1"},
       :content
              [{:tag :b, :attrs {}, :content [2]}
               {:tag :c, :attrs {}, :content [3 4 5]}]} )

    ; #todo add generative testing
    (is= tree-2 (-> tree-2 hiccup->enlive enlive->hiccup))
    (is=
      (-> tree-2 hiccup->enlive)
      (-> tree-2 hiccup->enlive enlive->hiccup hiccup->enlive))

    (is= tree-3 (-> tree-3 hiccup->enlive enlive->hiccup))
    (is=
      (-> tree-3 hiccup->enlive)
      (-> tree-3 hiccup->enlive enlive->hiccup hiccup->enlive))

    (is= tree-4 (-> tree-4 hiccup->enlive enlive->hiccup))
    (is=
      (-> tree-4 hiccup->enlive)
      (-> tree-4 hiccup->enlive enlive->hiccup hiccup->enlive))
    ;-----------------------------------------------------------------------------
    (is= tree-2 (-> tree-2 hiccup->tree tree->hiccup))
    (is=
      (-> tree-2 hiccup->tree)
      (-> tree-2 hiccup->tree tree->hiccup hiccup->tree))

    (is= tree-3 (-> tree-3 hiccup->tree tree->hiccup))
    (is=
      (-> tree-3 hiccup->tree)
      (-> tree-3 hiccup->tree tree->hiccup hiccup->tree))

    (is= tree-4 (-> tree-4 hiccup->tree tree->hiccup))
    (is=
      (-> tree-4 hiccup->tree)
      (-> tree-4 hiccup->tree tree->hiccup hiccup->tree))
    ;-----------------------------------------------------------------------------
    (is= tree-2 (-> tree-2 hiccup->bush bush->hiccup))
    (is=
      (-> tree-2 hiccup->bush)
      (-> tree-2 hiccup->bush bush->hiccup hiccup->bush))

    (is= tree-3 (-> tree-3 hiccup->bush bush->hiccup))
    (is=
      (-> tree-3 hiccup->bush)
      (-> tree-3 hiccup->bush bush->hiccup hiccup->bush))

    (is= tree-4 (-> tree-4 hiccup->bush bush->hiccup))
    (is=
      (-> tree-4 hiccup->bush)
      (-> tree-4 hiccup->bush bush->hiccup hiccup->bush))
    ))


(verify
  (let [tree-2 {:tag   :a,
                :attrs {},
                :content
                       [{:tag :b, :attrs {}, :content [2]}
                        {:tag :c, :attrs {}, :content [3]}]}

        tree-3 {:tag   :a,
                :attrs {:k1 "v1"},
                :content
                       [{:tag :b, :attrs {}, :content [2]}
                        {:tag :c, :attrs {}, :content [3]}]}
        tree-4 {:tag   :a,
                :attrs {:k1 "v1"},
                :content
                       [{:tag :b, :attrs {}, :content [2]}
                        {:tag :c, :attrs {:k3 "v3" :k4 4},
                         :content [ {:tag :d :attrs {} :content [4]}]}]}
        tree-5 {:tag   :a,
                :attrs {:k1 "v1"},
                :content
                       [{:tag :b, :attrs {}, :content [2]}
                        {:tag :c, :attrs {}, :content [3 4 5]}]}
  ]
     (is= (enlive->tree tree-2)
       {:tag :a,
        ::tf/kids
             [{:tag :b, :value 2, ::tf/kids []}
              {:tag :c, :value 3, ::tf/kids []}]})
    (is= (enlive->tree tree-3)
      {:k1  "v1",
       :tag :a,
       ::tf/kids
            [{:tag :b, :value 2, ::tf/kids []}
             {:tag :c, :value 3, ::tf/kids []}]})
    (is= (enlive->tree tree-4)
      {:k1  "v1",
       :tag :a,
       ::tf/kids
            [{:tag :b, :value 2, ::tf/kids []}
             {:k3  "v3",
              :k4  4,
              :tag :c,
              ::tf/kids
                   [{:tag :d, :value 4, ::tf/kids []}]}]})
    (is= (enlive->tree tree-5)
      {:k1  "v1",
       :tag :a,
       ::tf/kids
            [{:tag :b, :value 2, ::tf/kids []}
             {:tag :c,
              ::tf/kids
                   [{:tag ::tf/raw, :value 3 ::tf/kids []}
                    {:tag ::tf/raw, :value 4 ::tf/kids []}
                    {:tag ::tf/raw, :value 5 ::tf/kids []}]}]})

    ; #todo add generative testing
    (is= tree-2 (-> tree-2 enlive->tree tree->enlive))
    (is=
      (-> tree-2 enlive->tree)
      (-> tree-2 enlive->tree tree->enlive enlive->tree))

    (is= tree-3 (-> tree-3 enlive->tree tree->enlive))
    (is=
      (-> tree-3 enlive->tree)
      (-> tree-3 enlive->tree tree->enlive enlive->tree))

    (is= tree-4 (-> tree-4 enlive->tree tree->enlive))
    (is=
      (-> tree-4 enlive->tree)
      (-> tree-4 enlive->tree tree->enlive enlive->tree))
    (is= tree-5 (-> tree-5 enlive->tree tree->enlive))
    (is=
      (-> tree-5 enlive->tree)
      (-> tree-5 enlive->tree tree->enlive enlive->tree))


    (is= tree-2 (-> tree-2 enlive->bush bush->enlive))
    (is=
      (-> tree-2 enlive->bush)
      (-> tree-2 enlive->bush bush->enlive enlive->bush))
    (is= tree-3 (-> tree-3 enlive->bush bush->enlive))
    (is=
      (-> tree-3 enlive->bush)
      (-> tree-3 enlive->bush bush->enlive enlive->bush))
    (is= tree-4 (-> tree-4 enlive->bush bush->enlive))
    (is=
      (-> tree-4 enlive->bush)
      (-> tree-4 enlive->bush bush->enlive enlive->bush))
    (is= tree-5 (-> tree-5 enlive->bush bush->enlive))
    (is=
      (-> tree-5 enlive->bush)
      (-> tree-5 enlive->bush bush->enlive enlive->bush))
))


(verify
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
    (is= (hiccup->tree tree-2)
      {:tag :a,
       ::tf/kids
            [{:tag :b, :value 2 ::tf/kids []}
             {:tag :c, :value 3 ::tf/kids []}]} )

    (is= (hiccup->tree tree-3)
      {:k1  "v1",
       :tag :a,
       ::tf/kids
            [{:tag :b, :value 2 ::tf/kids []}
             {:tag :c, :value 3 ::tf/kids []}]})

    (is= (hiccup->tree tree-4)
      {:k1  "v1",
       :tag :a,
       ::tf/kids
            [{:tag :b, :value 2 ::tf/kids []}
             {:k3 "v3", :k4 4, :tag  :c,
              ::tf/kids [{:tag :d, :value 4  ::tf/kids []}]}]} )


    ; #todo add generative testing
    (is= tree-2 (-> tree-2 hiccup->tree tree->hiccup))
    (is=
      (-> tree-2 hiccup->tree)
      (-> tree-2 hiccup->tree tree->hiccup hiccup->tree))

    (is= tree-3 (-> tree-3 hiccup->tree tree->hiccup))
    (is=
      (-> tree-3 hiccup->tree)
      (-> tree-3 hiccup->tree tree->hiccup hiccup->tree))

    (is= tree-4 (-> tree-4 hiccup->tree tree->hiccup))
    (is=
      (-> tree-4 hiccup->tree)
      (-> tree-4 hiccup->tree tree->hiccup hiccup->tree))))

(verify
  (with-forest (new-forest)
    (let [x-hid  (add-node {:tag :char :color :red :value "x"})
          y-hid  (add-node {:tag :char :color :red :value "y"})
          z-hid  (add-node {:tag :char :color :red :value "z"})
          r-hid  (add-node {:tag :root :color :white} [x-hid y-hid z-hid])

          x-tree (hid->tree x-hid)
          y-tree (hid->tree y-hid)
          z-tree (hid->tree z-hid)
          r-tree (hid->tree r-hid)

          x-node (hid->node x-hid)
          y-node (hid->node y-hid)
          z-node (hid->node z-hid)
          r-node (hid->node r-hid)

          roots  (root-hids)]
      (is (and (forest-hid? x-hid) (forest-hid? y-hid) (forest-hid? z-hid) (forest-hid? r-hid)))

      (is (and (leaf-hid? x-hid) (leaf-hid? y-hid) (leaf-hid? z-hid)))
      (is (and (valid-forest-leaf? x-node) (valid-forest-leaf? y-node) (valid-forest-leaf? z-node)))

      (is (forest-hid? r-hid))
      (is (tree-node? r-tree))
      (is (valid-forest-node? r-node))

      (is= #{r-hid} roots)
      (isnt= #{x-hid} roots)

      (is= x-tree {::tf/kids  [],
                   :tag       :char,
                   :color     :red,
                   :value "x"})
      (is= r-tree
        {:tag      :root,
         :color    :white,
         ::tf/kids [{::tf/kids  [],
                     :tag       :char,
                     :color     :red,
                     :value "x"}
                    {::tf/kids  [],
                     :tag       :char,
                     :color     :red,
                     :value "y"}
                    {::tf/kids  [],
                     :tag       :char,
                     :color     :red,
                     :value "z"}]})
      (is (wild-match?
            {:tag       :root, :color :white,
             ::tf/khids [:* :* :*]}
            (hid->node r-hid)))
      (is (wild-match?
            {:tag       :root
             :color     :*
             ::tf/khids [:* :* :*]}
            (hid->node r-hid)))
      (isnt (wild-match?
              {:tag       :root
               :color     :*
               ::tf/khids [:* :*]}
              (hid->node r-hid)))
      (is (wild-match?
            {:tag       :root
             :color     :*
             ::tf/khids :*}
            (hid->node r-hid)))

      (attrs-merge x-hid {:color :green})
      (is (val=  (hid->node x-hid)
            { ::tf/khids [] :tag :char, :color :green, :value "x"} ))

      (is= (hid->attrs r-hid) {:tag :root, :color :white})
      (is= (hid->attrs z-hid) {:tag :char, :color :red :value "z"})
      (is= (hid->kids r-hid) [x-hid y-hid z-hid])
      (is= (grab :value (hid->node z-hid)) "z")

      (attrs-set z-hid {:type :tuna, :name :charlie})
      (is= (hid->attrs z-hid) {:type :tuna, :name :charlie})
    )))


(verify
  (with-forest (new-forest)
    (let [x (add-node {:tag :char :color :red :cnt 0 :value "x"})
          r (add-node {:tag :root :color :white :cnt 0} [x])
          x-tree (hid->tree x)
          r-tree (hid->tree r)]
       (is= r-tree
         {:tag   :root,
          :color :white,
          :cnt   0,
          ::tf/kids
                 [{::tf/kids  [],
                   :tag       :char,
                   :color     :red,
                   :cnt       0,
                   :value "x"}]})

      (attr-update x :cnt inc)
      (attr-update x :cnt inc)
      (attr-update r :cnt #(+ 3 %))
      (is= (hid->tree r)
        {:tag   :root,
         :color :white,
         :cnt   3,
         ::tf/kids
                [{::tf/kids  [],
                  :tag       :char,
                  :color     :red,
                  :cnt       2,
                  :value "x"}]})

      (attr-update r :cnt * 3)
      (attr-update r :cnt + 7)
      (is= (hid->tree r)
        {:tag   :root,
         :color :white,
         :cnt   16,
         ::tf/kids
                [{::tf/kids  [],
                  :tag       :char,
                  :color     :red,
                  :cnt       2,
                  :value "x"}]}))))

(verify
  (let [state    (atom {})
        forest-1 (with-forest-result (new-forest)
                   (let [x (add-node {:tag :char :color :red :value "x"})
                         y (add-node {:tag :char :color :red :value "y"})
                         z (add-node {:tag :char :color :red :value "z"})
                         r (add-node {:tag :root :color :white} [x y z])]
                     (reset! state (vals->map x y z r))
                     (is= (hid->kids r) [x y z])
                     (is= (grab :value (hid->node z)) "z")

                     (attrs-set z {:type :tuna, :name :charlie})
                     (is= (hid->attrs z) {:type :tuna, :name :charlie})

                     (is (val= (hid->node y)
                           {::tf/khids [], :tag :char, :color :red, :value "y"}))
                     (is (val= (attr-remove y :color)
                           {::tf/khids [], :tag :char, :value "y"}))))

        forest-2 (with-forest-result forest-1
                   (let [{:keys [x y z r]} @state]
                     (is (val= (hid->node y) {::tf/khids [], :tag :char, :value "y"}))
                     (is (val= (attr-set y :value "YYY")
                           {::tf/khids [], :tag :char, :value "YYY"}))

                     (is (val= (attr-set y :value 0)
                           {::tf/khids [], :tag :char, :value 0}))
                     (attr-update y :value + 7)
                     (attr-update y :value * 6)
                     (is (val= (hid->node y)
                           {::tf/khids [], :tag :char, :value 42}))))

        ; forest-1 is unaffected by changes that created forest-2
        forest-3 (with-forest-result forest-1
                   (let [{:keys [x y z r]} @state]
                     (is (val= (hid->node y)
                           {::tf/khids [], :tag :char, :value "y"})))) ; still has forest-1 value

        forest-4 (with-forest-result forest-2
                   (let [{:keys [x y z r]} @state
                         >> (is (val= (hid->node y)
                                  {::tf/khids [], :tag :char, :value 42})) ; still has forest-2 value
                         a  (add-node {:name :michael :value "do"})
                         b  (add-node {:name :tito :value "re"})
                         c  (add-node {:name :germain :value "mi"})]
                        (kids-set r [a b c])
                     (is= (hid->tree r)
                       {:tag   :root,
                        :color :white,
                        ::tf/kids
                               [{::tf/kids [], :name :michael, :value "do"}
                                {::tf/kids [], :name :tito, :value "re"}
                                {::tf/kids [], :name :germain, :value "mi"}]}
                       )
                     (kids-update r
                       (fn sort-kids [kids]
                         (sort-by #(grab :name (hid->attrs %)) kids)))
                     (is= (hid->tree r)
                       {:tag   :root,
                        :color :white,
                        ::tf/kids
                               [{::tf/kids [], :name :germain, :value "mi"}
                                {::tf/kids [], :name :michael, :value "do"}
                                {::tf/kids [], :name :tito, :value "re"} ]} )
                     (kids-update r
                       (fn sort-kids [kids]
                         (sort-by #(grab :value (hid->node %)) kids)))
                     (is= (hid->tree r)
                       {:tag   :root,
                        :color :white,
                        ::tf/kids
                               [{::tf/kids [], :name :michael, :value "do"}
                                {::tf/kids [], :name :germain, :value "mi"}
                                {::tf/kids [], :name :tito, :value "re"} ]} ) ))
  ])

  (with-forest (new-forest)
    (let [x (add-node {:tag :char :color :red :value "x"})
          y (add-node {:tag :char :color :green :value "y"})
          z (add-node {:tag :char :color :blue :value "z"})
          r (add-node {:tag :root :color :white})]
      (is= (hid->kids r) [])
      (kids-append r [x]) (is= (hid->kids r) [x])
      (kids-append r [y]) (is= (hid->kids r) [x y])
      (kids-append r [z]) (is= (hid->kids r) [x y z])
      (is= (hid->tree r)
        {:tag :root, :color :white,
         ::tf/kids
              [{::tf/kids [], :tag :char, :color :red, :value "x"}
               {::tf/kids [], :tag :char, :color :green, :value "y"}
               {::tf/kids [], :tag :char, :color :blue, :value "z"}]})

      (remove-kids r #{z x})
      (is= (hid->kids r) [y])
      (throws? (remove-kids r #{y x}))
      (remove-kids r #{y})
      (is= (hid->kids r) [])
      (is= (hid->tree r)
        {:tupelo.forest/kids [], :tag :root, :color :white} )
      (kids-append  r [z  ]) (is= (hid->kids r) [z])
      (kids-prepend r [x y]) (is= (hid->kids r) [x y z])))

  ; #todo fix up re:  remove-node-from-parents
  (with-forest (new-forest)
    (let [x (add-node {:tag :char :color :red :value "x"})
          y (add-node {:tag :char :color :green :value "y"})
          z (add-node {:tag :char :color :blue :value "z"})

          a (add-node {:tag :r1 :color :white} [x y z]) ]
      (is= (hid->kids a) [x y z])
      (is= (hid->tree a)
        {:tag :r1, :color :white,
         ::tf/kids
              [{::tf/kids [], :tag :char, :color :red, :value "x"}
               {::tf/kids [], :tag :char, :color :green, :value "y"}
               {::tf/kids [], :tag :char, :color :blue, :value "z"}]} )
      (remove-node-from-parents [a] y)
      (remove-node-from-parents [a] z)
      (is= (hid->kids a) [x])
      (is= (hid->tree a)
        {:tag :r1, :color :white,
         ::tf/kids
              [{::tf/kids [], :tag :char, :color :red, :value "x"}]} )
      (throws? (remove-node-from-parents [x] y))

      (remove-node-from-parents [a] x)
      (is= (hid->kids a) [])
      (is= (hid->tree a)
        {::tf/kids [], :tag :r1, :color :white}))) )

(verify
  (with-forest (new-forest)
    (let [x (add-node {:a 1 :b 2 :value "x"})]
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

(verify
  (with-forest (new-forest)
    (let [hiccup-1 [:a
                  [:b 1]
                  [:b 2]
                  [:b
                   [:c 4]
                   [:c 5]]
                  [:c 9]]
          root-1 (add-tree-hiccup hiccup-1)

          b1     (add-node {:tag :b :value 1})
          b2     (add-node {:tag :b :value 2})
          c4     (add-node {:tag :c :value 4})
          c5     (add-node {:tag :c :value 5})
          c9     (add-node {:tag :c :value 9})
          b3     (add-node {:tag :b } [c4 c5])
          aa     (add-node {:tag :a } [b1 b2 b3 c9])

          root-2 (add-node {:tag :a}
                   [(add-node {:tag :b :value 1})
                    (add-node {:tag :b :value 2})
                    (add-node {:tag :b}
                      [(add-node {:tag :c :value 4})
                       (add-node {:tag :c :value 5})])
                    (add-node {:tag :c :value 9})])
    ]
      (is= (hid->tree b1) {::tf/kids [], :tag :b, :value 1} )
      (is= (hid->tree b2) {::tf/kids [], :tag :b, :value 2} )
      (is= (hid->tree c4) {::tf/kids [], :tag :c, :value 4} )
      (is= (hid->tree c5) {::tf/kids [], :tag :c, :value 5} )
      (is= (hid->tree c9) {::tf/kids [], :tag :c, :value 9})
      (is= (hid->tree b3) {:tag :b,
                           ::tf/kids
                                [{::tf/kids [], :tag :c, :value 4}
                                 {::tf/kids [], :tag :c, :value 5}]} )
      (is= (hid->tree aa) {:tag :a,
                           ::tf/kids
                                [{::tf/kids [], :tag :b, :value 1}
                                 {::tf/kids [], :tag :b, :value 2}
                                 {:tag :b,
                                  ::tf/kids
                                       [{::tf/kids [], :tag :c, :value 4}
                                        {::tf/kids [], :tag :c, :value 5}]}
                                 {::tf/kids [], :tag :c, :value 9}]} )

      (is (validate-hid root-1))
      (is=
        (hid->tree aa)
        (hid->tree root-1)
        (hid->tree root-2)
        {:tag :a,
         ::tf/kids
              [{::tf/kids [], :tag :b, :value 1}
               {::tf/kids [], :tag :b, :value 2}
               {:tag :b,
                ::tf/kids
                     [{::tf/kids [], :tag :c, :value 4}
                      {::tf/kids [], :tag :c, :value 5}]}
               {::tf/kids [], :tag :c, :value 9}]} )
    )))

(verify
  (with-forest (new-forest)
    (let [enlive-tree {:tag   :a,
                       :attrs {},
                       :content
                              [{:tag :b, :attrs {}, :content [1]}
                               {:tag :b, :attrs {}, :content [2]}
                               {:tag   :b,
                                :attrs {},
                                :content
                                       ["First-String"
                                        {:tag :c, :attrs {}, :content [4]}
                                        "Second-String"
                                        {:tag :c, :attrs {}, :content [5]}
                                        "Third-String"]}
                               {:tag :c, :attrs {}, :content [9]}]}
          root-hid    (add-tree-enlive enlive-tree)]
         (is= (hid->bush root-hid)
           [{:tag :a}
            [{:tag :b, :value 1}]
            [{:tag :b, :value 2}]
            [{:tag :b}
             [{:tag ::tf/raw, :value "First-String"}]
             [{:tag :c, :value 4}]
             [{:tag ::tf/raw, :value "Second-String"}]
             [{:tag :c, :value 5}]
             [{:tag ::tf/raw, :value "Third-String"}]]
            [{:tag :c, :value 9}]]))))

(verify
  (with-forest (new-forest)
    (let [hiccup-tree [:a
                       [:b 1]
                       [:b 2]
                       [:b
                        "First-String"
                        [:c 4]
                        "Second-String"
                        [:c 5]
                        "Third-String"]
                       [:c 9]]
          root-hid    (add-tree-hiccup hiccup-tree)]
         (is= (hid->bush root-hid)
           [{:tag :a}
            [{:tag :b, :value 1}]
            [{:tag :b, :value 2}]
            [{:tag :b}
             [{:tag ::tf/raw, :value "First-String"}]
             [{:tag :c, :value 4}]
             [{:tag ::tf/raw, :value "Second-String"}]
             [{:tag :c, :value 5}]
             [{:tag ::tf/raw, :value "Third-String"}]]
            [{:tag :c, :value 9}]]
         ))))

(verify
  (hid-count-reset)
  (with-forest (new-forest)
    (let [root-hid (add-tree-hiccup [:a
                                     [:b 1]
                                     [:b 2]
                                     [:b
                                      [:c 4]
                                      [:c 5]]
                                     [:c 9]])]

      (is (empty? (find-paths root-hid [:z])))
      (is (empty? (find-paths root-hid [:z :b])))
      (is (empty? (find-paths root-hid [:z :b :c])))
      (is (empty? (find-paths root-hid [:a :z])))
      (is (empty? (find-paths root-hid [:a :z :c])))
      (is (empty? (find-paths root-hid [:a :b :z])))

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
      (is (wild-match? [[:* :*]
                        [:* :*]
                        [:* :*]]
            (find-paths root-hid [:a :b])))
      (is= (find-paths root-hid [:a :b])
        [[1007 1001] ; (with-debug-hid ...) uses only 4-digit hex HID
         [1007 1002]
         [1007 1005]])
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
           [{:tag :b}
            [{:tag :c :value 4}]
            [{:tag :c :value 5}]]]
          [{:tag :a}
           [{:tag :b :value 2}]]
          [{:tag :a}
           [{:tag :b :value 1}]]})
      (is= (format-paths (find-paths root-hid [:a :*]))
        [[{:tag :a}
          [{:tag :b :value 1}]]
         [{:tag :a}
          [{:tag :b :value 2}]]
         [{:tag :a}
          [{:tag :b}
           [{:tag :c :value 4}]
           [{:tag :c :value 5}]]]
         [{:tag :a}
          [{:tag :c :value 9}]]])
      (is= (format-paths (find-paths root-hid [:a :* :c]))
        [[{:tag :a} [{:tag :b} [{:tag :c :value 4}]]]
         [{:tag :a} [{:tag :b} [{:tag :c :value 5}]]]])
      )))

(verify
  (with-forest (new-forest)
    (let [aa (add-node :a
               [(add-node {:b :b1 :value 1})
                (add-node {:b :b2 :value 2})
                (add-node {:b :b3}
                  [(add-node {:c :c4 :value 4})
                   (add-node {:c :c5 :value 5})])
                (add-node {:c :c9 :value 9})])]
      (is= (set (format-paths (find-paths aa [:a :** :*])))
        #{[{:tag :a} [{:b :b1 :value 1}]]
          [{:tag :a} [{:b :b2 :value 2}]]
          [{:tag :a} [{:c :c9 :value 9}]]
          [{:tag :a} [{:b :b3}
                      [{:c :c4 :value 4}]
                      [{:c :c5 :value 5}]]]
          [{:tag :a} [{:b :b3} [{:c :c4 :value 4}]]]
          [{:tag :a} [{:b :b3} [{:c :c5 :value 5}]]]})

      (is= (set (format-paths (find-paths aa [:** {:c :*}])))
        #{[{:tag :a} [{:b :b3} [{:c :c5 :value 5}]]]
          [{:tag :a} [{:b :b3} [{:c :c4 :value 4}]]]
          [{:tag :a} [{:c :c9 :value 9}]]})

      (is= (set (format-paths (find-paths aa [:a :** {:c :*}])))
        #{[{:tag :a} [{:b :b3} [{:c :c5 :value 5}]]]
          [{:tag :a} [{:b :b3} [{:c :c4 :value 4}]]]
          [{:tag :a} [{:c :c9 :value 9}]]})))

  (with-forest (new-forest)
    (let [root-hid (add-node {:tag :a}
                     [(add-node {:tag :b :value 2})
                      (add-node {:tag :c :value 3})])]
      (throws? (find-paths root-hid [:**]))
      (throws? (find-paths root-hid [:a :**]))
      (is=
        (format-paths (find-paths root-hid [:a :** :c]))
        (format-paths (find-paths root-hid [:a :** :** :c]))
        (format-paths (find-paths root-hid [:** :c]))
        [[{:tag :a} [{:tag :c :value 3}]]])
      (is= (format-paths (find-paths root-hid [:** :*]))
        [[{:tag :a} [{:tag :b :value 2}] [{:tag :c :value 3}]]
         [{:tag :a} [{:tag :b :value 2}]]
         [{:tag :a} [{:tag :c :value 3}]]])
      (is= (hid->tree root-hid)
        {:tag :a,
         ::tf/kids
              [{::tf/kids [], :tag :b, :value 2}
               {::tf/kids [], :tag :c, :value 3}]})

      (is= (format-paths (find-paths root-hid [:a {:tag :b :value 2}]))
        [[{:tag :a} [{:tag :b :value 2}]]])

      (is= (format-paths (find-paths root-hid [:a :** {:tag :b :value 2}]))
        [[{:tag :a} [{:tag :b :value 2}]]])

      (is= (format-paths (find-paths root-hid [:a :** :** {:tag :b :value 2}]))
        [[{:tag :a} [{:tag :b :value 2}]]])

      (is= (format-paths (find-paths root-hid [:** {:tag :b :value 2}]))
        [[{:tag :a} [{:tag :b :value 2}]]])

      (is= (format-paths (find-paths root-hid [:a {:tag :c :value 3}]))
        [[{:tag :a} [{:tag :c :value 3}]]])

      (is= (format-paths (find-paths root-hid [:* {:tag :c :value 3}]))
        [[{:tag :a} [{:tag :c :value 3}]]])

      (is= (format-paths (find-paths root-hid [:a {:value 3}]))
        [[{:tag :a} [{:tag :c :value 3}]]])

      (is= (format-paths (find-paths root-hid [:** {:value 3}]))
        [[{:tag :a} [{:tag :c :value 3}]]])

      (is= (format-paths (find-paths root-hid [:a :** {:value 3 :tag :c}]))
        [[{:tag :a} [{:tag :c :value 3}]]])

      (is= (format-paths (find-paths root-hid [:a :** :** {:tag :c :value 3}]))
        [[{:tag :a} [{:tag :c :value 3}]]])

      (is= (format-paths (keep-if leaf-path? (find-paths root-hid [:** :*])))
        [[{:tag :a} [{:tag :b :value 2}]]
         [{:tag :a} [{:tag :c :value 3}]]])

      (throws? (find-paths root-hid [:**]))
      (throws? (find-paths root-hid [:a :**]))))

  (with-forest (new-forest)
    (throws? (add-node {:a :a1}
               [(add-node {:* :b2 :value 2})
                (add-node {:c :c3 :value 3})]))
    (throws? (add-node {:a :a1}
               [(add-node {:b :* :value 2})
                (add-node {:c :c3 :value 3})]))
    (throws? (add-node {:a :a1}
               [(add-node {:** :b2 :value 2})
                (add-node {:c :c3 :value 3})]))
    (throws? (add-node {:a :a1}
               [(add-node {:b :** :value 2})
                (add-node {:c :c3 :value 3})]))))

(verify
  (with-forest (new-forest)
    (let [x (add-node {:a :a1}
              [(add-node {:b :b1 :color :red :value 2})
               (add-node {:b :b2 :color :red :value 3})])
          y (add-node {:a :a2}
              [(add-node {:b :b1 :color :green :value 2})
               (add-node {:b :b2 :color :green :value 3})])
          z (add-node {:a :a3}
              [(add-node {:c :b1 :color :blue :value 2})
               (add-node {:c :b2 :color :blue :value 3})])]
      (is= (set (mapv hid->tree (root-hids)))
        #{{:a :a1,
           ::tf/kids
              [{::tf/kids [], :b :b1, :color :red, :value 2}
               {::tf/kids [], :b :b2, :color :red, :value 3}]}
          {:a :a3,
           ::tf/kids
              [{::tf/kids [], :c :b1, :color :blue, :value 2}
               {::tf/kids [], :c :b2, :color :blue, :value 3}]}
          {:a :a2,
           ::tf/kids
              [{::tf/kids [], :b :b1, :color :green, :value 2}
               {::tf/kids [], :b :b2, :color :green, :value 3}]}} )
      (is= (set (format-paths (find-paths (root-hids) [{:a :*}])))
        #{[{:a :a1}
           [{:b :b1, :color :red, :value 2}]
           [{:b :b2, :color :red, :value 3}]]
          [{:a :a2}
           [{:b :b1, :color :green, :value 2}]
           [{:b :b2, :color :green, :value 3}]]
          [{:a :a3}
           [{:c :b1, :color :blue, :value 2}]
           [{:c :b2, :color :blue, :value 3}]] } )
      (is= (set (format-paths (find-paths (root-hids) [{:a :*} {:b :*}])))
        #{[{:a :a1} [{:b :b1, :color :red, :value 2}]]
          [{:a :a1} [{:b :b2, :color :red, :value 3}]]
          [{:a :a2} [{:b :b1, :color :green, :value 2}]]
          [{:a :a2} [{:b :b2, :color :green, :value 3}]] } )
      (is= (set (format-paths (find-paths (root-hids) [{:a :*} {:b :* :value 3}] )))
        #{[{:a :a1} [{:b :b2, :color :red, :value 3}]]
          [{:a :a2} [{:b :b2, :color :green, :value 3}]]} )
      (is= (format-paths (find-paths (root-hids) [{:a :*} {:c :* :value 3}]))
        [[{:a :a3} [{:c :b2, :color :blue, :value 3}]]] )
      (is= (set (format-paths (find-paths (root-hids) [:** {:value 3}])))
        #{[{:a :a1} [{:b :b2, :color :red, :value 3}]]
          [{:a :a2} [{:b :b2, :color :green, :value 3}]]
          [{:a :a3} [{:c :b2, :color :blue, :value 3}]]} )
    )))

; #todo need to test find-paths using attrs
(verify
  (with-forest (new-forest)
    (let [aa (add-node {:color :red}
               [(add-node {:color :green :value 2})
                (add-node {:color :blue :value 3})])]
      (is= (format-paths (find-paths aa [{:color :red}]))
        [[{:color :red}
          [{:color :green, :value 2}]
          [{:color :blue, :value 3}]]] )
      (is= (format-paths (find-paths aa [{:color :red} {:color :green}]))
        [[{:color :red}
          [{:color :green, :value 2}]]] )
      (is= (format-paths (find-paths aa [:** {:color :green}]))
        [[{:color :red}
          [{:color :green, :value 2}]]] ))))

(verify
  (hid-count-reset)
  (with-forest (new-forest)
    (let [x (add-node {:tag :a :id :a1}
              [(add-node {:tag :b :color :red :value 2})
               (add-node {:tag :b :color :red :value 3})])
          y (add-node {:tag :a :id :a2}
              [(add-node {:tag :b :color :green :value 2})
               (add-node {:tag :b :color :green :value 3})])
          z (add-node {:tag :a :id :a3}
              [(add-node {:tag :c :color :blue :value 2})
               (add-node {:tag :c :color :blue :value 3})])]

      (is= (set (format-paths (find-paths (root-hids) [{:tag :a}])))
        #{[{:tag :a, :id :a1}
           [{:tag :b, :color :red, :value 2}]
           [{:tag :b, :color :red, :value 3}]]
          [{:tag :a, :id :a2}
           [{:tag :b, :color :green, :value 2}]
           [{:tag :b, :color :green, :value 3}]]
          [{:tag :a, :id :a3}
           [{:tag :c, :color :blue, :value 2}]
           [{:tag :c, :color :blue, :value 3}]]})

      (is= (format-paths (find-paths (root-hids) [{:id :a2}]))
        [[{:tag :a, :id :a2}
          [{:tag :b, :color :green, :value 2}]
          [{:tag :b, :color :green, :value 3}]]])

      (is= (format-paths (find-paths (root-hids) [:** {:color :green}]))
        [[{:tag :a, :id :a2}
          [{:tag :b, :color :green, :value 2}]]
         [{:tag :a, :id :a2}
          [{:tag :b, :color :green, :value 3}]]])

      (is= (find-paths (root-hids) [:** {:color :green}])
        [[1006 1004]
         [1006 1005]])

      (is= (set (format-paths (find-paths (root-hids) [:** {:tag :b :value 2}])))
        #{[{:tag :a, :id :a1}
           [{:tag :b, :color :red, :value 2}]]
          [{:tag :a, :id :a2}
           [{:tag :b, :color :green, :value 2}]]})

      (is= (format-paths (find-paths x [:** {:tag :b :value 2}]))
        [[{:tag :a, :id :a1}
          [{:tag :b, :color :red, :value 2}]]])

      (is (val= (hid->node (find-hid x [:** {:tag :b :value 2}]))
            {::tf/khids [], :tag :b, :color :red, :value 2}))

      (is (val= (hid->node (find-hid (root-hids) [:** {:color :blue :value 2}]))
            {::tf/khids [], :tag :c, :color :blue, :value 2}))

      (is= (set (format-paths (find-paths #{z y} [{:tag :a} {:value 2}])))
        #{[{:tag :a, :id :a2} [{:tag :b, :color :green, :value 2}]]
          [{:tag :a, :id :a3} [{:tag :c, :color :blue, :value 2}]]})
      (is= (set (format-paths (find-paths (root-hids) [{:tag :a} {:value 2}])))
        #{[{:tag :a, :id :a1} [{:tag :b, :color :red, :value 2}]]
          [{:tag :a, :id :a2} [{:tag :b, :color :green, :value 2}]]
          [{:tag :a, :id :a3} [{:tag :c, :color :blue, :value 2}]]})
      (is= (set (format-paths (find-paths (root-hids) [{:tag :a} {:tag :c}])))
        #{[{:tag :a, :id :a3} [{:tag :c, :color :blue, :value 3}]]
          [{:tag :a, :id :a3} [{:tag :c, :color :blue, :value 2}]]})

      (is= (find-paths (root-hids) [{:tag :a} {:tag :c}])
        [[1009 1007]
         [1009 1008]]))))

(verify
  (with-forest (new-forest)
    (let [data-1     [1 2 3]
          data-2     [[1 2 3]
                      [10]
                      []]
          data-3     [[[1 2 3]
                       [4 5 6]
                       [7 8 9]]
                      [[10 11]
                       [12 13]]
                      [[20]
                       [21]]
                      [[30]]
                      [[]]]

          data-4     [[[1 2 3]
                       [4 5 6]
                       [7 8 9]]
                      [[10 11]
                       [12 2]]
                      [[20]
                       [21]]
                      [[30]]
                      [[2]]]

          tree-1     (edn->tree data-1)
          tree-2     (edn->tree data-2)
          tree-3     (edn->tree data-3)
          tree-4     (edn->tree data-4)

          return-1     (tree->edn tree-1)
          return-2     (tree->edn tree-2)
          return-3     (tree->edn tree-3)
          return-4     (tree->edn tree-4)

          root-hid-1 (add-tree tree-1)
          root-hid-2 (add-tree tree-2)
          root-hid-3 (add-tree tree-3)
          root-hid-4 (add-tree tree-4)

          bush-1     (hid->bush root-hid-1)
          bush-2     (hid->bush root-hid-2)
          bush-3     (hid->bush root-hid-3)
          bush-4     (hid->bush root-hid-4)
    ]
      ;-------------------------------------------------------
      ; Note 2 different map formats re namespaced keys
      (is= tree-1
        {:tag   ::tf/vec,
         ::tf/index nil,
         ::tf/kids  [{::tf/value 1, ::tf/index 0, ::tf/kids []}
                     {::tf/value 2, ::tf/index 1, ::tf/kids []}
                     {::tf/value 3, ::tf/index 2, ::tf/kids []}]})
      (is= bush-1
        [{:tag :tupelo.forest/vec, ::tf/index nil}
         [{::tf/value 1, ::tf/index 0}]
         [{::tf/value 2, ::tf/index 1}]
         [{::tf/value 3, ::tf/index 2}]])
      ;-------------------------------------------------------

      ;(is= tree-2
      ;  #:tupelo.forest{:tag :tupelo.forest/list, :index nil, :kids
      ;                       [#:tupelo.forest{:tag :tupelo.forest/list, :index 0, :kids
      ;                                             [#:tupelo.forest{:value 1, :index 0, :kids []}
      ;                                              #:tupelo.forest{:value 2, :index 1, :kids []}
      ;                                              #:tupelo.forest{:value 3, :index 2, :kids []}]}
      ;                        #:tupelo.forest{:tag :tupelo.forest/list, :index 1, :kids
      ;                                             [#:tupelo.forest{:value 10, :index 0, :kids []}]}
      ;                        #:tupelo.forest{:tag :tupelo.forest/list, :index 2, :kids []}]})
      ;(is= bush-2   ; #todo document
      ;  [#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;    [#:tupelo.forest{:value 1, :index 0}]
      ;    [#:tupelo.forest{:value 2, :index 1}]
      ;    [#:tupelo.forest{:value 3, :index 2}]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;    [#:tupelo.forest{:value 10, :index 0}]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 2}]])
      ; -------------------------------------------------------
      ;(is= tree-3
      ;  #:tupelo.forest{:tag :tupelo.forest/list, :index nil, :kids
      ;                       [#:tupelo.forest{:tag :tupelo.forest/list, :index 0, :kids
      ;                                             [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                              :kids [#:tupelo.forest{:value 1, :index 0, :kids []}
      ;                                                                     #:tupelo.forest{:value 2, :index 1, :kids []}
      ;                                                                     #:tupelo.forest{:value 3, :index 2, :kids []}]}
      ;                                              #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                                              :kids [#:tupelo.forest{:value 4, :index 0, :kids []}
      ;                                                                     #:tupelo.forest{:value 5, :index 1, :kids []}
      ;                                                                     #:tupelo.forest{:value 6, :index 2, :kids []}]}
      ;                                              #:tupelo.forest{:tag  :tupelo.forest/list, :index 2,
      ;                                                              :kids [#:tupelo.forest{:value 7, :index 0, :kids []}
      ;                                                                     #:tupelo.forest{:value 8, :index 1, :kids []}
      ;                                                                     #:tupelo.forest{:value 9, :index 2, :kids []}]}]}
      ;                        #:tupelo.forest{:tag :tupelo.forest/list, :index 1, :kids
      ;                                             [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                              :kids [#:tupelo.forest{:value 10, :index 0, :kids []}
      ;                                                                     #:tupelo.forest{:value 11, :index 1, :kids []}]}
      ;                                              #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                                              :kids [#:tupelo.forest{:value 12, :index 0, :kids []}
      ;                                                                     #:tupelo.forest{:value 13, :index 1, :kids []}]}]}
      ;                        #:tupelo.forest{:tag :tupelo.forest/list, :index 2, :kids
      ;                                             [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                              :kids [#:tupelo.forest{:value 20, :index 0, :kids []}]}
      ;                                              #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                                              :kids [#:tupelo.forest{:value 21, :index 0, :kids []}]}]}
      ;                        #:tupelo.forest{:tag :tupelo.forest/list, :index 3, :kids
      ;                                             [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                              :kids [#:tupelo.forest{:value 30, :index 0, :kids []}]}]}
      ;                        #:tupelo.forest{:tag :tupelo.forest/list, :index 4, :kids
      ;                                             [#:tupelo.forest{:tag :tupelo.forest/list, :index 0, :kids []}]}]})
      ;(is= bush-3
      ;  [#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 1, :index 0}]
      ;     [#:tupelo.forest{:value 2, :index 1}]
      ;     [#:tupelo.forest{:value 3, :index 2}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:value 4, :index 0}]
      ;     [#:tupelo.forest{:value 5, :index 1}]
      ;     [#:tupelo.forest{:value 6, :index 2}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 2}
      ;     [#:tupelo.forest{:value 7, :index 0}]
      ;     [#:tupelo.forest{:value 8, :index 1}]
      ;     [#:tupelo.forest{:value 9, :index 2}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 10, :index 0}]
      ;     [#:tupelo.forest{:value 11, :index 1}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:value 12, :index 0}]
      ;     [#:tupelo.forest{:value 13, :index 1}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 2}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 20, :index 0}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:value 21, :index 0}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 3}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 30, :index 0}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 4}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}]]])

      ;(is= tree-4
      ;  #:tupelo.forest{:tag  :tupelo.forest/list, :index nil,
      ;                  :kids [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                         :kids [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                                :kids [#:tupelo.forest{:value 1, :index 0, :kids []}
      ;                                                                       #:tupelo.forest{:value 2, :index 1, :kids []}
      ;                                                                       #:tupelo.forest{:value 3, :index 2, :kids []}]}
      ;                                                #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                                                :kids [#:tupelo.forest{:value 4, :index 0, :kids []}
      ;                                                                       #:tupelo.forest{:value 5, :index 1, :kids []}
      ;                                                                       #:tupelo.forest{:value 6, :index 2, :kids []}]}
      ;                                                #:tupelo.forest{:tag  :tupelo.forest/list, :index 2,
      ;                                                                :kids [#:tupelo.forest{:value 7, :index 0, :kids []}
      ;                                                                       #:tupelo.forest{:value 8, :index 1, :kids []}
      ;                                                                       #:tupelo.forest{:value 9, :index 2, :kids []}]}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                         :kids [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                                :kids [#:tupelo.forest{:value 10, :index 0, :kids []}
      ;                                                                       #:tupelo.forest{:value 11, :index 1, :kids []}]}
      ;                                                #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                                                :kids [#:tupelo.forest{:value 12, :index 0, :kids []}
      ;                                                                       #:tupelo.forest{:value 2, :index 1, :kids []}]}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/list, :index 2,
      ;                                         :kids [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                                :kids [#:tupelo.forest{:value 20, :index 0, :kids []}]}
      ;                                                #:tupelo.forest{:tag  :tupelo.forest/list, :index 1,
      ;                                                                :kids [#:tupelo.forest{:value 21, :index 0, :kids []}]}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/list, :index 3,
      ;                                         :kids [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                                :kids [#:tupelo.forest{:value 30, :index 0, :kids []}]}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/list, :index 4,
      ;                                         :kids [#:tupelo.forest{:tag  :tupelo.forest/list, :index 0,
      ;                                                                :kids [#:tupelo.forest{:value 2, :index 0, :kids []}]}]}]} )
      ;(is= bush-4
      ;  [#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 1, :index 0}]
      ;     [#:tupelo.forest{:value 2, :index 1}]
      ;     [#:tupelo.forest{:value 3, :index 2}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:value 4, :index 0}]
      ;     [#:tupelo.forest{:value 5, :index 1}]
      ;     [#:tupelo.forest{:value 6, :index 2}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 2}
      ;     [#:tupelo.forest{:value 7, :index 0}]
      ;     [#:tupelo.forest{:value 8, :index 1}]
      ;     [#:tupelo.forest{:value 9, :index 2}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 10, :index 0}]
      ;     [#:tupelo.forest{:value 11, :index 1}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:value 12, :index 0}]
      ;     [#:tupelo.forest{:value 2, :index 1}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 2}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 20, :index 0}]]
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:value 21, :index 0}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 3}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 30, :index 0}]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index 4}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 2, :index 0}]]]])

      ;------------------------------------------------------------------------------
      ;(is= (format-paths (find-paths root-hid-1 [:** {::tf/value 2}]))
      ;  [[#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;    [#:tupelo.forest{:value 2, :index 1}]]])
      ;
      ;(is= (format-paths (find-paths root-hid-2 [:** {::tf/value 2}]))
      ;  [[#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:value 2, :index 1}]]]])
      ;
      ;(is= (format-paths (find-paths root-hid-3 [:** {::tf/value 2}]))
      ;  [[#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;      [#:tupelo.forest{:value 2, :index 1}]]]]])
      ;
      ;(is= (format-paths (find-paths root-hid-4 [:** {::tf/value 2}])) ; #todo document
      ;  [[#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;     [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;      [#:tupelo.forest{:value 2, :index 1}]]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;     [#:tupelo.forest{:tag :tupelo.forest/list, :index 1}
      ;      [#:tupelo.forest{:value 2, :index 1}]]]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index 4}
      ;     [#:tupelo.forest{:tag :tupelo.forest/list, :index 0}
      ;      [#:tupelo.forest{:value 2, :index 0}]]]]])

      ;-------------------------------------------------------
      ; basic inverse ok
      (is= data-1 return-1)
      (is= data-2 return-2)
      (is= data-3 return-3)
      (is= data-4 return-4)

    )))

(verify
  (with-forest (new-forest)
    (let [data-1     {:a 1 :b 2}
          tree-1     (edn->tree data-1)
          root-hid-1 (add-tree tree-1)
          bush-1     (hid->bush root-hid-1)
          return-1     (tree->edn tree-1)
         ]
      ;(is= tree-1
      ;  #:tupelo.forest{:tag  :tupelo.forest/entity, :index nil,
      ;                  :kids [#:tupelo.forest{:tag  :tupelo.forest/entry, :key :a,
      ;                                         :kids [#:tupelo.forest{:value 1, :index nil, :kids []}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/entry, :key :b,
      ;                                         :kids [#:tupelo.forest{:value 2, :index nil, :kids []}]}]} )
      ;(is= bush-1
      ;  [#:tupelo.forest{:tag :tupelo.forest/entity, :index nil}
      ;   [#:tupelo.forest{:tag :tupelo.forest/entry, :key :a}
      ;    [#:tupelo.forest{:value 1, :index nil}]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/entry, :key :b}
      ;    [#:tupelo.forest{:value 2, :index nil}]]])
      (is= data-1 return-1) )))

(verify
  (with-forest (new-forest)
    (let [data-2     {:a 1 :b [2 3 4]}
          tree-2     (edn->tree data-2)
          root-hid-2 (add-tree tree-2)
          bush-2     (hid->bush root-hid-2)
          return-2     (tree->edn tree-2) ]
      (is= data-2 return-2)
      ;(is= tree-2
      ;  #:tupelo.forest{:tag  :tupelo.forest/entity, :index nil,
      ;                  :kids [#:tupelo.forest{:tag  :tupelo.forest/entry, :key :a,
      ;                                         :kids [#:tupelo.forest{:value 1, :index nil, :kids []}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/entry, :key :b,
      ;                                         :kids [#:tupelo.forest{:tag   :tupelo.forest/list,
      ;                                                                :index nil,
      ;                                                                :kids  [#:tupelo.forest{:value 2, :index 0, :kids []}
      ;                                                                        #:tupelo.forest{:value 3, :index 1, :kids []}
      ;                                                                        #:tupelo.forest{:value 4, :index 2, :kids []}]}]}]} )
      ;(is= bush-2
      ;  [#:tupelo.forest{:tag :tupelo.forest/entity, :index nil}
      ;   [#:tupelo.forest{:tag :tupelo.forest/entry, :key :a}
      ;    [#:tupelo.forest{:value 1, :index nil}]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/entry, :key :b}
      ;    [#:tupelo.forest{:tag :tupelo.forest/list, :index nil}
      ;     [#:tupelo.forest{:value 2, :index 0}]
      ;     [#:tupelo.forest{:value 3, :index 1}]
      ;     [#:tupelo.forest{:value 4, :index 2}]]]])

    )))

(verify
  (with-forest (new-forest)
    (let [data-3     {:a 1 :b {:c 3}}
          tree-3     (edn->tree data-3)
          root-hid-3 (add-tree tree-3)
          bush-3     (hid->bush root-hid-3)
          return-3   (tree->edn tree-3)]
      (is= data-3 return-3)
      ;(is= tree-3
      ;  #:tupelo.forest{:tag  :tupelo.forest/entity, :index nil,
      ;                  :kids [#:tupelo.forest{:tag  :tupelo.forest/entry, :key :a,
      ;                                         :kids [#:tupelo.forest{:value 1, :index nil, :kids []}]}
      ;                         #:tupelo.forest{:tag  :tupelo.forest/entry, :key :b,
      ;                                         :kids [#:tupelo.forest{:tag  :tupelo.forest/entity, :index nil,
      ;                                                                :kids [#:tupelo.forest{:tag  :tupelo.forest/entry, :key  :c,
      ;                                                                                       :kids [#:tupelo.forest{:value 3, :index nil, :kids []}]}]}]}]})
      ;(is= bush-3
      ;  [#:tupelo.forest{:tag :tupelo.forest/entity, :index nil}
      ;   [#:tupelo.forest{:tag :tupelo.forest/entry, :key :a}
      ;    [#:tupelo.forest{:value 1, :index nil}]]
      ;   [#:tupelo.forest{:tag :tupelo.forest/entry, :key :b}
      ;    [#:tupelo.forest{:tag :tupelo.forest/entity, :index nil}
      ;     [#:tupelo.forest{:tag :tupelo.forest/entry, :key :c}
      ;      [#:tupelo.forest{:value 3, :index nil}]]]]])
    )))

;(verify
;  (let [tree-1   #:tupelo.forest{:tag   :tupelo.forest/list,
;                                 :index nil,
;                                 :kids  [#:tupelo.forest{:value 2, :index 0, :kids []}
;                                         #:tupelo.forest{:value 3, :index 1, :kids []}
;                                         #:tupelo.forest{:value 4, :index 2, :kids []}]}
;        data-1   (tree->data tree-1)
;        return-1 (data->tree data-1)]
;    (is= tree-1 return-1)
;    (is= (validate-list-kids-idx tree-1)
;      [#:tupelo.forest{:value 2, :index 0, :kids []}
;       #:tupelo.forest{:value 3, :index 1, :kids []}
;       #:tupelo.forest{:value 4, :index 2, :kids []}])
;    ))

(verify
  (let [data-1   {:a 1 :b 2}
        tree-1   (edn->tree data-1)
        return-1 (tree->edn tree-1)]
    (is= return-1 data-1))
  (let [data-2   {:a 1 :b [2 3 4]}
        tree-2   (edn->tree data-2)
        return-2 (tree->edn tree-2)]
    (is= return-2 data-2))
  (let [data-3   {:a 1 :b {:c 3}}
        tree-3   (edn->tree data-3)
        return-3 (tree->edn tree-3)]
    (is= return-3 data-3)) )

(verify
  (throws? (nest-enlive-nodes []))
  (is= (nest-enlive-nodes [{:tag :a, :attrs {:a 1}, :content [1 1]}])
    {:tag :a, :attrs {:a 1}, :content [1 1]})
  (is= (nest-enlive-nodes [{:tag :a :attrs {:a 1} :content []}
                           {:tag :b :attrs {:b 2} :content [2 2 2]}])
    {:tag     :a
     :attrs   {:a 1}
     :content [{:tag :b :attrs {:b 2} :content [2 2 2]}]})
  (is= (nest-enlive-nodes [{:tag :a :attrs {:a 1} :content [1 1 1]}
                           {:tag :b :attrs {:b 2} :content [2 2]}
                           {:tag :c :attrs {:c 3} :content [3 3 3]}])
    {:tag   :a,
     :attrs {:a 1},
     :content
            [{:tag     :b,
              :attrs   {:b 2},
              :content [{:tag :c, :attrs {:c 3}, :content [3 3 3]}]}]}))

