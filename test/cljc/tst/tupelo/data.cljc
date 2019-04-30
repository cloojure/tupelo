;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.data
  (:use tupelo.data tupelo.core)
  #?(:clj (:refer-clojure :exclude [load ->VecNode]))
  #?(:clj (:require
            [tupelo.test :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty unlazy let-spy only forv glue
                                       ]]
            [tupelo.data :as td]
            [tupelo.data.index :as tdi]
            [tupelo.lexical :as lex]
            [clojure.data.avl :as avl]
            [schema.core :as s]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [tupelo.data.index :as index]))
  #?(:cljs (:require
             [tupelo.test-cljs :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]
              :include-macros true]
             [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
             [tupelo.data :as td]
             [tupelo.lexical :as lex]
             [clojure.data.avl :as avl]
             [schema.core :as s]
             ))
  )

; #todo fix for cljs

#?(:cljs (enable-console-print!))

(dotest
  (let [ss123 (t/it-> (index/empty-index)
                (conj it [1 :a])
                (conj it [3 :a])
                (conj it [2 :a]))
        ss13  (disj ss123 [2 :a])]
    (is= #{[1 :a] [2 :a] [3 :a]} ss123)
    (is= [[1 :a] [2 :a] [3 :a]] (vec ss123))
    (is= #{[1 :a] [3 :a]} ss13))

  (is   (map? (leaf 3)))
  (is   (map? {:a 1}))
  (is   (record? (leaf 3)))
  (isnt (record? {:a 1}))

  ; Leaf and Hid records sort separately in the index. Eid sorts first since the type name
  ; `tupelo.data.Eid` sorts before `tupelo.data.Leaf`
  (let [idx      (-> (index/empty-index)
                   ; using shortcut constructors
                   (index/add-entry [1 (leaf 3)])
                   (index/add-entry [1 (eid 3)])
                   (index/add-entry [1 (leaf 1)])
                   (index/add-entry [1 (eid 1)])
                   (index/add-entry [1 (leaf 2)])
                   (index/add-entry [1 (eid 2)])

                   ; using Clojure record constructors
                   (index/add-entry [0 (->Leaf 3)])
                   (index/add-entry [0 (->Eid 3)])
                   (index/add-entry [0 (->Leaf 1)])
                   (index/add-entry [0 (->Eid 1)])
                   (index/add-entry [0 (->Leaf 2)])
                   (index/add-entry [0 (->Eid 2)]))

        expected [[0 #tupelo.data.Eid{:raw 1}] ; tagged record literal
                  [0 #tupelo.data.Eid{:raw 2}]
                  [0 #tupelo.data.Eid{:raw 3}]
                  [0 (leaf 1)]
                  [0 (leaf 2)]
                  [0 (leaf 3)]
                  [1 (eid 1)]
                  [1 (eid 2)]
                  [1 (eid 3)]
                  [1 #tupelo.data.Leaf{:raw 1}]
                  [1 #tupelo.data.Leaf{:raw 2}]
                  [1 #tupelo.data.Leaf{:raw 3}]]]
    (is= (vec idx) expected)) )

(dotest-focus
  (with-tdb (new-tdb)
    (eid-count-reset)
    (is= (deref *tdb*)
      {:eids-map #{} :eids-array #{} :eid->parent {} :idx-eav #{} :idx-vae #{} :idx-ave #{}} )


      (let [edn-val  {:a 1}
            root-eid (td/add-edn edn-val)]
        (is= 1001 root-eid)
        (is= (unlazy (deref *tdb*))
          {:eids-map    #{1001},
           :eids-array  #{},
           :eid->parent {1001 nil},
           :idx-eav     #{[1001 :a {:leaf 1}]},
           :idx-vae     #{[{:leaf 1} :a 1001]},
           :idx-ave     #{[:a {:leaf 1} 1001]}})

        (is= edn-val (td/eid->edn root-eid))




        )
    ))

  ;  (let [edn-val  {:a 1}
  ;        root-hid (td/add-edn edn-val)]
  ;    (is= (unlazy @td/*tdb*) ; coerce all from record to plain map for comparison
  ;      {:idx-array-entry-ei #{},
  ;       :idx-array-entry-ie #{},
  ;       :idx-hid            {1001 {:-leaf-val 5, :-parent-hid nil},
  ;                            1002 {:-mn-data {:a 1003}, :-parent-hid nil},
  ;                            1003 {:-me-key :a, :-me-val-hid 1004, :-parent-hid 1002},
  ;                            1004 {:-leaf-val 1, :-parent-hid 1003}},
  ;       :idx-leaf           #{[1 1004] [5 1001]},
  ;       :idx-map-entry-kv   #{[:a 1 1003]},
  ;       :idx-map-entry-vk   #{[1 :a 1003]}} )
  ;    (is= edn-val (td/hid->edn root-hid)))
  ;  (let [edn-val  [7 8]
  ;        root-hid (td/add-edn edn-val)]
  ;    (is= (unlazy @td/*tdb*) ; coerce all from record to plain map for comparison
  ;      {:idx-array-entry-ei #{[7 0 1006] [8 1 1008]},
  ;       :idx-array-entry-ie #{[0 7 1006] [1 8 1008]},
  ;       :idx-hid            {1001 {:-leaf-val 5, :-parent-hid nil},
  ;                            1002 {:-mn-data {:a 1003}, :-parent-hid nil},
  ;                            1003 {:-me-key :a, :-me-val-hid 1004, :-parent-hid 1002},
  ;                            1004 {:-leaf-val 1, :-parent-hid 1003},
  ;                            1005 {:-an-data {0 1006, 1 1008}, :-parent-hid nil},
  ;                            1006 {:-ae-elem-hid 1007, :-ae-idx 0, :-parent-hid 1005},
  ;                            1007 {:-leaf-val 7, :-parent-hid 1006},
  ;                            1008 {:-ae-elem-hid 1009, :-ae-idx 1, :-parent-hid 1005},
  ;                            1009 {:-leaf-val 8, :-parent-hid 1008}},
  ;       :idx-leaf           #{[1 1004] [5 1001] [7 1007] [8 1009]},
  ;       :idx-map-entry-kv   #{[:a 1 1003]},
  ;       :idx-map-entry-vk   #{[1 :a 1003]}})
  ;    (is= edn-val (td/hid->edn root-hid))) )
  ;
  ;(td/with-tdb (td/new-tdb)
  ;  (td/hid-count-reset)
  ;  (let [edn-val  #{3 4}
  ;        root-hid (td/add-edn edn-val)]
  ;    (is= (unlazy @td/*tdb*) ; coerce all from record to plain map for comparison
  ;      {:idx-array-entry-ei #{},
  ;       :idx-array-entry-ie #{},
  ;       :idx-hid            {1001 {:-parent-hid nil, :-sn-data {3 1003, 4 1002}},
  ;                            1002 {:-leaf-val 4, :-parent-hid 1001},
  ;                            1003 {:-leaf-val 3, :-parent-hid 1001}},
  ;       :idx-leaf           #{[3 1003] [4 1002]},
  ;       :idx-map-entry-kv   #{},
  ;       :idx-map-entry-vk   #{}} )
  ;    (is= edn-val (td/hid->edn root-hid))) )

;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [edn-val  {:num 5
;                    :map {:a 1 :b 2}
;                    :vec [5 6 7]
;                    :set #{3 4}
;                    :str "hello"
;                    :kw  :nothing}
;          root-hid (td/add-edn edn-val)]
;      (is= (unlazy @td/*tdb*)
;        {:idx-array-entry-ei #{[5 0 1012] [6 1 1014] [7 2 1016]},
;         :idx-array-entry-ie #{[0 5 1012] [1 6 1014] [2 7 1016]},
;         :idx-hid            {1001 {:-mn-data    {:kw 1024, :map 1004, :num 1002, :set 1018, :str 1022, :vec 1010},
;                                    :-parent-hid nil},
;                              1002 {:-me-key :num, :-me-val-hid 1003, :-parent-hid 1001},
;                              1003 {:-leaf-val 5, :-parent-hid 1002},
;                              1004 {:-me-key :map, :-me-val-hid 1005, :-parent-hid 1001},
;                              1005 {:-mn-data {:a 1006, :b 1008}, :-parent-hid 1004},
;                              1006 {:-me-key :a, :-me-val-hid 1007, :-parent-hid 1005},
;                              1007 {:-leaf-val 1, :-parent-hid 1006},
;                              1008 {:-me-key :b, :-me-val-hid 1009, :-parent-hid 1005},
;                              1009 {:-leaf-val 2, :-parent-hid 1008},
;                              1010 {:-me-key :vec, :-me-val-hid 1011, :-parent-hid 1001},
;                              1011 {:-an-data {0 1012, 1 1014, 2 1016}, :-parent-hid 1010},
;                              1012 {:-ae-elem-hid 1013, :-ae-idx 0, :-parent-hid 1011},
;                              1013 {:-leaf-val 5, :-parent-hid 1012},
;                              1014 {:-ae-elem-hid 1015, :-ae-idx 1, :-parent-hid 1011},
;                              1015 {:-leaf-val 6, :-parent-hid 1014},
;                              1016 {:-ae-elem-hid 1017, :-ae-idx 2, :-parent-hid 1011},
;                              1017 {:-leaf-val 7, :-parent-hid 1016},
;                              1018 {:-me-key :set, :-me-val-hid 1019, :-parent-hid 1001},
;                              1019 {:-parent-hid 1018, :-sn-data {3 1021, 4 1020}},
;                              1020 {:-leaf-val 4, :-parent-hid 1019},
;                              1021 {:-leaf-val 3, :-parent-hid 1019},
;                              1022 {:-me-key :str, :-me-val-hid 1023, :-parent-hid 1001},
;                              1023 {:-leaf-val "hello", :-parent-hid 1022},
;                              1024 {:-me-key :kw, :-me-val-hid 1025, :-parent-hid 1001},
;                              1025 {:-leaf-val :nothing, :-parent-hid 1024}},
;         :idx-leaf           #{[:nothing 1025] [1 1007] [2 1009] [3 1021] [4 1020] [5 1003]
;                               [5 1013] [6 1015] [7 1017] ["hello" 1023]},
;         :idx-map-entry-kv   #{[:a 1 1006] [:b 2 1008] [:kw :nothing 1024] [:num 5 1002]
;                               [:str "hello" 1022]},
;         :idx-map-entry-vk   #{[:nothing :kw 1024] [1 :a 1006] [2 :b 1008] [5 :num 1002]
;                               ["hello" :str 1022]}})
;      (is= edn-val (td/hid->edn root-hid))
;      (let [hid-num (only (td/index-find-mapentry-key :num))]
;        (is= 1002 hid-num)
;        (is= (unlazy (td/hid->node hid-num)) {:-me-key :num, :-me-val-hid 1003, :-parent-hid 1001} )
;        (is= (t/map-entry :num 5) (td/hid->edn hid-num)))
;      (let [hid-b (only (td/index-find-mapentry-key :b))]
;        (is= 1008 hid-b)
;        (is= (unlazy (td/hid->node hid-b)) {:-me-key :b, :-me-val-hid 1009, :-parent-hid 1005})
;        (is= (td/hid->edn hid-b) (t/map-entry :b 2)))
;      (let [hid-2 (only (td/index-find-arrayentry-idx 2))]
;        (is= 1016 hid-2)
;        (is= (unlazy (td/hid->node hid-2))
;          {:-ae-elem-hid 1017, :-ae-idx 2, :-parent-hid 1011} )
;        (is= (td/hid->edn hid-2) 7 )) )))
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [edn-val  {:aa [1 2 3]
;                    :bb [2 3 4]
;                    :cc [3 4 5 6]}
;          root-hid (td/add-edn edn-val)]
;      (is= (unlazy @td/*tdb*)
;        {:idx-array-entry-ei #{[1 0 1004] [2 0 1012] [2 1 1006] [3 0 1020] [3 1 1014] [3 2 1008]
;                               [4 1 1022] [4 2 1016] [5 2 1024] [6 3 1026]},
;         :idx-array-entry-ie #{[0 1 1004] [0 2 1012] [0 3 1020] [1 2 1006] [1 3 1014] [1 4 1022]
;                               [2 3 1008] [2 4 1016] [2 5 1024] [3 6 1026]},
;         :idx-hid            {1001 {:-mn-data {:aa 1002, :bb 1010, :cc 1018}, :-parent-hid nil},
;                              1002 {:-me-key :aa, :-me-val-hid 1003, :-parent-hid 1001},
;                              1003 {:-an-data {0 1004, 1 1006, 2 1008}, :-parent-hid 1002},
;                              1004 {:-ae-elem-hid 1005, :-ae-idx 0, :-parent-hid 1003},
;                              1005 {:-leaf-val 1, :-parent-hid 1004},
;                              1006 {:-ae-elem-hid 1007, :-ae-idx 1, :-parent-hid 1003},
;                              1007 {:-leaf-val 2, :-parent-hid 1006},
;                              1008 {:-ae-elem-hid 1009, :-ae-idx 2, :-parent-hid 1003},
;                              1009 {:-leaf-val 3, :-parent-hid 1008},
;                              1010 {:-me-key :bb, :-me-val-hid 1011, :-parent-hid 1001},
;                              1011 {:-an-data {0 1012, 1 1014, 2 1016}, :-parent-hid 1010},
;                              1012 {:-ae-elem-hid 1013, :-ae-idx 0, :-parent-hid 1011},
;                              1013 {:-leaf-val 2, :-parent-hid 1012},
;                              1014 {:-ae-elem-hid 1015, :-ae-idx 1, :-parent-hid 1011},
;                              1015 {:-leaf-val 3, :-parent-hid 1014},
;                              1016 {:-ae-elem-hid 1017, :-ae-idx 2, :-parent-hid 1011},
;                              1017 {:-leaf-val 4, :-parent-hid 1016},
;                              1018 {:-me-key :cc, :-me-val-hid 1019, :-parent-hid 1001},
;                              1019 {:-an-data {0 1020, 1 1022, 2 1024, 3 1026}, :-parent-hid 1018},
;                              1020 {:-ae-elem-hid 1021, :-ae-idx 0, :-parent-hid 1019},
;                              1021 {:-leaf-val 3, :-parent-hid 1020},
;                              1022 {:-ae-elem-hid 1023, :-ae-idx 1, :-parent-hid 1019},
;                              1023 {:-leaf-val 4, :-parent-hid 1022},
;                              1024 {:-ae-elem-hid 1025, :-ae-idx 2, :-parent-hid 1019},
;                              1025 {:-leaf-val 5, :-parent-hid 1024},
;                              1026 {:-ae-elem-hid 1027, :-ae-idx 3, :-parent-hid 1019},
;                              1027 {:-leaf-val 6, :-parent-hid 1026}},
;         :idx-leaf           #{[1 1005] [2 1007] [2 1013] [3 1009] [3 1015] [3 1021] [4 1017]
;                               [4 1023] [5 1025] [6 1027]},
;         :idx-map-entry-kv   #{},
;         :idx-map-entry-vk   #{}})
;      (let [hid-1-2 (only (td/index-find-arrayentry (t/map-entry 1 2)))]
;        (is= 1006 hid-1-2)
;        (is= (unlazy (td/hid->node hid-1-2))
;          {:-ae-elem-hid 1007, :-ae-idx 1, :-parent-hid 1003} )
;        (is= (td/hid->edn (td/hid->parent-hid hid-1-2)) [1 2 3]))
;      (let [hid-0-2 (only (td/index-find-arrayentry (t/map-entry 0 2)))]
;        (is= (td/hid->edn (td/hid->parent-hid hid-0-2)) [2 3 4]))
;      (is= [1 2 3] (it-> 2
;                     (t/map-entry it 3)
;                     (td/index-find-arrayentry it)
;                     (only it)
;                     (td/hid->parent-hid it)
;                     (td/hid->edn it)))
;      (is= [2 3 4] (it-> 1
;                     (t/map-entry it 3)
;                     (td/index-find-arrayentry it)
;                     (only it)
;                     (td/hid->parent-hid it)
;                     (td/hid->edn it)))
;      (is= [3 4 5 6] (it-> 0
;                       (t/map-entry it 3)
;                       (td/index-find-arrayentry it)
;                       (only it)
;                       (td/hid->parent-hid it)
;                       (td/hid->edn it))))))
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (let [edn-0    {:a 1 :b 2}
;          root-hid (td/add-edn edn-0)]
;      (is= edn-0 (td/hid->edn root-hid))))
;
;  (td/with-tdb (td/new-tdb)
;    (let [edn-0    [1 2 3]
;          root-hid (td/add-edn edn-0)]
;      (is= edn-0 (td/hid->edn root-hid))))
;
;  (td/with-tdb (td/new-tdb)
;    (let [edn-0    "hello"
;          root-hid (td/add-edn edn-0)]
;      (is= edn-0 (td/hid->edn root-hid))))
;
;  (td/with-tdb (td/new-tdb)
;    (let [data-1   {:a [{:b 2}
;                        {:c 3}
;                        {:d 4}]
;                    :e {:f 6}
;                    :g :green
;                    :h "hotel"
;                    :i 1}
;          root-hid (td/add-edn data-1)]
;      (is= data-1 (td/hid->edn root-hid)))))
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (let [edn-0      #{1 2 3}
;          root-hid   (td/add-edn edn-0)
;          edn-result (td/hid->edn root-hid)]
;      (is (set? edn-result)) ; ***** Sets are coerced to vectors! *****
;      (is-set= [1 2 3] edn-result)))
;  (td/with-tdb (td/new-tdb)
;    (let [edn-0    #{:a 1 :b 2}
;          root-hid (td/add-edn edn-0)]
;      (is= edn-0 (td/hid->edn root-hid))))
;  (td/with-tdb (td/new-tdb)
;    (let [edn-0    {:a 1 :b #{1 2 3}}
;          root-hid (td/add-edn edn-0)]
;      (is= edn-0 (td/hid->edn root-hid)))))
;
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:b 2}
;                        {:c 3}
;                        {:d 4}]
;                    :e {:f 6}
;                    :g :green
;                    :h "hotel"
;                    :i 1}
;          root-hid (td/add-edn data)]
;
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:a])))
;        [{:b 2} {:c 3} {:d 4}])
;
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:a 0]))) {:b 2})
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:a 2]))) {:d 4})
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:a 2 :d]))) 4)
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:e]))) {:f 6})
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:e :f]))) 6)
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:h]))) "hotel")
;      (is= (td/hid->edn (only (td/hid-nav root-hid [:i]))) 1)
;      (let [kid-hids     (td/hid-nav root-hid [:a :*])
;            parent-hids  (mapv td/hid->parent-hid kid-hids)
;            parent-hid   (t/xfirst parent-hids)
;            parent-hid-2 (td/hid->parent-hid parent-hid)
;            ]
;        (is= (mapv td/hid->edn kid-hids)
;          [{:b 2} {:c 3} {:d 4}])
;        (is (apply = parent-hids))
;        (is= (td/hid->edn parent-hid)
;          [{:b 2} {:c 3} {:d 4}])
;        (is= (td/hid->edn parent-hid-2) data))
;      (let [four-hid          (only (td/hid-nav root-hid [:a 2 :d]))
;            four-hid-parent-3 (-> four-hid
;                                td/hid->parent-hid
;                                td/hid->parent-hid
;                                td/hid->parent-hid)]
;        (is= 4 (td/hid->edn four-hid))
;        (is= data (td/hid->edn four-hid-parent-3))))))
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data         [{:a 1 :b :first}
;                        {:a 2 :b :second}
;                        {:a 3 :b :third}
;                        {:a 4 :b "fourth"}
;                        {:a 5 :b "fifth"}
;                        {:a 1 :b 101}
;                        {:a 1 :b 102}]
;          root-hid     (td/add-edn data)
;          hids-match   (td/index-find-leaf 1)
;          edn-match    (mapv td/hid->edn hids-match)
;          edn-parents  (it-> hids-match
;                         (mapv td/hid->parent-hid it)
;                         (mapv td/hid->edn it))]
;      (is= edn-match [1 1 1])
;      (is= edn-parents
;        [{:a 1, :b :first}
;         {:a 1, :b 101}
;         {:a 1, :b 102}])
;
;      (is= (t/map-entry :b 101) (td/hid->edn (only (td/index-find-mapentry (t/map-entry :b 101)))))
;      (is= {:a 1 :b 101} (td/hid->edn (td/hid->parent-hid (only (td/index-find-mapentry (map-entry :b 101))))))
;      (is= {:a 2 :b :second} (td/hid->edn (td/hid->parent-hid (only (td/index-find-mapentry (map-entry :b :second))))))
;      (is= {:a 3 :b :third} (td/hid->edn (td/hid->parent-hid (only (td/index-find-mapentry (map-entry :a 3))))))) )
;
;  (td/with-tdb (td/new-tdb)
;    (let [data      [{:a 1 :x :first}
;                     {:a 2 :x :second}
;                     {:a 3 :x :third}
;                     {:b 1 :x 101}
;                     {:b 2 :x 102}
;                     {:c 1 :x 301}
;                     {:c 2 :x 302}]
;          root-hid  (td/add-edn data)
;          hid-match (only (td/index-find-mapentry
;                              (->map-entry {:a 1})))
;          edn-match (td/hid->edn (td/hid->parent-hid hid-match))]
;      (is= edn-match {:a 1 :x :first})))
;
;  (td/with-tdb (td/new-tdb)
;    (let [data     [{:a 1 :b 1 :c 1}
;                    {:a 1 :b 2 :c 2}
;                    {:a 1 :b 1 :c 3}
;                    {:a 2 :b 2 :c 4}
;                    {:a 2 :b 1 :c 5}
;                    {:a 2 :b 2 :c 6}]
;          root-hid (td/add-edn data)]
;      ;(t/spy-pretty (deref td/*tdb*))
;      (let [edns (mapv #(td/hid->edn (td/hid->parent-hid %))
;                   (td/index-find-mapentry (map-entry :a 1)))]
;        (is= edns
;          [{:a 1, :b 1, :c 1}
;           {:a 1, :b 2, :c 2}
;           {:a 1, :b 1, :c 3}]))
;      (let [edns (mapv #(td/hid->edn (td/hid->parent-hid %))
;                   (td/index-find-mapentry (map-entry :a 2)))]
;        (is= edns
;          [{:a 2, :b 2, :c 4}
;           {:a 2, :b 1, :c 5}
;           {:a 2, :b 2, :c 6}]))
;      (let [edns (mapv #(td/hid->edn (td/hid->parent-hid %))
;                   (td/index-find-mapentry (map-entry :b 1)))]
;        (is= edns
;          [{:a 1, :b 1, :c 1}
;           {:a 1, :b 1, :c 3}
;           {:a 2, :b 1, :c 5}]))
;      (let [edns (mapv #(td/hid->edn (td/hid->parent-hid %))
;                   (td/index-find-mapentry (map-entry :c 6)))]
;        (is= edns [{:a 2, :b 2, :c 6}]))))
;
;  (td/with-tdb (td/new-tdb)
;    (let [data     [{:a 1 :b 1 :c 1}
;                    {:a 1 :b 2 :c 2}
;                    {:a 1 :b 1 :c 3}
;                    {:a 2 :b 2 :c 4}
;                    {:a 2 :b 1 :c 5}
;                    {:a 2 :b 2 :c 6}]
;          root-hid (td/add-edn data)]
;      (let [hid (only (td/index-find-submap {:a 1 :b 2}))
;            edn (td/hid->edn hid)]
;        (is= edn {:a 1 :b 2 :c 2}))
;      (let [hids (td/index-find-submap {:a 1 :b 1})
;            edns (mapv td/hid->edn hids)]
;        (is-set= edns [{:a 1, :b 1, :c 1}
;                       {:a 1, :b 1, :c 3}])))) )
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:id 2 :color :red}
;                        {:id 3 :color :yellow}
;                        {:id 4 :color :blue}] }
;          root-hid (td/add-edn data) ]
;      (is= (unlazy @td/*tdb*)
;        {:idx-array-entry-ei #{},
;         :idx-array-entry-ie #{},
;         :idx-hid            {1001 {:-mn-data {:a 1002}, :-parent-hid nil},
;                              1002 {:-me-key :a, :-me-val-hid 1003, :-parent-hid 1001},
;                              1003 {:-an-data {0 1004, 1 1010, 2 1016}, :-parent-hid 1002},
;                              1004 {:-ae-elem-hid 1005, :-ae-idx 0, :-parent-hid 1003},
;                              1005 {:-mn-data {:color 1008, :id 1006}, :-parent-hid 1004},
;                              1006 {:-me-key :id, :-me-val-hid 1007, :-parent-hid 1005},
;                              1007 {:-leaf-val 2, :-parent-hid 1006},
;                              1008 {:-me-key :color, :-me-val-hid 1009, :-parent-hid 1005},
;                              1009 {:-leaf-val :red, :-parent-hid 1008},
;                              1010 {:-ae-elem-hid 1011, :-ae-idx 1, :-parent-hid 1003},
;                              1011 {:-mn-data {:color 1014, :id 1012}, :-parent-hid 1010},
;                              1012 {:-me-key :id, :-me-val-hid 1013, :-parent-hid 1011},
;                              1013 {:-leaf-val 3, :-parent-hid 1012},
;                              1014 {:-me-key :color, :-me-val-hid 1015, :-parent-hid 1011},
;                              1015 {:-leaf-val :yellow, :-parent-hid 1014},
;                              1016 {:-ae-elem-hid 1017, :-ae-idx 2, :-parent-hid 1003},
;                              1017 {:-mn-data {:color 1020, :id 1018}, :-parent-hid 1016},
;                              1018 {:-me-key :id, :-me-val-hid 1019, :-parent-hid 1017},
;                              1019 {:-leaf-val 4, :-parent-hid 1018},
;                              1020 {:-me-key :color, :-me-val-hid 1021, :-parent-hid 1017},
;                              1021 {:-leaf-val :blue, :-parent-hid 1020}},
;         :idx-leaf           #{[:blue 1021] [:red 1009] [:yellow 1015] [2 1007] [3 1013] [4 1019]},
;         :idx-map-entry-kv   #{[:color :blue 1020] [:color :red 1008] [:color :yellow 1014]
;                               [:id 2 1006] [:id 3 1012] [:id 4 1018]},
;         :idx-map-entry-vk   #{[:blue :color 1020] [:red :color 1008] [:yellow :color 1014]
;                               [2 :id 1006] [3 :id 1012] [4 :id 1018]}})
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :*]))
;        [{:id 2, :color :red}
;         {:id 3, :color :yellow}
;         {:id 4, :color :blue}] )
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :* :id])) [2 3 4] ) ) ) )
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:id [2 22] :color :red}
;                        {:id [3 33] :color :yellow}
;                        {:id [4 44] :color :blue}] }
;          root-hid (td/add-edn data) ]
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :*]))
;        [{:id [2 22], :color :red}
;         {:id [3 33], :color :yellow}
;         {:id [4 44], :color :blue}])
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :* :id]))
;        [[2 22] [3 33] [4 44]] )
;      (newline)
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :* :id :*]))
;        [2 22 3 33 4 44])
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :* :id 0]))
;        [2 3 4])
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:a :* :id 1]))
;        [22 33 44]) ) ) )
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:id 2 :color :red}
;                        {:id 3 :color :yellow}
;                        {:id 4 :color :blue}]
;                    :e [{:id 2 :flower :rose}
;                        {:id 3 :flower :daisy}
;                        {:id 4 :flower :tulip}]}
;          root-hid (td/add-edn data)]
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:*]))
;        [[{:id 2, :color :red}
;          {:id 3, :color :yellow}
;          {:id 4, :color :blue}]
;         [{:id 2, :flower :rose}
;          {:id 3, :flower :daisy}
;          {:id 4, :flower :tulip}]])
;      (is= (mapv td/hid->edn (td/hid-nav root-hid [:* :*]))
;        [{:id 2, :color :red}
;         {:id 3, :color :yellow}
;         {:id 4, :color :blue}
;         {:id 2, :flower :rose}
;         {:id 3, :flower :daisy}
;         {:id 4, :flower :tulip}])
;      (let [id-hids        (td/hid-nav root-hid [:* :* :id])
;            id-vals        (mapv td/hid->edn id-hids)
;            id-vals-unique (distinct id-vals)
;            merged-recs    (forv [id id-vals-unique]
;                             (let [men-hids (td/index-find-mapentry (t/map-entry :id id))]
;                               (apply glue (mapv #(td/hid->edn (td/hid->parent-hid %)) men-hids))))]
;        (is= id-vals [2 3 4 2 3 4])
;        (is-set= id-vals-unique [2 3 4])
;        (is-set= merged-recs
;          [{:id 4, :color :blue, :flower :tulip}
;           {:id 3, :color :yellow, :flower :daisy}
;           {:id 2, :color :red, :flower :rose}]))
;      (let [id-hids        (td/hid-nav root-hid [:* :* :*])
;            id-vals        (mapv td/hid->edn id-hids)
;            id-vals-unique (distinct id-vals)]
;        (is-set= id-vals-unique
;          [2 :red 3 :yellow 4 :blue :rose :daisy :tulip])))) )
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:id 2 :color :red}
;                        {:id 3 :color :yellow}
;                        {:id 4 :color :blue}
;                        {:id 5 :color :pink}
;                        {:id 6 :color :white}]
;                    :b {:c [{:ident 2 :flower :rose}
;                            {:ident 3 :flower :daisy}
;                            {:ident 4 :flower :tulip}
;                            ]}}
;          root-hid (td/add-edn data)]
;      (let [id-hids           (td/hid-nav root-hid [:a :* :id])
;            id-vals           (mapv td/hid->edn id-hids)
;            id-vals-unique    (distinct id-vals)
;            ident-hids        (td/hid-nav root-hid [:b :c :* :ident])
;            ident-vals        (mapv td/hid->edn ident-hids)
;            ident-vals-unique (distinct ident-vals)
;
;            id-cmn            (set/intersection (set id-vals-unique) (set ident-vals-unique))
;
;            recs-id-cmn       (forv [id id-cmn]
;                                (let [rec-hids (td/index-find-mapentry (t/map-entry :id id))]
;                                  (apply glue (mapv #(td/edn (td/hid->node %)) rec-hids))))
;            recs-ident-cmn    (forv [ident id-cmn]
;                                (let [rec-hids (td/index-find-mapentry (t/map-entry :ident ident))]
;                                  (apply glue (mapv td/hid->edn rec-hids))))
;            ]
;        (is= id-vals [2 3 4 5 6])
;        (is-set= id-vals-unique [2 3 4 5 6])
;
;        (is= ident-vals [2 3 4])
;        (is-set= ident-vals-unique [2 3 4])
;        (is-set= id-cmn [2 3 4])
;
;        (is-set= recs-id-cmn
;          [{:id 2}
;           {:id 3}
;           {:id 4} ])
;        (is-set= recs-ident-cmn
;          [{:ident 4}
;           {:ident 3}
;           {:ident 2}]) ))) )
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:id 2 :color :red}
;                        {:id 3 :color :yellow}
;                        {:id 4 :color :blue}]}
;          root-hid (td/add-edn data)
;          hid-red  (only (td/index-find-leaf :red))]
;      (is= (unlazy @td/*tdb*)
;        {:idx-array-entry-ei #{},
;         :idx-array-entry-ie #{},
;         :idx-hid            {1001 {:-mn-data {:a 1002}, :-parent-hid nil},
;                              1002 {:-me-key :a, :-me-val-hid 1003, :-parent-hid 1001},
;                              1003 {:-an-data {0 1004, 1 1010, 2 1016}, :-parent-hid 1002},
;                              1004 {:-ae-elem-hid 1005, :-ae-idx 0, :-parent-hid 1003},
;                              1005 {:-mn-data {:color 1008, :id 1006}, :-parent-hid 1004},
;                              1006 {:-me-key :id, :-me-val-hid 1007, :-parent-hid 1005},
;                              1007 {:-leaf-val 2, :-parent-hid 1006},
;                              1008 {:-me-key :color, :-me-val-hid 1009, :-parent-hid 1005},
;                              1009 {:-leaf-val :red, :-parent-hid 1008},
;                              1010 {:-ae-elem-hid 1011, :-ae-idx 1, :-parent-hid 1003},
;                              1011 {:-mn-data {:color 1014, :id 1012}, :-parent-hid 1010},
;                              1012 {:-me-key :id, :-me-val-hid 1013, :-parent-hid 1011},
;                              1013 {:-leaf-val 3, :-parent-hid 1012},
;                              1014 {:-me-key :color, :-me-val-hid 1015, :-parent-hid 1011},
;                              1015 {:-leaf-val :yellow, :-parent-hid 1014},
;                              1016 {:-ae-elem-hid 1017, :-ae-idx 2, :-parent-hid 1003},
;                              1017 {:-mn-data {:color 1020, :id 1018}, :-parent-hid 1016},
;                              1018 {:-me-key :id, :-me-val-hid 1019, :-parent-hid 1017},
;                              1019 {:-leaf-val 4, :-parent-hid 1018},
;                              1020 {:-me-key :color, :-me-val-hid 1021, :-parent-hid 1017},
;                              1021 {:-leaf-val :blue, :-parent-hid 1020}},
;         :idx-leaf           #{[:blue 1021] [:red 1009] [:yellow 1015] [2 1007] [3 1013] [4 1019]},
;         :idx-map-entry-kv   #{[:color :blue 1020] [:color :red 1008] [:color :yellow 1014]
;                               [:id 2 1006] [:id 3 1012] [:id 4 1018]},
;         :idx-map-entry-vk   #{[:blue :color 1020] [:red :color 1008] [:yellow :color 1014]
;                               [2 :id 1006] [3 :id 1012] [4 :id 1018]}})
;      (is= [1002 1004 1008 1009] (td/parent-path-hid  hid-red) )
;      (is= [:a 0 :color :red ] (td/parent-path-vals  hid-red))
;      (is= [:a 2 :color :blue] (td/parent-path-vals (only (td/index-find-leaf :blue))))
;      (is= [:a 1 :id 3] (td/parent-path-vals (only (td/index-find-leaf 3)))) )))
;
;(dotest
;  (td/with-tdb (td/new-tdb)
;    (td/hid-count-reset)
;    (let [data     {:a [{:id 2 :color :red}
;                        {:id 3 :color :yellow}
;                        {:id 4 :color :blue}
;                        {:id 5 :color :pink}
;                        {:id 6 :color :white}]
;                    :b {:c [{:ident 2 :flower :rose}
;                            {:ident 3 :flower :daisy}
;                            {:ident 4 :flower :tulip}
;                            ]}}
;          root-hid (td/add-edn data)]
;      (is= (t/map-entry :id 2) (td/hid->edn (only (td/index-find-mapentry (t/map-entry :id 2)))))
;      (is= (t/map-entry :ident 2) (td/hid->edn (only (td/index-find-mapentry (t/map-entry :ident 2)))))
;      (is= [:a 0 :id 2] (td/parent-path-vals (only (td/index-find-mapentry (t/map-entry :id 2)))))
;      (is= [:a 1 :color :yellow] (td/parent-path-vals (only (td/index-find-mapentry (t/map-entry :color :yellow)))))
;      (is= [:a 3 :color :pink] (td/parent-path-vals (only (td/index-find-mapentry (t/map-entry :color :pink)))))
;      (is= [:b :c 0 :flower :rose] (td/parent-path-vals (only (td/index-find-mapentry (t/map-entry :flower :rose)))))
;      (is= [:b :c 2 :ident 4] (td/parent-path-vals (only (td/index-find-mapentry (t/map-entry :ident 4)))))
;      )))
;
;
;(dotest
;  (let [skynet-widgets [{:basic-info   {:producer-code "Cyberdyne"}
;                         :widgets      [{:widget-code      "Model-101"
;                                         :widget-type-code "t800"}
;                                        {:widget-code      "Model-102"
;                                         :widget-type-code "t800"}
;                                        {:widget-code      "Model-201"
;                                         :widget-type-code "t1000"}]
;                         :widget-types [{:widget-type-code "t800"
;                                         :description      "Resistance Infiltrator"}
;                                        {:widget-type-code "t1000"
;                                         :description      "Mimetic polyalloy"}]}
;                        {:basic-info   {:producer-code "ACME"}
;                         :widgets      [{:widget-code      "Dynamite"
;                                         :widget-type-code "c40"}]
;                         :widget-types [{:widget-type-code "c40"
;                                         :description      "Boom!"}]}]
;        normalized     [["Cyberdyne" "Model-101" "Resistance Infiltrator"]
;                        ["Cyberdyne" "Model-102" "Resistance Infiltrator"]
;                        ["Cyberdyne" "Model-201" "Mimetic polyalloy"]
;                        ["ACME" "Dynamite" "Boom!"]]
;
;        query-edn      (quote (find {:where  [{:basic-info   {:producer-code ?}
;                                               :widgets      [{:widget-code      ?
;                                                               :widget-type-code ?}]
;                                               :widget-types [{:widget-type-code ?
;                                                               :description      ?}]}]
;                                     :return [producer-code widget-code description] ; #todo output using vals->map
;                                    }))
;        ]
;    ))
;
;
;(comment  ; old way
;  (dotest
;    (is= (td/val->idx-type-kw :a) :idx-kw)
;    (is= (td/val->idx-type-kw 99) :idx-num)
;    (is= (td/val->idx-type-kw "hi") :idx-str)
;
;    (is= (td/mapentry->idx-type-kw (t/map-entry 9 1)) :me-num-num)
;    (is= (td/mapentry->idx-type-kw (t/map-entry 9 :b)) :me-num-kw)
;    (is= (td/mapentry->idx-type-kw (t/map-entry 9 "hi")) :me-num-str)
;    (is= (td/mapentry->idx-type-kw (t/map-entry :a 1)) :me-kw-num)
;    (is= (td/mapentry->idx-type-kw (t/map-entry :a :b)) :me-kw-kw)
;    (is= (td/mapentry->idx-type-kw (t/map-entry :a "hi")) :me-kw-str)
;    (is= (td/mapentry->idx-type-kw (t/map-entry "bye" 1)) :me-str-num)
;    (is= (td/mapentry->idx-type-kw (t/map-entry "bye" :b)) :me-str-kw)
;    (is= (td/mapentry->idx-type-kw (t/map-entry "bye" "hi")) :me-str-str) ) )
;
;
;
;
;
;

