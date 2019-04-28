;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.data
  (:use tupelo.core)
  #?(:clj (:refer-clojure :exclude [load ->VecNode]))
  #?(:clj (:require
            [tupelo.test :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty unlazy]]
            [tupelo.data :as td]
            [tupelo.data.index :as tdi]
            [tupelo.lexical :as lex]
            [clojure.data.avl :as avl]
            [schema.core :as s]
            [clojure.walk :as walk]))
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
; #todo fix dotest-focus so it works again!

#?(:cljs (enable-console-print!))

(dotest-focus
  (let [ss123 (t/it-> (tdi/->sorted-set-avl)
                (conj it [1 :a])
                (conj it [3 :a])
                (conj it [2 :a]))
        ss13  (disj ss123 [2 :a])]
    (is= #{[1 :a] [2 :a] [3 :a]} ss123)
    (is= [[1 :a] [2 :a] [3 :a]] (vec ss123))
    (is= #{[1 :a] [3 :a]} ss13))

  (td/with-tdb (td/new-tdb)
    (td/hid-count-reset)
    (is= @td/*tdb* {:idx-hid {}, :idx-leaf #{}, :idx-me #{}})
    (let [edn-val  5
          root-hid (td/add-edn edn-val)]
      (is= (unlazy @td/*tdb*)
        {:idx-hid  {1001 {:-leaf-val 5, :-parent-hid nil}},
         :idx-leaf #{[5 1001]},
         :idx-me   #{}} ))
    (let [edn-val  {:a 1}
          root-hid (td/add-edn edn-val)]
      (is= (unlazy @td/*tdb*) ; coerce all from record to plain map for comparison
        {:idx-hid  {1001 {:-leaf-val 5, :-parent-hid nil},
                    1002 {:-mn-data {:a 1003}, :-parent-hid nil},
                    1003 {:-me-hid 1004, :-me-key :a, :-parent-hid 1002},
                    1004 {:-leaf-val 1, :-parent-hid 1003}},
         :idx-leaf #{[1 1004] [5 1001]},
         :idx-me   #{[1 :a 1003]}} ))
    (let [edn-val  [7 8]
          root-hid (td/add-edn edn-val)]
      (is= (unlazy @td/*tdb*) ; coerce all from record to plain map for comparison
        {:idx-hid  {1001 {:-leaf-val 5, :-parent-hid nil},
                    1002 {:-mn-data {:a 1003}, :-parent-hid nil},
                    1003 {:-me-hid 1004, :-me-key :a, :-parent-hid 1002},
                    1004 {:-leaf-val 1, :-parent-hid 1003},
                    1005 {:-an-data {0 1006, 1 1008}, :-parent-hid nil},
                    1006 {:-ae-hid 1007, :-ae-idx 0, :-parent-hid 1005},
                    1007 {:-leaf-val 7, :-parent-hid 1006},
                    1008 {:-ae-hid 1009, :-ae-idx 1, :-parent-hid 1005},
                    1009 {:-leaf-val 8, :-parent-hid 1008}},
         :idx-leaf #{[1 1004] [5 1001] [7 1007] [8 1009]},
         :idx-me   #{[1 :a 1003]}}))

    )
  )

(comment

(dotest
  (td/with-tdb (td/new-tdb)
    (let [edn-0    {:a 1 :b 2}
          root-hid (td/add-edn edn-0)]
      (is= edn-0 (td/hid->edn root-hid))))

  (td/with-tdb (td/new-tdb)
    (let [edn-0    [1 2 3]
          root-hid (td/add-edn edn-0)]
      (is= edn-0 (td/hid->edn root-hid))))

  (td/with-tdb (td/new-tdb)
    (let [edn-0    "hello"
          root-hid (td/add-edn edn-0)]
      (is= edn-0 (td/hid->edn root-hid))))

  (td/with-tdb (td/new-tdb)
    (let [data-1   {:a [{:b 2}
                        {:c 3}
                        {:d 4}]
                    :e {:f 6}
                    :g :green
                    :h "hotel"
                    :i 1}
          root-hid (td/add-edn data-1)]
      (is= data-1 (td/hid->edn root-hid)))))

(dotest
  (td/with-tdb (td/new-tdb)
    (let [edn-0      #{1 2 3}
          root-hid   (td/add-edn edn-0)
          edn-result (td/hid->edn root-hid)]
      (is (set? edn-result)) ; ***** Sets are coerced to vectors! *****
      (is-set= [1 2 3] edn-result)))
  (td/with-tdb (td/new-tdb)
    (let [edn-0    #{:a 1 :b 2}
          root-hid (td/add-edn edn-0)]
      (is= edn-0 (td/hid->edn root-hid))))
  (td/with-tdb (td/new-tdb)
    (let [edn-0    {:a 1 :b #{1 2 3}}
          root-hid (td/add-edn edn-0)]
      (is= edn-0 (td/hid->edn root-hid)))))

(dotest
  (td/with-tdb (td/new-tdb)
    (let [data     {:a [{:b 2}
                        {:c 3}
                        {:d 4}]
                    :e {:f 6}
                    :g :green
                    :h "hotel"
                    :i 1}
          root-hid (td/add-edn data)]
      (is= (td/hid->edn (td/hid-nav root-hid [:a])) [{:b 2} {:c 3} {:d 4}])
      (is= (td/hid->edn (td/hid-nav root-hid [:a 0])) {:b 2})
      (is= (td/hid->edn (td/hid-nav root-hid [:a 2])) {:d 4})
      (is= (td/hid->edn (td/hid-nav root-hid [:a 2 :d])) 4)
      (is= (td/hid->edn (td/hid-nav root-hid [:e])) {:f 6})
      (is= (td/hid->edn (td/hid-nav root-hid [:e :f])) 6)
      (is= (td/hid->edn (td/hid-nav root-hid [:h])) "hotel")
      (is= (td/hid->edn (td/hid-nav root-hid [:i])) 1)
      (let [kid-hids     (td/hid-nav root-hid [:a :*])
            parent-hids  (mapv td/hid->parent-hid kid-hids)
            parent-hid   (t/xfirst parent-hids)
            parent-hid-2 (td/hid->parent-hid parent-hid)
            ]
        (is= (mapv td/hid->edn kid-hids)
          [{:b 2} {:c 3} {:d 4}])
        (is (apply = parent-hids))
        (is= (td/hid->edn parent-hid)
          [{:b 2} {:c 3} {:d 4}])
        (is= (td/hid->edn parent-hid-2) data))
      (let [four-hid          (td/hid-nav root-hid [:a 2 :d])
            four-hid-parent-3 (-> four-hid
                                td/hid->parent-hid
                                td/hid->parent-hid
                                td/hid->parent-hid)]
        (is= 4 (td/hid->edn four-hid))
        (is= data (td/hid->edn four-hid-parent-3))))))

(dotest
  (is= (td/val->idx-type-kw :a) :idx-kw)
  (is= (td/val->idx-type-kw 99) :idx-num)
  (is= (td/val->idx-type-kw "hi") :idx-str)

  (is= (td/mapentry->idx-type-kw (t/map-entry 9 1)) :me-num-num)
  (is= (td/mapentry->idx-type-kw (t/map-entry 9 :b)) :me-num-kw)
  (is= (td/mapentry->idx-type-kw (t/map-entry 9 "hi")) :me-num-str)
  (is= (td/mapentry->idx-type-kw (t/map-entry :a 1)) :me-kw-num)
  (is= (td/mapentry->idx-type-kw (t/map-entry :a :b)) :me-kw-kw)
  (is= (td/mapentry->idx-type-kw (t/map-entry :a "hi")) :me-kw-str)
  (is= (td/mapentry->idx-type-kw (t/map-entry "bye" 1)) :me-str-num)
  (is= (td/mapentry->idx-type-kw (t/map-entry "bye" :b)) :me-str-kw)
  (is= (td/mapentry->idx-type-kw (t/map-entry "bye" "hi")) :me-str-str) )

(dotest-focus
  (td/with-tdb (td/new-tdb)
    (let [data         [{:a 1 :b :first}
                        {:a 2 :b :second}
                        {:a 3 :b :third}
                        {:a 4 :b "fourth"}
                        {:a 5 :b "fifth"}
                        {:a 1 :b 101}
                        {:a 1 :b 102}]
          root-hid     (td/add-edn data)
          hids-match   (td/index-find-val 1)
          hids-parents (mapv td/hid->parent-hid hids-match)
          edn-match    (mapv td/hid->edn hids-match)
          edn-parent   (mapv td/hid->edn hids-parents)]
      (is= edn-match [1 1 1])
      (is= edn-parent [{:a 1, :b :first}
                       {:a 1, :b 101}
                       {:a 1, :b 102}])
      (is= {:a 1 :b 101} (td/hid->edn (only (td/index-find-mapentry (map-entry :b 101)))))
      (is= {:a 2 :b :second} (td/hid->edn (only (td/index-find-mapentry (map-entry :b :second)))))
      (is= {:a 3 :b :third} (td/hid->edn (only (td/index-find-mapentry (map-entry :a 3)))))))
  (td/with-tdb (td/new-tdb)
    (let [data      [{:a 1 :x :first}
                     {:a 2 :x :second}
                     {:a 3 :x :third}
                     {:b 1 :x 101}
                     {:b 2 :x 102}
                     {:c 1 :x 301}
                     {:c 2 :x 302}]
          root-hid  (td/add-edn data)
          hid-match (t/only (td/index-find-mapentry
                              (->map-entry {:a 1})))
          edn-match (td/hid->edn hid-match)]
      (is= edn-match {:a 1 :x :first})))

  (td/with-tdb (td/new-tdb)
    (let [data     [{:a 1 :b 1 :c 1}
                    {:a 1 :b 2 :c 2}
                    {:a 1 :b 1 :c 3}
                    {:a 2 :b 2 :c 4}
                    {:a 2 :b 1 :c 5}
                    {:a 2 :b 2 :c 6}]
          root-hid (td/add-edn data)]
      (let [hid (t/only (td/index-find-submap {:a 1 :b 2}))
            edn (td/hid->edn hid)]
        (is= edn {:a 1 :b 2 :c 2}))
      (let [hids (td/index-find-submap {:a 1 :b 1})
            edns (mapv td/hid->edn hids)]
        (is-set= edns [{:a 1, :b 1, :c 1}
                       {:a 1, :b 1, :c 3}]))))
  (newline) (println "===================================================================================================")
  (td/with-tdb (td/new-tdb)
    (let [data     [{:a 1 :b 1 :c 1}
                    {:a 1 :b 2 :c 2}
                    {:a 1 :b 1 :c 3}
                    {:a 2 :b 2 :c 4}
                    {:a 2 :b 1 :c 5}
                    {:a 2 :b 2 :c 6}]
          root-hid (td/add-edn data)]
      ;(t/spy-pretty (deref td/*tdb*))
      (let [edns (mapv td/hid->edn
                   (td/index-find-mapentry (map-entry :a 1)))]
        (is= edns
          [{:a 1, :b 1, :c 1}
           {:a 1, :b 2, :c 2}
           {:a 1, :b 1, :c 3}]))
      (let [edns (mapv td/hid->edn
                   (td/index-find-mapentry (map-entry :a 2)))]
        (is= edns
          [{:a 2, :b 2, :c 4}
           {:a 2, :b 1, :c 5}
           {:a 2, :b 2, :c 6}]))
      (let [edns (mapv td/hid->edn
                   (td/index-find-mapentry (map-entry :b 1)))]
        (is= edns
          [{:a 1, :b 1, :c 1}
           {:a 1, :b 1, :c 3}
           {:a 2, :b 1, :c 5}]))
      (let [edns (mapv td/hid->edn
                   (td/index-find-mapentry (map-entry :c 6)))]
        (is= edns
          [{:a 2, :b 2, :c 6}]))))
  (newline)
  (println "---------------------------------------------------------------------------------------------------")

  )











  )
