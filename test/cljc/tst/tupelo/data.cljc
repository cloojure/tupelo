;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.data
  #?(:clj (:refer-clojure :exclude [load ->VecNode]))
  #?(:clj (:require
            [tupelo.test :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx]]
            [tupelo.data :as td]
            [tupelo.lexical :as lex]
            [clojure.data.avl :as avl]
            [schema.core :as s]
            ))
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

(dotest
  (println "********* running tupelo.data #1 tests ********* ")
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
  (println "********* running tupelo.data #2 tests ********* ")
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
  (println "********* running tupelo.data #3 tests ********* ")
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

; (deftest ^:test-refresh/focus t-113
; (deftest ^:focus t-113
  (dotest-focus
    (println "********* running tupelo.data #4 tests ********* ")
    (td/with-tdb (td/new-tdb)
      (let [data         [{:a 1 :b :first}
                          {:a 2 :b :second}
                          {:a 3 :b :third}
                          {:a 4 :b "fourth"}
                          {:a 5 :b "fifth"}
                          {:a 1 :b 101}
                          {:a 1 :b 102} ]
            root-hid     (td/add-edn data)
            hids-match   (td/index-find-val 1)
            hids-parents (mapv td/hid->parent-hid hids-match)
            edn-match    (mapv td/hid->edn hids-match)
            edn-parent   (mapv td/hid->edn hids-parents)]
        (is= edn-match [1 1 1])
        (is= edn-parent [{:a 1, :b :first}
                         {:a 1, :b 101}
                         {:a 1, :b 102}])))
    (td/with-tdb (td/new-tdb)
      (let [data         [{:a 1 :x :first}
                          {:a 2 :x :second}
                          {:a 3 :x :third}
                          {:b 1 :x 101}
                          {:b 2 :x 102}
                          {:c 1 :x 301}
                          {:c 2 :x 302} ]
            root-hid     (td/add-edn data)
            hid-match (t/only (td/index-find-solomap {:a 1}))
            edn-match (td/hid->edn hid-match) ]
        (is= edn-match {:a 1 :x :first} ) ))

    (newline) (println "===================================================================================================")
    (td/with-tdb (td/new-tdb)
      (let [data      [{:a 1 :b 1 :c 1}
                       {:a 1 :b 2 :c 2}
                       {:a 1 :b 1 :c 3}
                       {:a 2 :b 2 :c 4}
                       {:a 2 :b 1 :c 5}
                       {:a 2 :b 2 :c 6}]
            root-hid  (td/add-edn data)
            hid-match (t/only (td/index-find-submap {:a 1 :b 2}))
            edn-match (td/hid->edn hid-match)]
        (is= edn-match {:a 1 :b 2 :c 2})
       ;(t/spy-pretty (deref td/*tdb*))
        (let [tgt        {:a 1 :b 1}
              match-hids (td/index-find-submap tgt)
              match-edns (mapv td/hid->edn match-hids)]
          (is-set= match-edns
            [{:a 1, :b 1, :c 1}
             {:a 1, :b 1, :c 3}]))))
    (newline)
    (println "---------------------------------------------------------------------------------------------------")
    )

















