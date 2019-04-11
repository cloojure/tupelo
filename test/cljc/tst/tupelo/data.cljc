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
            [tupelo.data :as data]
            [tupelo.lexical :as lex]
            [clojure.data.avl :as avl]
            [schema.core :as s]
            ))
  #?(:cljs (:require
             [tupelo.test-cljs :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]
              :include-macros true]
             [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true ]
             [tupelo.data :as data]
             [tupelo.lexical :as lex]
             [clojure.data.avl :as avl]
             [schema.core :as s]
             ))
  )
; #todo fix for cljs
#?(:cljs (enable-console-print!))

(dotest
  (data/with-tdb (data/new-tdb)
    (let [edn-0 {:a 1 :b 2}
          root-hid (data/load-edn edn-0) ]
      (is= edn-0 (data/hid->edn root-hid)) ))

  (data/with-tdb (data/new-tdb)
    (let [edn-0  [1 2 3]
          root-hid (data/load-edn edn-0) ]
      (is= edn-0 (data/hid->edn root-hid)) ))

  (data/with-tdb (data/new-tdb)
    (let [edn-0  "hello"
          root-hid (data/load-edn edn-0) ]
      (is= edn-0 (data/hid->edn root-hid)) ))

  (data/with-tdb (data/new-tdb)
    (let [data-1   {:a [{:b 2}
                        {:c 3}
                        {:d 4}]
                    :e {:f 6}
                    :g :green
                    :h "hotel"
                    :i 1}
          root-hid (data/load-edn data-1)]
      (is= data-1 (data/hid->edn root-hid)))) )

(dotest
  (data/with-tdb (data/new-tdb)
    (let [edn-0      #{1 2 3}
          root-hid   (data/load-edn edn-0)
          edn-result (data/hid->edn root-hid)]
      (is (set? edn-result)) ; ***** Sets are coerced to vectors! *****
      (is-set= [1 2 3] edn-result)))
  (data/with-tdb (data/new-tdb)
    (let [edn-0    #{:a 1 :b 2}
          root-hid (data/load-edn edn-0)]
      (is= edn-0 (data/hid->edn root-hid))))
  (data/with-tdb (data/new-tdb)
    (let [edn-0    {:a 1 :b #{1 2 3}}
          root-hid (data/load-edn edn-0)]
      (is= edn-0 (data/hid->edn root-hid)))))

(dotest
  (data/with-tdb (data/new-tdb)
    (let [data     {:a [{:b 2}
                        {:c 3}
                        {:d 4}]
                    :e {:f 6}
                    :g :green
                    :h "hotel"
                    :i 1}
          root-hid (data/load-edn data)]
      (is= (data/hid->edn (data/hid-nav root-hid [:a])) [{:b 2} {:c 3} {:d 4}])
      (is= (data/hid->edn (data/hid-nav root-hid [:a 0])) {:b 2})
      (is= (data/hid->edn (data/hid-nav root-hid [:a 2])) {:d 4})
      (is= (data/hid->edn (data/hid-nav root-hid [:a 2 :d])) 4)
      (is= (data/hid->edn (data/hid-nav root-hid [:e])) {:f 6})
      (is= (data/hid->edn (data/hid-nav root-hid [:e :f])) 6)
      (is= (data/hid->edn (data/hid-nav root-hid [:h])) "hotel")
      (is= (data/hid->edn (data/hid-nav root-hid [:i])) 1)
      (let [kid-hids     (data/hid-nav root-hid [:a :*])
            parent-hids  (mapv data/hid->parent kid-hids)
            parent-hid   (t/xfirst parent-hids)
            parent-hid-2 (data/hid->parent parent-hid)
            ]
        (is= (mapv data/hid->edn kid-hids)
          [{:b 2} {:c 3} {:d 4}])
        (is (apply = parent-hids))
        (is= (data/hid->edn parent-hid)
          [{:b 2} {:c 3} {:d 4}])
        (is= (data/hid->edn parent-hid-2) data))
      (let [four-hid          (data/hid-nav root-hid [:a 2 :d])
            four-hid-parent-3 (-> four-hid
                                data/hid->parent
                                data/hid->parent
                                data/hid->parent)]
        (is= 4 (data/hid->edn four-hid))
        (is= data (data/hid->edn four-hid-parent-3)))) ))

(dotest
  (newline) (println "===================================================================================================")
  (data/with-tdb (data/new-tdb)
    (let [data     [{:a 1
                     :b :first}
                    {:a 2
                     :b :second}
                    {:a 3
                     :b :third}]
          root-hid (data/load-edn data)]

      (newline) (println "---------------------------------------------------------------------------------------------------")
      )
    )
  )










