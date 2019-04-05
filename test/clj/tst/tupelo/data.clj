;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.data
  (:use tupelo.data tupelo.core tupelo.test)
  (:refer-clojure :exclude [load ->VecNode])
  (:require
    [tupelo.string :as ts] )
  )


(dotest
  (with-tdb (new-tdb)
    (let [edn-0 {:a 1 :b 2}
          root-hid (load-edn edn-0) ]
      (is= edn-0 (hid->edn root-hid)) ))

  (with-tdb (new-tdb)
    (let [edn-0  [1 2 3]
          root-hid (load-edn edn-0) ]
      (is= edn-0 (hid->edn root-hid)) ))

  (with-tdb (new-tdb)
    (let [edn-0  "hello"
          root-hid (load-edn edn-0) ]
      (is= edn-0 (hid->edn root-hid)) ))

  (with-tdb (new-tdb)
    (let [data-1   {:a [{:b 2}
                        {:c 3}
                        {:d 4}]
                    :e {:f 6}
                    :g :green
                    :h "hotel"
                    :i 1}
          root-hid (load-edn data-1)]
      (is= data-1 (hid->edn root-hid))))

  (with-tdb (new-tdb)
    (let [edn-0      #{1 2 3}
          root-hid   (load-edn edn-0)
          edn-result (hid->edn root-hid)]
      (is (vector? edn-result)) ; ***** Sets are coerced to vectors! *****
      (is-set= [1 2 3] edn-result))))

(dotest-focus
  (nl) (println "===================================================================================================")
  (with-tdb (new-tdb)
    (let [data     {:a [{:b 2}
                        {:c 3}
                        {:d 4}]
                    :e {:f 6}
                    :g :green
                    :h "hotel"
                    :i 1}
          root-hid (load-edn data)]
      (is= (hid->edn (hid-nav root-hid [:a])) [{:b 2} {:c 3} {:d 4}])
      (is= (hid->edn (hid-nav root-hid [:a 0])) {:b 2})
      (is= (hid->edn (hid-nav root-hid [:a 2])) {:d 4})
      (is= (hid->edn (hid-nav root-hid [:a 2 :d])) 4)
      (is= (hid->edn (hid-nav root-hid [:e])) {:f 6})
      (is= (hid->edn (hid-nav root-hid [:e :f])) 6)
      (is= (hid->edn (hid-nav root-hid [:h])) "hotel")
      (is= (hid->edn (hid-nav root-hid [:i])) 1)
      (let [kid-hids     (hid-nav root-hid [:a :*])
            parent-hids  (mapv hid->parent kid-hids)
            parent-hid   (xfirst parent-hids)
            parent-hid-2 (hid->parent parent-hid)
            ]
        (is= (mapv hid->edn kid-hids)
          [{:b 2} {:c 3} {:d 4}])
        (is (apply = parent-hids))
        (is= (hid->edn parent-hid)
          [{:b 2} {:c 3} {:d 4}])
        (is= (hid->edn parent-hid-2) data))
      (let [four-hid          (hid-nav root-hid [:a 2 :d])
            four-hid-parent-3 (-> four-hid
                                hid->parent
                                hid->parent
                                hid->parent)]
        (is= 4 (spyx (hid->edn four-hid)))
        (is= data (spyx (hid->edn four-hid-parent-3)))))


    (nl) (println "---------------------------------------------------------------------------------------------------")
    ))

