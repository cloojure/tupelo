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
    [tupelo.string :as ts]
    )
  (:import [tupelo.data MapNode VecNode LeafNode]))

(def data-1
  {:a [{:b 2}
       {:c 3}
       {:d 4}]
   :e {:f 6}
   :g :green
   :h "hotel"
   :i 1})

(def datatree-1
  (MapNode. {:a (VecNode.
                  (MapNode. {:b (LeafNode. 2)})
                  (MapNode. {:c (LeafNode. 3)})
                  (MapNode. {:d (LeafNode. 4)}))
             :e (MapNode. {:f (LeafNode. 6)})
             :g (LeafNode. :green)
             :h (LeafNode. "hotel")
             :i 1}))

(dotest-focus
  (let [edn-0  {:a 1 :b 2}
        node-0 (MapNode. {:a (LeafNode. 1)
                          :b (LeafNode. 2)})

        node-1 (->MapNode edn-0)
        node-2 (MapNode. edn-0)

        node-9 (edn->datatree edn-0)
        edn-9  (datatree->edn node-9)]

    (is= node-1 node-2)

    (is= edn-0 (raw node-1) (raw node-2))
    (is= node-0 node-9)
    (is= edn-0 edn-9))

  (let [edn-0  [1 2 3]
        node-0 (VecNode. [(LeafNode. 1)
                          (LeafNode. 2)
                          (LeafNode. 3)])

        node-1 (->VecNode edn-0)
        node-2 (VecNode. edn-0)
        node-9 (edn->datatree edn-0)
        edn-9  (datatree->edn node-9)]
    (is= node-1 node-2)
    (is= edn-0 (raw node-1) (raw node-2))
    (is= node-0 node-9)
    (is= edn-0 edn-9))

  (let [edn-0  "hello"
        node-1 (->LeafNode edn-0)
        node-2 (LeafNode. edn-0)
        node-3 (edn->datatree edn-0)
        edn-9  (datatree->edn node-1)]
    (is= node-1 node-2 node-3)
    (is= edn-0 (edn node-1) (raw node-1))
    (is= edn-0 edn-9))

  )
