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



(dotest-focus
  (let [n (->MapNode {:a 1 :b 2})]
    (is= n (MapNode. {:a 1 :b 2}))
    (is= {:a 1 :b 2} (content n))

    (is= (spyx (load {:a 1 :b 2}))
      (MapNode. {:a (LeafNode. 1)
                 :b (LeafNode. 2)})))

  (let [n (->VecNode [1 2 3])]
    (is= n (VecNode. [1 2 3]))
    (is= [1 2 3] (content (spyx n)))
    (is= (spyx (load [1 2 3]))
      (VecNode. [(LeafNode. 1)
                 (LeafNode. 2)
                 (LeafNode. 3)])))

  (let [n (->LeafNode "hello")]
    (is= n (LeafNode. "hello"))
    (is= "hello" (content (spyx n))))


  )
