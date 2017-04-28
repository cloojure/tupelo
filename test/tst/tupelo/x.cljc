;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x
  "Experimental new code"
  (:use clojure.test tupelo.test tupelo.x)
  (:require
    [clojure.string :as str]
    [clj-uuid :as uuid]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
  )
  (:import
    [java.nio ByteBuffer]
    [java.util UUID ]
  ))
(t/refer-tupelo)

(defrecord Node [attrs children])    ; ns -> tupelo.datatree
(defrecord Leaf [attrs value])    ; ns -> tupelo.datatree

(def DataTreeMember
  "Either an internal Node or a Leaf"
  (s/either Node Leaf))

(s/defn node  :- Node
  [attrs :- tsk/Map
   children :- [DataTreeMember]]
  (let [uuid (tm/sha-uuid)
        id4  (clip-str 4 uuid)
        attrs (assoc attrs :uuid uuid  :id4 id4) ]
    (->Node attrs children)))

(s/defn leaf  :- Leaf
  [attrs :- tsk/Map
   value :- s/Any]
  (let [uuid (tm/sha-uuid)
        id4  (clip-str 4 uuid)
        attrs (assoc attrs :uuid uuid  :id4 id4) ]
    (->Leaf attrs value)))

(nl) (def x (spyx-pretty (leaf {:tag :char :color :red} "x")))
(nl) (def y (spyx-pretty (leaf {:tag :char :color :red} "y")))
(nl) (def z (spyx-pretty (leaf {:tag :char :color :red} "z")))
(nl) (def a (spyx-pretty (node {:tag :root :color :white} [x y z])))


