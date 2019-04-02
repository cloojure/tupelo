;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.data
  (:use tupelo.core)
  (:refer-clojure :exclude [load ->VecNode])
  (:require
    [schema.core :as s] ) )

(def customers ; #todo be able to process this data & delete unwise users
  [{:customer-id 1
    :plants      [{:plant-id  1
                   :employees [{:name "Alice" :age 35 :sex "F"}
                               {:name "Bob" :age 25 :sex "M"}]}
                  {:plant-id  2
                   :employees []}]}
   {:customer-id 2}])
(def age-of-wisdom 30)

(defprotocol DataNode
  (raw [this])
  (edn [this]))

(defrecord MapNode ;Represents ths content of a Clojure map.
  [content] ; #todo add parent
  DataNode
  (raw [this]
    (validate map? content))
  (edn [this]
    (apply glue
      (forv [[k v] content]
        {k (edn v)}))))

(defrecord VecNode ;Represents ths content of a Clojure vector (any sequential type coerced into a vector).
  [content] ; #todo add parent
  DataNode
  (raw [this]
    (validate vector? content))
  (edn [this]
    (forv [elem content]
      (edn elem))))

; Represents a Clojure primitive (non-collection) type,
; (i.e. number, string, keyword, symbol, character, etc)
(defrecord LeafNode
  [content] ; #todo add parent
  DataNode
  (raw [this]
    (validate #(not (coll? %)) content))
  (edn [this]
    (validate #(not (coll? %)) content)))

(defn edn->datatree ; :- DataNode #todo
  [edn-val] ;  :- s/Any
  (cond
    (map? edn-val) (->MapNode (apply glue
                                (forv [[k v] edn-val]
                                  {k (edn->datatree v)})))
    (or (set? edn-val) ; coerce sets to vectors
      (sequential? edn-val)) (->VecNode (into []
                                          (forv [elem edn-val]
                                            (edn->datatree elem))))
    :else (->LeafNode edn-val)))

(defn datatree->edn ;  :- s/Any  ; #todo
  [datatree] ; :- DataNode  ; #todo
  (nl)
  (println :datatree->edn datatree)
  (edn datatree)
  )
