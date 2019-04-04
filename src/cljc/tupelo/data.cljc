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
    [schema.core :as s]
    [tupelo.schema :as tsk]))

(def customers ; #todo be able to process this data & delete unwise users
  [{:customer-id 1
    :plants      [{:plant-id  1
                   :employees [{:name "Alice" :age 35 :sex "F"}
                               {:name "Bob" :age 25 :sex "M"}]}
                  {:plant-id  2
                   :employees []}]}
   {:customer-id 2}])
(def age-of-wisdom 30)

;---------------------------------------------------------------------------------------------------
(def ^:dynamic ^:no-doc *tdb* nil)

(def HID
  "The Plumatic Schema type name for a pointer to a tdb node (abbrev. for Hex ID)"
  s/Int)

(s/defn hid->node ; :- Node
  "Returns the node corresponding to an HID"
  [hid :- HID]
  (grab hid (deref *tdb*)))

(defprotocol IDataNode
  (raw [this])
  (edn [this]))
(defprotocol INavNode
  (nav [this key]))

(s/defrecord MapNode ;Represents ths content of a Clojure map.
  ; content is a map from key to hid
  [content :- tsk/Map] ; #todo add parent
  IDataNode
  (raw [this]
    (validate map? content))
  (edn [this]
    (apply glue
      (forv [[k v-hid] (validate map? content)]
        {k (edn (hid->node v-hid))})))
  INavNode
  (nav [this key]
    (grab key (validate map? content))))

(s/defrecord VecNode ;Represents ths content of a Clojure vector (any sequential type coerced into a vector).
  ; content is a vector of hids
  [content :- tsk/Vec] ; #todo add parent
  IDataNode
  (raw [this]
    (validate vector? content))
  (edn [this]
    (forv [elem-hid (validate vector? content)]
      (edn (hid->node elem-hid))))
  INavNode
  (nav [this key]
    (if-not (= :* key)
      (nth (validate vector? content) key)
      (raw this))))

; Represents a Clojure primitive (non-collection) type,
; (i.e. number, string, keyword, symbol, character, etc)
(s/defrecord LeafNode
  ; content is a simple (non-collection) value
  [content :- s/Any] ; #todo add parent
  IDataNode
  (raw [this]
    (validate #(not (coll? %)) content))
  (edn [this]
    (validate #(not (coll? %)) content))
  )

(def DataNode
  "The Plumatic Schema type name for a MapNode VecNode LeafNode."
  (s/cond-pre MapNode VecNode LeafNode ))

(def HidRootSpec
  "The Plumatic Schema type name for the values accepted as starting points (roots) for a subtree path search."
  (s/conditional ; #todo why is this here?
    int? HID
    set? #{HID}
    :else [HID] ))

(def ^:no-doc hid-count-base 1000)
(def ^:no-doc hid-counter (atom hid-count-base))

(defn ^:no-doc hid-count-reset
  "Reset the hid-count to its initial value"
  [] (reset! hid-counter hid-count-base))

(s/defn ^:no-doc new-hid :- HID
  "Returns the next integer HID"
  [] (swap! hid-counter inc))

(s/defn hid? :- s/Bool
  "Returns true if the arg type is a legal HID value"
  [arg] (int? arg))

; HID & :hid are shorthand for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Node }
(defn new-tdb
  "Returns a new, empty db."
  []
  (sorted-map))

(defmacro with-tdb ; #todo swap names?
  [tdb-arg & forms]
  `(binding [*tdb* (atom ~tdb-arg)]
     ~@forms))

(s/defn set-node :- HID
  "Unconditionally sets the value of a node in the tdb"
  ([hid :- HID
    node  :- DataNode ]
    (swap! *tdb* glue {hid node})
    hid))

(s/defn edn->db :- HID
  [edn-val :- s/Any]
  (cond
    (map? edn-val) (set-node (new-hid)
                     (->MapNode
                       (apply glue
                         (forv [[k v] edn-val]
                           {k (edn->db v)}))))

    (or (set? edn-val) ; coerce sets to vectors
      (sequential? edn-val)) (set-node (new-hid)
                               (->VecNode
                                 (forv [elem edn-val]
                                   (edn->db elem))))

    (not (coll? edn-val)) (set-node (new-hid) (->LeafNode edn-val))

    :else (throw (ex-info "unknown value found" (vals->map edn-val)))))

(s/defn hid->edn :- s/Any
  "Returns EDN data for the subtree rooted at hid"
  [hid :- HID]
  (edn (hid->node hid)))

(s/defn hid-nav :- HID
  [hid :- HID
   path :- tsk/Vec]
  (if (empty? path)
    hid
    (let [node      (hid->node hid)
          key       (xfirst path)
          path-rest (xrest path)]
      (hid-nav (nav node key) path-rest))))
































