;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.data
  (:refer-clojure :exclude [load ->VecNode])
  (:use tupelo.core) ; #todo remove for cljs
  #?(:clj (:require
            [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab glue map-entry indexed
                                       forv vals->map fetch-in let-spy
                                       ]]
            [tupelo.schema :as tsk]
            [tupelo.data.index :as tdi]
            [clojure.set :as set]
            [schema.core :as s]
            ))
  #?(:cljs (:require
             [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab]] ; #todo :include-macros true
             [tupelo.schema :as tsk]
             [tupelo.data.index :as tdi]
             [clojure.set :as set]
             [schema.core :as s]
             ))
  )

; #todo add indexes
; #todo add sets (primative only or HID) => map with same key/value
; #todo copy destruct syntax for search

#?(:cljs (enable-console-print!))

; #todo Tupelo Data Language (TDL)

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
; HID & :hid are shorthand for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Node }
(def HID
  "The Plumatic Schema type name for a pointer to a tdb node (abbrev. for Hex ID)"
  s/Int)

(def HidRootSpec
  "The Plumatic Schema type name for the values accepted as starting points (roots) for a subtree path search."
  (s/conditional ; #todo why is this here?
    int? HID
    set? #{HID}
    :else [HID]))

;-----------------------------------------------------------------------------
(declare hid->node)

(defprotocol IParentable
  (parent-hid [this]))

(defprotocol IDataNode
  (content [this])
  (edn [this]))

(defprotocol INavNode
  (nav [this key]))

(defprotocol IMapEntryNode
  (me-key [this])
  (me-val-hid [this]))

(defprotocol IArrayEntryNode
  (ae-idx [this])
  (ae-elem-hid [this]))

;-----------------------------------------------------------------------------
; #todo add validation check that MapNode keys match all MapEntryNode keys
(s/defrecord MapNode ; Represents ths content of a Clojure map.
  ; a map from key to hid
  [-parent-hid :- (s/maybe HID)
   -mn-data :- tsk/Map] ; a map from key to hid
  IParentable
    (parent-hid [this] (s/validate (s/maybe HID) -parent-hid))
  IDataNode
    (content [this] (t/validate map? -mn-data))
    (edn [this]
      (apply t/glue
        (t/forv [[-key- men-hid] (t/validate map? -mn-data)]
          (edn (hid->node men-hid)))))
  INavNode
    (nav [this key-arg]
      (let [hid-men (t/grab key-arg -mn-data)
            hid-val (grab :-me-val-hid (hid->node hid-men))]
        hid-val)))

(s/defrecord MapEntryNode
  [-parent-hid :- HID
   -me-key     :- s/Any
   -me-val-hid :- HID]
  IParentable
    (parent-hid [this] (s/validate HID -parent-hid))
  IDataNode
  (content [this]
    (t/vals->map -parent-hid -me-key -me-val-hid)) ; #todo remove this?
  (edn [this]
    (t/map-entry -me-key (edn (hid->node -me-val-hid))))
  IMapEntryNode
    (me-key [this]  -me-key)
    (me-val-hid [this]  -me-val-hid) )

; #todo add validation check that ArrayNode keys match all ArrayEntryNode keys
(s/defrecord ArrayNode ; Represents ths content of a Clojure vector (any sequential type coerced into a vector).
  ; stored is a vector of hids
  [-parent-hid :- (s/maybe HID)
   -an-data :- tsk/Map] ; a map from index to hid
  IParentable
    (parent-hid [this] (s/validate (s/maybe HID) -parent-hid))
  IDataNode
    (content [this] (t/validate map? -an-data))
    (edn [this]
      (t/forv [[-idx- aen-hid] (t/validate map? -an-data)]
        (edn (hid->node aen-hid))))
  INavNode
    (nav [this key-arg]
      (if (= :* key-arg)
        (vals (t/->sorted-map -an-data))
        (let [hid-aen (t/grab key-arg -an-data)
              hid-val (grab :-ae-elem-hid (hid->node hid-aen))]
          hid-val) )))

(s/defrecord ArrayEntryNode
  [-parent-hid :- HID
   -ae-idx :- s/Any
   -ae-elem-hid :- HID]
  IParentable
    (parent-hid [this] (s/validate HID -parent-hid))
  IDataNode
  (content [this]
    (t/vals->map -parent-hid -ae-idx -ae-elem-hid)) ; #todo remove this?
  (edn [this]
     (edn (hid->node -ae-elem-hid)))
  IArrayEntryNode
    (ae-idx [this]  -ae-idx)
    (ae-elem-hid [this]  -ae-elem-hid))

; #todo need to enforce set uniqueness under mutation
(s/defrecord SetNode ; Represents ths content of a Clojure set
  ; a map from key to hid
  [-parent-hid :- (s/maybe HID)
   -sn-data :- tsk/Map]
  IParentable
    (parent-hid [this] (s/validate (s/maybe HID) -parent-hid))
  IDataNode
    (content [this]
      (t/validate map? -sn-data))
    (edn [this]
      (set
        (t/forv [[-key- elem-hid] (t/validate map? -sn-data)]
          (edn (hid->node elem-hid)))))
  INavNode
    (nav [this key]
      (t/grab key (t/validate set? -sn-data))))

; Represents a Clojure primitive (non-collection) type,
; (i.e. number, string, keyword, symbol, character, etc)
(s/defrecord LeafNode
  ; stored is a simple (non-collection) value
  [-parent-hid :- (s/maybe HID)
   -leaf-val :- s/Any]
  IParentable
    (parent-hid [this] (s/validate (s/maybe HID) -parent-hid))
  IDataNode
    (content [this]
      (t/validate #(not (coll? %)) -leaf-val))
    (edn [this]
      (t/validate #(not (coll? %)) -leaf-val)))

(def DataNode
  "The Plumatic Schema type name for a MapNode ArrayNode LeafNode."
  (s/cond-pre MapNode MapEntryNode ArrayNode ArrayEntryNode SetNode LeafNode))

;-----------------------------------------------------------------------------
(def ^:dynamic ^:no-doc *tdb* nil)

(defmacro with-tdb ; #todo swap names?
  [tdb-arg & forms]
  `(binding [*tdb* (atom ~tdb-arg)]
     ~@forms))

(defn new-tdb
  "Returns a new, empty db."
  []
  {:idx-hid (sorted-map)
   :idx-leaf (tdi/->sorted-set-avl)
   :idx-map-entry (tdi/->sorted-set-avl)
   :idx-array-entry (tdi/->sorted-set-avl) })

(s/defn hid->node :- DataNode
  "Returns the node corresponding to an HID"
  [hid :- HID]
  (fetch-in (deref *tdb*) [:idx-hid hid]))

(s/defn set-node! :- HID
  "Unconditionally sets the value of a node in the tdb"
  ([hid :- HID
    node :- DataNode]
    (swap! *tdb* assoc-in [:idx-hid hid] node)
    hid))

; #todo => tupelo.core
(s/defn mapentry->kv :- tsk/Pair ; #todo need test
  [mapentry :- tsk/MapEntry]
  [(key mapentry) (val mapentry)])

; #todo => tupelo.core
(s/defn solomap->kv :- tsk/Pair ; #todo need test
  [solo-map :- tsk/Map]
  (let [map-seq (seq solo-map)
        >>      (when-not #(= 1 (count map-seq))
                  (throw (ex-info "solo-map must be of length=1 " (t/vals->map solo-map))))]
    (mapentry->kv (only map-seq))))

(do ; keep these two in sync
  (s/defn leaf-val? :- s/Bool
    "Returns true iff a value is of leaf type (number, string, keyword)"
    [arg :- s/Any] (or (number? arg) (string? arg) (keyword? arg)))
  (def LeafType (s/cond-pre s/Num s/Str s/Keyword))) ; instant, uuid, Time ID (TID) (as strings?)

(comment  ; unused since added cc-cmp
  (s/defn ^:no-doc type-short
    [arg]
    (cond
      (number? arg) "num"
      (string? arg) "str"
      (keyword? arg) "kw"
      ;(symbol? arg) "sym"  ; #todo allow this?
      :else (throw (ex-info "invalid type found" (t/vals->map arg)))))

  (s/defn ^:no-doc val->idx-type-kw
    [leaf-val :- LeafType]
    (keyword (str "idx-" (type-short leaf-val))))

  (s/defn ^:no-doc mapentry->idx-type-kw
    [me :- tsk/MapEntry]
    (let [me-key-type (type-short (key me))
          me-val-type (type-short (val me))]
      (keyword (str "me-" me-key-type \- me-val-type))))
)

(s/defn update-index-leaf!
  [leaf-val :- LeafType
   hid-val :- HID]
  (swap! *tdb* (fn [tdb-map]
                 (update tdb-map :idx-leaf ; #todo make verify like fetch-in
                   (fn [index-avl-set]
                     (conj index-avl-set [leaf-val hid-val])))))
  nil)

(s/defn update-index-mapentry!
  [me-val :- LeafType
   me-key
   me-hid :- HID]
  (swap! *tdb* (fn [tdb-map]
                 (update tdb-map :idx-map-entry ; #todo make verify like fetch-in
                   (fn [index-avl-set]
                     (conj index-avl-set [me-val me-key me-hid])))))
  nil)

(s/defn update-index-arrayentry!
  [ae-elem :- LeafType
   ae-idx
   ae-hid :- HID]
  (swap! *tdb* (fn [tdb-map]
                 (update tdb-map :idx-array-entry ; #todo make verify like fetch-in
                   (fn [index-avl-set]
                     (conj index-avl-set [ae-elem ae-idx ae-hid])))))
  nil)

(def ^:no-doc hid-count-base 1000)
(def ^:no-doc hid-counter (atom hid-count-base))

(defn ^:no-doc hid-count-reset
  "Reset the hid-count to its initial value"
  [] (reset! hid-counter hid-count-base))

(s/defn ^:no-doc new-hid :- HID
  "Returns the next integer HID"
  [] (swap! hid-counter inc))

(s/defn hid? :- s/Bool
  "Returns true iff the arg type is a legal HID value"
  [arg] (int? arg))

(s/defn array-like? :- s/Bool
  "Returns true for vectors, lists, and seq's."
  [arg] (or (vector? arg) (list? arg) (seq? arg)))

(s/defn add-edn :- HID ; #todo maybe rename:  load-edn->hid  ???
  ([edn-val :- s/Any] (add-edn nil edn-val))
  ([hid-parent :- (s/maybe HID)
    edn-val :- s/Any]
   (cond
     (map? edn-val) (let [hid-map-node (new-hid)]
                      (set-node! hid-map-node
                        (->MapNode hid-parent
                          (apply glue
                            (forv [[<key> <val>] edn-val]
                              (let [hid-me-node (new-hid)
                                    hid-leaf    (add-edn hid-me-node <val>)]
                                (when (leaf-val? <val>)
                                  (update-index-mapentry! <val> <key> hid-me-node))
                                (set-node! hid-me-node (->MapEntryNode hid-map-node <key> hid-leaf))
                                (map-entry <key> hid-me-node)))))))

     (array-like? edn-val) (let [hid-array-node (new-hid)]
                             (set-node! hid-array-node
                               (->ArrayNode hid-parent
                                 (apply glue
                                   (forv [[idx elem] (indexed edn-val)]
                                     (let [hid-ae-node (new-hid)
                                           hid-leaf    (add-edn hid-ae-node elem)]
                                       (when (leaf-val? elem)
                                         (update-index-arrayentry! elem idx hid-ae-node))
                                       (set-node! hid-ae-node (->ArrayEntryNode hid-array-node idx hid-leaf))
                                       (map-entry idx hid-ae-node)))))))

     (set? edn-val) (let [hid-setnode (new-hid)]
                      (set-node! hid-setnode
                        (->SetNode
                          hid-parent
                          (apply glue
                            (forv [set-elem edn-val]
                              (t/map-entry set-elem (add-edn hid-setnode set-elem)))))) )

     (leaf-val? edn-val) (let [hid-leafnode (new-hid)]
                           (update-index-leaf! edn-val hid-leafnode)
                           (set-node! hid-leafnode (->LeafNode hid-parent edn-val)))

     :else (throw (ex-info "unknown value found" (vals->map edn-val))))))

(s/defn hid->edn :- s/Any
  "Returns EDN data for the subtree rooted at hid"
  [hid :- HID]
  (edn (hid->node hid)))

(s/defn hid-nav :- s/Any
  [hid :- HID
   path :- tsk/Vec]
  (let [node       (hid->node hid)
        key        (t/xfirst path)
        path-rest  (t/xrest path)
        nav-result (nav node key)]
    (if (empty? path-rest)
      nav-result
      (if (hid? nav-result)
        (hid-nav nav-result path-rest)
        (do
          (spy :hid-nav--else-361)
          (forv [hid nav-result]
            (hid-nav hid path-rest)))))))

(s/defn hid->parent-hid :- (s/maybe HID)
  "Returns the parent HID of the node at this HID"
  [hid :- HID]
  (t/cond-it-> (parent-hid (hid->node hid))
    (or (instance? MapEntryNode (hid->node it))
      (instance? ArrayEntryNode (hid->node it))) (parent-hid (hid->node it))))

(s/defn ^:private ^:no-doc index-find-val-impl ; #todo inline below
  [target :- tsk/Vec]
  (let [idx-avl-set      (t/validate set? (grab :idx-leaf (deref *tdb*)))
        matching-entries (grab :matches
                           (tdi/split-key-prefix target idx-avl-set))]
    matching-entries))

(s/defn index-find-leaf
  [target :- LeafType]
  (let [idx-entries (index-find-val-impl [target])
        hids        (mapv t/xsecond idx-entries)]
    hids))

(s/defn index-find-mapentry
  [tgt-me :- tsk/MapEntry]
  (let [[me-key me-val] (mapentry->kv tgt-me)
        tgt-prefix       [me-val me-key]
        idx-avl-set      (t/validate set? (fetch-in (deref *tdb*) [:idx-map-entry]))
        matching-entries (grab :matches
                           (tdi/split-key-prefix tgt-prefix idx-avl-set))
        aen-hids         (mapv last matching-entries)
        an-hids          (mapv hid->parent-hid aen-hids)]
    an-hids))

(s/defn index-find-submap
  [target-submap :- tsk/KeyMap]
  (let [map-hids (apply set/intersection
                   (t/forv [tgt-me target-submap]
                     (set (index-find-mapentry tgt-me))))]
    map-hids))
















