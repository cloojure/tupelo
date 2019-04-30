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
                                       forv vals->map fetch-in let-spy xlast xfirst
                                       ]]
            [tupelo.schema :as tsk]
            [tupelo.data.index :as index]
            [clojure.set :as set]
            [schema.core :as s]
            ))
  #?(:cljs (:require
             [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab]] ; #todo :include-macros true
             [tupelo.schema :as tsk]
             [tupelo.data.index :as index]
             [clojure.set :as set]
             [schema.core :as s]
             ))
  )

; #todo add indexes
; #todo add sets (primative only or EID) => map with same key/value
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
(do       ; keep these two in sync
  (def EidType
    "The Plumatic Schema type name for a pointer to a tdb node (abbrev. for Hex ID)"
    s/Int)
  (s/defn eid? :- s/Bool
    "Returns true iff the arg type is a legal EID value"
    [arg] (int? arg)))

(do       ; keep these two in sync
  (s/defn leaf-val? :- s/Bool
    "Returns true iff a value is of leaf type (number, string, keyword)"
    [arg :- s/Any] (or (number? arg) (string? arg) (keyword? arg)))
  (def LeafType (s/cond-pre s/Num s/Str s/Keyword))) ; instant, uuid, Time ID (TID) (as strings?)

(s/defn array-like? :- s/Bool
  "Returns true for vectors, lists, and seq's."
  [arg] (or (vector? arg) (list? arg) (seq? arg)))

;-----------------------------------------------------------------------------

(defprotocol IContent
  (content [this]))

(s/defrecord Eid ; wraps an Entity Id (EID)
  [eid :- (s/maybe EidType)]
  IContent
  (content [this] eid))

(s/defrecord Leaf ; wraps a primitive leaf value
  [leaf :- (s/maybe LeafType)]
  IContent
  (content [this] leaf))

;-----------------------------------------------------------------------------
(def ^:dynamic ^:no-doc *tdb* nil)

(defmacro with-tdb ; #todo swap names?
  [tdb-arg & forms]
  `(binding [*tdb* (atom ~tdb-arg)]
     ~@forms))

(defn new-tdb
  "Returns a new, empty db."
  []
  {:map-eids   (sorted-set)
   :array-eids (sorted-set)
   :idx-ekv    (sorted-map)
   :idx-vke    (index/empty-index)
   :idx-kve    (index/empty-index)
   })

(s/defn update-index-mapentry!
  [me-val :- LeafType
   me-key :- LeafType
   me-eid :- EidType]
  (swap! *tdb* (fn [tdb-map]
                 (it-> tdb-map
                   (update it :idx-map-entry-vk ; #todo make verify like fetch-in
                     (fn [index-avl-set]
                       (index/add-entry index-avl-set [me-val me-key me-eid])))
                   (update it :idx-map-entry-kv ; #todo make verify like fetch-in
                     (fn [index-avl-set]
                       (index/add-entry index-avl-set [me-key me-val me-eid]))))))
  nil)

; (s/defn eid->node

(def ^:no-doc eid-count-base 1000)
(def ^:no-doc eid-counter (atom eid-count-base))

(defn ^:no-doc eid-count-reset
  "Reset the eid-count to its initial value"
  [] (reset! eid-counter eid-count-base))

(s/defn ^:no-doc new-eid :- EidType
  "Returns the next integer EID"
  [] (swap! eid-counter inc))

;(s/defn add-edn :- EidType ; #todo maybe rename:  load-edn->eid  ???
;  ([edn-val :- s/Any] (add-edn nil edn-val))
;  ([eid-parent :- (s/maybe EidType)
;    edn-val :- s/Any]
;   (cond
;     (map? edn-val) (let [eid-map-node (new-eid)]
;                      (set-node! eid-map-node
;                        (->MapNode eid-parent
;                          (apply glue
;                            (forv [[<key> <val>] edn-val]
;                              (let [eid-me-node (new-eid)
;                                    eid-leaf    (add-edn eid-me-node <val>)]
;                                (when (leaf-val? <val>)
;                                  (update-index-mapentry! <val> <key> eid-me-node))
;                                (set-node! eid-me-node (->MapEntryNode eid-map-node <key> eid-leaf))
;                                (map-entry <key> eid-me-node)))))))
;
;     (leaf-val? edn-val) (let [eid-leafnode (new-eid)]
;                           (update-index-leaf! edn-val eid-leafnode)
;                           (set-node! eid-leafnode (->LeafNode eid-parent edn-val)))
;
;     :else (throw (ex-info "unknown value found" (vals->map edn-val))))))


; (s/defn eid->edn :- s/Any

; (s/defn eid-nav :- [EidType]

;(s/defn eid->parent-eid :- (s/maybe EidType)

;(s/defn ^:private ^:no-doc index-find-val-impl ; #todo inline below

;(s/defn index-find-leaf
;  [target :- LeafType]
;  (let [idx-entries (index-find-val-impl [target])
;        hids        (mapv t/xsecond idx-entries)]
;    hids))
;
;(s/defn index-find-mapentry :- [EidType]
;  [tgt-me :- tsk/MapEntry]
;  (let [[tgt-key tgt-val] (mapentry->kv tgt-me)
;        tgt-prefix       [tgt-val tgt-key]
;        idx-avl-set      (t/validate set? (fetch-in (deref *tdb*) [:idx-map-entry-vk]))
;        matching-entries (grab :matches
;                           (index/split-key-prefix tgt-prefix idx-avl-set))
;        men-hids         (mapv xlast matching-entries)
;        ]
;    men-hids))
;
;(s/defn index-find-submap
;  [target-submap :- tsk/KeyMap]
;  (let [map-hids (apply set/intersection
;                   (forv [tgt-me target-submap]
;                     (set (mapv hid->parent-hid
;                            (index-find-mapentry tgt-me)))))]
;    map-hids))
;
;(s/defn index-find-mapentry-key :- [EidType]
;  [tgt-key :- LeafType]
;  (let [tgt-prefix       [tgt-key]
;        index            (t/validate set? (fetch-in (deref *tdb*) [:idx-map-entry-kv]))
;        matching-entries (grab :matches
;                           (index/split-key-prefix tgt-prefix index))
;        men-hids         (mapv xlast matching-entries)
;        ]
;    men-hids))
;
;(s/defn index-find-arrayentry :- [EidType]
;  [tgt-ae :- tsk/MapEntry] ; {idx elem} as a MapEntry
;  (let [[tgt-idx tgt-elem] (mapentry->kv tgt-ae)
;        tgt-prefix       [tgt-elem tgt-idx]
;        index            (t/validate set? (fetch-in (deref *tdb*) [:idx-array-entry-ei]))
;        matching-entries (grab :matches
;                           (index/split-key-prefix tgt-prefix index))
;        aen-hids         (mapv xlast matching-entries)
;        ]
;    aen-hids))
;
;(s/defn index-find-arrayentry-idx :- [EidType]
;  [tgt-idx :- LeafType]
;  (let [tgt-prefix       [tgt-idx]
;        index            (t/validate set? (fetch-in (deref *tdb*) [:idx-array-entry-ie]))
;        matching-entries (grab :matches
;                           (index/split-key-prefix tgt-prefix index))
;        aen-hids         (mapv xlast matching-entries)
;        ]
;    aen-hids))
;
;(s/defn parent-path-hid :- [EidType]
;  [hid-in :- EidType]
;  (let [node-in (hid->node hid-in)]
;    (loop [result   (cond
;                      (instance? MapEntryNode node-in) [hid-in (me-val-hid node-in)]
;                      (instance? ArrayEntryNode node-in) [hid-in (ae-elem-hid node-in)]
;                      (instance? LeafNode node-in) [hid-in]
;                      :else (throw (ex-info "unrecognized node type" (vals->map hid-in node-in))))
;           hid-curr hid-in]
;      (let [hid-par (parent-hid (hid->node hid-curr))]
;        (if (nil? hid-par)
;          result
;          (if (or
;                (instance? MapEntryNode (hid->node hid-par))
;                (instance? ArrayEntryNode (hid->node hid-par)))
;            (recur (t/prepend hid-par result) hid-par)
;            (recur result hid-par)))))))
;
;(s/defn parent-path-vals
;  [hid-in :- EidType]
;  (let [path-hids        (parent-path-hid hid-in)
;        parent-selectors (forv [path-hid path-hids]
;                           (let [path-node (hid->node path-hid)]
;                             (cond
;                               (instance? MapEntryNode path-node) (me-key path-node)
;                               (instance? ArrayEntryNode path-node) (ae-idx path-node)
;                               (instance? LeafNode path-node) (edn path-node)
;                               :else (throw (ex-info "invalid parent node" (vals->map path-node))))))]
;    parent-selectors))


























