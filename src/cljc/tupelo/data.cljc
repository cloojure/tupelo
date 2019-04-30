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

(defprotocol IRaw
  (raw [this]))

(s/defrecord Eid ; wraps an Entity Id (EID)
  [raw :- (s/maybe EidType)]
  IRaw
  (raw [this] raw))

(s/defrecord Leaf ; wraps a primitive leaf value
  [raw :- (s/maybe LeafType)]
  IRaw
  (raw [this] raw))

(s/defn eid :- Eid
  "Wraps an eid value into an Eid record"
  [arg :- EidType ]
  (->Eid arg))

(s/defn leaf :- Leaf
  "Wraps a primitive value into a Leaf record"
  [arg :- LeafType ]
  (->Leaf arg))

;-----------------------------------------------------------------------------
(def ^:dynamic ^:no-doc *tdb* nil)

(defmacro with-tdb ; #todo swap names?
  [tdb-arg & forms]
  `(binding [*tdb* (atom ~tdb-arg)]
     ~@forms))

(defn new-tdb
  "Returns a new, empty db."
  []
  {:map-eids    (sorted-set) ; holds eids of all map input collections
   :array-eids  (sorted-set) ; holds eids of all array input collections
   :eid-parent  (sorted-map) ; map from eid to parent-eid
   :idx-eav     (index/empty-index)
   :idx-vae     (index/empty-index)
   :idx-ave     (index/empty-index) })

; (s/defn eid->node

(def ^:no-doc eid-count-base 1000)
(def ^:no-doc eid-counter (atom eid-count-base))

(defn ^:no-doc eid-count-reset
  "Reset the eid-count to its initial value"
  [] (reset! eid-counter eid-count-base))

(s/defn ^:no-doc new-eid :- EidType
  "Returns the next integer EID"
  [] (swap! eid-counter inc))

(s/defn update-idxs!
  [me-eid :- EidType
   me-key :- LeafType
   me-val :- LeafType]
  (swap! *tdb* (fn [tdb-map]
                 (it-> tdb-map

                   (update it :idx-map-entry-vk ; #todo make verify like fetch-in
                     (fn [index-avl-set]
                       (index/add-entry index-avl-set [me-val me-key me-eid])))

                   (update it :idx-map-entry-kv ; #todo make verify like fetch-in
                     (fn [index-avl-set]
                       (index/add-entry index-avl-set [me-key me-val me-eid])))
                   )))
  nil)

(s/defn set-add-eid :- tsk/Set
  [set-in :- tsk/Set
   eid-in :- EidType]
  (conj set-in eid-in))

(s/defn set-remove-eid :- tsk/Set
  [set-in :- tsk/Set
   eid-in :- EidType]
  (disj set-in eid-in))

(s/defn add-edn :- EidType ; #todo maybe rename:  load-edn->eid  ???
  ([edn-in :- s/Any] (add-edn nil edn-in))
  ([parent-eid :- (s/maybe EidType)
    edn-in :- s/Any]
   (let [eid-curr (new-eid)
         entity-type-key (cond
                           (map? edn-in) :map-eids
                           (array-like? edn-in) :array-eids
                           :else (throw (ex-info "unknown value found" (vals->map edn-in)))) ]
     (swap! *tdb* (fn [tdb-map]
                    (it-> tdb-map
                      (update it entity-type-key set-add-eid eid-curr)
                      (update it :eid-parent assoc eid-curr parent-eid)

                      (reduce
                        (fn [cum-result [attr-edn val-edn]]
                          (let [val-add (if (leaf-val? val-edn)
                                          (->Leaf val-edn)
                                          (->Eid (add-edn eid-curr val-edn)))]
                            (it-> cum-result
                              (update it :idx-eav index/add-entry [eid-curr attr-edn val-add])
                              (update it :idx-vae index/add-entry [val-add attr-edn eid-curr])
                              (update it :idx-ave index/add-entry [attr-edn val-add eid-curr]))))
                        it
                        edn-in)

                      ))))))

       ;(set-node! eid-this
       ;  (->MapNode eid-parent
       ;    (apply glue
       ;      (forv [[<key> <val>] edn-in]
       ;        (let [eid-me-node (new-eid)
       ;              eid-leaf    (add-edn eid-me-node <val>)]
       ;          (when (leaf-val? <val>)
       ;            (update-index-mapentry! <val> <key> eid-me-node))
       ;          (set-node! eid-me-node (->MapEntryNode eid-this <key> eid-leaf))
       ;          (map-entry <key> eid-me-node)))))))



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


























