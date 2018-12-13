;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.forest
  "Allows the use of multiple tree structures. Provides tools to create, manipulate, and query
  the the trees individually and/or collectively."
  (:use tupelo.core)
  (:require
    [clojure.set :as set]
    [clojure.core.async :as ca]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    #?@(:clj
        [[com.climate.claypoole :as claypoole]
         [net.cgrand.tagsoup :as enlive-tagsoup]
         [tupelo.misc :as tm :refer [HID]]
         ])))

; Benefits compared to nested maps like Enlive:
;   generalizes attrs/values; no special role for `tag` like enlive
;   clearly separates the roles of interior nodes vs terminal leaves
;   can get a handle to an individual node; don't need to navigate from root
;     can build up tree one node at a time
;   native can only transform 1 node -> 1 node
;     forest can delete a subtree; native can only replace with `nil` or equiv
;     forest can replace a subtree with 2 or more nodes
;   still immutable, native Clojure maps at base
;      `with-forest` macro restricted to a single thread at a time


; #todo  move to tupelo-dev.forest (tupelo.x-datapig ?)
; #todo  define tf/Bush type (& Hiccup, Enlive)

; forest  data-forest  ForestDb forest-db
; Sherwood  weald  wald  boreal

#?(:clj (do

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(def ^:dynamic ^:no-doc *forest* nil)
(def ^:dynamic ^:no-doc *new-hid-fn* tm/new-hid)

(def ^:dynamic ^:no-doc *debug-hid-count* nil)
(defn ^:no-doc new-hid-debug
  "Returns the next HID in debug mode"
  []
  (let [hex-str (format "%04x" (dec (swap! *debug-hid-count* inc)))
        hid     (keyword hex-str) ]
    hid) )

(defmacro with-debug-hid ; #todo swap names?
  [& forms]
  `(binding [*debug-hid-count* (atom 0)
             *new-hid-fn*      new-hid-debug]
     ~@forms))

(s/defn new-hid :- HID
  "Returns a new HexID"
  []
  (*new-hid-fn*))

(defn validate-forest []
  (when-not (map? (deref *forest*))
    (throw (ex-info "validate-forest: failed forest=" {:forest (deref *forest*)}))))

(defmacro with-forest ; #todo swap names?
  [forest-arg & forms]
  `(binding [*forest* (atom ~forest-arg)]
     (validate-forest)
     ~@forms ))

(defmacro with-forest-result ; #todo swap names?
  [forest-arg & forms]
  `(binding [*forest* (atom ~forest-arg)]
     (validate-forest)
     ~@forms
     (deref *forest*)))

; #todo: copy technique -> (with-state [x y]
; #todo:                      (set x 1)
; #todo:                      (set y (inc (get x)) ...)  ; can do imperative-style loops, etc

; HID & :hid are shorthand for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Node }
(defn new-forest
  "Returns a new, empty forest."
  []
  {})

; #todo need an attribute registry { :kw fn-validate }
; #todo need fn's to add/delete attributes; to delete verify no uses exist. Change = delete then re-add
; #todo on any data change, run validate fn
; #todo Global validation fn's;  apply all that match; implied wildcards for missing attr/vals
;   :attrs {:type :int            } => <parse-int works>
;   :attrs {:type :int :color :red} => <must be even>

; #todo kids save a parent reference set?
; #todo need to make operate on a copy of the DB; only save result if not throw
; #todo     => maybe switch to a ref instead of atom

; #todo RootedForest - has self-contained roots: #{HID}
; #todo validate-tree: kids ordered or not, exact or extras ok

; #todo MAYBE???
; #todo merge Node/Node -> generic Node: {:attrs <some map> :value <something> ::kids []}
; #todo              or -> plain map:    {:tupelo.forest/khids []  :value <something> :attr1 val1 :attr2 val2}
;                                         ^req      ^optional
; #todo maybe :value is just a regular (user-defined) attribute. not a special key

; #todo add { :parents #{:23 :14 :ab9} } to Node
; #todo add loop detection, recurse on parents not= <new child>
; #todo if loops are ok, need to add :max-depth to search queries to avoid infinite loop

; #todo allow multiple tags as a set {:tag :abc} -> {:tags #{ :abc :def :xyz }}

; #todo add ::tag? (only req for hiccup/enlive data?)
; { :tupelo.forest/khids  [hid...]  :k1 v1 :k2 v2 ...  :value s/Any  }
;    ^ req             ^opt k-v's          ^opt/leaf
; #todo rename :tupelo.forest/khids -> :kid-hids ?
; keep in mind that a Node is a "fragment" of a tree, and only contains "pointers" (HIDs) to the :tupelo.forest/khids
(def Node { (s/required-key ::khids) [HID]  s/Keyword s/Any } )

(s/defn ->Node :- Node
  "Constructs a Node from a vector of HIDs"
  [hids :- [HID]]
  (assert (every? tm/hid? hids))
  {::khids hids})

(s/defn forest-node? :- s/Bool
  "Returns true if the arg is a legal forest node"
  [arg :- tsk/KeyMap]
  (and   (contains-key? arg ::khids)
    (not (contains-key? arg ::kids))))

(s/defn forest-leaf? :- s/Bool
  "Returns true if the arg is a leaf node (empty :tupelo.forest/khids). "
  [node :- tsk/KeyMap]
  (and (forest-node? node)
    (empty? (grab ::khids node))))

(s/defn empty-leaf? :- s/Bool
  "Returns true if the arg is a leaf node (no kids) and has no `:value` "
  [node :- tsk/KeyMap]
  (and (forest-leaf? node)
    (not (contains-key? node :value))))

;-----------------------------------------------------------------------------
; #todo need to delete these???  Concentrate on forest nodes
(s/defn tree-node? :- s/Bool
  "Returns true if the arg is a legal tree node"
  [node :- tsk/KeyMap]
  (and   (contains-key? node ::kids)
    (not (contains-key? node ::khids))))

(s/defn tree-leaf? :- s/Bool
  "Returns true if the arg is a leaf node (no kids). "
  [node :- tsk/KeyMap]
  (and (tree-node? node)
    (empty? (grab ::kids node))))

;---------------------------------------------------------------------------------------------------
(s/defn edn->tree
  "Creates a tree from an EDN data structure"
  ([data :- s/Any]
    (edn->tree nil data))
  ([idx :- (s/either s/Int (s/eq nil))
    data :- s/Any]
    (cond
      (sequential? data) {::tag   ::list
                          ::index idx
                          ::kids  (forv [[idx val] (indexed data)]
                                    (edn->tree idx val))}
      (map? data) {::tag   ::entity
                   ::index idx
                   ::kids  (forv [[child-key child-val] data]
                             {::tag ::entry
                              ::key child-key
                              ::kids [(edn->tree child-val)]})}
      :else {::value data ::index idx ::kids []})))

(defn ^:no-doc validate-list-kids-idx
  "verify that a ::list node in a tree has a valid index for all kid nodes"
  [node]
  (assert (= ::list (grab ::tag node)))
  (let [kids        (grab ::kids node)
        kids-sorted (vec (sort-by #(grab ::index %) kids))
        idx-vals    (mapv #(grab ::index %) kids-sorted)
        idx-tgts    (range (count idx-vals))]
    (assert (= idx-vals idx-tgts))
    kids-sorted))

(s/defn ^:no-doc data-list-node?
  [node :- tsk/KeyMap]
  (and (contains-key? node ::tag)
    (= ::list (grab ::tag node))))

(s/defn ^:no-doc data-entity-node?
  [node :- tsk/KeyMap]
  (and (contains-key? node ::tag)
    (= ::entity (grab ::tag node))))

(s/defn ^:no-doc data-leaf-node?
  [node :- tsk/KeyMap]
  (and (= #{::value ::index ::kids} (set (keys node)))
    (= [] (grab ::kids node))))

(defn tree->edn
  "Converts a tree to an EDN data structure"
  [node]
  ; #todo assert valid tree?
  (cond
    (data-leaf-node? node) (let [leaf-value (grab ::value node)]
                             leaf-value)

    (data-list-node? node) (let [kids-sorted (validate-list-kids-idx node)
                                 kids-data   (forv [kid kids-sorted]
                                               (tree->edn kid))]
                             kids-data)

    (data-entity-node? node) (let [entries  (grab ::kids node)
                                   map-data (apply glue
                                              (forv [entry entries]
                                                {(grab ::key entry) (tree->edn (only (grab ::kids entry)))}))]
                               map-data)

    :else (throw (ex-info "tree->data: unrecognized node=" node))))

;---------------------------------------------------------------------------------------------------
(defn enlive-node-lax?
  "Returns true for nominal Enlive nodes, else false"
  [arg]
  (and
    (map? arg)
    (clj.set/superset? (set (keys arg)) #{:tag :attrs :content} )))

(defn enlive-node-strict?
  "Returns true for strictly valid Enlive nodes, else false"
  [arg]
  (and
    (map? arg)
    (= #{:tag :attrs :content} (set (keys arg)))
    (map? (grab :attrs arg))
    (sequential? (grab :content arg))))

(s/defn hiccup->enlive :- s/Any
  "Converts a data from Hiccup -> Enlive format"
  [node :- s/Any]
  (if-not (sequential? node)
    node ; leaf - just return it
    (let [tag    (xfirst node)
          less-1 (xrest node)]
      (if (empty? less-1)
        {:tag     tag
         :attrs   {}
         :content []}
        (let [v2 (xfirst less-1)]
          (if (map? v2)
            {:tag     tag
             :attrs   v2
             :content (forv [child (xrest less-1)]
                        (hiccup->enlive child))}
            {:tag     tag
             :attrs   {}
             :content (forv [child less-1]
                        (hiccup->enlive child))}))))))

(s/defn enlive->hiccup :- s/Any
  [node :- s/Any]
  (if-not (map? node)
    node ; leaf - just return it
    (do
      (assert (enlive-node-lax? node))
      (with-map-vals node [tag attrs content] ; destructure values
        (let [tag-attrs  (if (empty? attrs)
                           [tag]
                           [tag attrs])
              content-tx (forv [child content]
                           (enlive->hiccup child))
              result     (glue tag-attrs content-tx)]
             result)))))

(s/defn raw-leaf-node? :- s/Bool
  "Returns true if a node is a leaf with {:tag ::raw}."
  [node :- tsk/KeyMap]
  (let [tag-ok  (= ::raw (:tag node))
        kids    (::kids node)
        kids-ok (or (nil? kids) (= kids []))
        result  (and tag-ok kids-ok)]
     result))

(s/defn raw-kids-node? :- s/Bool
  "Returns true if all of a node's kids are raw leaf nodes."
  [node :- tsk/KeyMap]
  (let [kids (grab ::kids node)]
       (and (pos? (count kids))
         (every? i/truthy? (mapv raw-leaf-node? kids)))))

(s/defn consolidate-raw-kids :- tsk/Vec
  "Consolidates ::raw kids for a node into a single Enlive :content vector"
  [node :- tsk/KeyMap]
  (mapv #(grab :value %) (grab ::kids node)))

(s/defn tree->enlive :- (s/either tsk/KeyMap tsk/Vec)
  [tree-node :- tsk/KeyMap]
  (assert (tree-node? tree-node))
  (let [enlive-attrs (dissoc tree-node ::kids :tag :value)
        enlive-base  (glue (submap-by-keys tree-node #{:tag}) {:attrs enlive-attrs})]
    (cond
      (raw-kids-node? tree-node) (let [enlive-leaf (glue enlive-base {:content (consolidate-raw-kids tree-node)})]
                                   enlive-leaf)

      (tree-leaf? tree-node) (let [enlive-leaf (glue enlive-base
                                                 {:content (if (contains-key? tree-node :value)
                                                             [(grab :value tree-node)]
                                                             [])})]
                               enlive-leaf)

      :else (let [enlive-kids (mapv tree->enlive (grab ::kids tree-node))
                  enlive-node (glue enlive-base {:content enlive-kids})]
              enlive-node))))

(s/defn enlive->tree :- tsk/KeyMap ; #todo add test
  "Convert an Enlive-format data structure to a tree. "
  [enlive-tree  :- tsk/KeyMap]
  (assert (enlive-node-lax? enlive-tree))
  (let [attrs   (or (:attrs enlive-tree) {})
        content (or (:content enlive-tree) [])]
       (assert (not (contains-key? attrs :tag)))
    (let [attrs  (glue attrs (submap-by-keys enlive-tree #{:tag}))
          result (cond
                   (every? enlive-node-lax? content)
                   (let [kid-hids (glue [] (for [child content]
                                             (enlive->tree child)))]
                        (glue attrs {::kids kid-hids}))

                   (and
                     (= 1 (count content))
                     (not (enlive-node-lax? (only content))))
                   (glue attrs {:value (only content) ::kids []})

                   :else (let [kid-hids (glue []
                                          (for [child content]
                                            (if (enlive-node-lax? child)
                                              (enlive->tree child)
                                              {:tag ::raw :value child ::kids []})))]
                              (glue attrs {::kids kid-hids})))]
         result)))

(s/defn validate-hid
  "Returns HID arg iff it exists in the forest, else throws."
  [hid :- HID]
  (when-not (contains-key? (deref *forest*) hid)
    (throw (ex-info "validate-hid: HID does not exist=" (vals->map hid))))
  hid)

(s/defn hid->node :- Node
  "Returns the node corresponding to an HID"
  [hid :- HID]
  (grab hid (deref *forest*)))

(s/defn hid->leaf :- Node
  "Returns the leaf node corresponding to an HID"
  [hid :- HID]
  (validate forest-leaf? (hid->node hid)))

(s/defn hid->attrs :- tsk/KeyMap ; #todo remove OBE
  [hid :- HID]
  (dissoc (hid->node hid) ::khids))

(s/defn hid->attr :- s/Any ; #todo remove OBE
  "Given an HID, returns a single attr"
  [hid :- HID
   attr :- s/Keyword]
  (grab attr (hid->node hid)))

(s/defn hid->kids :- [HID]
  "Returns the HIDs for a nodes children."
  [hid :- HID]
  (grab ::khids (hid->node hid)))

(s/defn node-hid?  ; #todo remove OBE?
  "Returns true iff an HID is a Node"
  [hid :- HID]
  (forest-node? (hid->node hid)))

(s/defn leaf-hid?
  "Returns true iff an HID is a leaf"
  [hid :- HID]
  (forest-leaf? (hid->node hid)))

(s/defn empty-leaf-hid? :- s/Bool
  "Returns true if the arg is a leaf node (no kids) and has no `:value` "
  [hid :- HID]
  (empty-leaf? (hid->node hid)))

(s/defn all-hids :- #{HID} ; #todo test
  "Returns a set of all HIDs in the forest"
  []
  (set (keys (deref *forest*))) )

(s/defn all-leaf-hids :- #{HID} ; #todo remove OBE?
  "Returns a set of all leaf HIDs in the forest"
  []
  (set (keep-if leaf-hid? (all-hids))))

(s/defn root-hids :- #{HID}
  "Return a vector of all root HID's"
  []
  (let [kid-hids  (reduce
                    (fn [cum-kids hid]
                      (into cum-kids (grab ::khids (hid->node hid))))
                    #{}
                    (all-hids))
        root-hids (clj.set/difference (all-hids) kid-hids)]
    root-hids))

; #todo need hid->descendent-hids => depth-first list of all descendent hids

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn hid->tree :- tsk/KeyMap
  [hid :- HID]
  (let [node        (hid->node hid)
        base-result (it-> node
                      (into {} it)
                      (dissoc it ::khids))]
    (if (forest-leaf? node)
      ; leaf: nothing else to do
      (glue {::kids []} base-result) ; #todo can clean up more?
      ; Node: need to recursively resolve children
      (let [kid-trees       (mapv hid->tree (grab ::khids node))
            resolved-result (assoc base-result ::kids kid-trees)]
        resolved-result))))

; #todo naming choices:
; #todo   reset! vs  set
; #todo   swap!  vs  update
; #todo   remove vs  delete  vs drop

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn set-node
  "Unconditionally sets the value of a Node in the forest"
  ([hid :- HID
    node :- Node]
    (assert (not (contains-key? node ::kids)))
    (swap! *forest* glue {hid node})
    node)
  ([hid :- HID
    attrs :- tsk/KeyMap
    kids :- [HID]]
    (let [node (glue (->Node kids) attrs)]
      (set-node hid node)
      node)))

(s/defn validate-attrs
  [attrs :- tsk/KeyMap]
  (let [illegal-value?   (s/fn fn-illegal-value [arg] (or (= arg :*) (= arg :**))) ]
    (when (has-some? illegal-value? (keyvals attrs))
      (throw (ex-info "validate-attrs: failed attrs=" attrs)))
    attrs))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn add-node :- HID
  ([attrs-arg] (add-node attrs-arg [])) ; #todo need test ; => ctx (tag required)
  ([attrs-arg :- (s/either tsk/KeyMap s/Keyword) ; #todo merge args
    kid-hids :- [HID]]
    (doseq [kid kid-hids] (validate-hid kid))
    (let [attrs (if (map? attrs-arg)
                  attrs-arg
                  {:tag (validate keyword? attrs-arg)})
          hid   (new-hid)]
      (validate-attrs attrs)
      (set-node hid attrs kid-hids)
      hid)))

(s/defn add-leaf :- HID  ; #todo remove duplication
  ([attrs-arg] (add-leaf attrs-arg nil)) ; #todo test ; => ctx (tag required)
  ([attrs-arg :- (s/either tsk/KeyMap s/Keyword); #todo merge args
   value :- s/Any ]
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {:tag (validate keyword? attrs-arg)} )
        attrs (glue attrs {:value value}) ]
    (add-node attrs []))))

; #todo add [curr-path] to -impl and intc fn args
(s/defn ^:no-doc walk-tree-impl
  [ctx :- tsk/KeyMap]
  (with-map-vals ctx [parent-path hid interceptor parallel? threadpool]
    (s/validate [HID] parent-path)
    (s/validate HID hid)
    (s/validate tsk/KeyMap interceptor)
    (s/validate s/Bool parallel?)
    (let [enter-fn (grab :enter interceptor)
          leave-fn (grab :leave interceptor)]
      (enter-fn parent-path hid)
      (let [parent-path-new (append parent-path hid)]
        (if parallel?
          (claypoole/pdoseq threadpool [kid-hid (hid->kids hid)]
            (walk-tree-impl parent-path-new kid-hid interceptor))
          (doseq [kid-hid (hid->kids hid)]
            (walk-tree-impl parent-path-new kid-hid interceptor))))
      (leave-fn parent-path hid))))

(s/defn walk-tree   ; #todo add more tests
  "Recursively walks a subtree of the forest, applying the supplied `:enter` and ':leave` functions
   to each node.   Usage:

       (walk-tree <subtree-root-hid> intc-map)

   where `intc-map` is an interceptor map like:

       { :enter <pre-fn>       ; defaults to `identity`
         :leave <post-fn> }    ; defaults to `identity`

   Here, `pre-fn` and `post-fn` look like:

       (fn [parent-path hid] ...)

   where `parent-path` is a vector of parent HIDs beginning at the root of the sub-tree being processed,
   and `hid` points to the current node to be processed. "
  ([root-hid :- HID
    intc-map :- tsk/KeyMap]
    (walk-tree root-hid intc-map false))
  ([root-hid :- HID
    intc-map :- tsk/KeyMap
    parallel? :- s/Bool]
    (let [legal-keys   #{:id :enter :leave}
          counted-keys #{:enter :leave}
          keys-present (set (keys intc-map))]
      (let [extra-keys (set/difference keys-present legal-keys)]
        (when (not-empty? extra-keys)
          (throw (ex-info "walk-tree: unrecognized keys found:" intc-map))))
      (let [counted-keys-present (set/intersection counted-keys keys-present)]
        (when (empty? counted-keys-present)
          (throw (ex-info "walk-tree: no counted keys found:" intc-map)))))
    (let [enter-fn        (get intc-map :enter noop)
          leave-fn        (get intc-map :leave noop) ]
      (enter-fn [] root-hid)
      (if parallel?
        (claypoole/with-shutdown! [threadpool (claypoole/threadpool 8)]
          (walk-tree-impl {:parent-path []
                           :hid         root-hid
                           :interceptor {:enter enter-fn :leave leave-fn}
                           :parallel?   parallel?
                           :threadpool  threadpool}))
        (walk-tree-impl {:parent-path []
                         :hid         root-hid
                         :interceptor {:enter enter-fn :leave leave-fn}
                         :parallel?   parallel?
                         :threadpool  nil}))
      (leave-fn [] root-hid))))

(s/defn add-tree :- HID
  "Adds a tree to the forest."
  [tree-node  :- tsk/KeyMap]
  (when-not (tree-node? tree-node)
    (throw (ex-info "add-tree: invalid element=" tree-node)))
  (let [tree-node-attrs (dissoc tree-node ::kids)
        kid-hids        (forv [child (grab ::kids tree-node)] ; #todo forv & no glue ???
                          (add-tree child))]
       (add-node tree-node-attrs kid-hids)))

(s/defn bush-node? :- s/Bool ; #todo add test
  [arg]
  (and (vector? arg) ; it must be a vector
    (not-empty? arg) ; and cannot be empty
    (map? (xfirst arg)))) ; and the first item must be a map of attrs

(s/defn bush->tree :- tsk/KeyMap ; #todo add test
  "Converts a bush to a tree"
  [bush]
  (assert (bush-node? bush))
  (let [attrs  (xfirst bush)
        others (xrest bush)]
    (if (every? bush-node? others)
      (let [kids (glue [] (for [it others] (bush->tree it)))]
        (assoc attrs ::kids kids))
      (glue attrs {:value (only others)}))))

(s/defn tree->bush :- tsk/Vec
  [tree-node :- tsk/Map]
  (assert (tree-node? tree-node))
  (let [bush-kids (mapv tree->bush (grab ::kids tree-node))
        bush-node (prepend (dissoc tree-node ::kids) bush-kids)]
     bush-node))

(s/defn enlive->bush :- tsk/Vec ; #todo add test
  "Converts an Enlive-format data structure to a Bush. "
  [arg :- tsk/KeyMap]
  (-> arg enlive->tree tree->bush))

(s/defn bush->enlive :- tsk/KeyMap ; #todo add test
  "Converts a Bush to an Enlive-format data structure"
  [bush :- tsk/Vec]
  (-> bush bush->tree tree->enlive))

(s/defn xml->enlive :- tsk/KeyMap ; #todo need tree->xml  ???
  [xml-str :- s/Str]
  (->> xml-str
    ts/string->stream
    enlive-tagsoup/parser
    only))

(s/defn hiccup->tree :- tsk/KeyMap
  "Converts a Hiccup-format data structure to a Tree."
  [arg :- tsk/Vec]
  (-> arg
    hiccup->enlive
    enlive->tree))

; #todo need (spy-it-> (hiccup->enlive it)) that prints both input and output of fn
; #todo need (spyx-> val f g h) that prints both values at each step
;    (-> arg hiccup->enlive enlive->tree )
;       arg => 2
;       hiccup->enlive => 3
;       enlive->tree => 4


(s/defn tree->hiccup :- tsk/Vec
  "Converts a Tree to a Hiccup-format data structure."
  [arg :- tsk/KeyMap]
  (-> arg tree->enlive enlive->hiccup ))

(s/defn hiccup->bush :- tsk/Vec
  "Converts a Hiccup-format data structure to a Bush."
  [arg :- tsk/Vec]
  (-> arg hiccup->tree tree->bush))

(s/defn bush->hiccup :- tsk/Vec
  "Converts a Bush to a Hiccup-format data structure."
  [arg :- tsk/Vec]
  (-> arg bush->tree tree->hiccup ))

(s/defn add-bush :- HID
  "Adds a bush to the forest"
  [bush]
  (add-tree (bush->tree bush)))

(s/defn add-tree-enlive :- HID
  "Adds an Enlive-format tree to the forest "
  [arg]
  (add-tree (enlive->tree arg)))

(s/defn add-tree-hiccup :- HID  ; #todo maybe make (add-tree-edn ...) for simplicity?
  "Adds a Hiccup-format tree to the forest. Tag values are converted to nil attributes:
  [:a ...] -> {:a nil ...}..."
  [arg]
  (add-tree (hiccup->tree arg)))

(s/defn add-tree-xml :- HID
  "Adds a tree to the forest from an XML string."
  [xml-str :- s/Str]
  (add-tree-enlive
    (xml->enlive xml-str)))

(s/defn ^:no-doc nest-enlive-nodes :- tsk/EnliveNode
  "Reconstructs an Enlive tree from a sequence of nodes."
  [nodes :- [tsk/EnliveNode]]
  (let [num-nodes (count nodes)]
    (cond
      (zero? num-nodes) (throw (ex-info "num-nodes must be positive" (vals->map num-nodes)))
      (= 1 num-nodes)   (only nodes)
      :else (let [nodes-1           (xbutlast nodes)
                  nodes-2           (xbutlast nodes-1)
                  node-last         (xlast nodes)
                  node-last-1       (xlast nodes-1)
                  nodes-merged-last (append nodes-2 (assoc node-last-1 :content [node-last]))]
              (nest-enlive-nodes nodes-merged-last)))))

(defn ^:no-doc filter-enlive-subtrees-helper
  [ctx]
  (with-map-vals ctx [output-chan enlive-nodes-lazy parent-nodes path-target]
    (let [curr-tag (xfirst path-target)]
      (doseq [curr-node enlive-nodes-lazy]
        (when (map? curr-node) ; discard any embedded string content (esp. blanks)
          ;(spyx-pretty curr-node)
          (when (= curr-tag (grab :tag curr-node))
            (let [next-path-target (xrest path-target)]
              (if (not-empty? next-path-target)
                (let [next-parent-nodes (append parent-nodes
                                          (glue {:content []} (submap-by-keys curr-node [:tag :attrs])))]
                  (filter-enlive-subtrees-helper {:output-chan       output-chan
                                                  :enlive-nodes-lazy (grab :content curr-node)
                                                  :parent-nodes      next-parent-nodes
                                                  :path-target       next-path-target}))
                (let [enlive-subtree        (append parent-nodes (unlazy curr-node))
                      rooted-enlive-subtree (nest-enlive-nodes enlive-subtree)]
                  (ca/>!! output-chan rooted-enlive-subtree))))))))))

(def ^:dynamic *enlive-subtree-buffer-size*
  "Default output buffer size for `filter-enlive-subtrees`."
  32)
(defn filter-enlive-subtrees
  "Lazily read an enlive tree, retaining only rooted subtrees as specified by `subtree-path`"
  [enlive-tree-lazy subtree-path]
  (let [output-chan (ca/chan *enlive-subtree-buffer-size*) ]
    (ca/go
      (filter-enlive-subtrees-helper {:output-chan       output-chan
                                      :enlive-nodes-lazy [enlive-tree-lazy]
                                      :parent-nodes      []
                                      :path-target       subtree-path})
      (ca/close! output-chan))
    (chan->lazy-seq output-chan)))

(s/defn hid->bush :- tsk/Vec
  [hid :- HID]
  (-> (validate-hid hid) hid->tree tree->bush))

(s/defn hid->hiccup :- tsk/Vec
  [hid :- HID]
  (-> (validate-hid hid) hid->tree tree->hiccup))

; #todo make sure all permutations are available
(defn hid->enlive [hid]
  (-> hid hid->tree tree->enlive))

; #todo replace with set-node ?
(s/defn attrs-reset :- tsk/KeyMap
  "Replace the attrs of a Node with the supplied attrs map"
  [hid :- HID
   attrs-new :- tsk/KeyMap]
  (validate-attrs attrs-new)
  (let [node-curr  (hid->node hid)
        node-new   (it-> node-curr
                     (grab ::khids it)
                     (->Node it)
                     (glue it attrs-new))]
    (set-node hid node-new)
    node-new))

(s/defn attrs-merge :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node "
  [hid :- HID
   attrs-in :- tsk/KeyMap]
  (let [node-curr  (hid->node hid)
        node-new  (glue node-curr attrs-in) ]
    (validate-attrs node-new)
    (set-node hid node-new)
    node-new))

; #todo need attr-set
(s/defn attr-get :- tsk/KeyMap ; #todo test
  "Use the supplied function & arguments to update the attr value for a Node as in clojure.core/update"
  [hid :- HID
   attr-name :- s/Keyword ]
  (fetch (hid->node hid) attr-name ))

(s/defn attr-update :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node as in clojure.core/update"
  [hid :- HID
   attr-name :- s/Keyword
   fn-update-attr        ; signature: (fn-update-attr attr-curr x y z & more) -> attr-new
   & fn-update-attr-args]
  (let [node-curr      (hid->node hid)
        attr-val-curr  (fetch node-curr attr-name )
        attr-val-new   (apply fn-update-attr attr-val-curr fn-update-attr-args)
        node-new       (assoc node-curr attr-name attr-val-new) ]
    (validate-attrs node-new)
    (set-node hid node-new)
    node-new))

(s/defn attr-remove :- tsk/KeyMap
  "Removes the specified attribute for an element"
  [hid :- HID
   attr :- s/Keyword]
  (let [node-curr (hid->node hid)
        node-new  (dissoc node-curr attr)]
    (set-node hid node-new)))

; #todo make :value just another user-supplied attr :f/value
(s/defn value-set :- Node
  "Resets the value of a leaf"
  [hid :- HID
   value-new :- s/Any]
  (let [leaf-curr  (hid->leaf hid)
        leaf-new   (glue leaf-curr {:value value-new}) ]
    (set-node hid leaf-new)
    leaf-new))

(s/defn value-update :- Node
  "Given a leaf with a value, updates that value using a function"
  [hid :- HID
   fn-update-value  ; signature: (fn-update-value value-curr x y z & more) -> value-new
   & fn-update-value-args]
  (let [leaf-curr  (hid->leaf hid)
        value-curr (grab :value leaf-curr)
        value-new  (apply fn-update-value value-curr fn-update-value-args)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-node hid leaf-new)
    leaf-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-set :- Node
  "Resets the kids of a Node to the supplied list"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr  (hid->node hid)
        node-new   (glue node-curr {::khids kids-new})]
    (set-node hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-update :- tsk/KeyMap
  "Updates the kids for a Node using a function, as in clojure.core/update"
  [hid :- HID
   fn-update-kids   ; signature: (fn-update kids-curr x y z & more) -> kids-new
   & fn-update-kids-args]
  (let [node-curr (hid->node hid)
        kids-curr (grab ::khids node-curr)
        kids-new  (apply fn-update-kids kids-curr fn-update-kids-args)
        node-new  (glue node-curr {::khids kids-new})]
    (set-node hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-append :- tsk/KeyMap
  "Appends a list of kids a Node"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr (hid->node hid)
        kids-curr (grab ::khids node-curr)
        kids-new  (glue kids-curr kids-new)
        node-new  (glue node-curr {::khids kids-new})]
       (set-node hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-prepend :- tsk/KeyMap
  "Appends a list of kids a Node"
  [hid :- HID
   kids-in :- [HID]]
  (let [node-curr (hid->node hid)
        kids-curr (grab ::khids node-curr)
        kids-new  (glue kids-in kids-curr)
        node-new  (glue node-curr {::khids kids-new})]
    (set-node hid node-new)
    node-new))

; #todo (s/defn remove-orphans [roots-to-keep] ...)

(s/defn remove-kids :- tsk/KeyMap
  "Removes a set of children from a Node (including any duplcates)."
  ([hid :- HID
    kids-leaving :- (s/either [HID] #{HID})]
    (remove-kids hid kids-leaving false))
  ([hid :- HID
    kids-leaving :- (s/either [HID] #{HID})
    missing-kids-ok? :- s/Bool]
    (let [kids-leaving        (set kids-leaving)
          report-missing-kids (not missing-kids-ok?)
          node-curr           (hid->node hid)
          kids-curr           (grab ::khids node-curr)
          missing-kids        (clj.set/difference kids-leaving (into #{} kids-curr))
          _                   (when (and (not-empty? missing-kids) report-missing-kids)
                                (throw (ex-info  "remove-kids: missing-kids found=" (vals->map missing-kids))))
          kid-is-leaving?     (fn fn-kid-is-leaving? [kid] (contains-key? kids-leaving kid))
          kids-new            (drop-if kid-is-leaving? kids-curr)
          node-new            (glue node-curr {::khids kids-new})]
      (set-node hid node-new)
      node-new)))

(s/defn remove-all-kids :- tsk/KeyMap
  "Disconnects all children from a Node, but does not delete them from the forest. "
  ([hid :- HID ]
    (let [node-curr   (hid->node hid)
          node-new    (glue node-curr {::khids []})]
      (set-node hid node-new)
      node-new)))

(s/defn ^:no-doc remove-node-from-parents
  "Given the HID of a node and its parents, remove the node rooted at that HID
  and update its immediate parent (if any). Does not remove child nodes."
  [parents :- [HID]
   hid :- HID]
  (swap! *forest* dissoc hid)
  (if (not-empty? parents)
    (let [parent-hid       (last parents)
          parent-node      (hid->node parent-hid)
          parent-khids     (hid->kids parent-hid)
          parent-khids-new (drop-if #(= hid %) parent-khids)]
      (kids-set parent-hid parent-khids-new))))

(s/defn remove-subtree-from-parents
  "For each HID arg, removes the subtree (including all child nodes) rooted at that HID."
  [parents :- [HID]
   hid :- HID]
  (walk-tree hid {:leave (fn [-parents- hid]
                           (swap! *forest* dissoc hid))})
  (remove-node-from-parents parents hid))

(s/defn remove-subtree-path
  "Given a path from a tree root, remove the subtree beginning at the path end."
  [path :- [HID]]
  (remove-subtree-from-parents (butlast path) (last path)))

(s/defn ^:no-doc hid-matches?
  "Returns true if an HID node matches a pattern"
  [hid :- HID
   pattern-in :- s/Any]
  (let [node    (hid->node hid)
        pattern (cond
                  (map?         pattern-in)  pattern-in
                  (sequential?  pattern-in)  (zipmap pattern-in (repeat nil))
                  (keyword?     pattern-in)  {:tag pattern-in}
                  :else (throw (ex-info  "hid-matches?: illegal pattern-in=" (vals->map pattern-in))))]
    (let [pattern-keys         (keys pattern)
          pattern-keys-set     (set pattern-keys)
          node-keys-set        (set (keys node))
          pattern-keys-missing (clj.set/difference pattern-keys-set node-keys-set)]
      (if (not-empty? pattern-keys-missing)
        false
        (let [attrs-tst    (submap-by-keys node pattern-keys-set)
              ; replace any nil values with wildcard :*
              pattern-wild (apply glue (for [[k v] pattern]
                                         {k (if (nil? v) :* v)}))]
          (wild-match? {:pattern    pattern-wild
                            :values [attrs-tst]}))))))

; #todo list-roots
; #todo list-non-roots
; #todo list-leaves

; #todo list-cycle-nodes (whose kid is an ancestor)

; #todo find-node, find-node, find-leaf
; #todo find-node, find-node, find-leaf
; #todo find-roots function (& root for sole root or throw)

; #todo (find-leaf root [ :a :b  :c ] ) ->
; #todo (find-leaf root [ :a :b  {:tag :c :value <val> ::kids []} ] )

; #todo allow pred fn to replace match value in search path:
; #todo    { :tag :person  :age #(<= 21 %) }

; #todo allow pred fn to replace entire node in search path:
; #todo    (fn [node] (and (contains? #{:horse :dog} (grab :animal/species node))
; #todo                 (<= 1 (grab :age node) 3 )))   ; an "adolescent" animal



;---------------------------------------------------------------------------------------------------
(s/defn format-path
  [hids :- [HID]]
  (let [[hid-curr & hids-rest] hids]
    (if (empty? hids-rest)
      (hid->bush hid-curr)
      (let [node-part (hid->node hid-curr)
            curr-part (dissoc node-part ::khids)
            kids-part (format-path hids-rest)
            result    [curr-part kids-part]]
           result))))

(s/defn format-paths
  [solns :- [[HID]]]
  (forv [soln solns]
    (format-path soln)))

(s/defn ^:private ^:no-doc find-paths-impl
  [result-atom
   parents :- [HID]
   hid :- HID
   tgt-path :- [(s/either s/Keyword tsk/KeyMap)] ]
  (validate-hid hid)
  (when (not-empty? tgt-path)
    (let [tgt           (xfirst tgt-path)
          tgt-path-rest (xrest tgt-path)
          node          (hid->node hid) ]
      (let [parents-new (append parents hid)]
        (when (or (= tgt :*) (hid-matches? hid tgt))
          ;(println :200 (str "match node=" node ))
          (if (empty? tgt-path-rest)
            (let [soln parents-new]
              ;(println :210 "empty soln:" (mapv #(hid->node %) soln))
              (swap! result-atom append soln))
            (do
              ;(println :220 "NOT (empty? tgt-path-rest) parents-new=" (mapv #(hid->node %) parents-new))
              (when-not (leaf-hid? hid)   ; #todo revisit this
                ;(println :221)
                (doseq [kid (hid->kids hid)]
                  ;(println :230 "kid=" (hid->node kid))
                  (find-paths-impl result-atom parents-new kid tgt-path-rest))))))
        (when (= tgt :**)
          ;(println :300 "tgt = :**")
          (when (not-empty? tgt-path-rest) ; :** wildcard cannot terminate the tgt-path
            ;(println :320 ":** parents-new:" (mapv #(hid->node %) parents-new))
            ;(println (str :330 "  recurse  parents:" (mapv #(hid->node %) parents)
            ;           "   hid:" (hid->node hid) "  tgt-path-rest:" tgt-path-rest))
            (find-paths-impl result-atom parents hid tgt-path-rest)
            (when-not (leaf-hid? hid)   ; #todo revisit this
              (doseq [kid (hid->kids hid)]
                ;(println :340 ":** kid:" (hid->node kid))
                ;(println (str :350 "    recurse  parents-new:" (mapv #(hid->node %) parents-new)
                ;           "  tgt-path:" tgt-path))
                (find-paths-impl result-atom parents-new kid tgt-path)))))))))

(def HidRootSpec (s/either HID [HID] #{HID})) ; #todo why is this here?

; #todo need a find-paths-pred that takes a predicate fn to choose
; #todo maybe a fn like postwalk to apply transformation fn to each node recursively
(s/defn find-paths :- [[HID]]    ; #todo need update-tree & update-leaf fn's
  "Searches an Enlive-format tree for the specified tgt-path"
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (when (empty? tgt-path)
    (throw (ex-info "find-paths: tgt-path is empty" (vals->map tgt-path))))
  (when (= :** (last tgt-path))
    (throw (ex-info "find-paths: recursive-wildcard `:**` cannot terminate tgt-path" (vals->map tgt-path))))

  (let [result-atom (atom [])
        roots (cond
                (tm/hid? root-spec)     #{root-spec} ; scalar arg -> wrap in a set
                (vector? root-spec)  (set root-spec) ; vec of root hids -> convert to set
                (set? root-spec)     root-spec ; set of root hids -> use it as-is
                :else (throw (ex-info  "find-paths: invalid root-spec=" (vals->map root-spec)))) ]
    (doseq [root roots]
      (find-paths-impl result-atom [] root tgt-path))
    @result-atom))

(s/defn find-hids :- [HID] ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec]
  (mapv last (find-paths root-spec tgt-path)))

(s/defn find-hid :- HID     ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec]
  (only (find-hids root-spec tgt-path)))

(s/defn find-tree     ; #todo need test (maybe delete?)
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (hid->tree (find-hid root-spec tgt-path)))

(s/defn leaf-path? :- s/Bool
  "Returns true if an HID path ends in a leaf"
  [path :- [HID]]
  (leaf-hid? (xlast path)))

; #todo remove
(s/defn find-leaf-paths  :- [[HID]]    ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (let [paths      (find-paths root-spec tgt-path)
        leaf-paths (keep-if leaf-path? paths)]
    leaf-paths))

; #todo remove
(s/defn find-leaf-hids :- [HID]     ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (mapv last (find-leaf-paths root-spec tgt-path )) )

; #todo remove
(s/defn find-leaf-hid ; #todo remove single version -> (only (xyz ...))
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (only (find-leaf-hids root-spec tgt-path )))

; #todo remove
(s/defn find-leaf
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (hid->leaf (find-leaf-hid root-spec tgt-path )))

(s/defn whitespace-leaf-hid? :- s/Bool
  [hid :- HID]
  (let [node (hid->node hid)]
    (and (forest-leaf? node)
      (contains-key? node :value)
      (let [value (grab :value node)]
        (and (string? value)
          (ts/whitespace? value)))))) ; all whitespace string

(s/defn remove-whitespace-leaves
  "Removes leaves from all trees in the forest that are whitespace-only strings
  (including zero-length strings)."
  ([] (doseq [hid (root-hids)]
        (remove-whitespace-leaves hid)))
  ([root-hid :- HID]
    (walk-tree root-hid {:leave (fn [parents hid]
                                  (when (whitespace-leaf-hid? hid)
                                    (remove-node-from-parents parents hid)))})))

(s/defn find-paths-with
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec
   path-pred :- s/Any] ; #todo how func spec?
  (let [paths-found (find-paths root-spec tgt-path)
        keepers     (keep-if path-pred paths-found)]
     keepers))

(s/defn find-hids-with
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec
   hid-pred :- s/Any] ; #todo how func spec?
  (let [paths-found (find-paths root-spec tgt-path)
        hids-found  (mapv last paths-found)
        hids-keep   (keep-if hid-pred hids-found)]
    hids-keep))

(s/defn has-child-path? ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (pos? (count (find-paths root-spec tgt-path))))

(s/defn has-child-path-with? ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec
   path-pred :- s/Any] ; #todo how func spec?
  (pos? (count (find-paths-with root-spec tgt-path path-pred))))

(s/defn has-child-leaf?
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (pos? (count (find-leaf-hids root-spec tgt-path))))

))
