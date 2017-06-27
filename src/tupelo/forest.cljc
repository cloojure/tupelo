;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.forest
  "Allows the use of multiple tree structures. Provides tools to create, manipulate, and query
  the the trees individually and/or collectively."
  (:use tupelo.impl)
  (:require
    [clojure.set :as set]
    [schema.core :as s]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))

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
; forest  data-forest  ForestDb forest-db
; Sherwood  weald  wald  boreal

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(def ^:dynamic *forest* nil)

(defn validate-forest []
  (when-not (map? *forest*)
    (throw (IllegalArgumentException. (str "validate-forest: failed forest=" *forest*)))))

(defmacro with-forest ; #todo swap names?
  [forest-arg & forms]
  `(binding [*forest* ~forest-arg]
     (validate-forest)
     ~@forms ))

(defmacro with-forest-result ; #todo swap names?
  [forest-arg & forms]
  `(binding [*forest* ~forest-arg]
     (validate-forest)
     ~@forms
     *forest*))

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
; #todo merge Node/Node -> genric Node: {:attrs <some map> ::value <something> ::kids []}
; #todo              or -> plain map:   {:khids []  ::value <something> :attr1 val1 :attr2 val2}
;                                        ^req      ^optional

; #todo add { :parents #{:23 :14 :ab9} } to Node
; #todo add loop detection, recurse on parents not= <new child>

; { :khids  [hid...]  :k1 v1 :k2 v2 ...  ::value s/Any  }
;    ^ req             ^opt k-v's          ^opt/leaf
(defrecord Node [khids] ) ; #todo add ::tag? (only req for hiccup/enlive data?)
; #todo rename :khids -> :kid-hids ?
; keep in mind that a Node is a "fragment" of a tree, and only contains "pointers" (HIDs) to the :khids

(def HID s/Keyword) ; #todo find way to validate

(s/defn forest-node? :- s/Bool
  "Returns true if the arg is a legal forest node"
  [arg :- tsk/KeyMap]
  (or (instance? Node arg)
    (contains-key? arg :khids)))

(s/defn forest-leaf? :- s/Bool
  "Returns true if the arg is a leaf node (no khids). "
  [arg :- tsk/KeyMap]
  (and (forest-node? arg)
    (empty? (grab :khids arg))))

(s/defn tree-node? :- s/Bool
  "Returns true if the arg is a legal tree node"
  [arg :- tsk/KeyMap]
  (contains-key? arg ::kids))

(s/defn tree-leaf? :- s/Bool
  "Returns true if the arg is a leaf node (no kids). "
  [arg :- tsk/KeyMap]
  (and (tree-node? arg)
    (empty? (grab ::kids arg))))


(s/defn hid? :- s/Bool
  "Returns true if the arg is a legal HexID"
  [arg]
  (and (keyword? arg)
    (let [name-str (kw->str arg)]
         (and (ts/hex? name-str)
           (= 40 (count name-str))))))

(defn data->tree    ; #todo document
  "Convert raw data (nested vectors) into tree with generic `:data` tag and {::idx <index>} attribute."
  ([data]
   {::tag :root
    ::kids  (forv [[idx val] (indexed data)]
             (data->tree idx val))})
  ([idx data]
   (glue {::tag :data
          ::idx idx}
     (if (sequential? data)
       {::kids (forv [[idx val] (indexed data)]
                 (data->tree idx val))}
       {::value data}))))

(defn enlive-node?
  "Returns true for valid Enlive nodes, else false"
  [arg]
  (and
    (map? arg)
    (= #{:tag :attrs :content} (set (keys arg)))
    (it-> :attrs   (grab it arg) (map? it))
    (it-> :content (grab it arg) (sequential? it))))

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
      (assert (enlive-node? node))
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
         (every? truthy? (mapv raw-leaf-node? kids)))))

(s/defn consolidate-raw-kids :- tsk/Vec
  "Consolidates ::raw kids for a node into a single Enlive :content vector"
  [node :- tsk/KeyMap]
  (mapv #(grab ::value %) (grab ::kids node)))

(s/defn tree->enlive :- (s/either tsk/KeyMap tsk/Vec)
  [tree-node :- tsk/KeyMap]
  (assert (tree-node? tree-node))
  (let [enlive-attrs (dissoc tree-node ::kids :tag ::value)
        enlive-base  (glue (submap-by-keys tree-node #{:tag}) {:attrs enlive-attrs})]
       (cond
         (raw-kids-node? tree-node) (let [enlive-leaf (glue enlive-base {:content (consolidate-raw-kids tree-node)})]
                                         enlive-leaf)

         (tree-leaf? tree-node) (do ; (println "tree-leaf")
                                  (let [enlive-leaf (glue enlive-base {:content [(grab ::value tree-node)]})]
                                       enlive-leaf))
         :else (do  ; (println "not tree-leaf")
                 (let [enlive-kids (mapv tree->enlive (grab ::kids tree-node))
                       enlive-node (glue enlive-base {:content enlive-kids})]
                      enlive-node)))))

(s/defn enlive->tree :- tsk/KeyMap ; #todo add test
  "Convert an Enlive-format data structure to a tree. "
  [enlive-tree  :- tsk/KeyMap]
  (assert (enlive-node? enlive-tree))
  (let [attrs   (or (:attrs enlive-tree) {})
        content (or (:content enlive-tree) [])]
       (assert (not (contains-key? attrs :tag)))
    (let
      [attrs  (glue attrs (submap-by-keys enlive-tree #{:tag}))
       result (cond
                (every? enlive-node? content)
                (let [kid-hids (glue [] (for [child content]
                                          (enlive->tree child)))]
                     (glue attrs {::kids kid-hids}))

                (and
                  (= 1 (count content))
                  (not (enlive-node? (only content))))
                (glue attrs {::value (only content) ::kids []})

                :else (let [kid-hids (glue []
                                       (for [child content]
                                         (if (enlive-node? child)
                                           (enlive->tree child)
                                           {:tag ::raw ::value child ::kids []})))]
                           (glue attrs {::kids kid-hids}))) ]
      result)))




(s/defn new-hid :- HID
  "Returns a new HexID"
  []
  (keyword (tm/sha-uuid)))

(s/defn hid->id4  :- s/Keyword
  "Clips an HID to its first 4 hex digits"
  [hid :- HID]
  (keyword (clip-str 4 (kw->str hid))))

(s/defn hid->wid  :- s/Keyword
  "Uses an HID to look up a human-friendly Word-ID (WID) from an English dictionary.
  Useful for debugging purposes."
  [hid :- HID]
  nil)              ; #todo

(s/defn validate-hid
  "Returns HID arg iff it exists in the forest, else throws."
  [hid :- HID]
  (when-not (contains-key? *forest* hid)
    (throw (IllegalArgumentException. (str "validate-hid: HID does not exist=" hid))))
  hid)

(s/defn hid->node :- Node
  "Returns the node corresponding to an HID"
  [hid :- HID]
  (grab hid *forest*))

(s/defn hid->leaf :- Node
  "Returns the leaf node corresponding to an HID"
  [hid :- HID]
  (validate forest-leaf? (hid->node hid)))

(s/defn hid->value :- s/Any
  "Returns the value of a leaf node given an HID"
  [hid :- HID]
  (grab ::value (hid->leaf hid)))

(s/defn hid->attrs :- tsk/KeyMap ; #todo remove OBE
  [hid :- HID]
  (dissoc (hid->node hid) :khids))

(s/defn hid->attr :- s/Any ; #todo remove OBE
  "Given an HID, returns a single attr"
  [hid :- HID
   attr :- s/Keyword]
  (grab attr (hid->node hid)))

(s/defn hid->kids :- [HID]
  [hid :- HID]
  (grab :khids (hid->node hid)))

(s/defn node-hid?  ; #todo remove OBE?
  "Returns true iff an HID is a Node"
  [hid :- HID]
  (forest-node? (hid->node hid)))

(s/defn leaf-hid?
  "Returns true iff an HID is a leaf"
  [hid :- HID]
  (forest-leaf? (hid->node hid)))

(s/defn all-hids :- #{HID} ; #todo test
  "Returns a set of all HIDs in the forest"
  []
  (set (keys *forest*)))

(s/defn all-node-hids :- #{HID}  ; #todo remove OBE?
  "Returns a set of all node HIDs in the forest"
  []
  (all-hids))

(s/defn all-leaf-hids :- #{HID}   ; #todo remove OBE?
  "Returns a set of all leaf HIDs in the forest"
  []
  (set (keep-if leaf-hid? (all-hids))))

(s/defn root-hids :- #{HID}
  "Return a vector of all root HID's"
  []
  (let [kid-hids  (reduce
                    (fn [cum-kids hid]
                      (into cum-kids (grab :khids (hid->node hid))))
                    #{}
                    (all-hids))
        root-hids (set/difference (all-hids) kid-hids)]
    root-hids))

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn hid->tree :- tsk/KeyMap
  [hid :- HID]
  (let [node        (hid->node hid)
        base-result (it-> node
                      (into {} it)
                      (dissoc it :khids))]
    (if (forest-leaf? node)
      ; leaf: nothing else to do
      (glue {::kids []} base-result) ; #todo can clean up more?
      ; Node: need to recursively resolve children
      (let [kid-trees       (mapv hid->tree (grab :khids node))
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
    (set! *forest* (glue *forest* {hid node}))
    node)
  ([hid :- HID
    attrs :- tsk/KeyMap
    kids :- [HID]]
    (let [node (glue (->Node kids) attrs)]
      (set-node hid node)
      node)))

; #todo remove
;(s/defn set-leaf
;  "Unconditionally sets the value of a Leaf in the forest"
;  [hid :- HID
;   attrs :- tsk/KeyMap
;   value :- s/Any]
;  (let [leaf (->Node attrs value)]
;    (set-node hid leaf)
;    leaf))

(s/defn validate-attrs
  [attrs :- tsk/KeyMap]
  (let [illegal-value?   (s/fn fn-illegal-value [arg] (or (= arg :*) (= arg :**))) ]
    (when (has-some? illegal-value? (keyvals attrs))
      (throw (IllegalArgumentException. (str "validate-attrs: failed attrs=" (pr-str attrs)))))
    attrs))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn add-node :- HID
  [attrs-arg :- (s/either tsk/KeyMap s/Keyword); #todo merge args
   kid-hids :- [HID]]
  (doseq [kid kid-hids] (validate-hid kid))
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {:tag (validate keyword? attrs-arg)} )
        hid (new-hid)]
    (validate-attrs attrs)
    (set-node hid attrs kid-hids)
    hid))

(s/defn add-leaf :- HID  ; #todo remove duplication
  [attrs-arg :- (s/either tsk/KeyMap s/Keyword); #todo merge args
   value :- s/Any ]
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {:tag (validate keyword? attrs-arg)} )
        attrs (glue attrs {::value value}) ]
    (add-node attrs [])))

(s/defn add-tree :- HID
  "Adds a tree to the forest."
  [node]
  (when-not (forest-node? node)
    (throw (IllegalArgumentException. (str "add-tree: invalid element=" node))))
  (let [kid-hids (glue [] ; glue to an empty vec in case no kids
                   (for [child (grab ::kids node)] ; #todo forv & no glue ???
                     (add-tree child)))]
       (add-node node kid-hids)))

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
      (glue attrs {::value (only others)}))))

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

(s/defn add-tree-hiccup :- HID
  "Adds a Hiccup-format tree to the forest. Tag values are converted to nil attributes:
  [:a ...] -> {:a nil ...}..."
  [arg]
  (add-tree (hiccup->tree arg)))

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
                     (grab :khids it)
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

(s/defn value-set :- Node
  "Resets the value of a leaf"
  [hid :- HID
   value-new :- s/Any]
  (let [leaf-curr  (hid->leaf hid)
        leaf-new   (glue leaf-curr {::value value-new}) ]
    (set-node hid leaf-new)
    leaf-new))

(s/defn value-update :- Node
  "Given a leaf with a value, updates that value using a function"
  [hid :- HID
   fn-update-value  ; signature: (fn-update-value value-curr x y z & more) -> value-new
   & fn-update-value-args]
  (let [leaf-curr  (hid->leaf hid)
        value-curr (grab ::value leaf-curr)
        value-new  (apply fn-update-value value-curr fn-update-value-args)
        leaf-new   (glue leaf-curr {::value value-new})]
    (set-node hid leaf-new)
    leaf-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-set :- Node
  "Resets the kids of a Node to the supplied list"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr  (hid->node hid)
        node-new   (glue node-curr {::kids kids-new})]
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
        kids-curr (grab ::kids node-curr)
        kids-new  (apply fn-update-kids kids-curr fn-update-kids-args)
        node-new  (glue node-curr {::kids kids-new})]
    (set-node hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-append :- tsk/KeyMap
  "Appends a list of kids a Node"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr (hid->node hid)
        kids-curr (grab ::kids node-curr)
        kids-new  (glue kids-curr kids-new)
        node-new  (glue node-curr {::kids kids-new})]
       (set-node hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-prepend :- tsk/KeyMap
  "Appends a list of kids a Node"
  [hid :- HID
   kids-in :- [HID]]
  (let [node-curr (hid->node hid)
        kids-curr (grab ::kids node-curr)
        kids-new  (glue kids-in kids-curr)
        node-new  (glue node-curr {::kids kids-new})]
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
          kids-curr           (grab ::kids node-curr)
          missing-kids        (set/difference kids-leaving (into #{} kids-curr))
          _                   (when (and (not-empty? missing-kids) report-missing-kids)
                                (throw (IllegalArgumentException.
                                         (str "remove-kids: missing-kids found=" missing-kids))))
          kid-is-leaving?     (fn fn-kid-is-leaving? [kid] (contains-key? kids-leaving kid))
          kids-new            (drop-if kid-is-leaving? kids-curr)
          node-new            (glue node-curr {::kids kids-new})]
      (set-node hid node-new)
      node-new)))

(s/defn remove-all-kids :- tsk/KeyMap
  "Removes all children from a Node."
  ([hid :- HID ]
    (let [node-curr   (hid->node hid)
          node-new    (glue node-curr {::kids []})]
      (set-node hid node-new)
      node-new)))

(s/defn remove-hid :- #{HID}
  "Removes one or more nodes and all references to them from the database. May create orphaned nodes."
  [& hids-leaving :- [HID]]
  (let [hids-leaving (set hids-leaving)]
    (doseq [hid hids-leaving]
      (validate-hid hid))
    (set! *forest* (reduce
                     (fn fn-dissoc-elems [curr-forest hid]
                       (dissoc curr-forest hid))
                     *forest*
                     hids-leaving))
    ; Remove any kid references to deleted nodes
    (let [hids-staying (keys *forest*)]
       (doseq [hid hids-staying]
         (remove-kids hid hids-leaving true))) ; true => missing-kids-ok
    hids-leaving))

(s/defn ^:no-doc hid-matches?
  "Returns true if an HID node matches a pattern"
  [hid :- HID
   pattern-in :- s/Any]
  (let [node    (hid->node hid)
        pattern (cond
                  (map?         pattern-in)  pattern-in
                  (sequential?  pattern-in)  (zipmap pattern-in (repeat nil))
                  (keyword?     pattern-in)  {:tag pattern-in}
                  :else (throw (IllegalArgumentException.
                                 (str "hid-matches?: illegal pattern-in=" pattern-in))))]
    (let [pattern-keys         (keys pattern)
          pattern-keys-set     (set pattern-keys)
          node-keys-set        (set (keys node))
          pattern-keys-missing (set/difference pattern-keys-set node-keys-set)]
      (if (not-empty? pattern-keys-missing)
        false
        (let [attrs-tst    (submap-by-keys node pattern-keys-set)
              ; replace any nil values with wildcard :*
              pattern-wild (apply glue (for [[k v] pattern]
                                         {k (if (nil? v) :* v)}))]
          (wild-match? pattern-wild attrs-tst))))))

; #todo list-roots
; #todo list-non-roots
; #todo list-leaves

; #todo list-cycle-nodes (whose kid is an ancestor)

; #todo find-node, find-node, find-leaf
; #todo find-node, find-node, find-leaf
; #todo find-roots function (& root for sole root or throw)

; #todo (find-leaf root [ :a :b  :c ] ) ->
; #todo (find-leaf root [ :a :b  {:tag :c ::value <val> ::kids []} ] )

;---------------------------------------------------------------------------------------------------
(s/defn format-path
  [hids :- [HID]]
  (let [[hid-curr & hid-rest] hids]
    (if (empty? hid-rest)
      (hid->bush hid-curr)
      [ (hid->node hid-curr) (format-path hid-rest) ] )))

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

(def HidRootSpec (s/either HID [HID] #{HID}))

; #todo need a find-paths-pred that takes a predicate fn to choose
; #todo maybe a fn like postwalk to apply transformation fn to each node recursively
(s/defn find-paths :- [[HID]]    ; #todo need update-tree & update-leaf fn's
  "Searches an Enlive-format tree for the specified tgt-path"
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec ]
  (when (empty? tgt-path)
    (throw (IllegalStateException. "find-paths: tgt-path is empty")))
  (when (= :** (last tgt-path))
    (throw (IllegalArgumentException. "find-paths: recursive-wildcard `:**` cannot terminate tgt-path")))

  (let [result-atom (atom [])
        roots (cond
                (hid? root-spec)     #{root-spec} ; scalar arg -> wrap in a set
                (vector? root-spec)  (set root-spec) ; vec of root hids -> convert to set
                (set? root-spec)     root-spec ; set of root hids -> use it as-is
                :else (throw (IllegalArgumentException. (str "find-paths: invalid root-spec=" root-spec)))) ]
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

(s/defn find-leaf-paths  :- [[HID]]    ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- tsk/Vec
   tgt-value :- s/Any]
  (let [paths          (find-paths root-spec tgt-path)
        matching-leaf? (fn fn-matching-leaf?
                         [path tgt-val]
                         (let [tail-hid (last path)]
                            (and (leaf-hid? tail-hid)
                              (or (= tgt-val :*)
                                (= tgt-val (hid->value tail-hid))))))
        leaf-paths     (keep-if #(matching-leaf? % tgt-value) paths) ]
       leaf-paths))

(s/defn find-leaf-hids :- [HID]     ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   tgt-value :- s/Any]
  (mapv last (find-leaf-paths root-spec tgt-path tgt-value)) )

(s/defn find-leaf-hid
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   tgt-value :- s/Any]
  (only (find-leaf-hids root-spec tgt-path tgt-value)))

(s/defn find-leaf
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   tgt-value :- s/Any]
  (hid->leaf (find-leaf-hid root-spec tgt-path tgt-value)))

(s/defn find-tree     ; #todo need test (maybe delete?)
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any] ]
  (hid->tree (find-hid root-spec tgt-path)))

(s/defn find-leaf-value     ; #todo need test (maybe delete?)
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any] ]
  (hid->value (find-hid root-spec tgt-path)))

(s/defn find-paths-with
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   path-pred :- s/Any] ; #todo how func spec?
  (let [paths-found (find-paths root-spec tgt-path)
        keepers     (keep-if path-pred paths-found)]
       keepers))

(s/defn find-hids-with
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   hid-pred :- s/Any] ; #todo how func spec?
  (let [paths-found (find-paths root-spec tgt-path)
        hids-found  (mapv last paths-found)
        hids-keep   (keep-if hid-pred hids-found)]
       hids-keep))

(s/defn has-child-path? ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any] ]
  (pos? (count (find-paths root-spec tgt-path))))

(s/defn has-child-path-with? ; #todo need test
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   path-pred :- s/Any] ; #todo how func spec?
  (pos? (count (find-paths-with root-spec tgt-path path-pred))))

(s/defn has-child-leaf?
  [root-spec :- HidRootSpec
   tgt-path :- [s/Any]
   tgt-value :- s/Any]
  (pos? (count (find-leaf-hids root-spec tgt-path tgt-value))))
