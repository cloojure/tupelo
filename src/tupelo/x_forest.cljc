;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.x-forest
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

(s/defn enlive-node? :- s/Bool ; #todo add test and -> tupelo.core
  [arg]
  (and (map? arg)
    (= #{:tag :attrs :content} (set (keys arg)))))

(defn hiccup->enlive
  "Converts a tree of data from Hiccup -> Enlive format"
  [tree-node]
  (if-not (sequential? tree-node)
    tree-node       ; leaf - just return it
    (let [tag    (xfirst tree-node)
          less-1 (xrest tree-node)]
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

(defn enlive->hiccup
  [tree-node]
  (if-not (map? tree-node)
    tree-node       ; leaf - just return it
    (with-map-vals tree-node [tag attrs content]
      (let [tag-attrs  (if (empty? attrs)
                         [tag]
                         [tag attrs])
            content-tx (forv [child content]
                         (enlive->hiccup child))
            result     (glue tag-attrs content-tx)]
        result))))



; #todo  move to tupelo.x-tree (tupelo.x-datapig ?)
; forest  data-forest  ForestDb forest-db
; Sherwood  weald  wald  boreal

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(def ^:dynamic *forest* nil)

(defn validate-forest []
  (when-not (map? *forest*)
    (throw (IllegalArgumentException. (str "validate-forest: failed forest=" *forest*)))))

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
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

; :hid is short for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Element }
(defn new-forest ; #todo -> new-forest
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

(defrecord Node [attrs kids] )    ; { :attrs { :k1 v1 :k2 v2 ... }  :kids  [hid...] }
(defrecord Leaf [attrs value])    ; { :attrs { :k1 v1 :k2 v2 ... }  :value s/Any    }
(def Element (s/either Node Leaf))

(def HID s/Keyword) ; #todo find way to validate

(s/defn tree-node? :- s/Bool
  [elem :- tsk/KeyMap]
  (or (instance? Node elem)
    (= #{:attrs :kids} (set (keys elem)))))

(s/defn tree-leaf? :- s/Bool
  [elem :- tsk/KeyMap]
  (or (instance? Leaf elem)
    (= #{:attrs :value} (set (keys elem)))))

(s/defn tree-elem? :- s/Bool
  [elem :- tsk/KeyMap]
  (or (tree-node? elem) (tree-leaf? elem)))

(s/defn hid? :- s/Bool
  [arg]
  (and (keyword? arg)
    (let [name-str (kw->str arg)]
      (and (ts/hex? name-str)
        (= 40 (count name-str))))))

(s/defn new-hid :- HID
  []
  (keyword (tm/sha-uuid)))

(s/defn hid->id4  :- s/Keyword
  [hid :- HID]
  (keyword (clip-str 4 (kw->str hid))))

(s/defn hid->wid  :- s/Keyword
  "Uses an HID to look up a human-friendly Word-ID (WID) from an English dictionary.
  Useful for debugging purposes."
  [hid :- HID]
  nil)              ; #todo

(s/defn validate-hid
  "Returns true iff an HID exists in the forest, else throws."
  [hid :- HID]
  (when-not (contains-key? *forest* hid)
    (throw (IllegalArgumentException. (str "validate-hid: HID does not exist=" hid))))
  hid)

(s/defn hid->elem :- Element
  [hid :- HID]
  (grab hid *forest*))

(s/defn hid->node :- Node
  [hid :- HID]
  (validate tree-node? (hid->elem hid)))

(s/defn hid->leaf :- Leaf
  [hid :- HID]
  (validate tree-leaf? (hid->elem hid)))

(s/defn hid->attrs :- tsk/KeyMap
  [hid :- HID]
  (grab :attrs (hid->elem hid)))

(s/defn hid->kids :- [HID]
  [hid :- HID]
  (grab :kids (hid->node hid)))

(s/defn hid->value :- s/Any
  [hid :- HID]
  (grab :value (hid->leaf hid)))

(s/defn node-hid?
  "Returns true iff an HID is a Node"
  [hid :- HID]
  (instance? Node (hid->elem hid)))

(s/defn leaf-hid?
  "Returns true iff an HID is a Leaf"
  [hid :- HID]
  (instance? Leaf (hid->elem hid)))

(s/defn root-hids :- #{HID}
  "Return a vector of all root HID's"
  []
  (let [all-hids  (set (keys *forest*))
        kid-hids  (reduce
                    (fn [cum-kids hid]
                      (let [elem (hid->elem hid)]
                        (if (tree-node? elem)
                          (into cum-kids (grab :kids elem))
                          cum-kids)))
                    #{}
                    all-hids)
        root-hids (set/difference all-hids kid-hids)]
    root-hids))

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn hid->tree :- tsk/KeyMap
  [hid :- HID]
  (let [elem        (hid->elem hid)
        base-result (into {} elem)]
    (if (instance? Node elem)
      ; Node: need to recursively resolve children
      (let [kids            (mapv hid->tree (grab :kids elem))
            resolved-result (assoc base-result :kids kids)]
        resolved-result)
      ; Leaf: nothing else to do
      base-result)))

; #todo naming choices
; #todo reset! vs  set
; #todo swap!  vs  update
; #todo remove  vs  delete  vs drop

(s/defn ^:private set-elem
  "Unconditionally sets the value of an Element in the forest"
  [hid :- HID
   elem :- Element]
  (set! *forest* (glue *forest* {hid elem} ))
  elem)

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn set-node
  "Unconditionally sets the value of a Node in the forest"
  [hid :- HID
   attrs :- tsk/KeyMap
   kids :- [HID] ]
  (let [node (->Node attrs kids)]
    (set-elem hid node)
    node))

(s/defn set-leaf
  "Unconditionally sets the value of a Leaf in the forest"
  [hid :- HID
   attrs :- tsk/KeyMap
   value :- s/Any]
  (let [leaf (->Leaf attrs value)]
    (set-elem hid leaf)
    leaf))

(s/defn validate-attrs
  [attrs :- tsk/KeyMap]
  (let [illegal-value? (s/fn fn-illegal-value [arg] (or (= arg :*) (= arg :**)))]
    (when (has-some? illegal-value? (keyvals attrs))
      (throw (IllegalArgumentException. (str "validate-attrs: failed attrs=" (pr-str attrs)))))
    attrs))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn add-node :- HID
  [attrs-arg :- (s/either tsk/KeyMap s/Keyword)
   kids :- [s/Keyword]]
  (doseq [kid kids] (validate-hid kid))
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {:tag (validate keyword? attrs-arg)} )
        hid (new-hid)]
    (validate-attrs attrs)
    (set-node hid attrs kids)
    hid))

(s/defn add-leaf :- HID
  [attrs-arg :- (s/either tsk/KeyMap s/Keyword)
   value]         ; #todo   :- s/Any
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {:tag (validate keyword? attrs-arg)} )
        hid (new-hid)]
    (validate-attrs attrs)
    (set-leaf hid attrs value)
    hid))

(s/defn add-tree :- HID
  "Adds a tree to the forest."
  [tree]
  (assert (tree-elem? tree))
  (let [attrs (grab :attrs tree)]
    (cond
      (tree-node? tree) (let [kids (glue [] ; glue to an empty vec in case no kids
                                     (for [child (grab :kids tree)]
                                       (add-tree child)))]
                          (add-node attrs kids))
      (tree-leaf? tree) (add-leaf attrs (grab :value tree))
      :else (throw (IllegalArgumentException. (str "add-tree: invalid element=" tree))))))

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
      (let [kids (glue [] (for [item others] (bush->tree item)))]
        (vals->map attrs kids))
      {:attrs attrs :value (only others)})))

(s/defn tree->bush :- tsk/Vec
  [tree-elem :- tsk/Map]
  (assert (tree-elem? tree-elem))
  (if (tree-node? tree-elem)
    (let [bush-kids (mapv tree->bush (grab :kids tree-elem))
          bush-node (prepend (grab :attrs tree-elem) bush-kids)]
      bush-node)
    (let [bush-leaf [ (grab :attrs tree-elem) (grab :value tree-elem) ]]
      bush-leaf)))  ; #todo throw if not node or leaf

(s/defn tree->enlive :- tsk/KeyMap
  [tree-elem :- tsk/Map]
  (assert (tree-elem? tree-elem))
  (let [tree-attrs   (grab :attrs tree-elem)
        enlive-base  (submap-by-keys tree-attrs #{:tag})
        enlive-attrs (into {} (drop-if #(= :tag (key %)) (vec tree-attrs)))]
    (if (tree-node? tree-elem)
      (let [enlive-kids (mapv tree->enlive (grab :kids tree-elem))
            enlive-node (glue enlive-base {:attrs enlive-attrs :content enlive-kids})]
        enlive-node)
      (let [enlive-leaf (glue enlive-base {:attrs enlive-attrs :content [(grab :value tree-elem)]})]
        enlive-leaf)))) ; #todo throw if not node or leaf

(s/defn enlive->tree :- tsk/KeyMap ; #todo add test
  "Convert an Enlive-format data structure to a tree. "
  [enlive-tree]
  (assert (enlive-node? enlive-tree))
  (with-map-vals enlive-tree [attrs content]
    (assert (not (contains-key? attrs :tag)))
    (let [attrs (glue attrs (submap-by-keys enlive-tree #{:tag}))]
      (if (every? enlive-node? content)
        (let [kids (glue [] (for [child content] (enlive->tree child)))]
          (vals->map attrs kids))
        {:attrs attrs :value (only content)}))))

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
  (-> arg hiccup->enlive enlive->tree ))

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

(s/defn attrs-reset :- tsk/KeyMap
  "Replace the attrs of a Node or Leaf with the supplied attrs map"
  [hid :- HID
   attrs-new :- tsk/KeyMap]
  (validate-attrs attrs-new)
  (let [elem-curr  (hid->elem hid)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem hid elem-new)
    elem-new))

(s/defn attrs-merge :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [hid :- HID
   attrs-in :- tsk/KeyMap]
  (let [elem-curr  (hid->elem hid)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (glue attrs-curr attrs-in)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (validate-attrs attrs-new)
    (set-elem hid elem-new)
    elem-new))

(s/defn attr-get :- tsk/KeyMap ; #todo test
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   attr-name :- s/Keyword ]
  (fetch-in (hid->elem hid) [:attrs attr-name] ))

(s/defn attr-update :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   attr-name :- s/Keyword
   fn-update-attr        ; signature: (fn-update-attr attr-curr x y z & more) -> attr-new
   & fn-update-attr-args]
  (let [elem-curr  (hid->elem hid)
        attr-val-curr  (fetch-in elem-curr [:attrs attr-name] )
        attr-val-new   (apply fn-update-attr attr-val-curr fn-update-attr-args)
        elem-new   (assoc-in elem-curr [:attrs attr-name] attr-val-new) ]
    (validate-attrs (grab :attrs elem-new))
    (set-elem hid elem-new)
    elem-new))

(s/defn attr-remove :- tsk/KeyMap
  "Removes the specified attribute for an element"
  [hid :- HID
   attr :- s/Keyword]
  (let [attrs-curr (hid->attrs hid)
        attrs-new  (dissoc attrs-curr attr)]
    (attrs-reset hid attrs-new)))

(s/defn value-set :- Leaf
  "Resets the value of a Leaf"
  [hid :- HID
   value-new :- s/Any]
  (let [leaf-curr  (hid->leaf hid)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem hid leaf-new)
    leaf-new))

(s/defn value-update :- Leaf
  "Given a leaf with a value, updates that value using a function"
  [hid :- HID
   fn-update-value  ; signature: (fn-update-value value-curr x y z & more) -> value-new
   & fn-update-value-args]
  (let [leaf-curr  (hid->leaf hid)
        value-curr (grab :value leaf-curr)
        value-new  (apply fn-update-value value-curr fn-update-value-args)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem hid leaf-new)
    leaf-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-set :- Node
  "Resets the kids of a Node to the supplied list"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr  (hid->node hid)
        node-new   (glue node-curr {:kids kids-new})]
    (set-elem hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-update :- tsk/KeyMap
  "Updates the kids for a Node using a function, as in clojure.core/update"
  [hid :- HID
   fn-update-kids   ; signature: (fn-update kids-curr x y z & more) -> kids-new
   & fn-update-kids-args]
  (let [elem-curr (hid->elem hid)
        kids-curr (grab :kids elem-curr)
        kids-new  (apply fn-update-kids kids-curr fn-update-kids-args)
        elem-new  (glue elem-curr {:kids kids-new})]
    (set-elem hid elem-new)
    elem-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn kids-append :- tsk/KeyMap
  "Appends a list of kids a Node"
  [hid :- HID
   kids-new :- [HID]]
  (let [elem-curr (hid->elem hid)
        kids-curr (grab :kids elem-curr)
        kids-new  (glue kids-curr kids-new)
        elem-new  (glue elem-curr {:kids kids-new})]
    (set-elem hid elem-new)
    elem-new))

; #todo (s/defn remove-orphans [roots-to-keep] ...)

(s/defn kids-remove :- tsk/KeyMap
  "Removes all a set of children from a Node (including any duplcates)."
  ([hid :- HID
    kids-leaving :- (s/either [HID] #{HID})]
    (kids-remove hid kids-leaving false))
  ([hid :- HID
    kids-leaving :- (s/either [HID] #{HID})
    missing-kids-ok :- s/Bool]
    (let [kids-leaving        (set kids-leaving)
          report-missing-kids (not missing-kids-ok)
          node-curr           (hid->node hid)
          kids-curr           (grab :kids node-curr)
          missing-kids        (set/difference kids-leaving (into #{} kids-curr))
          _                   (when (and (not-empty? missing-kids) report-missing-kids)
                                (throw (IllegalArgumentException.
                                         (str "remove-kids: missing-kids found=" missing-kids))))
          kid-is-leaving?     (fn fn-kid-is-leaving? [kid] (contains-key? kids-leaving kid))
          kids-new            (drop-if kid-is-leaving? kids-curr)
          node-new            (glue node-curr {:kids kids-new})]
      (set-elem hid node-new)
      node-new)))

(s/defn kids-remove-all :- tsk/KeyMap
  "Removes all a set of children from a Node (including any duplcates)."
  ([hid :- HID ]
    (let [node-curr           (hid->node hid)
          node-new            (glue node-curr {:kids []})]
      (set-elem hid node-new)
      node-new)))

(s/defn remove-elems :- #{HID}
  "Removes a set of elements and all references to them from the database. May create orphaned elements."
  [hids-leaving :- #{HID}]
  (doseq [hid hids-leaving]
    (validate-hid hid))
  (set! *forest* (reduce
               (fn fn-dissoc-elems [curr-forest hid]
                 (dissoc curr-forest hid))
               *forest*
               hids-leaving))
  ; Remove any kid references to deleted elements
  (let [hids-staying (keys *forest*)]
    (doseq [hid hids-staying]
      (let [elem (hid->elem hid)]
        (when (instance? Node elem)
          (kids-remove hid hids-leaving true))))) ; true => missing-kids-ok
  hids-leaving)

(s/defn hid-matches?
  [hid :- HID
   pattern-in :- s/Any]
  (let [attrs   (hid->attrs hid)
        pattern (cond
                  (map?         pattern-in)  pattern-in
                  (sequential?  pattern-in)  (zipmap pattern-in (repeat nil))
                  (keyword?     pattern-in)  {:tag pattern-in}
                  :else (throw (IllegalArgumentException.
                                 (str "hid-matches?: illegal pattern-in=" pattern-in))))]
    (let [pattern-keys         (keys pattern)
          pattern-keys-set     (set pattern-keys)
          attrs-keys-set       (set (keys attrs))
          pattern-keys-missing (set/difference pattern-keys-set attrs-keys-set)]
      (if (not-empty? pattern-keys-missing)
        false
        (let [attrs-tst    (submap-by-keys attrs pattern-keys-set)
              ; replace any nil values with wildcard :*
              pattern-wild (apply glue (for [[k v] pattern]
                                         {k (if (nil? v) :* v)}))]
          (wild-match? pattern-wild attrs-tst))))))

; #todo list-roots
; #todo list-non-roots
; #todo list-leaves

; #todo list-cycle-nodes (whose kid is an ancestor)

; #todo find-elem, find-node, find-leaf
; #todo find-elem, find-node, find-leaf
; #todo find-roots function (& root for sole root or throw)

;---------------------------------------------------------------------------------------------------


(s/defn format-path
  [hids :- [HID]]
  (let [[hid-curr & hid-rest] hids]
    (if (empty? hid-rest)
      (hid->bush hid-curr)
      [ (hid->attrs hid-curr) (format-path hid-rest) ] )))

(s/defn format-paths [solns :- #{ [HID] }]
  (set (forv [soln solns]
         (format-path soln))))


(s/defn ^:private ^:no-doc find-paths-impl
  [result-atom
   parents :- [HID]
   hid :- HID
   tgt-path :- [(s/either s/Keyword tsk/KeyMap)] ]
  (validate-hid hid)
  (when (not-empty? tgt-path)
    (let [tgt (xfirst tgt-path)
              tgt-path-rest (xrest tgt-path)
              attrs (hid->attrs hid)]
      (let [parents-new (append parents hid)]
        (when (or (= tgt :*) (hid-matches? hid tgt))
          ;(println :200 (str "match attrs=" attrs ))
          (if (empty? tgt-path-rest)
            (let [soln parents-new]
              ;(println :210 "empty soln:" (mapv #(hid->attrs %) soln))
              (swap! result-atom glue #{soln}))
            (do
              ;(println :220 "NOT (empty? tgt-path-rest) parents-new=" (mapv #(hid->attrs %) parents-new))
              (when (node-hid? hid)
                ;(println :221)
                (doseq [kid (hid->kids hid)]
                  ;(println :230 "kid=" (hid->attrs kid))
                  (find-paths-impl result-atom parents-new kid tgt-path-rest))))))
        (when (= tgt :**)
          ;(println :300 "tgt = :**")
          (when (not-empty? tgt-path-rest) ; :** wildcard cannot terminate the tgt-path
            ;(println :320 ":** parents-new:" (mapv #(hid->attrs %) parents-new))
            ;(println (str :330 "  recurse  parents:" (mapv #(hid->attrs %) parents)
            ;           "   hid:" (hid->attrs hid) "  tgt-path-rest:" tgt-path-rest))
            (find-paths-impl result-atom parents hid tgt-path-rest)
            (when (node-hid? hid)
              (doseq [kid (hid->kids hid)]
                ;(println :340 ":** kid:" (hid->attrs kid))
                ;(println (str :350 "    recurse  parents-new:" (mapv #(hid->attrs %) parents-new)
                ;           "  tgt-path:" tgt-path))
                (find-paths-impl result-atom parents-new kid tgt-path)))))))))

; #todo need a find-paths-pred that takes a predicate fn to choose
; #todo maybe a fn like postwalk to apply transformation fn to each node recursively
(defn find-paths    ; #todo need update-tree & update-leaf fn's
  "Searches an Enlive-format tree for the specified tgt-path"
  [root-spec tgt-path]
  (when (empty? tgt-path)
    (throw (IllegalStateException. "find-paths: tgt-path is empty")))
  (when (= :** (last tgt-path))
    (throw (IllegalArgumentException. "find-paths: recursive-wildcard `:**` cannot terminate tgt-path")))

  (let [result-atom (atom #{})
        roots (cond
                (hid? root-spec) [root-spec] ; scalar arg -> wrap in a vector
                (set? root-spec) root-spec ; set of root hids -> use it as-is
                :else (throw (IllegalArgumentException. (str "find-paths: invalid root-spec=" root-spec)))) ]
    (doseq [root roots]
      (find-paths-impl result-atom [] root tgt-path))
    @result-atom))

(defn find-hids     ; #todo need test
  [root-spec tgt-path]
  (mapv last (find-paths root-spec tgt-path)))

(defn find-hid     ; #todo need test
  [root-spec tgt-path]
  (only (find-hids root-spec tgt-path)))

(defn find-tree     ; #todo need test
  [root-spec tgt-path]
  (hid->tree (find-hid root-spec tgt-path)))

(defn find-value     ; #todo need test
  [root-spec tgt-path]
  (grab :value (find-tree root-spec tgt-path)))

(s/defn leaf->value     ; #todo need test
  [leaf-hid :- HID]
  (grab :value (hid->tree leaf-hid)))

(defn- has-matching-leaf
  [path tgt-val]
  (let [tail-hid  (last path)
        tail-elem (hid->elem tail-hid)]
    (and (leaf-hid? tail-hid)
      (or (= tgt-val (grab :value tail-elem))
        (= tgt-val :*)))))

(defn find-paths-leaf     ; #todo need test
  [root-spec tgt-path tgt-value]
  (let [paths      (find-paths root-spec tgt-path)
        leaf-paths (keep-if #(has-matching-leaf % tgt-value) paths) ]
    leaf-paths))

(defn find-leaf-hids     ; #todo need test
  [root-spec tgt-path tgt-value]
  (mapv last (find-paths-leaf root-spec tgt-path tgt-value)) )

(defn find-leaf
  [root-spec tgt-path tgt-value]
  (hid->leaf (last (only (find-paths-leaf root-spec tgt-path tgt-value))))
)

