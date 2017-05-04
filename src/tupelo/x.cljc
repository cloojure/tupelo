;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.x
  "Experimental new code"
  (:require
    [clj-uuid :as uuid]
    [clojure.core.async     :as ca :refer [go go-loop chan thread]]
    [clojure.pprint :as pprint]
    [clojure.set :as set]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.async :as ta]
    [tupelo.core :as t]
    [tupelo.enlive :as te]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)


; #todo  move to tupelo.x-tree (tupelo.x-datapig ?)
; forest  data-forest  ForestDb forest-db
; Sherwood  weald  wald  boreal

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(def ^:dynamic *forest** nil)

(defn validate-forest []
  (when-not (map? *forest**)
    (throw (IllegalArgumentException. (str "validate-forest: failed forest=" *forest**)))))

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(defmacro with-forest ; #todo -> with-forest
  [forest-arg & forms]
  `(binding [*forest** ~forest-arg]
     (validate-forest)
     ~@forms
     *forest**))

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

(defrecord Node [attrs kids] )    ; { :attrs { :k1 v1 :k2 v2 ... }  :kids  [ hid...] }
(defrecord Leaf [attrs value])    ; { :attrs { :k1 v1 :k2 v2 ... }  :value s/Any     }
(def Element (s/either Node Leaf))

(def HID s/Keyword) ; #todo find way to validate

(s/defn node-elem? :- s/Bool
  [elem :- tsk/KeyMap]
  (or (instance? Node elem)
    (= #{:attrs :kids} (set (keys elem)))))

(s/defn leaf-elem? :- s/Bool
  [elem :- tsk/KeyMap]
  (or (instance? Leaf elem)
    (= #{:attrs :value} (set (keys elem)))))

(s/defn hid? :- s/Bool
  [arg :- s/Keyword]
  (assert (keyword? arg))
  (let [name-str (name arg)]
    (and (ts/hex? name-str)
      (= 40 (count name-str)))))

(s/defn new-hid :- HID
  []
  (keyword (tm/sha-uuid)))

(s/defn hid->id4  :- s/Keyword
  [hid :- HID]
  (keyword (clip-str 4 (name hid))))

(s/defn hid->wid  :- s/Keyword
  "Uses an HID to look up a human-friendly Word-ID (WID) from an English dictionary.
  Useful for debugging purposes."
  [hid :- HID]
  nil)              ; #todo

(s/defn validate-hid
  "Returns true iff an HID exists in the forest"
  [hid :- HID]
  (when-not (contains-key? *forest** hid)
    (throw (IllegalArgumentException. (str "validate-hid: HID does not exist=" hid))))
  hid)

(s/defn hid->elem :- Element
  [hid :- HID]
  (grab hid *forest**))

(s/defn hid->node :- Node
  [hid :- HID]
  (validate node-elem? (hid->elem hid)))

(s/defn hid->leaf :- Leaf
  [hid :- HID]
  (validate leaf-elem? (hid->elem hid)))

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

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn hid->tree :- tsk/KeyMap
  [hid :- HID]
  (let [elem (hid->elem hid)
        base-result (into {} elem)]
    (if (instance? Node elem)
      ; Node: need to recursively resolve children
      (let [kids   (mapv hid->tree (grab :kids elem))
            resolved-result (assoc base-result :kids kids) ]
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
  (set! *forest** (glue *forest** {hid elem} ))
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
   value :- s/Any ]
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
                {(validate keyword? attrs-arg) nil} )
        hid (new-hid)]
    (validate-attrs attrs)
    (set-node hid attrs kids)
    hid))

(s/defn add-leaf :- HID
  [attrs-arg :- (s/either tsk/KeyMap s/Keyword)
   value :- s/Any]
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {(validate keyword? attrs-arg) nil} )
        hid (new-hid)]
    (validate-attrs attrs)
    (set-leaf hid attrs value)
    hid))

(s/defn add-tree :- HID
  "Adds an Enlive-format tree to the DB. Tag values are converted to nil attributes:
  [:a ...] -> {:a nil ...}..."
  [tree]
  (assert (te/enlive-node? tree))
  (let [attrs    (glue {(grab :tag tree) nil} ; or { :tag <tag-val> }
                   (grab :attrs tree))
        children (grab :content tree) ]
    (if (every? te/enlive-node? children)
      (let [kids (glue [] (for [child children] (add-tree child))) ]
        (add-node attrs kids))
      (add-leaf attrs children))))

(s/defn add-tree-hiccup :- HID
  "Adds a Hiccup-format tree to the DB. Tag values are converted to nil attributes:
  [:a ...] -> {:a nil ...}..."
  [tree]
  (add-tree (te/hiccup->enlive tree)))

(s/defn set-attrs :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [hid :- HID
   attrs-new :- tsk/KeyMap]
  (validate-attrs attrs-new)
  (let [elem-curr  (hid->elem hid)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem hid elem-new)
    elem-new))

(s/defn merge-attrs :- tsk/KeyMap
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

(s/defn update-attrs :- tsk/KeyMap
  "Use the supplied function & arguments to update the attrs map for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   fn-update-attrs   ; signature: (fn-update attrs-curr x y z & more) -> attrs-new
   & fn-update-attrs-args ]
  (let [elem-curr  (hid->elem hid)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (apply fn-update-attrs attrs-curr fn-update-attrs-args)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (validate-attrs attrs-new)
    (set-elem hid elem-new)
    elem-new))

(s/defn update-attr :- tsk/KeyMap
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

(s/defn remove-attr :- tsk/KeyMap
  "Removes the specified attribute for an element"
  [hid :- HID
   attr :- s/Keyword ]
  (let [fn-update-attrs (fn fn-update-attrs [attrs-curr]
                          (let [attrs-new (dissoc attrs-curr attr) ]
                            attrs-new))]
    (update-attrs hid fn-update-attrs)))

(s/defn set-value :- Leaf
  "Resets the value of a Leaf"
  [hid :- HID
   value-new :- s/Any]
  (let [leaf-curr  (hid->leaf hid)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem hid leaf-new)
    leaf-new))

(s/defn update-value :- Leaf
  "Updates the value of a Leaf using a function"
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
(s/defn set-kids :- Node
  "Resets the kids of a Node to the supplied list"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr  (hid->node hid)
        node-new   (glue node-curr {:kids kids-new})]
    (set-elem hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn update-kids :- tsk/KeyMap
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
(s/defn add-kids :- tsk/KeyMap
  "Appends a list of kids a Node"
  [hid :- HID
   kids-new :- [HID]]
  (let [elem-curr (hid->elem hid)
        kids-curr (grab :kids elem-curr)
        kids-new  (glue kids-curr kids-new)
        elem-new  (glue elem-curr {:kids kids-new})]
    (set-elem hid elem-new)
    elem-new))

(s/defn remove-kids :- tsk/KeyMap
  "Removes all a set of children from a Node (including any duplcates)."
  ([hid :- HID
    kids-leaving :- #{HID}]
    (remove-kids hid kids-leaving false))
  ([hid :- HID
    kids-leaving :- #{HID}
    missing-kids-ok :- s/Bool]
    (let [report-missing-kids (not missing-kids-ok)
          node-curr           (hid->node hid)
          kids-curr           (grab :kids node-curr)
          missing-kids        (set/difference kids-leaving (into #{} kids-curr))
          _                   (when (and (not-empty? missing-kids) report-missing-kids)
                                (throw (IllegalArgumentException.
                                         (str "remove-kids: missing-kids found=" missing-kids))))
          kid-is-leaving?     (fn fn-kid-is-leaving? [kid] (contains-key? kids-leaving kid))
          kids-new            (drop-if kid-is-leaving? kids-curr)
          node-new            (glue node-curr {:kids kids-new}) ]
      (set-elem hid node-new)
      node-new)))

(s/defn remove-elems :- #{HID}
  "Removes a set of elements and all references to them from the database. May create orphaned elements."
  [hids-leaving :- #{HID}]
  (doseq [hid hids-leaving]
    (validate-hid hid))
  (set! *forest** (reduce
               (fn fn-dissoc-elems [curr-forest hid]
                 (dissoc curr-forest hid))
               *forest**
               hids-leaving))
  ; Remove any kid references to deleted elements
  (let [hids-staying (keys *forest**)]
    (doseq [hid hids-staying]
      (let [elem (hid->elem hid)]
        (when (instance? Node elem)
          (remove-kids hid hids-leaving true))))) ; true => missing-kids-ok
  hids-leaving)

(s/defn elem-matches?
  [hid :- HID
   pattern-in :- s/Any]
  (let [attrs   (hid->attrs hid)
        pattern (cond
                  (map?         pattern-in)  pattern-in
                  (sequential?  pattern-in)  (zipmap pattern-in (repeat nil))
                  (keyword?     pattern-in)  {pattern-in nil}
                  :else (throw (IllegalArgumentException.
                                 (str "elem-matches?: illegal pattern-in=" pattern-in))))]
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

(defn format-solns [solns]
  (set
    (for [soln solns]
      (let [path soln
            parents (butlast path)
            subtree (last path)]
        (append
          (forv [hid parents]
            (hid->attrs hid))
          (hid->tree subtree))))))

(s/defn ^:no-doc find-paths-impl
  [result-atom
   parents :- [HID]
   hid :- HID
   tgt-path :- [(s/either s/Keyword tsk/KeyMap)]
  ]
  ;(newline)
  ;(println :result-atom) (format-solns @result-atom)
  ;(spy :parents (mapv #(hid->attrs %) parents))
  ;(spy :hid (hid->attrs hid))
  ;(println :hid-tree ) (pretty (hid->tree hid))
  ;(spyx tgt-path)
  (when (not-empty? tgt-path)
    (let [tgt (first tgt-path)
              tgt-path-rest (rest tgt-path)
              attrs (hid->attrs hid)]
      (let [parents-new (append parents hid)]
        (when (or (= tgt :*) (elem-matches? hid tgt))
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

(defn find-paths    ; #todo need update-tree & update-leaf fn's
  "Searches an Enlive-format tree for the specified tgt-path"
  [root tgt-path]
  ;(println "=============================================================================")
  (validate-hid root)
  (when (empty? tgt-path)
    (throw (IllegalStateException. "find-tree: tgt-path is empty")))
  (when (= :** (last tgt-path))
    (throw (IllegalArgumentException. "find-tree: recursive-wildcard `:**` cannot terminate tgt-path")))

  (let [result-atom (atom #{})]
    (find-paths-impl result-atom [] root tgt-path)
    @result-atom))

(defn- has-matching-leaf
  [path tgt-val]
  (let [tail-hid  (last path)
        tail-elem (hid->elem tail-hid)]
    (and (leaf-hid? tail-hid)
      (or (= tgt-val (grab :value tail-elem))
        (= tgt-val :*)))))

(defn find-leaves
  [root tgt-path tgt-val]
  (let [paths      (find-paths root tgt-path)
        leaf-paths (keep-if #(has-matching-leaf % tgt-val) paths) ]
    leaf-paths))


