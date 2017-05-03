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
    [clojure.string :as str]
    [clojure.set :as set]
    [schema.core :as s]
    [tupelo.async :as ta]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)


; #todo  move to tupelo.x-tree (tupelo.x-datapig ?)
; forest  data-forest  ForestDb forest-db
; Sherwood  weald  wald  boreal

; :hid is short for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Element }
(defn new-db
  "Returns a new, empty db."
  []
  (atom {}))        ; #todo how write Plumatic Schema?

(defn validate-db
  [db]
  (if (and (instance? clojure.lang.Atom db)
        (map? @db))
    db
    (throw (IllegalArgumentException. (str "validate-db: failed db=" db)))))


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

(s/defn node? :- s/Bool
  [arg :- tsk/KeyMap]
  (= #{ :attrs :kids } (set (keys arg))))

(s/defn leaf? :- s/Bool
  [arg :- tsk/KeyMap]
  (= #{ :attrs :value } (set (keys arg))))

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

(s/defn hid-exists?
  "Returns true iff an HID exists in the db"
  [db
   hid :- HID]
  (validate-db db)
  (contains-key? @db hid))

(s/defn validate-hid
  "Returns true iff an HID exists in the db"
  [db
   hid :- HID]
  (when-not (hid-exists? db hid)
    (throw (IllegalArgumentException. (str "validate-hid: HID does not exist=" hid))))
  hid)

(s/defn hid->elem :- Element
  [db
   hid :- HID]
  (validate-db db)
  (grab hid @db))

(s/defn hid->node :- Node
  [db
   hid :- HID]
  (validate node? (hid->elem db hid)))

(s/defn hid->leaf :- Leaf
  [db
   hid :- HID]
  (validate leaf? (hid->elem db hid)))

(s/defn hid->attrs :- tsk/KeyMap
  [db
   hid :- HID]
  (grab :attrs (hid->elem db hid)))

(s/defn hid->kids :- [HID]
  [db
   hid :- HID]
  (grab :kids (hid->node db hid)))

(s/defn hid->value :- s/Any
  [db
   hid :- HID]
  (grab :value (hid->leaf db hid)))

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn hid->tree :- tsk/KeyMap
  [db
   hid :- HID]
  (let [elem (hid->elem db hid)
        base-result (into {} elem)]
    (if (instance? Node elem)
      ; Node: need to recursively resolve children
      (let [kids   (mapv #(hid->tree db %) (grab :kids elem))
            resolved-result (assoc base-result :kids kids) ]
        resolved-result)
      ; Leaf: nothing to do
      base-result)))


; #todo naming choices
; #todo reset! vs  set
; #todo swap!  vs  update
; #todo remove  vs  delete  vs drop

(s/defn ^:private set-elem!
  "Unconditionally reset the value of an Element in the db"
  [db
   hid :- HID
   elem :- Element]
  (validate-db db)
  (swap! db glue {hid elem} ))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn set-node
  "Unconditionally reset the value of an Node in the db"
  [db
   hid :- HID
   attrs :- tsk/KeyMap
   kids :- [HID] ]
  (set-elem! db hid (->Node attrs kids)))

(s/defn set-leaf
  "Unconditionally reset the value of an Leaf in the db"
  [db
   hid :- HID
   attrs :- tsk/KeyMap
   value :- s/Any ]
  (set-elem! db hid (->Leaf attrs value)))


; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn add-node :- HID
  [db
   attrs-arg :- (s/either tsk/KeyMap s/Keyword)
   kids :- [s/Keyword]]
  (doseq [kid kids] (validate-hid db kid))
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {(validate keyword? attrs-arg) nil} )
        hid (new-hid)]
    (set-node db hid attrs kids)
    hid))

(s/defn add-leaf :- HID
  [db
   attrs-arg :- (s/either tsk/KeyMap s/Keyword)
   value :- s/Any]
  (let [attrs (if (map? attrs-arg)
                attrs-arg
                {(validate keyword? attrs-arg) nil} )
        hid (new-hid)]
    (set-leaf db hid attrs value)
    hid))


(s/defn enlive-node? :- s/Bool ; #todo add test and -> tupelo.core
  [arg]
  (and (map? arg)
    (= #{:tag :attrs :content} (set (keys arg)))))

(s/defn add-tree :- HID
  "Adds an Enlive-format tree to the DB. Tag values are converted to nil attributes:
  [:a ...] -> {:a nil ...}..."
  [db tree]
  (assert (enlive-node? tree))
  (let [attrs    (glue {(grab :tag tree) nil} (grab :attrs tree))
        children (grab :content tree) ]
    (if (every? enlive-node? children)
      (let [kids (glue [] (for [child children] (add-tree db child))) ]
        (add-node db attrs kids))
      (add-leaf db attrs children))))

(s/defn add-tree-hiccup :- HID
  "Adds a Hiccup-format tree to the DB. Tag values are converted to nil attributes:
  [:a ...] -> {:a nil ...}..."
  [db tree]
  (add-tree db (hiccup->enlive tree)))


(s/defn set-attrs :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [db
   hid :- HID
   attrs-new :- tsk/KeyMap]
  (let [elem-curr  (hid->elem db hid)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem! db hid elem-new)
    elem-new))

(s/defn merge-attrs :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [db
   hid :- HID
   attrs-in :- tsk/KeyMap]
  (let [elem-curr  (hid->elem db hid)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (glue attrs-curr attrs-in)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem! db hid elem-new)
    elem-new))

(s/defn update-attrs :- tsk/KeyMap
  "Use the supplied function & arguments to update the attrs map for a Node or Leaf as in clojure.core/update"
  [db
   hid :- HID
   fn-update-attrs   ; signature: (fn-update attrs-curr x y z & more) -> attrs-new
   & fn-update-attrs-args ]
  (let [elem-curr  (hid->elem db hid)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (apply fn-update-attrs attrs-curr fn-update-attrs-args)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem! db hid elem-new)
    elem-new))

(s/defn update-attr :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [db
   hid :- HID
   attr :- s/Keyword
   fn-update-attr        ; signature: (fn-update-attr attr-curr x y z & more) -> attr-new
   & fn-update-attr-args]
  (let [elem-curr  (hid->elem db hid)
        attr-curr  (fetch-in elem-curr [:attrs attr] )
        attr-new   (apply fn-update-attr attr-curr fn-update-attr-args)
        elem-new   (assoc-in elem-curr [:attrs attr] attr-new) ]
    (set-elem! db hid elem-new)
    elem-new))

(s/defn remove-attr :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [db
   hid :- HID
   attr :- s/Keyword ]
  (let [fn-update-attrs (fn fn-update-attrs [attrs-curr]
                          (let [attrs-new (dissoc attrs-curr attr) ]
                            attrs-new))]
    (update-attrs db hid fn-update-attrs)))


(s/defn set-value :- Leaf
  "Merge the supplied value map into the value of a Node or Leaf"
  [db
   hid :- HID
   value-new :- s/Any]
  (let [leaf-curr  (hid->leaf db hid)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem! db hid leaf-new)
    leaf-new))

(s/defn update-value :- Leaf
  "Merge the supplied value map into the value of a Node or Leaf"
  [db
   hid :- HID
   fn-update-value  ; signature: (fn-update-value value-curr x y z & more) -> value-new
   & fn-update-value-args]
  (let [leaf-curr  (hid->leaf db hid)
        value-curr (grab :value leaf-curr)
        value-new  (apply fn-update-value value-curr fn-update-value-args)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem! db hid leaf-new)
    leaf-new))


; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn set-kids :- Node
  "Resets the kids of a Node to the supplied list"
  [db
   hid :- HID
   kids-new :- [HID]]
  (let [node-curr  (hid->node db hid)
        node-new   (glue node-curr {:kids kids-new})]
    (set-elem! db hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn update-kids :- tsk/KeyMap
  "Use the supplied function & arguments to update the kids map for a Node or Leaf as in clojure.core/update"
  [db
   hid :- HID
   fn-update-kids   ; signature: (fn-update kids-curr x y z & more) -> kids-new
   & fn-update-kids-args]
  (let [elem-curr (hid->elem db hid)
        kids-curr (grab :kids elem-curr)
        kids-new  (apply fn-update-kids kids-curr fn-update-kids-args)
        elem-new  (glue elem-curr {:kids kids-new})]
    (set-elem! db hid elem-new)
    elem-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn add-kids :- tsk/KeyMap
  "Appends a list of kids a Node"
  [db
   hid :- HID
   kids-new :- [HID]]
  (let [elem-curr (hid->elem db hid)
        kids-curr (grab :kids elem-curr)
        kids-new  (glue kids-curr kids-new)
        elem-new  (glue elem-curr {:kids kids-new})]
    (set-elem! db hid elem-new)
    elem-new))

(s/defn remove-kids :- tsk/KeyMap
  "Removes all a set of children from a Node (including any duplcates)."
  ([db
    hid :- HID
    kids-leaving :- #{HID}]
    (remove-kids db hid kids-leaving false))
  ([db
    hid :- HID
    kids-leaving :- #{HID}
    missing-kids-ok :- s/Bool]
    (let [report-missing-kids (not missing-kids-ok)
          node-curr           (hid->node db hid)
          kids-curr           (grab :kids node-curr)
          missing-kids        (set/difference kids-leaving (into #{} kids-curr))
          _                   (when (and (not-empty? missing-kids) report-missing-kids)
                                (throw (IllegalArgumentException.
                                         (str "remove-kids: missing-kids found=" missing-kids))))
          kid-is-leaving?     (fn fn-kid-is-leaving? [kid] (contains-key? kids-leaving kid))
          kids-new            (drop-if kid-is-leaving? kids-curr)
          node-new            (glue node-curr {:kids kids-new}) ]
      (set-elem! db hid node-new)
      node-new)))

(s/defn remove-elems :- #{HID}
  "Removes a set of elements and all references to them from the database. May create orphaned elements."
  [db
   hids-leaving :- #{HID}]
  (doseq [hid hids-leaving]
    (validate-hid db hid))
  (doseq [hid hids-leaving]
    (swap! db dissoc hid))
  (let [hids-staying (keys @db)]
    (doseq [hid hids-staying]
      (let [elem (hid->elem db hid)]
        (when (instance? Node elem)
          (remove-kids db hid hids-leaving true))))) ; true => missing-kids-ok
  hids-leaving)

(s/defn elem-matches?
  [db
   hid :- HID
   pattern-in :- s/Any]
  (let [attrs   (hid->attrs db hid)
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
(comment

(defn- ^:no-doc find-tree-impl
  [result-atom parents root tgt-path]
  (newline)
  (println :result-atom) (pretty @result-atom)
  (spyx parents)
  (spyx root)
  (spyx tgt-path)
  (when (map? root) ; avoid trying to process value `1` on leaf like [:a 1]
    (when (and (not-empty? root) (not-empty? tgt-path))
      (let [tgt           (first tgt-path)
            tgt-path-rest (rest tgt-path)
            tag           (grab :tag root)
            content       (grab :content root)]
        (when (or (= tag :*) (= tag :**))
          (throw (IllegalArgumentException. (str "fing-tag*: found reserved tag " tag " in tree"))))
        (spyx tgt)
        (spyx tgt-path-rest)
        (if (or (= tgt tag) (= tgt :*))
          (do
            (println :200 "match tag:" tag)
            (if (empty? tgt-path-rest)
              (let [soln {:parent-path parents
                          :subtree     root}]
                (println :210 "empty soln:" soln)
                (swap! result-atom glue #{soln}))
              (let [parents-new (append parents tag)]
                (println :220 "not-empty parents-new:" parents-new)
                (doseq [child-tree content]
                  (println :230 "child-tree:" child-tree)
                  (find-tree-impl result-atom parents-new child-tree tgt-path-rest)))))
          (when (= tgt :**)
            (println :300 "tgt = :**")
            (when (not-empty? tgt-path-rest) ; :** wildcard cannot terminate the tgt-path
              (let [parents-new (append parents tag)]
                (println :320 ":** parents-new:" parents-new)
                (println (str :331 "  recurse  parents:" parents "   root:" root "  tgt-path-rest:" tgt-path-rest))
                (find-tree-impl result-atom parents root tgt-path-rest)
                (doseq [child-tree content]
                  (println :330 ":** child-tree:" child-tree)
                  (println (str :332 "    recurse  parents-new:" parents-new "  tgt-path:" tgt-path))
                  (find-tree-impl result-atom parents-new child-tree tgt-path))))))))))

(defn find-tree ; #todo need update-tree & update-leaf fn's
  "Searches an Enlive-format tree for the specified tgt-path"
  [db root tgt-path]
  (println "=============================================================================")
  (when (empty? root)
    (throw (IllegalStateException. "find-tree: tree is empty")))
  (when (empty? tgt-path)
    (throw (IllegalStateException. "find-tree: tgt-path is empty")))
  (when (= :** (last tgt-path))
    (throw (IllegalArgumentException. "find-tree: recursive-wildcard `:**` cannot terminate tgt-path")))

  (let [result-atom (atom #{}) ]
    (try
      (find-tree-impl result-atom [] root tgt-path)
      (catch Exception e
        (throw (RuntimeException. (str "find-tree: failed for tree=" root \newline
                                    "  tgt-path=" tgt-path \newline
                                    "  caused by=" (.getMessage e))))))
    @result-atom))

)
