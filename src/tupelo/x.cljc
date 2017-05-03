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
(def db (atom {}))

(defn clear-db!
  "Clear all data from the db."
  []
  (reset! db {}))

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
  [hid :- HID]
  (contains-key? @db hid))

(s/defn validate-hid
  "Returns true iff an HID exists in the db"
  [hid :- HID]
  (when-not (hid-exists? hid)
    (throw (IllegalArgumentException. (str "validate-hid: HID does not exist=" hid))))
  hid)

(s/defn hid->elem :- Element
  [hid :- HID]
  (grab hid @db))

(s/defn hid->node :- Node
  [hid :- HID]
  (validate node? (hid->elem hid)))

(s/defn hid->leaf :- Leaf
  [hid :- HID]
  (validate leaf? (hid->elem hid)))

(s/defn hid->attrs :- tsk/KeyMap
  [hid :- HID]
  (grab :attrs (hid->elem hid)))

(s/defn hid->kids :- [HID]
  [hid :- HID]
  (grab :kids (hid->node hid)))

(s/defn hid->value :- s/Any
  [hid :- HID]
  (grab :value (hid->leaf hid)))

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
      ; Leaf: nothing to do
      base-result)))


; #todo naming choices
; #todo reset! vs  set
; #todo swap!  vs  update
; #todo remove  vs  delete  vs drop

(s/defn ^:private set-elem!
  "Unconditionally reset the value of an Element in the db"
  [hid :- HID
   elem :- Element]
  (swap! db glue {hid elem} ))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn set-node
  "Unconditionally reset the value of an Node in the db"
  [hid :- HID
   attrs :- tsk/KeyMap
   kids :- [HID] ]
  (set-elem! hid (->Node attrs kids)))

(s/defn set-leaf
  "Unconditionally reset the value of an Leaf in the db"
  [hid :- HID
   attrs :- tsk/KeyMap
   value :- s/Any ]
  (set-elem! hid (->Leaf attrs value)))


; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn add-node :- HID
  [attrs :- tsk/KeyMap
   kids :- [s/Keyword]]
  (doseq [kid kids] (validate-hid kid))
  (let [hid (new-hid)]
    (set-node hid attrs kids)
    hid))

(s/defn add-leaf :- HID
  [attrs :- tsk/KeyMap
   value :- s/Any]
  (let [hid (new-hid)]
    (set-leaf hid attrs value)
    hid))


(s/defn set-attrs :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [hid :- HID
   attrs-new :- tsk/KeyMap]
  (let [elem-curr  (hid->elem hid)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem! hid elem-new)
    elem-new))

(s/defn merge-attrs :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [hid :- HID
   attrs-in :- tsk/KeyMap]
  (let [elem-curr  (hid->elem hid)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (glue attrs-curr attrs-in)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (set-elem! hid elem-new)
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
    (set-elem! hid elem-new)
    elem-new))

(s/defn update-attr :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   attr :- s/Keyword
   fn-update-attr        ; signature: (fn-update-attr attr-curr x y z & more) -> attr-new
   & fn-update-attr-args]
  (let [elem-curr  (hid->elem hid)
        attr-curr  (fetch-in elem-curr [:attrs attr] )
        attr-new   (apply fn-update-attr attr-curr fn-update-attr-args)
        elem-new   (assoc-in elem-curr [:attrs attr] attr-new) ]
    (set-elem! hid elem-new)
    elem-new))

(s/defn remove-attr :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   attr :- s/Keyword ]
  (let [fn-update-attrs (fn fn-update-attrs [attrs-curr]
                          (let [attrs-new (dissoc attrs-curr attr) ]
                            attrs-new))]
    (update-attrs hid fn-update-attrs)))


(s/defn set-value :- Leaf
  "Merge the supplied value map into the value of a Node or Leaf"
  [hid :- HID
   value-new :- s/Any]
  (let [leaf-curr  (hid->leaf hid)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem! hid leaf-new)
    leaf-new))

(s/defn update-value :- Leaf
  "Merge the supplied value map into the value of a Node or Leaf"
  [hid :- HID
   fn-update-value  ; signature: (fn-update-value value-curr x y z & more) -> value-new
   & fn-update-value-args]
  (let [leaf-curr  (hid->leaf hid)
        value-curr (grab :value leaf-curr)
        value-new  (apply fn-update-value value-curr fn-update-value-args)
        leaf-new   (glue leaf-curr {:value value-new})]
    (set-elem! hid leaf-new)
    leaf-new))


; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn set-kids :- Node
  "Resets the kids of a Node to the supplied list"
  [hid :- HID
   kids-new :- [HID]]
  (let [node-curr  (hid->node hid)
        node-new   (glue node-curr {:kids kids-new})]
    (set-elem! hid node-new)
    node-new))

; #todo avoid self-cycles
; #todo avoid descendant-cycles
(s/defn update-kids :- tsk/KeyMap
  "Use the supplied function & arguments to update the kids map for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   fn-update-kids   ; signature: (fn-update kids-curr x y z & more) -> kids-new
   & fn-update-kids-args]
  (let [elem-curr (hid->elem hid)
        kids-curr (grab :kids elem-curr)
        kids-new  (apply fn-update-kids kids-curr fn-update-kids-args)
        elem-new  (glue elem-curr {:kids kids-new})]
    (set-elem! hid elem-new)
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
    (set-elem! hid elem-new)
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
      (set-elem! hid node-new)
      node-new)))

(s/defn remove-elems :- #{HID}
  "Removes a set of elements and all references to them from the database. May create orphaned elements."
  [hids-leaving :- #{HID}]
  (doseq [hid hids-leaving]
    (validate-hid hid))
  (doseq [hid hids-leaving]
    (swap! db dissoc hid))
  (let [hids-staying (keys @db)]
    (doseq [hid hids-staying]
      (let [elem (hid->elem hid)]
        (when (instance? Node elem)
          (remove-kids hid hids-leaving true))))) ; true => missing-kids-ok
  hids-leaving)

(s/defn matches-map?
  [pattern :- tsk/KeyMap
   data :- tsk/KeyMap]
  (let [pattern-keys (keys pattern)
            pattern-keys-set (set pattern-keys)
            data-keys-set (set (keys data))
            pattern-keys-missing (set/difference pattern-keys-set data-keys-set)]
    (if (not-empty? pattern-keys-missing)
      false
      (let [data-tst     (submap-by-keys data pattern-keys-set)
            ; replace any nil values with wildcard :*
            pattern-wild (apply glue (for [[k v] pattern]
                                       {k (if (nil? v) :* v)}))]
        (wild-match? pattern-wild data-tst)))))

; #todo list-roots
; #todo list-non-roots
; #todo list-leaves

; #todo list-cycle-nodes (whose kid is an ancestor)

; #todo find-elem, find-node, find-leaf
; #todo find-elem, find-node, find-leaf


