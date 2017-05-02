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


; #todo naming choices
; #todo reset! vs  set
; #todo swap!  vs  update
; #todo remove  vs  delete  vs drop

(s/defn ^:private set-elem!
  "Unconditionally reset the value of an Element in the db"
  [hid :- HID
   elem :- Element]
  (swap! db glue {hid elem} ))

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

(s/defn add-node :- HID
  [attrs :- tsk/KeyMap
   kids :- [s/Keyword]] ; #todo verify kids exist
  (let [hid  (new-hid) ]
    (set-node hid attrs kids)
    hid))

(s/defn add-leaf :- HID
  [attrs :- tsk/KeyMap
   value :- s/Any]
  (let [hid  (new-hid) ]
    (set-leaf hid attrs value)
    hid))

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
   fn-update-attr        ; signature: (fn-update-attr attr-curr x y z & more) -> attrs-new
   & fn-update-attr-args]
  (let [fn-update-attrs (fn fn-update-attrs [attrs-curr]
                          (let [attr-curr (grab attr attrs-curr)
                                attr-new  (apply fn-update-attr attr-curr fn-update-attr-args)
                                attrs-new (glue attrs-curr {attr attr-new}) ]
                            attrs-new))]
    (update-attrs hid fn-update-attrs)))

(s/defn remove-attr :- tsk/KeyMap
  "Use the supplied function & arguments to update the attr value for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   attr :- s/Keyword ]
  (let [fn-update-attrs (fn fn-update-attrs [attrs-curr]
                          (let [attrs-new (dissoc attrs-curr attr) ]
                            attrs-new))]
    (update-attrs hid fn-update-attrs)))

; for Node's
; #todo reset-kids
; #todo update-kids
; #todo add-kid
; #todo remove-kid

; for Leaf's
; #todo reset-value
; #todo update-value

; for any elem
; #todo remove-attr
; #todo remove-elem (need to

; #todo list-roots
; #todo list-non-roots
; #todo list-leaves

; #todo list-cycle-nodes (whose kid is an ancestor)

; #todo find-elem, find-node, find-leaf
; #todo find-elem, find-node, find-leaf


