;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x
  "Experimental new code"
  (:use clojure.test tupelo.test tupelo.x)
  (:require
    [clojure.string :as str]
    [clj-uuid :as uuid]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
  )
  (:import
    [java.nio ByteBuffer]
    [java.util UUID ]
  ))
(t/refer-tupelo)

; #todo  move to tupelo.x-tree (tupelo.x-datapig ?)

; :hid is short for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Element }
(def db (atom {}))

(defn reset-db!
  "Drop all data from the db."
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

(s/defn add-node :- HID
  [attrs :- tsk/KeyMap
   kids :- [s/Keyword]]
  ; #todo verify kids exist
  (let [hid  (new-hid)
        node (->Node attrs kids)]
    (swap! db glue {hid node})
    hid))

(s/defn add-leaf :- HID
  [attrs :- tsk/KeyMap
   value :- s/Any]
  (let [hid  (new-hid)
        leaf (->Leaf attrs value)]
    (swap! db glue {hid leaf})
    hid))

(s/defn merge-attrs :- tsk/KeyMap
  "Merge the supplied attrs map into the attrs of a Node or Leaf"
  [hid :- HID
   attrs-new :- tsk/KeyMap]
  (let [elem-curr  (grab hid @db)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (glue attrs-curr attrs-new)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (swap! db glue {hid elem-new})
    elem-new))

(s/defn update-attrs :- tsk/KeyMap
  "Use the supplied function & arguments to update the attrs for a Node or Leaf as in clojure.core/update"
  [hid :- HID
   update-fn   ; signature: (fn-update attrs-curr x y z & more) -> attrs-new
   & update-fn-args ]
  (let [elem-curr  (grab hid @db)
        attrs-curr (grab :attrs elem-curr)
        attrs-new  (apply update-fn attrs-curr update-fn-args)
        elem-new   (glue elem-curr {:attrs attrs-new})]
    (swap! db glue {hid elem-new})
    elem-new))

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn hid->tree :- tsk/KeyMap
  [hid :- HID]
  (let [elem (grab hid @db)
        base-result (into {} elem)]
    (if (instance? Node elem)
      ; Node: need to recursively resolve children
      (let [kids   (mapv hid->tree (grab :kids elem))
            resolved-result (assoc base-result :kids kids) ]
        resolved-result)
      ; Leaf: nothing to do
      base-result)))

(dotest
  (reset-db!)
  (let [x (add-leaf {:tag :char :color :red} "x")
        y (add-leaf {:tag :char :color :red} "y")
        z (add-leaf {:tag :char :color :red} "z")
        r (add-node {:tag :root :color :white} [x y z])
        x-tree (hid->tree x)
        y-tree (hid->tree y)
        z-tree (hid->tree z)
        r-tree (hid->tree r) ]
    (is (and (hid? x) (hid? y) (hid? z) (hid? r)))
    (is (and (leaf? x-tree) (leaf? y-tree) (leaf? z-tree)))
    (is (node? r-tree))
    (is= x-tree {:attrs {:tag :char, :color :red}, :value "x"} )
    (is= r-tree
      {:attrs {:tag :root, :color :white},
       :kids  [{:attrs {:tag :char, :color :red}, :value "x"}
               {:attrs {:tag :char, :color :red}, :value "y"}
               {:attrs {:tag :char, :color :red}, :value "z"}]})
    (merge-attrs x {:color :green})
    (is= (hid->tree x) {:attrs {:tag :char, :color :green}, :value "x"} )))

(dotest
  (reset-db!)
  (let [x (add-leaf {:tag :char :color :red :cnt 0} "x")
        r (add-node {:tag :root :color :white :cnt 0} [x])
        x-tree (hid->tree x)
        r-tree (hid->tree r) ]
    (is= r-tree
      {:attrs {:tag :root, :color :white :cnt 0},
       :kids  [{:attrs {:tag :char, :color :red :cnt 0}, :value "x"} ]})
    (update-attrs x #(update % :cnt inc))
    (update-attrs x #(update % :cnt inc))
    (update-attrs r #(update % :cnt inc))
    (is= (hid->tree r)
      {:attrs {:tag :root, :color :white, :cnt 1},
       :kids  [{:attrs {:tag :char, :color :red, :cnt 2}, :value "x"}]})

  ))
