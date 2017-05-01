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
    (swap! db assoc hid node)
    hid))

(s/defn add-leaf :- HID
  [attrs :- tsk/KeyMap
   value :- s/Any]
  (let [hid  (new-hid)
        leaf (->Leaf attrs value)]
    (swap! db assoc hid leaf)
    hid))

; #todo need to recurse with set of parent hid's to avoid cycles
(s/defn grab-elem :- tsk/KeyMap
  [hid :- HID]
  (let [elem (grab hid @db)
        base-result (into {} elem)]
    (if (instance? Node elem)
      ; Node: need to recursively resolve children
      (let [kids   (mapv grab-elem (grab :kids elem))
            resolved-result (assoc base-result :kids kids) ]
        resolved-result)
      ; Leaf: nothing to do
      base-result)))

(s/defn node? :- s/Bool
  [arg :- tsk/KeyMap]
  (= #{ :attrs :kids } (set (keys arg))))
(s/defn leaf? :- s/Bool
  [arg :- tsk/KeyMap]
  (= #{ :attrs :value } (set (keys arg))))

(dotest
  (let [x (add-leaf {:tag :char :color :red} "x")
        y (add-leaf {:tag :char :color :red} "y")
        z (add-leaf {:tag :char :color :red} "z")
        r (add-node {:tag :root :color :white} [x y z])
        x-elem (grab-elem x)
        y-elem (grab-elem y)
        z-elem (grab-elem z)
        r-elem (grab-elem r)
        ]
    (is= true (leaf? x-elem) (leaf? y-elem) (leaf? z-elem))
    (is= true (node? r-elem))
    (is= (spyx-pretty x-elem)
      {:attrs {:tag :char, :color :red}, :value "x"} )
    (is= (spyx-pretty r-elem)
      {:attrs {:tag :root, :color :white},
       :kids  [{:attrs {:tag :char, :color :red}, :value "x"}
               {:attrs {:tag :char, :color :red}, :value "y"}
               {:attrs {:tag :char, :color :red}, :value "z"}]})

  ))
