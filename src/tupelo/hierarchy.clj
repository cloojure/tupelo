;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.hierarchy
  (:require
    [clojure.set :as set]
    [schema.core :as s]
    [tupelo.core :as t :refer [spyx spyxx it-> assert-info]]
    [tupelo.schema :as tsk]))

(def ^:no-doc Symbol-or-Keyword
  (s/cond-pre s/Keyword s/Symbol))

(s/defn ^:no-doc validate-item-types
  [items :- [Symbol-or-Keyword]]
  (let [item-types (set (mapv type items))]
    (assert-info (or
                   (= #{clojure.lang.Keyword} item-types)
                   (= #{clojure.lang.Symbol} item-types))
      "items must be all Keyword or all Symbol" (t/vals->map items))))

(s/defn lineage-to-item :- tsk/Set
  "Returns a set of an items ancestors, including the item itself."
  [h :- tsk/KeyMap
   item :- Symbol-or-Keyword]
  (conj
    (set (ancestors h item)) ; could be nil
    item))

(s/defn num-ancestors :- s/Num
  "Returns the number of ancestors for an item."
  [h :- tsk/KeyMap
   item :- Symbol-or-Keyword]
  (count (ancestors h item)))

(s/defn common-lineage :- tsk/Set
  "Returns all common lineage elements for all items."
  [h :- tsk/KeyMap
   & items :- [Symbol-or-Keyword]] ; all symbols or all keywords
  (validate-item-types items)
  (apply set/intersection
    (mapv #(lineage-to-item h %) items)))

(s/defn greatest-common-derivation :- Symbol-or-Keyword
  "Returns the most derived element in the lineage of all items"
  [h :- tsk/KeyMap
   & items :- [Symbol-or-Keyword]] ; all symbols or all keywords
  (validate-item-types items)
  (let [common-derivations (vec (apply common-lineage h items))
        member-level-pairs (t/forv [member common-derivations]
                             [member (num-ancestors h member)])
        most-derived       (t/it-> member-level-pairs
                             (apply max-key t/xsecond it)
                             (t/xfirst it))]
    most-derived))

