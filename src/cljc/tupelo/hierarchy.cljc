;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.hierarchy 
  (:use tupelo.core)
  (:require
    [clojure.set :as set]
    [schema.core :as s]
    [tupelo.schema :as tsk]))

(def ^:no-doc Symbol-or-Keyword
  (s/cond-pre s/Keyword s/Symbol))

(s/defn ^:no-doc validate-item-types
  [items :- [Symbol-or-Keyword]]
  (let [item-types (set (mapv type items))]
    (when-not (or
                (= #{clojure.lang.Keyword} item-types)
                (= #{clojure.lang.Symbol} item-types))
      (throw (ex-info "items must be all Keyword or all Symbol" (vals->map items))))))

(s/defn lineage-to-item
  "Returns a set of an items ancestors, including the item itself."
  [h :- tsk/KeyMap
   item :- Symbol-or-Keyword]
  (conj
    (ancestors h item)
    item))

(s/defn num-ancestors
  "Returns the number of ancestors for an item."
  [h :- tsk/KeyMap
   item :- Symbol-or-Keyword]
  (count (ancestors h item)))

(s/defn common-lineage
  "Returns all common lineage elements for all items."
  [h :- tsk/KeyMap
   & items :- [Symbol-or-Keyword]] ; all symbols or all keywords
  (validate-item-types items)
  (apply set/intersection
    (mapv #(lineage-to-item h %) items)))

(s/defn greatest-common-derivation
  "Returns the most derived element in the lineage of all items"
  [h :- tsk/KeyMap
   & items :- [Symbol-or-Keyword]] ; all symbols or all keywords
  (validate-item-types items)
  (let [common-derivations (vec (apply common-lineage h items))
        member-level-pairs (forv [member common-derivations]
                             [member (num-ancestors h member)])
        most-derived       (it-> member-level-pairs
                             (apply max-key xsecond it)
                             (xfirst it))]
    most-derived))

