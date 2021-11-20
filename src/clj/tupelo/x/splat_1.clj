;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.x.splat-1
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    ))

;---------------------------------------------------------------------------------------------------
(declare splatter)

(s/defn ^:no-doc splat-map :- tsk/KeyMap
  [the-map :- tsk/Map]
  {:type    :map
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [me the-map]
                {:type :map-entry
                 :key  (splatter (key me))
                 :val  (splatter (val me))}))})

(s/defn ^:no-doc splat-list :- tsk/KeyMap
  [the-list :- tsk/List]
  {:type    :list
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [[idx item] (indexed the-list)]
                {:type :list-entry
                 :idx  idx
                 :val  (splatter item)}))})

(s/defn ^:no-doc splat-set :- tsk/KeyMap
  [the-set :- tsk/Set]
  {:type    :set
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [item the-set]
                {:type :set-entry
                 :val  (splatter item)}))})

(s/defn ^:no-doc splat-prim :- tsk/KeyMap
  [item :- s/Any]
  {:type :prim :data item})

; #todo add :depth to each map
(s/defn splatter
  [arg]
  (cond
    (xmap? arg) (splat-map arg)
    (xsequential? arg) (splat-list arg)
    (set? arg) (splat-set arg)
    :else (splat-prim arg)))

;---------------------------------------------------------------------------------------------------
; #todo add :depth to each map
(s/defn unsplatter :- s/Any
  [splat :- tsk/KeyMap]
  (let [type               (grab :type splat)
        non-nil-entries-fn (fn [coll]
                             (it-> coll
                               (grab :entries it)
                               (drop-if nil? it)))]
    (cond
      (= type :map) (apply glue
                      (forv [me-splat (non-nil-entries-fn splat)]
                        {(unsplatter (grab :key me-splat)) (unsplatter (grab :val me-splat))}))

      (= type :list) (let [list-vals-sorted-map (into (sorted-map)
                                                  (apply glue
                                                    (forv [le-splat (non-nil-entries-fn splat)]
                                                      {(grab :idx le-splat)
                                                       (grab :val le-splat)})))
                           list-vals            (mapv unsplatter
                                                  (vals list-vals-sorted-map))]
                       list-vals)

      (= type :set) (into #{}
                      (forv [se-splat (non-nil-entries-fn splat)]
                        (unsplatter (grab :val se-splat))))

      (= type :prim) (grab :data splat)

      :else (throw (ex-info "invalid splat found" (vals->map splat))))))

