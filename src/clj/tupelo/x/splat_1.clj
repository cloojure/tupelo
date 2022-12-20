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
(s/defn coll-node? :- s/Bool
  [node   :- tsk/KeyMap]
  (let [node-type (grab :type node)]
    (t/contains-key? #{:coll/list :coll/map :coll/set} node-type)))

(s/defn entry-node? :- s/Bool
  [node :- tsk/KeyMap]
  (let [node-type (grab :type node)]
    (t/contains-key? #{:entry/list :entry/set :entry/map} node-type)))

;---------------------------------------------------------------------------------------------------
(declare splatter)

(s/defn ^:no-doc splat-list :- tsk/KeyMap
  [the-list :- tsk/List]
  {:type    :coll/list
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [[idx item] (indexed the-list)]
                {:type :entry/list
                 :idx  idx
                 :val  (splatter item)}))})

(s/defn ^:no-doc splat-map :- tsk/KeyMap
  [the-map :- tsk/Map]
  {:type    :coll/map
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [me the-map]
                {:type :entry/map
                 :key  (splatter (key me))
                 :val  (splatter (val me))}))})

(s/defn ^:no-doc splat-set :- tsk/KeyMap
  [the-set :- tsk/Set]
  {:type    :coll/set
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [item the-set]
                {:type :entry/set
                 :val  (splatter item)}))})

(s/defn ^:no-doc splat-primative :- tsk/KeyMap
  [item :- s/Any]
  {:type :prim
   :data item})

; #todo add :depth to each map
(s/defn ^:no-doc splatter-impl
  [arg]
  (cond
    (xmap? arg) (splat-map arg)
    (xsequential? arg) (splat-list arg)
    (set? arg) (splat-set arg)
    :else (splat-primative arg)))

(s/defn splatter
  [arg] (splatter-impl arg))

;---------------------------------------------------------------------------------------------------
; #todo add :depth to each map
(s/defn unsplatter :- s/Any
  [splat :- tsk/KeyMap]
  (let [splat-type         (grab :type splat)
        non-nil-entries-fn (fn [coll]
                             (drop-if nil?
                               (grab :entries coll)))]
    (cond
      (= :coll/map splat-type) (apply glue
                            (forv [me-splat (non-nil-entries-fn splat)]
                              {(unsplatter (grab :key me-splat))
                               (unsplatter (grab :val me-splat))}))

      (= :coll/list splat-type) (let [list-vals-sorted-map (into (sorted-map)
                                                        (apply glue
                                                          (forv [le-splat (non-nil-entries-fn splat)]
                                                            {(grab :idx le-splat)
                                                             (grab :val le-splat)})))
                                 list-vals            (mapv unsplatter
                                                        (vals list-vals-sorted-map))]
                             list-vals)

      (= :coll/set splat-type) (into #{}
                            (forv [se-splat (non-nil-entries-fn splat)]
                              (unsplatter (grab :val se-splat))))

      (= :prim splat-type) (grab :data splat)

      :else (throw (ex-info "invalid splat found" (vals->map splat))))))

;---------------------------------------------------------------------------------------------------
(declare walk-recurse-dispatch)

(s/defn ^:no-doc walk-recurse-entry :- tsk/KeyMap
  [stack :- tsk/Vec
   intc :- tsk/KeyMap
   entry :- tsk/KeyMap]
  (let [entry-type (grab :type entry)
        result     (cond
                     (= entry-type :entry/map) (let [entry-key (it-> entry
                                                               (grab :key it)
                                                               (glue it {:branch :map/key}))
                                                     entry-val (it-> entry
                                                               (grab :val it)
                                                               (glue it {:branch :map/val}))
                                                     result (it-> entry
                                                              (glue it {:key (walk-recurse-dispatch stack intc entry-key)})
                                                              (glue it {:val (walk-recurse-dispatch stack intc entry-val)}))]
                                                 result)

                     (= entry-type :entry/list) ; retain existing :idx value
                     (let [entry-val (it-> entry
                                       (grab :val it)
                                       (glue it {:branch :list/val}))
                           result (glue entry {:val (walk-recurse-dispatch stack intc entry-val)})]
                       result)

                     (= entry-type :entry/set) ; has no :key
                     (let [entry-val (it-> entry
                                       (grab :val it)
                                       (glue it {:branch :set/val}))
                           result (glue entry {:val (walk-recurse-dispatch stack intc entry-val)})]
                       result)

                     :else (throw (ex-info "unrecognized :type" (vals->map stack entry )))
                     )]
    result))

(s/defn ^:no-doc walk-recurse-collection :- tsk/KeyMap
  [stack :- tsk/Vec
   intc :- tsk/KeyMap
   node :- tsk/KeyMap]
  (let [node-out   (glue node
                     {:entries (forv [item (grab :entries node)]
                                 (walk-recurse-dispatch stack intc item))})]
    node-out))

(s/defn ^:no-doc walk-recurse-dispatch ; dispatch fn
  [stack :- tsk/Vec
   intc :- tsk/KeyMap
   node :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (:enter intc)
          leave-fn (:leave intc)]
      (let ; -spy-pretty
        [node-type         (grab :type node)
         stack-next        (prepend (dissoc node :entries) stack)
         data-post-enter   (enter-fn stack-next node)
         data-post-recurse (cond
                             (= node-type :prim) data-post-enter ; no recursion for primitives

                             (coll-node? node) (walk-recurse-collection stack-next intc data-post-enter)
                             (entry-node? node) (walk-recurse-entry stack-next intc data-post-enter)

                             :else (throw (ex-info "unrecognized :type" (vals->map data-post-enter type))))
         data-post-leave   (leave-fn stack-next data-post-recurse)]
        data-post-leave))))

;---------------------------------------------------------------------------------------------------
(s/defn walk-identity-fn [stack arg] arg)

(s/defn walk :- s/Any
  [interceptor :- tsk/KeyMap
   splatter :- tsk/KeyMap] ; a splatter node

  ; a top-level splatter node must have both keys :type and :entries
  (assert (not-nil? (grab :type splatter)))
  (assert (not-nil? (grab :entries splatter)))

  ; throw if both be tx functions nil or missing
  (let [enter-fn (:enter interceptor)
        leave-fn (:leave interceptor)]
    (when (and (nil? enter-fn) (nil? leave-fn))
      (throw (ex-info "Invalid interceptor. :enter and :leave functions cannot both be nil." (vals->map interceptor))))

    ; set identify as default in case of nil, and verify both are functions
    (let [enter-fn (s/validate tsk/Fn (or enter-fn walk-identity-fn))
          leave-fn (s/validate tsk/Fn (or leave-fn walk-identity-fn))
          intc     {:enter enter-fn
                    :leave leave-fn}

          data-out (walk-recurse-dispatch [] intc splatter)]
      data-out)))

