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

(s/defn ^:no-doc splat-list :- tsk/KeyMap
  [the-list :- tsk/List]
  {:type    :list
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [[idx item] (indexed the-list)]
                {:type :list-entry
                 :idx  idx
                 :val  (splatter item)}))})

(s/defn ^:no-doc splat-map :- tsk/KeyMap
  [the-map :- tsk/Map]
  {:type    :map
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [me the-map]
                {:type :map-entry
                 :key  (splatter (key me))
                 :val  (splatter (val me))}))})

(s/defn ^:no-doc splat-set :- tsk/KeyMap
  [the-set :- tsk/Set]
  {:type    :set
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [item the-set]
                {:type :set-entry
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
      (= :map splat-type) (apply glue
                            (forv [me-splat (non-nil-entries-fn splat)]
                              {(unsplatter (grab :key me-splat))
                               (unsplatter (grab :val me-splat))}))

      (= :list splat-type) (let [list-vals-sorted-map (into (sorted-map)
                                                        (apply glue
                                                          (forv [le-splat (non-nil-entries-fn splat)]
                                                            {(grab :idx le-splat)
                                                             (grab :val le-splat)})))
                                 list-vals            (mapv unsplatter
                                                        (vals list-vals-sorted-map))]
                             list-vals)

      (= :set splat-type) (into #{}
                            (forv [se-splat (non-nil-entries-fn splat)]
                              (unsplatter (grab :val se-splat))))

      (= :prim splat-type) (grab :data splat)

      :else (throw (ex-info "invalid splat found" (vals->map splat))))))

;---------------------------------------------------------------------------------------------------
(declare walk-recurse-dispatch)

(s/defn ^:no-doc walk-recurse-listentry :- tsk/KeyMap
  [intc :- tsk/KeyMap
   le :- tsk/Map]
  ; (spyx-pretty :walk-recurse-listentry--enter le)
  (let [le-out (update-in le [:val] #(walk-recurse-dispatch intc %))]
    ; (spyx-pretty :walk-recurse-listentry--leave le-out)
    le-out))

(s/defn ^:no-doc walk-recurse-setentry :- tsk/KeyMap
  [intc :- tsk/KeyMap
   se :- tsk/Map]
  ; (spyx-pretty :walk-recurse-setentry--enter se)
  (let [se-out (update-in se [:val] #(walk-recurse-dispatch intc %))]
    ; (spyx-pretty :walk-recurse-setentry--leave se-out)
    se-out))

(s/defn ^:no-doc walk-recurse-mapentry :- tsk/KeyMap
  [intc :- tsk/KeyMap
   me :- tsk/Map]
  ; (spyx-pretty :walk-recurse-mapentry--enter me)
  (let [me-out {:type :map-entry
                :key  (walk-recurse-dispatch intc (grab :key me))
                :val  (walk-recurse-dispatch intc (grab :val me))}]
    ; (spyx-pretty :walk-recurse-mapentry--leave me-out)
    me-out))

(s/defn ^:no-doc walk-recurse-collection :- tsk/KeyMap
  [intc :- tsk/KeyMap
   node :- tsk/KeyMap]
  ; (spyx-pretty :walk-recurse-collection--enter node)
  (let [node-out (glue node
                   {:entries (forv [item (grab :entries node)]
                               (walk-recurse-dispatch intc item))})]
    ; (spyx-pretty :walk-recurse-collection--leave node-out)
    node-out))

(s/defn ^:no-doc walk-recurse-dispatch ; dispatch fn
  [intc :- tsk/KeyMap
   node :- tsk/KeyMap]
  (t/with-spy-indent
    ; (nl) (spyq :dispatch-enter---------------------------------)
    ; (spyx-pretty node)
    (let [enter-fn (:enter intc)
          leave-fn (:leave intc)]
      (let ; -spy-pretty
        [node-type         (grab :type node)
         data-post-enter   (enter-fn node)
         data-post-recurse (cond
                             (= node-type :prim) data-post-enter ; no recursion for primitives

                             (t/contains-key? #{:list :map :set} node-type) (walk-recurse-collection intc data-post-enter)

                             (= node-type :list-entry) (walk-recurse-listentry intc data-post-enter)
                             (= node-type :set-entry) (walk-recurse-setentry intc data-post-enter)
                             (= node-type :map-entry) (walk-recurse-mapentry intc data-post-enter)

                             :else (throw (ex-info "unrecognized :type" (vals->map data-post-enter type))))
         data-post-leave   (leave-fn data-post-recurse)]
        ; (spyq :dispatch-leave---------------------------------)
        data-post-leave))))

;---------------------------------------------------------------------------------------------------
(s/defn walk-interceptor :- s/Any
  [interceptor :- tsk/KeyMap
   splatter :- tsk/KeyMap] ; a splatter node
  ; (spyq :walk-enter---------------------------------)

  ; a top-level splatter node must have both keys :type and :entries
  (assert (not-nil? (grab :type splatter)))
  (assert (not-nil? (grab :entries splatter)))

  ; throw if both be tx functions nil or missing
  (let [enter-fn (:enter interceptor)
        leave-fn (:leave interceptor)]
    (when (and (nil? enter-fn) (nil? leave-fn))
      (throw (ex-info "Invalid interceptor. :enter and :leave functions cannot both be nil." (vals->map interceptor))))

    ; set identify as default in case of nil, and verify both are functions
    (let [enter-fn (s/validate tsk/Fn (or enter-fn identity))
          leave-fn (s/validate tsk/Fn (or leave-fn identity))
          intc     {:enter enter-fn
                    :leave leave-fn}

          data-out (walk-recurse-dispatch intc splatter)]
      ; (spyq :walk-leave---------------------------------)
      data-out)))


