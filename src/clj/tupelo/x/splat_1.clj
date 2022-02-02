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
  (let [splat-type         (grab :type splat)
        non-nil-entries-fn (fn [coll]
                             (drop-if nil?
                               (grab :entries coll)))]
    (cond
      (= :map splat-type) (apply glue
                            (forv [me-splat (non-nil-entries-fn splat)]
                              {(unsplatter (grab :key me-splat)) (unsplatter (grab :val me-splat))}))

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

;-----------------------------------------------------------------------------
(s/defn ^:no-doc walk-splatter-dispatch ; dispatch fn
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (nl) (spyq :dispatch-enter---------------------------------)
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)
          ]
      (spyx-pretty ctx-in)
      (let-spy-pretty
        [ctx-post-enter (enter-fn ctx-in)
         splat-pre-recurse (grab :splat ctx-post-enter)
         ]
        (let [ctx-post-leave (leave-fn ctx-post-enter)]
          (spyq :other-leave---------------------------------)
          (nl)
          ctx-post-leave)))))

;-----------------------------------------------------------------------------
(s/defn walk-splatter :- s/Any
  [data :- s/Any
   interceptor :- tsk/KeyMap]
  (spyq :walk-enter---------------------------------)
  (let [enter-fn (:enter interceptor) ; may be nil
        leave-fn (:leave interceptor)] ; may be nil
    (when (and (nil? enter-fn) (nil? leave-fn))
      (throw (ex-info "Invalid interceptor. :enter and :leave functions cannot both be nil." (vals->map interceptor))))
    (let-spy-pretty
      [splat-in    (splatter data)
       ctx-out  (walk-splatter-dispatch {:parents [] :splat splat-in} interceptor)
       splat-out (grab :splat ctx-out)
       data-out (unsplatter splat-out)
       ]
      (spyq :walk-leave---------------------------------)
      data-out
      )))


