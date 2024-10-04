;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.splat
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    ))

;---------------------------------------------------------------------------------------------------
(s/defn coll-node? :- s/Bool
  [node :- tsk/KeyMap]
  (let [node-type (grab :type node)]
    (t/contains-key? #{:coll/list :coll/map :coll/set} node-type)))

(s/defn entry-node? :- s/Bool
  [node :- tsk/KeyMap]
  (let [node-type (grab :type node)]
    (t/contains-key? #{:list/entry :set/entry :map/entry} node-type)))

;---------------------------------------------------------------------------------------------------
(declare splatter)

(s/defn ^:no-doc splat-list :- tsk/KeyMap
  [the-list :- tsk/List]
  {:type    :coll/list
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [[idx item] (indexed the-list)]
                {:type :list/entry
                 :idx  idx
                 :val  (splatter item)}))})

(s/defn ^:no-doc splat-map :- tsk/KeyMap
  [the-map :- tsk/Map]
  {:type    :coll/map
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [me the-map]
                {:type :map/entry
                 :key  (splatter (key me))
                 :val  (splatter (val me))}))})

(s/defn ^:no-doc splat-set :- tsk/KeyMap
  [the-set :- tsk/Set]
  {:type    :coll/set
   :entries (set ; must be a set so can unit test w/o regard to order
              (forv [item the-set]
                {:type :set/entry
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
  "Given arbitrary EDN data, returns a `splat` that wraps and annotates each level for
   collections (map/list/set), collection entries (map entries/list elements/set elements),
   and primitive values (numbers/strings/keywords/symbols/nil/etc)."
  [data] (splatter-impl data))

;---------------------------------------------------------------------------------------------------
; #todo add :depth to each map

(s/defn non-nil-entries-fn
  [coll :- (s/maybe tsk/KeyMap)]
  (when (not-nil? coll)
    (drop-if #(or (nil? %) ; entry is nil
                (nil? (:val %))) ; val for list, map, or set is nil
      (:entries coll))))

;---------------------------------------------------------------------------------------------------
(s/defn ^:no-doc remove-nils-map
  [splat :- tsk/KeyMap]
  (assert (= :coll/map (grab :type splat)))
  (assoc splat :entries (it-> (grab :entries splat)
                          (set
                            (drop-if (fn [entry]
                                       (or
                                         (nil? entry)
                                         (nil? (:key entry))
                                         (nil? (:val entry)))) it)))))

(s/defn ^:no-doc remove-nils-list :- tsk/KeyMap
  [splat :- tsk/KeyMap]
  (assert (= :coll/list (grab :type splat)))
  (assoc splat :entries (it-> (grab :entries splat)
                          (set
                            (drop-if (fn [entry]
                                       (or
                                         (nil? entry)
                                         (nil? (:idx entry))
                                         (nil? (:val entry)))) it)))))

(s/defn ^:no-doc remove-nils-set :- tsk/KeyMap
  [splat :- tsk/KeyMap]
  (assert (= :coll/set (grab :type splat)))
  (assoc splat :entries (it-> (grab :entries splat)
                          (set
                            (drop-if (fn [entry]
                                       (or
                                         (nil? entry)
                                         (nil? (:val entry)))) it)))))

(s/defn ^:no-doc remove-nils-collection :- tsk/KeyMap
  [splat :- tsk/KeyMap]
  (with-spy-indent
    (let [splat-type (grab :type splat)]
      (cond
        (= :coll/map splat-type) (remove-nils-map splat)
        (= :coll/list splat-type) (remove-nils-list splat)
        (= :coll/set splat-type) (remove-nils-set splat)
        :else (throw (ex-info "invalid splat found" (vals->map splat)))))))

;----------------------------------------------------------------------------------------------------
(declare unsplatter)

(s/defn ^:no-doc unsplatter-map :- tsk/Map
  [splat :- tsk/KeyMap]
  (into {} (forv [me-splat (grab :entries (remove-nils-map splat))]
             (let [me-key    (unsplatter (grab :key me-splat))
                   me-val    (unsplatter (grab :val me-splat))
                   me-result (map-entry me-key me-val)]
               me-result))))

(s/defn ^:no-doc unsplatter-list :- tsk/Vec
  [splat :- tsk/KeyMap]
  (let [list-vals-sorted-map (apply glue (sorted-map)
                               (forv [le-splat (grab :entries (remove-nils-list splat))]
                                 (let [le-idx (grab :idx le-splat)
                                       le-val (grab :val le-splat)]
                                   {le-idx le-val})))
        list-vals            (mapv unsplatter
                               (vals list-vals-sorted-map))]
    list-vals))

(s/defn ^:no-doc unsplatter-set :- tsk/Set
  [splat :- tsk/KeyMap]
  (set
    (forv [se-splat (grab :entries (remove-nils-set splat))]
      (unsplatter (grab :val se-splat)))))

(s/defn unsplatter :- s/Any
  "Remove annotations from a `splat`, eliding any `nil` values. Since the `splatter`
   annotation never includes raw `nil` values, any `nil` detected by `unsplatter`
   indicates a node or subtree has been deleted during prior processing
   (eg via `stack-walk` or `splatter-walk`)."
  [splat :- tsk/KeyMap]
  (with-spy-indent
    (let [splat-type (grab :type splat)]
      (cond
        (= :coll/map splat-type) (unsplatter-map splat)
        (= :coll/list splat-type) (unsplatter-list splat)
        (= :coll/set splat-type) (unsplatter-set splat)
        (= :prim splat-type) (grab :data splat)
        :else (throw (ex-info "invalid splat found" (vals->map splat)))))))

;---------------------------------------------------------------------------------------------------
(defn ^:no-doc prewalk-remove-entries
  [arg]
  (walk/prewalk (fn [arg]
                  (cond-it-> arg
                    (map? it) (dissoc it :entries)))
    arg))

;---------------------------------------------------------------------------------------------------
(declare walk-recurse-dispatch)
(def ^:dynamic stack-walk-noop? false)

; #todo fix to allow :enter interceptor to remove `nil` without crashing here
(s/defn ^:no-doc walk-recurse-entry :- tsk/KeyMap
  [stack :- tsk/Vec
   intc :- tsk/KeyMap
   entry :- tsk/KeyMap]
  (let [entry-type (grab :type entry)
        stack-next (prepend (prewalk-remove-entries entry) stack)
        result     (cond
                     (= entry-type :map/entry) (let [entry-key (it-> entry
                                                                 (grab :key it)
                                                                 (glue it {:branch :map/key}))
                                                     entry-val (it-> entry
                                                                 (grab :val it)
                                                                 (glue it {:branch :map/val}))
                                                     result    (it-> entry
                                                                 (glue it {:key (walk-recurse-dispatch stack-next intc entry-key)})
                                                                 (glue it {:val (walk-recurse-dispatch stack-next intc entry-val)}))]
                                                 result)

                     (= entry-type :list/entry) ; retain existing :idx value
                     (let [entry-val (it-> entry
                                       (grab :val it)
                                       (glue it {:branch :list/val}))
                           result    (glue entry {:val (walk-recurse-dispatch stack-next intc entry-val)})]
                       result)

                     (= entry-type :set/entry) ; has no :key
                     (let [entry-val (it-> entry
                                       (grab :val it)
                                       (glue it {:branch :set/val}))
                           result    (glue entry {:val (walk-recurse-dispatch stack-next intc entry-val)})]
                       result)

                     :else (throw (ex-info "unrecognized :type" (vals->map stack-next entry)))
                     )]
    result))

(s/defn ^:no-doc walk-recurse-collection :- tsk/KeyMap
  [stack :- tsk/Vec
   intc :- tsk/KeyMap
   node :- tsk/KeyMap]
  (let [stack-next        (prepend (prewalk-remove-entries node) stack)
        node-post-recurse (glue node
                            {:entries (set (forv [item (grab :entries node)]
                                             (walk-recurse-dispatch stack-next intc item)))})
        node-out          (remove-nils-collection node-post-recurse)]
    node-out))

; #todo if intc returns
;   :walk/trim    then <don't recurse>
(s/defn ^:no-doc walk-recurse-dispatch ; dispatch fn
  [stack :- tsk/Vec
   intc :- tsk/KeyMap
   node :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (:enter intc)
          leave-fn (:leave intc)]
      (let ; -spy-pretty
        [node-type         (grab :type node)

         data-post-enter   (enter-fn stack node)
         data-for-recurse  (if stack-walk-noop?
                             node
                             data-post-enter)

         data-post-recurse (cond
                             (= node-type :prim) data-for-recurse ; no recursion for primitives

                             (coll-node? node) (walk-recurse-collection stack intc data-for-recurse)
                             (entry-node? node) (walk-recurse-entry stack intc data-for-recurse)

                             :else (throw (ex-info "unrecognized :type" (vals->map data-for-recurse type))))
         data-for-leave    (if stack-walk-noop?
                             node
                             data-post-recurse)

         data-post-leave   (leave-fn stack data-for-leave)
         data-to-return    (if stack-walk-noop?
                             node
                             data-post-leave)]
        data-to-return))))

;---------------------------------------------------------------------------------------------------
(s/defn stack-identity
  "An identity function for use with `stack-walk`. It ignores the stack and returns the supplied node value."
  [stack node] node)

(s/defn stack-spy
  [stack node]
  (with-result node
    (newline)
    (spyq :-----------------------------------------------------------------------------)
    (spyx-pretty node)
    (spyx-pretty stack)))

(s/defn stack-walk :- s/Any
  "Uses an interceptor (with `:enter` and `:leave` functions) to walk a Splatter data structure.  Each interceptor
  function call receives a node-history stack as the first argument (current node => index 0) "
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
    (let [enter-fn   (s/validate tsk/Fn (or enter-fn stack-identity))
          leave-fn   (s/validate tsk/Fn (or leave-fn stack-identity))
          intc       {:enter enter-fn
                      :leave leave-fn}
          stack-init []
          data-out   (walk-recurse-dispatch stack-init intc splatter)]
      data-out)))

;---------------------------------------------------------------------------------------------------
(s/defn splatter-walk :- s/Any
  "Convenience function for performing a `stack-walk` using splattered data:

        (it-> <data>
          (splatter it)
          (stack-walk <interceptor> it)
          (unsplatter it))
  "
  [intc :- tsk/KeyMap
   data :- s/Any]
  (it-> data
    (splatter it)
    (stack-walk intc it)
    (unsplatter it)))

(s/defn splatter-walk-noop :- s/Any ; #todo need unit test
  "Same as `splatter-walk` but discards the result of each interceptor `:enter` and `:leave` function. "
  [intc :- tsk/KeyMap
   data :- s/Any]
  (binding [stack-walk-noop? true]
    (with-result data
      (it-> data
        (splatter it)
        (stack-walk intc it)))))

(s/defn splatter-walk-spy :- s/Any
  "LIke splatter-walk, but prints both node-history stack and subtree at each step. "
  [data :- s/Any]
  (let [intc {:enter (fn [stack node]
                       (with-result node
                         (newline)
                         (spyq :enter-----------------------------------------------------------------------------)
                         (spyx-pretty node)
                         (spyx-pretty stack)))
              :leave (fn [stack node]
                       (with-result node
                         (newline)
                         (spyq :leave-----------------------------------------------------------------------------)
                         (spyx-pretty node)
                         (spyx-pretty stack)))}]
    (it-> data
      (splatter it)
      (stack-walk intc it)
      (unsplatter it))))


