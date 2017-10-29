;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x.specter
  (:use tupelo.test )
  (:require
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    [clojure.set :as set]
    [schema.core :as s]))
(t/refer-tupelo)

; #todo -> tupelo.core
(def ->true  (constantly true))
(def ->false (constantly false))

; #todo -> tupelo.core macro forv-indexed

(s/defn pair->map
  [pair :- tsk/Pair]
  (apply hash-map pair))
(s/defn map->pair
  [map-arg :- tsk/Map]
  (let [pairs (vec map-arg)
        pair  (only pairs)]
    pair))

; #todo maybe specialize to tx-map-entry & tx-indexed-elem
(s/defn tx-val :- s/Any
  [val-in :- s/Any
   selector-fn  ; fn
   tx-fn        ; fn
   ]
  (if (selector-fn val-in)
    (tx-fn val-in)
    val-in))
(dotest
  (is= 3 (tx-val 2 even? inc))
  (is= 3 (tx-val 3 even? inc)))

(s/defn tx-map
  [map-in :- tsk/KeyMap
   selector-fn      ; fn (or kw shortcut or :*)
   tx-fn            ; fn (map -> map)
   ]
  (let [map-out (apply glue {}
                  (forv [entry-pair map-in]
                    (let [solo-map (pair->map entry-pair)]
                      (validate map?
                        (tx-val solo-map selector-fn tx-fn)))))]
    map-out))

(dotest
  (let [tx-val-fn            (fn [val] (tx-val val even? inc))
        solo-map-selector-fn ->true
        solo-map-selector-fn (fn [solo-map]
                               (let [[k v] (map->pair solo-map)]
                                 (->true k v)))
        solo-map-tx-fn       (fn [solo-map]
                               (assert (map? solo-map))
                               (let [[k v] (map->pair solo-map)]
                                 {k (tx-val-fn v)}))
        map-data             {:a 1 :b 2}
        map-result           (tx-map map-data solo-map-selector-fn solo-map-tx-fn)]
    ; (spyx map-data)
    (is=  {:a 1, :b 3} ) map-result ))

(def Indexed-Element [ (s/one s/Int "index") (s/one s/Any "element") ] )

(s/defn tx-vec
  [vec-in :- tsk/Vec
   selector-fn      ; fn (or idx shortcut or :*)
   tx-fn            ; fn (tsk/Single -> tsk/Single)
   ]
  (let [vec-out (apply glue []
                  (forv [indexed-elem (indexed vec-in)]
                    (s/validate tsk/Single
                      (tx-val indexed-elem selector-fn tx-fn))))]
    vec-out))
(dotest
  (let [tx-val-fn        (fn [val] [(tx-val val even? inc)])
        pair-selector-fn ->true
        pair-selector-fn (fn [pair]
                           (let [[idx val] pair]
                             (->true idx val)))
        pair-tx-fn       (fn [pair]
                           (assert (vector? pair))
                           (let [[idx val] pair
                                 result (tx-val-fn val)]
                             result))

        vec-data         [0 1 2 3 4]
        vec-result       (tx-vec vec-data pair-selector-fn pair-tx-fn)]
    ; (spyx vec-data)
    (is= [1 1 3 3 5] vec-result)))

(dotest
  (let [hand-data   [{:a 2 :b 3} {:a 1} {:a 4}]
        hand-result (forv [it hand-data]
                      (glue it
                        (let [it (grab :a it)]
                          {:a (if (even? it)
                                (inc it)
                                it)})))]
    ; (spyx hand-data)
    (is=  [{:a 3, :b 3} {:a 1} {:a 5}] hand-result)))


