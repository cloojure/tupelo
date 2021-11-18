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
    [tupelo.string :as str]
    ))

(declare splatter)

(s/defn ^:no-doc splat-map :- tsk/KeyMap
  [the-map :- tsk/Map]
  {:type    :map
   :entries (forv [me the-map]
              {:type :map-entry
               :key  (splatter (key me))
               :val  (splatter (val me))})})

(s/defn ^:no-doc splat-list :- tsk/KeyMap
  [the-list :- tsk/List]
  {:type    :list
   :entries (forv [[idx item] (indexed the-list)]
              {:type :list-entry
               :idx  idx
               :val  (splatter item)})})

(s/defn ^:no-doc splat-set :- tsk/KeyMap
  [the-set :- tsk/Set]
  {:type    :list
   :entries (forv [item the-set]
              {:type :set-entry
               :val  (splatter item)})})

(s/defn ^:no-doc splat-prim :- tsk/KeyMap
  [item :- s/Any]
  {:type :prim :data item})

(s/defn splatter
  [arg]
  (cond
    (xmap? arg) (splat-map arg)
    (xsequential? arg) (splat-list arg)
    (set? arg) (splat-set arg)
    :else (splat-prim arg)))
