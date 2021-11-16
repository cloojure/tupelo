;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.x.walk-1
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    ))

(declare walk-with-context-impl)

(s/defn ^:no-doc proc-map-entry
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)
          data     (grab :data ctx-in)]
      (nl) (spyq :map-entry--enter---------------------------------)
      (let-spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         ctx-key          (let [ctx-me-key {:data (grab :key data) :branch :map-entry/key :parent ctx-post-enter}]
                            (walk-with-context-impl ctx-me-key interceptor))
         ctx-val          (let [ctx-me-val {:data (grab :val data) :branch :map-entry/val :parent ctx-post-enter}]
                            (walk-with-context-impl ctx-me-val interceptor))
         data-out         {:key (grab :data ctx-key) :val (grab :data ctx-val)}
         ctx-post-recurse (glue ctx-post-enter {:data data-out})
         ctx-post-leave   (leave-fn ctx-post-recurse)
         ]
        (spyq :map-entry--leave---------------------------------)
        (nl)
        ctx-post-leave))))


(s/defn ^:no-doc proc-map
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)
          data     (grab :data ctx-in)]
      (nl) (spyq :map-enter---------------------------------)
      (let-spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         ctx-subs         (forv [me data] ; for each map-entry
                            (with-spy-indent
                              (let [ctx-me {:data {:key (key me) :val (val me)} :branch :map-entry :parent ctx-post-enter}]
                                (spyx-pretty ctx-me)
                                (proc-map-entry ctx-me interceptor))))
         data-new         (apply glue
                            (forv [me-spread (mapv :data ctx-subs)]
                              {(grab :key me-spread) (grab :val me-spread)}))
         ctx-post-recurse (glue ctx-post-enter {:data data-new})
         ctx-post-leave   (leave-fn ctx-post-recurse)]
        (spyq :map-leave---------------------------------)
        (nl)
        ctx-post-leave))))

(s/defn ^:no-doc proc-other
  [ctx :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      (nl) (spyq :other-enter---------------------------------)
      (let-spy-pretty
        [ctx-post-enter (enter-fn ctx)
         ctx-post-leave (leave-fn ctx-post-enter)]
        (spyq :other-leave---------------------------------)
        (nl)
        ctx-post-leave))))

(s/defn ^:no-doc walk-with-context-impl ; dispatch fn
  [ctx :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (spyq :impl-enter---------------------------------)
    (let [data    (grab :data ctx)
          ctx-out (cond
                    (t/xmap? data) (proc-map ctx interceptor)
                    (t/xsequential? data) (proc-other ctx interceptor)
                    (set? data) (proc-other ctx interceptor)
                    :else (proc-other ctx interceptor))]
      (spyq :impl-leave---------------------------------)
      ctx-out)))

(s/defn walk-with-context :- s/Any
  [data :- s/Any
   interceptor :- tsk/KeyMap]
  (spyq :walk-enter---------------------------------)
  (let [enter-fn (:enter interceptor) ; may be nil
        leave-fn (:leave interceptor)] ; may be nil
    (when (and (nil? enter-fn) (nil? leave-fn))
      (throw (ex-info "Invalid interceptor. :enter and :leave functions cannot both be nil." (vals->map interceptor))))
    (let-spy-pretty
      [ctx-out  (walk-with-context-impl {:branch :root :parent nil :data data} interceptor)
       data-out (grab :data ctx-out)]
      (spyq :walk-leave---------------------------------)
      data-out
      )))

