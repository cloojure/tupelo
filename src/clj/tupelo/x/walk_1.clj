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

(s/defn ctx-depth :- s/Int
  "Returns the depth of recursion in a data structure, where {:branch :root} => 0"
  [ctx :- tsk/KeyMap]
  (let [parent (grab :parent ctx)]
    (if (nil? parent)
      0
      (inc (ctx-depth parent)))))

(s/defn ^:no-doc apply-glue-not-nil :- s/Any
  "Given a sequence of collections, remove any `nil` values, then combine. All non-nil
  elements must be collections of the same type (eg all maps, all lists/vecs, etc) "
  [colls :- [s/Any]]
  (apply glue
    (filterv not-nil? colls)))

(declare walk-with-context-dispatch)

;-----------------------------------------------------------------------------
(s/defn ^:no-doc proc-map-entry
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      ; (nl) (spyq :map-entry--enter---------------------------------)
      ; (spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         data             (:data ctx-post-enter)
         ctx-post-key     (let [ctx-me-key {:data (:key data) :branch :map-entry/key :parent ctx-post-enter}]
                            (walk-with-context-dispatch ctx-me-key interceptor))
         ctx-post-val     (let [ctx-me-val {:data (:val data) :branch :map-entry/val :parent ctx-post-enter}]
                            (walk-with-context-dispatch ctx-me-val interceptor))
         data-out         {:key (grab :data ctx-post-key)
                           :val (grab :data ctx-post-val)}
         ctx-post-recurse (glue ctx-post-enter {:data data-out})
         ctx-post-leave   (leave-fn ctx-post-recurse)
         ]
        ;(spyq :map-entry--leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

(s/defn ^:no-doc proc-map
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      (nl) (spyq :map-enter---------------------------------)
      (spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         data             (grab :data ctx-post-enter)
         ctx-subs         (forv [me data] ; for each map-entry
                            (with-spy-indent
                              (let [ctx-me {:data {:key (key me)
                                                   :val (val me)}
                                            :branch :map-entry
                                            :parent ctx-post-enter}]
                                (proc-map-entry ctx-me interceptor))))
         data-new         (apply-glue-not-nil
                            (forv [me-spread (mapv :data ctx-subs)]
                              {(:key me-spread)
                               (:val me-spread)}))
         ctx-post-recurse (glue ctx-post-enter {:data data-new})
         ctx-post-leave   (leave-fn ctx-post-recurse)]
        ;(spyq :map-leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc proc-set-entry
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      ;(nl) (spyq :set-entry--enter---------------------------------)
      ;(spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         data             (:data ctx-post-enter)
         ctx-post-key     (let [ctx-me-key {:data (:val data) :branch :set-entry/elem :parent ctx-post-enter}]
                            (walk-with-context-dispatch ctx-me-key interceptor))
         data-out         {:val (grab :data ctx-post-key)}
         ctx-post-recurse (glue ctx-post-enter {:data data-out})
         ctx-post-leave   (leave-fn ctx-post-recurse)
         ]
        ;(spyq :set-entry--leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

(s/defn ^:no-doc proc-set
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      ;(nl) (spyq :set-enter---------------------------------)
      ;(spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         data             (grab :data ctx-post-enter)
         ctx-subs         (forv [se data] ; for each set-entry
                            (with-spy-indent
                              (let [ctx-se {:data   {:val se}
                                            :branch :set-entry
                                            :parent ctx-post-enter}]
                                (proc-set-entry ctx-se interceptor))))
         data-new         (apply-glue-not-nil
                            (forv [se-spread (mapv :data ctx-subs)]
                              #{(:val se-spread)}))
         ctx-post-recurse (glue ctx-post-enter {:data data-new})
         ctx-post-leave   (leave-fn ctx-post-recurse)]
        ;(spyq :set-leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc proc-list-entry
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      ;(nl) (spyq :list-entry--enter---------------------------------)
      ;(spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter   (enter-fn ctx-in)
         data             (grab :data ctx-post-enter)
         ctx-post-idx     (let [ctx-le-idx {:data (:idx data) :branch :list-entry/idx :parent ctx-post-enter}]
                            (walk-with-context-dispatch ctx-le-idx interceptor))
         ctx-post-val     (let [ctx-le-val {:data (:val data) :branch :list-entry/val :parent ctx-post-enter}]
                            (walk-with-context-dispatch ctx-le-val interceptor))
         data-out         {:idx (grab :data ctx-post-idx)
                           :val (grab :data ctx-post-val)}
         ctx-post-recurse (glue ctx-post-enter {:data data-out})
         ctx-post-leave   (leave-fn ctx-post-recurse)
         ]
        ;(spyq :list-entry--leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

(s/defn ^:no-doc proc-list
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      ;(nl) (spyq :list-enter---------------------------------)
      ;(spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter      (enter-fn ctx-in)
         data-indexed        (t/indexed (grab :data ctx-post-enter))
         ctx-subs            (forv [le data-indexed] ; for each list-entry
                               (with-spy-indent
                                 (let [ctx-le {:data   {:idx (xfirst le)
                                                        :val (xsecond le)}
                                               :branch :list-entry
                                               :parent ctx-post-enter}]
                                   (proc-list-entry ctx-le interceptor))))
         data-new-sorted-map (into (sorted-map)
                               (apply-glue-not-nil
                                 ; #todo filter not-nil? here vvv !!!
                                 (forv [le-spread (mapv :data ctx-subs)]
                                   {(:idx le-spread)
                                    (:val le-spread)})))
         data-new            (vec (vals data-new-sorted-map))
         ctx-post-recurse    (glue ctx-post-enter {:data data-new})
         ctx-post-leave      (leave-fn ctx-post-recurse)]
        ;(spyq :list-leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc proc-other
  [ctx-in :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    (let [enter-fn (or (:enter interceptor) identity)
          leave-fn (or (:leave interceptor) identity)]
      ;(nl) (spyq :other-enter---------------------------------)
      ;(spyx-pretty ctx-in)
      (let ; -spy-pretty
        [ctx-post-enter (enter-fn ctx-in)
         ctx-post-leave (leave-fn ctx-post-enter)]
        ;(spyq :other-leave---------------------------------)
        ;(nl)
        ctx-post-leave))))

;-----------------------------------------------------------------------------
(s/defn ^:no-doc walk-with-context-dispatch ; dispatch fn
  [ctx :- tsk/KeyMap
   interceptor :- tsk/KeyMap]
  (t/with-spy-indent
    ; (spyq :dispatch-enter---------------------------------)
    (let [data    (grab :data ctx)
          ctx-out (cond
                    (t/xmap? data) (proc-map ctx interceptor)
                    (t/xsequential? data) (proc-list ctx interceptor)
                    (set? data) (proc-set ctx interceptor)
                    :else (proc-other ctx interceptor))]
      ; (spyq :dispatch-leave---------------------------------)
      ctx-out)))

;-----------------------------------------------------------------------------
(s/defn walk-with-context :- s/Any
  [data :- s/Any
   interceptor :- tsk/KeyMap]
  ; (spyq :walk-enter---------------------------------)
  (let [enter-fn (:enter interceptor) ; may be nil
        leave-fn (:leave interceptor)] ; may be nil
    (when (and (nil? enter-fn) (nil? leave-fn))
      (throw (ex-info "Invalid interceptor. :enter and :leave functions cannot both be nil." (vals->map interceptor))))
    (let ; -spy-pretty
      [ctx-out  (walk-with-context-dispatch {:branch :root :parent nil :data data} interceptor)
       data-out (grab :data ctx-out)]
      ; (spyq :walk-leave---------------------------------)
      data-out
      )))

