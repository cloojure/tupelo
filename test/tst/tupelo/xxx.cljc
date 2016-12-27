;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.xxx
  (:use tupelo.xxx
        expectations )
  (:require [clojure.string   :as str]
            [schema.core      :as s]
            [tupelo.core      :as t]
            [tupelo.misc      :as misc]
            [clojure.math.combinatorics  :as combo]
  ))
(t/refer-tupelo)
(set! *warn-on-reflection* true)
; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs


(def data {
  :tag :root
  :content [
    { :tag :a
      :content [
        {:tag :a1 :content [] } ] }
    { :tag :b
      :content [
        {:tag :b1 :content [] }
        {:tag :b2 :content [] } ] }
    { :tag :c
      :content [
        {:tag :c1 :content [] }
        {:tag :c2 :content [] }
        {:tag :c3 :content [] } ] } ] }
)

(defn walk-1* [result path-in node]
  (newline)
  (let [tag      (grab :tag node)
        _        (spyx tag)
        path-new (t/append path-in tag)
        _        (spyx path-new)
        content  (grab :content node)]
    (if (empty? content)
      (do
        (swap! result append path-new)
        (println "....saved"))
      (doseq [child content]
        (walk-1* result path-new child)))))

(defn walk-1 [node]
  (let [result    (atom [])
        path      [] ]
    (walk-1* result path node)
    @result ))

(newline)
(let [result (walk-1 data)]
  (newline)
  (println :walk-1)
  (pretty result))

(newline)
(println "-----------------------------------------------------------------------------")

(defn walk-2 [node]
  (newline)
  (let [tag     (grab :tag node)
        _       (spyx tag)
        content (grab :content node)]
    (if (empty? content)
      [[tag]]
      (spy :mapv
        (mapv #(prepend tag %)
          (spy :apply-glue
            (apply glue
              (spy :forv
                (forv [child content]
                  (spy :walk-2
                    (walk-2 child)))))))))))

(let [result (walk-2 data)]
  (newline)
  (println :walk-2)
  (pretty result))

(newline)
(println "-----------------------------------------------------------------------------")
(defn walk-3 [node]
; (newline)
  (let [tag     (grab :tag node)
      ; _       (spyx tag)
        content (grab :content node)]
    (if (empty? content)
      (spyx [[tag]])
      (spy :mapv-prepend
        (mapv #(prepend tag %)
          (spy :apply-concat
            (apply concat
              (spy :mapv-recurse
                (mapv walk-3 content)))))))))

(let [result (walk-3 data)]
  (newline)
  (println :walk-3)
  (pretty result))



