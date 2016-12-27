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
    {:tag :a :content [
      {:tag :a1 :content [] }
      {:tag :a2 :content [] }
    ] }
    {:tag :b :content [
      {:tag :b1 :content [] }
      {:tag :b2 :content [] }
    ] }
  ]
}
)

(defn walk* [path-in node]
  (newline)
  (spyx path-in)
  (spyx (grab :tag node))
  (doseq [child (grab :content node)]
    (let [tag-child (grab :tag child)
          path-new (t/append path-in tag-child)]
      (walk* path-new child)))
)

(defn walk [node]
  (walk* [] node))

(spyx (walk data))

