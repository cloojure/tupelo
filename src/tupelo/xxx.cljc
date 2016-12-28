;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.xxx
  "Miscellaneous functions."
  (:require 
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t :refer [spyx spy glue grab append prepend forv]]
    [tupelo.schema :as ts]
  ))
(t/refer-tupelo)

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

(defn walk-3 [node]
  (let [tag     (grab :tag node)
        ; _       (spyx tag)
        content (grab :content node)]
    (if (empty? content)
      (spyx [[tag]])
      (spy :mapv-cons
        (mapv (partial cons tag)
          (spy :apply-concat
            (apply concat
              (spy :mapv-recurse
                (mapv walk-3 content)))))))))

(defn walk-4 [node]
  (let [tag     (grab :tag node)
        ; _       (spyx tag)
        content (grab :content node)]
    (if (empty? content)
      (spyx [[tag]])
      (spy :mapv-cons
        (mapv #(prepend tag %)
          (spy :mapcat
            (mapcat walk-4 content)))))))

(def Path [s/Keyword])
(def PathList [Path])
(def MapList  [ts/KeyMap])
(def Node {:tag     s/Any
           :content MapList})

(s/defn walk-6 :- PathList
  "Given a node, return a list of the paths to all leaf nodes."
  [node :- Node]
  (let [tag     (grab :tag node)
        content (grab :content node)]
    (if (empty? content)
      [[tag]]
      (mapv #(prepend tag %)
        (mapcat walk-6 content)))))

