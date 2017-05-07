;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.enlive
  "Experimental new code"
  (:require
    [clojure.core.async     :as ca :refer [go go-loop chan thread]]
    [schema.core :as s]
    [tupelo.core :as t]
  ))
(t/refer-tupelo)

;#todo write fn enlive-remove-attrs

;---------------------------------------------------------------------------------------------------
(defn- ^:no-doc find-tree-impl
  [result-atom parents tree tgt-path]
  ;(newline)
  ;(println :result-atom) (pretty @result-atom)
  ;(spyx parents)
  ;(spyx tree)
  ;(spyx tgt-path)
  (when (map? tree) ; avoid trying to process value `1` on leaf like [:a 1]
    (when (and (not-empty? tree) (not-empty? tgt-path))
      (let [tgt           (first tgt-path)
            tgt-path-rest (rest tgt-path)
            tag           (grab :tag tree)
            content       (grab :content tree)]
        (when (or (= tag :*) (= tag :**))
          (throw (IllegalArgumentException. (str "fing-tag*: found reserved tag " tag " in tree"))))
        ;(spyx tgt)
        ;(spyx tgt-path-rest)
        (if (or (= tgt tag) (= tgt :*))
          (do
            ;(println :200 "match tag:" tag)
            (if (empty? tgt-path-rest)
              (let [soln {:parent-path parents
                          :subtree     tree}]
                ;(println :210 "empty soln:" soln)
                (swap! result-atom glue #{soln}))
              (let [parents-new (append parents tag)]
                ;(println :220 "not-empty parents-new:" parents-new)
                (doseq [child-tree content]
                  ;(println :230 "child-tree:" child-tree)
                  (find-tree-impl result-atom parents-new child-tree tgt-path-rest)))))
          (when (= tgt :**)
            ;(println :300 "tgt = :**")
            (when (not-empty? tgt-path-rest) ; :** wildcard cannot terminate the tgt-path
              (let [parents-new (append parents tag)]
                ;(println :320 ":** parents-new:" parents-new)
                ;(println (str :331 "  recurse  parents:" parents "   tree:" tree "  tgt-path-rest:" tgt-path-rest))
                (find-tree-impl result-atom parents tree tgt-path-rest)
                (doseq [child-tree content]
                  ;(println :330 ":** child-tree:" child-tree)
                  ;(println (str :332 "    recurse  parents-new:" parents-new "  tgt-path:" tgt-path))
                  (find-tree-impl result-atom parents-new child-tree tgt-path))))))))))

; #todo need update-tree & update-leaf fn's
(defn find-tree
  "Searches an Enlive-format tree for the specified tgt-path"
  [tree tgt-path]
  ;(println "=============================================================================")
  (when (empty? tree)
    (throw (IllegalStateException. "find-tree: tree is empty")))
  (when (empty? tgt-path)
    (throw (IllegalStateException. "find-tree: tgt-path is empty")))
  (when (= :** (last tgt-path))
    (throw (IllegalArgumentException. "find-tree: recursive-wildcard `:**` cannot terminate tgt-path")))

  (let [result-atom (atom #{}) ]
    (try
      (find-tree-impl result-atom [] tree tgt-path)
      (catch Exception e
        (throw (RuntimeException. (str "find-tree: failed for tree=" tree \newline
                                    "  tgt-path=" tgt-path \newline
                                    "  caused by=" (.getMessage e))))))
    @result-atom))

(defn find-leaf [tree tgt-path leaf-val]
  "Searches an Enlive-format tree for the specified tgt-path & leaf value."
  (let [subtree-solns     (find-tree tree tgt-path)
        tgt-path-terminal (last tgt-path)
        tgt-leaf-node     {:tag tgt-path-terminal :attrs {} :content [leaf-val]}
        results           (into #{}
                            (for [soln subtree-solns
                                  :when (or (= tgt-leaf-node (grab :subtree soln))
                                          (= leaf-val :*)) ]
                              soln)) ]
    results ))

(defn- find-tree-result->hiccup
  [soln-set]
  (into #{}
    (for [soln soln-set]
      (update-in soln [:subtree] enlive->hiccup))))

(defn find-tree-hiccup [tree tgt-path]
  "Searches an Hiccup-format tree for the specified tgt-path,
  returning a Hiccup-format result."
  (find-tree-result->hiccup
    (find-tree (hiccup->enlive tree) tgt-path)))

(defn find-leaf-hiccup [tree tgt-path leaf-val]
  "Searches an Hiccup-format tree for the specified tgt-path & leaf value,
  returning a Hiccup-format result."
  (find-tree-result->hiccup
    (find-leaf (hiccup->enlive tree) tgt-path leaf-val)))

; Enlive format   #todo: need *-hiccup versin?
(defn get-tree      ; #todo need test & README
  [tree tgt-path]
  (-> (find-tree tree tgt-path)
    only
    :subtree ))

; Enlive format   #todo: need *-hiccup versin?
(defn get-leaf      ; #todo need test & README
  [tree tgt-path]
  (-> (get-tree tree tgt-path)
    :content
    only))

