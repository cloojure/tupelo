;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.xxx
  (:use tupelo.xxx
        clojure.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t :refer [pretty nl]]
  ))
(t/refer-tupelo)
(set! *warn-on-reflection* true)

(def data {
           :tag :root
           :content [
                     { :tag :a
                      :content [
                               ] }
                     { :tag :b
                      :content [
                                {:tag :b1 :content [] }
                                {:tag :b2 :content [] } ] }
                     { :tag :c
                      :content [
                                {:tag :c1 :content [] }
                                {:tag :c2 :content [] }
                                {:tag :c3 :content [
                                                    { :tag :d
                                                     :content [
                                                               {:tag :d1 :content [] }
                                                               {:tag :d2 :content [] } ] }
                                                   ] } ] } ] }
)

#_(deftest t-xxx

  (nl)
  (let [result (walk-1 data)]
    (nl) (println :walk-1) (pretty result))

  (nl)
  (println "-----------------------------------------------------------------------------")
  (let [result (walk-2 data)]
    (nl) (println :walk-2) (pretty result))

  (nl)
  (println "-----------------------------------------------------------------------------")
  (let [result (walk-3 data)]
    (nl) (println :walk-3) (pretty result))

  (nl)
  (println "-----------------------------------------------------------------------------")
  (let [result (walk-4 data)]
    (nl) (println :walk-4) (pretty result))

  (nl)
  (println "-----------------------------------------------------------------------------")
  (spyx (s/validate Path [:a :b]))
  (let [result (walk-6 data)]
    (nl) (println :walk-6) (pretty result))



)