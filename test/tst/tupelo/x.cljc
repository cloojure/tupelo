;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x
  "Experimental new code"
  (:use clojure.test tupelo.test tupelo.x)
  (:require
    [clojure.core.async :as ca :refer [go go-loop chan thread]]
    [clojure.pprint :refer [pprint]]
    [tupelo.core :as t :refer [lazy-cons]]
    [tupelo.async :as ta]
    ))
(t/refer-tupelo)

(spyx (lazy-seq nil))
(spyx (lazy-cons 3 (lazy-seq nil)))
(spyx (lazy-cons 2 (lazy-cons 3 (lazy-seq nil))))
(spyx (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 (lazy-seq nil)))))
(newline)


(defgen range-gen
; "A generator 'range' function."
  [limit]
  (loop [cnt 0]
    (when (< cnt limit)
      (yield cnt)
      (recur (inc cnt)))))

(defgen concat-gen
  "A generator 'range' function."
  [& collections]
  (doseq [curr-coll collections]
    (doseq [item curr-coll]
      (yield item))))

(defgen empty-gen-fn [])

(deftest t-global
  (is= (range 10) (range-gen 10))
  (let [c1 [1 2 3]
        c2 [4 5 6]
        c3 [7 8 9]
        result (concat-gen c1 c2 c3) ]
    (is= result (thru 1 9))
  )
  (is (nil? (empty-gen-fn)))
  )

(spyx (range-gen 5))
(spyx (range-gen 10))

(spyx (concat-gen [1 2 3] [4 5 6] [7 8 9] ))
(spyx (empty-gen-fn))
