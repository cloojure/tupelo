;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.async
  (:use tupelo.async
        clojure.test)
  (:require [clojure.string         :as str]
            [schema.core            :as s]
            [tupelo.core            :as t]
            [tupelo.async           :as tas]
            [clojure.core.async :refer [ go go-loop chan buffer close! thread alts! alts!! timeout close!  ]]
  ))

(defn cb-tst [tst-val arg] 
 ;(spy :msg "cb-tst: " arg)
  (is (= tst-val arg)))

(deftest t-basic
  (let [c1 (chan 1) ]
    (go 
      (tas/put-go! c1 42)
      (is (= 42 (tas/take-go! c1)))))

  (let [c1 (chan 9) ]
    (tas/put-now!   c1 88)
    (tas/put-later! c1 99)

    (is (= 88 (tas/take-now! c1)))
    (tas/take-later! c1 (partial cb-tst 99)))

  (let [ c0 (chan) ]
    (tas/put-later! c0 10 (partial cb-tst true))
    (tas/put-later! c0 11 (partial cb-tst true))
    (tas/put-later! c0 12 (partial cb-tst true))

    (is (= 10 (tas/take-now!  c0)))
    (is (= 11 (tas/take-now!  c0)))
    (is (= 12 (tas/take-now!  c0)))

    (close! c0) 
    (is (false? (tas/put-later! c0 -1 (partial cb-tst false) )))
    (is (nil? (tas/take-now!  c0)))))

; #todo maybe close! -> close-now! if current thread?
