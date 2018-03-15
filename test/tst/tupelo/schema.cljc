(ns tst.tupelo.schema
  (:use tupelo.schema tupelo.core tupelo.test )
  (:require [tupelo.core :as t]
            [schema.core :as s] ))

; #todo add more tests (& test.check for Eid, etc)
(dotest
  (is= [1]            (s/validate Single  [1] ))
  (is= [1 2]          (s/validate Pair    [1 2] ))
  (is= [1 2 3]        (s/validate Triple  [1 2 3] ))
  (is= [1 2 3 4]      (s/validate Quad    [1 2 3 4] ))

  (is= [1 2 3]        (s/validate List [1 2 3] ))

  (throws? (s/validate Single [1 2] ))
  (throws? (s/validate Pair   [1] ))
  (throws? (s/validate List  {:a 1 :b 2} ))
  (throws? (s/validate List #{:a 1 :b 2} )))

(dotest
  (is= {}           (s/validate Map  {} ))
  (is= {:a 1}       (s/validate Map  {:a 1} ))
  (is= {:a 1 :b 2}  (s/validate Map  {:a 1 :b 2} ))
  (is= {:a 1 :b 2}  (s/validate Map  {:a 1 :b 2} ))

  (throws? (s/validate Map  nil ))
  (throws? (s/validate Map  [1 2 3] ))
  (throws? (s/validate Map #{1 2 3} )))

(dotest
  (is= #{}           (s/validate Set  #{} ))
  (is= #{:a 1}       (s/validate Set  #{:a 1} ))
  (is= #{:a 1 \b 2}  (s/validate Set  #{:a 1 \b 2} ))
  (is= #{:a 1 \b 2}  (s/validate Set  #{:a 1 \b 2} ))

  (throws? (s/validate Set  nil ))
  (throws? (s/validate Set  [1 2 3] ))
  (throws? (s/validate Set  {:a 1 :b 2} )))
