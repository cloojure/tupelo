(ns tst.tupelo.schema
  (:use tupelo.test )
  (:require [tupelo.core :as t]
            [tupelo.schema :as ts]
            [schema.core :as s] ))
(t/refer-tupelo)
(set! *warn-on-reflection* false)

; #todo add more tests (& test.check for Eid, etc)
(deftest t-vecs
  (is= [1]            (s/validate ts/Vec1 [1] ))
  (is= [1 2]          (s/validate ts/Vec2 [1 2] ))
  (is= [1 2 3]        (s/validate ts/Vec3 [1 2 3] ))
  (is= [1 2 3 4]      (s/validate ts/Vec4 [1 2 3 4] ))
  (is= [1 2 3 4 5]    (s/validate ts/Vec5 [1 2 3 4 5] ))

  (is= [1 2 3]        (s/validate ts/List [1 2 3] ))

  (throws? (s/validate ts/Vec1 [1 2] ))
  (throws? (s/validate ts/Vec2 [1] ))
  (throws? (s/validate ts/List  {:a 1 :b 2} ))
  (throws? (s/validate ts/List #{:a 1 :b 2} )))

(deftest t-Map
  (is= {}           (s/validate ts/Map  {} ))
  (is= {:a 1}       (s/validate ts/Map  {:a 1} ))
  (is= {:a 1 :b 2}  (s/validate ts/Map  {:a 1 :b 2} ))
  (is= {:a 1 :b 2}  (s/validate ts/Map  {:a 1 :b 2} ))

  (throws? (s/validate ts/Map  nil ))
  (throws? (s/validate ts/Map  [1 2 3] ))
  (throws? (s/validate ts/Map #{1 2 3} )))

(deftest t-Set
  (is= #{}           (s/validate ts/Set  #{} ))
  (is= #{:a 1}       (s/validate ts/Set  #{:a 1} ))
  (is= #{:a 1 \b 2}  (s/validate ts/Set  #{:a 1 \b 2} ))
  (is= #{:a 1 \b 2}  (s/validate ts/Set  #{:a 1 \b 2} ))

  (throws? (s/validate ts/Set  nil ))
  (throws? (s/validate ts/Set  [1 2 3] ))
  (throws? (s/validate ts/Set  {:a 1 :b 2} )))
