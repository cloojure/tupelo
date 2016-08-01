(ns tst.tupelo.schema
  (:use tupelo.schema
        clojure.test )
  (:require [schema.core :as s]))

(set! *warn-on-reflection* false)

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

; #todo add more tests (& test.check for Eid, etc)
(deftest t-vecs
  (is (= [1]            (s/validate Vec1 [1] )))
  (is (= [1 2]          (s/validate Vec2 [1 2] )))
  (is (= [1 2 3]        (s/validate Vec3 [1 2 3] )))
  (is (= [1 2 3 4]      (s/validate Vec4 [1 2 3 4] )))
  (is (= [1 2 3 4 5]    (s/validate Vec5 [1 2 3 4 5] ))))

