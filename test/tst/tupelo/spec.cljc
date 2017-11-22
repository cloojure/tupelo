;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.spec
  (:use tupelo.test )
  (:require
    [clojure.set :as set]
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [tupelo.spec :as tsp]
  ))

(t/refer-tupelo)

(i/when-clojure-1-9-plus
  (require
    '[clojure.spec.alpha :as s]
    '[clojure.spec.gen.alpha :as gen]
    '[clojure.spec.test.alpha :as stest] )
  (dotest
    (is (s/valid? ::i/anything 5))
    (is (s/valid? ::i/anything "joe"))
    (is (s/valid? ::i/anything #{{:blah 42} [:blue 66] :hut! 'hut! "hut!"}))
    (isnt (s/valid? ::i/nothing 5))

    (when false
      (spyx (s/exercise ::i/anything))
     ;(spyx (s/exercise ::i/anything {::i/anything gen/int}))  ;#todo not quite right yet
      (nl) (time (spyx (stest/check `i/truthy? {:clojure.spec.test.check/opts {:num-tests 99}})))
      (nl) (time (spyx (stest/check `i/falsey? {:clojure.spec.test.check/opts {:num-tests 99}}))))

    ))

