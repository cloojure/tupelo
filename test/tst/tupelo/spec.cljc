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
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.spec.test.alpha :as stest]
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [tupelo.spec :as tsp] ))

(t/refer-tupelo)

(dotest
  (is   (s/valid? ::tsp/anything 5 ))
  (is   (s/valid? ::tsp/anything "joe" ))
  (is   (s/valid? ::tsp/anything #{ {:blah 42} [:blue 66] :hut! 'hut! "hut!"} ))
  (isnt (s/valid? ::tsp/nothing 5 ))

  (when false
    (spyx (s/exercise ::tsp/anything))
    ;(spyx (s/exercise ::tsp/anything {::tsp/anything gen/int}))  ;#todo not quite right yet
    (nl) (time (spyx (stest/check `i/truthy? {:clojure.spec.test.check/opts {:num-tests 99}})))
    (nl) (time (spyx (stest/check `i/falsey? {:clojure.spec.test.check/opts {:num-tests 99}}))))

)

