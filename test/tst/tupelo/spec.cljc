;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.spec
  (:use tupelo.test )
  (:require
    [clojure.spec.alpha :as sp]
    [clojure.spec.test.alpha :as stest]
    [clojure.spec.gen.alpha :as gen]
    [tupelo.core :as t]
    [tupelo.spec :as tsp]
  ))
(t/refer-tupelo)

(dotest
  (is   (sp/valid? ::tsp/anything 5 ))
  (is   (sp/valid? ::tsp/anything "joe" ))
  (is   (sp/valid? ::tsp/anything { :blah 42 :blue 66 :hut! 'hut! }))
  (isnt (sp/valid? ::tsp/nothing 5 )))

