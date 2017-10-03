;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.gotchas
  (:use tupelo.test)
  (:require
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [tupelo.core :as t] ))
; (t/refer-tupelo)
; #todo add example for duplicates in clojure.core.combo

; (is (thrown? Throwable (tar/row-set demo 2 [[1 2 3]])))
(dospec 9
  (prop/for-all [val (gen/vector gen/any)]
  #_(is (= (not (empty? val)) (not-empty? val))) ; RuntimeException: Unable to resolve symbol: not-empty?
    (is (= (not (empty? val)) (t/not-empty? val)))

    (isnt= (empty? val) (empty val))

  ))
(dotest
  (is= (not-empty  [1 2 3])  [1 2 3]
    (t/validate t/not-empty? [1 2 3]))
  (is= (not (empty [1 2 3])) false)
  (is= (empty [1 2 3]) [])

  (is= (empty? [1 2 3]) false)
  ;(not-empty?  [1 2 3])  => Unable to resolve symbol: not-empty? (fixed in tupelo)

  )


(dotest
  (isnt (t/grab :result
          (tc/quick-check 22
            (prop/for-all [val (gen/vector gen/int)]
              (= (any? val) (not-any? odd? val))))))
  (let [val [3]]
    (not= (any? val) (not-any? odd? val)))

  )




