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
; #todo add example for duplicates in clojure.core.combo

(dospec 9
  (prop/for-all [val (gen/vector gen/any)]
    (is (= (not (empty? val)) (t/not-empty? val)))
    (isnt= (empty? val) (empty val))))
(dotest
  (isnt
    (t/grab :result
      (tc/quick-check 33
        (prop/for-all [val (gen/vector gen/int)]
          (= (any? val) (not-any? odd? val)))))))

(dotest
  (is= (empty [1 2 3]) [])
  (is= (not-empty  [1 2 3])  [1 2 3]
    (t/validate t/not-empty? [1 2 3])) ; explicit validation of non-empty collection
  (is= (not (empty [1 2 3])) false)

  (is= (empty? [1 2 3]) false)
  ;(not-empty?  [1 2 3])  => Unable to resolve symbol: not-empty?
  (is= (t/not-empty? [1 2 3]) true) ; explicit test for non-empty collection
)

(dotest
  ; always returns true
  (is= true (any? false))
  (is= true (any? nil))
  (is= true (any? 5))
  (is= true (any? "hello"))

  ; tests a predicate fn on each element
  (is= false (not-any? odd? [1 2 3]))
  (is= true  (not-any? odd? [2 4 6]))

  ; explicit & consistent way of testing
  (is (t/has-some? odd? [1 2 3]))
  (is (t/has-none? odd? [2 4 6])))




