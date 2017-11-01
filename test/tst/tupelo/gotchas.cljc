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

; rest/next too loose
(dotest
  (is= nil (seq  nil)) ; should be undefined
  (is= []  (rest nil)) ; should throw
  (is= nil (next nil)) ; should throw

  (is= nil (seq  [])) ; should be []
  (is= []  (rest [])) ; should throw
  (is= nil (next [])) ; should throw

  (is= [5] (seq  [5]))
  (is= []  (rest [5]))
  (is= nil (next [5])) ; should be []

  ; consistent behavior: drop first item or throw if not first
  (throws? (t/xrest nil))
  (throws? (t/xrest []))
  (is= []  (t/xrest [5]))
  (is= [5] (t/xrest [4 5]))

  ; Unexpected, non-intuitive behavior
  (is= nil (first nil)) ; should throw
  (is= nil (first [])) ; should throw
  (is= 5   (first [5]))
  (is= nil (second nil)) ; should throw
  (is= nil (second [])) ; should throw
  (is= nil (second [5])) ; should throw

  ; Predictable bahavior
  (throws? (t/xfirst nil))
  (throws? (t/xfirst []))
  (is= 5   (t/xfirst [5]))
  (throws? (t/xsecond nil))
  (throws? (t/xsecond []))
  (throws? (t/xsecond [5]))
)

; vec & (apply list ...) too loose
(dotest
  (is= []  (vec        nil)) ; should throw
  (is= []  (apply list nil)) ; should throw

  (is= []  (vec        []))
  (is= []  (apply list []))

  (is= [5]  (vec        [5]))
  (is= [5]  (apply list [5])))

; Clojure is consistent & symmetric for if/if-not, when/when-not, every?/not-every?
; Clojure is inconsistent & broken for
;  not-empty
;  empty?
;  any?
;  some vs some?

; Clojure has `empty?` but no `not-empty?`.  However, it does have `empty` and `not-empty`.  Confusing!
; empty / not-empty vs empty? (not-empty? missing)
; not-empty? is missing for no good reason
; empty/not-empty are not mirror images of each other; (not (empty coll)) != (not-empty coll)
(dotest
  (is= (empty [1 2 3]) [])
  (is= (not-empty  [1 2 3])  [1 2 3]
    (t/validate t/not-empty? [1 2 3])) ; explicit validation of non-empty collection
  (is= (not (empty [1 2 3])) false)

  (is= (empty? [1 2 3]) false)
  ;(not-empty?  [1 2 3])  => Unable to resolve symbol: not-empty?
  (is= (t/not-empty? [1 2 3]) true) ; explicit test for non-empty collection

  ; empty? / count too loose:
  (is= true (empty? nil))
  (is= 0  ) (count nil))

(dotest
  ; `any?` always returns true
  (is= true (any? false))
  (is= true (any? nil))
  (is= true (any? 5))
  (is= true (any? "hello"))

  ; tests a predicate fn on each element
  (is= false (not-any? odd? [1 2 3]))
  (is= true  (not-any? odd? [2 4 6]))

  ; explicit & consistent way of testing predicate
  (is (t/has-some? odd? [1 2 3]))
  (is (t/has-none? odd? [2 4 6])))



; samples for dospec & check-not
;-----------------------------------------------------------------------------
(dospec 9
  (prop/for-all [val (gen/vector gen/any)]
    (is (= (not (empty? val)) (t/not-empty? val)))
    (isnt= (empty? val) (empty val))))
(dotest
  (check-isnt 33
    (prop/for-all [val (gen/vector gen/int)]
      (= (any? val) (not-any? odd? val)))))


(dotest
  (is= 'quote (first ''hello))  ; 2 single quotes
)
