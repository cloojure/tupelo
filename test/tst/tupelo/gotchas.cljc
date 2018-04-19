;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.gotchas
  (:use tupelo.core tupelo.test)
  (:require
    #?@(:clj [
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [tupelo.impl :as i]
    [tupelo.core :as t]
             ])
  ))
; #todo add example for duplicates in clojure.core.combo

#?(:clj (do

; rest/next too loose
(dotest
  ; Expected, intuitive behavior
  (throws? (seq  5))
  (= [5]   (vector 5))
  (throws? (vec  5))
  (= [5]   (list 5))
  (throws? (apply list 5))
  (throws? (first 5))
  (throws? (second 5))
  (throws? (rest 5))
  (throws? (next 5))

  ; Unexpected, non-intuitive behavior
  (is= nil   (seq  nil)) ; should throw
  (is= [nil] (vector  nil))
  (is= []    (vec  nil)) ; should throw
  (is= [nil] (list  nil))
  (is= []    (apply list nil))
  (is= nil   (first nil)) ; should throw
  (is= nil   (second nil)) ; should throw
  (is= []    (rest nil)) ; should throw
  (is= nil   (next nil)) ; should throw

  ; Unexpected, non-intuitive behavior
  (is= nil   (seq  [])) ; should be []
  (is= []    (vec  []))
  (is= [[]]  (list  []))
  (is= []    (apply list []))
  (is= nil   (first [])) ; should throw
  (is= nil   (second [])) ; should throw
  (is= []    (rest [])) ; should throw
  (is= nil   (next [])) ; should throw

  (is= [5]   (seq  [5]))
  (is= [5]   (vec  [5]))
  (is= [5 6] (vec  [5 6]))
  (is= [[5]] (list  [5]))
  (is= [5]   (apply list [5]))
  (is= [5 6] (apply list [5 6]))
  (is= [6 5] (into (list) [5 6])) ; accidentally reversed
  (is= 5     (first [5]))
  (is= nil   (second [5])) ; should throw
  (is= []    (rest [5]))
  (is= nil   (next [5])) ; should be []

  ; Predictable bahavior
  (throws? (t/xfirst nil))
  (throws? (t/xsecond nil))
  (throws? (t/xrest nil)) ; drop first item or throw if not more

  (throws? (t/xfirst []))
  (throws? (t/xsecond []))
  (throws? (t/xrest [])) ; drop first item or throw if not more

  (is= 5   (t/xfirst [5]))
  (throws? (t/xsecond [5]))
  (is= []  (t/xrest [5])) ; drop first item or throw if not more
  (is= [5] (t/xrest [4 5]))

)

; vec & (apply list ...) too loose
(dotest
  (is= []  (vec        nil)) ; should throw
  (is= []  (apply list nil)) ; should throw

  (is= []  (vec        []))
  (is= []  (apply list []))

  (is= [5]  (vec        [5]))
  (is= [5]  (apply list [5])))

(dotest
  (is= [1 2 3] (conj [1] 2 3))
  (is= [1 2 3] (conj [1 2] 3))

  (is= [3 2 1] (conj (list) 1 2 3))
  (is= [3 2 1] (conj (list 1) 2 3))
  (is= [3 1 2] (conj (list 1 2) 3))

  (is= [1 2 3] (into (vector) [1 2 3]))
  (is= [1 2 3] (into (vector 1) [2 3]))
  (is= [1 2 3] (into (vector 1 2) [3]))

  (is= [3 2 1] (into (list) [1 2 3]))
  (is= [3 2 1] (into (list 1) [2 3]))
  (is= [3 1 2] (into (list 1 2) [3]))
)

; Clojure is consistent & symmetric for if/if-not, when/when-not, every?/not-every?
; Clojure is inconsistent & broken for
;  not-empty
;  empty?
;  any?
;  some vs some? (truthy vs not-nil?)

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
  (is= 0  ) (count nil)
)

(i/when-clojure-1-9-plus
  (dotest
    ; `any?` always returns true
    (is= true (any? false))
    (is= true (any? nil))
    (is= true (any? 5))
    (is= true (any? "hello"))

    ; tests a predicate fn on each element
    (is= false (not-any? odd? [1 2 3]))
    (is= true (not-any? odd? [2 4 6]))

    ; explicit & consistent way of testing predicate
    (is (t/has-some? odd? [1 2 3]))
    (is (t/has-none? odd? [2 4 6]))
  ))

(dotest
  (is= nil  (some #{false} [false true]))
  (is= true (some #(= false %) [false true]))
  (is= true (some #{false true} [false true]))
  (is= true (some #{false true} [false true]))
  (is= true (some? false ))
  (is= true (some? true ))
)

; samples for dospec & check-not
;-----------------------------------------------------------------------------
(dospec 9
  (prop/for-all [val (gen/vector gen/any)]
    (is (= (not (empty? val)) (t/not-empty? val)))
    (isnt= (empty? val) (empty val))))
(i/when-clojure-1-9-plus
  (dotest
    (check-isnt 33
      (prop/for-all [val (gen/vector gen/int)]
        (= (any? val) (not-any? odd? val))))))

(dotest
  (is= 'quote (first ''hello))  ; 2 single quotes
)

))
