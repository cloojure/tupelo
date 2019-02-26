;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.gotchas
  (:require
    [clojure.set :as set]
    [tupelo.string :as ts]
    #?@(:clj [[clojure.test.check.generators :as gen]
              [clojure.test.check.properties :as prop]
              [tupelo.test :as tt :refer [define-fixture dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws? ]]
              [tupelo.core :as t :refer [spy spyx spyxx]]
              ])
    #?@(:cljs [[tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]]
               [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
               [tupelo.string :as ts :include-macros true]
               ])))

; #todo add example for duplicates in clojure.core.combo


; rest/next too loose
(dotest
  ; Expected, intuitive behavior
  (throws? (seq 5))
  (= [5] (vector 5))
  (throws? (vec 5))
  (= [5] (list 5))
  (throws? (apply list 5))
  (throws? (first 5))
  (throws? (second 5))
  (throws? (rest 5))
  (throws? (next 5)) )

(dotest
  ; Unexpected, non-intuitive behavior
  (is= nil (seq nil)) ; should throw
  (is= [nil] (vector nil))
  (is= [] (vec nil)) ; should throw
  (is= [nil] (list nil))
  (is= [] (apply list nil))
  (is= nil (first nil)) ; should throw
  (is= nil (second nil)) ; should throw
  (is= [] (rest nil)) ; should throw
  (is= nil (next nil))) ; should throw

(dotest
  ; Unexpected, non-intuitive behavior
  (is= nil (seq [])) ; should be []
  (is= [] (vec []))
  (is= [[]] (list []))
  (is= [] (apply list []))
  (is= nil (first [])) ; should throw
  (is= nil (second [])) ; should throw
  (is= [] (rest [])) ; should throw
  (is= nil (next []))) ; should throw

(dotest
  (is= [5] (seq [5]))
  (is= [5] (vec [5]))
  (is= [5 6] (vec [5 6]))
  (is= [[5]] (list [5]))
  (is= [5] (apply list [5]))
  (is= [5 6] (apply list [5 6]))
  (is= [6 5] (into (list) [5 6])) ; accidentally reversed
  (is= 5 (first [5]))
  (is= nil (second [5])) ; should throw
  (is= [] (rest [5]))
  (is= nil (next [5]))) ; should be []

(dotest
  ; Predictable bahavior
  (throws? (t/xfirst nil))
  (throws? (t/xsecond nil))
  (throws? (t/xrest nil)) ; drop first item or throw if not more

  (throws? (t/xfirst []))
  (throws? (t/xsecond []))
  (throws? (t/xrest [])) ; drop first item or throw if not more

  (is= 5 (t/xfirst [5]))
  (throws? (t/xsecond [5]))
  (is= [] (t/xrest [5])) ; drop first item or throw if not more
  (is= [5] (t/xrest [4 5])))

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
  (is= [3 1 2] (into (list 1 2) [3])) )

; Clojure is consistent & symmetric for if/if-not, when/when-not, every?/not-every?
; Clojure is inconsistent & broken for
;      empty vs     empty?
;  not-empty vs not-empty?
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
  (is= 0    (count nil)) )

(dotest
  (is= nil  (some #{false}      [false true]))
  (is= true (some #(= false %)  [false true]))
  (is= true (some #{false true} [false true]))
  (is= true (some #{false true} [false true]))
  (is= true (some? false ))
  (is= true (some? true )))

(dotest
  (is= false (contains? [1 2 3 4] 4))
  (is= false (contains? [:a :b :c :d] :a)))

; map oddities
(dotest
  (is= {:a 1 :b 2} (conj {:a 1} [:b 2])) ; MapEntry as 2-vec
  (is= {:a 1 }     (conj {:a 1} nil)) ; this is ok => noop
  (throws?         (conj {:a 1} [])) ; illegal
  (is= {:a 1}      (into {:a 1} [])) ; this works
  (is= {:a 1 :b 2} (conj {:a 1} {:b 2})) ; this works, but shouldn't

  ; nil same as {} (empty map)
  (is= {:a 1}      (assoc nil :a 1))
  (is= {:a {:b 1}} (assoc-in nil [:a :b] 1)))

(dotest             ; conj inconsistencies
  (is= [1 2 nil] (conj [1 2] nil))
  (is= [nil 1 2] (conj '(1 2) nil))
  (is= {:a 1} (conj {:a 1} nil))
  (is= #{1 2 nil} (conj #{1 2} nil))
  (throws? (conj "ab" nil)) )

(dotest
  (is= "abc" (str "ab" \c))
  (is= "ab" (str "ab" nil))
  (is= "abc" (str "ab" nil \c))
  (is=  "123" (ts/quotes->single (pr-str 123)))
  (is= "'abc'" (ts/quotes->single (pr-str "abc")))
  (is= "nil" (ts/quotes->single (pr-str nil))) )


; "generic" indexing is a problem; always be explicit with first, nth, get, etc
(dotest
  (let [vv [1 2 3]
        ll (list 1 2 3)
        cc (cons 1 [2 3])]
    (is= 1 (vv 0))  ; works fine
    (throws? (ll 0)) ; clojure.lang.PersistentList cannot be cast to clojure.lang.IFn
    (throws? (cc 0)) ; clojure.lang.Cons cannot be cast to clojure.lang.IFn

    ; best solution
    (is= 1 (first vv))
    (is= 1 (first ll))
    (is= 1 (first cc))))

; binding operates in parallel, not sequentially
(def ^:dynamic xx nil)
(def ^:dynamic yy nil)
(dotest
  (binding [xx 99
            yy xx]
    (is= 99 xx)
    (is= nil yy)))

; every? not-every? some not-any? + has-some? has-none?
(dotest             ; should throw if empty arg
  (is (every? even? []))
  (is (every? odd? [])))

#?(:clj
   ; transducer surprises
   (dotest
     ; when use `comp` with normal functions, data flows leftward (result <-- fn <-- data)
     (let [comp-fn        (comp #(mapv inc %) #(filter even? %))
           result-comp-fn (comp-fn (range 10))]
       (is= [1 3 5 7 9] result-comp-fn))

     ; when use `comp` with transducers, data flows rightward (data --> txd --> result )
     (let [xform            (comp (map inc) (filter even?))
           result-comp-txd  (into [] xform (range 10))
           result-transduce (transduce xform
                              conj [] (range 10)) ; alternate syntax
           ]
       (is= [2 4 6 8 10] result-comp-txd result-transduce))

     ; "dataflow" processing with thread-first macro is top->bottom (i.e. rightward)
     (let [result-thread (->> (range 10)
                           (map inc)
                           (filter even?))]
       (is= [2 4 6 8 10] result-thread))))

#?(:clj
   (do              ; #todo make work for clj/cljs

     (t/when-clojure-1-9-plus
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

     ; samples for dospec & check-isnt
     ;-----------------------------------------------------------------------------
     (tt/dospec 9
       (prop/for-all [val (gen/vector gen/any)]
         (is (= (not (empty? val)) (t/not-empty? val)))
         (isnt= (empty? val) (empty val))))
     (t/when-clojure-1-9-plus
       (dotest
         (tt/check-isnt 33
           (prop/for-all [val (gen/vector gen/int)]
             (= (any? val) (not-any? odd? val))))))

     ;-----------------------------------------------------------------------------
     ; quote surprises
     (dotest
       (is= 'quote (first ''hello)) ; 2 single quotes
       (isnt= '{:a 1 :b [1 2]} '{:a 1 :b '[1 2]})
       (is= '{:a 1 :b [1 2]} `{:a 1 :b [1 2]})
       (is= '{:a 1 :b [1 2]} (quote {:a 1 :b [1 2]})))

     ;-----------------------------------------------------------------------------
     ; record-map equality fails
     (defrecord SampleRec [a b])
     (dotest
       (let [sampleRec (->SampleRec 1 2)]
         (isnt (= sampleRec {:a 1 :b 2})) ; fails for clojure.core/= "
         (is (t/val= sampleRec {:a 1 :b 2})))) ; works for tupelo.core/val=

     ;-----------------------------------------------------------------------------
     ; clojure.set has no type-checking
     (dotest
       (is= [:z :y :x 1 2 3] (set/union '(1 2 3) '(:x :y :z)))
       (is= [1 2 3 :x :y :z] (set/union [1 2 3] [:x :y :z]))
       (is= #{1 2 3 :x :y :z} (set/union #{1 2 3} #{:x :y :z}))
       )
     ))


#?(:cljs
   (do

     ; assumes nil=0, etc (from JS)
     (dotest
       (is= {:a 1} (update {} :a inc))
       (is= 1 (inc nil))
       (is= 1 (+ 1 nil)))

     ))
