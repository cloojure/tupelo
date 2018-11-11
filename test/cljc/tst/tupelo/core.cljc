(ns tst.tupelo.core
  (:require
    [clojure.string :as str]
    #?@(:clj [
              [schema.core :as s]
              [tupelo.test   :refer [define-fixture dotest is isnt is= isnt= nonblank= testing throws?]]
              [tupelo.impl :as i]
              [tupelo.schema :as tsk]
              [tupelo.string :as ts]
             ])
    #?@(:cljs [
               [schema.core :as s]
               [tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= nonblank= testing throws?]]
               [tupelo.core :as t :include-macros true]
               [tupelo.schema :as tsk]
               [tupelo.string :as ts :include-macros true]
              ])
  ))

#?(:cljs (enable-console-print!))

(define-fixture :once
     {:enter (fn [ctx] (println "*** TEST ONCE *** - enter "))
      :leave (fn [ctx] (println "*** TEST ONCE *** - leave "))})
;--------------------------------------------------------------------------------------------------

(dotest
  (println "tst.tupelo.core test 1")
  (is= 2 (+ 1 1))

  (is (i/truthy? true))
  (is (i/truthy? 5))
  (is (i/falsey? false))
  (is (i/falsey? nil)))

(dotest
  (let [inf-rng-1 (map inc (range))]
    (is= 42 (i/only [42]))
    (is= :x (i/only [:x]))
    (is= "hello" (i/only ["hello"]))

    ; #todo #wip
    (throws? (i/only []))
    (throws? (i/only [:x :y]))
    (throws? (i/only inf-rng-1))

    (is= [1 2 3] (i/onlies [[1] [2] [3]]))
    (throws? (i/onlies [[1] [2] [3 4]]))
    (throws? (i/onlies [[1] [] [3]]))

    (is= 5 (i/only2 [[5]]))
    (throws? (i/only2 [[1 2]]))
    (throws? (i/only2 [[1] [2]]))

    (is (i/single? [42]))
    (is (i/single? [:x]))
    (is (i/single? ["hello"]))
    (isnt (i/single? []))
    (isnt (i/single? [:x :y]))
    (isnt (i/single? inf-rng-1))

    (is (i/pair? [42 43]))
    (is (i/pair? [:x :y]))
    (is (i/pair? ["hello" "there"]))
    (isnt (i/pair? []))
    (isnt (i/pair? [:y]))
    (isnt (i/pair? inf-rng-1))

    (is (i/triple? [42 43 44]))
    (is (i/triple? [:x :y :z]))
    (is (i/triple? ["hello" "there" "you"]))
    (isnt (i/triple? []))
    (isnt (i/triple? [:y]))
    (isnt (i/triple? [:x :y]))
    (isnt (i/triple? inf-rng-1))

    (is (i/quad? [42 43 44 45]))
    (is (i/quad? [:x :y :z :99]))
    (is (i/quad? ["hello" "there" "again" "you"]))
    (isnt (i/quad? []))
    (isnt (i/quad? [:x]))
    (isnt (i/quad? [:x :y]))
    (isnt (i/quad? [:x :y :z]))
    (isnt (i/quad? inf-rng-1))))

(dotest
  (let [inf-rng-1 (map inc (range))
        tst-map   (i/glue (sorted-map) {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})]

    (throws? (i/xtake 1 []))

    (is= [1] (i/xtake 1 [1]))
    (is= [1] (i/xtake 1 [1 2]))
    (is= [1] (i/xtake 1 inf-rng-1))
    (is= [1 2] (i/xtake 2 [1 2]))
    (is= [1 2] (i/xtake 2 inf-rng-1))
    (is= {:a 1} (i/xtake 1 tst-map))
    (is= {:a 1 :b 2} (i/xtake 2 tst-map))

    (throws? (i/xfirst []))
    (is= 1 (i/xfirst [1]))
    (is= 1 (i/xfirst [1 2]))
    (is= 1 (i/xfirst inf-rng-1))
    ;(is= {:a 1} (i/xfirst tst-map))

    (throws? (i/xsecond []))
    (throws? (i/xsecond [1]))
    (is= 2 (i/xsecond [1 2]))
    (is= 2 (i/xsecond [1 2 3]))
    (is= 2 (i/xsecond [1 2 3 4]))
    (is= 2 (i/xsecond inf-rng-1))
    ;(is= {:b 2} (i/xsecond tst-map))

    (throws? (i/xthird []))
    (throws? (i/xthird [1]))
    (throws? (i/xthird [1 2]))
    (is= 3 (i/xthird [1 2 3]))
    (is= 3 (i/xthird [1 2 3 4]))
    (is= 3 (i/xthird inf-rng-1))
    ;(is= {:b 92} (i/xthird tst-map))

    (throws? (i/xfourth []))
    (throws? (i/xfourth [1]))
    (throws? (i/xfourth [1 2]))
    (throws? (i/xfourth [1 2 3]))
    (is= 4 (i/xfourth [1 2 3 4]))
    (is= 4 (i/xfourth [1 2 3 4 5]))
    (is= 4 (i/xfourth inf-rng-1))
    ;(is= {:b 92} (i/xfourth tst-map))

    (throws? (i/xlast nil))
    (throws? (i/xlast []))
    (is= 5 (i/xlast [1 2 3 4 5]))
    ;(is= {:b 92} (i/xlast tst-map))

    (is= [1 2 3 4] (i/xbutlast [1 2 3 4 5]))
    (is= [] (i/xbutlast [1]))
    (throws? (i/xbutlast []))
    (throws? (i/xbutlast nil))
    ;(is= {:b 92} (i/xbutlast tst-map))

    (throws? (i/xrest []))
    (is= [] (i/xrest [1]))
    (is= [2] (i/xrest [1 2]))
    (is= [2 3] (i/xrest [1 2 3]))
    (is= [2 3 4] (i/xrest [1 2 3 4]))
    (is= 2 (first (i/xrest inf-rng-1)))

    (throws? (i/xvec nil))
    (is= [] (i/xvec []))
    (is= [1] (i/xvec '(1)))
    (is= [1 2] (i/xvec [1 2]))
    ))

(dotest
  (is= :23 (i/int->kw  23))
  (is=  23 (i/kw->int :23))

  (println :01 (i/edn->json {:a 1 :b 2}))
  (prn     :02 (i/edn->json {:a 1 :b 2}))

  (is=  {:a  1 :b  2}  (i/json->edn (ts/quotes->double "{'a':1, 'b':2}")))
  (is= "{'a':1,'b':2}" (ts/quotes->single (i/edn->json  {:a  1  :b  2})))

  (is= 'abc (i/kw->sym :abc))
  (is= "abc" (i/kw->str :abc))
  (is= 'abc (i/str->sym "abc"))
  (is= :abc (i/str->kw "abc"))
  (is= :abc (i/sym->kw 'abc))
  (is= "abc" (i/sym->str 'abc)))

(dotest
  (let [orig  {:b #{3 2 1}
               :a [1 2 3 { 5 :five 6 :six 4 :four }]
               :c (list 4 5 6)}
        result (str/replace
                 (with-out-str (println (i/prettify orig)))
                 \, \space)
        expected "{:a  [1 2    3 {4 :four
                                  5 :five
                                  6 :six}]
                   :b #{1 2 3}
                   :c  [4 5 6]} " ]
    (nonblank= result expected )))

(dotest
  ; (i/spyx (s/check-fn i/truthy? ))

  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [truthies    (i/keep-if boolean data)       ; coerce to primitive type
            falsies     (i/keep-if not     data) ]     ; unnatural syntax
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
              (= falsies  [false nil] ) )))
      (let [truthies    (i/keep-if i/truthy? data)
            falsies     (i/keep-if i/falsey? data) ]
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
              (= falsies  [false nil] ) ))
        (is (every? i/truthy? [true :a 'my-symbol 1 "hello" \x] ))
        (is (every? i/falsey? [false nil] ))
        (is (i/has-none? i/falsey? truthies))
        (is (i/has-none? i/truthy? falsies))

        (isnt (every? i/truthy? [true false]))
        (is (every? i/truthy? [true "FALSE"]))
        (is (every? i/truthy? [true ]))
        (is (every? i/truthy? []))))

    (testing "improved usage"
      (let [count-if (comp count i/keep-if) ]
        (let [num-true    (count-if boolean data)   ; awkward phrasing
              num-false   (count-if not     data) ] ; doesn't feel natural
          (is (and  (= 6 num-true)
                (= 2 num-false) )))
        (let [num-true    (count-if i/truthy? data)   ; matches intent much better
              num-false   (count-if i/falsey? data) ]
          (is (and  (= 6 num-true)
                (= 2 num-false) ))))))

  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [notties   (i/keep-if i/not-nil? data)
            nillies   (i/drop-if i/not-nil? data) ]
        (is (and  (= notties [true :a 'my-symbol 1 "hello" \x false] )
              (= nillies [nil] )))
        (is (every?    i/not-nil? notties))
        (is (every?        nil? [nil] ))
        (is (i/has-none?     nil? notties))
        (is (i/has-none? i/not-nil? nillies))))

    (testing "improved usage"
      (let [count-if (comp count i/keep-if) ]
        (let [num-valid-1     (count-if some?    data)  ; awkward phrasing, doesn't feel natural
              num-valid-2     (count-if i/not-nil? data)  ; matches intent much better
              num-nil         (count-if nil?     data) ]    ; intent is plain
          (is (and  (= 7 num-valid-1 num-valid-2 )
                (= 1 num-nil) )))))))


(dotest
  (is= true   (i/has-some? odd? [1 2 3] ) )
  (is= false  (i/has-some? odd? [2 4 6] ) )
  (is= false  (i/has-some? odd? []      ) )

  (is= false  (i/has-none? odd? [1 2 3] ) )
  (is= true   (i/has-none? odd? [2 4 6] ) )
  (is= true   (i/has-none? odd? []      ) ))

(dotest
  (is (every?        i/not-empty? ["one" [1] '(1) {:1 1} #{1}     ] ))
  (is (i/has-none?   i/not-empty? [ ""   [ ] '( ) {}     #{ }  nil] ))

  (is (i/has-none?   empty? ["one" [1] '(1) {:1 1} #{1}     ] ))
  (is (every?        empty? [ ""   [ ] '( ) {}     #{ }  nil] ))

  (is= (map i/not-empty? ["1" [1] '(1) {:1 1} #{1} ] )
    [true true true true true]  )
  (is= (map i/not-empty? ["" [] '() {} #{} nil] )
    [false false false false false false ] )

  (is= (i/keep-if i/not-empty?  ["1" [1] '(1) {:1 1} #{1} ] )
    ["1" [1] '(1) {:1 1} #{1} ] )
  (is= (i/drop-if i/not-empty?  [""  []  '()  {}     #{}  nil] )
    [""  []  '()  {}     #{}  nil] )

  (throws? (i/not-empty? 5))
  (throws? (i/not-empty? 3.14)))

;-----------------------------------------------------------------------------
; spy stuff
(dotest
  (is= "(+ 2 3) => 5"
    (ts/collapse-whitespace
      (with-out-str
        (is= 5 (i/spyx (+ 2 3))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace   "(inc 0) => 1
                                  (inc 1) => 2
                                  (inc 2) => 3 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 3 (i/spyx (inc 0)
                 (inc 1)
                 (inc 2))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace   ":some-kw
                                  (inc 1) => 2
                                  (inc 2) => 3 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 3    (i/spyx :some-kw
                    (inc 1)
                    (inc 2)))))) )

; #todo blog about this nested (is= ...) testing technique
(dotest
  (is=
    (ts/collapse-whitespace  " a => 1
                               b => 5
                               (-> (inc a) (* 2) inc) => 5 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 13
          (i/let-spy [a (inc 0)
                      b (+ 2 3)]
                   (i/spyx (-> (inc a) (* 2) inc))
            (-> b (* 2) (+ 3)))))))

  (is= (ts/collapse-whitespace  " a => 1
                                  b => 5 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 17
          (i/let-spy [a (inc 0)
                    b (+ 2 3)]
                   (-> b (* (inc a)) (+ 7))))))))


(dotest
  (testing "basic usage"
    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result)) ]
      (is= ":hi => 5"
        (ts/collapse-whitespace (with-out-str (i/spy (side-effect-add! 2 3) :hi))) )
      (is= ":hi => 5"
        (ts/collapse-whitespace (with-out-str (i/spy :hi  (side-effect-add! 2 3)))) )
      (is= ":after-inc => 2"
        (ts/collapse-whitespace (with-out-str (-> 1
                                                (inc)
                                                (i/spy :after-inc) ; add a custom keyword message
                                                (* 2)))))
      (is= ":after-inc => 2"
        (ts/collapse-whitespace (with-out-str (->> 1
                                                (inc)
                                                (i/spy :after-inc) ; add a custom keyword message
                                                (* 2)))))

      (is= "(side-effect-add! 2 3) => 5"
        (ts/collapse-whitespace (with-out-str (i/spyx (side-effect-add! 2 3)))) )
      (is= 15 @side-effect-cum-sum))

    (is= ":first => 5 :second => 25"
      (ts/collapse-whitespace
        (with-out-str (-> 2
                        (+ 3)
                        (i/spy :first )
                        (* 5)
                        (i/spy :second) ))))
    (is= ":first => 5 :second => 25"
      (ts/collapse-whitespace
        (with-out-str (->> 2
                        (+ 3)
                        (i/spy :first )
                        (* 5)
                        (i/spy :second) ))))

    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result))
          ]
      (is= ":value => 5"
        (ts/collapse-whitespace (with-out-str (i/spy (side-effect-add! 2 3) :value))))
      (is= ":value => 5"
        (ts/collapse-whitespace (with-out-str (i/spy :value  (side-effect-add! 2 3)))))
      (is= 10 @side-effect-cum-sum)

      (is= ":value => 5" (ts/collapse-whitespace (with-out-str (i/spy :value (+ 2 3) ))))
      (is=   ":spy => 5" (ts/collapse-whitespace (with-out-str (i/spy        (+ 2 3) ))))

      (is= "(str \"abc\" \"def\") => \"abcdef\""
        (ts/collapse-whitespace (with-out-str (i/spyx (str "abc" "def") ))))

      (throws? (i/spy :some-tag "some-str" 42)) )))

(dotest
  (let [fn2   (fn []  (i/with-spy-indent
                        (i/spy :msg2 (+ 2 3))))
        fn1   (fn []  (i/with-spy-indent
                        (i/spy :msg1 (+ 2 3))
                        (fn2)))
        fn0   (fn [] (i/spy :msg0 (+ 2 3))) ]
    (is= ":msg2 => 5"            (ts/collapse-whitespace (with-out-str (fn2))))
    (is= ":msg1 => 5 :msg2 => 5" (ts/collapse-whitespace (with-out-str (fn1))))
    (is= ":msg0 => 5"            (ts/collapse-whitespace (with-out-str (fn0)))) ))


(dotest
  (is= 3 (i/let-some [a 1
                      b 2
                      c (+ a b)]
           c))
  (is= nil (i/let-some [a 1
                        b nil
                        c 3]
             [a b c]))

  (is= 5 (i/let-some [a (+ 2 3)]
           a))
  (is= 7 (i/let-some [a (+ 2 3)
                      b (inc a)
                      c (inc b)]
           c))
  (is= nil (i/let-some [a (+ 2 3)
                        b nil
                        c (inc b)]
             c))
  (is= nil (i/let-some [a (+ 2 3)
                        b (when (< 5 0) a)
                        c (inc b)]
             c))
  (is= [0 [1 2 3 4]] (i/let-some [tgt 5
                                  [x & others] (range tgt)]
                       [x others]))
  (is= nil (i/let-some [tgt nil
                        [x & others] (range tgt)]
             [x others])))

(dotest
  (testing "vecs"
    (let [coll (range 3)]
      (isnt (i/contains-elem? coll -1))
      (is   (i/contains-elem? coll  0))
      (is   (i/contains-elem? coll  1))
      (is   (i/contains-elem? coll  2))
      (isnt (i/contains-elem? coll  3))
      (isnt (i/contains-elem? coll  nil)))

    (let [coll [ 1 :two "three" \4]]
      (isnt (i/contains-elem? coll  :no-way))
      (isnt (i/contains-elem? coll  nil))
      (is   (i/contains-elem? coll  1))
      (is   (i/contains-elem? coll  :two))
      (is   (i/contains-elem? coll  "three"))
      (is   (i/contains-elem? coll  \4)))

    (let [coll [:yes nil 3]]
      (isnt (i/contains-elem? coll  :no-way))
      (is   (i/contains-elem? coll  :yes))
      (is   (i/contains-elem? coll  nil))))

  (testing "maps"
    (let [coll {1 :two "three" \4}]
      (isnt (i/contains-elem? coll nil ))
      (isnt (i/contains-elem? coll [1 :no-way] ))
      (is   (i/contains-elem? coll [1 :two]))
      (is   (i/contains-elem? coll ["three" \4])))
    (let [coll {1 nil "three" \4}]
      (isnt (i/contains-elem? coll [nil 1] ))
      (is   (i/contains-elem? coll [1 nil] )))
    (let [coll {nil 2 "three" \4}]
      (isnt (i/contains-elem? coll [1 nil] ))
      (is   (i/contains-elem? coll [nil 2] ))))

  (testing "sets"
    (let [coll #{1 :two "three" \4}]
      (isnt (i/contains-elem? coll  :no-way))
      (is   (i/contains-elem? coll  1))
      (is   (i/contains-elem? coll  :two))
      (is   (i/contains-elem? coll  "three"))
      (is   (i/contains-elem? coll  \4)))

    (let [coll #{:yes nil}]
      (isnt (i/contains-elem? coll  :no-way))
      (is   (i/contains-elem? coll  :yes))
      (is   (i/contains-elem? coll  nil)))))

(dotest
  (is   (i/contains-key?  {:a 1 :b 2} :a))
  (is   (i/contains-key?  {:a 1 :b 2} :b))
  (isnt (i/contains-key?  {:a 1 :b 2} :x))
  (isnt (i/contains-key?  {:a 1 :b 2} :c))
  (isnt (i/contains-key?  {:a 1 :b 2}  1))
  (isnt (i/contains-key?  {:a 1 :b 2}  2))

  (is   (i/contains-key?  {:a 1 nil   2} nil))
  (isnt (i/contains-key?  {:a 1 :b  nil} nil))
  (isnt (i/contains-key?  {:a 1 :b    2} nil))

  (is   (i/contains-key? #{:a 1 :b 2} :a))
  (is   (i/contains-key? #{:a 1 :b 2} :b))
  (is   (i/contains-key? #{:a 1 :b 2}  1))
  (is   (i/contains-key? #{:a 1 :b 2}  2))
  (isnt (i/contains-key? #{:a 1 :b 2} :x))
  (isnt (i/contains-key? #{:a 1 :b 2} :c))

  (is   (i/contains-key? #{:a 5 nil   "hello"} nil))
  (isnt (i/contains-key? #{:a 5 :doh! "hello"} nil))

  (throws? (i/contains-key? [:a 1 :b 2] :a))
  (throws? (i/contains-key? [:a 1 :b 2]  1)))

(dotest
  (is   (i/contains-val? {:a 1 :b 2} 1))
  (is   (i/contains-val? {:a 1 :b 2} 2))
  (isnt (i/contains-val? {:a 1 :b 2} 0))
  (isnt (i/contains-val? {:a 1 :b 2} 3))
  (isnt (i/contains-val? {:a 1 :b 2} :a))
  (isnt (i/contains-val? {:a 1 :b 2} :b))

  (is   (i/contains-val? {:a 1 :b nil} nil))
  (isnt (i/contains-val? {:a 1 nil  2} nil))
  (isnt (i/contains-val? {:a 1 :b   2} nil))

  (throws? (i/contains-val?  [:a 1 :b 2] 1))
  (throws? (i/contains-val? #{:a 1 :b 2} 1)))

(dotest
  (is= (i/forv [x (range 4)] (* x x))
    [0 1 4 9] )
  (is= (i/forv [x (range 23)] (* x x))
    (for  [x (range 23)] (* x x)))
  (is= (i/forv [x (range 5)  y (range 2 9)] (str x y))
    (for  [x (range 5)  y (range 2 9)] (str x y))))

(dotest
  (let [xs [1 2 3]
        ys [10 20 30]]
    (is= [11 22 33]
      (i/map-let [x xs y ys] (+ x y))
      (i/map-let* {:lazy false :strict false} [x xs y ys] (+ x y))
      (i/map-let* {:lazy false :strict true}  [x xs y ys] (+ x y))
      (i/map-let* {:lazy true :strict false}  [x xs y ys] (+ x y))
      (i/map-let* {:lazy true :strict true}   [x xs y ys] (+ x y)))
    (let [result-vec (i/map-let* {:lazy false :strict true} [x xs y ys] (+ x y))
          result-lazyseq (i/map-let* {:lazy true :strict true} [result-vec xs y ys] (+ result-vec y))]
      (i/spyx (type result-vec))
      (i/spyx (type result-lazyseq))

      (is (instance?
            #?(:clj clojure.lang.PersistentVector)
            #?(:cljs cljs.core/PersistentVector)
            result-vec))
      (is (instance?
            #?(:clj clojure.lang.LazySeq)
            #?(:cljs cljs.core/LazySeq)
            result-lazyseq)))
      )

  (let [xs [1 2 3]
        ys [10 20 30 40]]
    (throws?        (i/map-let                  [x xs y ys] (+ x y)))
    (throws?        (i/map-let* {:strict true}  [x xs y ys] (+ x y)))
    (is= [11 22 33] (i/map-let* {:strict false} [x xs y ys] (+ x y)))))

(dotest
  (is= (vector [])               [[]] )
  (is= (mapv identity [] [])      []  )

  (is= [[:a 0] [:b 1] [:c 2]]
    (i/zip-lazy [:a :b :c] [0 1 2])
    (i/zip-lazy [:a :b :c] (range)))
  (is= (i/zip-lazy [:a :b :c] [1 2 3])   [[:a 1] [:b 2] [:c 3]] )
  (is= (i/zip-lazy [:a] [1])             [[:a 1]] )
  (is= (i/zip-lazy [] [])                []  )
  (is= (i/zip-lazy [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]] )
  (is (instance?
        #?(:clj clojure.lang.LazySeq)
        #?(:cljs cljs.core/LazySeq)
        (i/zip-lazy [:a :b :c] (range))))

  (is= (i/zip [:a :b :c] [1 2 3])   [[:a 1] [:b 2] [:c 3]] )   ; #todo fails when use Schema for append/prepend
  (is= (i/zip [:a] [1])             [[:a 1]] )                 ; #todo fails when use Schema for append/prepend
  (is= (i/zip [] [])                []  )
  (is= (i/zip [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]] )
  (throws? (i/zip [:a :b :c] [1 2 3 4]))
  (is= (i/zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]] )

  (is (instance?
        #?(:clj clojure.lang.PersistentVector)
        #?(:cljs cljs.core/PersistentVector)
        (i/zip*  {:trunc false} [:a :b :c] [1 2 3])))
  (let [keys   [:a :b :c]
        vals   [1 2 3]
        result (atom [])]
    (doseq [[k i] (i/zip keys vals)]
      (swap! result i/append {k i}))
    (is= [{:a 1} {:b 2} {:c 3}] @result))

  ; verify that zip throws if unequal lengths, even if some colls are infinite lazy seqs
  (throws? (i/zip            [:a :b :c] [1 2 3 4]))
  (throws? (i/zip [:A :B :C] [:a :b :c] [1 2 3 4]))
  (throws? (i/zip [:a :b :c] (range)))
  (is= (i/zip* {:strict false} [:a :b :c] (range))   [[:a 0] [:b 1] [:c 2]] )
  (is= (i/zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]] )
  (is= (i/zip* {:strict false} [:A :B :C] [:a :b :c] [1 2 3 4]) [[:A :a 1] [:B :b 2] [:C :c 3]] ))

(dotest
  (is= (i/indexed [:a :b :c]) [[0 :a] [1 :b] [2 :c]])
  (is= [[0 0] [1 2] [2 4] [3 6] [4 8]]
    (take 5 (i/indexed (map #(* 2 %) (range))))) ; can work with infinite lazy lists
  (is= (i/indexed [:a :b :c]  (map #(+ 10 %) (range)))
    [ [0 :a 10]
     [1 :b 11]
     [2 :c 12] ] )
  (is= (take 5 (i/indexed (map #(+ 10 %) (range))))
    [ [0 10]
     [1 11]
     [2 12]
     [3 13]
     [4 14] ] ))

(dotest
  ; unexpected results
  (is (= (concat {:a 1} {:b 2} {:c 3} )
        [ [:a 1] [:b 2] [:c 3] ] ))
  (is (= (conj [1 2] [3 4])
        [1 2  [3 4] ] ))

  (let [objs   [ [] '()   {} (sorted-map)   #{} (sorted-set) ] ]
    (is= (map sequential? objs) [true  true    false false   false false] )
    (is= (map map?        objs) [false false   true  true    false false] )
    (is= (map set?        objs) [false false   false false   true  true ] ))

  (is= (i/glue [1 2]  [3 4] [5 6])        [1 2 3 4 5 6])
  (is= (i/glue [1 2] '(3 4) [5 6])        [1 2 3 4 5 6])
  (is= (i/glue [] [1 2] )                [1 2] )
  (is= (i/glue [1 2] [] )                [1 2] )
  (is= (i/glue [] [1 2] [] )             [1 2] )

  (is= (i/glue '(1 2) '(3 4) '(5 6))        [1 2 3 4 5 6])
  (is= (i/glue '(1 2)  [3 4] '(5 6))        [1 2 3 4 5 6])
  (is= (i/glue  [1 2] '(3 4) '(5 6))        [1 2 3 4 5 6])
  (is= (i/glue '() '(1 2) )                 [1 2] )
  (is= (i/glue '(1 2) '() )                 [1 2] )
  (is= (i/glue '() '(1 2) '() )             [1 2] )

  (is= (i/glue (range 3) (range 5))      [0 1 2 0 1 2 3 4] )

  (is= (i/glue {:a 1} {:b 2} {:c 3})      {:a 1 :c 3 :b 2})
  (is= (i/glue {:a 1} {:b 2} )            {:a 1 :b 2})
  (is= (i/glue {:a 1} {} )                {:a 1} )
  (is= (i/glue {} {:a 1} )                {:a 1} )
  (is= (i/glue {} {:a 1} {} )             {:a 1} )

  (is= (i/glue #{1 2} #{3 4} #{6 5})     #{1 2 6 5 3 4})
  (is= (i/glue #{} #{1 2} )              #{1 2} )
  (is= (i/glue #{1 2} #{} )              #{1 2} )
  (is= (i/glue #{} #{1 2} #{} )          #{1 2} )

  (is= (i/glue (sorted-map) {:a 1} {:b 2} {:c 3})   {:a 1 :b 2 :c 3} )
  (is= (i/glue (sorted-set) #{1 2} #{3 4} #{6 5})   #{1 2 3 4 5 6})

  (is=      (i/glue (sorted-map) (hash-map :a 1   :b 2   :c 3   :d 4   :e 5   :f 6))
    {:a 1   :b 2   :c 3   :d 4   :e 5   :f 6} )
  (is= (seq (i/glue (sorted-map) (hash-map :a 1   :b 2   :c 3   :d 4   :e 5   :f 6)))
    [ [:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] ] )

  (is= (i/glue  {:band :VanHalen :singer :Dave} {:singer :Sammy} )
                {:band :VanHalen                 :singer :Sammy} )

  (is= (i/glue \a )           "a" )
  (is= (i/glue "a")           "a" )
  (is= (i/glue \a "")         "a" )
  (is= (i/glue "" "a")        "a" )
  (is= (i/glue \a  \b)        "ab" )
  (is= (i/glue "a" "b")       "ab" )
  (is= (i/glue "a" \b)        "ab" )
  (is= (i/glue \a  "b")       "ab" )
  (is= (i/glue "" "a" \b)     "ab" )
  (is= (i/glue "" \a  "b")    "ab" )
  (is= (i/glue "a" "" \b)     "ab" )
  (is= (i/glue \a  "" "b")    "ab" )
  (is= (i/glue "a" \b  "")    "ab" )
  (is= (i/glue \a  "b" "")    "ab" )
  (is= (i/glue \a  "b" "")    "ab" )
  (is= (i/glue "I" \space "like " \a " nap!" )    "I like a nap!" )
  (is= (apply i/glue [ "I" \space "like " \a " nap!"] )    "I like a nap!" )

  (throws? (i/glue   [1 2]     {:a 1} ))
  (throws? (i/glue  '(1 2)     {:a 1} ))
  (throws? (i/glue   [1 2]    #{:a 1} ))
  (throws? (i/glue  '(1 2)    #{:a 1} ))
  (throws? (i/glue   [1 2]    "hello" ))
  (throws? (i/glue  '(1 2)    "hello" ))
  (throws? (i/glue   {:a 1}   #{:a 1} ))
  (throws? (i/glue   {:a 1}   "hello" ))
  (throws? (i/glue   #{:a 1}  "hello" ))
  (throws? (i/glue   [1 2]     nil    ))
  (throws? (i/glue  '(1 2)     nil    ))
  (throws? (i/glue   {:a 1}    nil    ))
  (throws? (i/glue   #{:a 1}   nil    ))
  (throws? (i/glue   "hello"   nil    )) )

(dotest
  (let [data [[0 1 2]
              []
              [3]
              [4 5]
              [6 7 8 9]]]
    (is= (i/thru 9) (i/glue-rows data))
    (is= (i/thru 9) (reduce into [] data))))

(dotest
  (throws?            (i/append  1 2        ))
  (throws?            (i/append [1 2]       ))
  (throws?            (i/append nil   3     ))
  (is= [1 2 3    ]    (i/append [1 2] 3     ))
  (is= [1 2 3 4  ]    (i/append [1 2] 3 4   ))
  (is= [1 2 3 4 5]    (i/append [1 2] 3 4 5 ))

  (throws?            (i/append '(1 2)       ))
  (is= [1 2 3    ]    (i/append '(1 2) 3     ))
  (is= [1 2 3 4  ]    (i/append '(1 2) 3 4   ))
  (is= [1 2 3 4 5]    (i/append '(1 2) 3 4 5 ))

  (throws?            (i/append   {:a 1} 99     ))
  (throws?            (i/append   {:a 1} {:b 2} ))
  (throws?            (i/append  #{:a 1} 99     ))
  (throws?            (i/append  #{:a 1} #{99}  ))

  (testing "old conjv tests"
    (is= [  2  ]    (i/append  []  2   ))
    (is= [  2  ]    (i/append '()  2   ))
    (is= [  2 3]    (i/append  []  2  3))
    (is= [  2 3]    (i/append '()  2  3))

    (is= [1 2 3]    (i/append  [1] 2  3))
    (is= [1 2 3]    (i/append '(1) 2  3))
    (is= [1 2 3]    (i/append  [1  2] 3))
    (is= [1 2 3]    (i/append '(1  2) 3))

    (is= [1 2 3 4]  (i/append  [1  2] 3 4))
    (is= [1 2 3 4]  (i/append '(1  2) 3 4))
    (is= [1 2 3 4]  (i/append  [1] 2  3 4))
    (is= [1 2 3 4]  (i/append '(1) 2  3 4))

    (is= [[1 2] [3 4] [5 6]] (i/append  [[1 2] [3 4]]  [5 6] ))

    (is= [0 1 2 3 4 5] (i/append (range 4) 4 5))
    (is= [0 1 2 3 4 5] (apply i/append [0] (range 1 6)))))

(dotest
  (throws?            (i/prepend       [2 1] ))
  (throws?            (i/prepend     3  nil  ))
  (is= [    3 2 1]    (i/prepend     3 [2 1] ))
  (is= [  4 3 2 1]    (i/prepend   4 3 [2 1] ))
  (is= [5 4 3 2 1]    (i/prepend 5 4 3 [2 1] ))

  (throws?            (i/prepend       '(2 1) ))
  (is= [    3 2 1]    (i/prepend     3 '(2 1) ))
  (is= [  4 3 2 1]    (i/prepend   4 3 '(2 1) ))
  (is= [5 4 3 2 1]    (i/prepend 5 4 3 '(2 1) ))

  (throws?            (i/prepend   99     {:a 1} ))
  (throws?            (i/prepend   {:b 2} {:a 1} ))
  (throws?            (i/prepend   99    #{:a 1} ))
  (throws?            (i/prepend  #{99}  #{:a 1} )))

(dotest
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 2 3 4 5 6 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 (i/unwrap [2 3 4 5 6 7 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 (i/unwrap [2 (i/unwrap [3 4 5 6 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 (i/unwrap [2 (i/unwrap [3 (i/unwrap [4 5 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 (i/unwrap [2 (i/unwrap [3 (i/unwrap [4 (i/unwrap [5]) 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 2 3 (i/unwrap [4 5 6]) 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (i/->vector 1 (i/unwrap [2 3 (i/unwrap [4 5 6]) 7 8]) 9))

  (is= [1 2 3 [4  5  6] 7 8 9] (i/->vector 1 (i/unwrap [2 3 [4  5  6] 7 8]) 9))
  (is= [1 2 3 [4 [5] 6] 7 8 9] (i/->vector 1 (i/unwrap [2 3 [4 [5] 6] 7 8]) 9))

  (is= [1 [2 3 4 [5] 6 7 8] 9] (i/->vector 1 `(2 3 ~(i/unwrap [4 [5] 6]) 7 8) 9))
  (is= [1 [2 3 4 [5] 6 7 8] 9] (i/->vector 1  [2 3  (i/unwrap [4 [5] 6]) 7 8] 9))

  (is= [1 2 3 4 5 6 7 8 9] (i/glue   [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat   [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (i/glue   [1   2   3] [4 5 6] [7   8   9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat   [1   2   3] [4 5 6] [7   8   9])))

(dotest
  (isnt (i/increasing? [1 2] [1]))
  (isnt (i/increasing? [1 2] [1 1]))
  (isnt (i/increasing? [1 2] [1 2]))
  (is (i/increasing? [1 2] [1 2 nil]))
  (is (i/increasing? [1 2] [1 2 3]))
  (is (i/increasing? [1 2] [1 3]))
  (is (i/increasing? [1 2] [2 1]))
  (is (i/increasing? [1 2] [2]))

  (isnt (i/increasing-or-equal? [1 2] [1]))
  (isnt (i/increasing-or-equal? [1 2] [1 1]))
  (is (i/increasing-or-equal? [1 2] [1 2]))
  (is (i/increasing-or-equal? [1 2] [1 2 nil]))
  (is (i/increasing-or-equal? [1 2] [1 2 3]))
  (is (i/increasing-or-equal? [1 2] [1 3]))
  (is (i/increasing-or-equal? [1 2] [2 1]))
  (is (i/increasing-or-equal? [1 2] [2])))

(dotest
  (let [map1  { :a 1 :b 2 :c nil
               nil :NIL
               "hi" "hello"
               5 "five"}]
    (is= 1           (i/grab :a   map1))
    (is= 2           (i/grab :b   map1))
    (is= nil         (i/grab :c   map1))
    (is= :NIL        (i/grab nil  map1))
    (is= "hello"     (i/grab "hi"  map1))
    (is= "five"      (i/grab 5  map1))
    (throws?  (i/grab :z map1))
    (throws?  (i/grab 42 map1))
    ))

(dotest
  (testing "basic usage"
    (let [map1  {:a1 "a1"
                 :a2 { :b1 "b1"
                      :b2 { :c1 "c1"
                           :c2 "c2" }}
                 nil "NIL"
                 :nil nil} ]
      (is= "a1"  (i/fetch-in map1 [:a1]))
      (is= "b1"  (i/fetch-in map1 [:a2 :b1]))
      (is= "c1"  (i/fetch-in map1 [:a2 :b2 :c1]))
      (is= "c2"  (i/fetch-in map1 [:a2 :b2 :c2]))
      (is= "NIL" (i/fetch-in map1 [nil]))
      (is= nil   (i/fetch-in map1 [:nil]))
      (throws?   (i/fetch-in map1 [:a9]))
      (throws?   (i/fetch-in map1 [:a2 :b9]))
      (throws?   (i/fetch-in map1 [:a2 :b2 :c9])))))

(dotest
  (let [mm    {:a { :b { :c "c" }}} ]
    (is= (i/dissoc-in mm []         )          mm )
    (is= (i/dissoc-in mm [:a]       )          {} )
    (is= (i/dissoc-in mm [:a :b]    )          {:a  {}} )
    (is= (i/dissoc-in mm [:a :b :c] )          {:a  { :b  {}}} )
    (is= (i/dissoc-in mm [:a :x :y] )          {:a  { :b  { :c "c" }
                                                   :x  nil }} )
    (is= (i/dissoc-in mm [:a :x :y :z] )       {:a  { :b  { :c "c" }
                                                   :x  { :y nil }}} )
    (is= (i/dissoc-in mm [:k1 :k2 :k3 :kz] )   {:a  { :b  { :c  "c" }}
                                              :k1 { :k2 { :k3 nil }}} ))
  (let [mm    {:a1 "a1"
               :a2 { :b1 "b1"
                    :b2 { :c1 "c1"
                         :c2 "c2" }}} ]
    (is= (i/dissoc-in mm [:a1] )
      {:a2 { :b1 "b1"
            :b2 { :c1 "c1"
                 :c2 "c2" }}} )
    (is= (i/dissoc-in mm [:a2] )
      {:a1 "a1" } )
    (is= (i/dissoc-in mm [:a2 :b1] )
      {:a1 "a1"
       :a2 { :b2 { :c1 "c1"
                  :c2 "c2" }}} )
    (is= (i/dissoc-in mm [:a2 :b2] )
      {:a1 "a1"
       :a2 { :b1 "b1" }} )
    (is= (i/dissoc-in mm [:a2 :b2 :c1] )
      {:a1 "a1"
       :a2 { :b1 "b1"
            :b2 { :c2 "c2" }}} )
    (is= (i/dissoc-in mm [:a2 :b2 :c2] )
      {:a1 "a1"
       :a2 { :b1 "b1"
            :b2 { :c1 "c1" }}} )))

(dotest
  (println :awt100 "*****************************************************************************")
  (let [x (i/with-exception-default :XXX (throw (ex-info "some-big-error" nil)) )]
    (println :awt110 x) )
  (println :awt100 "*****************************************************************************")
  )

(dotest
  (let [map1  {:a 1 :b 2 :c 3 :d 4 :e 5}]
    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (i/submap-by-keys map1 #{ :a :b :c :d :e } ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (i/submap-by-keys map1 #{    :b :c :d :e } ))
    (is= {          :c 3 :d 4 :e 5} (i/submap-by-keys map1 #{       :c :d :e } ))
    (is= {               :d 4 :e 5} (i/submap-by-keys map1 #{          :d :e } ))
    (is= {                    :e 5} (i/submap-by-keys map1 #{             :e } ))
    (is= {                        } (i/submap-by-keys map1 #{                } ))
    (throws? (i/submap-by-keys map1 #{:z}))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (i/submap-by-keys map1 #{ :a :b :c :d :e  :z } :missing-ok))
    (is= {     :b 2 :c 3 :d 4 :e 5} (i/submap-by-keys map1 #{    :b :c :d :e  :z } :missing-ok))
    (is= {          :c 3 :d 4 :e 5} (i/submap-by-keys map1 #{       :c :d :e  :z } :missing-ok))
    (is= {               :d 4 :e 5} (i/submap-by-keys map1 #{          :d :e  :z } :missing-ok))
    (is= {                    :e 5} (i/submap-by-keys map1 #{             :e  :z } :missing-ok))
    (is= {                        } (i/submap-by-keys map1 #{                 :z } :missing-ok))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (i/submap-by-vals map1 #{ 1 2 3 4 5 } ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (i/submap-by-vals map1 #{   2 3 4 5 } ))
    (is= {          :c 3 :d 4 :e 5} (i/submap-by-vals map1 #{     3 4 5 } ))
    (is= {               :d 4 :e 5} (i/submap-by-vals map1 #{       4 5 } ))
    (is= {                    :e 5} (i/submap-by-vals map1 #{         5 } ))
    (is= {                        } (i/submap-by-vals map1 #{           } ))
    (throws? (i/submap-by-vals map1 #{ 99 }))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (i/submap-by-vals map1 #{ 1 2 3 4 5  99 } :missing-ok ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (i/submap-by-vals map1 #{   2 3 4 5  99 } :missing-ok ))
    (is= {          :c 3 :d 4 :e 5} (i/submap-by-vals map1 #{     3 4 5  99 } :missing-ok ))
    (is= {               :d 4 :e 5} (i/submap-by-vals map1 #{       4 5  99 } :missing-ok ))
    (is= {                    :e 5} (i/submap-by-vals map1 #{         5  99 } :missing-ok ))
    (is= {                        } (i/submap-by-vals map1 #{            99 } :missing-ok ))
    (is= {                        } (i/submap-by-vals map1 #{               } :missing-ok ))

    (is= { 0 :even 2 :even } (i/submap-by-vals
                               { 0 :even 1 :odd 2 :even 3 :odd }
                                #{ :even } ))
    (is= { 0 :even 2 :even } (i/submap-by-vals
                               { 0 :even 1 :odd 2 :even 3 :odd }
                                #{ :even :prime } :missing-ok ))) )

(dotest             ; -1 implies "in order"
  ; empty list is smaller than any non-empty list
  (is (neg? (i/lexical-compare [] [2])))
  (is (neg? (i/lexical-compare [] [\b])))
  (is (neg? (i/lexical-compare [] ["b"])))
  (is (neg? (i/lexical-compare [] [:b])))
  (is (neg? (i/lexical-compare [] ['b])))

  ; nil is smaller than any non-nil item
  (is (neg? (i/lexical-compare [nil] [2])))
  (is (neg? (i/lexical-compare [nil] [\b])))
  (is (neg? (i/lexical-compare [nil] ["b"])))
  (is (neg? (i/lexical-compare [nil] [:b])))
  (is (neg? (i/lexical-compare [nil] ['b])))

  ; Cannot compare items from different classes:  number, char, string, keyword, symbol
  (throws? (i/lexical-compare [1] [\b]))
  (throws? (i/lexical-compare [1] ["b"]))
  (throws? (i/lexical-compare [1] [:b]))
  (throws? (i/lexical-compare [1] ['b]))
  (throws? (i/lexical-compare [\b] ["b"]))
  (throws? (i/lexical-compare [\b] [:b]))
  (throws? (i/lexical-compare [\b] ['b]))
  (throws? (i/lexical-compare ["b"] [:b]))
  (throws? (i/lexical-compare ["b"] ['b]))
  (throws? (i/lexical-compare [:b] ['b]))

  ; different positions in list can be of different class
  (is (neg? (i/lexical-compare [:a] [:b])))
  (is (neg? (i/lexical-compare [:a] [:a 1])))
  (is (neg? (i/lexical-compare [1 :a] [2])))
  (is (neg? (i/lexical-compare [:a] [:a 1])))
  (is (neg? (i/lexical-compare [1] [1 :a])))
  (is (neg? (i/lexical-compare [1 :a] [2])))

  ; same position in list can be of different class if sorted by previous positions
  (is (neg? (i/lexical-compare [1 :z] [2 9]))) ; OK since prefix lists [1] & [2] define order
  (throws?  (i/lexical-compare [1 :z] [1 2])) ; not OK since have same prefix list: [1]

  (is= (vec (sorted-set-by i/lexical-compare [1 :a] [1] [2]))
    [[1] [1 :a] [2]])
  (is= (vec (sorted-set-by i/lexical-compare [2 0] [2] [3] [3 :y] [1] [1 :a] [1 :b] [1 :b 3]))
    [[1]
     [1 :a]
     [1 :b]
     [1 :b 3]
     [2]
     [2 0]
     [3]
     [3 :y]]))

(dotest
  (is= 3 (i/validate pos? 3))
  (is= 3.14 (i/validate number? 3.14))
  (is= 3.14 (i/validate #(< 3 % 4) 3.14))
  (is= [0 1 2] (i/validate vector? (vec (range 3))))
  (is= nil (i/validate nil? (next [])))
  (is= [0 1 2] (i/validate #(= 3 (count %)) [0 1 2]))
  (throws? (i/validate number? "hello"))
  (throws? (i/validate i/truthy? nil)) )

(dotest
  (throws? (i/verify (= 1 2)))
  (is= 333 (i/verify (* 3 111))))

(dotest
  (let [m1 {:a 1 :b 2 :c 3}
        m2 {:a 1 :b 2 :c [3 4]}]
    (is= m1 (apply hash-map (i/keyvals m1)))
    (is= m2 (apply hash-map (i/keyvals m2)))))
; AWTAWT TODO: add test.check

(dotest
  (let [m1 {:a 1 :b 2 :c 3} ]
    (is= [ :a 1 :b 2      ] (i/keyvals-seq m1 [:a :b]))
    (is= [ :b 2 :a 1      ] (i/keyvals-seq m1 [:b :a]))
    (is= [ :a 1 :b 2 :c 3 ] (i/keyvals-seq m1 [:a :b :c]))
    (is= [ :c 3 :a 1 :b 2 ] (i/keyvals-seq m1 [:c :a :b]))
    (is= [ :c 3 :b 2 :a 1 ] (i/keyvals-seq m1 [:c :b :a]))
    (is= [ :a 1 :b 2 :a 1 ] (i/keyvals-seq m1 [:a :b :a]))

    (throws? (i/keyvals-seq m1 [:a :b :z]))
    (is= [:a 1 :b 2] (i/keyvals-seq {:missing-ok true
                                     :the-map    m1 :the-keys [:a :b :z]}))
    (is= [:b 2 :c 3] (i/keyvals-seq {:missing-ok true
                                     :the-map    m1 :the-keys [:z :b :c]})) ))

(dotest
  (is= 2 (i/it-> 1
           (inc it)
           (+ 3 it)
           (/ 10 it)))
  (let [mm  {:a {:b 2}}]
    (is= (i/it-> mm (:a it)          )  {:b 2} )
    (is= (i/it-> mm (it :a)  (:b it) )      2  ))
  (is= 48 (i/it-> 42
            (let [x 5]
              (+ x it))
            (inc it))))

(dotest
  (let [params {:a 1 :b 1 :c nil :d nil}]
    (is= (i/cond-it-> params
           (:a it)              (update it :b inc)
           (= (:b it) 2)        (assoc it :c "here")
           (= "here" (:c it))   (assoc it :d "again"))
      {:a 1, :b 2, :c "here", :d "again"}))

  (let [params {:a nil :b 1 :c nil :d nil}]
    (is= (i/cond-it-> params
           (:a it)                (update it :b inc)
           (= (:b it) 1)          (assoc it :c "here")
           (= "here" (:c it))     (assoc it :d "again"))
      {:a nil, :b 1, :c "here", :d "again"}))

  (let [params {:a 1 :b 1 :c nil :d nil}]
    (is= (i/cond-it-> params
           (:a it)        (update it :b inc)
           (= (:b it) 2)  (update it :b inc)
           (:c it)        (assoc it :d "again"))
      {:a 1, :b 3, :c nil :d nil})) )

(dotest
  (is= 8 (i/some-it-> 1
           (inc it)
           (* it 3)
           (+ 2 it)))
  (is (nil? (i/some-it-> nil
              (inc it)
              (* it 3)
              (+ 2 it))))
  (is (nil? (i/some-it-> 1 (inc it)
              (when false (* it 3))
              (+ 2 it)))) )

(dotest
  (throws? (/ 1 0))
  (is= nil (i/with-exception-default nil (/ 1 0)))
  (is= :dummy (i/with-exception-default :dummy (/ 1 0)))
  (is= 123 (i/with-exception-default 0 (Long/parseLong "123")))
  (is= 0 (i/with-exception-default 0 (Long/parseLong "12xy3"))))

(dotest
  (is= (i/validate-or-default i/not-nil? nil 0) 0)
  (is= (i/validate-or-default i/not-empty? "" "How you doin?") "How you doin?")
  (is= (mapv #(i/with-nil-default :some-default %)
         [0 1 "" [] nil           true false])
    [0 1 "" [] :some-default true false]))



















