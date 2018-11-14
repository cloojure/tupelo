(ns tst.tupelo.core
  (:require
    [clojure.string :as str]
    #?@(:clj [
              [schema.core :as s]
              [tupelo.test   :refer [define-fixture dotest is isnt is= isnt= nonblank= testing throws?]]
              [tupelo.core :as t]
              [tupelo.schema :as tsk]
              [tupelo.string :as ts]
             ])
    #?@(:cljs [
               [schema.core :as s]
               [tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= nonblank= testing throws?]]
               [tupelo.core :as t :refer [spyx spyxx] :include-macros true]
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

  (is (t/truthy? true))
  (is (t/truthy? 5))
  (is (t/falsey? false))
  (is (t/falsey? nil)))

(dotest
  (let [inf-rng-1 (map inc (range))]
    (is= 42 (t/only [42]))
    (is= :x (t/only [:x]))
    (is= "hello" (t/only ["hello"]))

    ; #todo #wip
    (throws? (t/only []))
    (throws? (t/only [:x :y]))
    (throws? (t/only inf-rng-1))

    (is= [1 2 3] (t/onlies [[1] [2] [3]]))
    (throws? (t/onlies [[1] [2] [3 4]]))
    (throws? (t/onlies [[1] [] [3]]))

    (is= 5 (t/only2 [[5]]))
    (throws? (t/only2 [[1 2]]))
    (throws? (t/only2 [[1] [2]]))

    (is (t/single? [42]))
    (is (t/single? [:x]))
    (is (t/single? ["hello"]))
    (isnt (t/single? []))
    (isnt (t/single? [:x :y]))
    (isnt (t/single? inf-rng-1))

    (is (t/pair? [42 43]))
    (is (t/pair? [:x :y]))
    (is (t/pair? ["hello" "there"]))
    (isnt (t/pair? []))
    (isnt (t/pair? [:y]))
    (isnt (t/pair? inf-rng-1))

    (is (t/triple? [42 43 44]))
    (is (t/triple? [:x :y :z]))
    (is (t/triple? ["hello" "there" "you"]))
    (isnt (t/triple? []))
    (isnt (t/triple? [:y]))
    (isnt (t/triple? [:x :y]))
    (isnt (t/triple? inf-rng-1))

    (is (t/quad? [42 43 44 45]))
    (is (t/quad? [:x :y :z :99]))
    (is (t/quad? ["hello" "there" "again" "you"]))
    (isnt (t/quad? []))
    (isnt (t/quad? [:x]))
    (isnt (t/quad? [:x :y]))
    (isnt (t/quad? [:x :y :z]))
    (isnt (t/quad? inf-rng-1))))

(dotest
  (let [inf-rng-1 (map inc (range))
        tst-map   (t/glue (sorted-map) {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})]

    (throws? (t/xtake 1 []))

    (is= [1] (t/xtake 1 [1]))
    (is= [1] (t/xtake 1 [1 2]))
    (is= [1] (t/xtake 1 inf-rng-1))
    (is= [1 2] (t/xtake 2 [1 2]))
    (is= [1 2] (t/xtake 2 inf-rng-1))
    (is= {:a 1} (t/xtake 1 tst-map))
    (is= {:a 1 :b 2} (t/xtake 2 tst-map))

    (throws? (t/xfirst []))
    (is= 1 (t/xfirst [1]))
    (is= 1 (t/xfirst [1 2]))
    (is= 1 (t/xfirst inf-rng-1))
    ;(is= {:a 1} (t/xfirst tst-map))

    (throws? (t/xsecond []))
    (throws? (t/xsecond [1]))
    (is= 2 (t/xsecond [1 2]))
    (is= 2 (t/xsecond [1 2 3]))
    (is= 2 (t/xsecond [1 2 3 4]))
    (is= 2 (t/xsecond inf-rng-1))
    ;(is= {:b 2} (t/xsecond tst-map))

    (throws? (t/xthird []))
    (throws? (t/xthird [1]))
    (throws? (t/xthird [1 2]))
    (is= 3 (t/xthird [1 2 3]))
    (is= 3 (t/xthird [1 2 3 4]))
    (is= 3 (t/xthird inf-rng-1))
    ;(is= {:b 92} (t/xthird tst-map))

    (throws? (t/xfourth []))
    (throws? (t/xfourth [1]))
    (throws? (t/xfourth [1 2]))
    (throws? (t/xfourth [1 2 3]))
    (is= 4 (t/xfourth [1 2 3 4]))
    (is= 4 (t/xfourth [1 2 3 4 5]))
    (is= 4 (t/xfourth inf-rng-1))
    ;(is= {:b 92} (t/xfourth tst-map))

    (throws? (t/xlast nil))
    (throws? (t/xlast []))
    (is= 5 (t/xlast [1 2 3 4 5]))
    ;(is= {:b 92} (t/xlast tst-map))

    (is= [1 2 3 4] (t/xbutlast [1 2 3 4 5]))
    (is= [] (t/xbutlast [1]))
    (throws? (t/xbutlast []))
    (throws? (t/xbutlast nil))
    ;(is= {:b 92} (t/xbutlast tst-map))

    (throws? (t/xrest []))
    (is= [] (t/xrest [1]))
    (is= [2] (t/xrest [1 2]))
    (is= [2 3] (t/xrest [1 2 3]))
    (is= [2 3 4] (t/xrest [1 2 3 4]))
    (is= 2 (first (t/xrest inf-rng-1)))

    (throws? (t/xvec nil))
    (is= [] (t/xvec []))
    (is= [1] (t/xvec '(1)))
    (is= [1 2] (t/xvec [1 2]))
    ))

(dotest
  (is= :23 (t/int->kw  23))
  (is=  23 (t/kw->int :23))

  (println :01 (t/edn->json {:a 1 :b 2}))
  (prn     :02 (t/edn->json {:a 1 :b 2}))

  (is=  {:a  1 :b  2}  (t/json->edn (ts/quotes->double "{'a':1, 'b':2}")))
  (is= "{'a':1,'b':2}" (ts/quotes->single (t/edn->json  {:a  1  :b  2})))

  (is= 'abc (t/kw->sym :abc))
  (is= "abc" (t/kw->str :abc))
  (is= 'abc (t/str->sym "abc"))
  (is= :abc (t/str->kw "abc"))
  (is= :abc (t/sym->kw 'abc))
  (is= "abc" (t/sym->str 'abc)))

(dotest
  (let [orig  {:b #{3 2 1}
               :a [1 2 3 { 5 :five 6 :six 4 :four }]
               :c (list 4 5 6)}
        result (str/replace
                 (with-out-str (println (t/prettify orig)))
                 \, \space)
        expected "{:a  [1 2    3 {4 :four
                                  5 :five
                                  6 :six}]
                   :b #{1 2 3}
                   :c  [4 5 6]} " ]
    (nonblank= result expected )))

(dotest
  ; (t/spyx (s/check-fn t/truthy? ))

  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [truthies    (t/keep-if boolean data)       ; coerce to primitive type
            falsies     (t/keep-if not     data) ]     ; unnatural syntax
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
              (= falsies  [false nil] ) )))
      (let [truthies    (t/keep-if t/truthy? data)
            falsies     (t/keep-if t/falsey? data) ]
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
              (= falsies  [false nil] ) ))
        (is (every? t/truthy? [true :a 'my-symbol 1 "hello" \x] ))
        (is (every? t/falsey? [false nil] ))
        (is (t/has-none? t/falsey? truthies))
        (is (t/has-none? t/truthy? falsies))

        (isnt (every? t/truthy? [true false]))
        (is (every? t/truthy? [true "FALSE"]))
        (is (every? t/truthy? [true ]))
        (is (every? t/truthy? []))))

    (testing "improved usage"
      (let [count-if (comp count t/keep-if) ]
        (let [num-true    (count-if boolean data)   ; awkward phrasing
              num-false   (count-if not     data) ] ; doesn't feel natural
          (is (and  (= 6 num-true)
                (= 2 num-false) )))
        (let [num-true    (count-if t/truthy? data)   ; matches intent much better
              num-false   (count-if t/falsey? data) ]
          (is (and  (= 6 num-true)
                (= 2 num-false) ))))))

  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [notties   (t/keep-if t/not-nil? data)
            nillies   (t/drop-if t/not-nil? data) ]
        (is (and  (= notties [true :a 'my-symbol 1 "hello" \x false] )
              (= nillies [nil] )))
        (is (every?    t/not-nil? notties))
        (is (every?        nil? [nil] ))
        (is (t/has-none?     nil? notties))
        (is (t/has-none? t/not-nil? nillies))))

    (testing "improved usage"
      (let [count-if (comp count t/keep-if) ]
        (let [num-valid-1     (count-if some?    data)  ; awkward phrasing, doesn't feel natural
              num-valid-2     (count-if t/not-nil? data)  ; matches intent much better
              num-nil         (count-if nil?     data) ]    ; intent is plain
          (is (and  (= 7 num-valid-1 num-valid-2 )
                (= 1 num-nil) )))))))


(dotest
  (is= true   (t/has-some? odd? [1 2 3] ) )
  (is= false  (t/has-some? odd? [2 4 6] ) )
  (is= false  (t/has-some? odd? []      ) )

  (is= false  (t/has-none? odd? [1 2 3] ) )
  (is= true   (t/has-none? odd? [2 4 6] ) )
  (is= true   (t/has-none? odd? []      ) ))

(dotest
  (is (every?        t/not-empty? ["one" [1] '(1) {:1 1} #{1}     ] ))
  (is (t/has-none?   t/not-empty? [ ""   [ ] '( ) {}     #{ }  nil] ))

  (is (t/has-none?   empty? ["one" [1] '(1) {:1 1} #{1}     ] ))
  (is (every?        empty? [ ""   [ ] '( ) {}     #{ }  nil] ))

  (is= (map t/not-empty? ["1" [1] '(1) {:1 1} #{1} ] )
    [true true true true true]  )
  (is= (map t/not-empty? ["" [] '() {} #{} nil] )
    [false false false false false false ] )

  (is= (t/keep-if t/not-empty?  ["1" [1] '(1) {:1 1} #{1} ] )
    ["1" [1] '(1) {:1 1} #{1} ] )
  (is= (t/drop-if t/not-empty?  [""  []  '()  {}     #{}  nil] )
    [""  []  '()  {}     #{}  nil] )

  (throws? (t/not-empty? 5))
  (throws? (t/not-empty? 3.14)))

;-----------------------------------------------------------------------------
; spy stuff
(dotest
  (is= "(+ 2 3) => 5"
    (ts/collapse-whitespace
      (with-out-str
        (is= 5 (t/spyx (+ 2 3))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace   "(inc 0) => 1
                                  (inc 1) => 2
                                  (inc 2) => 3 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 3 (t/spyx (inc 0)
                 (inc 1)
                 (inc 2))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace   ":some-kw
                                  (inc 1) => 2
                                  (inc 2) => 3 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 3    (t/spyx :some-kw
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
          (t/let-spy [a (inc 0)
                      b (+ 2 3)]
                   (t/spyx (-> (inc a) (* 2) inc))
            (-> b (* 2) (+ 3)))))))

  (is= (ts/collapse-whitespace  " a => 1
                                  b => 5 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 17
          (t/let-spy [a (inc 0)
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
        (ts/collapse-whitespace (with-out-str (t/spy (side-effect-add! 2 3) :hi))) )
      (is= ":hi => 5"
        (ts/collapse-whitespace (with-out-str (t/spy :hi  (side-effect-add! 2 3)))) )
      (is= ":after-inc => 2"
        (ts/collapse-whitespace (with-out-str (-> 1
                                                (inc)
                                                (t/spy :after-inc) ; add a custom keyword message
                                                (* 2)))))
      (is= ":after-inc => 2"
        (ts/collapse-whitespace (with-out-str (->> 1
                                                (inc)
                                                (t/spy :after-inc) ; add a custom keyword message
                                                (* 2)))))

      (is= "(side-effect-add! 2 3) => 5"
        (ts/collapse-whitespace (with-out-str (t/spyx (side-effect-add! 2 3)))) )
      (is= 15 @side-effect-cum-sum))

    (is= ":first => 5 :second => 25"
      (ts/collapse-whitespace
        (with-out-str (-> 2
                        (+ 3)
                        (t/spy :first )
                        (* 5)
                        (t/spy :second) ))))
    (is= ":first => 5 :second => 25"
      (ts/collapse-whitespace
        (with-out-str (->> 2
                        (+ 3)
                        (t/spy :first )
                        (* 5)
                        (t/spy :second) ))))

    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result))
          ]
      (is= ":value => 5"
        (ts/collapse-whitespace (with-out-str (t/spy (side-effect-add! 2 3) :value))))
      (is= ":value => 5"
        (ts/collapse-whitespace (with-out-str (t/spy :value  (side-effect-add! 2 3)))))
      (is= 10 @side-effect-cum-sum)

      (is= ":value => 5" (ts/collapse-whitespace (with-out-str (t/spy :value (+ 2 3) ))))
      (is=   ":spy => 5" (ts/collapse-whitespace (with-out-str (t/spy        (+ 2 3) ))))

      (is= "(str \"abc\" \"def\") => \"abcdef\""
        (ts/collapse-whitespace (with-out-str (t/spyx (str "abc" "def") ))))

      ; (throws? (t/spy :some-tag "some-str" 42))  ; #todo how test in cljs?
    )))

(dotest
  (let [fn2   (fn []  (t/with-spy-indent
                        (t/spy :msg2 (+ 2 3))))
        fn1   (fn []  (t/with-spy-indent
                        (t/spy :msg1 (+ 2 3))
                        (fn2)))
        fn0   (fn [] (t/spy :msg0 (+ 2 3))) ]
    (is= ":msg2 => 5"            (ts/collapse-whitespace (with-out-str (fn2))))
    (is= ":msg1 => 5 :msg2 => 5" (ts/collapse-whitespace (with-out-str (fn1))))
    (is= ":msg0 => 5"            (ts/collapse-whitespace (with-out-str (fn0)))) ))


(dotest
  (is= 3 (t/let-some [a 1
                      b 2
                      c (+ a b)]
           c))
  (is= nil (t/let-some [a 1
                        b nil
                        c 3]
             [a b c]))

  (is= 5 (t/let-some [a (+ 2 3)]
           a))
  (is= 7 (t/let-some [a (+ 2 3)
                      b (inc a)
                      c (inc b)]
           c))
  (is= nil (t/let-some [a (+ 2 3)
                        b nil
                        c (inc b)]
             c))
  (is= nil (t/let-some [a (+ 2 3)
                        b (when (< 5 0) a)
                        c (inc b)]
             c))
  (is= [0 [1 2 3 4]] (t/let-some [tgt 5
                                  [x & others] (range tgt)]
                       [x others]))
  (is= nil (t/let-some [tgt nil
                        [x & others] (range tgt)]
             [x others])))

(dotest
  (testing "vecs"
    (let [coll (range 3)]
      (isnt (t/contains-elem? coll -1))
      (is   (t/contains-elem? coll  0))
      (is   (t/contains-elem? coll  1))
      (is   (t/contains-elem? coll  2))
      (isnt (t/contains-elem? coll  3))
      (isnt (t/contains-elem? coll  nil)))

    (let [coll [ 1 :two "three" \4]]
      (isnt (t/contains-elem? coll  :no-way))
      (isnt (t/contains-elem? coll  nil))
      (is   (t/contains-elem? coll  1))
      (is   (t/contains-elem? coll  :two))
      (is   (t/contains-elem? coll  "three"))
      (is   (t/contains-elem? coll  \4)))

    (let [coll [:yes nil 3]]
      (isnt (t/contains-elem? coll  :no-way))
      (is   (t/contains-elem? coll  :yes))
      (is   (t/contains-elem? coll  nil))))

  (testing "maps"
    (let [coll {1 :two "three" \4}]
      (isnt (t/contains-elem? coll nil ))
      (isnt (t/contains-elem? coll [1 :no-way] ))
      (is   (t/contains-elem? coll [1 :two]))
      (is   (t/contains-elem? coll ["three" \4])))
    (let [coll {1 nil "three" \4}]
      (isnt (t/contains-elem? coll [nil 1] ))
      (is   (t/contains-elem? coll [1 nil] )))
    (let [coll {nil 2 "three" \4}]
      (isnt (t/contains-elem? coll [1 nil] ))
      (is   (t/contains-elem? coll [nil 2] ))))

  (testing "sets"
    (let [coll #{1 :two "three" \4}]
      (isnt (t/contains-elem? coll  :no-way))
      (is   (t/contains-elem? coll  1))
      (is   (t/contains-elem? coll  :two))
      (is   (t/contains-elem? coll  "three"))
      (is   (t/contains-elem? coll  \4)))

    (let [coll #{:yes nil}]
      (isnt (t/contains-elem? coll  :no-way))
      (is   (t/contains-elem? coll  :yes))
      (is   (t/contains-elem? coll  nil)))))

(dotest
  (is   (t/contains-key?  {:a 1 :b 2} :a))
  (is   (t/contains-key?  {:a 1 :b 2} :b))
  (isnt (t/contains-key?  {:a 1 :b 2} :x))
  (isnt (t/contains-key?  {:a 1 :b 2} :c))
  (isnt (t/contains-key?  {:a 1 :b 2}  1))
  (isnt (t/contains-key?  {:a 1 :b 2}  2))

  (is   (t/contains-key?  {:a 1 nil   2} nil))
  (isnt (t/contains-key?  {:a 1 :b  nil} nil))
  (isnt (t/contains-key?  {:a 1 :b    2} nil))

  (is   (t/contains-key? #{:a 1 :b 2} :a))
  (is   (t/contains-key? #{:a 1 :b 2} :b))
  (is   (t/contains-key? #{:a 1 :b 2}  1))
  (is   (t/contains-key? #{:a 1 :b 2}  2))
  (isnt (t/contains-key? #{:a 1 :b 2} :x))
  (isnt (t/contains-key? #{:a 1 :b 2} :c))

  (is   (t/contains-key? #{:a 5 nil   "hello"} nil))
  (isnt (t/contains-key? #{:a 5 :doh! "hello"} nil))

  (throws? (t/contains-key? [:a 1 :b 2] :a))
  (throws? (t/contains-key? [:a 1 :b 2]  1)))

(dotest
  (is   (t/contains-val? {:a 1 :b 2} 1))
  (is   (t/contains-val? {:a 1 :b 2} 2))
  (isnt (t/contains-val? {:a 1 :b 2} 0))
  (isnt (t/contains-val? {:a 1 :b 2} 3))
  (isnt (t/contains-val? {:a 1 :b 2} :a))
  (isnt (t/contains-val? {:a 1 :b 2} :b))

  (is   (t/contains-val? {:a 1 :b nil} nil))
  (isnt (t/contains-val? {:a 1 nil  2} nil))
  (isnt (t/contains-val? {:a 1 :b   2} nil))

  (throws? (t/contains-val?  [:a 1 :b 2] 1))
  (throws? (t/contains-val? #{:a 1 :b 2} 1)))

(dotest
  (is= (t/forv [x (range 4)] (* x x))
    [0 1 4 9] )
  (is= (t/forv [x (range 23)] (* x x))
    (for  [x (range 23)] (* x x)))
  (is= (t/forv [x (range 5)  y (range 2 9)] (str x y))
    (for  [x (range 5)  y (range 2 9)] (str x y))))

(dotest
  (let [xs [1 2 3]
        ys [10 20 30]]
    (is= [11 22 33]
      (t/map-let [x xs y ys] (+ x y))
      (t/map-let* {:lazy false :strict false} [x xs y ys] (+ x y))
      (t/map-let* {:lazy false :strict true}  [x xs y ys] (+ x y))
      (t/map-let* {:lazy true :strict false}  [x xs y ys] (+ x y))
      (t/map-let* {:lazy true :strict true}   [x xs y ys] (+ x y)))
    (let [result-vec (t/map-let* {:lazy false :strict true} [x xs y ys] (+ x y))
          result-lazyseq (t/map-let* {:lazy true :strict true} [result-vec xs y ys] (+ result-vec y))]
      (t/spyx (type result-vec))
      (t/spyx (type result-lazyseq))

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
    (throws?        (t/map-let                  [x xs y ys] (+ x y)))
    (throws?        (t/map-let* {:strict true}  [x xs y ys] (+ x y)))
    (is= [11 22 33] (t/map-let* {:strict false} [x xs y ys] (+ x y)))))

(dotest
  (is= (vector [])               [[]] )
  (is= (mapv identity [] [])      []  )

  (is= [[:a 0] [:b 1] [:c 2]]
    (t/zip-lazy [:a :b :c] [0 1 2])
    (t/zip-lazy [:a :b :c] (range)))
  (is= (t/zip-lazy [:a :b :c] [1 2 3])   [[:a 1] [:b 2] [:c 3]] )
  (is= (t/zip-lazy [:a] [1])             [[:a 1]] )
  (is= (t/zip-lazy [] [])                []  )
  (is= (t/zip-lazy [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]] )
  (is (instance?
        #?(:clj clojure.lang.LazySeq)
        #?(:cljs cljs.core/LazySeq)
        (t/zip-lazy [:a :b :c] (range))))

  (is= (t/zip [:a :b :c] [1 2 3])   [[:a 1] [:b 2] [:c 3]] )   ; #todo fails when use Schema for append/prepend
  (is= (t/zip [:a] [1])             [[:a 1]] )                 ; #todo fails when use Schema for append/prepend
  (is= (t/zip [] [])                []  )
  (is= (t/zip [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]] )
  (throws? (t/zip [:a :b :c] [1 2 3 4]))
  (is= (t/zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]] )

  (is (instance?
        #?(:clj clojure.lang.PersistentVector)
        #?(:cljs cljs.core/PersistentVector)
        (t/zip*  {:trunc false} [:a :b :c] [1 2 3])))
  (let [keys   [:a :b :c]
        vals   [1 2 3]
        result (atom [])]
    (doseq [[k i] (t/zip keys vals)]
      (swap! result t/append {k i}))
    (is= [{:a 1} {:b 2} {:c 3}] @result))

  ; verify that zip throws if unequal lengths, even if some colls are infinite lazy seqs
  (throws? (t/zip            [:a :b :c] [1 2 3 4]))
  (throws? (t/zip [:A :B :C] [:a :b :c] [1 2 3 4]))
  (throws? (t/zip [:a :b :c] (range)))
  (is= (t/zip* {:strict false} [:a :b :c] (range))   [[:a 0] [:b 1] [:c 2]] )
  (is= (t/zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]] )
  (is= (t/zip* {:strict false} [:A :B :C] [:a :b :c] [1 2 3 4]) [[:A :a 1] [:B :b 2] [:C :c 3]] ))

(dotest
  (is= (t/indexed [:a :b :c]) [[0 :a] [1 :b] [2 :c]])
  (is= [[0 0] [1 2] [2 4] [3 6] [4 8]]
    (take 5 (t/indexed (map #(* 2 %) (range))))) ; can work with infinite lazy lists
  (is= (t/indexed [:a :b :c]  (map #(+ 10 %) (range)))
    [ [0 :a 10]
     [1 :b 11]
     [2 :c 12] ] )
  (is= (take 5 (t/indexed (map #(+ 10 %) (range))))
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

  (is= (t/glue [1 2]  [3 4] [5 6])        [1 2 3 4 5 6])
  (is= (t/glue [1 2] '(3 4) [5 6])        [1 2 3 4 5 6])
  (is= (t/glue [] [1 2] )                [1 2] )
  (is= (t/glue [1 2] [] )                [1 2] )
  (is= (t/glue [] [1 2] [] )             [1 2] )

  (is= (t/glue '(1 2) '(3 4) '(5 6))        [1 2 3 4 5 6])
  (is= (t/glue '(1 2)  [3 4] '(5 6))        [1 2 3 4 5 6])
  (is= (t/glue  [1 2] '(3 4) '(5 6))        [1 2 3 4 5 6])
  (is= (t/glue '() '(1 2) )                 [1 2] )
  (is= (t/glue '(1 2) '() )                 [1 2] )
  (is= (t/glue '() '(1 2) '() )             [1 2] )

  (is= (t/glue (range 3) (range 5))      [0 1 2 0 1 2 3 4] )

  (is= (t/glue {:a 1} {:b 2} {:c 3})      {:a 1 :c 3 :b 2})
  (is= (t/glue {:a 1} {:b 2} )            {:a 1 :b 2})
  (is= (t/glue {:a 1} {} )                {:a 1} )
  (is= (t/glue {} {:a 1} )                {:a 1} )
  (is= (t/glue {} {:a 1} {} )             {:a 1} )

  (is= (t/glue #{1 2} #{3 4} #{6 5})     #{1 2 6 5 3 4})
  (is= (t/glue #{} #{1 2} )              #{1 2} )
  (is= (t/glue #{1 2} #{} )              #{1 2} )
  (is= (t/glue #{} #{1 2} #{} )          #{1 2} )

  (is= (t/glue (sorted-map) {:a 1} {:b 2} {:c 3})   {:a 1 :b 2 :c 3} )
  (is= (t/glue (sorted-set) #{1 2} #{3 4} #{6 5})   #{1 2 3 4 5 6})

  (is=      (t/glue (sorted-map) (hash-map :a 1   :b 2   :c 3   :d 4   :e 5   :f 6))
    {:a 1   :b 2   :c 3   :d 4   :e 5   :f 6} )
  (is= (seq (t/glue (sorted-map) (hash-map :a 1   :b 2   :c 3   :d 4   :e 5   :f 6)))
    [ [:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] ] )

  (is= (t/glue  {:band :VanHalen :singer :Dave} {:singer :Sammy} )
                {:band :VanHalen                 :singer :Sammy} )

  (is= (t/glue \a )           "a" )
  (is= (t/glue "a")           "a" )
  (is= (t/glue \a "")         "a" )
  (is= (t/glue "" "a")        "a" )
  (is= (t/glue \a  \b)        "ab" )
  (is= (t/glue "a" "b")       "ab" )
  (is= (t/glue "a" \b)        "ab" )
  (is= (t/glue \a  "b")       "ab" )
  (is= (t/glue "" "a" \b)     "ab" )
  (is= (t/glue "" \a  "b")    "ab" )
  (is= (t/glue "a" "" \b)     "ab" )
  (is= (t/glue \a  "" "b")    "ab" )
  (is= (t/glue "a" \b  "")    "ab" )
  (is= (t/glue \a  "b" "")    "ab" )
  (is= (t/glue \a  "b" "")    "ab" )
  (is= (t/glue "I" \space "like " \a " nap!" )    "I like a nap!" )
  (is= (apply t/glue [ "I" \space "like " \a " nap!"] )    "I like a nap!" )

  (throws? (t/glue   [1 2]     {:a 1} ))
  (throws? (t/glue  '(1 2)     {:a 1} ))
  (throws? (t/glue   [1 2]    #{:a 1} ))
  (throws? (t/glue  '(1 2)    #{:a 1} ))
  (throws? (t/glue   [1 2]    "hello" ))
  (throws? (t/glue  '(1 2)    "hello" ))
  (throws? (t/glue   {:a 1}   #{:a 1} ))
  (throws? (t/glue   {:a 1}   "hello" ))
  (throws? (t/glue   #{:a 1}  "hello" ))
  (throws? (t/glue   [1 2]     nil    ))
  (throws? (t/glue  '(1 2)     nil    ))
  (throws? (t/glue   {:a 1}    nil    ))
  (throws? (t/glue   #{:a 1}   nil    ))
  (throws? (t/glue   "hello"   nil    )) )

(dotest
  (let [data [[0 1 2]
              []
              [3]
              [4 5]
              [6 7 8 9]]]
    (is= (t/thru 9) (t/glue-rows data))
    (is= (t/thru 9) (reduce into [] data))))

(dotest
  (throws?            (t/append  1 2        ))
  (throws?            (t/append [1 2]       ))
  (throws?            (t/append nil   3     ))
  (is= [1 2 3    ]    (t/append [1 2] 3     ))
  (is= [1 2 3 4  ]    (t/append [1 2] 3 4   ))
  (is= [1 2 3 4 5]    (t/append [1 2] 3 4 5 ))

  (throws?            (t/append '(1 2)       ))
  (is= [1 2 3    ]    (t/append '(1 2) 3     ))
  (is= [1 2 3 4  ]    (t/append '(1 2) 3 4   ))
  (is= [1 2 3 4 5]    (t/append '(1 2) 3 4 5 ))

  (throws?            (t/append   {:a 1} 99     ))
  (throws?            (t/append   {:a 1} {:b 2} ))
  (throws?            (t/append  #{:a 1} 99     ))
  (throws?            (t/append  #{:a 1} #{99}  ))

  (testing "old conjv tests"
    (is= [  2  ]    (t/append  []  2   ))
    (is= [  2  ]    (t/append '()  2   ))
    (is= [  2 3]    (t/append  []  2  3))
    (is= [  2 3]    (t/append '()  2  3))

    (is= [1 2 3]    (t/append  [1] 2  3))
    (is= [1 2 3]    (t/append '(1) 2  3))
    (is= [1 2 3]    (t/append  [1  2] 3))
    (is= [1 2 3]    (t/append '(1  2) 3))

    (is= [1 2 3 4]  (t/append  [1  2] 3 4))
    (is= [1 2 3 4]  (t/append '(1  2) 3 4))
    (is= [1 2 3 4]  (t/append  [1] 2  3 4))
    (is= [1 2 3 4]  (t/append '(1) 2  3 4))

    (is= [[1 2] [3 4] [5 6]] (t/append  [[1 2] [3 4]]  [5 6] ))

    (is= [0 1 2 3 4 5] (t/append (range 4) 4 5))
    (is= [0 1 2 3 4 5] (apply t/append [0] (range 1 6)))))

(dotest
  (throws?            (t/prepend       [2 1] ))
  (throws?            (t/prepend     3  nil  ))
  (is= [    3 2 1]    (t/prepend     3 [2 1] ))
  (is= [  4 3 2 1]    (t/prepend   4 3 [2 1] ))
  (is= [5 4 3 2 1]    (t/prepend 5 4 3 [2 1] ))

  (throws?            (t/prepend       '(2 1) ))
  (is= [    3 2 1]    (t/prepend     3 '(2 1) ))
  (is= [  4 3 2 1]    (t/prepend   4 3 '(2 1) ))
  (is= [5 4 3 2 1]    (t/prepend 5 4 3 '(2 1) ))

  (throws?            (t/prepend   99     {:a 1} ))
  (throws?            (t/prepend   {:b 2} {:a 1} ))
  (throws?            (t/prepend   99    #{:a 1} ))
  (throws?            (t/prepend  #{99}  #{:a 1} )))

(dotest
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 2 3 4 5 6 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 3 4 5 6 7 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 (t/unwrap [3 4 5 6 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 (t/unwrap [3 (t/unwrap [4 5 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 (t/unwrap [3 (t/unwrap [4 (t/unwrap [5]) 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 2 3 (t/unwrap [4 5 6]) 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 3 (t/unwrap [4 5 6]) 7 8]) 9))

  (is= [1 2 3 [4  5  6] 7 8 9] (t/->vector 1 (t/unwrap [2 3 [4  5  6] 7 8]) 9))
  (is= [1 2 3 [4 [5] 6] 7 8 9] (t/->vector 1 (t/unwrap [2 3 [4 [5] 6] 7 8]) 9))

  (is= [1 [2 3 4 [5] 6 7 8] 9] (t/->vector 1 `(2 3 ~(t/unwrap [4 [5] 6]) 7 8) 9))
  (is= [1 [2 3 4 [5] 6 7 8] 9] (t/->vector 1  [2 3  (t/unwrap [4 [5] 6]) 7 8] 9))

  (is= [1 2 3 4 5 6 7 8 9] (t/glue   [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat   [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (t/glue   [1   2   3] [4 5 6] [7   8   9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat   [1   2   3] [4 5 6] [7   8   9])))

(dotest
  (isnt (t/increasing? [1 2] [1]))
  (isnt (t/increasing? [1 2] [1 1]))
  (isnt (t/increasing? [1 2] [1 2]))
  (is (t/increasing? [1 2] [1 2 nil]))
  (is (t/increasing? [1 2] [1 2 3]))
  (is (t/increasing? [1 2] [1 3]))
  (is (t/increasing? [1 2] [2 1]))
  (is (t/increasing? [1 2] [2]))

  (isnt (t/increasing-or-equal? [1 2] [1]))
  (isnt (t/increasing-or-equal? [1 2] [1 1]))
  (is (t/increasing-or-equal? [1 2] [1 2]))
  (is (t/increasing-or-equal? [1 2] [1 2 nil]))
  (is (t/increasing-or-equal? [1 2] [1 2 3]))
  (is (t/increasing-or-equal? [1 2] [1 3]))
  (is (t/increasing-or-equal? [1 2] [2 1]))
  (is (t/increasing-or-equal? [1 2] [2])))

(dotest
  (let [map1  { :a 1 :b 2 :c nil
               nil :NIL
               "hi" "hello"
               5 "five"}]
    (is= 1           (t/grab :a   map1))
    (is= 2           (t/grab :b   map1))
    (is= nil         (t/grab :c   map1))
    (is= :NIL        (t/grab nil  map1))
    (is= "hello"     (t/grab "hi"  map1))
    (is= "five"      (t/grab 5  map1))
    (throws?  (t/grab :z map1))
    (throws?  (t/grab 42 map1))
    ))

(dotest
  (testing "basic usage"
    (let [map1  {:a1 "a1"
                 :a2 { :b1 "b1"
                      :b2 { :c1 "c1"
                           :c2 "c2" }}
                 nil "NIL"
                 :nil nil} ]
      (is= "a1"  (t/fetch-in map1 [:a1]))
      (is= "b1"  (t/fetch-in map1 [:a2 :b1]))
      (is= "c1"  (t/fetch-in map1 [:a2 :b2 :c1]))
      (is= "c2"  (t/fetch-in map1 [:a2 :b2 :c2]))
      (is= "NIL" (t/fetch-in map1 [nil]))
      (is= nil   (t/fetch-in map1 [:nil]))
      (throws?   (t/fetch-in map1 [:a9]))
      (throws?   (t/fetch-in map1 [:a2 :b9]))
      (throws?   (t/fetch-in map1 [:a2 :b2 :c9])))))

(dotest
  (let [mm    {:a { :b { :c "c" }}} ]
    (is= (t/dissoc-in mm []         )          mm )
    (is= (t/dissoc-in mm [:a]       )          {} )
    (is= (t/dissoc-in mm [:a :b]    )          {:a  {}} )
    (is= (t/dissoc-in mm [:a :b :c] )          {:a  { :b  {}}} )
    (is= (t/dissoc-in mm [:a :x :y] )          {:a  { :b  { :c "c" }
                                                   :x  nil }} )
    (is= (t/dissoc-in mm [:a :x :y :z] )       {:a  { :b  { :c "c" }
                                                   :x  { :y nil }}} )
    (is= (t/dissoc-in mm [:k1 :k2 :k3 :kz] )   {:a  { :b  { :c  "c" }}
                                              :k1 { :k2 { :k3 nil }}} ))
  (let [mm    {:a1 "a1"
               :a2 { :b1 "b1"
                    :b2 { :c1 "c1"
                         :c2 "c2" }}} ]
    (is= (t/dissoc-in mm [:a1] )
      {:a2 { :b1 "b1"
            :b2 { :c1 "c1"
                 :c2 "c2" }}} )
    (is= (t/dissoc-in mm [:a2] )
      {:a1 "a1" } )
    (is= (t/dissoc-in mm [:a2 :b1] )
      {:a1 "a1"
       :a2 { :b2 { :c1 "c1"
                  :c2 "c2" }}} )
    (is= (t/dissoc-in mm [:a2 :b2] )
      {:a1 "a1"
       :a2 { :b1 "b1" }} )
    (is= (t/dissoc-in mm [:a2 :b2 :c1] )
      {:a1 "a1"
       :a2 { :b1 "b1"
            :b2 { :c2 "c2" }}} )
    (is= (t/dissoc-in mm [:a2 :b2 :c2] )
      {:a1 "a1"
       :a2 { :b1 "b1"
            :b2 { :c1 "c1" }}} )))

(dotest
  (t/try-catchall
    (throw (ex-info "some-big-error" {:answer 43}))
    (catch e        ; (println "Caught exception:" e)
      (is= (ex-data e) {:answer 43}) ))

  (let [result (t/with-exception-default :some-val
            (throw (ex-info "some-big-error" {:answer 43})))]
    (is= result :some-val)))

(dotest
  (let [map1  {:a 1 :b 2 :c 3 :d 4 :e 5}]
    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{ :a :b :c :d :e } ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{    :b :c :d :e } ))
    (is= {          :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{       :c :d :e } ))
    (is= {               :d 4 :e 5} (t/submap-by-keys map1 #{          :d :e } ))
    (is= {                    :e 5} (t/submap-by-keys map1 #{             :e } ))
    (is= {                        } (t/submap-by-keys map1 #{                } ))
    (throws? (t/submap-by-keys map1 #{:z}))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{ :a :b :c :d :e  :z } :missing-ok))
    (is= {     :b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{    :b :c :d :e  :z } :missing-ok))
    (is= {          :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{       :c :d :e  :z } :missing-ok))
    (is= {               :d 4 :e 5} (t/submap-by-keys map1 #{          :d :e  :z } :missing-ok))
    (is= {                    :e 5} (t/submap-by-keys map1 #{             :e  :z } :missing-ok))
    (is= {                        } (t/submap-by-keys map1 #{                 :z } :missing-ok))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{ 1 2 3 4 5 } ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{   2 3 4 5 } ))
    (is= {          :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{     3 4 5 } ))
    (is= {               :d 4 :e 5} (t/submap-by-vals map1 #{       4 5 } ))
    (is= {                    :e 5} (t/submap-by-vals map1 #{         5 } ))
    (is= {                        } (t/submap-by-vals map1 #{           } ))
    (throws? (t/submap-by-vals map1 #{ 99 }))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{ 1 2 3 4 5  99 } :missing-ok ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{   2 3 4 5  99 } :missing-ok ))
    (is= {          :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{     3 4 5  99 } :missing-ok ))
    (is= {               :d 4 :e 5} (t/submap-by-vals map1 #{       4 5  99 } :missing-ok ))
    (is= {                    :e 5} (t/submap-by-vals map1 #{         5  99 } :missing-ok ))
    (is= {                        } (t/submap-by-vals map1 #{            99 } :missing-ok ))
    (is= {                        } (t/submap-by-vals map1 #{               } :missing-ok ))

    (is= { 0 :even 2 :even } (t/submap-by-vals
                               { 0 :even 1 :odd 2 :even 3 :odd }
                                #{ :even } ))
    (is= { 0 :even 2 :even } (t/submap-by-vals
                               { 0 :even 1 :odd 2 :even 3 :odd }
                                #{ :even :prime } :missing-ok ))) )

(dotest             ; -1 implies "in order"
  ; empty list is smaller than any non-empty list
  (is (neg? (t/lexical-compare [] [2])))
  (is (neg? (t/lexical-compare [] [\b])))
  (is (neg? (t/lexical-compare [] ["b"])))
  (is (neg? (t/lexical-compare [] [:b])))
  (is (neg? (t/lexical-compare [] ['b])))

  ; nil is smaller than any non-nil item
  (is (neg? (t/lexical-compare [nil] [2])))
  (is (neg? (t/lexical-compare [nil] [\b])))
  (is (neg? (t/lexical-compare [nil] ["b"])))
  (is (neg? (t/lexical-compare [nil] [:b])))
  (is (neg? (t/lexical-compare [nil] ['b])))

  ; Cannot compare items from different classes:  number, char, string, keyword, symbol
  (throws? (t/lexical-compare [1] [\b]))
  (throws? (t/lexical-compare [1] ["b"]))
  (throws? (t/lexical-compare [1] [:b]))
  (throws? (t/lexical-compare [1] ['b]))

 #?(:clj (throws? (t/lexical-compare [\b] ["b"])))

  (throws? (t/lexical-compare [\b] [:b]))
  (throws? (t/lexical-compare [\b] ['b]))
  (throws? (t/lexical-compare ["b"] [:b]))
  (throws? (t/lexical-compare ["b"] ['b]))
  (throws? (t/lexical-compare [:b] ['b]))

  ; different positions in list can be of different class
  (is (neg? (t/lexical-compare [:a] [:b])))
  (is (neg? (t/lexical-compare [:a] [:a 1])))
  (is (neg? (t/lexical-compare [1 :a] [2])))
  (is (neg? (t/lexical-compare [:a] [:a 1])))
  (is (neg? (t/lexical-compare [1] [1 :a])))
  (is (neg? (t/lexical-compare [1 :a] [2])))

  ; same position in list can be of different class if sorted by previous positions
  (is (neg? (t/lexical-compare [1 :z] [2 9]))) ; OK since prefix lists [1] & [2] define order
  (throws?  (t/lexical-compare [1 :z] [1 2])) ; not OK since have same prefix list: [1]

  (is= (vec (sorted-set-by t/lexical-compare [1 :a] [1] [2]))
    [[1] [1 :a] [2]])
  (is= (vec (sorted-set-by t/lexical-compare [2 0] [2] [3] [3 :y] [1] [1 :a] [1 :b] [1 :b 3]))
    [[1]
     [1 :a]
     [1 :b]
     [1 :b 3]
     [2]
     [2 0]
     [3]
     [3 :y]]))

(dotest
  (is= 3 (t/validate pos? 3))
  (is= 3.14 (t/validate number? 3.14))
  (is= 3.14 (t/validate #(< 3 % 4) 3.14))
  (is= [0 1 2] (t/validate vector? (vec (range 3))))
  (is= nil (t/validate nil? (next [])))
  (is= [0 1 2] (t/validate #(= 3 (count %)) [0 1 2]))
  (throws? (t/validate number? "hello"))
  (throws? (t/validate t/truthy? nil)) )

(dotest
  (throws? (t/verify (= 1 2)))
  (is= 333 (t/verify (* 3 111))))

(dotest
  (let [m1 {:a 1 :b 2 :c 3}
        m2 {:a 1 :b 2 :c [3 4]}]
    (is= m1 (apply hash-map (t/keyvals m1)))
    (is= m2 (apply hash-map (t/keyvals m2)))))
; AWTAWT TODO: add test.check

(dotest
  (let [m1 {:a 1 :b 2 :c 3} ]
    (is= [ :a 1 :b 2      ] (t/keyvals-seq m1 [:a :b]))
    (is= [ :b 2 :a 1      ] (t/keyvals-seq m1 [:b :a]))
    (is= [ :a 1 :b 2 :c 3 ] (t/keyvals-seq m1 [:a :b :c]))
    (is= [ :c 3 :a 1 :b 2 ] (t/keyvals-seq m1 [:c :a :b]))
    (is= [ :c 3 :b 2 :a 1 ] (t/keyvals-seq m1 [:c :b :a]))
    (is= [ :a 1 :b 2 :a 1 ] (t/keyvals-seq m1 [:a :b :a]))

    (throws? (t/keyvals-seq m1 [:a :b :z]))
    (is= [:a 1 :b 2] (t/keyvals-seq {:missing-ok true
                                     :the-map    m1 :the-keys [:a :b :z]}))
    (is= [:b 2 :c 3] (t/keyvals-seq {:missing-ok true
                                     :the-map    m1 :the-keys [:z :b :c]})) ))

(dotest
  (is= 2 (t/it-> 1
           (inc it)
           (+ 3 it)
           (/ 10 it)))
  (let [mm  {:a {:b 2}}]
    (is= (t/it-> mm (:a it)          )  {:b 2} )
    (is= (t/it-> mm (it :a)  (:b it) )      2  ))
  (is= 48 (t/it-> 42
            (let [x 5]
              (+ x it))
            (inc it))))

(dotest
  (let [params {:a 1 :b 1 :c nil :d nil}]
    (is= (t/cond-it-> params
           (:a it)              (update it :b inc)
           (= (:b it) 2)        (assoc it :c "here")
           (= "here" (:c it))   (assoc it :d "again"))
      {:a 1, :b 2, :c "here", :d "again"}))

  (let [params {:a nil :b 1 :c nil :d nil}]
    (is= (t/cond-it-> params
           (:a it)                (update it :b inc)
           (= (:b it) 1)          (assoc it :c "here")
           (= "here" (:c it))     (assoc it :d "again"))
      {:a nil, :b 1, :c "here", :d "again"}))

  (let [params {:a 1 :b 1 :c nil :d nil}]
    (is= (t/cond-it-> params
           (:a it)        (update it :b inc)
           (= (:b it) 2)  (update it :b inc)
           (:c it)        (assoc it :d "again"))
      {:a 1, :b 3, :c nil :d nil})) )

(dotest
  (is= 8 (t/some-it-> 1
           (inc it)
           (* it 3)
           (+ 2 it)))
  (is (nil? (t/some-it-> nil
              (inc it)
              (* it 3)
              (+ 2 it))))
  (is (nil? (t/some-it-> 1 (inc it)
              (when false (* it 3))
              (+ 2 it)))) )

(dotest
  ; java way to throw
  #?(:clj
     (do (throws? (throw (RuntimeException. "bummer")))
         (is= nil (t/with-exception-default nil (throw (RuntimeException. "bummer"))))
         (is= :dummy (t/with-exception-default :dummy (throw (RuntimeException. "bummer dude"))))

         (is= 123 (t/with-exception-default 0 (Long/parseLong "123")))
         (is= 0 (t/with-exception-default 0 (Long/parseLong "12xy3")))))
  ; clojurescript way to throw
  #?(:cljs
     (do (throws? (throw (js/Error "bummer")))
         (is= nil (t/with-exception-default nil (throw (js/Error "bummer"))))
         (is= :dummy (t/with-exception-default :dummy (throw (js/Error "bummer dude"))))))
  ; cross-platform way to throw
  (do (throws? (throw (ex-info "some msg" :some-data)))
      (is= nil (t/with-exception-default nil (throw (ex-info "bummer" nil))))
      (is= :dummy (t/with-exception-default :dummy (throw (ex-info "bummer " " dude"))))))

(dotest
  (is= (t/validate-or-default t/not-nil? nil 0) 0)
  (is= (t/validate-or-default t/not-empty? "" "How you doin?") "How you doin?")
  (is= (mapv #(t/with-nil-default :some-default %)
         [0 1 "" [] nil true false])
    [0 1 "" [] :some-default true false]))

(dotest
  (is (t/rel= 1 1 :digits 4 ))
  (is (t/rel= 1 1 :tol    0.01 ))

  (throws? (t/rel= 1 1 ))
  (throws? (t/rel= 1 1 4))
  (throws? (t/rel= 1 1 :xxdigits 4      ))
  (throws? (t/rel= 1 1 :digits   4.1    ))
  (throws? (t/rel= 1 1 :digits   0      ))
  (throws? (t/rel= 1 1 :digits  -4      ))
  (throws? (t/rel= 1 1 :tol    -0.01    ))
  (throws? (t/rel= 1 1 :tol     "xx"    ))
  (throws? (t/rel= 1 1 :xxtol   0.01    ))

  (is      (t/rel=   0   0   :digits 3 ))
  (is      (t/rel=  42  42   :digits 99 ))
  (is      (t/rel=  42  42.0 :digits 99 ))

  (is      (t/rel= 1 1.001 :digits 3 ))
  (is (not (t/rel= 1 1.001 :digits 4 )))
  (is      (t/rel=   123450000   123456789 :digits 4 ))
  (is (not (t/rel=   123450000   123456789 :digits 6 )))
  (is      (t/rel= 0.123450000 0.123456789 :digits 4 ))
  (is (not (t/rel= 0.123450000 0.123456789 :digits 6 )))

  (is      (t/rel= 1 1.001 :tol 0.01 ))
  (is (not (t/rel= 1 1.001 :tol 0.0001 ))) )

(dotest
  (is (every? t/truthy? (t/forv [ul (range 0 4)] (vector? (t/range-vec ul)))))

  (is (every? t/truthy? (t/forv [ul (range 0 4)] (= (t/range-vec ul) (range ul)))))

  (is (every? t/truthy? (t/forv [lb (range 0 4)
                                 ub (range lb 4)]
                          (= (t/range-vec lb ub) (range lb ub))))))

(dotest
  (testing "positive step"
    (is= [0      ] (t/thru 0))
    (is= [0 1    ] (t/thru 1))
    (is= [0 1 2  ] (t/thru 2))
    (is= [0 1 2 3] (t/thru 3))

    (is= [0      ] (t/thru 0 0))
    (is= [0 1    ] (t/thru 0 1))
    (is= [0 1 2  ] (t/thru 0 2))
    (is= [0 1 2 3] (t/thru 0 3))

    (is= [       ] (t/thru 1 0))
    (is= [  1    ] (t/thru 1 1))
    (is= [  1 2  ] (t/thru 1 2))
    (is= [  1 2 3] (t/thru 1 3))

    (is= [       ] (t/thru 2 0))
    (is= [       ] (t/thru 2 1))
    (is= [    2  ] (t/thru 2 2))
    (is= [    2 3] (t/thru 2 3))

    (is= [       ] (t/thru 3 0))
    (is= [       ] (t/thru 3 1))
    (is= [       ] (t/thru 3 2))
    (is= [      3] (t/thru 3 3))

    (is= [       ] (t/thru 4 0))
    (is= [       ] (t/thru 4 1))
    (is= [       ] (t/thru 4 2))
    (is= [       ] (t/thru 4 3))


    (is= [0      ] (t/thru 0 0 1))
    (is= [0 1    ] (t/thru 0 1 1))
    (is= [0 1 2  ] (t/thru 0 2 1))
    (is= [0 1 2 3] (t/thru 0 3 1))

    (is= [       ] (t/thru 1 0 1))
    (is= [  1    ] (t/thru 1 1 1))
    (is= [  1 2  ] (t/thru 1 2 1))
    (is= [  1 2 3] (t/thru 1 3 1))

    (is= [       ] (t/thru 2 0 1))
    (is= [       ] (t/thru 2 1 1))
    (is= [    2  ] (t/thru 2 2 1))
    (is= [    2 3] (t/thru 2 3 1))

    (is= [       ] (t/thru 3 0 1))
    (is= [       ] (t/thru 3 1 1))
    (is= [       ] (t/thru 3 2 1))
    (is= [      3] (t/thru 3 3 1))

    (is= [       ] (t/thru 4 0 1))
    (is= [       ] (t/thru 4 1 1))
    (is= [       ] (t/thru 4 2 1))
    (is= [       ] (t/thru 4 3 1))


    (is=        [0      ] (t/thru 0 0 2))
    (throws?              (t/thru 0 1 2))
    (is=        [0   2  ] (t/thru 0 2 2))
    (throws?              (t/thru 0 3 2))

    (throws?              (t/thru 1 0 2))
    (is=        [  1    ] (t/thru 1 1 2))
    (throws?              (t/thru 1 2 2))
    (is=        [  1   3] (t/thru 1 3 2))

    (is=        [       ] (t/thru 2 0 2))
    (throws?              (t/thru 2 1 2))
    (is=        [    2  ] (t/thru 2 2 2))
    (throws?              (t/thru 2 3 2))

    (throws?              (t/thru 3 0 2))
    (is=        [       ] (t/thru 3 1 2))
    (throws?              (t/thru 3 2 2))
    (is=        [      3] (t/thru 3 3 2))


    (is=        [0      ] (t/thru 0 0 3))
    (throws?              (t/thru 0 1 3))
    (throws?              (t/thru 0 2 3))
    (is=        [0     3] (t/thru 0 3 3))

    (throws?              (t/thru 1 0 3))
    (is=        [  1    ] (t/thru 1 1 3))
    (throws?              (t/thru 1 2 3))
    (throws?              (t/thru 1 3 3))

    (throws?              (t/thru 2 0 3))
    (throws?              (t/thru 2 1 3))
    (is=        [    2  ] (t/thru 2 2 3))
    (throws?              (t/thru 2 3 3))

    (is=        [       ] (t/thru 3 0 3))
    (throws?              (t/thru 3 1 3))
    (throws?              (t/thru 3 2 3))
    (is=        [      3] (t/thru 3 3 3)))
  (testing "negative step"
    (is= [      0] (t/thru 0 0 -1))
    (is= [    1 0] (t/thru 1 0 -1))
    (is= [  2 1 0] (t/thru 2 0 -1))
    (is= [3 2 1 0] (t/thru 3 0 -1))

    (is= [       ] (t/thru 0 1 -1))
    (is= [    1  ] (t/thru 1 1 -1))
    (is= [  2 1  ] (t/thru 2 1 -1))
    (is= [3 2 1  ] (t/thru 3 1 -1))

    (is= [       ] (t/thru 0 2 -1))
    (is= [       ] (t/thru 1 2 -1))
    (is= [  2    ] (t/thru 2 2 -1))
    (is= [3 2    ] (t/thru 3 2 -1))

    (is= [       ] (t/thru 0 3 -1))
    (is= [       ] (t/thru 1 3 -1))
    (is= [       ] (t/thru 2 3 -1))
    (is= [3      ] (t/thru 3 3 -1))


    (is=         [      0] (t/thru 0 0 -2))
    (throws?               (t/thru 1 0 -2))
    (is=         [  2   0] (t/thru 2 0 -2))
    (throws?               (t/thru 3 0 -2))

    (throws?               (t/thru 0 1 -2))
    (is=         [    1  ] (t/thru 1 1 -2))
    (throws?               (t/thru 2 1 -2))
    (is=         [3   1  ] (t/thru 3 1 -2))

    (is=         [       ] (t/thru 0 2 -2))
    (throws?               (t/thru 1 2 -2))
    (is=         [  2    ] (t/thru 2 2 -2))
    (throws?               (t/thru 3 2 -2))

    (throws?               (t/thru 0 3 -2))
    (is=         [       ] (t/thru 1 3 -2))
    (throws?               (t/thru 2 3 -2))
    (is=         [3      ] (t/thru 3 3 -2))


    (is=         [      0] (t/thru 0 0 -3))
    (throws?               (t/thru 1 0 -3))
    (throws?               (t/thru 2 0 -3))
    (is=         [3     0] (t/thru 3 0 -3))

    (throws?               (t/thru 0 1 -3))
    (is=         [    1  ] (t/thru 1 1 -3))
    (throws?               (t/thru 2 1 -3))
    (throws?               (t/thru 3 1 -3))

    (throws?               (t/thru 0 2 -3))
    (throws?               (t/thru 1 2 -3))
    (is=         [  2    ] (t/thru 2 2 -3))
    (throws?               (t/thru 3 2 -3))

    (is=         [       ] (t/thru 0 3 -3))
    (throws?               (t/thru 1 3 -3))
    (throws?               (t/thru 2 3 -3))
    (is=         [3      ] (t/thru 3 3 -3)))
  (testing "combinations"
    (is= [    0  2  4  6  8  10] (t/thru   0  10  2))
    (is= [    0 -2 -4 -6 -8 -10] (t/thru   0 -10 -2))
    (is= [       2  4  6  8  10] (t/thru   2  10  2))
    (is= [      -2 -4 -6 -8 -10] (t/thru  -2 -10 -2))
    (is= [ 2  0 -2 -4 -6 -8 -10] (t/thru   2 -10 -2))
    (is= [-2  0  2  4  6  8  10] (t/thru  -2  10  2))

    (is= [ 10  8  6  4  2  0   ] (t/thru  10   0 -2))
    (is= [-10 -8 -6 -4 -2  0   ] (t/thru -10   0  2))
    (is= [ 10  8  6  4  2      ] (t/thru  10   2 -2))
    (is= [-10 -8 -6 -4 -2      ] (t/thru -10  -2  2))
    (is= [ 10  8  6  4  2  0 -2] (t/thru  10  -2 -2))
    (is= [-10 -8 -6 -4 -2  0  2] (t/thru -10   2  2))

    (is= [    0  5  10] (t/thru   0  10  5))
    (is= [    0 -5 -10] (t/thru   0 -10 -5))
    (is= [       5  10] (t/thru   5  10  5))
    (is= [      -5 -10] (t/thru  -5 -10 -5))
    (is= [ 5  0 -5 -10] (t/thru   5 -10 -5))
    (is= [-5  0  5  10] (t/thru  -5  10  5))

    (is= [ 10  5  0   ] (t/thru  10   0 -5))
    (is= [-10 -5  0   ] (t/thru -10   0  5))
    (is= [ 10  5      ] (t/thru  10   5 -5))
    (is= [-10 -5      ] (t/thru -10  -5  5))
    (is= [ 10  5  0 -5] (t/thru  10  -5 -5))
    (is= [-10 -5  0  5] (t/thru -10   5  5)))
  (testing "floats"
    (is (t/all-rel= [1.1 1.3 1.5 1.7] (t/thru 1.1 1.7 0.2) :digits 7))
    (is (t/all-rel= [1.1 1.2 1.3 1.4] (t/thru 1.1 1.4 0.1) :digits 7)))
  (throws? (t/thru 1.1 2.1 0.3)) )

(dotest
  (is= [0 2 4 6 8]  (t/keep-if even? (range 10))
    (t/drop-if odd?  (range 10)))
  (is= [1 3 5 7 9]  (t/keep-if odd?  (range 10))
    (t/drop-if even? (range 10)))

  ; If we supply a 2-arg fn when filtering a sequence, we get an Exception (CLJ only)
  #?(:clj (throws? (t/keep-if (fn [arg1 arg2] :dummy) #{1 2 3})))

  ; Verify throw if coll is not a sequential, map, or set.
  (throws? (t/keep-if t/truthy? 2 ))
  (throws? (t/keep-if t/truthy? :some-kw )))

(dotest
  (let [m1 {10 0, 20 0
            11 1, 21 1
            12 2, 22 2
            13 3, 23 3}]
    (is= (t/keep-if (fn [k v] (odd? k)) m1)
      (t/drop-if (fn [k v] (even? k)) m1)
      {11 1, 21 1
       13 3, 23 3})
    (is= (t/keep-if (fn [k v] (even? k)) m1) (t/keep-if (fn [k v] (even? v)) m1)
      (t/drop-if (fn [k v] (odd? k)) m1) (t/drop-if (fn [k v] (odd? v)) m1)
      {10 0, 20 0
       12 2, 22 2})
    (is= (t/keep-if (fn [k v] (< k 19)) m1)
      (t/drop-if (fn [k v] (> k 19)) m1)
      {10 0
       11 1
       12 2
       13 3})
    (is= (t/keep-if (fn [k v] (= 1 (int (/ k 10)))) m1)
      (t/drop-if (fn [k v] (= 2 (int (/ k 10)))) m1)
      {10 0
       11 1
       12 2
       13 3})
    (is= (t/keep-if (fn [k v] (= 2 (int (/ k 10)))) m1)
      (t/drop-if (fn [k v] (= 1 (int (/ k 10)))) m1)
      {20 0
       21 1
       22 2
       23 3})
    (is= (t/keep-if (fn [k v] (<= v 1)) m1)
      (t/drop-if (fn [k v] (<= 2 v)) m1)
      {10 0, 20 0
       11 1, 21 1})

    ; If we supply a 1-arg fn when filtering a map, we get an Exception
    #?(:clj (throws? (t/keep-if (fn [arg] :dummy) {:a 1}))) )
  (let [s1 (into (sorted-set) (range 10))]
    (is= #{0 2 4 6 8} (t/keep-if even? s1)
      (t/drop-if odd? s1))
    (is= #{1 3 5 7 9} (t/keep-if odd? s1)
      (t/drop-if even? s1))

    ; If we supply a 2-arg fn when filtering a set, we get an Exception
    #?(:clj (throws? (t/keep-if (fn [arg1 arg2] :dummy) #{1 2 3}))) ))

#?(:cljs
   (dotest ; in JS a char is just a single-char string
     (is= "a" \a (t/int->char 97))
     (is= 97 (t/char->int "a") (t/char->int \a))
     (is= [\a \b \c] (vec "abc"))
     (is= [97 98 99] (t/spyx (mapv t/char->int (t/str->chars "abc"))))))

(dotest
  (is= "a" (t/strcat \a  ) (t/strcat [\a]  ))
  (is= "a" (t/strcat "a" ) (t/strcat ["a"] ))
  (is= "a" (t/strcat 97  ) (t/strcat [97]  ))

  (is= "ab" (t/strcat \a   \b   ) (t/strcat [\a]  \b   ))
  (is= "ab" (t/strcat \a  [\b]  ) (t/strcat [\a   \b]  ))
  (is= "ab" (t/strcat "a"  "b"  ) (t/strcat ["a"] "b"  ))
  (is= "ab" (t/strcat "a" ["b"] ) (t/strcat ["a"  "b"] ))
  (is= "ab" (t/strcat 97   98   ) (t/strcat [97]  98   ))
  (is= "ab" (t/strcat 97  [98]  ) (t/strcat [97   98]  ))
  (is= "ab" (t/strcat ""  "ab"  ) (t/strcat ["" \a "b"]))

  ; #todo make work for CLJS
  (is= "abcd" (t/strcat 97 98 "cd"))
  (is= "abcd" (t/strcat [97 98] "cd"))
  (is= "abcd" (str/join (t/chars-thru \a \d)))
  #?(:clj
     (do
       (is= "abcd" (t/strcat (byte-array [97 98]) "cd"))

       (is= (t/strcat "I " [\h \a nil \v [\e \space nil (byte-array [97])
                                          [nil 32 "complicated" (Math/pow 2 5) '("str" nil "ing")]]])
         "I have a complicated string")))
  #?(:cljs
     (do
       (is= (t/strcat "I " [\h \a nil \v [\e \space nil [97]
                                          [nil 32 "complicated" (Math/pow 2 5) '("str" nil "ing")]]])
         "I have a complicated string")))
  (let [chars-set (into #{} (t/chars-thru \a \z))
        str-val   (t/strcat chars-set)]
    (is= 26 (count chars-set))
    (is= 26 (count str-val))
    (is= 26 (count (re-seq #"[a-z]" str-val)))))

(dotest
  (testing "single string"
    (is (= ""         (t/clip-str 0 "abcdefg")))
    (is (= "a"        (t/clip-str 1 "abcdefg")))
    (is (= "ab"       (t/clip-str 2 "abcdefg")))
    (is (= "abc"      (t/clip-str 3 "abcdefg")))
    (is (= "abcd"     (t/clip-str 4 "abcdefg")))
    (is (= "abcde"    (t/clip-str 5 "abcdefg"))))
  (testing "two strings"
    (is (= ""         (t/clip-str 0 "abc defg")))
    (is (= "a"        (t/clip-str 1 "abc defg")))
    (is (= "ab"       (t/clip-str 2 "abc defg")))
    (is (= "abc"      (t/clip-str 3 "abc defg")))
    (is (= "abc "     (t/clip-str 4 "abc defg")))
    (is (= "abc d"    (t/clip-str 5 "abc defg"))))
  (testing "two strings & char"
    (is (= ""         (t/clip-str 0 "ab" \c "defg")))
    (is (= "a"        (t/clip-str 1 "ab" \c "defg")))
    (is (= "ab"       (t/clip-str 2 "ab" \c "defg")))
    (is (= "abc"      (t/clip-str 3 "ab" \c "defg")))
    (is (= "abcd"     (t/clip-str 4 "ab" \c "defg")))
    (is (= "abcde"    (t/clip-str 5 "ab" \c "defg"))))
  (testing "two strings & digit"
    (is (= ""         (t/clip-str 0 "ab" 9 "defg")))
    (is (= "a"        (t/clip-str 1 "ab" 9 "defg")))
    (is (= "ab"       (t/clip-str 2 "ab" 9 "defg")))
    (is (= "ab9"      (t/clip-str 3 "ab" 9 "defg")))
    (is (= "ab9d"     (t/clip-str 4 "ab" 9 "defg")))
    (is (= "ab9de"    (t/clip-str 5 "ab" 9 "defg"))))
  (testing "vector"
    (is (= ""               (t/clip-str  0 [1 2 3 4 5] )))
    (is (= "["              (t/clip-str  1 [1 2 3 4 5] )))
    (is (= "[1"             (t/clip-str  2 [1 2 3 4 5] )))
    (is (= "[1 2"           (t/clip-str  4 [1 2 3 4 5] )))
    (is (= "[1 2 3 4"       (t/clip-str  8 [1 2 3 4 5] )))
    (is (= "[1 2 3 4 5]"    (t/clip-str 16 [1 2 3 4 5] ))))
  (testing "map"
    (is (= ""               (t/clip-str  0 (sorted-map :a 1 :b 2) )))
    (is (= "{"              (t/clip-str  1 (sorted-map :a 1 :b 2) )))
    (is (= "{:"             (t/clip-str  2 (sorted-map :a 1 :b 2) )))
    (is (= "{:a "           (t/clip-str  4 (sorted-map :a 1 :b 2) )))
    (is (= "{:a 1, :"       (t/clip-str  8 (sorted-map :a 1 :b 2) )))
    (is (= "{:a 1, :b 2}"   (t/clip-str 16 (sorted-map :a 1 :b 2) ))))
  (testing "set"
    (let [tst-set (sorted-set 5 4 3 2 1) ]
      (is (= ""             (t/clip-str  0 tst-set )))
      (is (= "#"            (t/clip-str  1 tst-set )))
      (is (= "#{"           (t/clip-str  2 tst-set )))
      (is (= "#{1 "         (t/clip-str  4 tst-set )))
      (is (= "#{1 2 3 "     (t/clip-str  8 tst-set )))
      (is (= "#{1 2 3 4 5}" (t/clip-str 16 tst-set ))))) )

(dotest
  (is= [] (t/drop-at (range 1) 0))

  (is= [  1] (t/drop-at (range 2) 0))
  (is= [0  ] (t/drop-at (range 2) 1))

  (is= [  1 2] (t/drop-at (range 3) 0))
  (is= [0   2] (t/drop-at (range 3) 1))
  (is= [0 1  ] (t/drop-at (range 3) 2))

  (throws? (t/drop-at []         0))
  (throws? (t/drop-at (range 3) -1))
  (throws? (t/drop-at (range 3)  3)))

(dotest
  (is= [9] (t/insert-at [] 0 9))

  (is= [9 0] (t/insert-at [0] 0 9))
  (is= [0 9] (t/insert-at [0] 1 9))

  (is= [9 0 1] (t/insert-at [0 1] 0 9))
  (is= [0 9 1] (t/insert-at [0 1] 1 9))
  (is= [0 1 9] (t/insert-at [0 1] 2 9))

  (throws? (t/insert-at [] -1 9))
  (throws? (t/insert-at []  1 9))

  (throws? (t/insert-at [0] -1 9))
  (throws? (t/insert-at [0]  2 9))

  (throws? (t/insert-at [0 1] -1 9))
  (throws? (t/insert-at [0 1]  3 9)))

(dotest
  (is= [9] (t/replace-at (range 1) 0 9))

  (is= [9 1] (t/replace-at (range 2) 0 9))
  (is= [0 9] (t/replace-at (range 2) 1 9))

  (is= [9 1 2] (t/replace-at (range 3) 0 9))
  (is= [0 9 2] (t/replace-at (range 3) 1 9))
  (is= [0 1 9] (t/replace-at (range 3) 2 9))

  (throws? (t/replace-at []         0 9))
  (throws? (t/replace-at (range 3) -1 9))
  (throws? (t/replace-at (range 3)  3 9)))

(dotest             ; #todo need more tests
  (is= (mapv #(mod % 3) (t/thru -6 6)) [0 1 2 0 1 2 0 1 2 0 1 2 0])
  (is= (mapv #(t/idx [0 1 2] %) (t/thru -3 3)) [0 1 2 0 1 2 0 ]))

(dotest
  (println :awt100 (t/chars-thru \a \a))
  (is (= [\a ] (t/spyx (t/chars-thru \a \a))))
  (is (= [\a \b]            (t/chars-thru \a \b)))
  (is (= [\a \b \c]         (t/chars-thru \a \c)))

  (is (= [\a ]              (t/chars-thru 97 97)))
  (is (= [\a \b]            (t/chars-thru 97 98)))
  (is (= [\a \b \c]         (t/chars-thru 97 99)))

  (throws? (t/chars-thru 987654321 987654321))
  (throws? (t/chars-thru \c \a))
  (throws? (t/chars-thru 99 98)))

(dotest
  (let [map-ab  {:a 1 :b 2}
        map-abc {:a 1 :b 2 :c 3}]
    (is= map-ab (t/validate-map-keys map-ab [:a :b]))
    (is= map-ab (t/validate-map-keys map-ab [:a :b :x]))
    (is= map-ab (t/validate-map-keys map-ab #{:a :b}))
    (is= map-ab (t/validate-map-keys map-ab #{:a :b :x}))
    (is= map-abc (t/validate-map-keys map-abc [:a :b :c :x]))
    (throws? (t/validate-map-keys map-ab [:a]))
    (throws? (t/validate-map-keys map-ab [:b]))
    (throws? (t/validate-map-keys map-ab [:a :x]))
    (throws? (t/validate-map-keys map-abc [:a :b]))
    (throws? (t/validate-map-keys map-abc [:a :c :x]))))

(dotest
  (let [map-123 {1 :a 2 :b 3 :c}
        tx-fn   {1 101 2 202 3 303}]
    (is= (t/map-keys map-123 inc) {2 :a 3 :b 4 :c})
    (is= (t/map-keys map-123 tx-fn) {101 :a 202 :b 303 :c}))
  (let [map-123 {:a 1 :b 2 :c 3}
        tx-fn   {1 101 2 202 3 303}]
    (is= (t/map-vals map-123 inc) {:a 2, :b 3, :c 4})
    (is= (t/map-vals map-123 tx-fn) {:a 101, :b 202, :c 303})))

(dotest
  (is   (t/starts-with? (range 0 3) (range 0 0)))

  (is   (t/starts-with? (range 0 3) (range 0 1)))
  (is   (t/starts-with? (range 0 3) (range 0 2)))
  (is   (t/starts-with? (range 0 3) (range 0 3)))

  (isnt (t/starts-with? (range 0 3) (range 1 2)))
  (isnt (t/starts-with? (range 0 3) (range 1 3)))

  (isnt (t/starts-with? (range 0 3) (range 2 3)))

  (isnt (t/starts-with? (range 1 3) (range 0 1)))
  (isnt (t/starts-with? (range 1 3) (range 0 2)))
  (isnt (t/starts-with? (range 1 3) (range 0 3)))

  (is   (t/starts-with? (range 1 3) (range 1 2)))
  (is   (t/starts-with? (range 1 3) (range 1 3)))

  (isnt (t/starts-with? (range 1 3) (range 2 3)))

  (isnt (t/starts-with? (range 2 3) (range 0 1)))
  (isnt (t/starts-with? (range 2 3) (range 0 2)))
  (isnt (t/starts-with? (range 2 3) (range 0 3)))

  (isnt (t/starts-with? (range 2 3) (range 1 2)))
  (isnt (t/starts-with? (range 2 3) (range 1 3)))

  (is   (t/starts-with? (range 2 3) (range 2 3)))

  (isnt (t/starts-with? (range 3 3) (range 0 1)))
  (isnt (t/starts-with? (range 3 3) (range 0 2)))
  (isnt (t/starts-with? (range 3 3) (range 0 3))) )

(defrecord SampleRec [a b])
(dotest
  (let [sr1 (->SampleRec 1 2)]
    (is (map? sr1))
    (is (t/val= sr1 {:a 1 :b 2}))
    (is (t/val= 1 1))
    (is (t/val= "abc" "abc"))
    (is (t/val= [1 2 3] [1 2 3]))
    (is (t/val= #{1 2 sr1} #{1 2 {:a 1 :b 2}}))
    (is (t/val= [1 2 3 #{1 2 sr1}] [1 2 3 #{1 2 {:a 1 :b 2}}])) ) )

(dotest
  (testing "fibo stuff"
    (is= (take  0 (t/fibonacci-seq))  [] )
    (is= (take  5 (t/fibonacci-seq))  [0 1 1 2 3] )
    (is= (take 10 (t/fibonacci-seq))  [0 1 1 2 3 5 8 13 21 34] )

    (is= (t/fibo-thru  0) [0] )
    (is= (t/fibo-thru  1) [0 1 1] )
    (is= (t/fibo-thru  2) [0 1 1 2] )
    (is= (t/fibo-thru  3) [0 1 1 2 3] )
    (is= (t/fibo-thru  4) [0 1 1 2 3] )
    (is= (t/fibo-thru  5) [0 1 1 2 3 5] )
    (is= (t/fibo-thru  6) [0 1 1 2 3 5] )
    (is= (t/fibo-thru  7) [0 1 1 2 3 5] )
    (is= (t/fibo-thru  8) [0 1 1 2 3 5 8] )
    (is= (t/fibo-thru 34) [0 1 1 2 3 5 8 13 21 34] )

    (is=  0 (t/fibo-nth 0))
    (is=  1 (t/fibo-nth 1))
    (is=  1 (t/fibo-nth 2))
    (is=  2 (t/fibo-nth 3))
    (is=  3 (t/fibo-nth 4))
    (is=  5 (t/fibo-nth 5))
    (is=  8 (t/fibo-nth 6))
    (is= 13 (t/fibo-nth 7))
    (is= 21 (t/fibo-nth 8))
    (is= 34 (t/fibo-nth 9))
    (is (< (Math/pow 2 62) (t/fibo-nth 91) (Math/pow 2 63)))))

;---------------------------------------------------------------------------------------------------
(dotest
  (is= [ [] [\a   \b   \c   \d   \e   \f]      ] (t/split-match "abcdef" "a"   ))
  (is= [ [] [\a   \b   \c   \d   \e   \f]      ] (t/split-match "abcdef" "ab"  ))
  (is= [    [\a] [\b   \c   \d   \e   \f]      ] (t/split-match "abcdef" "bc"  ))
  (is= [    [\a   \b] [\c   \d   \e   \f]      ] (t/split-match "abcdef" "cde" ))
  (is= [    [\a   \b   \c] [\d   \e   \f]      ] (t/split-match "abcdef" "de"  ))
  (is= [    [\a   \b   \c   \d] [\e   \f]      ] (t/split-match "abcdef" "ef"  ))
  (is= [    [\a   \b   \c   \d   \e] [\f]      ] (t/split-match "abcdef" "f"   ))
  (is= [    [\a   \b   \c   \d   \e   \f]  []  ] (t/split-match "abcdef" "fg"  ))
  (is= [    [\a   \b   \c   \d   \e   \f]  []  ] (t/split-match "abcdef" "gh"  ))

  (is= [    [0   1   2   3   4   5]  []  ]       (t/split-match (range 6) [-1]    ))
  (is= [ [] [0   1   2   3   4   5]      ]       (t/split-match (range 6) [0]     ))
  (is= [ [] [0   1   2   3   4   5]      ]       (t/split-match (range 6) [0 1]   ))
  (is= [    [0   1] [2   3   4   5]      ]       (t/split-match (range 6) [2 3]   ))
  (is= [    [0   1   2] [3   4   5]      ]       (t/split-match (range 6) [3 4 5] ))
  (is= [    [0   1   2   3] [4   5]      ]       (t/split-match (range 6) [4 5]   ))
  (is= [    [0   1   2   3   4] [5]      ]       (t/split-match (range 6) [5]     ))
  (is= [    [0   1   2   3   4   5]  []  ]       (t/split-match (range 6) [5 6]   ))
  (is= [    [0   1   2   3   4   5]  []  ]       (t/split-match (range 6) [6 7]   )))

(dotest
  (is= nil (t/index-using #(= [666]       %) (range 5)))
  (is= 0   (t/index-using #(= [0 1 2 3 4] %) (range 5)))
  (is= 1   (t/index-using #(= [  1 2 3 4] %) (range 5)))
  (is= 2   (t/index-using #(= [    2 3 4] %) (range 5)))
  (is= 3   (t/index-using #(= [      3 4] %) (range 5)))
  (is= 4   (t/index-using #(= [        4] %) (range 5)))
  (is= nil (t/index-using #(= [         ] %) (range 5))))

(dotest
  (is= [ [] [0   1   2   3   4]    ] (t/split-using #(= 0 (first %)) (range 5)))
  (is= [    [0] [1   2   3   4]    ] (t/split-using #(= 1 (first %)) (range 5)))
  (is= [    [0   1] [2   3   4]    ] (t/split-using #(= 2 (first %)) (range 5)))
  (is= [    [0   1   2] [3   4]    ] (t/split-using #(= 3 (first %)) (range 5)))
  (is= [    [0   1   2   3] [4]    ] (t/split-using #(= 4 (first %)) (range 5)))
  (is= [    [0   1   2   3   4] [] ] (t/split-using #(= 5 (first %)) (range 5)))
  (is= [    [0   1   2   3   4] [] ] (t/split-using #(= 9 (first %)) (range 5)))

  (is= [[\a \b \c] [\d \e \f]] (t/split-using #(t/starts-with? % "de") "abcdef")))

(dotest
  (let [start-segment? (fn [vals] (zero? (rem (first vals) 3))) ]
    (is= (t/partition-using start-segment? [1 2 3 6 7 8])
      [[1 2] [3] [6 7 8]])
    (is= (t/partition-using start-segment? [3 6 7 9])
      [[3] [6 7] [9]])
    (is= (t/partition-using start-segment? [1 2 3 6 7 8 9 12 13 15 16 17 18 18 18 3 4 5])
      [[1 2] [3] [6 7 8] [9] [12 13] [15 16 17] [18] [18] [18] [3 4 5]]))
  (throws? (t/partition-using even? 5)))


















