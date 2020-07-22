;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.core
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.core]
             [tupelo.misc]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.string :as str]
    [tupelo.misc :as misc]
    [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty
                               vals->map map-plain? forv glue]]
    [tupelo.string :as ts]
    [tupelo.testy :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]])
  #?(:clj (:require [tupelo.types :as types]))
  )

#?(:cljs (enable-console-print!))

(define-fixture :once
  {:enter (fn [ctx]
            (println "*** TEST ONCE *** - tst.tupelo.core enter ")
            )
   :leave (fn [ctx]
            (println "*** TEST ONCE *** - tst.tupelo.core leave ")
            )})

;--------------------------------------------------------------------------------------------------
#?(:clj
   (dotest
     (is (ts/contains-str? (with-out-str
                             (println "clojure.core/println"))
           "println"))
     (is (ts/contains-str? (t/with-system-out-str
                             (doto System/out
                               (.println "System.out.println")))
           "println"))
     (is (ts/contains-str? (t/with-system-err-str
                             (doto System/err
                               (.println "System.err.println")))
           "println"))))

;--------------------------------------------------------------------------------------------------

(dotest
  (is (nil? (t/noop)))
  (is (nil? (t/noop 1)))
  (is (nil? (t/noop 1 2 3 4))))
(dotest
  (let [always-42-fn (t/const-fn 42)]
    (is= 42 (always-42-fn))
    (is= 42 (always-42-fn 1))
    (is= 42 (always-42-fn 1 2 3 4))))

(dotest
  (is (t/truthy? true))
  (is (t/truthy? :never))
  (is (t/truthy? 5))
  (is (t/truthy? 0))
  (is (t/truthy? "false"))
  (is (t/truthy? []))
  (is (t/truthy? {}))
  (is (t/truthy? #{}))
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


(defrecord DummyRec
  [stuff])
(dotest
  (let [dummyRec (->DummyRec 5)]
    (is (map? {:a 1})) ; expected
    (isnt (record? {:a 1})) ; expected
    (is (record? dummyRec)) ; expected
    (is (map? dummyRec)) ; *** problem ***

    (is (map-plain? (sorted-map))) ; expected
    (is (map-plain? {:a 1})) ; solution
    (isnt (map-plain? dummyRec))) ;solution

  (let [vv [1 2 3]]
    (isnt (map? vv))
    (isnt (map-plain? vv))))


(dotest   ; #todo => tupelo.core
  (is (t/only? [1]))
  (is (t/only? {:a 1}))
  (is (t/only? #{:stuff}))
  (isnt (t/only? [1 2]))
  (isnt (t/only? {:a 1 :b 2}))
  (isnt (t/only? #{:stuff :more}))

  (is (t/only2? [[1]]))
  (is (t/only2? #{{:a 1}}))
  (is (t/only2? #{#{:stuff}}))
  (isnt (t/only2? [[1 2]]))
  (isnt (t/only2? [{:a 1 :b 2}]))
  (isnt (t/only2? [#{:stuff :more}])))

(dotest   ; #todo => tupelo.core
  (is= 5 (t/with-cum-val 0
           (doseq [ii (t/thru 5)]
             (t/cum-val-set-it ii))))
  (is= 15 (t/with-cum-val 0
            (doseq [ii (t/thru 5)]
              (t/cum-val-set-it (+ it ii)))))
  (is= (t/with-cum-val {}
         (doseq [ii (t/thru 3)]
           (t/cum-val-set-it (glue it {ii (+ 10 ii)}))))
    {0 10
     1 11
     2 12
     3 13})
  (is= (t/with-cum-val {}
         (doseq [ii (t/thru 3)]
           (swap! t/*cumulative-val* assoc ii (+ 10 ii)))) ; can do it "manually" if desired
    {0 10
     1 11
     2 12
     3 13}))


(dotest
  (let [vals [-3.14 -2 0 2 3.14]]
    (is= [false false false true false] (mapv t/int-pos? vals))
    (is= [false true false false false] (mapv t/int-neg? vals))
    (is= [false false true true false] (mapv t/int-nonneg? vals))
    (is= [false true true false false] (mapv t/int-nonpos? vals))
    (is= [false false true true true] (mapv t/nonneg? vals))
    (is= [true true true false false] (mapv t/nonpos? vals))))

(dotest
  (let [inf-rng-1 (map inc (range))
        tst-map   (t/glue (sorted-map) {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})
        tst-set   (t/glue (sorted-set) #{3 2 1})]

    (throws? (t/xtake 1 []))
    (is= [1] (t/xtake 1 [1]))
    (is= [1] (t/xtake 1 [1 2]))
    (is= [1] (t/xtake 1 inf-rng-1))
    (is= [1 2] (t/xtake 2 [1 2]))
    (is= [1 2] (t/xtake 2 inf-rng-1))
    (is= {:a 1} (t/xtake 1 tst-map))
    (is= {:a 1 :b 2} (t/xtake 2 tst-map))
    (is= #{1 2} (t/xtake 2 tst-set))

    (throws? (t/xdrop 1 []))
    (throws? (t/xdrop 2 [1]))
    (is= [] (t/xdrop 1 [1]))
    (is= [2] (t/xdrop 1 [1 2]))
    (is= [] (t/xdrop 2 [1 2]))
    (is= [3] (t/xdrop 2 [1 2 3]))
    (is= {:b 2 :c 3 :d 4 :e 5 :f 6} (t/xdrop 1 tst-map))
    (is= {:c 3 :d 4 :e 5 :f 6} (t/xdrop 2 tst-map))
    (is= {} (t/xdrop 6 tst-map))
    (throws? (t/xdrop 7 tst-map))
    (is= #{2 3} (t/xdrop 1 tst-set))

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
    (is= [1 2] (t/xvec [1 2]))))

(dotest
  (let [inf-rng-1 (map inc (range))]
    (is= nil (t/first-or-nil []))
    (is= 1 (t/first-or-nil [1]))
    (is= 1 (t/first-or-nil [1 2]))
    (is= 1 (t/first-or-nil inf-rng-1))

    (is= nil (t/second-or-nil []))
    (is= nil (t/second-or-nil [1]))
    (is= 2 (t/second-or-nil [1 2]))
    (is= 2 (t/second-or-nil [1 2 3]))
    (is= 2 (t/second-or-nil [1 2 3 4]))
    (is= 2 (t/second-or-nil inf-rng-1))

    (is= nil (t/third-or-nil []))
    (is= nil (t/third-or-nil [1]))
    (is= nil (t/third-or-nil [1 2]))
    (is= 3 (t/third-or-nil [1 2 3]))
    (is= 3 (t/third-or-nil [1 2 3 4]))
    (is= 3 (t/third-or-nil inf-rng-1))

    (is= nil (t/fourth-or-nil []))
    (is= nil (t/fourth-or-nil [1]))
    (is= nil (t/fourth-or-nil [1 2]))
    (is= nil (t/fourth-or-nil [1 2 3]))
    (is= 4 (t/fourth-or-nil [1 2 3 4]))
    (is= 4 (t/fourth-or-nil [1 2 3 4 5]))
    (is= 4 (t/fourth-or-nil inf-rng-1))

    (is= nil (t/last-or-nil nil))
    (is= nil (t/last-or-nil []))
    (is= 3 (t/last-or-nil [1 2 3]))

    (is= [] (rest nil) (t/rest-or-empty nil))
    (is= [] (rest []) (t/rest-or-empty []))
    (is= [] (rest [1]) (t/rest-or-empty [1]))
    (is= [2] (rest [1 2]) (t/rest-or-empty [1 2]))
    (is= [2 3] (rest [1 2 3]) (t/rest-or-empty [1 2 3]))
    (is= [2 3 4] (rest [1 2 3 4]) (t/rest-or-empty [1 2 3 4]))
    (is= [2 3 4] (take 3 (rest inf-rng-1)) (take 3 (t/rest-or-empty inf-rng-1)))

    (is= nil (next nil) (t/rest-or-nil nil))
    (is= nil (next []) (t/rest-or-nil []))
    (is= nil (next [1]) (t/rest-or-nil [1]))
    (is= [2] (next [1 2]) (t/rest-or-nil [1 2]))
    (is= [2 3] (next [1 2 3]) (t/rest-or-nil [1 2 3]))
    (is= [2 3 4] (next [1 2 3 4]) (t/rest-or-nil [1 2 3 4]))
    (is= [2 3 4] (take 3 (next inf-rng-1)) (take 3 (t/rest-or-nil inf-rng-1)))))

(dotest
  (let [data {:a 1}]
    (is= 1 (t/get-or-nil data :a))
    (is= nil (t/get-or-nil data :x))
    (is= 1 (t/get-or-default data :a 666))
    (is= 666 (t/get-or-default data :x 666)))
  (let [data [0 1 2 3]]
    (is= 1 (t/get-or-nil data 1))
    (is= nil (t/get-or-nil data 9))
    (is= 1 (t/get-or-default data 1 666))
    (is= 666 (t/get-or-default data 9 666))))

(dotest
  (is= :23 (t/int->kw 23))
  (is= 23 (t/kw->int :23))

  (is= {:a 1 :b 2} (t/json->edn (ts/quotes->double "{'a':1, 'b':2}")))
  (is= "{'a':1,'b':2}" (ts/quotes->single (t/edn->json {:a 1 :b 2})))
  (is= {:a 1 :b 2} (-> {:a 1 :b 2} (t/edn->json) (t/json->edn)))

  (is= 'abc (t/kw->sym :abc))
  (is= "abc" (t/kw->str :abc))
  (is= 'abc (t/str->sym "abc"))
  (is= :abc (t/str->kw "abc"))
  (is= :abc (t/sym->kw 'abc))
  (is= "abc" (t/sym->str 'abc)))

(dotest
  (do
    (is= :abc (t/->kw (quote abc)))
    (is= :abc (t/->kw :abc))
    (is= :abc (t/->kw "abc"))
    (is= :123 (t/->kw 123))
    (is= :12.3 (t/->kw 12.3)))
  (do
    (is= "abc" (t/->str (quote abc)))
    (is= "abc" (t/->str :abc))
    (is= "abc" (t/->str "abc"))
    (is= "123" (t/->str 123))
    (is= "12.3" (t/->str 12.3)))

  (do
    (is= (t/str->sym "abc") (t/->sym (quote abc)))
    (is= (t/str->sym "abc") (t/->sym :abc))
    (is= (t/str->sym "abc") (t/->sym "abc"))))

(dotest
  (let [orig     {:b #{3 2 1}
                  :a [1 2 3 {5 :five 6 :six 4 :four}]
                  :c (list 4 5 6)}
        result   (str/replace
                   (with-out-str (println (t/prettify orig)))
                   \, \space)
        expected "{:a  [1 2    3 {4 :four
                                  5 :five
                                  6 :six}]
                   :b #{1 2 3}
                   :c  [4 5 6]} "]
    (is-nonblank= result expected)))

(dotest
  (let [data [true :a 'my-symbol 1 "hello" \x false nil]]
    (testing "basic usage"
      (let [truthies (t/keep-if boolean data) ; coerce to primitive type
            falsies  (t/keep-if not data)] ; unnatural syntax
        (is (and (= truthies [true :a 'my-symbol 1 "hello" \x])
              (= falsies [false nil]))))
      (let [truthies (t/keep-if t/truthy? data)
            falsies  (t/keep-if t/falsey? data)]
        (is (and (= truthies [true :a 'my-symbol 1 "hello" \x])
              (= falsies [false nil])))
        (is (every? t/truthy? [true :a 'my-symbol 1 "hello" \x]))
        (is (every? t/falsey? [false nil]))
        (is (t/has-none? t/falsey? truthies))
        (is (t/has-none? t/truthy? falsies))

        (isnt (every? t/truthy? [true false]))
        (is (every? t/truthy? [true "FALSE"]))
        (is (every? t/truthy? [true]))
        (is (every? t/truthy? []))))

    (testing "improved usage"
      (let [count-if (comp count t/keep-if)]
        (let [num-true  (count-if boolean data) ; awkward phrasing
              num-false (count-if not data)] ; doesn't feel natural
          (is (and (= 6 num-true)
                (= 2 num-false))))
        (let [num-true  (count-if t/truthy? data) ; matches intent much better
              num-false (count-if t/falsey? data)]
          (is (and (= 6 num-true)
                (= 2 num-false)))))))

  (let [data [true :a 'my-symbol 1 "hello" \x false nil]]
    (testing "basic usage"
      (let [notties (t/keep-if t/not-nil? data)
            nillies (t/drop-if t/not-nil? data)]
        (is (and (= notties [true :a 'my-symbol 1 "hello" \x false])
              (= nillies [nil])))
        (is (every? t/not-nil? notties))
        (is (every? nil? [nil]))
        (is (t/has-none? nil? notties))
        (is (t/has-none? t/not-nil? nillies))))

    (testing "improved usage"
      (let [count-if (comp count t/keep-if)]
        (let [num-valid-1 (count-if some? data) ; awkward phrasing, doesn't feel natural
              num-valid-2 (count-if t/not-nil? data) ; matches intent much better
              num-nil     (count-if nil? data)] ; intent is plain
          (is (and (= 7 num-valid-1 num-valid-2)
                (= 1 num-nil))))))))


(dotest
  (is= true (t/has-some? odd? [1 2 3]))
  (is= false (t/has-some? odd? [2 4 6]))
  (is= false (t/has-some? odd? []))

  (is= false (t/has-none? odd? [1 2 3]))
  (is= true (t/has-none? odd? [2 4 6]))
  (is= true (t/has-none? odd? [])))

(dotest
  (is (every? t/not-empty? ["one" [1] '(1) {:1 1} #{1}]))
  (is (t/has-none? t/not-empty? ["" [] '() {} #{} nil]))

  (is (t/has-none? empty? ["one" [1] '(1) {:1 1} #{1}]))
  (is (every? empty? ["" [] '() {} #{} nil]))

  (is= (map t/not-empty? ["1" [1] '(1) {:1 1} #{1}])
    [true true true true true])
  (is= (map t/not-empty? ["" [] '() {} #{} nil])
    [false false false false false false])

  (is= (t/keep-if t/not-empty? ["1" [1] '(1) {:1 1} #{1}])
    ["1" [1] '(1) {:1 1} #{1}])
  (is= (t/drop-if t/not-empty? ["" [] '() {} #{} nil])
    ["" [] '() {} #{} nil])

  (throws? (t/not-empty? 5))
  (throws? (t/not-empty? 3.14)))

;-----------------------------------------------------------------------------
; spy stuff
(dotest
  (is= "(+ 2 3) => 5"
    (ts/collapse-whitespace
      (with-out-str
        (is= 5 (spyx (+ 2 3))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace "(inc 0) => 1
                                  (inc 1) => 2
                                  (inc 2) => 3 ")
    (ts/collapse-whitespace
      (with-out-str
        (is= 3 (spyx (inc 0)
                 (inc 1)
                 (inc 2))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace ":some-kw
                                  (inc 1) => 2
                                  (inc 2) => 3 ")
    (ts/collapse-whitespace
      (with-out-str
        (is= 3 (spyx :some-kw
                 (inc 1)
                 (inc 2)))))))

; #todo blog about this nested (is= ...) testing technique
(dotest
  (is=
    (ts/collapse-whitespace " a => 1
                               b => 5
                               (-> (inc a) (* 2) inc) => 5 ")
    (ts/collapse-whitespace
      (with-out-str
        (is= 13
          (t/let-spy [a (inc 0)
                      b (+ 2 3)]
                     (spyx (-> (inc a) (* 2) inc))
            (-> b (* 2) (+ 3)))))))

  (is= (ts/collapse-whitespace " a => 1
                                  b => 5 ")
    (ts/collapse-whitespace
      (with-out-str
        (is= 17
          (t/let-spy [a (inc 0)
                      b (+ 2 3)]
                     (-> b (* (inc a)) (+ 7))))))))

(dotest
  (testing "basic usage"
    (let [side-effect-cum-sum (atom 0) ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!    (fn [& args]
                                (let [result (apply + args)]
                                  (swap! side-effect-cum-sum + result)
                                  result))]
      (is= ":hi => 5"
        (ts/collapse-whitespace (with-out-str (spy (side-effect-add! 2 3) :hi))))
      (is= ":hi => 5"
        (ts/collapse-whitespace (with-out-str (spy :hi (side-effect-add! 2 3)))))
      (is= ":after-inc => 2"
        (ts/collapse-whitespace (with-out-str (-> 1
                                                (inc)
                                                (spy :after-inc) ; add a custom keyword message
                                                (* 2)))))
      (is= ":after-inc => 2"
        (ts/collapse-whitespace (with-out-str (->> 1
                                                (inc)
                                                (spy :after-inc) ; add a custom keyword message
                                                (* 2)))))

      (is= "(side-effect-add! 2 3) => 5"
        (ts/collapse-whitespace (with-out-str (spyx (side-effect-add! 2 3)))))
      (is= 15 @side-effect-cum-sum))

    (is= ":first => 5 :second => 25"
      (ts/collapse-whitespace
        (with-out-str (-> 2
                        (+ 3)
                        (spy :first)
                        (* 5)
                        (spy :second)))))
    (is= ":first => 5 :second => 25"
      (ts/collapse-whitespace
        (with-out-str (->> 2
                        (+ 3)
                        (spy :first)
                        (* 5)
                        (spy :second)))))

    (let [side-effect-cum-sum (atom 0) ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!    (fn [& args]
                                (let [result (apply + args)]
                                  (swap! side-effect-cum-sum + result)
                                  result))
          ]
      (is= ":value => 5"
        (ts/collapse-whitespace (with-out-str (spy (side-effect-add! 2 3) :value))))
      (is= ":value => 5"
        (ts/collapse-whitespace (with-out-str (spy :value (side-effect-add! 2 3)))))
      (is= 10 @side-effect-cum-sum)

      (is= ":value => 5" (ts/collapse-whitespace (with-out-str (spy :value (+ 2 3)))))
      (is= ":spy => 5" (ts/collapse-whitespace (with-out-str (spy (+ 2 3)))))

      (is= "(str \"abc\" \"def\") => \"abcdef\""
        (ts/collapse-whitespace (with-out-str (spyx (str "abc" "def")))))

      ; (throws? (spy :some-tag "some-str" 42))  ; #todo how test in cljs?
      )))

(dotest
  (let [fn2 (fn [] (t/with-spy-indent
                     (spy :msg2 (+ 2 3))))
        fn1 (fn [] (t/with-spy-indent
                     (spy :msg1 (+ 2 3))
                     (fn2)))
        fn0 (fn [] (spy :msg0 (+ 2 3)))]
    (is= ":msg2 => 5" (ts/collapse-whitespace (with-out-str (fn2))))
    (is= ":msg1 => 5 :msg2 => 5" (ts/collapse-whitespace (with-out-str (fn1))))
    (is= ":msg0 => 5" (ts/collapse-whitespace (with-out-str (fn0))))))

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
      (is (t/contains-elem? coll 0))
      (is (t/contains-elem? coll 1))
      (is (t/contains-elem? coll 2))
      (isnt (t/contains-elem? coll 3))
      (isnt (t/contains-elem? coll nil)))

    (let [coll [1 :two "three" \4]]
      (isnt (t/contains-elem? coll :no-way))
      (isnt (t/contains-elem? coll nil))
      (is (t/contains-elem? coll 1))
      (is (t/contains-elem? coll :two))
      (is (t/contains-elem? coll "three"))
      (is (t/contains-elem? coll \4)))

    (let [coll [:yes nil 3]]
      (isnt (t/contains-elem? coll :no-way))
      (is (t/contains-elem? coll :yes))
      (is (t/contains-elem? coll nil))))

  (testing "maps"
    (let [coll {1 :two "three" \4}]
      (isnt (t/contains-elem? coll nil))
      (isnt (t/contains-elem? coll [1 :no-way]))
      (is (t/contains-elem? coll [1 :two]))
      (is (t/contains-elem? coll ["three" \4])))
    (let [coll {1 nil "three" \4}]
      (isnt (t/contains-elem? coll [nil 1]))
      (is (t/contains-elem? coll [1 nil])))
    (let [coll {nil 2 "three" \4}]
      (isnt (t/contains-elem? coll [1 nil]))
      (is (t/contains-elem? coll [nil 2]))))

  (testing "sets"
    (let [coll #{1 :two "three" \4}]
      (isnt (t/contains-elem? coll :no-way))
      (is (t/contains-elem? coll 1))
      (is (t/contains-elem? coll :two))
      (is (t/contains-elem? coll "three"))
      (is (t/contains-elem? coll \4)))

    (let [coll #{:yes nil}]
      (isnt (t/contains-elem? coll :no-way))
      (is (t/contains-elem? coll :yes))
      (is (t/contains-elem? coll nil)))))

(dotest
  (is (t/contains-key? {:a 1 :b 2} :a))
  (is (t/contains-key? {:a 1 :b 2} :b))
  (isnt (t/contains-key? {:a 1 :b 2} :x))
  (isnt (t/contains-key? {:a 1 :b 2} :c))
  (isnt (t/contains-key? {:a 1 :b 2} 1))
  (isnt (t/contains-key? {:a 1 :b 2} 2))

  (is (t/contains-key? {:a 1 nil 2} nil))
  (isnt (t/contains-key? {:a 1 :b nil} nil))
  (isnt (t/contains-key? {:a 1 :b 2} nil))

  (is (t/contains-key? #{:a 1 :b 2} :a))
  (is (t/contains-key? #{:a 1 :b 2} :b))
  (is (t/contains-key? #{:a 1 :b 2} 1))
  (is (t/contains-key? #{:a 1 :b 2} 2))
  (isnt (t/contains-key? #{:a 1 :b 2} :x))
  (isnt (t/contains-key? #{:a 1 :b 2} :c))

  (is (t/contains-key? #{:a 5 nil "hello"} nil))
  (isnt (t/contains-key? #{:a 5 :doh! "hello"} nil))

  (throws? (t/contains-key? [:a 1 :b 2] :a))
  (throws? (t/contains-key? [:a 1 :b 2] 1)))

(dotest
  (is (t/contains-val? {:a 1 :b 2} 1))
  (is (t/contains-val? {:a 1 :b 2} 2))
  (isnt (t/contains-val? {:a 1 :b 2} 0))
  (isnt (t/contains-val? {:a 1 :b 2} 3))
  (isnt (t/contains-val? {:a 1 :b 2} :a))
  (isnt (t/contains-val? {:a 1 :b 2} :b))

  (is (t/contains-val? {:a 1 :b nil} nil))
  (isnt (t/contains-val? {:a 1 nil 2} nil))
  (isnt (t/contains-val? {:a 1 :b 2} nil))

  (throws? (t/contains-val? [:a 1 :b 2] 1))
  (throws? (t/contains-val? #{:a 1 :b 2} 1)))

(dotest
  (is=
    (t/forv [x (range 4)] (* x x))
    (t/for-list [x (range 4)] (* x x))
    [0 1 4 9])
  (is=
    (t/forv [x (range 23)] (* x x))
    (t/for-list [x (range 23)] (* x x))
    (for [x (range 23)] (* x x)))
  (is=
    (t/forv [x (range 5) y (range 2 9)] (str x y))
    (t/for-list [x (range 5) y (range 2 9)] (str x y))
    (for [x (range 5) y (range 2 9)] (str x y))))

(dotest
  (is= (t/for-indexed [[i x] [:a :b :c]]
         ; (println (format "i=%d x=%s" i x)) ; uncomment to print to stdout :wa
         {:i i :x x})
    [{:i 0 :x :a}
     {:i 1 :x :b}
     {:i 2 :x :c}]))

(dotest
  (let [xs [1 2 3]
        ys [10 20 30]]
    (is= [11 22 33]
      (t/map-let [x xs y ys] (+ x y))
      (t/map-let* {:lazy false :strict false} [x xs y ys] (+ x y))
      (t/map-let* {:lazy false :strict true} [x xs y ys] (+ x y))
      (t/map-let* {:lazy true :strict false} [x xs y ys] (+ x y))
      (t/map-let* {:lazy true :strict true} [x xs y ys] (+ x y)))
    (let [result-vec     (t/map-let* {:lazy false :strict true} [x xs y ys] (+ x y))
          result-lazyseq (t/map-let* {:lazy true :strict true} [result-vec xs y ys] (+ result-vec y))]
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
    (throws? (t/map-let [x xs y ys] (+ x y)))
    (throws? (t/map-let* {:strict true} [x xs y ys] (+ x y)))
    (is= [11 22 33] (t/map-let* {:strict false} [x xs y ys] (+ x y)))

    (throws? (t/xmap inc))
    (is= [1 2 3] (t/xmap inc (range 3)))
    (is= [0 2 4] (t/xmap (fn [x y] (+ x y)) (range 3) (range 3)))
    (throws? (t/xmap (fn [x y] (+ x y)) (range 3) (range 4)))))

(dotest
  (is= (vector []) [[]])
  (is= (mapv identity [] []) [])

  (is= [[:a 0] [:b 1] [:c 2]]
    (t/zip-lazy [:a :b :c] [0 1 2])
    (t/zip-lazy [:a :b :c] (range)))
  (is= (t/zip-lazy [:a :b :c] [1 2 3]) [[:a 1] [:b 2] [:c 3]])
  (is= (t/zip-lazy [:a] [1]) [[:a 1]])
  (is= (t/zip-lazy [] []) [])
  (is= (t/zip-lazy [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]])
  (is (instance?
        #?(:clj clojure.lang.LazySeq)
        #?(:cljs cljs.core/LazySeq)
        (t/zip-lazy [:a :b :c] (range))))

  (is= (t/zip [:a :b :c] [1 2 3]) [[:a 1] [:b 2] [:c 3]]) ; #todo fails when use Schema for append/prepend
  (is= (t/zip [:a] [1]) [[:a 1]]) ; #todo fails when use Schema for append/prepend
  (is= (t/zip [] []) [])
  (is= (t/zip [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]])
  (throws? (t/zip [:a :b :c] [1 2 3 4]))
  (is= (t/zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]])

  (is (instance?
        #?(:clj clojure.lang.PersistentVector)
        #?(:cljs cljs.core/PersistentVector)
        (t/zip* {:trunc false} [:a :b :c] [1 2 3])))
  (let [keys   [:a :b :c]
        vals   [1 2 3]
        result (atom [])]
    (doseq [[k i] (t/zip keys vals)]
      (swap! result t/append {k i}))
    (is= [{:a 1} {:b 2} {:c 3}] @result))

  ; verify that zip throws if unequal lengths, even if some colls are infinite lazy seqs
  (throws? (t/zip [:a :b :c] [1 2 3 4]))
  (throws? (t/zip [:A :B :C] [:a :b :c] [1 2 3 4]))
  (throws? (t/zip [:a :b :c] (range)))
  (is= (t/zip* {:strict false} [:a :b :c] (range)) [[:a 0] [:b 1] [:c 2]])
  (is= (t/zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]])
  (is= (t/zip* {:strict false} [:A :B :C] [:a :b :c] [1 2 3 4]) [[:A :a 1] [:B :b 2] [:C :c 3]]))

(dotest
  (is= (t/indexed [:a :b :c]) [[0 :a] [1 :b] [2 :c]])
  (is= [[0 0] [1 2] [2 4] [3 6] [4 8]]
    (take 5 (t/indexed (map #(* 2 %) (range))))) ; can work with infinite lazy lists
  (is= (t/indexed [:a :b :c] (map #(+ 10 %) (range)))
    [[0 :a 10]
     [1 :b 11]
     [2 :c 12]])
  (is= (take 5 (t/indexed (map #(+ 10 %) (range))))
    [[0 10]
     [1 11]
     [2 12]
     [3 13]
     [4 14]]))

(dotest
  ; unexpected results
  (is (= (concat {:a 1} {:b 2} {:c 3})
        [[:a 1] [:b 2] [:c 3]]))
  (is (= (conj [1 2] [3 4])
        [1 2 [3 4]]))

  (let [objs [[] '() {} (sorted-map) #{} (sorted-set)]]
    (is= (map sequential? objs) [true true false false false false])
    (is= (map map? objs) [false false true true false false])
    (is= (map set? objs) [false false false false true true]))

  (is= (t/glue [1 2] [3 4] [5 6]) [1 2 3 4 5 6])
  (is= (t/glue [1 2] '(3 4) [5 6]) [1 2 3 4 5 6])
  (is= (t/glue [] [1 2]) [1 2])
  (is= (t/glue [1 2] []) [1 2])
  (is= (t/glue [] [1 2] []) [1 2])

  (is= (t/glue '(1 2) '(3 4) '(5 6)) [1 2 3 4 5 6])
  (is= (t/glue '(1 2) [3 4] '(5 6)) [1 2 3 4 5 6])
  (is= (t/glue [1 2] '(3 4) '(5 6)) [1 2 3 4 5 6])
  (is= (t/glue '() '(1 2)) [1 2])
  (is= (t/glue '(1 2) '()) [1 2])
  (is= (t/glue '() '(1 2) '()) [1 2])

  (is= (t/glue (range 3) (range 5)) [0 1 2 0 1 2 3 4])

  (is= (t/glue {:a 1} {:b 2} {:c 3}) {:a 1 :c 3 :b 2})
  (is= (t/glue {:a 1} {:b 2}) {:a 1 :b 2})
  (is= (t/glue {:a 1} {}) {:a 1})
  (is= (t/glue {} {:a 1}) {:a 1})
  (is= (t/glue {} {:a 1} {}) {:a 1})

  (is= (t/glue #{1 2} #{3 4} #{6 5}) #{1 2 6 5 3 4})
  (is= (t/glue #{} #{1 2}) #{1 2})
  (is= (t/glue #{1 2} #{}) #{1 2})
  (is= (t/glue #{} #{1 2} #{}) #{1 2})

  (is= (t/glue (sorted-map) {:a 1} {:b 2} {:c 3}) {:a 1 :b 2 :c 3})
  (is= (t/glue (sorted-set) #{1 2} #{3 4} #{6 5}) #{1 2 3 4 5 6})

  (is= (t/glue (sorted-map) (hash-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6))
    {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})
  (is= (seq (t/glue (sorted-map) (hash-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6)))
    [[:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6]])

  (is= (t/glue {:band :VanHalen :singer :Dave} {:singer :Sammy})
    {:band :VanHalen :singer :Sammy})

  (is= (t/glue \a) "a")
  (is= (t/glue "a") "a")
  (is= (t/glue \a "") "a")
  (is= (t/glue "" "a") "a")
  (is= (t/glue \a \b) "ab")
  (is= (t/glue "a" "b") "ab")
  (is= (t/glue "a" \b) "ab")
  (is= (t/glue \a "b") "ab")
  (is= (t/glue "" "a" \b) "ab")
  (is= (t/glue "" \a "b") "ab")
  (is= (t/glue "a" "" \b) "ab")
  (is= (t/glue \a "" "b") "ab")
  (is= (t/glue "a" \b "") "ab")
  (is= (t/glue \a "b" "") "ab")
  (is= (t/glue \a "b" "") "ab")
  (is= (t/glue "I" \space "like " \a " nap!") "I like a nap!")
  (is= (apply t/glue ["I" \space "like " \a " nap!"]) "I like a nap!")

  (throws? (t/glue [1 2] {:a 1}))
  (throws? (t/glue '(1 2) {:a 1}))
  (throws? (t/glue [1 2] #{:a 1}))
  (throws? (t/glue '(1 2) #{:a 1}))
  (throws? (t/glue [1 2] "hello"))
  (throws? (t/glue '(1 2) "hello"))
  (throws? (t/glue {:a 1} #{:a 1}))
  (throws? (t/glue {:a 1} "hello"))
  (throws? (t/glue #{:a 1} "hello"))
  (throws? (t/glue [1 2] nil))
  (throws? (t/glue '(1 2) nil))
  (throws? (t/glue {:a 1} nil))
  (throws? (t/glue #{:a 1} nil))
  (throws? (t/glue "hello" nil)))

;-----------------------------------------------------------------------------
; #todo get strange cljs compiler errors if combine these 2 into a single #?(:clj ...)
#?(:clj
   (dotest
     (try
       (throw (Exception. "Boom!"))
       (catch Exception ex
         (is= "Boom!" (spyx (t/exception-message ex)))
         (let [strace (t/exception-stacktrace ex)]
           (is (str/starts-with? strace "java.lang.Exception"))
           (is (ts/contains-str? strace "Boom!"))
           (is (ts/contains-str? strace "tst.tupelo.core")))))))

#?(:clj   ; #todo get cljs compiler strange errors if `?` is missing in '#?(:clj ...)'
   (dotest
     (let [zz (byte-array 0)
           aa (byte-array 1 (byte 1))
           bb (byte-array 2 (byte 2))
           cc (byte-array 3 (byte 3))
           dd (byte-array 4 (byte 4))]
       (is= [] (vec (t/glue zz)))
       (is= [1] (vec (t/glue aa)))
       (is= [1] (vec (t/glue zz aa)))
       (is= [1] (vec (t/glue aa zz)))
       (is= [1 1] (vec (t/glue aa aa)))
       (is= [1 1] (vec (t/glue aa zz aa)))
       (is= [1 2 2] (vec (t/glue aa bb)))
       (is= [1 2 2] (vec (t/glue zz aa bb zz)))
       (is= [1 2 2 3 3 3] (vec (t/glue aa bb cc)))
       (is= [1 2 2 3 3 3 4 4 4 4] (vec (t/glue aa bb cc dd)))
       (is= [1 2 2 3 3 3 4 4 4 4] (vec (t/glue aa bb cc dd zz)))
       (is= [1 2 2 3 3 3 4 4 4 4] (vec (t/glue zz aa bb zz cc dd)))
       (is (types/byte-array? (t/glue aa bb cc dd))))))
;-----------------------------------------------------------------------------


(dotest
  (let [data [[0 1 2]
              []
              [3]
              [4 5]
              [6 7 8 9]]]
    (is= (t/thru 9) (t/glue-rows data))
    (is= (t/thru 9) (reduce into [] data))))

(dotest
  (throws? (t/append 1 2))
  (throws? (t/append [1 2]))
  (throws? (t/append nil 3))
  (is= [1 2 3] (t/append [1 2] 3))
  (is= [1 2 3 4] (t/append [1 2] 3 4))
  (is= [1 2 3 4 5] (t/append [1 2] 3 4 5))

  (throws? (t/append '(1 2)))
  (is= [1 2 3] (t/append '(1 2) 3))
  (is= [1 2 3 4] (t/append '(1 2) 3 4))
  (is= [1 2 3 4 5] (t/append '(1 2) 3 4 5))

  (throws? (t/append {:a 1} 99))
  (throws? (t/append {:a 1} {:b 2}))
  (throws? (t/append #{:a 1} 99))
  (throws? (t/append #{:a 1} #{99}))

  (testing "old conjv tests"
    (is= [2] (t/append [] 2))
    (is= [2] (t/append '() 2))
    (is= [2 3] (t/append [] 2 3))
    (is= [2 3] (t/append '() 2 3))

    (is= [1 2 3] (t/append [1] 2 3))
    (is= [1 2 3] (t/append '(1) 2 3))
    (is= [1 2 3] (t/append [1 2] 3))
    (is= [1 2 3] (t/append '(1 2) 3))

    (is= [1 2 3 4] (t/append [1 2] 3 4))
    (is= [1 2 3 4] (t/append '(1 2) 3 4))
    (is= [1 2 3 4] (t/append [1] 2 3 4))
    (is= [1 2 3 4] (t/append '(1) 2 3 4))

    (is= [[1 2] [3 4] [5 6]] (t/append [[1 2] [3 4]] [5 6]))

    (is= [0 1 2 3 4 5] (t/append (range 4) 4 5))
    (is= [0 1 2 3 4 5] (apply t/append [0] (range 1 6)))))

(dotest
  (throws? (t/prepend [2 1]))
  (throws? (t/prepend 3 nil))
  (is= [3 2 1] (t/prepend 3 [2 1]))
  (is= [4 3 2 1] (t/prepend 4 3 [2 1]))
  (is= [5 4 3 2 1] (t/prepend 5 4 3 [2 1]))

  (throws? (t/prepend '(2 1)))
  (is= [3 2 1] (t/prepend 3 '(2 1)))
  (is= [4 3 2 1] (t/prepend 4 3 '(2 1)))
  (is= [5 4 3 2 1] (t/prepend 5 4 3 '(2 1)))

  (throws? (t/prepend 99 {:a 1}))
  (throws? (t/prepend {:b 2} {:a 1}))
  (throws? (t/prepend 99 #{:a 1}))
  (throws? (t/prepend #{99} #{:a 1})))

(dotest
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 2 3 4 5 6 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 3 4 5 6 7 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 (t/unwrap [3 4 5 6 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 (t/unwrap [3 (t/unwrap [4 5 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 (t/unwrap [3 (t/unwrap [4 (t/unwrap [5]) 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 2 3 (t/unwrap [4 5 6]) 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (t/->vector 1 (t/unwrap [2 3 (t/unwrap [4 5 6]) 7 8]) 9))

  (is= [1 2 3 [4 5 6] 7 8 9] (t/->vector 1 (t/unwrap [2 3 [4 5 6] 7 8]) 9))
  (is= [1 2 3 [4 [5] 6] 7 8 9] (t/->vector 1 (t/unwrap [2 3 [4 [5] 6] 7 8]) 9))

  (is= [1 [2 3 4 [5] 6 7 8] 9] (t/->vector 1 `(2 3 ~(t/unwrap [4 [5] 6]) 7 8) 9))
  (is= [1 [2 3 4 [5] 6 7 8] 9] (t/->vector 1 [2 3 (t/unwrap [4 [5] 6]) 7 8] 9))

  (is= [1 2 3 4 5 6 7 8 9] (t/glue [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (t/glue [1 2 3] [4 5 6] [7 8 9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat [1 2 3] [4 5 6] [7 8 9])))

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
  (isnt (t/increasing? [\a \b] [\a]))
  (isnt (t/increasing? [\a \b] [\a \a]))
  (isnt (t/increasing? [\a \b] [\a \b]))
  (is (t/increasing? [\a \b] [\a \b nil]))
  (is (t/increasing? [\a \b] [\a \b \c]))
  (is (t/increasing? [\a \b] [\a \c]))
  (is (t/increasing? [\a \b] [\b \a]))
  (is (t/increasing? [\a \b] [\b]))

  (isnt (t/increasing-or-equal? [\a \b] [\a]))
  (isnt (t/increasing-or-equal? [\a \b] [\a \a]))
  (is (t/increasing-or-equal? [\a \b] [\a \b]))
  (is (t/increasing-or-equal? [\a \b] [\a \b nil]))
  (is (t/increasing-or-equal? [\a \b] [\a \b \c]))
  (is (t/increasing-or-equal? [\a \b] [\a \c]))
  (is (t/increasing-or-equal? [\a \b] [\b \a]))
  (is (t/increasing-or-equal? [\a \b] [\b])))

(dotest
  (isnt (t/increasing? [:a :b] [:a]))
  (isnt (t/increasing? [:a :b] [:a :a]))
  (isnt (t/increasing? [:a :b] [:a :b]))
  (is (t/increasing? [:a :b] [:a :b nil]))
  (is (t/increasing? [:a :b] [:a :b :c]))
  (is (t/increasing? [:a :b] [:a :c]))
  (is (t/increasing? [:a :b] [:b :a]))
  (is (t/increasing? [:a :b] [:b]))

  (isnt (t/increasing-or-equal? [:a :b] [:a]))
  (isnt (t/increasing-or-equal? [:a :b] [:a :a]))
  (is (t/increasing-or-equal? [:a :b] [:a :b]))
  (is (t/increasing-or-equal? [:a :b] [:a :b nil]))
  (is (t/increasing-or-equal? [:a :b] [:a :b :c]))
  (is (t/increasing-or-equal? [:a :b] [:a :c]))
  (is (t/increasing-or-equal? [:a :b] [:b :a]))
  (is (t/increasing-or-equal? [:a :b] [:b])))

(dotest
  (isnt (t/increasing? ["a" "b"] ["a"]))
  (isnt (t/increasing? ["a" "b"] ["a" "a"]))
  (isnt (t/increasing? ["a" "b"] ["a" "b"]))
  (is (t/increasing? ["a" "b"] ["a" "b" nil]))
  (is (t/increasing? ["a" "b"] ["a" "b" "c"]))
  (is (t/increasing? ["a" "b"] ["a" "c"]))
  (is (t/increasing? ["a" "b"] ["b" "a"]))
  (is (t/increasing? ["a" "b"] ["b"]))

  (isnt (t/increasing-or-equal? ["a" "b"] ["a"]))
  (isnt (t/increasing-or-equal? ["a" "b"] ["a" "a"]))
  (is (t/increasing-or-equal? ["a" "b"] ["a" "b"]))
  (is (t/increasing-or-equal? ["a" "b"] ["a" "b" nil]))
  (is (t/increasing-or-equal? ["a" "b"] ["a" "b" "c"]))
  (is (t/increasing-or-equal? ["a" "b"] ["a" "c"]))
  (is (t/increasing-or-equal? ["a" "b"] ["b" "a"]))
  (is (t/increasing-or-equal? ["a" "b"] ["b"])))


(dotest
  (let [map1 {:a   1 :b 2 :c nil
              nil  :NIL
              "hi" "hello"
              5    "five"}]
    (is= 1 (t/grab :a map1))
    (is= 2 (t/grab :b map1))
    (is= nil (t/grab :c map1))
    (is= :NIL (t/grab nil map1))
    (is= "hello" (t/grab "hi" map1))
    (is= "five" (t/grab 5 map1))
    (throws? (t/grab :z map1))
    (throws? (t/grab 42 map1))
    ))

(dotest
  (testing "basic usage"
    (let [map1 {:a1  "a1"
                :a2  {:b1 "b1"
                      :b2 {:c1 "c1"
                           :c2 "c2"}}
                nil  "NIL"
                :nil nil}
          vv   [[:00 :01 :02]
                [:10 :11 :12]]]
      (is= "a1" (t/fetch-in map1 [:a1]))
      (is= "b1" (t/fetch-in map1 [:a2 :b1]))
      (is= "c1" (t/fetch-in map1 [:a2 :b2 :c1]))
      (is= "c2" (t/fetch-in map1 [:a2 :b2 :c2]))
      (is= "NIL" (t/fetch-in map1 [nil]))
      (is= nil (t/fetch-in map1 [:nil]))
      (throws? (t/fetch-in map1 [:a9]))
      (throws? (t/fetch-in map1 [:a2 :b9]))
      (throws? (t/fetch-in map1 [:a2 :b2 :c9]))
      (is= :12 (t/fetch-in vv [1 2]))
      )))

(dotest
  (let [mm {:a {:b {:c "c"}}}]
    (is= (t/dissoc-in mm []) mm)
    (is= (t/dissoc-in mm [:a]) {})
    (is= (t/dissoc-in mm [:a :b]) {:a {}})
    (is= (t/dissoc-in mm [:a :b :c]) {:a {:b {}}})
    (is= (t/dissoc-in mm [:a :x :y]) {:a {:b {:c "c"}
                                          :x nil}})
    (is= (t/dissoc-in mm [:a :x :y :z]) {:a {:b {:c "c"}
                                             :x {:y nil}}})
    (is= (t/dissoc-in mm [:k1 :k2 :k3 :kz]) {:a  {:b {:c "c"}}
                                             :k1 {:k2 {:k3 nil}}}))
  (let [mm {:a1 "a1"
            :a2 {:b1 "b1"
                 :b2 {:c1 "c1"
                      :c2 "c2"}}}]
    (is= (t/dissoc-in mm [:a1])
      {:a2 {:b1 "b1"
            :b2 {:c1 "c1"
                 :c2 "c2"}}})
    (is= (t/dissoc-in mm [:a2])
      {:a1 "a1"})
    (is= (t/dissoc-in mm [:a2 :b1])
      {:a1 "a1"
       :a2 {:b2 {:c1 "c1"
                 :c2 "c2"}}})
    (is= (t/dissoc-in mm [:a2 :b2])
      {:a1 "a1"
       :a2 {:b1 "b1"}})
    (is= (t/dissoc-in mm [:a2 :b2 :c1])
      {:a1 "a1"
       :a2 {:b1 "b1"
            :b2 {:c2 "c2"}}})
    (is= (t/dissoc-in mm [:a2 :b2 :c2])
      {:a1 "a1"
       :a2 {:b1 "b1"
            :b2 {:c1 "c1"}}})))

(dotest
  (t/try-catchall
    (throw (ex-info "some-big-error" {:answer 43}))
    (catch e ; (println "Caught exception:" e)
           (is= (ex-data e) {:answer 43})))

  (let [result (t/with-exception-default :some-val
                 (throw (ex-info "some-big-error" {:answer 43})))]
    (is= result :some-val)))

(dotest
  (is= 42 (t/with-result 42
            (with-out-str
              (for [i (range 3)]
                (println i))))))

(dotest
  (let [map1 {:a 1 :b 2 :c 3 :d 4 :e 5}]
    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{:a :b :c :d :e}))
    (is= {:b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{:b :c :d :e}))
    (is= {:c 3 :d 4 :e 5} (t/submap-by-keys map1 #{:c :d :e}))
    (is= {:d 4 :e 5} (t/submap-by-keys map1 #{:d :e}))
    (is= {:e 5} (t/submap-by-keys map1 #{:e}))
    (is= {} (t/submap-by-keys map1 #{}))
    (throws? (t/submap-by-keys map1 #{:z}))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{:a :b :c :d :e :z} :missing-ok))
    (is= {:b 2 :c 3 :d 4 :e 5} (t/submap-by-keys map1 #{:b :c :d :e :z} :missing-ok))
    (is= {:c 3 :d 4 :e 5} (t/submap-by-keys map1 #{:c :d :e :z} :missing-ok))
    (is= {:d 4 :e 5} (t/submap-by-keys map1 #{:d :e :z} :missing-ok))
    (is= {:e 5} (t/submap-by-keys map1 #{:e :z} :missing-ok))
    (is= {} (t/submap-by-keys map1 #{:z} :missing-ok))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{1 2 3 4 5}))
    (is= {:b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{2 3 4 5}))
    (is= {:c 3 :d 4 :e 5} (t/submap-by-vals map1 #{3 4 5}))
    (is= {:d 4 :e 5} (t/submap-by-vals map1 #{4 5}))
    (is= {:e 5} (t/submap-by-vals map1 #{5}))
    (is= {} (t/submap-by-vals map1 #{}))
    (throws? (t/submap-by-vals map1 #{99}))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{1 2 3 4 5 99} :missing-ok))
    (is= {:b 2 :c 3 :d 4 :e 5} (t/submap-by-vals map1 #{2 3 4 5 99} :missing-ok))
    (is= {:c 3 :d 4 :e 5} (t/submap-by-vals map1 #{3 4 5 99} :missing-ok))
    (is= {:d 4 :e 5} (t/submap-by-vals map1 #{4 5 99} :missing-ok))
    (is= {:e 5} (t/submap-by-vals map1 #{5 99} :missing-ok))
    (is= {} (t/submap-by-vals map1 #{99} :missing-ok))
    (is= {} (t/submap-by-vals map1 #{} :missing-ok))

    (is= {0 :even 2 :even} (t/submap-by-vals
                             {0 :even 1 :odd 2 :even 3 :odd}
                             #{:even}))
    (is= {0 :even 2 :even} (t/submap-by-vals
                             {0 :even 1 :odd 2 :even 3 :odd}
                             #{:even :prime} :missing-ok))))

(dotest
  (is= 3 (t/validate pos? 3))
  (is= 3.14 (t/validate number? 3.14))
  (is= 3.14 (t/validate #(< 3 % 4) 3.14))
  (is= [0 1 2] (t/validate vector? (vec (range 3))))
  (is= nil (t/validate nil? (next [])))
  (is= [0 1 2] (t/validate #(= 3 (count %)) [0 1 2]))
  (throws? (t/validate number? "hello"))
  (throws? (t/validate t/truthy? nil)))

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
  (let [m1 {:a 1 :b 2 :c 3}]
    (is= [:a 1 :b 2] (t/keyvals-seq m1 [:a :b]))
    (is= [:b 2 :a 1] (t/keyvals-seq m1 [:b :a]))
    (is= [:a 1 :b 2 :c 3] (t/keyvals-seq m1 [:a :b :c]))
    (is= [:c 3 :a 1 :b 2] (t/keyvals-seq m1 [:c :a :b]))
    (is= [:c 3 :b 2 :a 1] (t/keyvals-seq m1 [:c :b :a]))
    (is= [:a 1 :b 2 :a 1] (t/keyvals-seq m1 [:a :b :a]))

    (throws? (t/keyvals-seq m1 [:a :b :z]))
    (is= [:a 1 :b 2] (t/keyvals-seq {:missing-ok true
                                     :the-map    m1 :the-keys [:a :b :z]}))
    (is= [:b 2 :c 3] (t/keyvals-seq {:missing-ok true
                                     :the-map    m1 :the-keys [:z :b :c]}))))

(dotest
  (is= 2 (t/it-> 1
           (inc it)
           (+ 3 it)
           (/ 10 it)))
  (let [mm {:a {:b 2}}]
    (is= (t/it-> mm (:a it)) {:b 2})
    (is= (t/it-> mm (it :a) (:b it)) 2))
  (is= 48 (t/it-> 42
            (let [x 5]
              (+ x it))
            (inc it))))

(dotest
  (let [params {:a 1 :b 1 :c nil :d nil}]
    (is= (t/cond-it-> params
           (:a it) (update it :b inc)
           (= (:b it) 2) (assoc it :c "here")
           (= "here" (:c it)) (assoc it :d "again"))
      {:a 1, :b 2, :c "here", :d "again"}))

  (let [params {:a nil :b 1 :c nil :d nil}]
    (is= (t/cond-it-> params
           (:a it) (update it :b inc)
           (= (:b it) 1) (assoc it :c "here")
           (= "here" (:c it)) (assoc it :d "again"))
      {:a nil, :b 1, :c "here", :d "again"}))

  (let [params {:a 1 :b 1 :c nil :d nil}]
    (is= (t/cond-it-> params
           (:a it) (update it :b inc)
           (= (:b it) 2) (update it :b inc)
           (:c it) (assoc it :d "again"))
      {:a 1, :b 3, :c nil :d nil})))

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
              (+ 2 it)))))

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
  (is (t/rel= 1 1 :digits 4))
  (is (t/rel= 1 1 :tol 0.01))

  (throws? (t/rel= 1 1))
  (throws? (t/rel= 1 1 4))
  (throws? (t/rel= 1 1 :xxdigits 4))
  (throws? (t/rel= 1 1 :digits 4.1))
  (throws? (t/rel= 1 1 :digits 0))
  (throws? (t/rel= 1 1 :digits -4))
  (throws? (t/rel= 1 1 :tol -0.01))
  (throws? (t/rel= 1 1 :tol "xx"))
  (throws? (t/rel= 1 1 :xxtol 0.01))

  (is (t/rel= 0 0 :digits 3))
  (is (t/rel= 42 42 :digits 99))
  (is (t/rel= 42 42.0 :digits 99))

  (is (t/rel= 1 1.001 :digits 3))
  (is (not (t/rel= 1 1.001 :digits 4)))
  (is (t/rel= 123450000 123456789 :digits 4))
  (is (not (t/rel= 123450000 123456789 :digits 6)))
  (is (t/rel= 0.123450000 0.123456789 :digits 4))
  (is (not (t/rel= 0.123450000 0.123456789 :digits 6)))

  (is (t/rel= 1 1.001 :tol 0.01))
  (is (not (t/rel= 1 1.001 :tol 0.0001))))

(dotest
  (is (every? t/truthy? (t/forv [ul (range 0 4)] (vector? (t/range-vec ul)))))

  (is (every? t/truthy? (t/forv [ul (range 0 4)] (= (t/range-vec ul) (range ul)))))

  (is (every? t/truthy? (t/forv [lb (range 0 4)
                                 ub (range lb 4)]
                          (= (t/range-vec lb ub) (range lb ub))))))

(dotest
  (testing "positive step"
    (is= [0] (t/thru 0))
    (is= [0 1] (t/thru 1))
    (is= [0 1 2] (t/thru 2))
    (is= [0 1 2 3] (t/thru 3))

    (is= [0] (t/thru 0 0))
    (is= [0 1] (t/thru 0 1))
    (is= [0 1 2] (t/thru 0 2))
    (is= [0 1 2 3] (t/thru 0 3))

    (is= [] (t/thru 1 0))
    (is= [1] (t/thru 1 1))
    (is= [1 2] (t/thru 1 2))
    (is= [1 2 3] (t/thru 1 3))

    (is= [] (t/thru 2 0))
    (is= [] (t/thru 2 1))
    (is= [2] (t/thru 2 2))
    (is= [2 3] (t/thru 2 3))

    (is= [] (t/thru 3 0))
    (is= [] (t/thru 3 1))
    (is= [] (t/thru 3 2))
    (is= [3] (t/thru 3 3))

    (is= [] (t/thru 4 0))
    (is= [] (t/thru 4 1))
    (is= [] (t/thru 4 2))
    (is= [] (t/thru 4 3))


    (is= [0] (t/thru 0 0 1))
    (is= [0 1] (t/thru 0 1 1))
    (is= [0 1 2] (t/thru 0 2 1))
    (is= [0 1 2 3] (t/thru 0 3 1))

    (is= [] (t/thru 1 0 1))
    (is= [1] (t/thru 1 1 1))
    (is= [1 2] (t/thru 1 2 1))
    (is= [1 2 3] (t/thru 1 3 1))

    (is= [] (t/thru 2 0 1))
    (is= [] (t/thru 2 1 1))
    (is= [2] (t/thru 2 2 1))
    (is= [2 3] (t/thru 2 3 1))

    (is= [] (t/thru 3 0 1))
    (is= [] (t/thru 3 1 1))
    (is= [] (t/thru 3 2 1))
    (is= [3] (t/thru 3 3 1))

    (is= [] (t/thru 4 0 1))
    (is= [] (t/thru 4 1 1))
    (is= [] (t/thru 4 2 1))
    (is= [] (t/thru 4 3 1))


    (is= [0] (t/thru 0 0 2))
    (throws? (t/thru 0 1 2))
    (is= [0 2] (t/thru 0 2 2))
    (throws? (t/thru 0 3 2))

    (throws? (t/thru 1 0 2))
    (is= [1] (t/thru 1 1 2))
    (throws? (t/thru 1 2 2))
    (is= [1 3] (t/thru 1 3 2))

    (is= [] (t/thru 2 0 2))
    (throws? (t/thru 2 1 2))
    (is= [2] (t/thru 2 2 2))
    (throws? (t/thru 2 3 2))

    (throws? (t/thru 3 0 2))
    (is= [] (t/thru 3 1 2))
    (throws? (t/thru 3 2 2))
    (is= [3] (t/thru 3 3 2))


    (is= [0] (t/thru 0 0 3))
    (throws? (t/thru 0 1 3))
    (throws? (t/thru 0 2 3))
    (is= [0 3] (t/thru 0 3 3))

    (throws? (t/thru 1 0 3))
    (is= [1] (t/thru 1 1 3))
    (throws? (t/thru 1 2 3))
    (throws? (t/thru 1 3 3))

    (throws? (t/thru 2 0 3))
    (throws? (t/thru 2 1 3))
    (is= [2] (t/thru 2 2 3))
    (throws? (t/thru 2 3 3))

    (is= [] (t/thru 3 0 3))
    (throws? (t/thru 3 1 3))
    (throws? (t/thru 3 2 3))
    (is= [3] (t/thru 3 3 3)))
  (testing "negative step"
    (is= [0] (t/thru 0 0 -1))
    (is= [1 0] (t/thru 1 0 -1))
    (is= [2 1 0] (t/thru 2 0 -1))
    (is= [3 2 1 0] (t/thru 3 0 -1))

    (is= [] (t/thru 0 1 -1))
    (is= [1] (t/thru 1 1 -1))
    (is= [2 1] (t/thru 2 1 -1))
    (is= [3 2 1] (t/thru 3 1 -1))

    (is= [] (t/thru 0 2 -1))
    (is= [] (t/thru 1 2 -1))
    (is= [2] (t/thru 2 2 -1))
    (is= [3 2] (t/thru 3 2 -1))

    (is= [] (t/thru 0 3 -1))
    (is= [] (t/thru 1 3 -1))
    (is= [] (t/thru 2 3 -1))
    (is= [3] (t/thru 3 3 -1))


    (is= [0] (t/thru 0 0 -2))
    (throws? (t/thru 1 0 -2))
    (is= [2 0] (t/thru 2 0 -2))
    (throws? (t/thru 3 0 -2))

    (throws? (t/thru 0 1 -2))
    (is= [1] (t/thru 1 1 -2))
    (throws? (t/thru 2 1 -2))
    (is= [3 1] (t/thru 3 1 -2))

    (is= [] (t/thru 0 2 -2))
    (throws? (t/thru 1 2 -2))
    (is= [2] (t/thru 2 2 -2))
    (throws? (t/thru 3 2 -2))

    (throws? (t/thru 0 3 -2))
    (is= [] (t/thru 1 3 -2))
    (throws? (t/thru 2 3 -2))
    (is= [3] (t/thru 3 3 -2))


    (is= [0] (t/thru 0 0 -3))
    (throws? (t/thru 1 0 -3))
    (throws? (t/thru 2 0 -3))
    (is= [3 0] (t/thru 3 0 -3))

    (throws? (t/thru 0 1 -3))
    (is= [1] (t/thru 1 1 -3))
    (throws? (t/thru 2 1 -3))
    (throws? (t/thru 3 1 -3))

    (throws? (t/thru 0 2 -3))
    (throws? (t/thru 1 2 -3))
    (is= [2] (t/thru 2 2 -3))
    (throws? (t/thru 3 2 -3))

    (is= [] (t/thru 0 3 -3))
    (throws? (t/thru 1 3 -3))
    (throws? (t/thru 2 3 -3))
    (is= [3] (t/thru 3 3 -3)))
  (testing "combinations"
    (is= [0 2 4 6 8 10] (t/thru 0 10 2))
    (is= [0 -2 -4 -6 -8 -10] (t/thru 0 -10 -2))
    (is= [2 4 6 8 10] (t/thru 2 10 2))
    (is= [-2 -4 -6 -8 -10] (t/thru -2 -10 -2))
    (is= [2 0 -2 -4 -6 -8 -10] (t/thru 2 -10 -2))
    (is= [-2 0 2 4 6 8 10] (t/thru -2 10 2))

    (is= [10 8 6 4 2 0] (t/thru 10 0 -2))
    (is= [-10 -8 -6 -4 -2 0] (t/thru -10 0 2))
    (is= [10 8 6 4 2] (t/thru 10 2 -2))
    (is= [-10 -8 -6 -4 -2] (t/thru -10 -2 2))
    (is= [10 8 6 4 2 0 -2] (t/thru 10 -2 -2))
    (is= [-10 -8 -6 -4 -2 0 2] (t/thru -10 2 2))

    (is= [0 5 10] (t/thru 0 10 5))
    (is= [0 -5 -10] (t/thru 0 -10 -5))
    (is= [5 10] (t/thru 5 10 5))
    (is= [-5 -10] (t/thru -5 -10 -5))
    (is= [5 0 -5 -10] (t/thru 5 -10 -5))
    (is= [-5 0 5 10] (t/thru -5 10 5))

    (is= [10 5 0] (t/thru 10 0 -5))
    (is= [-10 -5 0] (t/thru -10 0 5))
    (is= [10 5] (t/thru 10 5 -5))
    (is= [-10 -5] (t/thru -10 -5 5))
    (is= [10 5 0 -5] (t/thru 10 -5 -5))
    (is= [-10 -5 0 5] (t/thru -10 5 5)))
  (testing "floats"
    (is (t/all-rel= [1.1 1.3 1.5 1.7] (t/thru 1.1 1.7 0.2) :digits 7))
    (is (t/all-rel= [1.1 1.2 1.3 1.4] (t/thru 1.1 1.4 0.1) :digits 7)))
  (throws? (t/thru 1.1 2.1 0.3)))

(dotest
  (is= [0 2 4 6 8] (t/keep-if even? (range 10))
    (t/drop-if odd? (range 10)))
  (is= [1 3 5 7 9] (t/keep-if odd? (range 10))
    (t/drop-if even? (range 10)))

  ; If we supply a 2-arg fn when filtering a sequence, we get an Exception (CLJ only)
  #?(:clj (throws? (t/keep-if (fn [arg1 arg2] :dummy) #{1 2 3})))

  ; Verify throw if coll is not a sequential, map, or set.
  (throws? (t/keep-if t/truthy? 2))
  (throws? (t/keep-if t/truthy? :some-kw)))

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
    #?(:clj (throws? (t/keep-if (fn [arg] :dummy) {:a 1}))))
  (let [s1 (into (sorted-set) (range 10))]
    (is= #{0 2 4 6 8} (t/keep-if even? s1)
      (t/drop-if odd? s1))
    (is= #{1 3 5 7 9} (t/keep-if odd? s1)
      (t/drop-if even? s1))

    ; If we supply a 2-arg fn when filtering a set, we get an Exception
    #?(:clj (throws? (t/keep-if (fn [arg1 arg2] :dummy) #{1 2 3})))))

#?(:cljs
   (dotest ; in JS a char is just a single-char string
     (is= "a" \a (t/codepoint->char 97))
     (is= 97 (t/char->codepoint "a") (t/char->codepoint \a))
     (is= [\a \b \c] (vec "abc"))
     (is= [97 98 99] (mapv t/char->codepoint (t/str->chars "abc")))))

(dotest
  (is= "a" (t/strcat \a) (t/strcat [\a]))
  (is= "a" (t/strcat "a") (t/strcat ["a"]))
  (is= "a" (t/strcat 97) (t/strcat [97]))

  (is= "ab" (t/strcat \a \b) (t/strcat [\a] \b))
  (is= "ab" (t/strcat \a [\b]) (t/strcat [\a \b]))
  (is= "ab" (t/strcat "a" "b") (t/strcat ["a"] "b"))
  (is= "ab" (t/strcat "a" ["b"]) (t/strcat ["a" "b"]))
  (is= "ab" (t/strcat 97 98) (t/strcat [97] 98))
  (is= "ab" (t/strcat 97 [98]) (t/strcat [97 98]))
  (is= "ab" (t/strcat "" "ab") (t/strcat ["" \a "b"]))

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
    (is (= "" (t/clip-str 0 "abcdefg")))
    (is (= "a" (t/clip-str 1 "abcdefg")))
    (is (= "ab" (t/clip-str 2 "abcdefg")))
    (is (= "abc" (t/clip-str 3 "abcdefg")))
    (is (= "abcd" (t/clip-str 4 "abcdefg")))
    (is (= "abcde" (t/clip-str 5 "abcdefg"))))
  (testing "two strings"
    (is (= "" (t/clip-str 0 "abc defg")))
    (is (= "a" (t/clip-str 1 "abc defg")))
    (is (= "ab" (t/clip-str 2 "abc defg")))
    (is (= "abc" (t/clip-str 3 "abc defg")))
    (is (= "abc " (t/clip-str 4 "abc defg")))
    (is (= "abc d" (t/clip-str 5 "abc defg"))))
  (testing "two strings & char"
    (is (= "" (t/clip-str 0 "ab" \c "defg")))
    (is (= "a" (t/clip-str 1 "ab" \c "defg")))
    (is (= "ab" (t/clip-str 2 "ab" \c "defg")))
    (is (= "abc" (t/clip-str 3 "ab" \c "defg")))
    (is (= "abcd" (t/clip-str 4 "ab" \c "defg")))
    (is (= "abcde" (t/clip-str 5 "ab" \c "defg"))))
  (testing "two strings & digit"
    (is (= "" (t/clip-str 0 "ab" 9 "defg")))
    (is (= "a" (t/clip-str 1 "ab" 9 "defg")))
    (is (= "ab" (t/clip-str 2 "ab" 9 "defg")))
    (is (= "ab9" (t/clip-str 3 "ab" 9 "defg")))
    (is (= "ab9d" (t/clip-str 4 "ab" 9 "defg")))
    (is (= "ab9de" (t/clip-str 5 "ab" 9 "defg"))))
  (testing "vector"
    (is (= "" (t/clip-str 0 [1 2 3 4 5])))
    (is (= "[" (t/clip-str 1 [1 2 3 4 5])))
    (is (= "[1" (t/clip-str 2 [1 2 3 4 5])))
    (is (= "[1 2" (t/clip-str 4 [1 2 3 4 5])))
    (is (= "[1 2 3 4" (t/clip-str 8 [1 2 3 4 5])))
    (is (= "[1 2 3 4 5]" (t/clip-str 16 [1 2 3 4 5]))))
  (testing "map"
    (is (= "" (t/clip-str 0 (sorted-map :a 1 :b 2))))
    (is (= "{" (t/clip-str 1 (sorted-map :a 1 :b 2))))
    (is (= "{:" (t/clip-str 2 (sorted-map :a 1 :b 2))))
    (is (= "{:a " (t/clip-str 4 (sorted-map :a 1 :b 2))))
    (is (= "{:a 1, :" (t/clip-str 8 (sorted-map :a 1 :b 2))))
    (is (= "{:a 1, :b 2}" (t/clip-str 16 (sorted-map :a 1 :b 2)))))
  (testing "set"
    (let [tst-set (sorted-set 5 4 3 2 1)]
      (is (= "" (t/clip-str 0 tst-set)))
      (is (= "#" (t/clip-str 1 tst-set)))
      (is (= "#{" (t/clip-str 2 tst-set)))
      (is (= "#{1 " (t/clip-str 4 tst-set)))
      (is (= "#{1 2 3 " (t/clip-str 8 tst-set)))
      (is (= "#{1 2 3 4 5}" (t/clip-str 16 tst-set))))))

(dotest
  (is= [] (t/drop-at (range 1) 0))

  (is= [1] (t/drop-at (range 2) 0))
  (is= [0] (t/drop-at (range 2) 1))

  (is= [1 2] (t/drop-at (range 3) 0))
  (is= [0 2] (t/drop-at (range 3) 1))
  (is= [0 1] (t/drop-at (range 3) 2))

  (throws? (t/drop-at [] 0))
  (throws? (t/drop-at (range 3) -1))
  (throws? (t/drop-at (range 3) 3)))

(dotest
  (is= [9] (t/insert-at [] 0 9))

  (is= [9 0] (t/insert-at [0] 0 9))
  (is= [0 9] (t/insert-at [0] 1 9))

  (is= [9 0 1] (t/insert-at [0 1] 0 9))
  (is= [0 9 1] (t/insert-at [0 1] 1 9))
  (is= [0 1 9] (t/insert-at [0 1] 2 9))

  (throws? (t/insert-at [] -1 9))
  (throws? (t/insert-at [] 1 9))

  (throws? (t/insert-at [0] -1 9))
  (throws? (t/insert-at [0] 2 9))

  (throws? (t/insert-at [0 1] -1 9))
  (throws? (t/insert-at [0 1] 3 9)))

(dotest
  (is= [9] (t/replace-at (range 1) 0 9))

  (is= [9 1] (t/replace-at (range 2) 0 9))
  (is= [0 9] (t/replace-at (range 2) 1 9))

  (is= [9 1 2] (t/replace-at (range 3) 0 9))
  (is= [0 9 2] (t/replace-at (range 3) 1 9))
  (is= [0 1 9] (t/replace-at (range 3) 2 9))

  (throws? (t/replace-at [] 0 9))
  (throws? (t/replace-at (range 3) -1 9))
  (throws? (t/replace-at (range 3) 3 9)))

(dotest   ; #todo need more tests
  (is= (mapv #(mod % 3) (t/thru -6 6)) [0 1 2 0 1 2 0 1 2 0 1 2 0])
  (is= (mapv #(t/idx [0 1 2] %) (t/thru -3 3)) [0 1 2 0 1 2 0]))

(dotest
  (is (= [\a] (t/chars-thru \a \a)))
  (is (= [\a \b] (t/chars-thru \a \b)))
  (is (= [\a \b \c] (t/chars-thru \a \c)))

  (is (= [\a] (t/chars-thru 97 97)))
  (is (= [\a \b] (t/chars-thru 97 98)))
  (is (= [\a \b \c] (t/chars-thru 97 99)))

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
  (is (t/starts-with? (range 0 3) (range 0 0)))

  (is (t/starts-with? (range 0 3) (range 0 1)))
  (is (t/starts-with? (range 0 3) (range 0 2)))
  (is (t/starts-with? (range 0 3) (range 0 3)))

  (isnt (t/starts-with? (range 0 3) (range 1 2)))
  (isnt (t/starts-with? (range 0 3) (range 1 3)))

  (isnt (t/starts-with? (range 0 3) (range 2 3)))

  (isnt (t/starts-with? (range 1 3) (range 0 1)))
  (isnt (t/starts-with? (range 1 3) (range 0 2)))
  (isnt (t/starts-with? (range 1 3) (range 0 3)))

  (is (t/starts-with? (range 1 3) (range 1 2)))
  (is (t/starts-with? (range 1 3) (range 1 3)))

  (isnt (t/starts-with? (range 1 3) (range 2 3)))

  (isnt (t/starts-with? (range 2 3) (range 0 1)))
  (isnt (t/starts-with? (range 2 3) (range 0 2)))
  (isnt (t/starts-with? (range 2 3) (range 0 3)))

  (isnt (t/starts-with? (range 2 3) (range 1 2)))
  (isnt (t/starts-with? (range 2 3) (range 1 3)))

  (is (t/starts-with? (range 2 3) (range 2 3)))

  (isnt (t/starts-with? (range 3 3) (range 0 1)))
  (isnt (t/starts-with? (range 3 3) (range 0 2)))
  (isnt (t/starts-with? (range 3 3) (range 0 3))))

(defrecord SampleRec [a b])
(dotest
  (let [sr1 (->SampleRec 1 2)]
    (is (map? sr1))
    (is (t/val= sr1 {:a 1 :b 2}))
    (is (t/val= 1 1))
    (is (t/val= "abc" "abc"))
    (is (t/val= [1 2 3] [1 2 3]))
    (is (t/val= #{1 2 sr1} #{1 2 {:a 1 :b 2}}))
    (is (t/val= [1 2 3 #{1 2 sr1}] [1 2 3 #{1 2 {:a 1 :b 2}}]))))

(dotest
  (is (t/set= [1 2 3] [1 2 3]))
  (is (t/set= [1 2 3] [3 2 1]))
  (is-set= [1 2 3] [1 2 3])
  (is-set= [1 2 3] [3 2 1])
  )

(dotest
  (testing "fibo stuff"
    (is= (take 0 (t/fibonacci-seq)) [])
    (is= (take 5 (t/fibonacci-seq)) [0 1 1 2 3])
    (is= (take 10 (t/fibonacci-seq)) [0 1 1 2 3 5 8 13 21 34])

    (is= (t/fibo-thru 0) [0])
    (is= (t/fibo-thru 1) [0 1 1])
    (is= (t/fibo-thru 2) [0 1 1 2])
    (is= (t/fibo-thru 3) [0 1 1 2 3])
    (is= (t/fibo-thru 4) [0 1 1 2 3])
    (is= (t/fibo-thru 5) [0 1 1 2 3 5])
    (is= (t/fibo-thru 6) [0 1 1 2 3 5])
    (is= (t/fibo-thru 7) [0 1 1 2 3 5])
    (is= (t/fibo-thru 8) [0 1 1 2 3 5 8])
    (is= (t/fibo-thru 34) [0 1 1 2 3 5 8 13 21 34])

    (is= 0 (t/fibo-nth 0))
    (is= 1 (t/fibo-nth 1))
    (is= 1 (t/fibo-nth 2))
    (is= 2 (t/fibo-nth 3))
    (is= 3 (t/fibo-nth 4))
    (is= 5 (t/fibo-nth 5))
    (is= 8 (t/fibo-nth 6))
    (is= 13 (t/fibo-nth 7))
    (is= 21 (t/fibo-nth 8))
    (is= 34 (t/fibo-nth 9))
    (is (< (Math/pow 2 62) (t/fibo-nth 91) (Math/pow 2 63)))))

;---------------------------------------------------------------------------------------------------
(dotest
  (is= [[] [\a \b \c \d \e \f]] (t/split-match "abcdef" "a"))
  (is= [[] [\a \b \c \d \e \f]] (t/split-match "abcdef" "ab"))
  (is= [[\a] [\b \c \d \e \f]] (t/split-match "abcdef" "bc"))
  (is= [[\a \b] [\c \d \e \f]] (t/split-match "abcdef" "cde"))
  (is= [[\a \b \c] [\d \e \f]] (t/split-match "abcdef" "de"))
  (is= [[\a \b \c \d] [\e \f]] (t/split-match "abcdef" "ef"))
  (is= [[\a \b \c \d \e] [\f]] (t/split-match "abcdef" "f"))
  (is= [[\a \b \c \d \e \f] []] (t/split-match "abcdef" "fg"))
  (is= [[\a \b \c \d \e \f] []] (t/split-match "abcdef" "gh"))

  (is= [[0 1 2 3 4 5] []] (t/split-match (range 6) [-1]))
  (is= [[] [0 1 2 3 4 5]] (t/split-match (range 6) [0]))
  (is= [[] [0 1 2 3 4 5]] (t/split-match (range 6) [0 1]))
  (is= [[0 1] [2 3 4 5]] (t/split-match (range 6) [2 3]))
  (is= [[0 1 2] [3 4 5]] (t/split-match (range 6) [3 4 5]))
  (is= [[0 1 2 3] [4 5]] (t/split-match (range 6) [4 5]))
  (is= [[0 1 2 3 4] [5]] (t/split-match (range 6) [5]))
  (is= [[0 1 2 3 4 5] []] (t/split-match (range 6) [5 6]))
  (is= [[0 1 2 3 4 5] []] (t/split-match (range 6) [6 7])))

(dotest
  (is= nil (t/index-using #(= [666] %) (range 5)))
  (is= 0 (t/index-using #(= [0 1 2 3 4] %) (range 5)))
  (is= 1 (t/index-using #(= [1 2 3 4] %) (range 5)))
  (is= 2 (t/index-using #(= [2 3 4] %) (range 5)))
  (is= 3 (t/index-using #(= [3 4] %) (range 5)))
  (is= 4 (t/index-using #(= [4] %) (range 5)))
  (is= nil (t/index-using #(= [] %) (range 5))))

(dotest
  (is= [[] [0 1 2 3 4]] (t/split-using #(= 0 (first %)) (range 5)))
  (is= [[0] [1 2 3 4]] (t/split-using #(= 1 (first %)) (range 5)))
  (is= [[0 1] [2 3 4]] (t/split-using #(= 2 (first %)) (range 5)))
  (is= [[0 1 2] [3 4]] (t/split-using #(= 3 (first %)) (range 5)))
  (is= [[0 1 2 3] [4]] (t/split-using #(= 4 (first %)) (range 5)))
  (is= [[0 1 2 3 4] []] (t/split-using #(= 5 (first %)) (range 5)))
  (is= [[0 1 2 3 4] []] (t/split-using #(= 9 (first %)) (range 5)))

  (is= [[\a \b \c] [\d \e \f]] (t/split-using #(t/starts-with? % "de") "abcdef")))

(dotest
  (let [start-segment? (fn [vals] (zero? (rem (first vals) 3)))]
    (is= (t/partition-using start-segment? [1 2 3 6 7 8])
      [[1 2] [3] [6 7 8]])
    (is= (t/partition-using start-segment? [3 6 7 9])
      [[3] [6 7] [9]])
    (is= (t/partition-using start-segment? [1 2 3 6 7 8 9 12 13 15 16 17 18 18 18 3 4 5])
      [[1 2] [3] [6 7 8] [9] [12 13] [15 16 17] [18] [18] [18] [3 4 5]]))
  (throws? (t/partition-using even? 5)))

(dotest
  (let [items     [{:name :a :count 1}
                   {:name :b :count 2}
                   {:name :c :count 3}
                   {:name :d :count 4}
                   {:name :e :count 5}]
        sum-count (fn sum-count-fn [items] (reduce + (map :count items)))]
    (throws? (t/take-while-result #(<= (sum-count %) 0) []))

    (is= (t/take-while-result #(<= (sum-count %) -1) items) [])
    (is= (t/take-while-result #(<= (sum-count %) 0) items) [])
    (is= (t/take-while-result #(<= (sum-count %) 1) items)
      [{:name :a, :count 1}])
    (is= (t/take-while-result #(<= (sum-count %) 2) items)
      [{:name :a, :count 1}])
    (is= (t/take-while-result #(<= (sum-count %) 3) items)
      [{:name :a, :count 1} {:name :b, :count 2}])
    (is= (t/take-while-result #(<= (sum-count %) 4) items)
      [{:name :a, :count 1} {:name :b, :count 2}])
    (is= (t/take-while-result #(<= (sum-count %) 5) items)
      [{:name :a, :count 1} {:name :b, :count 2}])
    (is= (t/take-while-result #(<= (sum-count %) 6) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3}])
    (is= (t/take-while-result #(<= (sum-count %) 7) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3}])
    (is= (t/take-while-result #(<= (sum-count %) 8) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3}])
    (is= (t/take-while-result #(<= (sum-count %) 9) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3}])
    (is= (t/take-while-result #(<= (sum-count %) 10) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3} {:name :d, :count 4}])
    (is= (t/take-while-result #(<= (sum-count %) 11) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3} {:name :d, :count 4}])
    (is= (t/take-while-result #(<= (sum-count %) 14) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3} {:name :d, :count 4}])
    (is= (t/take-while-result #(<= (sum-count %) 15) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3} {:name :d, :count 4} {:name :e, :count 5}])
    (is= (t/take-while-result #(<= (sum-count %) 16) items)
      [{:name :a, :count 1} {:name :b, :count 2} {:name :c, :count 3} {:name :d, :count 4} {:name :e, :count 5}]))

  (let [cum-within (fn cum-within-fn
                     [items limit]
                     (<= (apply + items) limit))]
    (is= (t/take-while-result #(cum-within % -1) (range)) [])
    (is= (t/take-while-result #(cum-within % 0) (range)) [0])
    (is= (t/take-while-result #(cum-within % 1) (range)) [0 1])
    (is= (t/take-while-result #(cum-within % 2) (range)) [0 1])
    (is= (t/take-while-result #(cum-within % 3) (range)) [0 1 2])
    (is= (t/take-while-result #(cum-within % 4) (range)) [0 1 2])
    (is= (t/take-while-result #(cum-within % 5) (range)) [0 1 2])
    (is= (t/take-while-result #(cum-within % 6) (range)) [0 1 2 3])
    (is= (t/take-while-result #(cum-within % 7) (range)) [0 1 2 3])
    (is= (t/take-while-result #(cum-within % 8) (range)) [0 1 2 3])
    (is= (t/take-while-result #(cum-within % 9) (range)) [0 1 2 3])
    (is= (t/take-while-result #(cum-within % 10) (range)) [0 1 2 3 4])))

(dotest
  (let [data [[:a 1]
              [:a 2]
              [:a 3]
              [:b 1]
              [:b 2]
              [:b 3]]]
    (is= (t/distinct-using first data)
      [[:a 1] [:b 1]])
    (is= (t/distinct-using second data)
      [[:a 1] [:a 2] [:a 3]])
    (is= (t/distinct-using identity data)
      [[:a 1] [:a 2] [:a 3] [:b 1] [:b 2] [:b 3]])))

(dotest
  (let [ctx (let [a 1
                  b 2
                  c 3
                  d 4
                  e 5]
              (t/vals->map a b c d e))]
    (is= ctx {:a 1 :b 2 :c 3 :d 4 :e 5})

    (let [{:keys [a b c d e]} ctx]
      (is= [a b c d e] [1 2 3 4 5]))
    (t/with-map-vals ctx [a b c d e]
      (is= [a b c d e] [1 2 3 4 5])
      (is= 15 (+ a b c d e)))
    (t/with-map-vals ctx [b a d c e] ; order doesn't matter
      (is= [a b c d e] [1 2 3 4 5])
      (is= 15 (+ a b c d e)))
    (t/with-map-vals ctx [b a d] ; can ignore stuff you don't care about
      (is= [d a b] [4 1 2]))

    (throws?
      (t/with-map-vals ctx [a b z] ; thows if key doesn't exist
        (println "won't ever get here")))))

;---------------------------------------------------------------------------------------------------
; demo and poc for td/construct
(def a-1400 1401)
(def b-1400 1402)

(def c 3)
(dotest
  (let [env {:a 1 :b 2}]
    (t/with-map-vals env [a b]
      (is= {:likes {:a a :b b}} ; normal
        {:likes {:a 1, :b 2}})
      (is= (t/construct-impl (quote {:likes {:a ? :b ?}}))
        (quote {:likes {:a a, :b b}}))
      (is= (t/construct {:likes {:a ? :b ?}})
        {:likes {:a a, :b b}}
        {:likes {:a 1, :b 2}} )
      (is= (t/construct {:likes {:a ? :b ? :c ?}}) ; works for locals and Vars
        {:likes {:a 1, :b 2 :c 3}} ) )))

;---------------------------------------------------------------------------------------------------
(dotest
  (let [lazy-next-int (fn lazy-next-int [n]
                        (t/lazy-cons n (lazy-next-int (inc n))))
        all-ints      (lazy-next-int 0)
        ]
    (is= (take 0 all-ints) [])
    (is= (take 1 all-ints) [0])
    (is= (take 5 all-ints) [0 1 2 3 4]))

  (let [lazy-range (fn lazy-range
                     [limit]
                     (let [lazy-range-step (fn lazy-range-step [curr limit]
                                             (when (< curr limit)
                                               (t/lazy-cons curr (lazy-range-step (inc curr) limit))))]
                       (lazy-range-step 0 limit)))]
    (is= (lazy-range 0) nil)
    (is= (lazy-range 1) [0])
    (is= (lazy-range 5) [0 1 2 3 4]))

  (let [lazy-countdown
        (fn lazy-countdown [n]
          (when (<= 0 n)
            (t/lazy-cons n (lazy-countdown (dec n)))))]
    (is= (lazy-countdown 5) [5 4 3 2 1 0])
    (is= (lazy-countdown 1) [1 0])
    (is= (lazy-countdown 0) [0])
    (is= (lazy-countdown -1) nil)))

(dotest
  (let [N                  9
        nums               (range N)
        kws                (mapv #(-> % str keyword) nums)
        nums-cycle         (mapv #(rem % 3) nums)
        mes                (mapv #(hash-map :rem %) nums-cycle)
        stuff              (zipmap kws mes)
        stuff-sorted-asc-0 (t/sorted-map-via-path stuff [:* :rem])
        stuff-sorted-asc-1 (t/sorted-map-via-path stuff [:* :rem] true)
        stuff-sorted-desc  (t/sorted-map-via-path stuff [:* :rem] false)]
    (is= clojure.data.avl.AVLMap (type stuff-sorted-asc-0))
    (throws? (t/sorted-map-via-path stuff []))
    (throws? (t/sorted-map-via-path stuff [:*]))
    (throws? (t/sorted-map-via-path stuff [:invalid :rem]))
    (is= stuff {:0 {:rem 0},
                :4 {:rem 1},
                :7 {:rem 1},
                :1 {:rem 1},
                :8 {:rem 2},
                :2 {:rem 2},
                :5 {:rem 2},
                :3 {:rem 0},
                :6 {:rem 0}})
    (is= stuff stuff-sorted-asc-0)
    (is= stuff stuff-sorted-asc-1)
    (is= stuff stuff-sorted-desc)
    (is= (vec stuff-sorted-asc-0)
      [[:0 {:rem 0}]
       [:3 {:rem 0}]
       [:6 {:rem 0}]
       [:1 {:rem 1}]
       [:4 {:rem 1}]
       [:7 {:rem 1}]
       [:2 {:rem 2}]
       [:5 {:rem 2}]
       [:8 {:rem 2}]])
    (is= (vec stuff-sorted-asc-1)
      [[:0 {:rem 0}]
       [:3 {:rem 0}]
       [:6 {:rem 0}]
       [:1 {:rem 1}]
       [:4 {:rem 1}]
       [:7 {:rem 1}]
       [:2 {:rem 2}]
       [:5 {:rem 2}]
       [:8 {:rem 2}]])
    (is= (vec stuff-sorted-desc)
      [[:8 {:rem 2}]
       [:5 {:rem 2}]
       [:2 {:rem 2}]
       [:7 {:rem 1}]
       [:4 {:rem 1}]
       [:1 {:rem 1}]
       [:6 {:rem 0}]
       [:3 {:rem 0}]
       [:0 {:rem 0}]])
    (let [stuff2 (zipmap kws nums-cycle)]
      (is= stuff2 {:0 0, :4 1, :7 1, :1 1, :8 2, :2 2, :5 2, :3 0, :6 0})
      (is= (t/sorted-map-via-path stuff2 [:*])
        {:0 0, :3 0, :6 0, :1 1, :4 1, :7 1, :2 2, :5 2, :8 2})))
  )

(dotest   ; #todo #clojure.core/sorted-map-by bug
  (when false
    (let [unsorted {:x.y/a {:rem 0}
                    :x.y/b {:rem 1}}
          sorted   (into (sorted-map-by
                           (fn [k1 k2]
                             (println {:keys-seen [k1 k2]})
                             (compare k1 k2)))
                     unsorted)]
      (println :unsorted unsorted)
      (println :sorted sorted))))

(dotest
  (let [sample (t/->set [1 2 3])]
    (is (set? sample))
    (is= #{3 2 1} sample))
  (let [raw    (t/->set (range 100))
        sorted (t/->sorted-set raw)]
    (is (set? sorted))
    (is= raw sorted)
    (is (apply < (seq sorted))))
  (let [sorted-map?   (fn [it]
                        (and (map? it)
                          (let [keys-orig   (keys it)
                                keys-sorted (sort keys-orig)]
                            (= keys-orig keys-sorted))))
        nums          (shuffle (range 25))
        map-orig      (zipmap nums nums)
        map-sorted    (t/->sorted-map map-orig)
        nested-orig   {:a map-orig
                       :b [map-orig 2 3]}
        nested-sorted (t/walk-maps->sorted nested-orig)]
    (is (sorted-map? map-sorted))
    (is (sorted-map? (get-in nested-sorted [:a])))
    (isnt (sorted-map? (get-in nested-sorted [:b])))
    (is (sorted-map? (get-in nested-sorted [:b 0])))
    (is (sorted-map? nested-sorted))))

(dotest
  (let [intc {:enter (fn [parents data]
                       (t/with-result data
                         (spy :enter (t/vals->map parents data))))
              :leave (fn [parents data]
                       (t/with-result data
                         (spy :leave (t/vals->map parents data))))}]
    ; demo with map
    (let [data            {:a 1 :b {:c 3}}
          walk-result-str (with-out-str
                            (let [data-noop (t/walk-with-parents data intc)]
                              (is= data data-noop)))]
      ;(newline) (println walk-result-str) (newline)
      (is-nonblank= walk-result-str
        " :enter => {:parents [], :data {:a 1, :b {:c 3}}}
          :enter => {:parents [{:a 1, :b {:c 3}} [:a 1] {:type :map-key, :value :a}], :data :a}
          :leave => {:parents [{:a 1, :b {:c 3}} [:a 1] {:type :map-key, :value :a}], :data :a}
          :enter => {:parents [{:a 1, :b {:c 3}} [:a 1] {:type :map-val, :value 1}], :data 1}
          :leave => {:parents [{:a 1, :b {:c 3}} [:a 1] {:type :map-val, :value 1}], :data 1}
          :enter => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-key, :value :b}], :data :b}
          :leave => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-key, :value :b}], :data :b}
          :enter => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-val, :value {:c 3}}], :data {:c 3}}
          :enter => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-val, :value {:c 3}} {:c 3} [:c 3] {:type :map-key, :value :c}], :data :c}
          :leave => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-val, :value {:c 3}} {:c 3} [:c 3] {:type :map-key, :value :c}], :data :c}
          :enter => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-val, :value {:c 3}} {:c 3} [:c 3] {:type :map-val, :value 3}], :data 3}
          :leave => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-val, :value {:c 3}} {:c 3} [:c 3] {:type :map-val, :value 3}], :data 3}
          :leave => {:parents [{:a 1, :b {:c 3}} [:b {:c 3}] {:type :map-val, :value {:c 3}}], :data {:c 3}}
          :leave => {:parents [], :data {:a 1, :b {:c 3}}} "))))

(dotest
  (let [intc {:enter (fn [parents data]
                       (t/with-result data
                         (spy :enter (t/vals->map parents data))))}]
    ; demo with vectors
    (let [data            [10 [20 21]]
          walk-result-str (with-out-str
                            (let [data-noop (t/walk-with-parents data intc)]
                              (is= data data-noop)))]
      ; (newline) (println walk-result-str) (newline)
      (is-nonblank= walk-result-str
        " :enter => {:parents [], :data [10 [20 21]]}
          :enter => {:parents [[10 [20 21]] {:type :list-entry, :idx 0, :val 10}], :data 10}
          :enter => {:parents [[10 [20 21]] {:type :list-entry, :idx 1, :val [20 21]}], :data [20 21]}
          :enter => {:parents [[10 [20 21]] {:type :list-entry, :idx 1, :val [20 21]} [20 21] {:type :list-entry, :idx 0, :val 20}], :data 20}
          :enter => {:parents [[10 [20 21]] {:type :list-entry, :idx 1, :val [20 21]} [20 21] {:type :list-entry, :idx 1, :val 21}], :data 21} "))))

#?(:clj
   (dotest
     (let [data   {:a 1 :b [20 21 22] :c {:d 4}}
           intc   {:enter t/noop
                   :leave t/->nil}
           result (t/walk-with-parents-readonly data intc)] ; return values don't matter
       (throws? (t/walk-with-parents-readonly data {})) ; must have at least one of :enter or :leave

       (do ; #todo #bug 2020-2-6 this fails in CLJS
         ; (println :2113-before)
         ; (spyx result)
         (is= result data)
         ; (println :2113-after)
         ))

     ; walk-with-parents disallows any user-data MapEntry or ListEntry objects in input data structure
     (let [data {:a 1 :b (t/map-entry :c 3)}]
       (throws?
         (t/walk-with-parents data {:leave t/noop})))

     (let [le (t/list-entry 0 100)]
       (is= le {:type :list-entry, :idx 0, :val 100})
       (is (map? le))

       (isnt (sequential? le)) ; since a ListEntry passes (map? x), must process before plain maps in tupelo.core!!!

       (is= 0 (:idx le))
       (is= 100 (:val le)))

     (let [le (t/list-entry 5 6)
           ii (:idx le)
           vv (:val le)]
       (is= 5 ii)
       (is= 6 vv)
       (throws-not? (t/list-entry 0 6))
       (throws? (t/list-entry -1 6)))
     (let [data         [:a :b :c]
           list-entries (t/list->entries data)
           data-out     (t/list-entries->vec list-entries)]
       (is= data-out [:a :b :c])
       (is (every? t/list-entry? list-entries))
       (throws? (t/list-entries->vec (reverse list-entries))) ; data indexes must be in order  0..(N-1)
       )))

(dotest
  ; only increment numeric mapentry values when key is :c
  (let [data   {:a 1 :b {:c 3} 41 42}
        intc   {:leave (fn [parents data]
                         (t/with-nil-default data
                           (when (<= 2 (count parents))
                             (let [ancestors (reverse parents)
                                   a1        (t/xfirst ancestors)
                                   a2        (t/xsecond ancestors)]
                               (when (and (number? data)
                                       (= :map-val (:type a1))
                                       (map-entry? a2)
                                       (= :c (key a2)))
                                 (inc data))))))}
        result (t/walk-with-parents data intc)]
    (is= result {:a 1 :b {:c 4} 41 42})))

(dotest
  ; only increment numeric values at even index
  (let [data   [0 1 :two 3 4 5]
        intc   {:leave (fn [parents data]
                         (spyx (t/vals->map parents data))
                         (t/with-nil-default data
                           (when (number? data)
                             (let [parent (t/xlast parents)]
                               (when (and (t/list-entry? parent) (even? (t/grab :idx parent)))
                                 (inc data))))))}
        result (t/walk-with-parents data intc)]
    (is= result [1 1 :two 3 5 5])))

(dotest
  (is= (range 10) ; vector/list
    (t/unnest 0 1 2 3 4 5 6 7 8 9)
    (t/unnest 0 1 2 [3 [4] 5 6] 7 [8 9])
    (t/unnest [0 1 2 3 4 5 6 7 8 9])
    (t/unnest [0 [1 2 3 4 5 6 7 8 9]])
    (t/unnest [0 [1 [2 3 4 5 6 7 8 9]]])
    (t/unnest [0 [1 [2 [3 4 5 6 7 8 9]]]])
    (t/unnest [0 [1 [2 [3 [4 5 6 7 8 9]]]]])
    (t/unnest [0 [1 [2 [3 [4 [5 6 7 8 9]]]]]])
    (t/unnest [0 [1 [2 [3 [4 [5 [6 7 8 9]]]]]]])
    (t/unnest [0 [1 [2 [3 [4 [5 [6 [7 8 9]]]]]]]])
    (t/unnest [0 [1 [2 [3 [4 [5 [6 [7 [8 9]]]]]]]]])
    (t/unnest [0 [1 [2 [3 [4 [5 [6 [7 [8 [9]]]]]]]]]])
    (t/unnest [[[[[[[[[[0] 1] 2] 3] 4] 5] 6] 7] 8] 9])
    (t/unnest [0 1 [2 [3 [4] 5] 6] 7 8 9]))

  (is= [1 2 3 4 5] (t/unnest [[[1] 2] [3 [4 [5]]]]))

  (is= (set [:a :1 :b 2 :c 3]) ; map
    (set (t/unnest [:a :1 {:b 2 :c 3}]))
    (set (t/unnest [:a :1 {[:b] 2 #{3} :c}]))
    (set (t/unnest [:a :1 {:b 2 :c 3}]))
    (set (t/unnest [:a :1 {:b {:c [2 3]}}])))
  (is-set= #{1 2 3 4 5 6} (t/unnest {1 {2 {3 {4 {5 6}}}}}))

  (is= (set (range 10)) ; set
    (set (t/unnest #{0 1 2 3 4 5 6 7 8 9}))
    (set (t/unnest #{0 1 #{2 3 4 5 6 7 8} 9}))
    (set (t/unnest #{0 1 #{2 3 #{4 5 6} 7 8} 9})))
  (is-set= #{1 2 3 4 5 6} (t/unnest #{1 #{2 #{3 #{4 #{5 #{6}}}}}})))

;-----------------------------------------------------------------------------
; lazy-gen/yield tests

#?(:clj (do

          (dotest
            (let [empty-gen-fn (fn [] (t/lazy-gen))]
              (is (nil? (empty-gen-fn))))

            (let [range-gen (fn [limit] ; " A generator 'range' function. "
                              (t/lazy-gen
                                (loop [cnt 0]
                                  (when (< cnt limit)
                                    (assert (= cnt (t/yield cnt)))
                                    (recur (inc cnt))))))]

              (is= (range 1) (range-gen 1))
              (is= (range 5) (range-gen 5))
              (is= (range 10) (range-gen 10))

              ; Note different behavior for empty result
              (is= [] (range 0))
              (is= nil (range-gen 0))
              (is= (seq (range 0))
                (seq (range-gen 0))
                nil))

            (let [concat-gen        (fn [& collections]
                                      (t/lazy-gen
                                        (doseq [curr-coll collections]
                                          (doseq [item curr-coll]
                                            (t/yield item)))))
                  concat-gen-mirror (fn [& collections]
                                      (t/lazy-gen
                                        (doseq [curr-coll collections]
                                          (doseq [item curr-coll]
                                            (let [items [item (- item)]]
                                              (assert (= items (t/yield-all items))))))))
                  c1                [1 2 3]
                  c2                [4 5 6]
                  c3                [7 8 9]
                  ]
              (is= [1 2 3 4 5 6 7 8 9] (concat-gen c1 c2 c3))
              (is= [1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8 9 -9] (concat-gen-mirror c1 c2 c3))))

          (dotest
            (let [sq-yield   (fn [xs]
                               (t/lazy-gen
                                 (doseq [x xs]
                                   (t/yield (* x x)))))

                  sq-recur   (fn [xs]
                               (loop [cum-result []
                                      xs         xs]
                                 (if (empty? xs)
                                   cum-result
                                   (let [x (first xs)]
                                     (recur (conj cum-result (* x x))
                                       (rest xs))))))

                  sq-lazyseq (fn lazy-squares [xs]
                               (when (t/not-empty? xs)
                                 (let [x (first xs)]
                                   (lazy-seq (cons (* x x) (lazy-squares (rest xs)))))))

                  sq-reduce  (fn [xs]
                               (reduce (fn [cum-result x]
                                         (conj cum-result (* x x)))
                                 [] xs))

                  sq-for     (fn [xs]
                               (for [x xs]
                                 (* x x)))

                  sq-map     (fn [xs]
                               (map #(* % %) xs))

                  xs         (range 5)
                  res-yield  (sq-yield xs)
                  res-recur  (sq-recur xs)
                  res-lzsq   (sq-lazyseq xs)
                  res-reduce (sq-reduce xs)
                  res-for    (sq-for xs)
                  res-map    (sq-map xs)
                  ]
              (is= [0 1 4 9 16]
                res-yield
                res-recur
                res-lzsq
                res-reduce
                res-for
                res-map)))

          (dotest
            (let [heads-pairs (fn [flips]
                                (reduce +
                                  (let [flips (vec flips)]
                                    (t/lazy-gen
                                      (doseq [i (range (dec (count flips)))]
                                        (when (= :h (flips i) (flips (inc i)))
                                          (t/yield 1)))))))]
              (is= 3 (heads-pairs [:h :t :h :h :h :t :h :h])))

            ; from S. Sierra blogpost: https://stuartsierra.com/2015/04/26/clojure-donts-concat
            (let [next-results   (fn [n] (t/thru 1 n)) ; (thru 1 3) => [1 2 3]
                  build-1        (fn [n]
                                   (t/lazy-gen
                                     (loop [counter 1]
                                       (when (<= counter n)
                                         (doseq [item (next-results counter)] ; #todo -> yield-all
                                           (t/yield item))
                                         (recur (inc counter))))))
                  build-2        (fn [n]
                                   (t/lazy-gen
                                     (doseq [counter (t/thru n)]
                                       (when (<= counter n)
                                         (doseq [item (next-results counter)] ; #todo -> yield-all
                                           (t/yield item))))))
                  build-3        (fn [n]
                                   (t/lazy-gen
                                     (doseq [counter (t/thru n)]
                                       (t/yield-all (next-results counter)))))
                  build-result-1 (build-1 3) ; => (1 1 2 1 2 3)
                  build-result-2 (build-2 3)
                  build-result-3 (build-3 3)]
              (is= [1 1 2 1 2 3] build-result-1 build-result-2 build-result-3)))

          (dotest
            (let [N                99
                  cat-glue-fn      (fn [coll] (t/lazy-gen
                                                (t/yield-all (t/glue coll [1 2 3]))))
                  iter-glue        (iterate cat-glue-fn [1 2 3]) ; #todo some sort of bug here!
                  iter-glue-result (nth iter-glue N) ; #todo hangs w. 2 'yield-all' if (50 < x < 60)

                  cat-flat-fn      (fn [coll] (t/lazy-gen
                                                (t/yield-all (t/unnest [coll [1 [2 [3]]]]))))
                  iter-flat        (iterate cat-flat-fn [1 2 3]) ; #todo some sort of bug here!
                  iter-flat-result (nth iter-flat N) ; #todo hangs w. 2 'yield-all' if (50 < x < 60)
                  ]
              (when false
                (spyx (count iter-glue-result))
                (spyx (count iter-flat-result)))
              ; for N = 1299
              ; (count iter-glue-result) => 3900 " Elapsed time: 2453.917953 msecs "
              ; (count iter-flat-result) => 3900 " Elapsed time: 2970.726412 msecs "
              (is= iter-glue-result iter-flat-result))

            ; Bare yield won't compile => java.lang.RuntimeException: Unable to resolve symbol: lazy-gen-output-buffer
            ; (t/yield 99)

            ; (lazy-seq nil) => ()
            ; (lazy-cons 3 (lazy-seq nil)) => (3)
            ; (lazy-cons 2 (lazy-cons 3 (lazy-seq nil))) => (2 3)
            ; (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 (lazy-seq nil)))) => (1 2 3)
            ;
            ; (range-gen 5) => (0 1 2 3 4)
            ; (range-gen 10) => (0 1 2 3 4 5 6 7 8 9)
            ; (concat-gen [1 2 3] [4 5 6] [7 8 9]) => (1 2 3 4 5 6 7 8 9)
            ; (empty-gen-fn) => nil

            (let [seq-of-seqs [(range 0 5)
                               (range 10 15)
                               (range 20 25)]
                  flat-seq    (t/lazy-gen
                                (doseq [curr-seq seq-of-seqs]
                                  (t/yield-all curr-seq)))]
              (is= flat-seq [0 1 2 3 4 10 11 12 13 14 20 21 22 23 24])))
          ))

(dotest
  (is (t/submap? {:a 1} {:a 1 :b 2}))
  (is (t/submap? {:b 2} {:a 1 :b 2})))

#?(:clj
   (do    ; #todo fix this cljs failure
     (dotest
       (is= (range 5) (t/unlazy (range 5)))
       (let [c1 {:a 1 :b (range 3) :c {:x (range 4) (range 5) " end "}}]
         (is= c1 (t/unlazy c1))) ; line 2484
       (let [l2 '({:a (" zero " 0)} {:a (" one " 1)} {:a (" two " 2)})
             e2 (t/unlazy l2)]
         (is= l2 e2)
         (is= " one " (get-in e2 [1 :a 0] l2))
         ; (throws? (spyx (get-in l2 [1 :a 0] l2)))    ; #todo: SHOULD throw
         )
       (is= [1 2 3] (t/unlazy (map inc (range 3))))
       (is= #{1 2 3} (t/unlazy #{3 2 1})))
     ))

(dotest
  (let [info  {:a 1
               :b {:c 3
                   :d 4}}
        mania {:x 6
               :y {:w 333
                   :z 666}}]

    ;(spy :info-orig info)
    ;(spy :mania-orig mania)
    (t/it-> (t/destruct [info {:a ?
                               :b {:c ?
                                   :d ?}}
                         mania {:y {:z ?}}] ; can ignore unwanted keys like :x
              ;(spyx [a c])
              (let [a (+ 100 a)
                    c (+ 100 c)
                    d z
                    z 777]
                ;(spyx [a c])
                (restruct-all)))
      (t/with-map-vals it [info mania]
        (is= info {:a 101, :b {:c 103, :d 666}})
        (is= mania {:x 6, :y {:w 333, :z 777}})))

    (t/it-> (t/destruct [info {:a ?
                               :b {:c ?
                                   :d ?}}
                         mania {:y {:z ?}}] ; can ignore unwanted keys like :x
              ;(spyx [a c])
              (let [a (+ 100 a)
                    c (+ 100 c)
                    d z
                    z 777]
                ;(spyx [a c])
                (restruct info)))
      (is= it {:a 101, :b {:c 103, :d 666}}))

    (t/it-> (t/destruct [info {:a ?
                               :b {:c ?
                                   :d ?}}]
              ;(spyx [a c])
              (let [a (+ 100 a)
                    c (+ 100 c)]
                ;(spyx [a c])
                (restruct)))
      (is= it {:a 101, :b {:c 103, :d 4}}))))

(dotest
  (let [info  {:a 777
               :b [2 3 4]}
        mania [{:a 11} {:b 22} {:c [7 8 9]}]]
    ;(spy :info-orig info)
    ;(spy :mania-orig mania)
    (let [z ::dummy]
      (t/it-> (t/destruct [info {:a z
                                 :b [d e f]}
                           mania [{:a ?} BBB {:c clutter}]]
                ;(spyx z)
                ;(spyx [d e f])
                ;(spyx a)
                ;(spyx BBB)
                ;(spyx clutter)
                (let [clutter (mapv inc clutter)
                      BBB     {:BBB 33}
                      z       77
                      d       (+ 7 d)]
                  (restruct-all)))
        (t/with-map-vals it [info mania]
          (is= info {:a 77, :b [9 3 4]})
          (is= mania [{:a 11} {:BBB 33} {:c [8 9 10]}]))))))

(dotest
  (let [data {:a 1
              :b {:c 3
                  :d 4}}] ; can ignore unwanted keys like :d
    (t/destruct [data {:a ?
                       :b {:c ?}}]
      (is= [1 3] [a c]))
    (throws?
      (t/destruct [data {:a ?
                         :b {:z ?}}] ; bad data example (non-existant key)
        (println [a z]))))

  (let [data [1 2 3 4 5]]
    (t/destruct [data [a b c]] ; can ignore unwanted indexes 3 or 4 (0-based)
      (is= [1 2 3] [a b c]))
    (t/destruct [data {0 a
                       2 c}] ; can destructure vectors using map-index technique
      (is= [1 3] [a c])))
  (throws?
    (let [data [1 2 3]]
      (t/destruct [data [a b c d]] ; bad data example (non-existant element)
        (println [a b c d]))))

  ; multi-destruct
  (let [data-1 {:a 1 :b {:c 3}}
        data-2 {:x 7 :y {:z 9}}]
    (t/destruct [data-1 {:a ? :b {:c ?}}
                 data-2 {:x ? :y {:z ?}}]
      (is= [1 3 7 9] [a c x z])))
  (let [data-1 {:a 1 :b {:c 3}}
        data-2 [:x :y :z :666]]
    (t/destruct [data-1 {:a ? :b {:c ?}}
                 data-2 [x y z]]
      (is= [1 3 :x :y :z] [a c x y z]))
    (t/destruct [[data-1 data-2]
                 [item-1 item-2]]
      (is= [item-1 item-2] [data-1 data-2])))
  (let [data-1 {:a 1 :b {:c [:x :y :z :666]}}]
    (t/destruct [data-1 {:a ? :b {:c [x y z]}}]
      (is= [1 :x :y :z] [a x y z]))
    (t/destruct [data-1 {:a ? :b ?}]
      (is= a 1)
      (is= b {:c [:x :y :z :666]})))

  (let [data [{:a 1 :b {:c 3}}
              {:x 7 :y {:z 9}}]]
    (t/destruct [data
                 [{:a ? :b {:c ?}}
                  {:x ? :y {:z ?}}]]
      (is= [1 3 7 9] [a c x z])))
  (let [data {:a [{:b 2}
                  {:c 3}
                  [7 8 9]]}]
    (t/destruct [data {:a [{:b p}
                           {:c q}
                           [r s t]]}]
      (is= [2 3 7 8 9] [p q r s t])))

  ; duplicate vars
  (let [data-1 {:a 1 :b {:c 3}}
        data-2 {:x 7 :y {:c 9}}]
    (t/destruct [data-1 {:a ? :b {:c p}}
                 data-2 {:x ? :y {:c q}}]
      (is= [1 7 3 9] [a x p q]))
    (t/destruct [data-1 {:a ? :b {:c ?}}
                 data-2 {:x ? :y {:c q}}]
      (is= [1 7 3 9] [a x c q]))

    ; duplicate variables: these generate compile-time errors
    (comment
      (t/destruct [data-1 {:a ? :b {:c ?}}
                   data-2 {:x ? :y {:c ?}}]
        (println " destruct/dummy "))
      (t/destruct [{:a {:b {:c ?}}
                    :x {:y {:c ?}}}]
        (println " destruct/dummy ")))))

; #todo add different lengths a/b
; #todo add missing entries a/b
(dotest
  (testing " vectors "
    (is (t/wild-match? [1] [1]))
    (is (t/wild-match? [1] [1] [1]))
    (is (t/wild-match? [:*] [1] [1]))
    (is (t/wild-match? [:*] [1] [9]))

    (is (t/wild-match? [1] [1]))
    (is (t/wild-match? [1] [1] [1]))

    (isnt (t/wild-match? [1] []))
    (isnt (t/wild-match? [] [1]))
    (isnt (t/wild-match? [1] [] []))
    (isnt (t/wild-match? [] [1] []))
    (isnt (t/wild-match? [] [] [1]))
    (isnt (t/wild-match? [1] [1] []))
    (isnt (t/wild-match? [1] [] [1]))

    (is (t/wild-match? [1 2 3]
          [1 2 3]))
    (is (t/wild-match? [1 :* 3]
          [1 2 3]))
    (is (t/wild-match? [1 :* 3]
          [1 2 3]
          [1 9 3]))
    (isnt (t/wild-match? [1 2 3]
            [1 2 9]))
    (isnt (t/wild-match? [1 2]
            [1 2 9]))
    (isnt (t/wild-match? [1 2 3]
            [1 2]))

    (is (t/wild-match? [1 [2 3]]
          [1 [2 3]]))
    (is (t/wild-match? [:* [2 3]]
          [1 [2 3]]))
    (is (t/wild-match? [:* [2 3]]
          [1 [2 3]]
          [9 [2 3]]))
    (is (t/wild-match? [1 [2 :*]]
          [1 [2 33]]
          [1 [2 99]]))
    (is (t/wild-match? [1 :*]
          [1 2]
          [1 [2 3]]))
    (isnt (t/wild-match? [1 [2 3]]
            [1 [2 9]]))
    )
  (testing " maps "
    (is (t/wild-match? {:a 1} {:a 1}))
    (is (t/wild-match? {:a :*} {:a 1}))
    (is (t/wild-match? {:a :*} {:a 1} {:a 1}))
    (is (t/wild-match? {:a :*} {:a 1} {:a 9}))
    (is (t/wild-match? {:a :*} {:a :*} {:a 9}))
    (is (t/wild-match? {:a :*} {:a :*} {:a :*}))

    (isnt (t/wild-match? {:a 1} {:a 9}))
    (isnt (t/wild-match? {:a 1} {:a 1 :b 2}))
    (isnt (t/wild-match? {:a :*} {:b 1}))
    (isnt (t/wild-match? {:a :*} {:a 1} {:b 1}))
    (isnt (t/wild-match? {:a :*} {:a 1 :b 2}))

    (let [vv {:a 1 :b {:c 3}}
          tt {:a 1 :b {:c 3}}
          w2 {:a :* :b {:c 3}}
          w5 {:a 1 :b {:c :*}}
          zz {:a 2 :b {:c 3}}
          ]
      (is (t/wild-match? tt vv))
      (is (t/wild-match? w2 vv))
      (is (t/wild-match? w5 vv))
      (isnt (t/wild-match? zz vv)))
    )
  (testing " vecs & maps 1 "
    (let [vv [:a 1 :b {:c 3}]
          tt [:a 1 :b {:c 3}]
          w1 [:* 1 :b {:c 3}]
          w2 [:a :* :b {:c 3}]
          w3 [:a 1 :* {:c 3}]
          w5 [:a 1 :b {:c :*}]
          zz [:a 2 :b {:c 3}]
          ]
      (is (t/wild-match? tt vv))
      (is (t/wild-match? w1 vv))
      (is (t/wild-match? w2 vv))
      (is (t/wild-match? w3 vv))
      (is (t/wild-match? w5 vv))
      (isnt (t/wild-match? zz vv)))
    )
  (testing " vecs & maps 2 "
    (let [vv {:a 1 :b [:c 3]}
          tt {:a 1 :b [:c 3]}
          w2 {:a :* :b [:c 3]}
          w4 {:a 1 :b [:* 3]}
          w5 {:a 1 :b [:c :*]}
          z1 {:a 2 :b [:c 3]}
          z2 {:a 1 :b [:c 9]}
          ]
      (is (t/wild-match? tt vv))
      (is (t/wild-match? w2 vv))
      (is (t/wild-match? w4 vv))
      (is (t/wild-match? w5 vv))
      (isnt (t/wild-match? z1 vv))
      (isnt (t/wild-match? z2 vv)))
    )
  (testing " sets "
    (is (t/wild-match? #{1} #{1}))
    (isnt (t/wild-match? #{1} #{9}))
    (isnt (t/wild-match? #{1} #{:a :b}))
    (is (t/wild-match? #{1 #{:a :b}}
          #{1 #{:a :b}}))
    (isnt (t/wild-match? #{1 #{:a :c}}
            #{1 #{:a :x}}))
    ))

(dotest
  (isnt (t/wild-match? #{1 2} #{1 2 3 4}))
  (isnt (t/wild-match? {:pattern #{1 2}
                        :values  [#{1 2 3 4}]}))
  (is (t/wild-match? {:subset-ok true
                      :pattern   #{1 2}
                      :values    [#{1 2 3 4}]}))

  (isnt (t/wild-match? {:a 1} {:a 1 :b 2}))
  (isnt (t/wild-match? {:pattern {:a 1}
                        :values  [{:a 1 :b 2}]}))
  (is (t/wild-match? {:submap-ok true
                      :pattern   {:a 1}
                      :values    [{:a 1 :b 2}]}))

  (isnt (t/wild-match? '(1 2) '(1 2 3 4)))
  (isnt (t/wild-match? {:pattern '(1 2)
                        :values  ['(1 2 3 4)]}))
  (is (t/wild-match? {:subvec-ok true
                      :pattern   '(1 2)
                      :values    ['(1 2 3 4)]}))

  (isnt (t/wild-match? [1 2] [1 2 3 4]))
  (isnt (t/wild-match? {:pattern [1 2]
                        :values  [[1 2 3 4]]}))
  (is (t/wild-match? {:subvec-ok true
                      :pattern   [1 2]
                      :values    [[1 2 3 4]]}))

  (isnt (t/wild-submatch? #{1 :*} #{1 2 3 4}))
  (is (t/wild-submatch? #{1 2} #{1 2 3 4}))
  (is (t/wild-submatch? {:a :*} {:a 1 :b 2}))
  (is (t/wild-submatch? '(1 :* 3) '(1 2 3 4)))
  (is (t/wild-submatch? [1 :* 3] [1 2 3 4]))

  (is (t/submatch? #{1 2} #{1 2 3 4}))
  (is (t/submatch? {:a 1} {:a 1 :b 2}))
  (is (t/submatch? '(1 2) '(1 2 3 4)))
  (is (t/submatch? [1 2 3] [1 2 3 4]))
  (isnt (t/submatch? [1 :* 3] [1 2 3 4]))
  (isnt (t/submatch? {:a :*} {:a 1 :b 2}))
  (isnt (t/submatch? #{1 :*} #{1 2 3 4}))

  (let [sample-rec (->SampleRec 1 2)]
    (isnt= sample-rec {:a 1 :b 2})
    (is (t/wild-submatch? sample-rec {:a 1 :b 2}))
    (is (t/wild-submatch? {:a 1 :b 2} sample-rec))
    (is (t/submatch? sample-rec {:a 1 :b 2}))
    (is (t/submatch? {:a 1 :b 2} sample-rec))))

(dotest
  (is (t/set-match? #{1 2 3} #{1 2 3}))
  (is (t/set-match? #{:* 2 3} #{1 2 3}))
  (is (t/set-match? #{1 :* 3} #{1 2 3}))
  (is (t/set-match? #{1 2 :*} #{1 2 3}))

  (is (t/set-match? #{1 2 3 4 5} #{1 2 3 4 5}))
  (is (t/set-match? #{:* 2 3 4 5} #{1 2 3 4 5}))
  (is (t/set-match? #{1 :* 3 4 5} #{1 2 3 4 5}))
  (is (t/set-match? #{1 2 :* 4 5} #{1 2 3 4 5}))
  (is (t/set-match? #{1 2 3 :* 5} #{1 2 3 4 5}))
  (is (t/set-match? #{1 2 3 4 :*} #{1 2 3 4 5}))

  (is (t/wild-item? :*))
  (isnt (t/wild-item? :a))

  (is (t/wild-item? :*))
  (isnt (t/wild-item? :a))
  (isnt (t/wild-item? 5))
  (isnt (t/wild-item? " hello "))

  (is (t/wild-item? [:* 2 3]))
  (is (t/wild-item? [1 [:* 3]]))
  (is (t/wild-item? [1 [2 [:*]]]))
  (isnt (t/wild-item? [1 2 3]))
  (isnt (t/wild-item? [1 [2 3]]))
  (isnt (t/wild-item? [1 [2 [3]]]))

  (is (t/wild-item? #{:* 2 3}))
  (is (t/wild-item? #{1 #{:* 3}}))
  (is (t/wild-item? #{1 #{2 #{:*}}}))
  (isnt (t/wild-item? #{1 2 3}))
  (isnt (t/wild-item? #{1 #{2 3}}))
  (isnt (t/wild-item? #{1 #{2 #{3}}}))

  (is (t/wild-item? {:* 1 :b 2 :c 3}))
  (is (t/wild-item? {:a {:* 2 :c 3}}))
  (is (t/wild-item? {:a {:b {:* 3}}}))
  (is (t/wild-item? {:a :* :b 2 :c 3}))
  (is (t/wild-item? {:a {:b :* :c 3}}))
  (is (t/wild-item? {:a {:b {:c :*}}}))
  (isnt (t/wild-item? {:a 1 :b 2 :c 3}))
  (isnt (t/wild-item? {:a {:b 2 :c 3}}))
  (isnt (t/wild-item? {:a {:b {:c 3}}}))

  (is (t/set-match? #{#{1 2 3} #{4 5 :*}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 2 3} #{4 :* 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{:* 2 3} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 :* 3} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 2 :*} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))

  (is (t/set-match? #{#{1 :* 3} #{4 5 :*}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 2 :*} #{4 :* 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{:* 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{:* 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 :* 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (t/set-match? #{#{1 2 :*} #{:* 5 6}} #{#{1 2 3} #{4 5 6}})))

; #todo fix for cljs
;(deftest t-2332
;  (spyx " abc ")
;  (spyx (type " abc "))
;  (spyxx " abc ")
;  (spyx 42)
;  (spyx (type 42))
;  (spyxx 42)
;  (spyx {:a 1})
;  (spyx (type {:a 1}))
;  (spyxx {:a 1})
;  )


;-----------------------------------------------------------------------------
; Clojure version stuff

;***************************************************************************************************
;***************************************************************************************************
;***************************************************************************************************
#?(:clj
   (do

     (dotest
       (binding [*clojure-version* {:major 1 :minor 7}]
         (is (t/is-clojure-1-7-plus?))
         (isnt (t/is-clojure-1-8-plus?))
         (isnt (t/is-clojure-1-9-plus?))
         (is (t/is-pre-clojure-1-8?))
         (is (t/is-pre-clojure-1-9?)))
       (binding [*clojure-version* {:major 1 :minor 8}]
         (is (t/is-clojure-1-7-plus?))
         (is (t/is-clojure-1-8-plus?))
         (isnt (t/is-clojure-1-9-plus?))
         (isnt (t/is-pre-clojure-1-8?))
         (is (t/is-pre-clojure-1-9?)))
       (binding [*clojure-version* {:major 1 :minor 9}]
         (is (t/is-clojure-1-7-plus?))
         (is (t/is-clojure-1-8-plus?))
         (is (t/is-clojure-1-9-plus?))
         (isnt (t/is-pre-clojure-1-8?))
         (isnt (t/is-pre-clojure-1-9?))))

     ; (s/instrument-all)
     ; (s/instrument #'tupelo.core/truthy?)  ; instrument just one var

     ;-----------------------------------------------------------------------------
     ; Java version stuff

     (defn fn-any [] 42)
     (defn fn7 [] (t/if-java-1-7-plus
                    7
                    (throw (ex-info " Unimplemented prior to Java 1.7: " nil))))
     (defn fn8 [] (t/if-java-1-8-plus
                    8
                    (throw (ex-info " Unimplemented prior to Java 1.8: " nil))))

     (dotest
       (when (t/is-java-1-7?)
         (throws? (fn8)))

       (when (t/is-java-1-8-plus?)
         (is= 8 (fn8)))

       (is= 7 (fn7))
       (is= 42 (fn-any))

       (with-redefs [t/java-version (constantly "1.7")]
         (is (t/java-version-min? "1.7"))
         (isnt (t/java-version-min? "1.7.0"))
         (isnt (t/java-version-min? "1.7.0-b1234"))
         (isnt (t/java-version-min? "1.8"))

         (is (t/java-version-matches? "1.7"))
         (isnt (t/java-version-matches? "1.7.0"))
         (isnt (t/java-version-matches? "1.7.0-b1234"))
         (isnt (t/java-version-matches? "1.8"))
         )
       (with-redefs [t/java-version (constantly " 1.7.0 ")]
         (is (t/java-version-min? " 1.7 "))
         (is (t/java-version-min? "1.7.0"))
         (isnt (t/java-version-min? "1.7.0-b1234"))
         (isnt (t/java-version-min? " 1.8 "))

         (is (t/java-version-matches? "1.7"))
         (is (t/java-version-matches? " 1.7.0 "))
         (isnt (t/java-version-matches? " 1.7.0-b1234 "))
         (isnt (t/java-version-matches? " 1.8 "))
         )
       (with-redefs [t/java-version (constantly " 1.7.0-b1234 ")]
         (is (t/java-version-min? "1.7"))
         (is (t/java-version-min? " 1.7.0 "))
         (is (t/java-version-min? " 1.7.0-b1234 "))
         (isnt (t/java-version-min? " 1.8 "))

         (is (t/java-version-matches? " 1.7 "))
         (is (t/java-version-matches? " 1.7.0 "))
         (is (t/java-version-matches? " 1.7.0-b1234 "))
         (isnt (t/java-version-matches? " 1.8 "))
         )

       (with-redefs [t/java-version (constantly " 1.7 ")]
         (when false
           (println " testing java 1.7 ")
           (t/spyx (t/is-java-1-7?))
           (t/spyx (t/is-java-1-8?))
           (t/spyx (t/is-java-1-7-plus?))
           (t/spyx (t/is-java-1-8-plus?)))

         (is (t/is-java-1-7?))
         (is (t/is-java-1-7-plus?))
         (isnt (t/is-java-1-8?))
         (isnt (t/is-java-1-8-plus?)))

       (with-redefs [t/java-version (constantly " 1.8 ")]
         (when false
           (println " testing java 1.8 ")
           (t/spyx (t/is-java-1-7?))
           (t/spyx (t/is-java-1-8?))
           (t/spyx (t/is-java-1-7-plus?))
           (t/spyx (t/is-java-1-8-plus?)))

         (isnt (t/is-java-1-7?))
         (is (t/is-java-1-7-plus?))
         (is (t/is-java-1-8?))
         (is (t/is-java-1-8-plus?))))

     (dotest
       (let [tst-fn (fn [vals5]
                      (is= 5 (count vals5))
                      ; w/o endpoint
                      ;(spyx vals5)
                      (is= vals5 (t/sublist vals5 0))
                      (is= [2 3 4] (t/sublist vals5 2))
                      (is= [4] (t/sublist vals5 4))
                      (is= [] (t/sublist vals5 5))
                      (throws? (t/sublist vals5 13))
                      ;  with endpoint
                      (is= [] (t/sublist vals5 1 1))
                      (is= [2] (t/sublist vals5 2 3))
                      (is= [2 3 4] (t/sublist vals5 2 5))
                      (is= vals5 (t/sublist vals5 0 5))
                      (throws? (t/sublist vals5 5 13)))]
         (tst-fn (range 5))
         (tst-fn (list 0 1 2 3 4))
         (tst-fn (vector 0 1 2 3 4))
         (tst-fn (seq (range 5)))
         (tst-fn (seq (list 0 1 2 3 4)))
         (tst-fn (seq (vector 0 1 2 3 4))))


       (is= [2 3] (t/sublist (seq (range 5)) 2 4))
       (is= [2 3] (t/sublist (seq (vec (range 5))) 2 4)))


     (dotest
       (let [val1 (into (sorted-map) {:a 1 :b 2})]
         (is= "val1 => <#clojure.lang.PersistentTreeMap {:a 1, :b 2}>"
           (ts/collapse-whitespace (with-out-str (t/spyxx val1))))
         (is= "(+ 2 3) => <#java.lang.Long 5>"
           (ts/collapse-whitespace (with-out-str (t/spyxx (+ 2 3)))))
         (is= "(mapv inc (range 3)) => <#clojure.lang.PersistentVector [1 2 3]>"
           (ts/collapse-whitespace (with-out-str (t/spyxx (mapv inc (range 3))))))))

     ;(sp/def ::vector (sp/coll-of clj/any :kind vector?))
     ;(dotest
     ;  (is   (sp/valid? ::vector [1 2 3]))
     ;  (isnt (sp/valid? ::vector '(1 2 3)))
     ;  (isnt (sp/valid? ::vector {:a 1}))
     ; ;(spyx (sp/exercise ::vector))
     ;)

     #_(tst/defspec ^:slow t-keep-if-drop-if 999
         (prop/for-all [vv (gen/vector gen/int)]
           (let [even-1    (keep-if even? vv)
                 even-2    (drop-if odd? vv)
                 even-filt (filter even? vv)

                 odd-1     (keep-if odd? vv)
                 odd-2     (drop-if even? vv)
                 odd-rem   (remove even? vv)]
             (and (= even-1 even-2 even-filt)
               (= odd-1 odd-2 odd-rem)))))

     #_(tst/defspec ^:slow t-keep-if-drop-if-set 999
         (prop/for-all [ss (gen/set gen/int)]
           (let [even-1    (keep-if even? ss)
                 even-2    (drop-if odd? ss)
                 even-filt (into #{} (filter even? (seq ss)))

                 odd-1     (keep-if odd? ss)
                 odd-2     (drop-if even? ss)
                 odd-rem   (into #{} (remove even? (seq ss)))]
             (and (= even-1 even-2 even-filt)
               (= odd-1 odd-2 odd-rem)))))

     #_(tst/defspec ^:slow t-keep-if-drop-if-map-key 99 ; seems to hang if (< 99 limit)
         (prop/for-all [mm (gen/map gen/int gen/keyword {:max-elements 99})]
           (let [even-1    (keep-if (fn [k v] (even? k)) mm)
                 even-2    (drop-if (fn [k v] (odd? k)) mm)
                 even-filt (into {} (filter #(even? (key %)) (seq mm)))

                 odd-1     (keep-if (fn [k v] (odd? k)) mm)
                 odd-2     (drop-if (fn [k v] (even? k)) mm)
                 odd-rem   (into {} (remove #(even? (key %)) (seq mm)))
                 ]
             (and (= even-1 even-2 even-filt)
               (= odd-1 odd-2 odd-rem)))))

     #_(tst/defspec ^:slow t-keep-if-drop-if-map-value 99 ; seems to hang if (< 99 limit)
         (prop/for-all [mm (gen/map gen/keyword gen/int {:max-elements 99})]
           (let [even-1    (keep-if (fn [k v] (even? v)) mm)
                 even-2    (drop-if (fn [k v] (odd? v)) mm)
                 even-filt (into {} (filter #(even? (val %)) (seq mm)))

                 odd-1     (keep-if (fn [k v] (odd? v)) mm)
                 odd-2     (drop-if (fn [k v] (even? v)) mm)
                 odd-rem   (into {} (remove #(even? (val %)) (seq mm)))
                 ]
             (and (= even-1 even-2 even-filt)
               (= odd-1 odd-2 odd-rem)))))

     ; #todo ***** toptop *****

     ; #todo add different lengths a/b
     ; #todo add missing entries a/b
     (dotest
       (is (t/matches? [] []))
       (is (t/matches? [1] [1]))
       (isnt (t/matches? [1] [2]))
       ;        (t/matches?  [1]   [1 2] )))  ***** error *****
       (is (t/matches? [_] [1]))
       (is (t/matches? [_] [nil]))
       (is (t/matches? [_] [1] [2] [3]))
       (is (t/matches? [1 2] [1 2]))
       (is (t/matches? [_ 2] [1 2]))
       (is (t/matches? [1 _] [1 2]))
       (is (t/matches? [1 _] [1 2] [1 3] [1 nil]))
       (is (t/matches? [1 _ 3] [1 2 3] [1 nil 3]))

       (is (t/matches? {:a 1} {:a 1}))
       (isnt (t/matches? {:a 1} {:a 2}))
       (isnt (t/matches? {:a 1} {:b 1}))
       (is (t/matches? {:a _} {:a 1} {:a 2} {:a 3}))
       ;        (t/matches?  { _ 1} {:a 1} )   ***** error *****

       (is (t/matches? {:a _ :b _ :c 3}
             {:a 1 :b [1 2 3] :c 3}))
       (isnt (t/matches? {:a _ :b _ :c 4}
               {:a 1 :b [1 2 3] :c 3}))
       (isnt (t/matches? {:a _ :b _ :c 3}
               {:a 1 :b [1 2 3] :c 4}))
       (isnt (t/matches? {:a 9 :b _ :c 3}
               {:a 1 :b [1 2 3] :c 3}))

       (is (t/matches? {:a _ :b _ :c 3}
             {:a 1 :b [1 2 3] :c 3}
             {:a 2 :b 99 :c 3}
             {:a 3 :b nil :c 3}))
       (isnt (t/matches? {:a _ :b _ :c 3}
               {:a 1 :b [1 2 3] :c 9}
               {:a 2 :b 99 :c 3}
               {:a 3 :b nil :c 3}))
       (isnt (t/matches? {:a _ :b _ :c 3}
               {:a 1 :b [1 2 3] :c 3}
               {:a 2 :b 99 :c 3}
               {:a 3 :b nil :c 9}))
       )

     (dotest
       (isnt (= 5 5.0))
       (is (== 5 5.0))

       (is (t/int-val? 5))
       (is (t/int-val? 5.0))
       (is (t/int-val? 5N))
       (is (t/int-val? 5M))
       (is (t/int-val? (byte 5)))
       (is (t/int-val? (short 5)))
       (is (t/int-val? (int 5)))
       (is (t/int-val? (long 5)))
       (is (t/int-val? (float 5)))
       (is (t/int-val? (double 5)))
       (is (t/int-val? (bigdec 5)))
       (is (t/int-val? (bigint 5)))
       (is (t/int-val? (biginteger 5)))

       (isnt (t/int-val? 5.5))
       (isnt (t/int-val? 5.5M))
       (isnt (t/int-val? (float 5.5)))
       (isnt (t/int-val? (double 5.5)))
       (isnt (t/int-val? (bigdec 5.5)))

       (let [x 5/3]
         (isnt (t/int-val? x))
         (is (t/int-val? (* 3 x))))
       (let [n (bigint 1e20)
             x (/ n 3)]
         (isnt (t/int-val? x))
         (is (t/int-val? (* 3 x))))

       (throws? (t/int-val? " five "))
       (throws? (t/int-val? :five))
       )

     (dotest
       (throws? (/ 1 0))
       (throws? Exception (/ 1 0)) ; catches specified Throwable (or subclass) - JVM only
       (throws? ArithmeticException (/ 1 0)) ; catches specified Throwable (or subclass) - JVM only
       )

     (dotest
       (is (t/macro? 'and))
       (is (t/macro? '->))
       (isnt (t/macro? '+))
       (isnt (t/macro? 'if)))


     (dotest ; #todo => tupelo.core
       (is= (t/with-cum-vector
              (dotimes [ii 5]
                (t/cum-vector-append ii)))
         [0 1 2 3 4])
       (let [ff (fn ff-fn [n]
                  (when (t/nonneg? n)
                    (t/cum-vector-append n)
                    (ff-fn (dec n))))]
         (is= (t/with-cum-vector (ff 5))
           [5 4 3 2 1 0]))
       ; It will even work across multiple futures:  https://clojure.org/reference/vars#conveyance
       (let [N     10
             randy (fn [n]
                     (Thread/sleep (int (+ 50 (* 50 (Math/random)))))
                     (t/cum-vector-append n)
                     n)
             nums  (t/with-cum-vector
                     (let [futures     (forv [ii (range N)]
                                         (future (randy ii)))
                           future-vals (forv [future futures] @future)] ; wait for all futures to finish
                       (is= future-vals (range N))))] ; in order of creation
         (is-set= nums (range N)))) ; nums is in random order

     ))

