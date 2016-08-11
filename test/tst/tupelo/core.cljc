;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.core
  (:refer-clojure :exclude [any? ] )
  (:use tupelo.core
        clojure.test )
  (:require [clojure.string                         :as str]
            [clojure.spec                           :as s]
            [clojure.spec.gen                       :as sgen]
            [clojure.test.check.generators          :as gen]
            [clojure.test.check.properties          :as prop]
            [clojure.test.check.clojure-test        :as tst]
            [tupelo.misc                            :as tm]
            [schema.core                            :as schema])
  )

; Prismatic Schema type definitions
(schema/set-fn-validation! true)   ; #todo add to Schema docs

; (s/instrument-all)
; (s/instrument #'tupelo.core/truthy?)  ; instrument just one var

(deftest t-spy
  (testing "basic usage"
    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result)) ]
      (is (= "hi => 5"
          (str/trim (with-out-str (spy (side-effect-add! 2 3) :msg "hi"))) ))
      (is (= "hi => 5"
          (str/trim (with-out-str (spy :msg "hi"  (side-effect-add! 2 3)))) ))
      (is (= "(side-effect-add! 2 3) => 5"
          (str/trim (with-out-str (spyx (side-effect-add! 2 3)))) ))
      (is (= 15 @side-effect-cum-sum)))

    (is (= "first => 5\nsecond => 25"
        (str/trim (with-out-str (-> 2
                                    (+ 3)
                                    (spy :msg "first" )
                                    (* 5)
                                    (spy :msg "second") )))))
    (is (= "first => 5\nsecond => 25"
        (str/trim (with-out-str (->> 2
                                    (+ 3)
                                    (spy :msg "first" )
                                    (* 5)
                                    (spy :msg "second") )))))

    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result))
    ]
      (is (= "value => 5"
          (str/trim (with-out-str (spy (side-effect-add! 2 3) :msg "value")))))
      (is (= "value => 5"
          (str/trim (with-out-str (spy :msg "value"  (side-effect-add! 2 3))))))
      (is (= 10 @side-effect-cum-sum))

      (is (= "value => 5" (str/trim (with-out-str (spy "value" (+ 2 3) )))))
      (is (=   "spy => 5" (str/trim (with-out-str (spy         (+ 2 3) )))))

      (is (= "(str \"abc\" \"def\") => \"abcdef\""
          (str/trim (with-out-str (spyx (str "abc" "def") )))))

      (is (thrown? IllegalArgumentException  (spy "some-msg" 42 :msg)))
    )))

(deftest t-spyxx
  (let [val1  (into (sorted-map) {:a 1 :b 2})
        val2  (+ 2 3) ]
    (is (= "val1 => clojure.lang.PersistentTreeMap->{:a 1, :b 2}"
        (str/trim (with-out-str (spyxx val1 )))  ))

    (is (= "val2 => java.lang.Long->5"
        (str/trim (with-out-str (spyxx val2 ))) ))
  ))

(deftest t-with-spy-indent
  (let [fn2   (fn []  (with-spy-indent
                        (spy :msg "msg2" (+ 2 3))))
        fn1   (fn []  (with-spy-indent
                        (spy :msg "msg1" (+ 2 3))
                        (fn2)))
        fn0   (fn [] (spy :msg "msg0" (+ 2 3))) ]
    (is (= "  msg2 => 5\n"                  (with-out-str (fn2))))
    (is (= "  msg1 => 5\n    msg2 => 5\n"   (with-out-str (fn1))))
    (is (= "msg0 => 5\n"                    (with-out-str (fn0))))
    ))


(deftest t-truthy-falsey
  (is (truthy? 5))
; (spyx (s/check-fn truthy? ))

  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [truthies    (keep-if boolean data)       ; coerce to primitive type
            falsies     (keep-if not     data) ]     ; unnatural syntax
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
                  (= falsies  [false nil] ) )))
      (let [truthies    (keep-if truthy? data)
            falsies     (keep-if falsey? data) ]
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
                  (= falsies  [false nil] ) ))
        (is (every? truthy? [true :a 'my-symbol 1 "hello" \x] ))
        (is (every? falsey? [false nil] ))
        (is (not-any? falsey? truthies))
        (is (not-any? truthy? falsies))))

    (testing "improved usage"
      (let [count-if (comp count keep-if) ]
        (let [num-true    (count-if boolean data)   ; awkward phrasing
              num-false   (count-if not     data) ] ; doesn't feel natural
          (is (and  (= 6 num-true)
                    (= 2 num-false) )))
        (let [num-true    (count-if truthy? data)   ; matches intent much better
              num-false   (count-if falsey? data) ]
          (is (and  (= 6 num-true)
                    (= 2 num-false) )))))
  ))

(deftest t-not-nil?
  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [notties   (keep-if not-nil? data)
            nillies   (drop-if not-nil? data) ]
        (is (and  (= notties [true :a 'my-symbol 1 "hello" \x false] )
                  (= nillies [nil] )))
        (is (every?   not-nil? notties))
        (is (every?       nil? [nil] ))
        (is (not-any?     nil? notties))
        (is (not-any? not-nil? nillies))))

    (testing "improved usage"
      (let [count-if (comp count keep-if) ]
        (let [num-valid-1     (count-if some?    data)  ; awkward phrasing, doesn't feel natural
              num-valid-2     (count-if not-nil? data)  ; matches intent much better
              num-nil     (count-if nil?     data) ]    ; intent is plain
          (is (and  (= 7 num-valid-1 num-valid-2 )
                    (= 1 num-nil) )))))))

(deftest t-any
  (is (= true   (any? odd? [1 2 3] ) ))
  (is (= false  (any? odd? [2 4 6] ) ))
  (is (= false  (any? odd? []      ) )))

(deftest t-not-empty
  (is (every?     not-empty? ["1" [1] '(1) {:1 1} #{1}    ] ))
  (is (not-any?   not-empty? [""  []  '()  {}     #{}  nil] ))

  (is (= (map not-empty? ["1" [1] '(1) {:1 1} #{1} ] )
         [true true true true true]  ))
  (is (= (map not-empty? ["" [] '() {} #{} nil] )
         [false false false false false false ] ))

  (is (= (keep-if not-empty?  ["1" [1] '(1) {:1 1} #{1} ] )
                              ["1" [1] '(1) {:1 1} #{1} ] ))
  (is (= (drop-if not-empty?  [""  []  '()  {}     #{}  nil] )
                              [""  []  '()  {}     #{}  nil] )))

(deftest t-forv
  (is (= (forv [x (range 4)] (* x x))
         [0 1 4 9] ))
  (is (= (forv [x (range 23)] (* x x))
         (for  [x (range 23)] (* x x))))
  (is (= (forv [x (range 5)  y (range 2 9)] (str x y))
         (for  [x (range 5)  y (range 2 9)] (str x y)))))

(deftest t-glue
  ; unexpected results
  (is (= (concat {:a 1} {:b 2} {:c 3} )
               [ [:a 1] [:b 2] [:c 3] ] ))
  (is (= (conj [1 2] [3 4])
               [1 2  [3 4] ] ))

  (let [objs   [ [] '()   {} (sorted-map)   #{} (sorted-set) ] ]
    (is (= (map sequential? objs) [true  true    false false   false false] ))
    (is (= (map map?        objs) [false false   true  true    false false] ))
    (is (= (map set?        objs) [false false   false false   true  true ] )))

  (is (= (glue [1 2] [3 4] [5 6])        [1 2 3 4 5 6]))
  (is (= (glue [] [1 2] )                [1 2] ))
  (is (= (glue [1 2] [] )                [1 2] ))
  (is (= (glue [] [1 2] [] )             [1 2] ))

  (is (= (glue '(1 2) '(3 4) '(5 6))        [1 2 3 4 5 6]))
  (is (= (glue '(1 2)  [3 4] '(5 6))        [1 2 3 4 5 6]))
  (is (= (glue  [1 2] '(3 4) '(5 6))        [1 2 3 4 5 6]))
  (is (= (glue '() '(1 2) )                 [1 2] ))
  (is (= (glue '(1 2) '() )                 [1 2] ))
  (is (= (glue '() '(1 2) '() )             [1 2] ))

  (is (= (glue (range 3) (range 5))      [0 1 2 0 1 2 3 4] ))

  (is (= (glue {:a 1} {:b 2} {:c 3})      {:a 1 :c 3 :b 2}))
  (is (= (glue {:a 1} {:b 2} )            {:a 1 :b 2}))
  (is (= (glue {:a 1} {} )                {:a 1} ))
  (is (= (glue {} {:a 1} )                {:a 1} ))
  (is (= (glue {} {:a 1} {} )             {:a 1} ))

  (is (= (glue #{1 2} #{3 4} #{6 5})     #{1 2 6 5 3 4}))
  (is (= (glue #{} #{1 2} )              #{1 2} ))
  (is (= (glue #{1 2} #{} )              #{1 2} ))
  (is (= (glue #{} #{1 2} #{} )          #{1 2} ))

  (is (= (glue (sorted-map) {:a 1} {:b 2} {:c 3})   {:a 1 :b 2 :c 3} ))
  (is (= (glue (sorted-set) #{1 2} #{3 4} #{6 5})   #{1 2 3 4 5 6}))

  (is (= (glue (sorted-map) {:a 1 :b 2} {:c 3 :d 4} {:e 5 :f 6})
                            {:a 1 :b 2   :c 3 :d 4   :e 5 :f 6} ))
  (is (= (seq (glue (sorted-map) {:a 1   :b 2} {:c 3   :d 4   :e 5} {:f 6}))
                               [ [:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] ] ))

  (is (= (glue  {:band :VanHalen :singer :Dave} {:singer :Sammy} )
                {:band :VanHalen                 :singer :Sammy} ))

  (is (= (glue \a )           "a" ))
  (is (= (glue "a")           "a" ))
  (is (= (glue \a "")         "a" ))
  (is (= (glue "" "a")        "a" ))
  (is (= (glue \a  \b)        "ab" ))
  (is (= (glue "a" "b")       "ab" ))
  (is (= (glue "a" \b)        "ab" ))
  (is (= (glue \a  "b")       "ab" ))
  (is (= (glue "" "a" \b)     "ab" ))
  (is (= (glue "" \a  "b")    "ab" ))
  (is (= (glue "a" "" \b)     "ab" ))
  (is (= (glue \a  "" "b")    "ab" ))
  (is (= (glue "a" \b  "")    "ab" ))
  (is (= (glue \a  "b" "")    "ab" ))
  (is (= (glue \a  "b" "")    "ab" ))
  (is (= (glue "I" \space "like " \a " nap!" )    "I like a nap!" ))

  (is (thrown? IllegalArgumentException   (glue   [1 2]     {:a 1} )))
  (is (thrown? IllegalArgumentException   (glue  '(1 2)     {:a 1} )))
  (is (thrown? IllegalArgumentException   (glue   [1 2]    #{:a 1} )))
  (is (thrown? IllegalArgumentException   (glue  '(1 2)    #{:a 1} )))
  (is (thrown? IllegalArgumentException   (glue   [1 2]    "hello" )))
  (is (thrown? IllegalArgumentException   (glue  '(1 2)    "hello" )))
  (is (thrown? IllegalArgumentException   (glue   {:a 1}   #{:a 1} )))
  (is (thrown? IllegalArgumentException   (glue   {:a 1}   "hello" )))
  (is (thrown? IllegalArgumentException   (glue   #{:a 1}  "hello" )))

  (is (thrown? IllegalArgumentException   (glue   [1 2]     nil )))
  (is (thrown? IllegalArgumentException   (glue  '(1 2)     nil )))
  (is (thrown? IllegalArgumentException   (glue   {:a 1}    nil )))
  (is (thrown? IllegalArgumentException   (glue   #{:a 1}   nil )))
  (is (thrown? IllegalArgumentException   (glue   "hello"   nil )))
  )

(deftest t-append
  (is (thrown? Exception  (append  1 2        )))
  (is (thrown? Exception  (append [1 2]       )))
  (is (= [1 2 3    ]      (append [1 2] 3     )))
  (is (= [1 2 3 4  ]      (append [1 2] 3 4   )))
  (is (= [1 2 3 4 5]      (append [1 2] 3 4 5 )))

  (is (thrown? Exception  (append '(1 2)       )))
  (is (= [1 2 3    ]      (append '(1 2) 3     )))
  (is (= [1 2 3 4  ]      (append '(1 2) 3 4   )))
  (is (= [1 2 3 4 5]      (append '(1 2) 3 4 5 )))

  (is (thrown? Exception (append   {:a 1} 99     )))
  (is (thrown? Exception (append   {:a 1} {:b 2} )))
  (is (thrown? Exception (append  #{:a 1} 99     )))
  (is (thrown? Exception (append  #{:a 1} #{99}  )))

  (testing "old conjv tests"
    (is (= [  2  ]  (append  []  2   )))
    (is (= [  2  ]  (append '()  2   )))
    (is (= [  2 3]  (append  []  2  3)))
    (is (= [  2 3]  (append '()  2  3)))

    (is (= [1 2 3]  (append  [1] 2  3)))
    (is (= [1 2 3]  (append '(1) 2  3)))
    (is (= [1 2 3]  (append  [1  2] 3)))
    (is (= [1 2 3]  (append '(1  2) 3)))

    (is (= [1 2 3 4]  (append  [1  2] 3 4)))
    (is (= [1 2 3 4]  (append '(1  2) 3 4)))
    (is (= [1 2 3 4]  (append  [1] 2  3 4)))
    (is (= [1 2 3 4]  (append '(1) 2  3 4)))

    (is (= [[1 2] [3 4] [5 6]] (append  [[1 2] [3 4]]  [5 6] )))

    (is (= [0 1 2 3 4 5] (append (range 4) 4 5)))
    (is (= [0 1 2 3 4 5] (apply append [0] (range 1 6))))))

(deftest t-prepend
  (is (thrown? Exception  (prepend       [2 1] )))
  (is (= [    3 2 1]      (prepend     3 [2 1] )))
  (is (= [  4 3 2 1]      (prepend   4 3 [2 1] )))
  (is (= [5 4 3 2 1]      (prepend 5 4 3 [2 1] )))

  (is (thrown? Exception  (prepend       '(2 1) )))
  (is (= [    3 2 1]      (prepend     3 '(2 1) )))
  (is (= [  4 3 2 1]      (prepend   4 3 '(2 1) )))
  (is (= [5 4 3 2 1]      (prepend 5 4 3 '(2 1) )))

  (is (thrown? Exception (prepend   99     {:a 1} )))
  (is (thrown? Exception (prepend   {:b 2} {:a 1} )))
  (is (thrown? Exception (prepend   99    #{:a 1} )))
  (is (thrown? Exception (prepend  #{99}  #{:a 1} ))))

(deftest t-grab
  (let [map1  {:a 1 :b 2}]
    (is (= 1                                  (grab :a map1)))
    (is (= 2                                  (grab :b map1)))
    (is (thrown?    IllegalArgumentException  (grab :c map1))) ))

(deftest t-fetch-in
  (testing "basic usage"
    (let [map1  {:a1 "a1"
                 :a2 { :b1 "b1"
                       :b2 { :c1 "c1"
                             :c2 "c2" }}} ]
      (is (= (fetch-in map1 [:a1] ) "a1" ))
      (is (= (fetch-in map1 [:a2 :b1] ) "b1" ))
      (is (= (fetch-in map1 [:a2 :b2 :c1] ) "c1" ))
      (is (= (fetch-in map1 [:a2 :b2 :c2] ) "c2" ))
      (is (thrown? IllegalArgumentException  (fetch-in map1 [:a9]) ))
      (is (thrown? IllegalArgumentException  (fetch-in map1 [:a2 :b9]) ))
      (is (thrown? IllegalArgumentException  (fetch-in map1 [:a2 :b2 :c9]) ))
    )))

(deftest t-dissoc-in
  (let [mm    {:a { :b { :c "c" }}} ]
    (is (= (dissoc-in mm []         )          mm ))
    (is (= (dissoc-in mm [:a]       )          {} ))
    (is (= (dissoc-in mm [:a :b]    )          {:a  {}} ))
    (is (= (dissoc-in mm [:a :b :c] )          {:a  { :b  {}}} ))
    (is (= (dissoc-in mm [:a :x :y] )          {:a  { :b  { :c "c" }
                                                         :x  nil }} ))
    (is (= (dissoc-in mm [:a :x :y :z] )       {:a  { :b  { :c "c" }
                                                         :x  { :y nil }}} ))
    (is (= (dissoc-in mm [:k1 :k2 :k3 :kz] )   {:a  { :b  { :c  "c" }}
                                                   :k1 { :k2 { :k3 nil }}} )))
  (let [mm    {:a1 "a1"
               :a2 { :b1 "b1"
                     :b2 { :c1 "c1"
                           :c2 "c2" }}} ]
    (is (= (dissoc-in mm [:a1] )
              {:a2 { :b1 "b1"
                     :b2 { :c1 "c1"
                           :c2 "c2" }}} ))
    (is (= (dissoc-in mm [:a2] )
              {:a1 "a1" } ))
    (is (= (dissoc-in mm [:a2 :b1] )
              {:a1 "a1"
               :a2 { :b2 { :c1 "c1"
                           :c2 "c2" }}} ))
    (is (= (dissoc-in mm [:a2 :b2] )
              {:a1 "a1"
               :a2 { :b1 "b1" }} ))
    (is (= (dissoc-in mm [:a2 :b2 :c1] )
              {:a1 "a1"
               :a2 { :b1 "b1"
                     :b2 { :c2 "c2" }}} ))
    (is (= (dissoc-in mm [:a2 :b2 :c2] )
              {:a1 "a1"
               :a2 { :b1 "b1"
                     :b2 { :c1 "c1" }}} ))))

(deftest t-only
  (is (= 42 (only [42])))
  (is (= :x (only [:x])))
  (is (= "hello" (only ["hello"] )))
  (is (thrown? IllegalArgumentException (only [])))
  (is (thrown? IllegalArgumentException (only [:x :y]))))

(deftest t-validate
  (is (= 3        (validate pos? 3)))
  (is (= 3.14     (validate number? 3.14 )))
  (is (= 3.14     (validate #(< 3 % 4) 3.14 )))
  (is (= [0 1 2]  (validate vector? (vec (range 3)))))
  (is (= nil      (validate nil? (next []))))
  (is (= [0 1 2]  (validate #(= 3 (count %)) [0 1 2])))
  (is (thrown? Exception (validate number? "hello")))
  (is (thrown? Exception (validate truthy? nil)))
)

(deftest t-keyvals
  (testing "basic usage"
    (let [m1 {:a 1 :b 2 :c 3}
          m2 {:a 1 :b 2 :c [3 4]} ]
      (is (= m1 (apply hash-map (keyvals m1))))
      (is (= m2 (apply hash-map (keyvals m2))))
    )))
; AWTAWT TODO: add test.check

(deftest t-safe->
  (is (= 7 (safe-> 3 (* 2) (+ 1))))
  (let [mm  {:a {:b 2}}]
    (is (= (safe-> mm :a)     {:b 2} ))
    (is (= (safe-> mm :a :b)      2))
    (is (thrown? IllegalArgumentException   (safe-> mm :x)))
    (is (thrown? IllegalArgumentException   (safe-> mm :a :x)))
    (is (thrown? IllegalArgumentException   (safe-> mm :a :b :x)))
  ))

(deftest t-it->
  (is (= 2  (it-> 1
                  (inc it)
                  (+ 3 it)
                  (/ 10 it))))
  (let [mm  {:a {:b 2}}]
    (is (= (it-> mm (:a it)          )  {:b 2} ))
    (is (= (it-> mm (it :a)  (:b it) )      2  ))))

(deftest t-with-exception-default
  (testing "basic usage"
    (is (thrown?    Exception                       (/ 1 0)))
    (is (= nil      (with-exception-default nil     (/ 1 0))))
    (is (= :dummy   (with-exception-default :dummy  (/ 1 0))))
    (is (= 123      (with-exception-default 0       (Long/parseLong "123"))))
    (is (= 0        (with-exception-default 0       (Long/parseLong "12xy3"))))
    ))

(deftest t-rel=
  (is (rel= 1 1 :digits 4 ))
  (is (rel= 1 1 :tol    0.01 ))

  (is (thrown? IllegalArgumentException  (rel= 1 1 )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 4)))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :xxdigits 4      )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :digits   4.1    )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :digits   0      )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :digits  -4      )))

  (is (thrown? IllegalArgumentException  (rel= 1 1 :tol    -0.01    )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :tol     "xx"    )))
  (is (thrown? IllegalArgumentException  (rel= 1 1 :xxtol   0.01    )))

  (is      (rel=   0   0   :digits 3 ))
  (is      (rel=  42  42   :digits 99 ))
  (is      (rel=  42  42.0 :digits 99 ))

  (is      (rel= 1 1.001 :digits 3 ))
  (is (not (rel= 1 1.001 :digits 4 )))
  (is      (rel=   123450000   123456789 :digits 4 ))
  (is (not (rel=   123450000   123456789 :digits 6 )))
  (is      (rel= 0.123450000 0.123456789 :digits 4 ))
  (is (not (rel= 0.123450000 0.123456789 :digits 6 )))

  (is      (rel= 1 1.001 :tol 0.01 ))
  (is (not (rel= 1 1.001 :tol 0.0001 )))
)

(deftest t-keep-if
  (is (= [0 2 4 6 8]  (keep-if even? (range 10))
                      (drop-if odd?  (range 10))))
  (is (= [1 3 5 7 9]  (keep-if odd?  (range 10))
                      (drop-if even? (range 10))))

  ; If we supply a 2-arg fn when filtering a sequence, we get an Exception
  (is (thrown? clojure.lang.ArityException (keep-if (fn [arg1 arg2] :dummy) #{1 2 3} )))

  ; Verify throw if coll is not a sequential, map, or set.
  (is (thrown? IllegalArgumentException (keep-if truthy? 2 )))
  (is (thrown? IllegalArgumentException (keep-if truthy? :some-kw ))))

(deftest t-keep-if-map
  (let [m1  {10  0,   20 0
             11  1,   21 1
             12  2,   22 2
             13  3,   23 3} ]
    (is (= (keep-if   (fn [k v] (odd?  k))  m1)
           (drop-if   (fn [k v] (even? k))  m1)
            {11  1,   21 1
             13  3,   23 3} ))
    (is (= (keep-if   (fn [k v] (even? k))  m1)     (keep-if   (fn [k v] (even? v))  m1) 
           (drop-if   (fn [k v] (odd?  k))  m1)     (drop-if   (fn [k v] (odd?  v))  m1)     
            {10  0,   20 0
             12  2,   22 2} ))
    (is (=  (keep-if   (fn [k v] (< k 19))  m1)
            (drop-if   (fn [k v] (> k 19))  m1)
            {10  0
             11  1
             12  2
             13  3} ))
    (is (=  (keep-if   (fn [k v] (= 1 (int (/ k 10))))  m1)
            (drop-if   (fn [k v] (= 2 (int (/ k 10))))  m1)
            {10  0
             11  1
             12  2
             13  3} ))
    (is (=  (keep-if   (fn [k v] (= 2 (int (/ k 10))))  m1)
            (drop-if   (fn [k v] (= 1 (int (/ k 10))))  m1)
            {20  0
             21  1
             22  2
             23  3} ))
    (is (=  (keep-if   (fn [k v] (<= v 1   ))  m1)
            (drop-if   (fn [k v] (<=   2 v ))  m1)
            {10  0,   20 0
             11  1,   21 1 } ))

    ; If we supply a 1-arg fn when filtering a map, we get an Exception
    (is (thrown? clojure.lang.ArityException (keep-if (fn [arg] :dummy) {:a 1} )))
  ))

(deftest t-keep-if-set
  (let [s1  (into (sorted-set) (range 10)) ]
    (is (= #{0 2 4 6 8}   (keep-if even? s1)
                          (drop-if odd?  s1)))
    (is (= #{1 3 5 7 9}   (keep-if odd?  s1)
                          (drop-if even? s1)))

    ; If we supply a 2-arg fn when filtering a set, we get an Exception
    (is (thrown? clojure.lang.ArityException (keep-if (fn [arg1 arg2] :dummy) #{1 2 3} )))
  ))

(tst/defspec ^:slow t-keep-if-drop-if 999
  (prop/for-all [vv (gen/vector gen/int) ]
    (let [even-1      (keep-if   even?  vv)
          even-2      (drop-if   odd?   vv)
          even-filt   (filter    even?  vv)

          odd-1       (keep-if   odd?   vv)
          odd-2       (drop-if   even?  vv)
          odd-rem     (remove    even?  vv) ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

(tst/defspec ^:slow t-keep-if-drop-if-set 999
  (prop/for-all [ss (gen/set gen/int) ]
    (let [even-1      (keep-if   even?  ss)
          even-2      (drop-if   odd?   ss)
          even-filt   (into #{} (filter even? (seq ss)))

          odd-1       (keep-if   odd?   ss)
          odd-2       (drop-if   even?  ss)
          odd-rem     (into #{} (remove even? (seq ss))) ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

(tst/defspec ^:slow t-keep-if-drop-if-map-key 99  ; seems to hang if (< 99 limit)
  (prop/for-all [mm (gen/map gen/int gen/keyword {:max-elements 99} ) ]
    (let [even-1      (keep-if  (fn [k v] (even? k))  mm)
          even-2      (drop-if  (fn [k v] (odd?  k))  mm)
          even-filt   (into {} (filter #(even? (key %)) (seq mm)))

          odd-1      (keep-if  (fn [k v] (odd?  k))  mm)
          odd-2      (drop-if  (fn [k v] (even? k))  mm)
          odd-rem    (into {} (remove #(even? (key %)) (seq mm)))
    ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

(tst/defspec ^:slow t-keep-if-drop-if-map-value 99  ; seems to hang if (< 99 limit)
  (prop/for-all [mm (gen/map gen/keyword gen/int {:max-elements 99} ) ]
    (let [even-1      (keep-if  (fn [k v] (even? v))  mm)
          even-2      (drop-if  (fn [k v] (odd?  v))  mm)
          even-filt   (into {} (filter #(even? (val %)) (seq mm)))

          odd-1      (keep-if  (fn [k v] (odd?  v))  mm)
          odd-2      (drop-if  (fn [k v] (even? v))  mm)
          odd-rem    (into {} (remove #(even? (val %)) (seq mm)))
    ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

(deftest t-strcat
  (is (= "a" (strcat \a  )) (strcat [\a]  ))
  (is (= "a" (strcat "a" )) (strcat ["a"] ))
  (is (= "a" (strcat 97  )) (strcat [97]  ))

  (is (= "ab" (strcat \a   \b   )) (strcat [\a]  \b   ))
  (is (= "ab" (strcat \a  [\b]  )) (strcat [\a   \b]  ))
  (is (= "ab" (strcat "a"  "b"  )) (strcat ["a"] "b"  ))
  (is (= "ab" (strcat "a" ["b"] )) (strcat ["a"  "b"] ))
  (is (= "ab" (strcat 97   98   )) (strcat [97]  98   ))
  (is (= "ab" (strcat 97  [98]  )) (strcat [97   98]  ))

  (is (= "abcd" (strcat              97  98   "cd" )))
  (is (= "abcd" (strcat             [97  98]  "cd" )))
  (is (= "abcd" (strcat (byte-array [97  98]) "cd" )))

  (is (= (strcat "I " [ \h \a \v [\e \space (byte-array [97]) 
                        [ 32 "complicated" (Math/pow 2 5) '( "str" "ing") ]]] )
         "I have a complicated string" ))

  (let [chars-set   (into #{} (tm/char-seq \a \z))
        str-val     (strcat chars-set) ]
    (is (= 26 (count chars-set)))
    (is (= 26 (count str-val)))
    (is (= 26 (count (re-seq #"[a-z]" str-val))))))

(deftest t-clip-str
  (testing "single string"
    (is (= ""         (clip-str 0 "abcdefg")))
    (is (= "a"        (clip-str 1 "abcdefg")))
    (is (= "ab"       (clip-str 2 "abcdefg")))
    (is (= "abc"      (clip-str 3 "abcdefg")))
    (is (= "abcd"     (clip-str 4 "abcdefg")))
    (is (= "abcde"    (clip-str 5 "abcdefg"))))
  (testing "two strings"
    (is (= ""         (clip-str 0 "abc defg")))
    (is (= "a"        (clip-str 1 "abc defg")))
    (is (= "ab"       (clip-str 2 "abc defg")))
    (is (= "abc"      (clip-str 3 "abc defg")))
    (is (= "abc "     (clip-str 4 "abc defg")))
    (is (= "abc d"    (clip-str 5 "abc defg"))))
  (testing "two strings & char"
    (is (= ""         (clip-str 0 "ab" \c "defg")))
    (is (= "a"        (clip-str 1 "ab" \c "defg")))
    (is (= "ab"       (clip-str 2 "ab" \c "defg")))
    (is (= "abc"      (clip-str 3 "ab" \c "defg")))
    (is (= "abcd"     (clip-str 4 "ab" \c "defg")))
    (is (= "abcde"    (clip-str 5 "ab" \c "defg"))))
  (testing "two strings & digit"
    (is (= ""         (clip-str 0 "ab" 9 "defg")))
    (is (= "a"        (clip-str 1 "ab" 9 "defg")))
    (is (= "ab"       (clip-str 2 "ab" 9 "defg")))
    (is (= "ab9"      (clip-str 3 "ab" 9 "defg")))
    (is (= "ab9d"     (clip-str 4 "ab" 9 "defg")))
    (is (= "ab9de"    (clip-str 5 "ab" 9 "defg"))))
  (testing "vector"
    (is (= ""               (clip-str  0 [1 2 3 4 5] )))
    (is (= "["              (clip-str  1 [1 2 3 4 5] )))
    (is (= "[1"             (clip-str  2 [1 2 3 4 5] )))
    (is (= "[1 2"           (clip-str  4 [1 2 3 4 5] )))
    (is (= "[1 2 3 4"       (clip-str  8 [1 2 3 4 5] )))
    (is (= "[1 2 3 4 5]"    (clip-str 16 [1 2 3 4 5] ))))
  (testing "map"
    (is (= ""               (clip-str  0 (sorted-map :a 1 :b 2) )))
    (is (= "{"              (clip-str  1 (sorted-map :a 1 :b 2) )))
    (is (= "{:"             (clip-str  2 (sorted-map :a 1 :b 2) )))
    (is (= "{:a "           (clip-str  4 (sorted-map :a 1 :b 2) )))
    (is (= "{:a 1, :"       (clip-str  8 (sorted-map :a 1 :b 2) )))
    (is (= "{:a 1, :b 2}"   (clip-str 16 (sorted-map :a 1 :b 2) ))))
  (testing "set"
    (let [tst-set (sorted-set 5 4 3 2 1) ]
      (is (= ""             (clip-str  0 tst-set )))
      (is (= "#"            (clip-str  1 tst-set )))
      (is (= "#{"           (clip-str  2 tst-set )))
      (is (= "#{1 "         (clip-str  4 tst-set )))
      (is (= "#{1 2 3 "     (clip-str  8 tst-set )))
      (is (= "#{1 2 3 4 5}" (clip-str 16 tst-set )))))
)

(deftest t-drop-at
  (is (= [] (drop-at (range 1) 0)))

  (is (= [  1] (drop-at (range 2) 0)))
  (is (= [0  ] (drop-at (range 2) 1)))

  (is (= [  1 2] (drop-at (range 3) 0)))
  (is (= [0   2] (drop-at (range 3) 1)))
  (is (= [0 1  ] (drop-at (range 3) 2)))

  (is (thrown? IllegalArgumentException (drop-at []         0)))
  (is (thrown? IllegalArgumentException (drop-at (range 3) -1)))
  (is (thrown? IllegalArgumentException (drop-at (range 3)  3))))

(deftest t-insert-at
  (is (= [9] (insert-at [] 0 9)))

  (is (= [9 0] (insert-at [0] 0 9)))
  (is (= [0 9] (insert-at [0] 1 9)))

  (is (= [9 0 1] (insert-at [0 1] 0 9)))
  (is (= [0 9 1] (insert-at [0 1] 1 9)))
  (is (= [0 1 9] (insert-at [0 1] 2 9)))

  (is (thrown? IllegalArgumentException (insert-at [] -1 9)))
  (is (thrown? IllegalArgumentException (insert-at []  1 9)))

  (is (thrown? IllegalArgumentException (insert-at [0] -1 9)))
  (is (thrown? IllegalArgumentException (insert-at [0]  2 9)))

  (is (thrown? IllegalArgumentException (insert-at [0 1] -1 9)))
  (is (thrown? IllegalArgumentException (insert-at [0 1]  3 9))))

(deftest t-replace-at
  (is (= [9] (replace-at (range 1) 0 9)))

  (is (= [9 1] (replace-at (range 2) 0 9)))
  (is (= [0 9] (replace-at (range 2) 1 9)))

  (is (= [9 1 2] (replace-at (range 3) 0 9)))
  (is (= [0 9 2] (replace-at (range 3) 1 9)))
  (is (= [0 1 9] (replace-at (range 3) 2 9)))

  (is (thrown? IllegalArgumentException (replace-at []         0 9)))
  (is (thrown? IllegalArgumentException (replace-at (range 3) -1 9)))
  (is (thrown? IllegalArgumentException (replace-at (range 3)  3 9))))

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(deftest  ^{:deprecated "1.9.0-alpha5" } t-seqable
  (is (seqable?   "abc"))
  (is (seqable?   {1 2 3 4} ))
  (is (seqable?  #{1 2 3} ))
  (is (seqable?  '(1 2 3) ))
  (is (seqable?   [1 2 3] ))
  (is (seqable?   (byte-array [1 2] )))

  (is (not (seqable?  1 )))
  (is (not (seqable? \a ))))

; #todo add different lengths a/b
; #todo add missing entries a/b
(deftest t-matches
  (is      (matches?  []    [] ))
  (is      (matches?  [1]   [1] ))
  (is (not (matches?  [1]   [2] )))
  ;        (matches?  [1]   [1 2] )))  ***** error *****
  (is      (matches?  [_]   [1] ))
  (is      (matches?  [_]   [nil] ))
  (is      (matches?  [_]   [1] [2] [3]))
  (is      (matches?  [1 2] [1 2] ))
  (is      (matches?  [_ 2] [1 2] ))
  (is      (matches?  [1 _] [1 2] ))
  (is      (matches?  [1 _] [1 2] [1 3] [1 nil] ))
  (is      (matches?  [1 _ 3] [1 2 3] [1 nil 3] ))

  (is      (matches?  {:a 1} {:a 1} ))
  (is (not (matches?  {:a 1} {:a 2} )))
  (is (not (matches?  {:a 1} {:b 1} )))
  (is      (matches?  {:a _} {:a 1} {:a 2} {:a 3} ))
  ;        (matches?  { _ 1} {:a 1} )   ***** error *****

  (is      (matches?  {:a _ :b _       :c 3} 
                      {:a 1 :b [1 2 3] :c 3} ))
  (is (not (matches?  {:a _ :b _       :c 4} 
                      {:a 1 :b [1 2 3] :c 3} )))
  (is (not (matches?  {:a _ :b _       :c 3} 
                      {:a 1 :b [1 2 3] :c 4} )))
  (is (not (matches?  {:a 9 :b _       :c 3} 
                      {:a 1 :b [1 2 3] :c 3} )))

  (is      (matches?  {:a _ :b _       :c 3} 
                      {:a 1 :b [1 2 3] :c 3}
                      {:a 2 :b 99      :c 3}
                      {:a 3 :b nil     :c 3} ))
  (is (not (matches?  {:a _ :b _       :c 3} 
                      {:a 1 :b [1 2 3] :c 9}
                      {:a 2 :b 99      :c 3}
                      {:a 3 :b nil     :c 3} )))
  (is (not (matches?  {:a _ :b _       :c 3} 
                      {:a 1 :b [1 2 3] :c 3}
                      {:a 2 :b 99      :c 3}
                      {:a 3 :b nil     :c 9} )))
)

; #todo add different lengths a/b
; #todo add missing entries a/b
(deftest t-wild-match
  (testing "vectors"
    (is      (wild-match? [1]  [1] ))
    (is      (wild-match? [1]  [1] [1] ))
    (is      (wild-match? [:*] [1] [1] ))
    (is      (wild-match? [:*] [1] [9] ))

    (is      (wild-match? [1] [1] ))
    (is      (wild-match? [1] [1] [1] ))

    (is (not (wild-match? [1] [ ] )))
    (is (not (wild-match? [ ] [1] )))
    (is (not (wild-match? [1] [ ] [ ] )))
    (is (not (wild-match? [ ] [1] [ ] )))
    (is (not (wild-match? [ ] [ ] [1] )))
    (is (not (wild-match? [1] [1] [ ] )))
    (is (not (wild-match? [1] [ ] [1] )))

    (is      (wild-match? [1 2  3]
                          [1 2  3] ))
    (is      (wild-match? [1 :* 3]
                          [1 2  3] ))
    (is      (wild-match? [1 :* 3]
                          [1 2  3]
                          [1 9  3] ))
    (is (not (wild-match? [1 2  3]
                          [1 2  9] )))
    (is (not (wild-match? [1 2   ]
                          [1 2  9] )))
    (is (not (wild-match? [1 2  3]
                          [1 2   ] )))

    (is      (wild-match? [1  [2 3]]
                          [1  [2 3]] ))
    (is      (wild-match? [:* [2 3]]
                          [1  [2 3]] ))
    (is      (wild-match? [:* [2 3]]
                          [1  [2 3]]
                          [9  [2 3]] ))
    (is      (wild-match? [1  [2 :*]]
                          [1  [2 33]]
                          [1  [2 99]] ))
    (is      (wild-match? [1  :*]
                          [1   2]
                          [1  [2 3]] ))
    (is (not (wild-match? [1  [2 3]]
                          [1  [2 9]] )))
  )
  (testing "maps"
    (is (wild-match? {:a 1 } {:a 1} ))
    (is (wild-match? {:a :*} {:a 1} ))
    (is (wild-match? {:a :*} {:a 1 } {:a 1 } ))
    (is (wild-match? {:a :*} {:a 1 } {:a 9 } ))
    (is (wild-match? {:a :*} {:a :*} {:a 9 } ))
    (is (wild-match? {:a :*} {:a :*} {:a :*} ))

    (is (not (wild-match? {:a 1 } {:a 9} )))
    (is (not (wild-match? {:a 1 } {:a 1 :b 2} )))
    (is (not (wild-match? {:a :*} {:b 1} )))
    (is (not (wild-match? {:a :*} {:a 1} {:b 1} )))
    (is (not (wild-match? {:a :*} {:a 1 :b 2} )))

    (let [vv {:a 1  :b {:c 3}}
          tt {:a 1  :b {:c 3}}
          w2 {:a :* :b {:c 3}}
          w5 {:a 1  :b {:c :*}}
          zz {:a 2  :b {:c 3}}
    ]
      (is (wild-match? tt vv))
      (is (wild-match? w2 vv))
      (is (wild-match? w5 vv))
      (is (not (wild-match? zz vv)))
    )
  )
  (testing "vecs & maps 1"
    (let [vv [:a 1  :b {:c  3} ]
          tt [:a 1  :b {:c  3} ]
          w1 [:* 1  :b {:c  3} ]
          w2 [:a :* :b {:c  3} ]
          w3 [:a 1  :* {:c  3} ]
          w5 [:a 1  :b {:c :*} ]
          zz [:a 2  :b {:c  3} ]
    ]
      (is (wild-match? tt vv))
      (is (wild-match? w1 vv))
      (is (wild-match? w2 vv))
      (is (wild-match? w3 vv))
      (is (wild-match? w5 vv))
      (is (not (wild-match? zz vv)))
    )
  )
  (testing "vecs & maps 2"
    (let [vv {:a 1  :b [:c  3] }
          tt {:a 1  :b [:c  3] }
          w2 {:a :* :b [:c  3] }
          w4 {:a 1  :b [:*  3] }
          w5 {:a 1  :b [:c :*] }
          z1 {:a 2  :b [:c  3] }
          z2 {:a 1  :b [:c  9] }
    ]
      (is (wild-match? tt vv))
      (is (wild-match? w2 vv))
      (is (wild-match? w4 vv))
      (is (wild-match? w5 vv))
      (is (not (wild-match? z1 vv)))
      (is (not (wild-match? z2 vv)))
    )
  )
  (testing "sets"
    (is      (wild-match? #{1} #{1} ))
    (is (not (wild-match? #{1} #{9} )))
    (is (not (wild-match? #{1} #{:a :b} )))
    (is      (wild-match? #{1  #{:a :b}}
                          #{1  #{:a :b} }))
    (is (not (wild-match? #{1  #{:a :c}}
                          #{1  #{:a :x} })))
  )
)

;---------------------------------------------------------------------------------------------------

(deftest ^:deprecated ^:no-doc t-str->lines
  (let [s1    "  hello there 
                 again
                 and again!   "
        r1     ["hello there"
                "again"
                "and again!"]
  ]
    (is (= r1 (map str/trim (str->lines s1))))
    (is (= r1 (map str/trim (str/split-lines s1))))))

