;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.core
  (:use tupelo.test )
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [tupelo.misc :as tm]
    [tupelo.string :as ts]
  ))
(t/refer-tupelo)

; (s/instrument-all)
; (s/instrument #'tupelo.core/truthy?)  ; instrument just one var

;-----------------------------------------------------------------------------
; Java version stuff

(defn fn-any [] 42)
(defn fn7 [] (t/if-java-1-7-plus
               7
               (throw (RuntimeException. "Unimplemented prior to Java 1.7: "))))
(defn fn8 [] (t/if-java-1-8-plus
               8
               (throw (RuntimeException. "Unimplemented prior to Java 1.8: "))))

(dotest
  (when (t/is-java-1-7?)
    (throws? (fn8)))

  (when (t/is-java-1-8-plus?)
    (is= 8 (fn8)))

  (is= 7 (fn7))
  (is= 42 (fn-any))

  (with-redefs [t/java-version (constantly "1.7")]
    (is   (t/java-version-min? "1.7"))
    (isnt (t/java-version-min? "1.7.0"))
    (isnt (t/java-version-min? "1.7.0-b1234"))
    (isnt (t/java-version-min? "1.8"))

    (is   (t/java-version-matches? "1.7"))
    (isnt (t/java-version-matches? "1.7.0"))
    (isnt (t/java-version-matches? "1.7.0-b1234"))
    (isnt (t/java-version-matches? "1.8"))
    )
  (with-redefs [t/java-version (constantly "1.7.0")]
    (is   (t/java-version-min? "1.7"))
    (is   (t/java-version-min? "1.7.0"))
    (isnt (t/java-version-min? "1.7.0-b1234"))
    (isnt (t/java-version-min? "1.8"))

    (is   (t/java-version-matches? "1.7"))
    (is   (t/java-version-matches? "1.7.0"))
    (isnt (t/java-version-matches? "1.7.0-b1234"))
    (isnt (t/java-version-matches? "1.8"))
    )
  (with-redefs [t/java-version (constantly "1.7.0-b1234")]
    (is   (t/java-version-min? "1.7"))
    (is   (t/java-version-min? "1.7.0"))
    (is   (t/java-version-min? "1.7.0-b1234"))
    (isnt (t/java-version-min? "1.8"))

    (is   (t/java-version-matches? "1.7"))
    (is   (t/java-version-matches? "1.7.0"))
    (is   (t/java-version-matches? "1.7.0-b1234"))
    (isnt (t/java-version-matches? "1.8"))
    )

  (with-redefs [t/java-version (constantly "1.7") ]
    (when false
      (println "testing java 1.7")
      (spyx (t/is-java-1-7?))
      (spyx (t/is-java-1-8?))
      (spyx (t/is-java-1-7-plus?))
      (spyx (t/is-java-1-8-plus?)))

    (is   (t/is-java-1-7?))
    (is   (t/is-java-1-7-plus?))
    (isnt (t/is-java-1-8?))
    (isnt (t/is-java-1-8-plus?))
    )

  (with-redefs [t/java-version (constantly "1.8") ]
    (when false
      (println "testing java 1.8")
      (spyx (t/is-java-1-7?))
      (spyx (t/is-java-1-8?))
      (spyx (t/is-java-1-7-plus?))
      (spyx (t/is-java-1-8-plus?)))

    (isnt (t/is-java-1-7?))
    (is   (t/is-java-1-7-plus?))
    (is   (t/is-java-1-8?))
    (is   (t/is-java-1-8-plus?))
    )
  )

;-----------------------------------------------------------------------------
; Clojure version stuff

(dotest
  (binding [*clojure-version* {:major 1 :minor 7}]
    (is   (t/is-clojure-1-7-plus?))
    (isnt (t/is-clojure-1-8-plus?))
    (isnt (t/is-clojure-1-9-plus?))
    (is   (t/is-pre-clojure-1-8?))
    (is   (t/is-pre-clojure-1-9?)))
  (binding [*clojure-version* {:major 1 :minor 8}]
    (is   (t/is-clojure-1-7-plus?))
    (is   (t/is-clojure-1-8-plus?))
    (isnt (t/is-clojure-1-9-plus?))
    (isnt (t/is-pre-clojure-1-8?))
    (is   (t/is-pre-clojure-1-9?)))
  (binding [*clojure-version* {:major 1 :minor 9}]
    (is   (t/is-clojure-1-7-plus?))
    (is   (t/is-clojure-1-8-plus?))
    (is   (t/is-clojure-1-9-plus?))
    (isnt (t/is-pre-clojure-1-8?))
    (isnt (t/is-pre-clojure-1-9?)))
)

;-----------------------------------------------------------------------------
; spy stuff
(dotest
  (is= "(+ 2 3) => 5"
    (ts/collapse-whitespace
      (with-out-str
        (is= 5 (spyx (+ 2 3))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace   "(inc 0) => 1
                                  (inc 1) => 2
                                  (inc 2) => 3 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 3 (spyx (inc 0)
                     (inc 1)
                     (inc 2))))))

  ; #todo -> readme
  (is= (ts/collapse-whitespace   ":some-kw
                                  (inc 1) => 2
                                  (inc 2) => 3 " )
       (ts/collapse-whitespace
         (with-out-str
           (is= 3    (spyx :some-kw
                           (inc 1)
                           (inc 2))))))
)

; #todo blog about this nested (is= ...) testing technique
(dotest
  (is=
    (ts/collapse-whitespace  " a => 1
                               b => 5
                               (-> (inc a) (* 2) inc) => 5 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 13
          (t/spy-let [a (inc 0)
                      b (+ 2 3)]
            (spyx (-> (inc a) (* 2) inc))
            (-> b (* 2) (+ 3)))))))

  (is= (ts/collapse-whitespace  " a => 1
                                  b => 5 " )
    (ts/collapse-whitespace
      (with-out-str
        (is= 17
          (t/spy-let [a (inc 0)
                      b (+ 2 3)]
            (-> b (* (inc a)) (+ 7)))))))

)


(dotest
  (testing "basic usage"
    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result)) ]
      (is= "hi => 5"
          (ts/collapse-whitespace (with-out-str (spy (side-effect-add! 2 3) :msg "hi"))) )
      (is= "hi => 5"
          (ts/collapse-whitespace (with-out-str (spy :msg "hi"  (side-effect-add! 2 3)))) )
      (is= "(side-effect-add! 2 3) => 5"
          (ts/collapse-whitespace (with-out-str (spyx (side-effect-add! 2 3)))) )
      (is= 15 @side-effect-cum-sum))

    (is= "first => 5 second => 25"
        (ts/collapse-whitespace
          (with-out-str (-> 2
                            (+ 3)
                            (spy :msg "first" )
                            (* 5)
                            (spy :msg "second") ))))
    (is= "first => 5 second => 25"
        (ts/collapse-whitespace
          (with-out-str (->> 2
                             (+ 3)
                             (spy :msg "first" )
                             (* 5)
                             (spy :msg "second") ))))

    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result))
    ]
      (is= "value => 5"
          (ts/collapse-whitespace (with-out-str (spy (side-effect-add! 2 3) :msg "value"))))
      (is= "value => 5"
          (ts/collapse-whitespace (with-out-str (spy :msg "value"  (side-effect-add! 2 3)))))
      (is= 10 @side-effect-cum-sum)

      (is= "value => 5" (ts/collapse-whitespace (with-out-str (spy "value" (+ 2 3) ))))
      (is=   "spy => 5" (ts/collapse-whitespace (with-out-str (spy         (+ 2 3) ))))

      (is= "(str \"abc\" \"def\") => \"abcdef\""
          (ts/collapse-whitespace (with-out-str (spyx (str "abc" "def") ))))

      (throws? IllegalArgumentException  (spy "some-msg" 42 :msg))
    )))

(dotest
  (let [val1  (into (sorted-map) {:a 1 :b 2})
        val2  (+ 2 3) ]
    (is= "val1 => clojure.lang.PersistentTreeMap->{:a 1, :b 2}"
        (ts/collapse-whitespace (with-out-str (spyxx val1 )))  )

    (is= "val2 => java.lang.Long->5"
        (ts/collapse-whitespace (with-out-str (spyxx val2 ))) )
  ))

(dotest
  (let [fn2   (fn []  (with-spy-indent
                        (spy :msg "msg2" (+ 2 3))))
        fn1   (fn []  (with-spy-indent
                        (spy :msg "msg1" (+ 2 3))
                        (fn2)))
        fn0   (fn [] (spy :msg "msg0" (+ 2 3))) ]
    (is= "msg2 => 5"            (ts/collapse-whitespace (with-out-str (fn2))))
    (is= "msg1 => 5 msg2 => 5"  (ts/collapse-whitespace (with-out-str (fn1))))
    (is= "msg0 => 5"            (ts/collapse-whitespace (with-out-str (fn0))))
    ))

; (dotest
; )

(dotest
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
        (is (has-none? falsey? truthies))
        (is (has-none? truthy? falsies))

        (isnt (every? truthy? [true false]))
        (is (every? truthy? [true "FALSE"]))
        (is (every? truthy? [true ]))
        (is (every? truthy? []))))

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

(dotest
  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [notties   (keep-if not-nil? data)
            nillies   (drop-if not-nil? data) ]
        (is (and  (= notties [true :a 'my-symbol 1 "hello" \x false] )
                  (= nillies [nil] )))
        (is (every?    not-nil? notties))
        (is (every?        nil? [nil] ))
        (is (has-none?     nil? notties))
        (is (has-none? not-nil? nillies))))

    (testing "improved usage"
      (let [count-if (comp count keep-if) ]
        (let [num-valid-1     (count-if some?    data)  ; awkward phrasing, doesn't feel natural
              num-valid-2     (count-if not-nil? data)  ; matches intent much better
              num-nil         (count-if nil?     data) ]    ; intent is plain
          (is (and  (= 7 num-valid-1 num-valid-2 )
                    (= 1 num-nil) )))))))

(dotest
  (is= true   (t/has-some? odd? [1 2 3] ) )
  (is= false  (t/has-some? odd? [2 4 6] ) )
  (is= false  (t/has-some? odd? []      ) )

  (is= false  (has-none? odd? [1 2 3] ) )
  (is= true   (has-none? odd? [2 4 6] ) )
  (is= true   (has-none? odd? []      ) ))

(dotest
  (is (every?      not-empty? ["one" [1] '(1) {:1 1} #{1}     ] ))
  (is (has-none?   not-empty? [ ""   [ ] '( ) {}     #{ }  nil] ))

  (is= (map not-empty? ["1" [1] '(1) {:1 1} #{1} ] )
         [true true true true true]  )
  (is= (map not-empty? ["" [] '() {} #{} nil] )
         [false false false false false false ] )

  (is= (keep-if not-empty?  ["1" [1] '(1) {:1 1} #{1} ] )
                            ["1" [1] '(1) {:1 1} #{1} ] )
  (is= (drop-if not-empty?  [""  []  '()  {}     #{}  nil] )
                            [""  []  '()  {}     #{}  nil] )

  (throws? IllegalArgumentException (not-empty? 5))
  (throws? IllegalArgumentException (not-empty? 3.14)))

(dotest
  (testing "vecs"
    (let [coll (range 3)]
      (isnt (contains-elem? coll -1))
      (is   (contains-elem? coll  0))
      (is   (contains-elem? coll  1))
      (is   (contains-elem? coll  2))
      (isnt (contains-elem? coll  3))
      (isnt (contains-elem? coll  nil)))

    (let [coll [ 1 :two "three" \4]]
      (isnt (contains-elem? coll  :no-way))
      (isnt (contains-elem? coll  nil))
      (is   (contains-elem? coll  1))
      (is   (contains-elem? coll  :two))
      (is   (contains-elem? coll  "three"))
      (is   (contains-elem? coll  \4)))

    (let [coll [:yes nil 3]]
      (isnt (contains-elem? coll  :no-way))
      (is   (contains-elem? coll  :yes))
      (is   (contains-elem? coll  nil))))

  (testing "maps"
    (let [coll {1 :two "three" \4}]
      (isnt (contains-elem? coll nil ))
      (isnt (contains-elem? coll [1 :no-way] ))
      (is   (contains-elem? coll [1 :two]))
      (is   (contains-elem? coll ["three" \4])))
    (let [coll {1 nil "three" \4}]
      (isnt (contains-elem? coll [nil 1] ))
      (is   (contains-elem? coll [1 nil] )))
    (let [coll {nil 2 "three" \4}]
      (isnt (contains-elem? coll [1 nil] ))
      (is   (contains-elem? coll [nil 2] ))))

  (testing "sets"
    (let [coll #{1 :two "three" \4}]
      (isnt (contains-elem? coll  :no-way))
      (is   (contains-elem? coll  1))
      (is   (contains-elem? coll  :two))
      (is   (contains-elem? coll  "three"))
      (is   (contains-elem? coll  \4)))

    (let [coll #{:yes nil}]
      (isnt (contains-elem? coll  :no-way))
      (is   (contains-elem? coll  :yes))
      (is   (contains-elem? coll  nil)))))

(dotest
  (is   (contains-key?  {:a 1 :b 2} :a))
  (is   (contains-key?  {:a 1 :b 2} :b))
  (isnt (contains-key?  {:a 1 :b 2} :x))
  (isnt (contains-key?  {:a 1 :b 2} :c))
  (isnt (contains-key?  {:a 1 :b 2}  1))
  (isnt (contains-key?  {:a 1 :b 2}  2))

  (is   (contains-key?  {:a 1 nil   2} nil))
  (isnt (contains-key?  {:a 1 :b  nil} nil))
  (isnt (contains-key?  {:a 1 :b    2} nil))

  (is   (contains-key? #{:a 1 :b 2} :a))
  (is   (contains-key? #{:a 1 :b 2} :b))
  (is   (contains-key? #{:a 1 :b 2}  1))
  (is   (contains-key? #{:a 1 :b 2}  2))
  (isnt (contains-key? #{:a 1 :b 2} :x))
  (isnt (contains-key? #{:a 1 :b 2} :c))

  (is   (contains-key? #{:a 5 nil   "hello"} nil))
  (isnt (contains-key? #{:a 5 :doh! "hello"} nil))

  (throws? (contains-key? [:a 1 :b 2] :a))
  (throws? (contains-key? [:a 1 :b 2]  1)))

(dotest
  (is   (contains-val? {:a 1 :b 2} 1))
  (is   (contains-val? {:a 1 :b 2} 2))
  (isnt (contains-val? {:a 1 :b 2} 0))
  (isnt (contains-val? {:a 1 :b 2} 3))
  (isnt (contains-val? {:a 1 :b 2} :a))
  (isnt (contains-val? {:a 1 :b 2} :b))

  (is   (contains-val? {:a 1 :b nil} nil))
  (isnt (contains-val? {:a 1 nil  2} nil))
  (isnt (contains-val? {:a 1 :b   2} nil))

  (throws? (contains-val?  [:a 1 :b 2] 1))
  (throws? (contains-val? #{:a 1 :b 2} 1)))

;(sp/def ::vector (sp/coll-of clj/any :kind vector?))
;(dotest
;  (is   (sp/valid? ::vector [1 2 3]))
;  (isnt (sp/valid? ::vector '(1 2 3)))
;  (isnt (sp/valid? ::vector {:a 1}))
; ;(spyx (sp/exercise ::vector))
;)

(dotest
  (is= (forv [x (range 4)] (* x x))
         [0 1 4 9] )
  (is= (forv [x (range 23)] (* x x))
       (for  [x (range 23)] (* x x)))
  (is= (forv [x (range 5)  y (range 2 9)] (str x y))
       (for  [x (range 5)  y (range 2 9)] (str x y))))

(dotest
  (let [xs [1 2 3]
        ys [10 20 30]]
    (is= [11 22 33]
      (map-let [x xs y ys] (+ x y))
      (map-let* {:lazy false :strict false} [x xs y ys] (+ x y))
      (map-let* {:lazy false :strict true}  [x xs y ys] (+ x y))
      (map-let* {:lazy true :strict false}  [x xs y ys] (+ x y))
      (map-let* {:lazy true :strict true}   [x xs y ys] (+ x y)))
    (is (instance? clojure.lang.PersistentVector
          (map-let* {:lazy false :strict true} [x xs y ys] (+ x y))))
    (is (instance? clojure.lang.LazySeq
          (map-let* {:lazy true :strict true} [x xs y ys] (+ x y)))))
  (let [xs [1 2 3]
        ys [10 20 30 40]]
    (throws?        (map-let                  [x xs y ys] (+ x y)))
    (throws?        (map-let* {:strict true}  [x xs y ys] (+ x y)))
    (is= [11 22 33] (map-let* {:strict false} [x xs y ys] (+ x y)))))

(dotest
  (is= (vector [])               [[]] )
  (is= (mapv identity [] [])      []  )

  (is= [[:a 0] [:b 1] [:c 2]]
    (zip-lazy [:a :b :c] [0 1 2])
    (zip-lazy [:a :b :c] (range)))
  (is= (zip-lazy [:a :b :c] [1 2 3])   [[:a 1] [:b 2] [:c 3]] )
  (is= (zip-lazy [:a] [1])             [[:a 1]] )
  (is= (zip-lazy [] [])                []  )
  (is= (zip-lazy [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]] )
  (is (instance? clojure.lang.LazySeq
        (zip-lazy [:a :b :c] (range))))

  (is= (zip [:a :b :c] [1 2 3])   [[:a 1] [:b 2] [:c 3]] )
  (is= (zip [:a] [1])             [[:a 1]] )
  (is= (zip [] [])                []  )
  (is= (zip [:A :B :C] [:a :b :c] [1 2 3])
    [[:A :a 1] [:B :b 2] [:C :c 3]] )
  (throws? (zip [:a :b :c] [1 2 3 4]))
  (is= (zip* {:strict false} [:a :b :c] [1 2 3 4]) [[:a 1] [:b 2] [:c 3]] )

  (is (instance? clojure.lang.PersistentVector
        (zip*  {:trunc false} [:a :b :c] [1 2 3])))
  (let [keys   [:a :b :c]
        vals   [1 2 3]
        result (atom [])]
       (doseq [[k i] (zip keys vals)]
         (swap! result append {k i}))
    (is= [{:a 1} {:b 2} {:c 3}] @result)))

(dotest
  (is= (indexed [:a :b :c]) [[0 :a] [1 :b] [2 :c]])
  (is= [[0 0] [1 2] [2 4] [3 6] [4 8]]
    (take 5 (indexed (map #(* 2 %) (range))))) ; can work with infinite lazy lists
  (is= (indexed [:a :b :c]  (map #(+ 10 %) (range)))
          [ [0 :a 10]
            [1 :b 11]
            [2 :c 12] ] )
  (is= (take 5 (indexed (map #(+ 10 %) (range))))
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

  (is= (glue [1 2]  [3 4] [5 6])        [1 2 3 4 5 6])
  (is= (glue [1 2] '(3 4) [5 6])        [1 2 3 4 5 6])
  (is= (glue [] [1 2] )                [1 2] )
  (is= (glue [1 2] [] )                [1 2] )
  (is= (glue [] [1 2] [] )             [1 2] )

  (is= (glue '(1 2) '(3 4) '(5 6))        [1 2 3 4 5 6])
  (is= (glue '(1 2)  [3 4] '(5 6))        [1 2 3 4 5 6])
  (is= (glue  [1 2] '(3 4) '(5 6))        [1 2 3 4 5 6])
  (is= (glue '() '(1 2) )                 [1 2] )
  (is= (glue '(1 2) '() )                 [1 2] )
  (is= (glue '() '(1 2) '() )             [1 2] )

  (is= (glue (range 3) (range 5))      [0 1 2 0 1 2 3 4] )

  (is= (glue {:a 1} {:b 2} {:c 3})      {:a 1 :c 3 :b 2})
  (is= (glue {:a 1} {:b 2} )            {:a 1 :b 2})
  (is= (glue {:a 1} {} )                {:a 1} )
  (is= (glue {} {:a 1} )                {:a 1} )
  (is= (glue {} {:a 1} {} )             {:a 1} )

  (is= (glue #{1 2} #{3 4} #{6 5})     #{1 2 6 5 3 4})
  (is= (glue #{} #{1 2} )              #{1 2} )
  (is= (glue #{1 2} #{} )              #{1 2} )
  (is= (glue #{} #{1 2} #{} )          #{1 2} )

  (is= (glue (sorted-map) {:a 1} {:b 2} {:c 3})   {:a 1 :b 2 :c 3} )
  (is= (glue (sorted-set) #{1 2} #{3 4} #{6 5})   #{1 2 3 4 5 6})

  (is=      (glue (sorted-map) (hash-map :a 1   :b 2   :c 3   :d 4   :e 5   :f 6))
                                        {:a 1   :b 2   :c 3   :d 4   :e 5   :f 6} )
  (is= (seq (glue (sorted-map) (hash-map :a 1   :b 2   :c 3   :d 4   :e 5   :f 6)))
                                      [ [:a 1] [:b 2] [:c 3] [:d 4] [:e 5] [:f 6] ] )

  (is= (glue  {:band :VanHalen :singer :Dave} {:singer :Sammy} )
              {:band :VanHalen                 :singer :Sammy} )

  (is= (glue \a )           "a" )
  (is= (glue "a")           "a" )
  (is= (glue \a "")         "a" )
  (is= (glue "" "a")        "a" )
  (is= (glue \a  \b)        "ab" )
  (is= (glue "a" "b")       "ab" )
  (is= (glue "a" \b)        "ab" )
  (is= (glue \a  "b")       "ab" )
  (is= (glue "" "a" \b)     "ab" )
  (is= (glue "" \a  "b")    "ab" )
  (is= (glue "a" "" \b)     "ab" )
  (is= (glue \a  "" "b")    "ab" )
  (is= (glue "a" \b  "")    "ab" )
  (is= (glue \a  "b" "")    "ab" )
  (is= (glue \a  "b" "")    "ab" )
  (is= (glue "I" \space "like " \a " nap!" )    "I like a nap!" )
  (is= (apply glue [ "I" \space "like " \a " nap!"] )    "I like a nap!" )

  (throws? (glue   [1 2]     {:a 1} ))
  (throws? (glue  '(1 2)     {:a 1} ))
  (throws? (glue   [1 2]    #{:a 1} ))
  (throws? (glue  '(1 2)    #{:a 1} ))
  (throws? (glue   [1 2]    "hello" ))
  (throws? (glue  '(1 2)    "hello" ))
  (throws? (glue   {:a 1}   #{:a 1} ))
  (throws? (glue   {:a 1}   "hello" ))
  (throws? (glue   #{:a 1}  "hello" ))
  (throws? (glue   [1 2]     nil    ))
  (throws? (glue  '(1 2)     nil    ))
  (throws? (glue   {:a 1}    nil    ))
  (throws? (glue   #{:a 1}   nil    ))
  (throws? (glue   "hello"   nil    ))
)

(dotest
  (throws?            (append  1 2        ))
  (throws?            (append [1 2]       ))
  (throws?            (append nil   3     ))
  (is= [1 2 3    ]    (append [1 2] 3     ))
  (is= [1 2 3 4  ]    (append [1 2] 3 4   ))
  (is= [1 2 3 4 5]    (append [1 2] 3 4 5 ))

  (throws?            (append '(1 2)       ))
  (is= [1 2 3    ]    (append '(1 2) 3     ))
  (is= [1 2 3 4  ]    (append '(1 2) 3 4   ))
  (is= [1 2 3 4 5]    (append '(1 2) 3 4 5 ))

  (throws?            (append   {:a 1} 99     ))
  (throws?            (append   {:a 1} {:b 2} ))
  (throws?            (append  #{:a 1} 99     ))
  (throws?            (append  #{:a 1} #{99}  ))

  (testing "old conjv tests"
    (is= [  2  ]    (append  []  2   ))
    (is= [  2  ]    (append '()  2   ))
    (is= [  2 3]    (append  []  2  3))
    (is= [  2 3]    (append '()  2  3))

    (is= [1 2 3]    (append  [1] 2  3))
    (is= [1 2 3]    (append '(1) 2  3))
    (is= [1 2 3]    (append  [1  2] 3))
    (is= [1 2 3]    (append '(1  2) 3))

    (is= [1 2 3 4]  (append  [1  2] 3 4))
    (is= [1 2 3 4]  (append '(1  2) 3 4))
    (is= [1 2 3 4]  (append  [1] 2  3 4))
    (is= [1 2 3 4]  (append '(1) 2  3 4))

    (is= [[1 2] [3 4] [5 6]] (append  [[1 2] [3 4]]  [5 6] ))

    (is= [0 1 2 3 4 5] (append (range 4) 4 5))
    (is= [0 1 2 3 4 5] (apply append [0] (range 1 6)))))

(dotest
  (throws?            (prepend       [2 1] ))
  (throws?            (prepend     3  nil  ))
  (is= [    3 2 1]    (prepend     3 [2 1] ))
  (is= [  4 3 2 1]    (prepend   4 3 [2 1] ))
  (is= [5 4 3 2 1]    (prepend 5 4 3 [2 1] ))

  (throws?            (prepend       '(2 1) ))
  (is= [    3 2 1]    (prepend     3 '(2 1) ))
  (is= [  4 3 2 1]    (prepend   4 3 '(2 1) ))
  (is= [5 4 3 2 1]    (prepend 5 4 3 '(2 1) ))

  (throws?            (prepend   99     {:a 1} ))
  (throws?            (prepend   {:b 2} {:a 1} ))
  (throws?            (prepend   99    #{:a 1} ))
  (throws?            (prepend  #{99}  #{:a 1} )))

(dotest
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 2 3 4 5 6 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 (unwrap [2 3 4 5 6 7 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 (unwrap [2 (unwrap [3 4 5 6 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 (unwrap [2 (unwrap [3 (unwrap [4 5 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 (unwrap [2 (unwrap [3 (unwrap [4 (unwrap [5]) 6]) 7]) 8]) 9))
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9))
  (is= [1 2 3 4 5 6 7 8 9] (->vector 1 (unwrap [2 3 (unwrap [4 5 6]) 7 8]) 9))

  (is= [1 2 3 [4  5  6] 7 8 9] (->vector 1 (unwrap [2 3 [4  5  6] 7 8]) 9))
  (is= [1 2 3 [4 [5] 6] 7 8 9] (->vector 1 (unwrap [2 3 [4 [5] 6] 7 8]) 9))

  (is= [1 [2 3 4 [5] 6 7 8] 9] (->vector 1 `(2 3 ~(unwrap [4 [5] 6]) 7 8) 9))
  (is= [1 [2 3 4 [5] 6 7 8] 9] (->vector 1  [2 3  (unwrap [4 [5] 6]) 7 8] 9))

  (is= [1 2 3 4 5 6 7 8 9] (glue   [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat [1] [2] [3] [4 5 6] [7] [8] [9]))
  (is= [1 2 3 4 5 6 7 8 9] (glue   [1   2   3] [4 5 6] [7   8   9]))
  (is= [1 2 3 4 5 6 7 8 9] (concat [1   2   3] [4 5 6] [7   8   9])))

(dotest
  (isnt (increasing? [1 2] [1]))
  (isnt (increasing? [1 2] [1 1]))
  (isnt (increasing? [1 2] [1 2]))
  (is   (increasing? [1 2] [1 2 nil]))
  (is   (increasing? [1 2] [1 2 3]))
  (is   (increasing? [1 2] [1 3]))
  (is   (increasing? [1 2] [2 1]))
  (is   (increasing? [1 2] [2]))

  (isnt (increasing-or-equal? [1 2] [1]))
  (isnt (increasing-or-equal? [1 2] [1 1]))
  (is   (increasing-or-equal? [1 2] [1 2]))
  (is   (increasing-or-equal? [1 2] [1 2 nil]))
  (is   (increasing-or-equal? [1 2] [1 2 3]))
  (is   (increasing-or-equal? [1 2] [1 3]))
  (is   (increasing-or-equal? [1 2] [2 1]))
  (is   (increasing-or-equal? [1 2] [2]))
)

(dotest
  (let [map1  { :a 1 :b 2 :c nil
               nil :NIL
               "hi" "hello"
               5 "five"}]
    (is= 1           (grab :a   map1))
    (is= 2           (grab :b   map1))
    (is= nil         (grab :c   map1))
    (is= :NIL        (grab nil  map1))
    (is= "hello"     (grab "hi"  map1))
    (is= "five"      (grab 5  map1))
    (throws?  (grab :z map1))
    (throws?  (grab 42 map1))
    ))

(dotest
  (testing "basic usage"
    (let [map1  {:a1 "a1"
                 :a2 { :b1 "b1"
                       :b2 { :c1 "c1"
                             :c2 "c2" }}
                 nil "NIL"
                 :nil nil} ]
      (is= "a1"  (fetch-in map1 [:a1]))
      (is= "b1"  (fetch-in map1 [:a2 :b1]))
      (is= "c1"  (fetch-in map1 [:a2 :b2 :c1]))
      (is= "c2"  (fetch-in map1 [:a2 :b2 :c2]))
      (is= "NIL" (fetch-in map1 [nil]))
      (is= nil   (fetch-in map1 [:nil]))
      (throws?   (fetch-in map1 [:a9]))
      (throws?   (fetch-in map1 [:a2 :b9]))
      (throws?   (fetch-in map1 [:a2 :b2 :c9])))))

(dotest
  (let [mm    {:a { :b { :c "c" }}} ]
    (is= (dissoc-in mm []         )          mm )
    (is= (dissoc-in mm [:a]       )          {} )
    (is= (dissoc-in mm [:a :b]    )          {:a  {}} )
    (is= (dissoc-in mm [:a :b :c] )          {:a  { :b  {}}} )
    (is= (dissoc-in mm [:a :x :y] )          {:a  { :b  { :c "c" }
                                                         :x  nil }} )
    (is= (dissoc-in mm [:a :x :y :z] )       {:a  { :b  { :c "c" }
                                                    :x  { :y nil }}} )
    (is= (dissoc-in mm [:k1 :k2 :k3 :kz] )   {:a  { :b  { :c  "c" }}
                                              :k1 { :k2 { :k3 nil }}} ))
  (let [mm    {:a1 "a1"
               :a2 { :b1 "b1"
                     :b2 { :c1 "c1"
                           :c2 "c2" }}} ]
    (is= (dissoc-in mm [:a1] )
              {:a2 { :b1 "b1"
                     :b2 { :c1 "c1"
                           :c2 "c2" }}} )
    (is= (dissoc-in mm [:a2] )
              {:a1 "a1" } )
    (is= (dissoc-in mm [:a2 :b1] )
              {:a1 "a1"
               :a2 { :b2 { :c1 "c1"
                           :c2 "c2" }}} )
    (is= (dissoc-in mm [:a2 :b2] )
              {:a1 "a1"
               :a2 { :b1 "b1" }} )
    (is= (dissoc-in mm [:a2 :b2 :c1] )
              {:a1 "a1"
               :a2 { :b1 "b1"
                     :b2 { :c2 "c2" }}} )
    (is= (dissoc-in mm [:a2 :b2 :c2] )
              {:a1 "a1"
               :a2 { :b1 "b1"
                     :b2 { :c1 "c1" }}} )))

(dotest
  (is= [nil    ] (t/conjv nil   nil ))
  (is= [      9] (t/conjv nil     9 ))

  (is= [1      ] (t/conjv nil 1     ))
  (is= [1 2    ] (t/conjv nil 1 2   ))
  (is= [1 2 3  ] (t/conjv nil 1 2 3 ))

  (is= [1      ] (t/conjv  [] 1     ))
  (is= [1 2    ] (t/conjv  [] 1 2   ))
  (is= [1 2 3  ] (t/conjv  [] 1 2 3 ))
  (is= [1      ] (t/conjv '() 1     ))
  (is= [1 2    ] (t/conjv '() 1 2   ))
  (is= [1 2 3  ] (t/conjv '() 1 2 3 ))

  (is= [      9] (t/conjv  [     ] 9 ))
  (is= [1     9] (t/conjv  [1    ] 9 ))
  (is= [1 2   9] (t/conjv  [1 2  ] 9 ))
  (is= [1 2 3 9] (t/conjv  [1 2 3] 9 ))

  (is= [      9] (t/conjv '(     ) 9 ))
  (is= [1     9] (t/conjv '(1    ) 9 ))
  (is= [1 2   9] (t/conjv '(1 2  ) 9 ))
  (is= [1 2 3 9] (t/conjv '(1 2 3) 9 ))
)

(dotest
  (let [map1  {:a 1 :b 2 :c 3 :d 4 :e 5}]
    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (submap-by-keys map1 #{ :a :b :c :d :e } ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (submap-by-keys map1 #{    :b :c :d :e } ))
    (is= {          :c 3 :d 4 :e 5} (submap-by-keys map1 #{       :c :d :e } ))
    (is= {               :d 4 :e 5} (submap-by-keys map1 #{          :d :e } ))
    (is= {                    :e 5} (submap-by-keys map1 #{             :e } ))
    (is= {                        } (submap-by-keys map1 #{                } ))
    (throws? (submap-by-keys map1 #{:z}))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (submap-by-keys map1 #{ :a :b :c :d :e  :z } :missing-ok))
    (is= {     :b 2 :c 3 :d 4 :e 5} (submap-by-keys map1 #{    :b :c :d :e  :z } :missing-ok))
    (is= {          :c 3 :d 4 :e 5} (submap-by-keys map1 #{       :c :d :e  :z } :missing-ok))
    (is= {               :d 4 :e 5} (submap-by-keys map1 #{          :d :e  :z } :missing-ok))
    (is= {                    :e 5} (submap-by-keys map1 #{             :e  :z } :missing-ok))
    (is= {                        } (submap-by-keys map1 #{                 :z } :missing-ok))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (submap-by-vals map1 #{ 1 2 3 4 5 } ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (submap-by-vals map1 #{   2 3 4 5 } ))
    (is= {          :c 3 :d 4 :e 5} (submap-by-vals map1 #{     3 4 5 } ))
    (is= {               :d 4 :e 5} (submap-by-vals map1 #{       4 5 } ))
    (is= {                    :e 5} (submap-by-vals map1 #{         5 } ))
    (is= {                        } (submap-by-vals map1 #{           } ))
    (throws? (submap-by-vals map1 #{ 99 }))

    (is= {:a 1 :b 2 :c 3 :d 4 :e 5} (submap-by-vals map1 #{ 1 2 3 4 5  99 } :missing-ok ))
    (is= {     :b 2 :c 3 :d 4 :e 5} (submap-by-vals map1 #{   2 3 4 5  99 } :missing-ok ))
    (is= {          :c 3 :d 4 :e 5} (submap-by-vals map1 #{     3 4 5  99 } :missing-ok ))
    (is= {               :d 4 :e 5} (submap-by-vals map1 #{       4 5  99 } :missing-ok ))
    (is= {                    :e 5} (submap-by-vals map1 #{         5  99 } :missing-ok ))
    (is= {                        } (submap-by-vals map1 #{            99 } :missing-ok ))
    (is= {                        } (submap-by-vals map1 #{               } :missing-ok ))

    (is= { 0 :even 2 :even } (submap-by-vals
                               { 0 :even 1 :odd 2 :even 3 :odd }
                               #{ :even } ))
    (is= { 0 :even 2 :even } (submap-by-vals
                               { 0 :even 1 :odd 2 :even 3 :odd }
                               #{ :even :prime } :missing-ok )))
)

(dotest
  (is= :23 (int->kw  23))
  (is=  23 (kw->int :23))
)

(dotest
  (is= 42 (only [42]))
  (is= :x (only [:x]))
  (is= "hello" (only ["hello"] ))
  (throws? IllegalArgumentException (only []))
  (throws? IllegalArgumentException (only [:x :y])))

(dotest
  (is= nil (third [       ]))
  (is= nil (third [1      ]))
  (is= nil (third [1 2    ]))
  (is= 3   (third [1 2 3  ]))
  (is= 3   (third [1 2 3 4])))

(dotest
  (is= 3        (t/validate pos? 3))
  (is= 3.14     (t/validate number? 3.14 ))
  (is= 3.14     (t/validate #(< 3 % 4) 3.14 ))
  (is= [0 1 2]  (t/validate vector? (vec (range 3))))
  (is= nil      (t/validate nil? (next [])))
  (is= [0 1 2]  (t/validate #(= 3 (count %)) [0 1 2]))
  (throws? Exception (t/validate number? "hello"))
  (throws? Exception (t/validate truthy? nil))
)

(dotest
  (testing "basic usage"
    (let [m1 {:a 1 :b 2 :c 3}
          m2 {:a 1 :b 2 :c [3 4]} ]
      (is= m1 (apply hash-map (keyvals m1)))
      (is= m2 (apply hash-map (keyvals m2)))
    )))
; AWTAWT TODO: add test.check

(dotest
  (is= 7 (safe-> 3 (* 2) (+ 1)))
  (let [mm  {:a {:b 2}}]
    (is= (safe-> mm :a)     {:b 2} )
    (is= (safe-> mm :a :b)      2)
    (throws? IllegalArgumentException   (safe-> mm :x))
    (throws? IllegalArgumentException   (safe-> mm :a :x))
    (throws? IllegalArgumentException   (safe-> mm :a :b :x))))

(dotest
  (is= 2 (it-> 1
           (inc it)
           (+ 3 it)
           (/ 10 it)))
  (let [mm  {:a {:b 2}}]
    (is= (it-> mm (:a it)          )  {:b 2} )
    (is= (it-> mm (it :a)  (:b it) )      2  )))

(dotest
  (testing "basic usage"
    (throws?    Exception                       (/ 1 0))
    (is= nil      (with-exception-default nil     (/ 1 0)))
    (is= :dummy   (with-exception-default :dummy  (/ 1 0)))
    (is= 123      (with-exception-default 0       (Long/parseLong "123")))
    (is= 0        (with-exception-default 0       (Long/parseLong "12xy3")))
    ))

(dotest
  (is (rel= 1 1 :digits 4 ))
  (is (rel= 1 1 :tol    0.01 ))

  (throws? IllegalArgumentException  (rel= 1 1 ))
  (throws? IllegalArgumentException  (rel= 1 1 4))
  (throws? IllegalArgumentException  (rel= 1 1 :xxdigits 4      ))
  (throws? IllegalArgumentException  (rel= 1 1 :digits   4.1    ))
  (throws? IllegalArgumentException  (rel= 1 1 :digits   0      ))
  (throws? IllegalArgumentException  (rel= 1 1 :digits  -4      ))

  (throws? IllegalArgumentException  (rel= 1 1 :tol    -0.01    ))
  (throws? IllegalArgumentException  (rel= 1 1 :tol     "xx"    ))
  (throws? IllegalArgumentException  (rel= 1 1 :xxtol   0.01    ))

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

(dotest
  (is (every? t/truthy? (forv [ul (range 0 4)] (vector? (t/range-vec ul)))))

  (is (every? t/truthy? (forv [ul (range 0 4)] (= (t/range-vec ul) (range ul)))))

  (is (every? t/truthy? (forv [lb (range 0 4)
                               ub (range lb 4) ]
                          (= (t/range-vec lb ub) (range lb ub)))))
)

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
    (is= [    0  2  4  6  8  10] (thru   0  10  2))
    (is= [    0 -2 -4 -6 -8 -10] (thru   0 -10 -2))
    (is= [       2  4  6  8  10] (thru   2  10  2))
    (is= [      -2 -4 -6 -8 -10] (thru  -2 -10 -2))
    (is= [ 2  0 -2 -4 -6 -8 -10] (thru   2 -10 -2))
    (is= [-2  0  2  4  6  8  10] (thru  -2  10  2))

    (is= [ 10  8  6  4  2  0   ] (thru  10   0 -2))
    (is= [-10 -8 -6 -4 -2  0   ] (thru -10   0  2))
    (is= [ 10  8  6  4  2      ] (thru  10   2 -2))
    (is= [-10 -8 -6 -4 -2      ] (thru -10  -2  2))
    (is= [ 10  8  6  4  2  0 -2] (thru  10  -2 -2))
    (is= [-10 -8 -6 -4 -2  0  2] (thru -10   2  2))

    (is= [    0  5  10] (thru   0  10  5))
    (is= [    0 -5 -10] (thru   0 -10 -5))
    (is= [       5  10] (thru   5  10  5))
    (is= [      -5 -10] (thru  -5 -10 -5))
    (is= [ 5  0 -5 -10] (thru   5 -10 -5))
    (is= [-5  0  5  10] (thru  -5  10  5))

    (is= [ 10  5  0   ] (thru  10   0 -5))
    (is= [-10 -5  0   ] (thru -10   0  5))
    (is= [ 10  5      ] (thru  10   5 -5))
    (is= [-10 -5      ] (thru -10  -5  5))
    (is= [ 10  5  0 -5] (thru  10  -5 -5))
    (is= [-10 -5  0  5] (thru -10   5  5)))
)

(dotest
  (is= [0 2 4 6 8]  (keep-if even? (range 10))
                    (drop-if odd?  (range 10)))
  (is= [1 3 5 7 9]  (keep-if odd?  (range 10))
                    (drop-if even? (range 10)))

  ; If we supply a 2-arg fn when filtering a sequence, we get an Exception
  (throws? clojure.lang.ArityException (keep-if (fn [arg1 arg2] :dummy) #{1 2 3} ))

  ; Verify throw if coll is not a sequential, map, or set.
  (throws? IllegalArgumentException (keep-if truthy? 2 ))
  (throws? IllegalArgumentException (keep-if truthy? :some-kw )))

(dotest
  (let [m1  {10  0,   20 0
             11  1,   21 1
             12  2,   22 2
             13  3,   23 3} ]
    (is= (keep-if   (fn [k v] (odd?  k))  m1)
         (drop-if   (fn [k v] (even? k))  m1)
          {11  1,   21 1
           13  3,   23 3} )
    (is= (keep-if   (fn [k v] (even? k))  m1)     (keep-if   (fn [k v] (even? v))  m1)
         (drop-if   (fn [k v] (odd?  k))  m1)     (drop-if   (fn [k v] (odd?  v))  m1)
          {10  0,   20 0
           12  2,   22 2} )
    (is=  (keep-if   (fn [k v] (< k 19))  m1)
          (drop-if   (fn [k v] (> k 19))  m1)
          {10  0
           11  1
           12  2
           13  3} )
    (is=  (keep-if   (fn [k v] (= 1 (int (/ k 10))))  m1)
          (drop-if   (fn [k v] (= 2 (int (/ k 10))))  m1)
          {10  0
           11  1
           12  2
           13  3} )
    (is=  (keep-if   (fn [k v] (= 2 (int (/ k 10))))  m1)
          (drop-if   (fn [k v] (= 1 (int (/ k 10))))  m1)
          {20  0
           21  1
           22  2
           23  3} )
    (is=  (keep-if   (fn [k v] (<= v 1   ))  m1)
          (drop-if   (fn [k v] (<=   2 v ))  m1)
            {10  0,   20 0
             11  1,   21 1 } )

    ; If we supply a 1-arg fn when filtering a map, we get an Exception
    (throws? clojure.lang.ArityException (keep-if (fn [arg] :dummy) {:a 1} ))
  ))

(dotest
  (let [s1  (into (sorted-set) (range 10)) ]
    (is= #{0 2 4 6 8}   (keep-if even? s1)
                        (drop-if odd?  s1))
    (is= #{1 3 5 7 9}   (keep-if odd?  s1)
                        (drop-if even? s1))

    ; If we supply a 2-arg fn when filtering a set, we get an Exception
    (throws? clojure.lang.ArityException (keep-if (fn [arg1 arg2] :dummy) #{1 2 3} ))))

#_(tst/defspec ^:slow t-keep-if-drop-if 999
  (prop/for-all [vv (gen/vector gen/int) ]
    (let [even-1      (keep-if   even?  vv)
          even-2      (drop-if   odd?   vv)
          even-filt   (filter    even?  vv)

          odd-1       (keep-if   odd?   vv)
          odd-2       (drop-if   even?  vv)
          odd-rem     (remove    even?  vv) ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

#_(tst/defspec ^:slow t-keep-if-drop-if-set 999
  (prop/for-all [ss (gen/set gen/int) ]
    (let [even-1      (keep-if   even?  ss)
          even-2      (drop-if   odd?   ss)
          even-filt   (into #{} (filter even? (seq ss)))

          odd-1       (keep-if   odd?   ss)
          odd-2       (drop-if   even?  ss)
          odd-rem     (into #{} (remove even? (seq ss))) ]
      (and  (= even-1 even-2 even-filt)
            (=  odd-1  odd-2  odd-rem)))))

#_(tst/defspec ^:slow t-keep-if-drop-if-map-key 99  ; seems to hang if (< 99 limit)
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

#_(tst/defspec ^:slow t-keep-if-drop-if-map-value 99  ; seems to hang if (< 99 limit)
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

(dotest
  (is (= "a" (strcat \a  )) (strcat [\a]  ))
  (is (= "a" (strcat "a" )) (strcat ["a"] ))
  (is (= "a" (strcat 97  )) (strcat [97]  ))

  (is (= "ab" (strcat \a   \b   )) (strcat [\a]  \b   ))
  (is (= "ab" (strcat \a  [\b]  )) (strcat [\a   \b]  ))
  (is (= "ab" (strcat "a"  "b"  )) (strcat ["a"] "b"  ))
  (is (= "ab" (strcat "a" ["b"] )) (strcat ["a"  "b"] ))
  (is (= "ab" (strcat 97   98   )) (strcat [97]  98   ))
  (is (= "ab" (strcat 97  [98]  )) (strcat [97   98]  ))
  (is (= "ab" (strcat ""  "ab"  )) (strcat ["" \a "b"]))

  (is (= "abcd" (strcat              97  98   "cd" )))
  (is (= "abcd" (strcat             [97  98]  "cd" )))
  (is (= "abcd" (strcat (byte-array [97  98]) "cd" )))

  (is (= (strcat "I " [ \h \a nil \v [\e \space nil (byte-array [97])
                        [ nil 32 "complicated" (Math/pow 2 5) '( "str" nil "ing") ]]] )
         "I have a complicated string" ))

  (let [chars-set   (into #{} (t/char-seq \a \z))
        str-val     (strcat chars-set) ]
    (is= 26 (count chars-set))
    (is= 26 (count str-val))
    (is= 26 (count (re-seq #"[a-z]" str-val)))
    (is= "abc" (str/join (t/char-seq \a \c)))
    ))

(dotest
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

(dotest
  (is (= [\a ]              (t/char-seq \a \a)))
  (is (= [\a \b]            (t/char-seq \a \b)))
  (is (= [\a \b \c]         (t/char-seq \a \c)))

  (is (= [\a ]              (t/char-seq 97 97)))
  (is (= [\a \b]            (t/char-seq 97 98)))
  (is (= [\a \b \c]         (t/char-seq 97 99)))

  (throws? Exception (t/char-seq 987654321 987654321))
  (throws? Exception (t/char-seq \c \a))
  (throws? Exception (t/char-seq 99 98))
  )

(dotest
  (is= [] (drop-at (range 1) 0))

  (is= [  1] (drop-at (range 2) 0))
  (is= [0  ] (drop-at (range 2) 1))

  (is= [  1 2] (drop-at (range 3) 0))
  (is= [0   2] (drop-at (range 3) 1))
  (is= [0 1  ] (drop-at (range 3) 2))

  (throws? IllegalArgumentException (drop-at []         0))
  (throws? IllegalArgumentException (drop-at (range 3) -1))
  (throws? IllegalArgumentException (drop-at (range 3)  3)))

(dotest
  (is= [9] (insert-at [] 0 9))

  (is= [9 0] (insert-at [0] 0 9))
  (is= [0 9] (insert-at [0] 1 9))

  (is= [9 0 1] (insert-at [0 1] 0 9))
  (is= [0 9 1] (insert-at [0 1] 1 9))
  (is= [0 1 9] (insert-at [0 1] 2 9))

  (throws? IllegalArgumentException (insert-at [] -1 9))
  (throws? IllegalArgumentException (insert-at []  1 9))

  (throws? IllegalArgumentException (insert-at [0] -1 9))
  (throws? IllegalArgumentException (insert-at [0]  2 9))

  (throws? IllegalArgumentException (insert-at [0 1] -1 9))
  (throws? IllegalArgumentException (insert-at [0 1]  3 9)))

(dotest
  (is= [9] (replace-at (range 1) 0 9))

  (is= [9 1] (replace-at (range 2) 0 9))
  (is= [0 9] (replace-at (range 2) 1 9))

  (is= [9 1 2] (replace-at (range 3) 0 9))
  (is= [0 9 2] (replace-at (range 3) 1 9))
  (is= [0 1 9] (replace-at (range 3) 2 9))

  (throws? IllegalArgumentException (replace-at []         0 9))
  (throws? IllegalArgumentException (replace-at (range 3) -1 9))
  (throws? IllegalArgumentException (replace-at (range 3)  3 9)))

(dotest
  (is= (mapv #(mod % 3) (thru -6 6)) [0 1 2 0 1 2 0 1 2 0 1 2 0])
  (is= (mapv #(idx [0 1 2] %) (thru -3 3)) [0 1 2 0 1 2 0]))

; #todo add different lengths a/b
; #todo add missing entries a/b
(dotest
  (is      (t/matches?  []    [] ))
  (is      (t/matches?  [1]   [1] ))
  (isnt    (t/matches?  [1]   [2] ))
  ;        (t/matches?  [1]   [1 2] )))  ***** error *****
  (is      (t/matches?  [_]   [1] ))
  (is      (t/matches?  [_]   [nil] ))
  (is      (t/matches?  [_]   [1] [2] [3]))
  (is      (t/matches?  [1 2] [1 2] ))
  (is      (t/matches?  [_ 2] [1 2] ))
  (is      (t/matches?  [1 _] [1 2] ))
  (is      (t/matches?  [1 _] [1 2] [1 3] [1 nil] ))
  (is      (t/matches?  [1 _ 3] [1 2 3] [1 nil 3] ))

  (is      (t/matches?  {:a 1} {:a 1} ))
  (isnt    (t/matches?  {:a 1} {:a 2} ))
  (isnt    (t/matches?  {:a 1} {:b 1} ))
  (is      (t/matches?  {:a _} {:a 1} {:a 2} {:a 3} ))
  ;        (t/matches?  { _ 1} {:a 1} )   ***** error *****

  (is      (t/matches?  {:a _ :b _       :c 3} 
                        {:a 1 :b [1 2 3] :c 3} ))
  (isnt    (t/matches?  {:a _ :b _       :c 4}
                        {:a 1 :b [1 2 3] :c 3} ))
  (isnt    (t/matches?  {:a _ :b _       :c 3}
                        {:a 1 :b [1 2 3] :c 4} ))
  (isnt    (t/matches?  {:a 9 :b _       :c 3}
                        {:a 1 :b [1 2 3] :c 3} ))

  (is      (t/matches?  {:a _ :b _       :c 3} 
                        {:a 1 :b [1 2 3] :c 3}
                        {:a 2 :b 99      :c 3}
                        {:a 3 :b nil     :c 3} ))
  (isnt    (t/matches?  {:a _ :b _       :c 3}
                        {:a 1 :b [1 2 3] :c 9}
                        {:a 2 :b 99      :c 3}
                        {:a 3 :b nil     :c 3} ))
  (isnt    (t/matches?  {:a _ :b _       :c 3}
                        {:a 1 :b [1 2 3] :c 3}
                        {:a 2 :b 99      :c 3}
                        {:a 3 :b nil     :c 9} ))
)

; #todo add different lengths a/b
; #todo add missing entries a/b
(dotest
  (testing "vectors"
    (is   (wild-match? [1]  [1] ))
    (is   (wild-match? [1]  [1] [1] ))
    (is   (wild-match? [:*] [1] [1] ))
    (is   (wild-match? [:*] [1] [9] ))

    (is   (wild-match? [1] [1] ))
    (is   (wild-match? [1] [1] [1] ))

    (isnt (wild-match? [1] [ ] ))
    (isnt (wild-match? [ ] [1] ))
    (isnt (wild-match? [1] [ ] [ ] ))
    (isnt (wild-match? [ ] [1] [ ] ))
    (isnt (wild-match? [ ] [ ] [1] ))
    (isnt (wild-match? [1] [1] [ ] ))
    (isnt (wild-match? [1] [ ] [1] ))

    (is   (wild-match? [1 2  3]
                       [1 2  3] ))
    (is   (wild-match? [1 :* 3]
                       [1 2  3] ))
    (is   (wild-match? [1 :* 3]
                       [1 2  3]
                       [1 9  3] ))
    (isnt (wild-match? [1 2  3]
                       [1 2  9] ))
    (isnt (wild-match? [1 2   ]
                       [1 2  9] ))
    (isnt (wild-match? [1 2  3]
                       [1 2   ] ))

    (is   (wild-match? [1  [2 3]]
                       [1  [2 3]] ))
    (is   (wild-match? [:* [2 3]]
                       [1  [2 3]] ))
    (is   (wild-match? [:* [2 3]]
                       [1  [2 3]]
                       [9  [2 3]] ))
    (is   (wild-match? [1  [2 :*]]
                       [1  [2 33]]
                       [1  [2 99]] ))
    (is   (wild-match? [1  :*]
                       [1   2]
                       [1  [2 3]] ))
    (isnt (wild-match? [1  [2 3]]
                       [1  [2 9]] ))
  )
  (testing "maps"
    (is (wild-match? {:a 1 } {:a 1} ))
    (is (wild-match? {:a :*} {:a 1} ))
    (is (wild-match? {:a :*} {:a 1 } {:a 1 } ))
    (is (wild-match? {:a :*} {:a 1 } {:a 9 } ))
    (is (wild-match? {:a :*} {:a :*} {:a 9 } ))
    (is (wild-match? {:a :*} {:a :*} {:a :*} ))

    (isnt (wild-match? {:a 1 } {:a 9} ))
    (isnt (wild-match? {:a 1 } {:a 1 :b 2} ))
    (isnt (wild-match? {:a :*} {:b 1} ))
    (isnt (wild-match? {:a :*} {:a 1} {:b 1} ))
    (isnt (wild-match? {:a :*} {:a 1 :b 2} ))

    (let [vv {:a 1  :b {:c 3}}
          tt {:a 1  :b {:c 3}}
          w2 {:a :* :b {:c 3}}
          w5 {:a 1  :b {:c :*}}
          zz {:a 2  :b {:c 3}}
    ]
      (is   (wild-match? tt vv))
      (is   (wild-match? w2 vv))
      (is   (wild-match? w5 vv))
      (isnt (wild-match? zz vv)))
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
      (isnt (wild-match? zz vv)))
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
      (isnt (wild-match? z1 vv))
      (isnt (wild-match? z2 vv)))
  )
  (testing "sets"
    (is   (wild-match? #{1} #{1} ))
    (isnt (wild-match? #{1} #{9} ))
    (isnt (wild-match? #{1} #{:a :b} ))
    (is   (wild-match? #{1  #{:a :b}}
                       #{1  #{:a :b} }))
    (isnt (wild-match? #{1  #{:a :c}}
                       #{1  #{:a :x} }))
  ))

(defrecord SampleRec [a b])
(dotest
  (let [sr1 (->SampleRec 1 2)]
    (is (map? sr1))
    (is (val= sr1 {:a 1 :b 2}))
    (is (val= 1 1))
    (is (val= "abc" "abc"))
    (is (val= [1 2 3] [1 2 3]))
    (is (val= #{1 2 sr1} #{1 2 {:a 1 :b 2}}))
    (is (val= [1 2 3 #{1 2 sr1}] [1 2 3 #{1 2 {:a 1 :b 2}}])) ) )

(dotest
  (isnt (wild-match? #{1 2} #{1 2 3 4}))
  (isnt (wild-match-ctx? {} #{1 2} #{1 2 3 4}))
  (is (wild-match-ctx? {:subset-ok true} #{1 2} #{1 2 3 4}))

  (isnt (wild-match? {:a 1} {:a 1 :b 2}))
  (isnt (wild-match-ctx? {} {:a 1} {:a 1 :b 2}))
  (is (wild-match-ctx? {:submap-ok true} {:a 1} {:a 1 :b 2}))

  (isnt (wild-match? '(1 2) '(1 2 3 4)))
  (isnt (wild-match-ctx? {} '(1 2) '(1 2 3 4)))
  (is (wild-match-ctx? {:subvec-ok true} '(1 2) '(1 2 3 4)))

  (isnt (wild-match? [1 2] [1 2 3 4]))
  (isnt (wild-match-ctx? {} [1 2] [1 2 3 4]))
  (is (wild-match-ctx? {:subvec-ok true} [1 2] [1 2 3 4]))

  (isnt (wild-submatch? #{1 :*}    #{1 2 3 4}))
  (is (wild-submatch?   #{1 2}     #{1 2 3 4}))
  (is (wild-submatch?    {:a :*}    {:a 1 :b 2}))
  (is (wild-submatch?   '(1 :* 3)  '(1 2 3 4)))
  (is (wild-submatch?    [1 :* 3]   [1 2 3 4]))

  (is (submatch? #{1 2} #{1 2 3 4}))
  (is (submatch? {:a 1} {:a 1 :b 2}))
  (is (submatch? '(1 2) '(1 2 3 4)))
  (is (submatch? [1 2 3] [1 2 3 4]))
  (isnt (submatch? [1 :* 3] [1 2 3 4]))
  (isnt (submatch? {:a :*} {:a 1 :b 2}))
  (isnt (submatch? #{1 :*} #{1 2 3 4}))

  (let [sample-rec (->SampleRec 1 2)]
    (isnt= sample-rec {:a 1 :b 2})
    (is (wild-submatch? sample-rec {:a 1 :b 2}))
    (is (wild-submatch? {:a 1 :b 2} sample-rec))
    (is (submatch? sample-rec {:a 1 :b 2}))
    (is (submatch? {:a 1 :b 2} sample-rec))))

(dotest
  (is (i/set-match? #{1 2 3} #{1 2 3}))
  (is (i/set-match? #{:* 2 3} #{1 2 3}))
  (is (i/set-match? #{1 :* 3} #{1 2 3}))
  (is (i/set-match? #{1 2 :*} #{1 2 3}))

  (is (i/set-match? #{1 2 3 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{:* 2 3 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 :* 3 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 2 :* 4 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 2 3 :* 5} #{1 2 3 4 5}))
  (is (i/set-match? #{1 2 3 4 :*} #{1 2 3 4 5}))

  (is   (wild-item? :*))
  (isnt (wild-item? :a))

  (is   (wild-item? :*))
  (isnt (wild-item? :a))
  (isnt (wild-item? 5))
  (isnt (wild-item? "hello"))

  (is   (wild-item? [:* 2 3]))
  (is   (wild-item? [1 [:* 3]]))
  (is   (wild-item? [1 [2 [:*]]]))
  (isnt (wild-item? [1 2 3]))
  (isnt (wild-item? [1 [2 3]]))
  (isnt (wild-item? [1 [2 [3]]]))

  (is   (wild-item? #{:* 2 3}))
  (is   (wild-item? #{1 #{:* 3}}))
  (is   (wild-item? #{1 #{2 #{:*}}}))
  (isnt (wild-item? #{1 2 3}))
  (isnt (wild-item? #{1 #{2 3}}))
  (isnt (wild-item? #{1 #{2 #{3}}}))

  (is   (wild-item? {:* 1 :b 2 :c 3}))
  (is   (wild-item? {:a {:* 2 :c 3}}))
  (is   (wild-item? {:a {:b {:* 3}}}))
  (is   (wild-item? {:a :* :b 2 :c 3}))
  (is   (wild-item? {:a {:b :* :c 3}}))
  (is   (wild-item? {:a {:b {:c :*}}}))
  (isnt (wild-item? {:a 1 :b 2 :c 3}))
  (isnt (wild-item? {:a {:b 2 :c 3}}))
  (isnt (wild-item? {:a {:b {:c 3}}}))

  (is (i/set-match? #{#{1 2 3} #{4 5 :*}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 3} #{4 :* 6}} #{#{1 2 3} #{4 5  6}}))
  (is (i/set-match? #{#{1 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{:* 2 3} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 :* 3} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 :*} #{4 5 6}} #{#{1 2 3} #{4 5 6}}))

  (is (i/set-match? #{#{1 :* 3} #{4 5 :*}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 :*} #{4 :* 6}} #{#{1 2 3} #{4 5  6}}))
  (is (i/set-match? #{#{:* 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{:* 2 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 :* 3} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
  (is (i/set-match? #{#{1 2 :*} #{:* 5 6}} #{#{1 2 3} #{4 5 6}}))
)

(dotest
  (is= (range 10)   ; vector/list
    (unnest  0 1 2 3 4 5 6 7 8 9 )
    (unnest  0 1 2 [3 [4] 5 6] 7 [8 9])
    (unnest [0 1 2 3 4 5 6 7 8 9])
    (unnest [0 [1 2 3 4 5 6 7 8 9]])
    (unnest [0 [1 [2 3 4 5 6 7 8 9]]])
    (unnest [0 [1 [2 [3 4 5 6 7 8 9]]]])
    (unnest [0 [1 [2 [3 [4 5 6 7 8 9]]]]])
    (unnest [0 [1 [2 [3 [4 [5 6 7 8 9]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 7 8 9]]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 [7 8 9]]]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 [7 [8 9]]]]]]]]])
    (unnest [0 [1 [2 [3 [4 [5 [6 [7 [8 [9]]]]]]]]]])
    (unnest [[[[[[[[[[0] 1] 2] 3] 4] 5] 6] 7] 8] 9])
    (unnest [0 1 [2 [3 [4] 5] 6] 7 8 9]))

  (is= (set [:a :1 :b 2 :c 3]) ; map
    (set (unnest [:a :1 {:b 2 :c 3}]))
    (set (unnest [:a :1 {[:b] 2 #{3} :c}]))
    (set (unnest [:a :1 {:b 2 :c 3}]))
    (set (unnest [:a :1 {:b {:c [2 3]}}])))
  (is= #{ 1 2 3 4 5 6 } (set (unnest {1 {2 {3 {4 {5 6}}}}})))

  (is= (set (range 10)) ; set
    (set (unnest #{0 1 2 3 4 5 6 7 8 9}))
    (set (unnest #{0 1 #{2 3 4 5 6 7 8} 9}))
    (set (unnest #{0 1 #{2 3 #{4 5 6} 7 8} 9})) )
  (is= #{ 1 2 3 4 5 6 } (set (unnest #{1 #{2 #{3 #{4 #{5 #{6}}}}}}))))

(dotest
  (is   (starts-with? (range 0 3) (range 0 0)))

  (is   (starts-with? (range 0 3) (range 0 1)))
  (is   (starts-with? (range 0 3) (range 0 2)))
  (is   (starts-with? (range 0 3) (range 0 3)))

  (isnt (starts-with? (range 0 3) (range 1 2)))
  (isnt (starts-with? (range 0 3) (range 1 3)))

  (isnt (starts-with? (range 0 3) (range 2 3)))

  (isnt (starts-with? (range 1 3) (range 0 1)))
  (isnt (starts-with? (range 1 3) (range 0 2)))
  (isnt (starts-with? (range 1 3) (range 0 3)))

  (is   (starts-with? (range 1 3) (range 1 2)))
  (is   (starts-with? (range 1 3) (range 1 3)))

  (isnt (starts-with? (range 1 3) (range 2 3)))

  (isnt (starts-with? (range 2 3) (range 0 1)))
  (isnt (starts-with? (range 2 3) (range 0 2)))
  (isnt (starts-with? (range 2 3) (range 0 3)))

  (isnt (starts-with? (range 2 3) (range 1 2)))
  (isnt (starts-with? (range 2 3) (range 1 3)))

  (is   (starts-with? (range 2 3) (range 2 3)))

  (isnt (starts-with? (range 3 3) (range 0 1)))
  (isnt (starts-with? (range 3 3) (range 0 2)))
  (isnt (starts-with? (range 3 3) (range 0 3)))
  )


(dotest
  ; (println "t-throws enter")
  ; (newline) (spyx (throws?-impl '(/ 1 2)))
  ; (newline) (spyx (throws?-impl 'Exception '(/ 1 2)))
  ; (newline) (spyx (throws?-impl 'ArithmeticException '(/ 1 2)))

  (throws? (/ 1 0))
  (throws? Exception (/ 1 0))             ; catches specified Throwable (or subclass)
  (throws? ArithmeticException (/ 1 0))   ; catches specified Throwable (or subclass)

  ; (println "t-throws exit")
)

(dotest
  (testing "fibo stuff"
    (is= (take  0 (fibonacci-seq))  [] )
    (is= (take  5 (fibonacci-seq))  [0 1 1 2 3] )
    (is= (take 10 (fibonacci-seq))  [0 1 1 2 3 5 8 13 21 34] )

    (is= (fibo-thru  0) [0] )
    (is= (fibo-thru  1) [0 1 1] )
    (is= (fibo-thru  2) [0 1 1 2] )
    (is= (fibo-thru  3) [0 1 1 2 3] )
    (is= (fibo-thru  4) [0 1 1 2 3] )
    (is= (fibo-thru  5) [0 1 1 2 3 5] )
    (is= (fibo-thru  6) [0 1 1 2 3 5] )
    (is= (fibo-thru  7) [0 1 1 2 3 5] )
    (is= (fibo-thru  8) [0 1 1 2 3 5 8] )
    (is= (fibo-thru 34) [0 1 1 2 3 5 8 13 21 34] )

    (is=  0 (fibo-nth 0))
    (is=  1 (fibo-nth 1))
    (is=  1 (fibo-nth 2))
    (is=  2 (fibo-nth 3))
    (is=  3 (fibo-nth 4))
    (is=  5 (fibo-nth 5))
    (is=  8 (fibo-nth 6))
    (is= 13 (fibo-nth 7))
    (is= 21 (fibo-nth 8))
    (is= 34 (fibo-nth 9))
    (is (< (Math/pow 2 62) (fibo-nth 91) (Math/pow 2 63)))
  )

  (testing "lazy-cons"
    (let [lazy-next-int (fn lazy-next-int [n]
                          (t/lazy-cons n (lazy-next-int (inc n))))
          all-ints      (lazy-next-int 0)
          ]
      (is= (take 0 all-ints) [])
      (is= (take 1 all-ints) [0] )
      (is= (take 5 all-ints) [0 1 2 3 4] ))

    (let [lazy-range (fn lazy-range
                       [limit]
                       (let [lazy-range-step (fn lazy-range-step [curr limit]
                                               ; (spyx [curr limit]) (flush)
                                               (when (< curr limit)
                                                 (t/lazy-cons curr (lazy-range-step (inc curr) limit))))]
                         (lazy-range-step 0 limit))) ]
      (is= (lazy-range 0) nil)
      (is= (lazy-range 1) [0])
      (is= (lazy-range 5) [0 1 2 3 4]))

    (let [lazy-countdown
          (fn lazy-countdown [n]
            (when (<= 0 n)
              (lazy-cons n (lazy-countdown (dec n))))) ]
      (is= (lazy-countdown  5) [5 4 3 2 1 0] )
      (is= (lazy-countdown  1) [1 0] )
      (is= (lazy-countdown  0) [0] )
      (is= (lazy-countdown -1) nil )))

;-----------------------------------------------------------------------------
; defgen/yield tests

  (let [empty-gen-fn (fn [] (lazy-gen)) ]
    (is (nil? (empty-gen-fn))))

  (let [range-gen (fn [limit] ; "A generator 'range' function."
                    (lazy-gen
                      (loop [cnt 0]
                        (when (< cnt limit)
                          (assert (= cnt (yield cnt)))
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

  (let [
        concat-gen        (fn [& collections]
                            (lazy-gen
                              (doseq [curr-coll collections]
                                (doseq [item curr-coll]
                                  (yield item)))))
        concat-gen-mirror (fn [& collections]
                            (lazy-gen
                              (doseq [curr-coll collections]
                                (doseq [item curr-coll]
                                  (let [items [item (- item)]]
                                    (assert (= items (yield-all items))))))))
        c1              [1 2 3]
        c2              [4 5 6]
        c3              [7 8 9]
  ]
      (is= [1 2 3 4 5 6 7 8 9] (concat-gen c1 c2 c3))
      (is= [1 -1  2 -2  3 -3  4 -4  5 -5  6 -6  7 -7  8 -8  9 -9]  (concat-gen-mirror c1 c2 c3)))

  (let [sq-yield   (fn [xs]
                     (lazy-gen
                       (doseq [x xs]
                         (yield (* x x)))))

        sq-recur   (fn [xs]
                     (loop [cum-result []
                            xs         xs]
                       (if (empty? xs)
                         cum-result
                         (let [x (first xs)]
                           (recur (conj cum-result (* x x))
                             (rest xs))))))

        sq-lazyseq (fn lazy-squares [xs]
                     (when (not-empty? xs)
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
      res-map
    )
  )

  (let [heads-pairs (fn [flips]
                      (reduce +
                        (let [flips (vec flips)]
                          (lazy-gen
                            (doseq [i (range (dec (count flips)))]
                              (when (= :h (flips i) (flips (inc i)))
                                (yield 1))))))) ]
    (is= 3 (heads-pairs [:h :t :h :h :h :t :h :h])))

  ; from S. Sierra blogpost: https://stuartsierra.com/2015/04/26/clojure-donts-concat
  (let [next-results   (fn [n] (thru 1 n)) ; (thru 1 3) => [1 2 3]
        build-1        (fn [n]
                         (lazy-gen
                           (loop [counter 1]
                             (when (<= counter n)
                               (doseq [item (next-results counter)] ; #todo -> yield-all
                                 (yield item))
                               (recur (inc counter))))))
        build-2        (fn [n]
                         (lazy-gen
                           (doseq [counter (thru n)]
                             (when (<= counter n)
                               (doseq [item (next-results counter)] ; #todo -> yield-all
                                 (yield item))))))
        build-3        (fn [n]
                         (lazy-gen
                           (doseq [counter (thru n)]
                             (t/yield-all (next-results counter)))))
        build-result-1 (build-1 3) ; => (1 1 2 1 2 3)
        build-result-2 (build-2 3)
        build-result-3 (build-3 3) ]
    (is= [1 1 2 1 2 3] build-result-1 build-result-2 build-result-3 ))

  (let [N                 99
        cat-glue-fn      (fn [coll] (lazy-gen
                                      (yield-all (glue coll [1 2 3]))))
        iter-glue        (iterate cat-glue-fn [1 2 3]) ; #todo some sort of bug here!
        iter-glue-result (nth iter-glue N) ; #todo hangs w. 2 'yield-all' if (50 < x < 60)

        cat-flat-fn      (fn [coll] (lazy-gen
                                      (yield-all (flat-vec [coll [1 [2 [3]]]]))))
        iter-flat        (iterate cat-flat-fn [1 2 3]) ; #todo some sort of bug here!
        iter-flat-result (nth iter-flat N) ; #todo hangs w. 2 'yield-all' if (50 < x < 60)
        ]
    (when false
      (spyx (count iter-glue-result))
      (spyx (count iter-flat-result)))
      ; for N = 1299
      ; (count iter-glue-result) => 3900 "Elapsed time: 2453.917953 msecs"
      ; (count iter-flat-result) => 3900 "Elapsed time: 2970.726412 msecs"
    (is= iter-glue-result iter-flat-result))

; Bare yield won't compile => java.lang.RuntimeException: Unable to resolve symbol: lazy-gen-output-buffer
  ; (yield 99)

  ; (lazy-seq nil) => ()
  ; (lazy-cons 3 (lazy-seq nil)) => (3)
  ; (lazy-cons 2 (lazy-cons 3 (lazy-seq nil))) => (2 3)
  ; (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 (lazy-seq nil)))) => (1 2 3)
  ;
  ; (range-gen 5) => (0 1 2 3 4)
  ; (range-gen 10) => (0 1 2 3 4 5 6 7 8 9)
  ; (concat-gen [1 2 3] [4 5 6] [7 8 9]) => (1 2 3 4 5 6 7 8 9)
  ; (empty-gen-fn) => nil

  (is= [1 2 3 4 5] (flat-vec [[[1] 2] [3 [4 [5]]]]))

  (testing "split-match"
    (is= [ [] [\a   \b   \c   \d   \e   \f]      ] (split-match "abcdef" "a"   ))
    (is= [ [] [\a   \b   \c   \d   \e   \f]      ] (split-match "abcdef" "ab"  ))
    (is= [    [\a] [\b   \c   \d   \e   \f]      ] (split-match "abcdef" "bc"  ))
    (is= [    [\a   \b] [\c   \d   \e   \f]      ] (split-match "abcdef" "cde" ))
    (is= [    [\a   \b   \c] [\d   \e   \f]      ] (split-match "abcdef" "de"  ))
    (is= [    [\a   \b   \c   \d] [\e   \f]      ] (split-match "abcdef" "ef"  ))
    (is= [    [\a   \b   \c   \d   \e] [\f]      ] (split-match "abcdef" "f"   ))
    (is= [    [\a   \b   \c   \d   \e   \f]  []  ] (split-match "abcdef" "fg"  ))
    (is= [    [\a   \b   \c   \d   \e   \f]  []  ] (split-match "abcdef" "gh"  ))

    (is= [    [0   1   2   3   4   5]  []  ]       (split-match (range 6) [-1]    ))
    (is= [ [] [0   1   2   3   4   5]      ]       (split-match (range 6) [0]     ))
    (is= [ [] [0   1   2   3   4   5]      ]       (split-match (range 6) [0 1]   ))
    (is= [    [0   1] [2   3   4   5]      ]       (split-match (range 6) [2 3]   ))
    (is= [    [0   1   2] [3   4   5]      ]       (split-match (range 6) [3 4 5] ))
    (is= [    [0   1   2   3] [4   5]      ]       (split-match (range 6) [4 5]   ))
    (is= [    [0   1   2   3   4] [5]      ]       (split-match (range 6) [5]     ))
    (is= [    [0   1   2   3   4   5]  []  ]       (split-match (range 6) [5 6]   ))
    (is= [    [0   1   2   3   4   5]  []  ]       (split-match (range 6) [6 7]   )))

  (testing "index-using"
    (is= nil (t/index-using #(= [666]       %) (range 5)))
    (is= 0   (t/index-using #(= [0 1 2 3 4] %) (range 5)))
    (is= 1   (t/index-using #(= [  1 2 3 4] %) (range 5)))
    (is= 2   (t/index-using #(= [    2 3 4] %) (range 5)))
    (is= 3   (t/index-using #(= [      3 4] %) (range 5)))
    (is= 4   (t/index-using #(= [        4] %) (range 5)))
    (is= nil (t/index-using #(= [         ] %) (range 5))))

  (testing "split-using"
    (is= [ [] [0   1   2   3   4]    ] (split-using #(= 0 (first %)) (range 5)))
    (is= [    [0] [1   2   3   4]    ] (split-using #(= 1 (first %)) (range 5)))
    (is= [    [0   1] [2   3   4]    ] (split-using #(= 2 (first %)) (range 5)))
    (is= [    [0   1   2] [3   4]    ] (split-using #(= 3 (first %)) (range 5)))
    (is= [    [0   1   2   3] [4]    ] (split-using #(= 4 (first %)) (range 5)))
    (is= [    [0   1   2   3   4] [] ] (split-using #(= 5 (first %)) (range 5)))
    (is= [    [0   1   2   3   4] [] ] (split-using #(= 9 (first %)) (range 5)))

    (is= [[\a \b \c] [\d \e \f]] (split-using #(starts-with? % "de") "abcdef")))

  (testing "partition-using"
    (let [start-segment? (fn [vals] (zero? (rem (first vals) 3))) ]
      (is= (partition-using start-segment? [1 2 3 6 7 8])
        [[1 2] [3] [6 7 8]])
      (is= (partition-using start-segment? [3 6 7 9])
        [[3] [6 7] [9]])
      (is= (partition-using start-segment? [1 2 3 6 7 8 9 12 13 15 16 17 18 18 18 3 4 5])
        [[1 2] [3] [6 7 8] [9] [12 13] [15 16 17] [18] [18] [18] [3 4 5]]))
    (throws? (partition-using even? 5))))


(dotest
  (let [some-fn (fn []
                  (let [a 1
                        b 2
                        c 3
                        d 4
                        e 5]
                    (vals->map a b c d e)))

       the-map (some-fn) ]
    (is= the-map {:a 1 :b 2 :c 3 :d 4 :e 5})

    (let [{:keys [a b c d e]} the-map]
      (is= [a b c d e] [1 2 3 4 5]))

    (with-map-vals the-map [a b c d e]
      (is= [a b c d e] [1 2 3 4 5])
      (is= 15 (+ a b c d e)))

    (throws?
      (with-map-vals the-map [x y z]
        (println "shouldn't ever get here")))))
(dotest
  (is   (macro? 'and))
  (is   (macro? '->))
  (isnt (macro? '+))
  (isnt (macro? 'if)))

(dotest
  (is= 'abc (kw->sym :abc))
  (is= "abc" (kw->str :abc))
  (is= 'abc (str->sym "abc"))
  (is= :abc (str->kw "abc"))
  (is= :abc (sym->kw 'abc))
  (is= "abc" (sym->str 'abc)))



; #todo move to tst.tupelo.core.deprecated
;---------------------------------------------------------------------------------------------------
; Deprecated functions

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(dotest
  ; ^{:deprecated "1.9.0-alpha5" }
  (t/when-not-clojure-1-9-plus
    (is   (t/seqable?   "abc"))
    (is   (t/seqable?   {1 2 3 4} ))
    (is   (t/seqable?  #{1 2 3} ))
    (is   (t/seqable?  '(1 2 3) ))
    (is   (t/seqable?   [1 2 3] ))
    (is   (t/seqable?   (byte-array [1 2] )))
    (isnt (t/seqable?  1 ))
    (isnt (t/seqable? \a ))))

(dotest
  ; ^:deprecated ^:no-doc
  (let [s1    "  hello there
                 again
                 and again!   "
        r1     ["hello there"
                "again"
                "and again!"]
  ]
    (is= r1 (map str/trim (t/str->lines s1)))
    (is= r1 (map str/trim (str/split-lines s1)))))

