;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tst.cooljure.core
  (:require [clojure.string     :as str]
            [cooljure.misc      :as misc] )
  (:use cooljure.core 
        clojure.test ))

(deftest truthy-falsey-tst
  (let [data [true :a 'my-symbol 1 "hello" \x false nil] ]
    (testing "basic usage"
      (let [truthies    (filter boolean data)       ; coerce to primitive type
            falsies     (filter not     data) ]     ; unnatural syntax
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
                  (= falsies  [false nil] ) )))
      (let [truthies    (filter truthy? data)
            falsies     (filter falsey? data) ]
        (is (and  (= truthies [true :a 'my-symbol 1 "hello" \x] )
                  (= falsies  [false nil] ) ))))

    (testing "improved usage"
      (let [count-if (comp count filter) ]
        (let [num-true    (count-if boolean data)   ; awkward phrasing
              num-false   (count-if not     data) ] ; doesn't feel natural
          (is (and  (= 6 num-true) 
                    (= 2 num-false) )))
        (let [num-true    (count-if truthy? data)   ; matches intent much better
              num-false   (count-if falsey? data) ]
          (is (and  (= 6 num-true)
                    (= 2 num-false) )))))
  ))

(deftest any-tst
  (testing "basic usage"
    (is (= true   (any? odd? [1 2 3] ) ))
    (is (= false  (any? odd? [2 4 6] ) ))
    (is (= false  (any? odd? []      ) )) ))

(deftest not-empty-tst
  (testing "basic usage"
    (is (every?     not-empty? ["1" [1] '(1) {:1 1} #{1}    ] ))
    (is (not-any?   not-empty? [""  []  '()  {}     #{}  nil] ))

    (is (= (map not-empty? ["1" [1] '(1) {:1 1} #{1} ] )
           [true true true true true]  ))
    (is (= (map not-empty? ["" [] '() {} #{} nil] )
           [false false false false false false ] ))))

(deftest conjv-tst
  (testing "basic usage"
    (is (= [  2  ]  (conjv  []  2   )))
    (is (= [  2  ]  (conjv '()  2   )))
    (is (= [  2 3]  (conjv  []  2  3)))
    (is (= [  2 3]  (conjv '()  2  3)))

    (is (= [1 2 3]  (conjv  [1] 2  3)))
    (is (= [1 2 3]  (conjv '(1) 2  3)))
    (is (= [1 2 3]  (conjv  [1  2] 3)))
    (is (= [1 2 3]  (conjv '(1  2) 3)))

    (is (= [1 2 3 4]  (conjv  [1  2] 3 4)))
    (is (= [1 2 3 4]  (conjv '(1  2) 3 4)))
    (is (= [1 2 3 4]  (conjv  [1] 2  3 4)))
    (is (= [1 2 3 4]  (conjv '(1) 2  3 4))) )

  (testing "vector elements"
    (is (=    [[1 2] [3 4]  [5 6] ]
      (conjv '([1 2] [3 4]) [5 6] ) )))

  (testing "lazy seqs/apply"
    (is (= [0 1 2 3 4 5] (conjv (range 4) 4 5)))
    (is (= [0 1 2 3 4 5] (apply conjv [0] (range 1 6)))) ))

(deftest strcat-tst
  (is (= "a" (strcat \a  )))    (is (= "a" (strcat [\a]  )))
  (is (= "a" (strcat "a" )))    (is (= "a" (strcat ["a"] )))
  (is (= "a" (strcat 97  )))    (is (= "a" (strcat [97]  )))

  (is (= "ab" (strcat \a   \b   ))) (is (= "ab" (strcat [\a]  \b   )))
  (is (= "ab" (strcat \a  [\b]  ))) (is (= "ab" (strcat [\a   \b]  )))
  (is (= "ab" (strcat "a"  "b"  ))) (is (= "ab" (strcat ["a"] "b"  )))
  (is (= "ab" (strcat "a" ["b"] ))) (is (= "ab" (strcat ["a"  "b"] )))
  (is (= "ab" (strcat 97   98   ))) (is (= "ab" (strcat [97]  98   )))
  (is (= "ab" (strcat 97  [98]  ))) (is (= "ab" (strcat [97   98]  )))

  (is (= "abcd" (strcat              97  98   "cd" )))
  (is (= "abcd" (strcat             [97  98]  "cd" )))
  (is (= "abcd" (strcat (byte-array [97  98]) "cd" )))

  (let [chars-set   (into #{} (misc/char-seq \a \z)) 
        str-val     (strcat chars-set) ]
    (is (= 26 (count chars-set)))
    (is (= 26 (count str-val)))
    (is (= 26 (count (re-seq #"[a-z]" str-val))))))

(deftest seqable-tst
  (is (seqable? "abc"))
  (is (seqable?  {1 2 3 4}))
  (is (seqable? #{1 2 3}))
  (is (seqable? '(1 2 3)))
  (is (seqable?  [1 2 3]))
  (is (seqable? (byte-array [1 2])))

  (is (not (seqable?  1 )))
  (is (not (seqable? \a ))))

(deftest keyvals-t
  (testing "basic usage"
    (let [m1 {:a 1 :b 2 :c 3} 
          m2 {:a 1 :b 2 :c [3 4]} ]
      (is (= m1 (apply hash-map (keyvals m1))))
      (is (= m2 (apply hash-map (keyvals m2)))) 
    )))
; AWTAWT TODO: add test.check

(deftest with-exception-default-t
  (testing "basic usage"
    (is (thrown?    Exception                       (/ 1 0)))
    (is (= nil      (with-exception-default nil     (/ 1 0))))
    (is (= :dummy   (with-exception-default :dummy  (/ 1 0))))
    (is (= 123      (with-exception-default 0       (Long/parseLong "123"))))
    (is (= 0        (with-exception-default 0       (Long/parseLong "12xy3"))))
    ))

(deftest forv-t
  (is (= (forv [x (range 23)] (* x x))
         (for  [x (range 23)] (* x x))))
  (is (= (forv [x (range 5)  y (range 2 9)] (str x y))
         (for  [x (range 5)  y (range 2 9)] (str x y)))))

(deftest spy-t
  (testing "basic usage"
    (let [side-effect-cum-sum (atom 0)  ; side-effect running total

          ; Returns the sum of its arguments AND keep a running total.
          side-effect-add!  (fn [ & args ]
                              (let [result (apply + args) ]
                                (swap! side-effect-cum-sum + result)
                                result))
    ]
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

(deftest spyxx-t
  (let [val1  {:a 1 :b 2}
        val2  (+ 2 3) ]
    (is (= "val1 => clojure.lang.PersistentArrayMap->{:a 1, :b 2}"
        (str/trim (with-out-str (spyxx val1 )))  ))

    (is (= "val2 => java.lang.Long->5"
        (str/trim (with-out-str (spyxx val2 ))) ))
  ))

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

  (is      (rel= 0 0 :digits 3 ))

  (is      (rel= 1 1.001 :digits 3 ))
  (is (not (rel= 1 1.001 :digits 4 )))
  (is      (rel= 123450000 123456789 :digits 4 ))
  (is (not (rel= 123450000 123456789 :digits 6 )))

  (is      (rel= 1 1.001 :tol 0.01 ))
  (is (not (rel= 1 1.001 :tol 0.0001 )))
)

(comment    ; example usage w/o -> macro
  (def cust->zip
    "A map from (int) customer-id to (string-5) zipcode, like { 96307657 \"54665\", ...}"
    (spy-last "#00 map:"
      (into (sorted-map) 
        (spy-last "#01 for:"
          (for [cust-zip-map cust-zips]
            (spy-last "#02 vec:" 
              [ (:customer-id  cust-zip-map)
                (:zipcode      cust-zip-map)  ] ))))))
)

