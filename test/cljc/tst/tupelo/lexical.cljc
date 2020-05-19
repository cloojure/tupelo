;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.lexical
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.data.avl :as avl]
    [tupelo.lexical :as lex]
    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]]
    [tupelo.core.impl :as impl])
)

#?(:cljs (enable-console-print!))

(defrecord ^:no-doc DummyEid [raw])
(defrecord ^:no-doc DummyLeaf [raw])
(def eid-0 (->DummyEid 0)) ; "Eid" sorts before "Leaf"
(def eid-1 (->DummyEid 1))
(def leaf-0 (->DummyLeaf 0))
(def leaf-1 (->DummyLeaf 1))

(dotest
  (is= "Type/Clojure-IPersistentMap" (lex/comparison-class {:a 1}))
  (is= "Type/Clojure-Sequential" (lex/comparison-class (first {:a 1}))) ; a MapEntry
  (is= "Type/Clojure-Sequential" (lex/comparison-class [1 2 3]))
  (is= "Type/Clojure-Sequential" (lex/comparison-class (list 1 2 3)))
  (is= "Type/Clojure-IPersistentSet" (lex/comparison-class #{1 3 2}))
  (is= "Type/Clojure-Keyword" (lex/comparison-class :hello))
  (is= "Type/Clojure-Symbol" (lex/comparison-class (symbol "hello")))
  (is= "Type/Clojure-Number" (lex/comparison-class 3))
  (is= "Type/Clojure-Number" (lex/comparison-class 3.14159))
  (is= "Type/Clojure-Character" (lex/comparison-class \X))
  (is= "Type/Clojure-String" (lex/comparison-class "hello"))
  (is= "Type/Clojure-Boolean" (lex/comparison-class true))

  (is= "tst.tupelo.lexical/DummyEid" (impl/type-name-str eid-0))
  (is= "tst.tupelo.lexical/DummyLeaf" (impl/type-name-str leaf-0))
  (is= "tst.tupelo.lexical/DummyEid" (lex/comparison-class eid-0))
  (is= "tst.tupelo.lexical/DummyLeaf" (lex/comparison-class leaf-0)))

(dotest
  (is= [[:raw 0]] (seq eid-0))
  (is= [[:raw 1]] (seq leaf-1))

  (is (neg? (lex/compare-lex [eid-0] [eid-1])))
  (is (zero? (lex/compare-lex [eid-0] [eid-0])))
  (is (pos? (lex/compare-lex [eid-1] [eid-0])))

  (is (neg? (lex/compare-lex [eid-0] [leaf-0])))
  (is (neg? (lex/compare-lex [eid-0] [leaf-1])))
  (is (neg? (lex/compare-lex [eid-1] [leaf-0])))
  (is (neg? (lex/compare-lex [eid-1] [leaf-1]))))

(dotest
  (let [ss123 (-> (avl/sorted-set)
                (conj 1)
                (conj 3)
                (conj 2))
        ss13  (disj ss123 2)]
    (is= #{1 2 3} ss123)
    (is= [1 2 3] (vec ss123))
    (is= #{1 3} ss13)))

(dotest   ; -1 => "in order",  0 => "same", +1 => "out of order"
  ; empty list is smaller than any non-empty list
  (is (neg? -99))
  (is (neg? (lex/compare-lex [] [2])))
  (is (neg? (lex/compare-lex [] [\b])))
  (is (neg? (lex/compare-lex [] ["b"])))
  (is (neg? (lex/compare-lex [] [:b])))
  (is (neg? (lex/compare-lex [] ['b])))
  (is (neg? (lex/compare-lex [] [nil])))

  ; nil is smaller than any non-nil item
  (is (neg? (lex/compare-lex [nil] [2])))
  (is (neg? (lex/compare-lex [nil] [\b])))
  (is (neg? (lex/compare-lex [nil] ["b"])))
  (is (neg? (lex/compare-lex [nil] [:b])))
  (is (neg? (lex/compare-lex [nil] ['b])))
  (is (neg? (lex/compare-lex [nil] [:b nil])))
  (is (neg? (lex/compare-lex [nil] [nil nil])))

  ; Can compare items from different classes:  number, char, string, keyword, symbol
  (isnt (zero? (lex/compare-lex [1] ["b"])))
  (isnt (zero? (lex/compare-lex [:b] [1])))
  (isnt (zero? (lex/compare-lex ['b] [1])))
  (isnt (zero? (lex/compare-lex [:b] ["b"])))
  (isnt (zero? (lex/compare-lex ['b] ["b"])))
  (isnt (zero? (lex/compare-lex [:b] ['b])))
  (isnt (zero? (lex/compare-lex [\b] [1])))
  (isnt (zero? (lex/compare-lex [:b] [\b])))
  (isnt (zero? (lex/compare-lex ['b] [\b])))

  #?(:clj  (isnt (zero? (lex/compare-lex [\b] ["b"])))
     :cljs (is (zero? (lex/compare-lex [\b] ["b"])))) ; On CLJS, char === len-1 String


  ; numeric types all compare as equal as with clojure.core/compare
  (is (zero? (lex/compare-lex [1] [1])))
  (is (zero? (lex/compare-lex [1] [1N])))
  (is (zero? (lex/compare-lex [1] [1.0])))
  (is (zero? (lex/compare-lex [1] [1.0M])))

  (is (zero? (lex/compare-lex [66] [66])))
  (is (zero? (lex/compare-lex [:a] [:a])))
  (is (zero? (lex/compare-lex ["abc"] ["abc"])))
  (is (zero? (lex/compare-lex [nil] [nil])))
  (is (zero? (lex/compare-lex [\a] [\a])))
  (is (zero? (lex/compare-lex [1 2] [1 2])))

  ; different positions in list can be of different class
  (is (neg? (lex/compare-lex [:a] [:b])))
  (is (neg? (lex/compare-lex [:a] [:a 1])))
  (is (neg? (lex/compare-lex [1 :a] [2])))
  (is (neg? (lex/compare-lex [:a] [:a 1])))
  (is (neg? (lex/compare-lex [1] [1 :a])))
  (is (neg? (lex/compare-lex [1 :a] [2])))
  (is (neg? (lex/compare-lex [1 nil] [1 2])))
  (is (neg? (lex/compare-lex [1 nil nil] [1 2])))
  (is (neg? (lex/compare-lex [1 2] [1 2 nil])))

  (is (neg? (lex/compare-lex [1 :z] [2 9])))
  (is (neg? (lex/compare-lex [1 :z] [1 2])))
  )


