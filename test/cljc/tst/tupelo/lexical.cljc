;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.lexical
  #?(:clj (:require
            [tupelo.test :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx]]
            [schema.core :as s]
            [clojure.data.avl :as avl]
            [tupelo.lexical :as lex]
            ))
  #?(:cljs (:require
             [tupelo.test-cljs :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?] :include-macros true]
             [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true ]
             [schema.core :as s :include-macros true ]
             [clojure.data.avl :as avl]
             [tupelo.lexical :as lex]
             ))
)

#?(:cljs (enable-console-print!))

; #todo fix for cljs
#?(:clj (do

(dotest
  (is= "clojure.lang.IPersistentMap" (lex/comparison-class {:a 1}))
  (is= "clojure.lang.Sequential" (lex/comparison-class (first {:a 1}))) ; a MapEntry
  (is= "clojure.lang.Sequential" (lex/comparison-class [1 2 3]))
  (is= "clojure.lang.Sequential" (lex/comparison-class (list 1 2 3)))
  (is= "clojure.lang.IPersistentSet" (lex/comparison-class #{1 3 2}))
  (is= "clojure.lang.Keyword" (lex/comparison-class :hello))
  (is= "clojure.lang.Symbol" (lex/comparison-class (symbol "hello")))
  (is= "java.lang.Number" (lex/comparison-class 3))
  (is= "java.lang.Number" (lex/comparison-class 3.14159))
  (is= "java.lang.String" (lex/comparison-class "hello"))
  (is= "java.lang.Character" (lex/comparison-class \X))
  (is= "java.lang.Boolean" (lex/comparison-class true)))

(dotest
  (defrecord Leaf [raw])
  (defrecord Eid [raw])
  (let [eid-0  (->Eid 0) ; "Eid" sorts before "Leaf"
        eid-1  (->Eid 1)
        leaf-0 (->Leaf 0)
        leaf-1 (->Leaf 1)]
    (is= "tst.tupelo.lexical.Eid" (lex/comparison-class eid-0))
    (is= "tst.tupelo.lexical.Leaf" (lex/comparison-class leaf-0))
    (is= [[:raw 0]] (seq eid-0))
    (is= [[:raw 1]] (seq leaf-1))

    (is (neg? (lex/compare-lex [eid-0] [eid-1])))
    (is (zero? (lex/compare-lex [eid-0] [eid-0])))
    (is (pos? (lex/compare-lex [eid-1] [eid-0])))

    (is (neg? (lex/compare-lex [eid-0] [leaf-0])))
    (is (neg? (lex/compare-lex [eid-0] [leaf-1])))
    (is (neg? (lex/compare-lex [eid-1] [leaf-0])))
    (is (neg? (lex/compare-lex [eid-1] [leaf-1]))) ))

(dotest
  (let [ss123 (t/it-> (avl/sorted-set)
                (conj it 1)
                (conj it 3)
                (conj it 2))
        ss13  (disj ss123 2) ]
    (is= #{1 2 3} ss123)
    (is= [1 2 3] (vec ss123))
    (is= #{1 3} ss13)) )

(dotest ; -1 => "in order",  0 => "same", +1 => "out of order"
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
  (is (neg? (lex/compare-lex [1] ["b"])))
  (is (neg? (lex/compare-lex [:b] [1])))
  (is (neg? (lex/compare-lex ['b] [1])))
  (is (neg? (lex/compare-lex [:b] ["b"])))
  (is (neg? (lex/compare-lex ['b] ["b"])))
  (is (neg? (lex/compare-lex [:b] ['b])))
  #?(:clj  ; char is only allowed in CLJ.
     (do
       (is (neg? (lex/compare-lex [\b] [1])))
       (is (neg? (lex/compare-lex [\b] ["b"])))
       (is (neg? (lex/compare-lex [:b] [\b])))
       (is (neg? (lex/compare-lex ['b] [\b])))))

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

))
