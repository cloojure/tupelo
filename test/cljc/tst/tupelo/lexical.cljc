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

(dotest
  (let [ss123 (t/it-> (avl/sorted-set)
                (conj it 1)
                (conj it 3)
                (conj it 2))
        ss13  (disj ss123 2) ]
    (is= #{1 2 3} ss123)
    (is= [1 2 3] (vec ss123))
    (is= #{1 3} ss13))

  (let [ss123 (t/it-> (lex/->sorted-set)
                (conj it [1 :a])
                (conj it [3 :a])
                (conj it [2 :a]))
        ss13  (disj ss123 [2 :a])]
    (is= #{[1 :a] [2 :a] [3 :a]} ss123)
    (is= [[1 :a] [2 :a] [3 :a]] (vec ss123))
    (is= #{[1 :a] [3 :a]} ss13)))

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

  ; Cannot compare items from different classes:  number, char, string, keyword, symbol
  (throws? (lex/compare-lex [1] ["b"]))
  (throws? (lex/compare-lex [1] [:b]))
  (throws? (lex/compare-lex [1] ['b]))
  (throws? (lex/compare-lex ["b"] [:b]))
  (throws? (lex/compare-lex ["b"] ['b]))
  (throws? (lex/compare-lex [:b] ['b]))
  #?(:clj
     (do
       (throws? (lex/compare-lex [1] [\b]))
       (throws? (lex/compare-lex [\b] ["b"]))
       (throws? (lex/compare-lex [\b] [:b]))
       (throws? (lex/compare-lex [\b] ['b]))))

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

  ; same position in list can be of different class if sorted by previous positions
  (is (neg? (lex/compare-lex [1 :z] [2 9]))) ; OK since prefix lists [1] & [2] define order
  (throws?  (lex/compare-lex [1 :z] [1 2])) ; not OK since have same prefix list: [1]

  (is= (vec (avl/sorted-set-by lex/compare-lex [1 :a] [1] [2]))
    [[1] [1 :a] [2]])
  (is= (vec (avl/sorted-set-by lex/compare-lex [1 :a] [1 nil] [1] [2]))
    [[1] [1 nil] [1 :a] [2]])
  (let [expected-vec [[1]
                      [1 nil]
                      [1 :a]
                      [1 :b]
                      [1 :b nil]
                      [1 :b nil 9]
                      [1 :b 3]
                      [2]
                      [2 0]
                      [3]
                      [3 :y]]
        expected-set (lex/->sorted-set expected-vec)
        data         (reverse expected-vec)
        result-set   (apply avl/sorted-set-by lex/compare-lex data)
        result-vec   (vec result-set)]
    (is= result-vec expected-vec)
    (is= result-set expected-set) )
  (let [expected   [[1]
                    [1 nil]
                    [1 nil nil]
                    [1 nil 9]
                    [1 2]
                    [1 2 nil]
                    [1 2 3]]
        data       (reverse expected)
        result-vec (vec (lex/->sorted-set data))]
    (is= result-vec expected)))

(dotest
  (println "********* running tupelo.lexical tests ********* ")
  (let [lex-set (avl/sorted-set 1 2 3)
        lex-map (avl/sorted-map :a 1 :b 2 :c 3)]
    (s/validate lex/Set lex-set)
    (s/validate lex/Map lex-map)
    (is= #{1 2 3} lex-set)
    (is= {:a 1 :b 2 :c 3} lex-map))

  (let [data-raw (lex/->sorted-set #{[:b 1] [:b 2] [:b 3]
                                     [:f 1] [:f 2] [:f 3]
                                     [:h 1] [:h 2]})]
    ; test with prefix-key
    (is= (lex/split-key-prefix (lex/bound-lower [:a 2]) data-raw)
      {:smaller #{},
       :matches #{},
       :larger  #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix (lex/bound-lower [:b 2]) data-raw)
      {:smaller #{}
       :matches #{[:b 1] [:b 2] [:b 3]},
       :larger  #{[:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix (lex/bound-lower [:c 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3]},
       :matches #{}
       :larger  #{[:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix (lex/bound-lower [:f 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3]},
       :matches #{[:f 1] [:f 2] [:f 3]},
       :larger  #{[:h 1] [:h 2]}})
    (is= (lex/split-key-prefix (lex/bound-lower [:g 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3]},
       :matches #{},
       :larger  #{[:h 1] [:h 2]}})
    (is= (lex/split-key-prefix (lex/bound-lower [:h 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3]},
       :matches #{[:h 1] [:h 2]}
       :larger  #{}})
    (is= (lex/split-key-prefix (lex/bound-lower [:joker 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]},
       :matches #{}
       :larger  #{}}))

  ; test with full-key
  (let [data-raw (lex/->sorted-set #{[:b 1] [:b 2] [:b 3]
                                     [:f 1] [:f 2] [:f 3]
                                     [:h 1] [:h 2]})]
    (is= (lex/split-key-prefix [:a 2] data-raw)
      {:smaller #{},
       :matches #{},
       :larger  #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix [:b 2] data-raw)
      {:smaller #{[:b 1]}
       :matches #{ [:b 2] },
       :larger  #{[:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix [:c 2] data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3]},
       :matches #{}
       :larger  #{[:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix [:f 2] data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1]},
       :matches #{[:f 2]},
       :larger  #{[:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key-prefix [:g 2] data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3]},
       :matches #{},
       :larger  #{[:h 1] [:h 2]}})
    (is= (lex/split-key-prefix [:h 2] data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1]},
       :matches #{ [:h 2]}
       :larger  #{}})
    (is= (lex/split-key-prefix [:joker 2] data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]},
       :matches #{}
       :larger  #{}}))

  )






















