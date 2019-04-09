;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.lexical
  ; (:refer-clojure :exclude [compare load ->VecNode])
  #?(:clj (:require
            [tupelo.test :as ttst :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx]]
            [tupelo.lexical :as lex]
            [tupelo.string :as ts]
            [clojure.data.avl :as avl]
            [schema.core :as s]
          ))
  #?(:cljs (:require
             [tupelo.test-cljs :include-macros true :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]]
             [tupelo.core :include-macros true :as t :refer [spy spyx spyxx]]
             [tupelo.lexical :as lex]
             [tupelo.string  :as ts]
             [clojure.data.avl :as avl]
             [schema.core :as s]
           )))

(dotest ; -1 => "in order",  0 => "same", +1 => "out of order"
  ; empty list is smaller than any non-empty list
  (is (neg? (lex/compare [] [2])))
  (is (neg? (lex/compare [] [\b])))
  (is (neg? (lex/compare [] ["b"])))
  (is (neg? (lex/compare [] [:b])))
  (is (neg? (lex/compare [] ['b])))
  (is (neg? (lex/compare [] [nil])))

  ; nil is smaller than any non-nil item
  (is (neg? (lex/compare [nil] [2])))
  (is (neg? (lex/compare [nil] [\b])))
  (is (neg? (lex/compare [nil] ["b"])))
  (is (neg? (lex/compare [nil] [:b])))
  (is (neg? (lex/compare [nil] ['b])))
  (is (neg? (lex/compare [nil] [:b nil])))
  (is (neg? (lex/compare [nil] [nil nil])))

  ; Cannot compare items from different classes:  number, char, string, keyword, symbol
  (throws? (lex/compare [1] ["b"]))
  (throws? (lex/compare [1] [:b]))
  (throws? (lex/compare [1] ['b]))
  (throws? (lex/compare ["b"] [:b]))
  (throws? (lex/compare ["b"] ['b]))
  (throws? (lex/compare [:b] ['b]))
  #?(:clj
     (do
       (throws? (lex/compare [1] [\b]))
       (throws? (lex/compare [\b] ["b"]))
       (throws? (lex/compare [\b] [:b]))
       (throws? (lex/compare [\b] ['b]))))

  (is (zero? (lex/compare [66] [66])))
  (is (zero? (lex/compare [:a] [:a])))
  (is (zero? (lex/compare ["abc"] ["abc"])))
  (is (zero? (lex/compare [nil] [nil])))
  (is (zero? (lex/compare [\a] [\a])))
  (is (zero? (lex/compare [1 2] [1 2])))

  ; different positions in list can be of different class
  (is (neg? (lex/compare [:a] [:b])))
  (is (neg? (lex/compare [:a] [:a 1])))
  (is (neg? (lex/compare [1 :a] [2])))
  (is (neg? (lex/compare [:a] [:a 1])))
  (is (neg? (lex/compare [1] [1 :a])))
  (is (neg? (lex/compare [1 :a] [2])))
  (is (neg? (lex/compare [1 nil] [1 2])))
  (is (neg? (lex/compare [1 nil nil] [1 2])))
  (is (neg? (lex/compare [1 2] [1 2 nil])))

  ; same position in list can be of different class if sorted by previous positions
  (is (neg? (lex/compare [1 :z] [2 9]))) ; OK since prefix lists [1] & [2] define order
  (throws?  (lex/compare [1 :z] [1 2])) ; not OK since have same prefix list: [1]

  (is= (vec (avl/sorted-set-by lex/compare [1 :a] [1] [2]))
    [[1] [1 :a] [2]])
  (is= (vec (avl/sorted-set-by lex/compare [1 :a] [1 nil] [1] [2]))
    [[1] [1 nil] [1 :a] [2]])
  (let [got-set      (avl/sorted-set-by lex/compare [2 0] [2] [3] [3 :y] [1] [1 :a] [1 nil] [1 :b nil 9] [1 :b nil] [1 :b] [1 :b 3])
        got-vec      (vec got-set)
        expected-vec [[1]
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
        expected-set (lex/->sorted-set expected-vec)]
    (is= got-vec expected-vec)
    (is= got-set expected-set)
    ))


(dotest-focus
  (let [lex-set (avl/sorted-set 1 2 3)
        lex-map (avl/sorted-map :a 1 :b 2 :c 3)]
    (s/validate lex/Set lex-set)
    (s/validate lex/Map lex-map)
    (is= #{1 2 3} lex-set)
    (is= {:a 1 :b 2 :c 3} lex-map))

  (let [data-raw (lex/->sorted-set #{[:b 1] [:b 2] [:b 3]
                                     [:f 1] [:f 2] [:f 3]
                                     [:h 1] [:h 2]})]
    (is= (lex/split-key (lex/bound-lower [:a 2]) data-raw)
      {:smaller #{},
       :matches #{},
       :larger  #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key (lex/bound-lower [:b 2]) data-raw)
      {:smaller #{}
       :matches #{[:b 1] [:b 2] [:b 3]},
       :larger  #{[:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key (lex/bound-lower [:c 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3]},
       :matches #{}
       :larger  #{[:f 1] [:f 2] [:f 3] [:h 1] [:h 2]}})
    (is= (lex/split-key (lex/bound-lower [:f 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3]},
       :matches #{[:f 1] [:f 2] [:f 3]},
       :larger  #{[:h 1] [:h 2]}})
    (is= (lex/split-key (lex/bound-lower [:g 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3]},
       :matches #{},
       :larger  #{[:h 1] [:h 2]}})
    (is= (lex/split-key (lex/bound-lower [:h 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3]},
       :matches #{[:h 1] [:h 2]}
       :larger  #{}})
    (is= (lex/split-key (lex/bound-lower [:joker 2]) data-raw)
      {:smaller #{[:b 1] [:b 2] [:b 3] [:f 1] [:f 2] [:f 3] [:h 1] [:h 2]},
       :matches #{}
       :larger  #{}}))

  )






















