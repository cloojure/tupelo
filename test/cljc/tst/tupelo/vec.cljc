;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.vec
  (:use tupelo.vec tupelo.core)
  #?(:clj (:refer-clojure :exclude [load ->VecNode]))
  #?(:clj (:require
            [tupelo.test :refer [define-fixture deftest dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
            [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty unlazy let-spy only forv glue
                                       ]]
            [tupelo.data :as td]
            [tupelo.data.index :as tdi]
            [tupelo.lexical :as lex]
            [tupelo.vec :as tv]
            [clojure.data.avl :as avl]
            [schema.core :as s]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [tupelo.data.index :as index]))
  #?(:cljs (:require
             [tupelo.test-cljs :refer [define-fixture deftest dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]
              :include-macros true]
             [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
             [tupelo.data :as td]
             [tupelo.lexical :as lex]
             [clojure.data.avl :as avl]
             [schema.core :as s]
             ))
  )

; #todo fix for cljs

#?(:cljs (enable-console-print!))

(s/defn validate-unique  :- s/Any ; #todo move to tupelo.core
  "Validates that a collection has unique items"
  [coll]
  (assert (apply distinct? coll))
  coll)


(dotest
  (is= [2 1 0] (validate-indexes-complete [2 1 0]))
  (throws? (validate-indexes-complete [2 1 3]))
  (is= [3 2 1] (validate-unique [3 2 1]))
  (throws? (validate-unique [3 2 3]))
  (is= 0 (assert-index-bound 0 6))
  (is= 5 (assert-index-bound 5 6))
  (throws? (assert-index-bound -1 6))
  (throws? (assert-index-bound 3 3))
  (throws? (assert-index-bound 0 0))

  (is= [2 4 5] (tv/get (range 9) [2 4 5]))

  (let [decades  [0 10 20 30 40 50 60 70 80]
        result   (pred-index #(zero? (rem % 3)) decades)
        expected {:idxs-true  [0 3 6]
                  :idxs-false [1 2 4 5 7 8]}]
    (is= result expected)
    (t/with-map-vals result [idxs-true idxs-false] ; easy usage
      (is= idxs-true [0 3 6])
      (is= idxs-false [1 2 4 5 7 8])
      (is= [:div3 10 20 :div3 40 50 :div3 70 80]
        (spyx (tv/set decades idxs-true (repeat 3 :div3)))
        (set-lax decades idxs-true :div3))
      (is= [0 :not :not 30 :not :not 60 :not :not]
        (tv/set decades idxs-false (repeat 6 :not))
        (set-lax decades idxs-false (repeat 99 :not)) )
      (throws? (tv/set decades idxs-false (repeat 5 :not)))))

  )

