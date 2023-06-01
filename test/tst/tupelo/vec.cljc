;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.vec
  #?(:clj (:refer-clojure :exclude [load ->VecNode]))
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             ; [tupelo.core]
             [tupelo.misc]
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [schema.core :as s]
    [tupelo.vec :as tv]
    [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty unlazy let-spy only forv glue ]]
    [tupelo.test :refer [testing is verify verify-focus
                         is isnt is= isnt= is-set= is-nonblank= is-nonblank-lines=
                         throws? throws-not?
                         ]]
    ))

; #todo fix for cljs

#?(:cljs (enable-console-print!))

(s/defn validate-unique  :- s/Any ; #todo move to tupelo.core
  "Validates that a collection has unique items"
  [coll]
  (assert (apply distinct? coll))
  coll)


(verify
  (is= [2 1 0] (tv/validate-indexes-complete [2 1 0]))
  (throws? (tv/validate-indexes-complete [2 1 3]))
  (is= [3 2 1] (validate-unique [3 2 1]))
  (throws? (validate-unique [3 2 3]))

  (let [r5 (range 5)]
    (is= [0] (tv/verify-idxs r5 [0]))
    (is= [4] (tv/verify-idxs r5 [4]))
    (is= r5 (tv/verify-idxs r5 r5))
    (throws? (tv/verify-idxs (range 5) 5))
    (throws? (tv/verify-idxs (range 0) 0)))

  (let [r5 (vec (range 5))]
    (is= [3] (tv/get r5 3))
    (is= [3] (tv/get r5 [3]))
    (is= [2 3 4] (tv/get r5 [2 3 4]))

    (is= [0 1 9 3 4] (tv/set r5 2 [9]))
    (is= [0 1 9 3 4] (tv/set r5 [2] [9]))
    (is= [0 1 12 13 4] (tv/set r5 [2 3] [12 13]))

    (is= [1 3] (tv/del r5 [0 2 4]))
    ))

 (verify
  (let [decades  [0 10 20 30 40 50 60 70 80]
        result   (tv/pred-index #(zero? (rem % 3)) decades)
        expected {:idxs-true  [0 3 6]
                  :idxs-false [1 2 4 5 7 8]}]
    (is= result expected)
    (t/with-map-vals result [idxs-true idxs-false] ; easy usage
      (is= idxs-true [0 3 6])
      (is= idxs-false [1 2 4 5 7 8])
      (is= [:div3 10 20 :div3 40 50 :div3 70 80]
        (tv/set decades idxs-true (repeat 3 :div3))
        (tv/set-lax decades idxs-true (repeat 33 :div3))
        (tv/set-lax decades idxs-true :div3) )
      (is= [0 :not :not 30 :not :not 60 :not :not]
        (tv/set decades idxs-false (repeat 6 :not))
        (tv/set-lax decades idxs-false (repeat 99 :not))
        (tv/set-lax decades idxs-false :not) )
      (throws? (tv/set decades idxs-false (repeat 5 :not)))))

  )










