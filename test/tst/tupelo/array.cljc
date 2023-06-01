;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.array
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.array]
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.array :as array]
    [tupelo.string :as ts]
    [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty forv vals->map glue truthy? falsey?
                               ]]
    [tupelo.test :refer [testing is verify verify-focus
                         is isnt is= isnt= is-set= is-nonblank=
                         throws? throws-not?
                         ]]
    ))
;---------------------------------------------------------------------------------------------------
#?(:cljs (enable-console-print!))
;---------------------------------------------------------------------------------------------------

(verify
  (let [a34  (array/new 3 4 :a)
        a34f (flatten a34)]
    (is= 3 (count a34) (array/num-rows a34))
    (is= 4 (count (a34 0)) (array/num-cols a34))
    (is= 12 (count a34f))
    (is (every? #(= :a %) a34f))
    (is (every? #(= :a %) (forv [ii (range (array/num-rows a34))
                                 jj (range (array/num-cols a34))]
                            (array/elem-get a34 ii jj)))))

  (let [a34  (array/new 3 4)
        a34f (flatten a34)]
    (is= 3 (count a34) (array/num-rows a34))
    (is= 4 (count (a34 0)) (array/num-cols a34))
    (is= 12 (count a34f))
    (is (every? nil? a34f))
    (is (every? nil? (forv [ii (range (array/num-rows a34))
                            jj (range (array/num-cols a34))]
                       (array/elem-get a34 ii jj)))))

  (is= (array/zeros 2 3)
    [[0 0 0]
     [0 0 0]])
  (is= (array/ones 3 2)
    [[1 1]
     [1 1]
     [1 1]]))

(verify
  (let [a34               (atom (array/new 3 4))
        target            [[00 01 02 03]
                           [10 11 12 13]
                           [20 21 22 23]]
        target-rows-vec   [00 01 02 03 10 11 12 13 20 21 22 23]
        target-cols-vec   [00 10 20 01 11 21 02 12 22 03 13 23]

        target-flip-ud    [[20 21 22 23]
                           [10 11 12 13]
                           [00 01 02 03]]

        target-flip-lr    [[03 02 01 00]
                           [13 12 11 10]
                           [23 22 21 20]]

        target-tx         [[00 10 20]
                           [01 11 21]
                           [02 12 22]
                           [03 13 23]]

        target-rot-left-1 [[03 13 23]
                           [02 12 22]
                           [01 11 21]
                           [00 10 20]]
        target-rot-left-2 [[23 22 21 20]
                           [13 12 11 10]
                           [03 02 01 00]]
        target-rot-left-3 [[20 10 00]
                           [21 11 01]
                           [22 12 02]
                           [23 13 03]]
        ]
    (when false
      (println \newline :awt01)
      (println (array/array->str target)))
    (dotimes [ii 3]
      (dotimes [jj 4]
        (do
          (let [elem-val (+ (* ii 10) jj)]
            (swap! a34 array/elem-set ii jj elem-val)))))
    (let [arr-val @a34
          str-val (array/array->str arr-val)]
      (is (ts/nonblank= str-val
            " 0    1    2    3    10   11   12   13    20   21   22   23")))

    (is= (array/row-get target 0) [00 01 02 03])
    (is= (array/row-get target 1) [10 11 12 13])
    (is= (array/row-get target 2) [20 21 22 23])

    (is= (array/col-get target 0) [00 10 20])
    (is= (array/col-get target 1) [01 11 21])
    (is= (array/col-get target 2) [02 12 22])
    (is= (array/col-get target 3) [03 13 23])

    (is= (array/array->row-vals target) [00 01 02 03
                                       10 11 12 13
                                       20 21 22 23])
    (is= (-> target (array/transpose) (array/array->col-vals)) [00 01 02 03
                                                            10 11 12 13
                                                            20 21 22 23])

    (is= target-rows-vec (array/array->row-vals target))
    (is= target-cols-vec (array/array->col-vals target))
    (is= target (array/row-vals->array 3 4 target-rows-vec))
    (is= target (array/col-vals->array 3 4 target-cols-vec))
    (is= target (->> target
                  (array/array->row-vals)
                  (array/row-vals->array 3 4)))
    (is= target (->> target
                  (array/array->col-vals)
                  (array/col-vals->array 3 4)))

    (is= target @a34)
    (is= target-flip-ud (array/flip-ud target))
    (is= target-flip-lr (array/flip-lr target))
    (is= target-tx (array/transpose target))

    (is= target-rot-left-1 (-> target (array/rotate-left)))
    (is= target-rot-left-2 (-> target (array/rotate-left) (array/rotate-left)))
    (is= target-rot-left-3 (-> target (array/rotate-left) (array/rotate-left) (array/rotate-left)))
    (is= target (-> target (array/rotate-left) (array/rotate-left) (array/rotate-left) (array/rotate-left)))

    (is= target-rot-left-3 (-> target (array/rotate-right)))
    (is= target-rot-left-2 (-> target (array/rotate-right) (array/rotate-right)))
    (is= target-rot-left-1 (-> target (array/rotate-right) (array/rotate-right) (array/rotate-right)))
    (is= target (-> target (array/rotate-right) (array/rotate-right) (array/rotate-right) (array/rotate-right)))
    ))

(verify
  (let [demo [[00 01 02 03]
              [10 11 12 13]
              [20 21 22 23]]
        ]
    (throws? (array/array->rows demo 0 0))
    (is= (array/array->rows demo 0 1) [[00 01 02 03]])
    (is= (array/array->rows demo 0 2) [[00 01 02 03]
                                     [10 11 12 13]])
    (is= (array/array->rows demo 0 3) [[00 01 02 03]
                                     [10 11 12 13]
                                     [20 21 22 23]])
    (is= (array/array->rows demo 1 3) [[10 11 12 13]
                                     [20 21 22 23]])
    (is= (array/array->rows demo 2 3) [[20 21 22 23]])
    (throws? (array/array->rows demo 3 3))

    (is= demo (array/array->rows demo))
    (is= (array/array->rows demo [2 0 1]) [[20 21 22 23]
                                         [00 01 02 03]
                                         [10 11 12 13]])
    (is= demo (array/edn-rows->array [[00 01 02 03]
                                    [10 11 12 13]
                                    [20 21 22 23]]))
    (throws? (array/edn-rows->array [[00 01 02 03]
                                   [10 11 12]
                                   [20 21 22 23]]))))

(verify
  (let [demo [[00 01 02 03]
              [10 11 12 13]
              [20 21 22 23]]
        ]
    (throws? (array/array->cols demo 0 0))
    (is= (array/array->cols demo 0 1) [[00 10 20]])
    (is= (array/array->cols demo 0 2) [[00 10 20]
                                     [01 11 21]])
    (is= (array/array->cols demo 0 3) [[00 10 20]
                                     [01 11 21]
                                     [02 12 22]])
    (is= (array/array->cols demo 0 4) [[00 10 20]
                                     [01 11 21]
                                     [02 12 22]
                                     [03 13 23]])
    (is= (array/array->cols demo 1 4) [[01 11 21]
                                     [02 12 22]
                                     [03 13 23]])
    (is= (array/array->cols demo 2 4) [[02 12 22]
                                     [03 13 23]])
    (is= (array/array->cols demo 3 4) [[03 13 23]])
    (throws? (array/array->cols demo 4 4))

    (is= (array/array->cols demo) (array/array->cols demo 0 4))
    (is= (array/array->cols demo [2 0 3 1]) [[02 12 22]
                                           [00 10 20]
                                           [03 13 23]
                                           [01 11 21]])
    (is= demo (array/edn-cols->array [[00 10 20]
                                    [01 11 21]
                                    [02 12 22]
                                    [03 13 23]]))
    (throws? (array/edn-cols->array [[00 10 20]
                                   [01 11 21]
                                   [02 12]
                                   [03 13 23]]))))

(verify
  (is (array/symmetric? [[1 2]
                       [2 1]]))
  (isnt (array/symmetric? [[1 3]
                         [2 1]]))
  (is (array/symmetric? [[1 2 3]
                       [2 4 5]
                       [3 5 6]]))
  (isnt (array/symmetric? [[1 9 3]
                         [2 4 5]
                         [3 5 6]]))
  (isnt (array/symmetric? [[1 2 9]
                         [2 4 5]
                         [3 5 6]])))

(verify
  (let [demo [[00 01 02 03]
              [10 11 12 13]
              [20 21 22 23]]]
    (is= (array/row-drop demo 0) [[10 11 12 13]
                                [20 21 22 23]])
    (is= (array/row-drop demo 0 2)
      (array/row-drop demo 0 2 0 2 0) [[10 11 12 13]])
    (is= (array/row-drop demo 1 2) [[00 01 02 03]])

    (is= (array/col-drop demo 1) [[00 02 03]
                                [10 12 13]
                                [20 22 23]])
    (is= (array/col-drop demo 1 2 2 1) [[00 03]
                                      [10 13]
                                      [20 23]])
    (is= (array/col-drop demo 0 2 3) [[01]
                                    [11]
                                    [21]])
    (throws? (array/row-drop demo :x))))

(verify
  (let [a13 [[00 01 02]]
        a23 [[00 01 02]
             [10 11 12]]
        a33 [[00 01 02]
             [10 11 12]
             [20 21 22]]
        a34 [[00 01 02 03]
             [10 11 12 13]
             [20 21 22 23]]]
    (throws? (array/rows-append a13 [1 2]))
    (throws? (array/rows-append a13 [1 2] [1 2 3]))
    (throws? (array/rows-append a13 [1 2 3 4]))
    (is= a23 (array/rows-append a13 [10 11 12]))
    (is= a33 (array/rows-append a13 [10 11 12] [20 21 22]))))

(verify
  (let [a22 [[00 01]
             [10 11]]
        a23 [[00 01 02]
             [10 11 12]]
        a24 [[00 01 02 03]
             [10 11 12 13]]]
    (throws? (array/cols-append a23 [1 2 3]))
    (throws? (array/cols-append a23 [1 2] [1 2 3]))
    (is= a23 (array/cols-append a22 [2 12]))
    (is= a24 (array/cols-append a22 [2 12] [3 13]))))

(verify
  (let [a12 [[00 01]]
        a22 [[00 01]
             [10 11]]
        a32 [[00 01]
             [10 11]
             [20 21]]
        a42 [[00 01]
             [10 11]
             [20 21]
             [30 31]]]
    (throws? (array/glue-vert a22 [[1 2 3]]))
    (is= a22 (array/glue-vert
               [[00 01]]
               [[10 11]]))
    (is= a32
      (array/glue-vert ; noop
        [[00 01]
         [10 11]
         [20 21]])
      (array/glue-vert
        [[00 01]]
        [[10 11]]
        [[20 21]])
      (array/glue-vert
        [[00 01]]
        [[10 11]
         [20 21]])
      (array/glue-vert
        [[00 01]
         [10 11]]
        [[20 21]]))

    (is= a42
      (array/glue-vert
        [[00 01]]
        [[10 11]]
        [[20 21]]
        [[30 31]])
      (array/glue-vert
        [[00 01]
         [10 11]]
        [[20 21]
         [30 31]])
      (array/glue-vert
        [[00 01]
         [10 11]
         [20 21]]
        [[30 31]])
      (array/glue-vert
        [[00 01]]
        [[10 11]
         [20 21]
         [30 31]]))))

(verify
  (let [a21 [[00]
             [10]]
        a22 [[00 01]
             [10 11]]
        a23 [[00 01 02]
             [10 11 12]]
        a24 [[00 01 02 03]
             [10 11 12 13]]]
    (throws? (array/glue-horiz a22 [[1 2 3]]))
    (is= a22 (array/glue-horiz a21
               [[01]
                [11]]))
    (is= a23 (array/glue-horiz a21
               [[01]
                [11]]
               [[02]
                [12]]))
    (is= a23 (array/glue-horiz a21
               [[01 02]
                [11 12]]))

    (is= a24 (array/glue-horiz a22
               [[02]
                [12]]
               [[03]
                [13]]))
    (is= a24 (array/glue-horiz a22
               [[02 03]
                [12 13]]))
    (is= a24 (array/glue-horiz a21
               [[01]
                [11]]
               [[02 03]
                [12 13]]))))

(verify
  (let [demo
        [[1 2 3]
         [4 5 6]]]
    (throws? (array/row-set demo 2 [[1 2 3]]))
    (throws? (array/row-set demo 1 [[1 2 3 4]]))
    (is= (array/row-set demo 1 [7 8 9]) [[1 2 3]
                                       [7 8 9]])

    (throws? (array/col-set demo 3 [[1 2]]))
    (throws? (array/col-set demo 1 [[1 2 3 4]]))
    (is= (array/col-set demo 1 [7 8]) [[1 7 3]
                                     [4 8 6]]))

  ; demonstrate diag fns
  (let [linear-array (array/row-vals->array 3 3 [0 1 2
                                               3 4 5
                                               6 7 8])]
    (is= [0 4 8] (array/diagonal-main linear-array))
    (is= [2 4 6] (array/diagonal-anti linear-array))))



