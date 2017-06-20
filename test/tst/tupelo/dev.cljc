;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.dev
  (:use tupelo.dev tupelo.test)
  (:require
    [tupelo.core :as t]
  ))
(t/refer-tupelo)


(dotest
  (let [data-1 [1 2 3]
        data-2 [[1 2 3]
                [10 11]
                []]
        data-3 [[[1 2 3]
                 [4 5 6]
                 [7 8 9]]
                [[10 11]
                 [12 13]]
                [[20]
                 [21]]
                [[30]]
                [[]]]
        data-4 [[[1 2 3]
                 [4 5 6]
                 [7 8 9]]
                [[10 11]
                 [12  2]]
                [[20]
                 [21]]
                [[30]]
                [[2]]]
        ]
    (is= (index data-1  2) [{:idxs [1], :val 2}])
    (is= (index data-2 10) [{:idxs [1 0], :val 10}] )
    (is= (index data-3 13) [{:idxs [1 1 1], :val 13}])
    (is= (index data-3 21) [{:idxs [2 1 0], :val 21}])
    (is= (index data-3 99) [])
    (is= (index data-4 2) [{:idxs [0 0 1], :val 2}
                           {:idxs [1 1 1], :val 2}
                           {:idxs [4 0 0], :val 2}])
    ))


