;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.interval
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.core]
             [tupelo.misc]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.string :as str]
    [clojure.walk :as walk]
    [tupelo.misc :as misc]
    [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty nl
                               vals->map map-plain? forv glue keep-if]]
    [tupelo.interval :as interval]
    [tupelo.testy :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]])
  #?(:clj (:require [tupelo.types :as types]))
  )

#?(:cljs (enable-console-print!))

(dotest
  (is= (< 0 1.0) true)
  (is= (= 0 1.0) false)
  (is= (> 0 1.0) false)

  (is= (< 1 1.0) false)
  (is= (= 1 1.0) false) ; ***** integers and floating point values are never equal! *****
  (is= (== 1 1.0) true) ; *****  works for numbers (only) in different categories *****
  (is= (> 1 1.0) false)

  (is= (< 2 1.0) false)
  (is= (= 2 1.0) false)
  (is= (> 2 1.0) true)

  ; `compare` works correctly for numbers in different categories (eg int vs float)
  (is= (compare 0 1.0) -1)
  (is= (compare 1 1.0) 0)
  (is= (compare 2 1.0) 1)

  (let [itvl        (interval/new 1.0 5.0) ; float interval bounds vs integer values
        open-vals   (keep-if #(interval/open-contains? itvl %) (range 10))
        slice-vals  (keep-if #(interval/slice-contains? itvl %) (range 10))
        closed-vals (keep-if #(interval/closed-contains? itvl %) (range 10))]
    (is= open-vals [2 3 4])
    (is= slice-vals [1 2 3 4])
    (is= closed-vals [1 2 3 4 5])))


