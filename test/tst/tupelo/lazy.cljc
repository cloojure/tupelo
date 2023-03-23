;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.lazy
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.testy] ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [schema.core :as s]
    [tupelo.lazy :as lazy]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty ]]
    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture ]]
    ))

; #todo add generative testing?
; #todo add clojure.spec testing?

(dotest
  (is= [] (lazy/join [[]]))
  (is= [1] (lazy/join [[1]]))
  (is= [1 2 3 ] (lazy/join [[1] [2 3]]))
  (is= [1 2 3 4 5 6] (lazy/join [[1] [2 3] [4 5 6]]))
  (is= [1 2 3 4 5 6] (lazy/join [[] [1] [] [2 3] [4 5 6] []])))

