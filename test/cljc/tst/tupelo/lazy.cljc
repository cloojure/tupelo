;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.lazy
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.lazy :as lazy]

    #?(:clj  [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty ]]
       :cljs [tupelo.core :as t :include-macros true :refer [spy spyx spyxx spyx-pretty]])

    #?(:clj [clojure.test] :cljs [cljs.test])
    #?(:clj  [tupelo.test :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank= throws? throws-not? define-fixture]]
       :cljs [tupelo.test-cljs ; :include-macros true
              :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank= throws? throws-not? define-fixture]])

    ))

; #todo add generative testing?
; #todo add clojure.spec testing?

(dotest
  (is= [] (lazy/join [[]]))
  (is= [1] (lazy/join [[1]]))
  (is= [1 2 3 ] (lazy/join [[1] [2 3]]))
  (is= [1 2 3 4 5 6] (lazy/join [[1] [2 3] [4 5 6]]))
  (is= [1 2 3 4 5 6] (lazy/join [[] [1] [] [2 3] [4 5 6] []])))

