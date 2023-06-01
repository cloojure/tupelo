;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.set
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.set :as set]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.test :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture ]]
  ))

(dotest
  (is= #{1 2 3 4 5} (set/union #{1 2 3 4} #{2 3 4 5}))
  (throws? (set/union #{1 2 3 4} {2 3 4 5}))
  (throws? (set/union #{1 2 3 4} [2 3 4 5])))

(dotest
  (is= #{:a} (set/add nil :a))
  (is= #{:a :b} (set/add #{:a} :b))
  (is= #{:a :b :c} (set/add #{:a} :a :b :c))

  (is= #{:b} (set/remove #{:a :b} :a))
  (is= #{:a :b} (set/remove #{:a :b} :zzz))
  (is= #{:b :c} (set/remove #{:a :b :c} :a))
  (is= #{:c} (set/remove #{:a :b :c} :a :b))
  (is= #{} (set/remove #{:a} :a))
  (is= #{} (set/remove nil :a))
  )

; #todo tests for clojure.set stuff

