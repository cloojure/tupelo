;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.quote
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.core]
             [tupelo.quote]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.quote :as q]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]]
    ))

#?(:cljs (enable-console-print!))

(def vec234 [2 3 4])

(dotest
  (println (q/tmpl-impl '[a b (insert (+ 2 3))]))
  )

(dotest
  (is (q/insert-form? (quote (insert (+ 2 3)))))
  (is (q/splice-form? (quote (splice (+ 2 3)))))

  (is= (q/tmpl-impl (quote {:a 1 :b (insert (+ 2 3))}))
    {:a 1, :b 5})
  (is= (q/tmpl-impl (quote [a b (insert (+ 2 3))]))
    (quote [a b 5]))

  (is= (q/tmpl {:a 1 :b (insert (+ 2 3))})
    {:a 1, :b 5})
  (is= (q/tmpl {:a 1 :b (insert (vec (range 3)))})
    {:a 1, :b [0 1 2]})
  (is= (q/tmpl {:a 1 :b (insert vec234)})
    {:a 1, :b [2 3 4]})

  (let [result (q/tmpl (list 1 2 (insert (inc 2)) 4 5))]
    (is (list? result))
    (is= result (quote (1 2 3 4 5))))

  (is= (q/tmpl-impl (quote [1 (splice (range 2 5)) 5]))
    [1 2 3 4 5])
  (is= (q/tmpl-impl (quote [1 (splice tst.tupelo.quote/vec234) 5])) ; must be fully-qualified Var here
    [1 2 3 4 5])
  (is= (q/tmpl [1 (splice vec234) 5]) ; unqualified name OK here
    [1 2 3 4 5])
  (is= (q/tmpl [1 (splice (t/thru 2 4)) 5])
    [1 2 3 4 95])
  (is= (q/tmpl [1 (insert (t/thru 2 4)) 5])
    [1 [2 3 4] 95])

  (is= 3 (eval (quote (+ 1 2))))


  ;(is=  99
  ;  (quote [a b 95]))
  )






