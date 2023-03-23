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
             ; [tupelo.core]
             [tupelo.quote]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.walk :as walk]
    [tupelo.quote :as q]
    [tupelo.core :as t :refer [glue grab it-> forv spy spyx spyxx vals->map
                               xfirst xsecond]]
    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]]
    ))

#?(:cljs (enable-console-print!))

(dotest
  (is= (q/tmpl-fn (quote [a b (insert (+ 2 3))]))
    (quote [a b 5]))
  (let [a 1
        b 2]
    (is= `[a b ~(+ 2 3)] ; problem with clojure.core/syntax-quote
      (quote [tst.tupelo.quote/a tst.tupelo.quote/b 5]))
    (is= (q/tmpl [a b (insert (+ 2 3))])
      [1 2 5]) ; desired result
    ))

(def vec234 [2 3 4])

(dotest
  (is (q/insert-form? (quote (insert (+ 2 3)))))
  (is (q/splice-form? (quote (splice (+ 2 3)))))

  (is= (q/tmpl-fn (quote {:a 1 :b (insert (+ 2 3))}))
    {:a 1, :b 5})
  (is= (q/tmpl-fn (quote [a b (insert (+ 2 3))]))
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

  (is= (q/tmpl-fn (quote [1 (splice (range 2 5)) 5]))
    [1 2 3 4 5])
  (is= (q/tmpl-fn (quote [1 (splice tst.tupelo.quote/vec234) 5])) ; must be fully-qualified Var here
    [1 2 3 4 5])
  (is= (q/tmpl [1 (splice vec234) 5]) ; unqualified name OK here
    [1 2 3 4 5])
  (is= (q/tmpl [1 (splice (t/thru 2 4)) 5])
    [1 2 3 4 5])
  (is= (q/tmpl [1 (insert (t/thru 2 4)) 5])
    [1 [2 3 4] 5])

  (is= [1 [2 3 4] 5] (q/tmpl [1 (insert (t/thru 2 4)) 5]))
  (is= [1 2 3 4 5] (q/tmpl [1 (splice (t/thru 2 4)) 5]))

  (is= 3 (eval (quote (+ 1 2)))))

(comment  ; demo
  (let [a 1
        b (inc a)]
    (println :1 (quote [a b]))
    (println :2 '{:out [~a ~b]}) ; ***** doesn't work *****
    (println :3 `{:out [~a ~b]})
    ; (println (td/quote-template {:out (unquote [a b])})) ; ***** fails due to locals *****
    (println :4 (q/tmpl {:out (insert [a-1400 b-1400])})) ; globals are OK
    (println :5 (q/tmpl {:a 1 :b (insert (+ 2 3))})) ; global function works too
    ;(println  :6 (q/tmpl {:out (insert [a b])})) ; fails when try to use locals
    ))

; #todo review & merge (from tupelo.data/quoted ns)
(comment
  (ns tst.tupelo.quoted
    (:use tupelo.core tupelo.test)
    (:require
      [clojure.walk :as walk]
      ))

  (comment
    (defn unquoted-form?
      [form]
      (and (list? form)
        (= (quote unquoted) (first form))))

    (defn quoted-impl
      [form]
      (spyx form)
      (let [env-pairs   (atom [])
            form-walked (walk/prewalk (fn [item]
                                        (t/with-nil-default item
                                          (when (unquoted-form? item)
                                            (let ; -spy
                                              [payload (only (rest item))
                                               gsym    (gensym "unq-val-")
                                               pair    [gsym payload]]
                                              (swap! env-pairs t/append pair)
                                              gsym))))
                          form)
            >>          (spyx-pretty env-pairs)
            >>          (spyx-pretty form-walked)
            let-form    (list
                          (quote let)
                          (apply glue @env-pairs)
                          form-walked)]
        let-form))

    (defmacro quoted
      [form] (quoted-impl form))

    (defmacro shower
      [& form]
      `(prn (quote ~form)))

    (dotest
      (is (unquoted-form? (quote (unquoted xxx))))
      (isnt (unquoted-form? (quote (something xxx))))

      (comment ; or do
        (spy-pretty :impl-4
          (quoted-impl (quote
                         (+
                           (unquoted (+ 1 2))
                           (unquoted (+ 3 4))
                           (unquoted (+ 5 6))
                           (unquoted (+ 7 8))))))
        (comment ; =>  (sample result)
          (let [unq-val-32909 (+ 1 2)
                unq-val-32910 (+ 3 4)
                unq-val-32911 (+ 5 6)
                unq-val-32912 (+ 7 8)]
            (+ unq-val-32909 unq-val-32910 unq-val-32911 unq-val-32912))))

      (newline)
      (println :-----------------------------------------------------------------------------)
      (spy-pretty :shower
        (quoted-impl (quote
                       (shower (+
                                 (unquoted (+ 1 2))
                                 (unquoted (+ 3 4))
                                 (unquoted (+ 5 6))
                                 (unquoted (+ 7 8)))))))) )

  )
