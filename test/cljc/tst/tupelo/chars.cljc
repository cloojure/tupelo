;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns       ; ^:test-refresh/focus
  tst.tupelo.chars
  (:refer-clojure :exclude [take drop])
  (:require
    [clojure.core :as cc ]
    [tupelo.chars :as chars]

    #?(:clj  [tupelo.core :as t :refer [spyx spyx-pretty forv]]
       :cljs [tupelo.core :as t :include-macros true])

    #?(:clj [clojure.test] :cljs [cljs.test] )
    #?(:clj  [tupelo.test :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set=
                                  is-nonblank= is-nonblank-lines= throws? define-fixture]]
       :cljs [tupelo.test-cljs :include-macros true
              :refer [deftest testing is dotest isnt is= isnt= is-set=
                      is-nonblank= is-nonblank-lines= throws? define-fixture]])
    ))

; #todo add generative testing?
; #todo add clojure.spec testing?

(dotest
  (is (every? t/truthy? (mapv chars/lowercase? chars/lowercase)))
  (is (every? t/truthy? (mapv chars/uppercase? chars/uppercase)))

  (is (every? t/truthy? (mapv chars/alphanumeric? chars/alphanumeric)))
  (is (every? t/truthy? (mapv chars/whitespace-horiz? chars/whitespace-horiz)))
  (is (every? t/truthy? (mapv chars/whitespace-eol? chars/whitespace-eol)))
  (is (every? t/truthy? (mapv chars/whitespace? chars/whitespace)))
  (is (every? t/truthy? (mapv chars/digit? chars/digit)))
  (is (every? t/truthy? (mapv chars/hex? chars/hex)))
  (is (every? t/truthy? (mapv chars/alpha? chars/alpha)))
  (is (every? t/truthy? (mapv chars/visible? chars/visible)))
  (is (every? t/truthy? (mapv chars/text? chars/text)))

  )

#?(:clj   ; #todo make work for cljs
   (do

     (dotest
       (is (chars/lowercase? \a))
       (is (chars/uppercase? \A))
       (is (every? t/truthy? (t/it-> chars/uppercase
                               (mapv chars/->lowercase it)
                               (mapv chars/lowercase? it))))
       (is (every? t/truthy? (t/it-> chars/lowercase
                               (mapv chars/->uppercase it)
                               (mapv chars/uppercase? it))))

       )

     ))
