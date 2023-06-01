;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.chars
  (:refer-clojure :exclude [take drop])
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.chars :as chars]
    [tupelo.core :as t :refer [spyx spyx-pretty forv]]

    [tupelo.test :refer [deftest testing verify is isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not?  ]]
    ))

; #todo add generative testing?
; #todo add clojure.spec testing?

(verify
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
  (is (every? t/truthy? (mapv chars/text? chars/text))))

(verify
  (is (chars/lowercase? \a))
  (is (chars/uppercase? \A))
  (is (every? t/truthy? (t/it-> chars/uppercase
                          (mapv chars/->lowercase it)
                          (mapv chars/lowercase? it))))
  (is (every? t/truthy? (t/it-> chars/lowercase
                          (mapv chars/->uppercase it)
                          (mapv chars/uppercase? it)))))

