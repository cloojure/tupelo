;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.test
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.string :as str]) )

(verify
  (is true)         ; basic functionality
  (throws? (is))    ; must have exactly 1 arg
  (throws? (is true true)) ; must have exactly 1 arg

  (isnt false)
  (throws? (isnt))  ; must have exactly 1 arg
  (throws? (isnt false false)) ; must have exactly 1 arg

  (throws? (is= 1)) ; must have at least 2 args
  (is= 1 1)
  (is= 1 1 1)
  (is= 1 1 1 1)

  (throws? (isnt= 1))      ; must have at least 2 args
  (isnt= 1 2)
  (isnt= 1 2 1)
  (isnt= 1 2 1 1)

  (throws?     (do (throw (Exception. "oh the misery!"))))
  (throws-not? (do (+ 1 2)))

  ; default 4/7 decimals must agree for "equality"
  (is-float= 1.2345678 1.2345)
  (isnt-float= 1.2345678 1.234)

  ; default 10/15 decimals must agree for "equality"
  (is-double= 1.123456789012345 1.123456789)
  (isnt-double= 1.123456789012345 1.12345678)

  ; can redefine number of decimals required for "equality"
  (binding [tupelo.test/*equality-digits-double=* 4]
    (is-double= 1.2345678 1.2345)
    (isnt-double= 1.2345678 1.234))
  )

(verify
  (throws? (is-set= [1]))
  (is-set= [1] [1])
  (is-set= [1 2] [2 1])
  (is-set= [1 2 3 1 2 3] [2 1 3] [1 3 2] [3 2 1])
  ; (set= 1 2) ; must have colls as args ; (set 1) => exception

  (throws? (is-nonblank= "abc")) ; must have at least 2 args
  (is-nonblank= "abc" "abc"  )
  (is-nonblank= "abc" " abc   "  )
  (is-nonblank= "a   b  c" "a b c"  "  a b   c  ")
)

