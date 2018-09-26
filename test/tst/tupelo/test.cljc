;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.test
  #?@(:clj
      [(:use tupelo.core tupelo.test)
       (:require
         [clojure.string :as str] )
      ]))

(deftest
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
  )

(deftest
  (throws? (set= [1]))
  (set= [1] [1])
  (set= [1 2] [2 1])
  (set= [1 2 3 1 2 3] [2 1 3] [1 3 2] [3 2 1])
  ; (set= 1 2) ; must have colls as args ; (set 1) => exception

  (throws? (nonblank= "abc")) ; must have at least 2 args
  (nonblank= "abc" "abc"  )
  (nonblank= "abc" " abc   "  )
  (nonblank= "a   b  c" "a b c"  "  a b   c  ")
  )


