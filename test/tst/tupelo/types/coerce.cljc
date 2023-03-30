;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.types.coerce
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.testy]))
  (:require
    [clojure.test]  ; sometimes this is required - not sure why
    [tupelo.types.coerce :as coerce]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.testy :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]])
  #?(:clj (:import [java.lang Math]))
  )

#?(:cljs (enable-console-print!))

#?(:clj
   (do

     (dotest
       (is= 15
         (coerce/->byte "   15")
         (coerce/->byte "15 ")
         (coerce/->byte 15.0)
         (coerce/->byte 15N)
         (coerce/->byte 15M))
       (throws? (coerce/->byte " 1 5 "))
       (throws? (coerce/->byte "fred"))
       (throws? (coerce/->byte "")))

     (dotest
       (is= 15
         (coerce/->short "   15")
         (coerce/->short "15 ")
         (coerce/->short 15.0)
         (coerce/->short 15N)
         (coerce/->short 15M))
       (throws? (coerce/->short " 1 5 "))
       (throws? (coerce/->short "fred"))
       (throws? (coerce/->short "")))

     (dotest
       (is= 15
         (coerce/->int "   15")
         (coerce/->int "15 ")
         (coerce/->int 15.0)
         (coerce/->int 15N)
         (coerce/->int 15M))
       (throws? (coerce/->int " 1 5 "))
       (throws? (coerce/->int "fred"))
       (throws? (coerce/->int "")))

     (dotest
       (is= 15
         (coerce/->long "   15")
         (coerce/->long "15 ")
         (coerce/->long 15.0)
         (coerce/->long 15N)
         (coerce/->long 15M))
       (throws? (coerce/->long " 1 5 "))
       (throws? (coerce/->long "fred"))
       (throws? (coerce/->long "")))

     (dotest
       (is= 15.0
         (coerce/->float "15")
         (coerce/->float "15.0")
         (coerce/->float 15)
         (coerce/->float 15N)
         (coerce/->float 15M)
         )
       (throws? (coerce/->float ""))
       (throws? (coerce/->float "xyz"))
       (is= 0.5 (coerce/->float ".5"))

       (is (t/rel= 1/10 (coerce/->double "0.1") :digits 9)))

     (dotest
       (is= 15.0
         (coerce/->double "15")
         (coerce/->double "15.0")
         (coerce/->double 15)
         (coerce/->double 15N)
         (coerce/->double 15M)
         )
       (throws? (coerce/->double ""))
       (throws? (coerce/->double "xyz"))
       (is= 0.5 (coerce/->double ".5"))

       (is (t/rel= 1/10 (coerce/->double "0.1") :digits 9)))
     ))

;#?(:cljs
;   (do
;       (dotest
;         (is= 0 (tpar/->int "0"))
;         (is= 15 (tpar/->int "15"))
;         (is= -5 (tpar/->int "-5"))
;         (is= 99999 (tpar/->int "99999"))
;
;         (throws? (tpar/->int ""))
;         (throws? (tpar/->int "05"))
;         (throws? (tpar/->int "123xxx"))
;         (throws? (tpar/->int "12x3"))
;         (throws? (tpar/->int "12.3"))
;         (throws? (tpar/->int "xxx123")))
;
;       (dotest
;         (is= 0 (tpar/->float "0"))
;         (is= 0 (tpar/->float "0.0"))
;         (is= 12.345 (tpar/->float "12.345"))
;         (is= -5.1 (tpar/->float "-5.1"))
;         (is= 42 (tpar/->float "42.0"))
;         (is= 42 (tpar/->float "42"))
;         (is= 123.45 (tpar/->float "1.2345e2"))
;
;         (throws? (tpar/->float ""))
;         (throws? (tpar/->float "xxx1.23"))
;         (throws? (tpar/->float "1.23xxx"))
;         (throws? (tpar/->float "1.2xx34")))
;       ))
