;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.parse.coerce
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.testy]))
  (:require
    [clojure.test]  ; sometimes this is required - not sure why
    [tupelo.parse.coerce :as coerce]
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


     (comment
       (dotest
         (is= 15 (coerce/->byte "15"))
         (is= -5 (coerce/->byte "-5"))
         (throws? NumberFormatException (coerce/->byte "999"))
         (throws? NumberFormatException (coerce/->byte " "))

         (is= 15 (coerce/->byte "15" :default nil))
         (is= -5 (coerce/->byte "-5" :default nil))
         (is= nil (coerce/->byte "999" :default nil))
         (is= nil (coerce/->byte "" :default nil))
         (is= 0 (coerce/->byte "xyz" :default 0)))

       (dotest
         (is= 15 (coerce/->short "15"))
         (is= -5 (coerce/->short "-5"))
         (is= 999 (coerce/->short "999"))
         (throws? NumberFormatException (coerce/->short "99999"))
         (throws? NumberFormatException (coerce/->short " "))

         (is= 15 (coerce/->short "15" :default nil))
         (is= -5 (coerce/->short "-5" :default nil))
         (is= 999 (coerce/->short "999" :default nil))
         (is= nil (coerce/->short "99999" :default nil))
         (is= nil (coerce/->short "" :default nil))
         (is= 0 (coerce/->short "xyz" :default 0)))

       (dotest
         (is= 15 (coerce/->int "15"))
         (is= -5 (coerce/->int "-5"))
         (is= 99999 (coerce/->int "99999"))
         (throws? NumberFormatException (coerce/->int "9876543210"))
         (throws? NumberFormatException (coerce/->int ""))

         (is= 15 (coerce/->int "15" :default nil))
         (is= -5 (coerce/->int "-5" :default nil))
         (is= 99999 (coerce/->int "99999" :default nil))
         (is= nil (coerce/->int "9876543210" :default nil))
         (is= nil (coerce/->int "" :default nil))
         (is= 0 (coerce/->int "xyz" :default 0)))

       (dotest
         (is= 15.0 (coerce/->float "15"))
         (is= -5.0 (coerce/->float "-5"))
         (is= 0.5 (coerce/->float "0.5"))
         (is (t/rel= 0.1 (coerce/->float "0.1") :digits 7))
         (is (t/rel= 3.141592654 (coerce/->float "3.141592654") :digits 7))
         (throws? NumberFormatException (coerce/->float ""))
         (throws? NumberFormatException (coerce/->float "xyz")

           (is= 15.0 (coerce/->float "15" :default nil))
           (is= -5.0 (coerce/->float "-5" :default nil))
           (is= nil (coerce/->float "" :default nil))
           (is= 0 (coerce/->float "xyz" :default 0))
           (is= 0.5 (coerce/->float "0.5" :default nil))
           (is (t/rel= (/ 1 10) (coerce/->float "0.1" :default 0) :digits 7))
           (is (t/rel= 3.141592654 (coerce/->float "3.141592654" :default 0) :digits 7))))


       (dotest
         (is= 15.0M (coerce/->decimal "15"))
         (is= -5.0M (coerce/->decimal "-5"))
         (throws? NumberFormatException (coerce/->decimal ""))
         (throws? NumberFormatException (coerce/->decimal "xyz"))
         (is= 0.5M (coerce/->decimal "0.5"))
         (is (= 0.1M (coerce/->decimal "0.1")))
         (is (t/rel= Math/PI (coerce/->decimal "3.141592654") :digits 9))

         (is= 15.0M (coerce/->decimal {:default nil} "15"))
         (is= -5.0M (coerce/->decimal {:default nil} "-5"))
         (is= nil (coerce/->decimal {:default nil} ""))
         (is= 0 (coerce/->decimal {:default 0} "xyz"))
         (is= 0.5M (coerce/->decimal {:default nil} "0.5"))
         (is (t/rel= (/ 1 10) (coerce/->decimal {:default 0} "0.1") :digits 9))
         (is (t/rel= Math/PI (coerce/->decimal {:default 0} "3.141592654") :digits 9))))

     ))

#?(:cljs
   #_(do
       (dotest
         (is= 0 (tpar/->int "0"))
         (is= 15 (tpar/->int "15"))
         (is= -5 (tpar/->int "-5"))
         (is= 99999 (tpar/->int "99999"))

         (throws? (tpar/->int ""))
         (throws? (tpar/->int "05"))
         (throws? (tpar/->int "123xxx"))
         (throws? (tpar/->int "12x3"))
         (throws? (tpar/->int "12.3"))
         (throws? (tpar/->int "xxx123")))

       (dotest
         (is= 0 (tpar/->float "0"))
         (is= 0 (tpar/->float "0.0"))
         (is= 12.345 (tpar/->float "12.345"))
         (is= -5.1 (tpar/->float "-5.1"))
         (is= 42 (tpar/->float "42.0"))
         (is= 42 (tpar/->float "42"))
         (is= 123.45 (tpar/->float "1.2345e2"))

         (throws? (tpar/->float ""))
         (throws? (tpar/->float "xxx1.23"))
         (throws? (tpar/->float "1.23xxx"))
         (throws? (tpar/->float "1.2xx34")))
       ))


