(ns tst.tupelo.uuid
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.core]
             [tupelo.misc]
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.core :as t]
    [tupelo.uuid :as uuid]
    [tupelo.test :refer [testing is verify verify-focus
                         is isnt is= isnt= is-set= is-nonblank= is-nonblank-lines=
                         throws? throws-not?
                         ]]
    ))

; #todo add tests for cljs

(verify
  (is= "00000000-0000-0000-0000-000000000000"
    (uuid/null-str))
  (is= "cafebabe-1953-0510-0970-0123456789ff"
    (uuid/dummy-str))

  (is (uuid/uuid-str? (uuid/null-str)))
  (is (uuid/uuid-str? (uuid/dummy-str)))

  (isnt (uuid/uuid-str? "cafebabe-1953-0510-0970-0123456789fff")) ; too long
  (isnt (uuid/uuid-str? "cafebabe-1953-0510-09700-123456789ff")) ; wrong shape
  (isnt (uuid/uuid-str? "cafebabe-1953-0510-066x-0123456789ff")) ; illegal char
  (isnt (uuid/uuid-str? "cafebabe-1953-0510-0123456789ff")) ; wrong shape
  (isnt (uuid/uuid-str? "cafebabe-1953-0510|0970-0123456789ff")) ; illegal char
  (isnt (uuid/uuid-str? 5)) ; wrong type
  (isnt (uuid/uuid-str? :nope)) ; wrong type
  (isnt (uuid/uuid-str? nil)) ; wrong type

  )


; we return uuids as an object or a string
#?(:clj
   (do
     (verify
       (is= "00000000-0000-0000-0000-000000000000"
         (str (uuid/null)))
       (is= "cafebabe-1953-0510-0970-0123456789ff"
         (str (uuid/dummy)))

       (is (uuid? (uuid/null)))
       (is (uuid? (uuid/dummy)))

       (is= java.util.UUID (type (uuid/rand)))
       (is (string? (uuid/rand-str)))
       (is (t/with-exception-default false
             (dotimes [i 99] ; 2 uuids are never equal
               (assert (not= (uuid/rand) (uuid/rand)))
               (assert (not= (uuid/rand-str) (uuid/rand-str))))
             true)) ; if no failures, we pass the test

       ; demonstrate uuid/with-null usage for testing
       (uuid/with-null ; force to always return null UUID for testing
         (is= (uuid/rand-str) "00000000-0000-0000-0000-000000000000")
         (is= (uuid/rand-str) "00000000-0000-0000-0000-000000000000")
         (is= (uuid/rand-str) "00000000-0000-0000-0000-000000000000"))

       ; demonstrate uuid/with-counted for testing
       (uuid/with-counted ; force to always return counted UUID for testing
         (is= (uuid/rand-str) "00000000-aaaa-bbbb-cccc-ddddeeeeffff")
         (is= (uuid/rand-str) "00000001-aaaa-bbbb-cccc-ddddeeeeffff")
         (is= (uuid/rand-str) "00000002-aaaa-bbbb-cccc-ddddeeeeffff")
         #?(:clj
            (let [r1 (uuid/rand)
                  r2 (uuid/rand-str)]
              (is= (type r1) java.util.UUID)
              (is= (type r2) java.lang.String)
              (is= (str r1) "00000003-aaaa-bbbb-cccc-ddddeeeeffff")
              (is= r2 "00000004-aaaa-bbbb-cccc-ddddeeeeffff")
              (is (uuid/uuid-str? r2)))))

       ; demonstrate uuid/counted-str (manual reset)
       (uuid/counted-reset!)
       (is= (uuid/counted-str) "00000000-aaaa-bbbb-cccc-ddddeeeeffff")
       (is= (uuid/counted-str) "00000001-aaaa-bbbb-cccc-ddddeeeeffff")
       (is= (uuid/counted-str) "00000002-aaaa-bbbb-cccc-ddddeeeeffff")
       (is= (str (uuid/counted)) "00000003-aaaa-bbbb-cccc-ddddeeeeffff")
       (uuid/counted-reset!)
       (is= (uuid/counted-str) "00000000-aaaa-bbbb-cccc-ddddeeeeffff")
       (is= (uuid/counted-str) "00000001-aaaa-bbbb-cccc-ddddeeeeffff"))

     ))
