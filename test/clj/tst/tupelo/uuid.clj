(ns tst.tupelo.uuid
  (:use tupelo.core tupelo.test)
  (:refer-clojure :exclude [rand])
  (:require
    [tupelo.uuid :as uuid]))

(dotest
  (is= "00000000-0000-0000-0000-000000000000"
    (uuid/null-str)
    (str (uuid/null)))
  (is= "cafebabe-1953-0510-0970-0123456789ff"
    (uuid/dummy-str)
    (str (uuid/dummy)))

  (is (uuid? (uuid/null)))
  (is (uuid? (uuid/dummy)))
  (is (uuid/uuid-str? (uuid/null-str)))
  (is (uuid/uuid-str? (uuid/dummy-str)))

  (isnt (uuid/uuid-str? "cafebabe-1953-0510-0970-0123456789fff"))
  (isnt (uuid/uuid-str? "cafebabe-1953-0510-09700-123456789ff"))
  (isnt (uuid/uuid-str? "cafebabe-1953-0510-066x-0123456789ff"))
  (isnt (uuid/uuid-str? "cafebabe-1953-0510-0123456789ff"))
  (isnt (uuid/uuid-str? "cafebabe-1953-0510|0970-0123456789ff"))
  (isnt (uuid/uuid-str? 5))
  (isnt (uuid/uuid-str? :nope))
  (isnt (uuid/uuid-str? nil))

  ; we return uuids as an object or a string
  (is= java.util.UUID (type (uuid/rand)))
  (is (string? (uuid/rand-str)))
  (is (with-exception-default false
        (dotimes [i 99] ; 2 uuids are never equal
          (assert (not= (uuid/rand) (uuid/rand)))
          (assert (not= (uuid/rand-str) (uuid/rand-str))))
        true)) ; if no failures, we pass the test

  ; demonstrate uuid/with-null usage for testing
  (uuid/with-null
    (is= (uuid/rand-str) "00000000-0000-0000-0000-000000000000")
    (is= (uuid/rand-str) "00000000-0000-0000-0000-000000000000")
    (is= (uuid/rand-str) "00000000-0000-0000-0000-000000000000"))

  ; demonstrate uuid/with-counted for testing
  (uuid/with-counted
    (is= (uuid/rand-str) "00000000-aaaa-bbbb-cccc-ddddeeeeffff")
    (is= (uuid/rand-str) "00000001-aaaa-bbbb-cccc-ddddeeeeffff")
    (is= (uuid/rand-str) "00000002-aaaa-bbbb-cccc-ddddeeeeffff")
    (let [r1 (uuid/rand)
          r2 (uuid/rand-str)]
      (is= (type r1) java.util.UUID)
      (is= (type r2) java.lang.String)
      (is= (str r1) "00000003-aaaa-bbbb-cccc-ddddeeeeffff")
      (is= r2 "00000004-aaaa-bbbb-cccc-ddddeeeeffff")
      (is (uuid/uuid-str? r2))))

  ; demonstrate uuid/counted (manual)
  (uuid/counted-reset!)
  (is= (uuid/counted-str) "00000000-aaaa-bbbb-cccc-ddddeeeeffff")
  (is= (uuid/counted-str) "00000001-aaaa-bbbb-cccc-ddddeeeeffff")
  (is= (uuid/counted-str) "00000002-aaaa-bbbb-cccc-ddddeeeeffff")
  (uuid/counted-reset!)
  (is= (uuid/counted-str) "00000000-aaaa-bbbb-cccc-ddddeeeeffff")
  (is= (uuid/counted-str) "00000001-aaaa-bbbb-cccc-ddddeeeeffff")
)
