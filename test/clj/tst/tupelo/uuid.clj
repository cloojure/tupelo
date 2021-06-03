(ns tst.tupelo.uuid
  (:use tupelo.core tupelo.test)
  (:require
    [tupelo.uuid :as uuid]
    ))

(dotest
  (is= "00000000-0000-0000-0000-000000000000"
       uuid/null-str
       (uuid/null))
  (is= "cafebabe-0867-5309-0666-0123456789ff"
       uuid/dummy-str
       (uuid/dummy))

  (is (uuid/uuid-str? uuid/null-str))
  (is (uuid/uuid-str? uuid/dummy-str))

  (isnt (uuid/uuid-str? "cafebabe-0867-5309-0666-0123456789fff"))
  (isnt (uuid/uuid-str? "cafebabe-0867-5309-06660-123456789ff"))
  (isnt (uuid/uuid-str? "cafebabe-0867-5309-066x-0123456789ff"))
  (isnt (uuid/uuid-str? "cafebabe-0867-5309-0123456789ff"))
  (isnt (uuid/uuid-str? "cafebabe-0867-5309|0666-0123456789ff"))

  ; we return uuids as a string
  (is (string? (uuid/rand)))
  (dotimes [i 99] ; 2 uuids are never equal
    (isnt (= (uuid/rand) (uuid/rand))))

  ; demonstrate uuid/with-null usage for testing
  (uuid/with-null
    (is= (uuid/rand) "00000000-0000-0000-0000-000000000000")
    (is= (uuid/rand) "00000000-0000-0000-0000-000000000000")
    (is= (uuid/rand) "00000000-0000-0000-0000-000000000000"))

  ; demonstrate uuid/with-counted for testing
  (uuid/with-counted
    (is= (uuid/rand) "abcd0000-aaaa-bbbb-cccc-0123456789ff")
    (is= (uuid/rand) "abcd0001-aaaa-bbbb-cccc-0123456789ff")
    (is= (uuid/rand) "abcd0002-aaaa-bbbb-cccc-0123456789ff"))

  ; demonstrate uuid/counted (manual)
  (uuid/counted-reset!)
  (is= (uuid/counted) "abcd0000-aaaa-bbbb-cccc-0123456789ff")
  (is= (uuid/counted) "abcd0001-aaaa-bbbb-cccc-0123456789ff")
  (is= (uuid/counted) "abcd0002-aaaa-bbbb-cccc-0123456789ff")
  (uuid/counted-reset!)
  (is= (uuid/counted) "abcd0000-aaaa-bbbb-cccc-0123456789ff")
  (is= (uuid/counted) "abcd0001-aaaa-bbbb-cccc-0123456789ff")
)
