(ns tst.tupelo.java-time
  (:use tupelo.java-time tupelo.core tupelo.test)
  (:import (java.time ZonedDateTime ZoneId)))

(dotest
  (is (temporal? (ZonedDateTime/parse "2018-09-08T13:03:04.500Z")))
  (is (temporal? (ZonedDateTime/parse "2018-09-08T13:03:04Z")))
  (is (temporal? (ZonedDateTime/parse "2018-09-08T00:00Z")))

  ;(is (period? (time/days 3)))
  ;(is (period? (time/weeks 3)))
  ;(is (period? (time/months 3)))
  ;(is (period? (time/years 3)))

  )


(dotest
  (spyx (into (sorted-set) (ZoneId/getAvailableZoneIds)))
  (is= (ZonedDateTime/parse "2018-08-26T00:00Z")
    (floor-sunday  (ZonedDateTime/parse "2018-09-01T00:00Z") ))
  (spyx (zoned-date-time 2018))
  (spyx (zoned-date-time 2018 2))
  (spyx (zoned-date-time 2018 2 3))
  (spyx (zoned-date-time 2018 2 3  4))
  (spyx (zoned-date-time 2018 2 3  4 5 6))
  (spyx (zoned-date-time 2018 2 3  4 5 6  777))

;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 2)))
;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 3)))
;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 4)))
;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 5)))
;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 6)))
;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 7)))
;  (is= (time/date-time 2018 9  2) (floor-sunday (time/date-time 2018 9 8)))
;  (is= (time/date-time 2018 9  9) (floor-sunday (time/date-time 2018 9 9)))
;  (is= (time/date-time 2018 9  9) (floor-sunday (time/date-time 2018 9 10)))
;  (is= (time/date-time 2018 9  9) (floor-sunday (time/date-time 2018 9 10 2 3 4)))
)


;(dotest
;  (is= [(time/date-time 2018 9 1)
;        (time/date-time 2018 9 2)
;        (time/date-time 2018 9 3)
;        (time/date-time 2018 9 4)]
;       (range
;         (time/date-time 2018 9 1)
;         (time/date-time 2018 9 5)
;         (time/days 1)))
;
;  (is= [(time/date-time 2018 9 1  2 3 4)
;        (time/date-time 2018 9 2  2 3 4)
;        (time/date-time 2018 9 3  2 3 4)
;        (time/date-time 2018 9 4  2 3 4)]
;       (range
;         (time/date-time 2018 9 1  2 3 4)
;         (time/date-time 2018 9 5  2 3 4)
;         (time/days 1)))
;
;  (is= [(time/date-time 2018 9 1  1)
;        (time/date-time 2018 9 1  2)
;        (time/date-time 2018 9 1  3)
;        (time/date-time 2018 9 1  4)]
;       (range
;         (time/date-time 2018 9 1  1 )
;         (time/date-time 2018 9 1  5 )
;         (time/hours 1)))
;  )

;(dotest
;  (let [ (spyx (floor-sunday (ZonedDateTime. 2018 9 1))))
;        stop-inst      (time/date-time 2018 9 17)
;        start-instants (range start-sunday stop-inst (time/weeks 1))]
;    (is= start-instants
;         [(time/date-time 2018 8 26)
;          (time/date-time 2018 9 2)
;          (time/date-time 2018 9 9)
;          (time/date-time 2018 9 16)])))
;
;(dotest
;  (let [inst (time/date-time 2018 9 8 2 3 4 500)]
;    (is= (->iso-date-str       inst) "2018-09-08" )
;    (is= (->iso-date-time-str  inst) "2018-09-08T02:03:04.500Z" )
;    (is= (->nice-date-time-str inst) "2018-09-08 02:03:04.500Z" ) ))
