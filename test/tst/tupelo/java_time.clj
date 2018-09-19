(ns tst.tupelo.java-time
  (:refer-clojure :exclude [range])
  (:use tupelo.java-time tupelo.core tupelo.test)
  (:require [clojure.string :as str])
  (:import (java.time ZonedDateTime ZoneId Duration)
           [java.time.temporal ChronoUnit TemporalAdjuster TemporalAdjusters]))

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
  (let [zone-ids (vec (sort (ZoneId/getAvailableZoneIds))) ; all ZoneId String values
        zone-ids-america (vec (keep-if #(str/starts-with? % "America/" ) zone-ids))
        zone-ids-europe (vec (keep-if #(str/starts-with? % "Europe/" ) zone-ids))
        zone-ids-us (vec (keep-if #(str/starts-with? % "US/" ) zone-ids)) ]
    (is (< 590 (count zone-ids)))
    (is (< 160 (count zone-ids-america)))
    (is (<  60 (count zone-ids-europe)))
    (is (<  10 (count zone-ids-us))))

  (let [ref           (ZonedDateTime/of 2018 2 3 4 5 6 123456789 (ZoneId/of "UTC"))
        ref-from-str  (ZonedDateTime/parse "2018-02-03T04:05:06.123456789Z") ; zulu = utc
        inst          (.toInstant ref)
        inst-from-str (.toInstant ref-from-str)]
    ; time zone info is presereved in ZonedDateTime, so they are not "equal" objects
    (isnt (.equals ref ; #object[java.time.ZonedDateTime 0x4b881b83 "2018-02-03T04:05:06.123456789Z[UTC]"]
            ref-from-str)) ; #object[java.time.ZonedDateTime 0x6ada3244 "2018-02-03T04:05:06.123456789Z"]
    (isnt= ref ref-from-str)
    (isnt= 0 (.compareTo ref ref-from-str))

    ; they are identical instants
    (is= inst inst-from-str)
    (is (.isEqual ref ref-from-str)) ; converts to Instant, then compares
    (is (same-instant? ref ref-from-str)) ; more Clojurey way

    (is (same-instant? (zoned-date-time 2018)                 (trunc-to-year ref)))
    (is (same-instant? (zoned-date-time 2018 2)               (trunc-to-month ref)))
    (is (same-instant? (zoned-date-time 2018 2 3)             (trunc-to-day ref)))
    (is (same-instant? (zoned-date-time 2018 2 3 ,, 4)        (trunc-to-hour ref)))
    (is (same-instant? (zoned-date-time 2018 2 3 ,, 4 5)      (trunc-to-minute ref)))
    (is (same-instant? (zoned-date-time 2018 2 3 ,, 4 5 6)    (trunc-to-second ref)))
    (is (same-instant? (zoned-date-time 2018 2 3 ,, 4 5 6 ,, 123456789) ref))
    (is (same-instant? (zoned-date-time 2018 2 3 ,, 4 5 6 ,, 123456789 zoneid-utc) ref))

    (is (same-instant? ref (with-zoneid zoneid-utc
                             (zoned-date-time 2018 2 3 ,, 4 5 6 123456789))))
    (is (same-instant?                    (zoned-date-time 2018 2 3 ,, 12 5 6 ,, 123456789)
          (with-zoneid zoneid-us-eastern  (zoned-date-time 2018 2 3 ,,  7 5 6 ,, 123456789))
          (with-zoneid zoneid-us-central  (zoned-date-time 2018 2 3 ,,  6 5 6 ,, 123456789))
          (with-zoneid zoneid-us-mountain (zoned-date-time 2018 2 3 ,,  5 5 6 ,, 123456789))
          (with-zoneid zoneid-us-pacific  (zoned-date-time 2018 2 3 ,,  4 5 6 ,, 123456789)))))

  (is (same-instant? (zoned-date-time 2018 8 26)
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 1))))
  (is (same-instant? (zoned-date-time 2018 9 2)
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 2))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 3))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 4))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 5))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 6))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 7))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 8))))
  (is (same-instant? (zoned-date-time 2018 9 9)
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 9))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 10))
        (trunc-to-sunday-midnight (zoned-date-time 2018 9 10 2 3 4))))
  (let [zdt (zoned-date-time 2018 10 7)]
    (is (same-instant? (zoned-date-time 2018 10 1) (trunc-to-monday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 10 2) (trunc-to-tuesday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 10 3) (trunc-to-wednesday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 10 4) (trunc-to-thursday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 10 5) (trunc-to-friday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 10 6) (trunc-to-saturday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 10 7) (trunc-to-sunday-midnight zdt))))
  (let [zdt (zoned-date-time 2018 9 7)]
    (is (same-instant? (zoned-date-time 2018 9 1) (trunc-to-saturday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 9 2) (trunc-to-sunday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 9 3) (trunc-to-monday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 9 4) (trunc-to-tuesday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 9 5) (trunc-to-wednesday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 9 6) (trunc-to-thursday-midnight zdt)))
    (is (same-instant? (zoned-date-time 2018 9 7) (trunc-to-friday-midnight zdt))) ) )

(dotest
  (let [zdt (zoned-date-time 2018 9 8,, 2 3 4)]
    (is= (iso-date-str zdt)         "2018-09-08")
    (is= (iso-date-time-str zdt)    "2018-09-08T02:03:04Z")
    (is= (nice-date-time-str zdt)   "2018-09-08 02:03:04Z"))
  (let [zdt (zoned-date-time 2018 9 8,, 2 3 4,, 123456789)]
    (is= (iso-date-str zdt)         "2018-09-08")
    (is= (iso-date-time-str zdt)    "2018-09-08T02:03:04.123456789Z")
    (is= (nice-date-time-str zdt)   "2018-09-08 02:03:04.123456789Z")))


(dotest
  (is= [(zoned-date-time 2018 9 1)
        (zoned-date-time 2018 9 2)
        (zoned-date-time 2018 9 3)
        (zoned-date-time 2018 9 4)]
       (range
         (zoned-date-time 2018 9 1)
         (zoned-date-time 2018 9 5)
         (Duration/ofDays 1)))

  (is= [(zoned-date-time 2018 9 1  2 3 4)
        (zoned-date-time 2018 9 2  2 3 4)
        (zoned-date-time 2018 9 3  2 3 4)
        (zoned-date-time 2018 9 4  2 3 4)]
       (range
         (zoned-date-time 2018 9 1  2 3 4)
         (zoned-date-time 2018 9 5  2 3 4)
         (Duration/ofDays 1)))

  (is= [(zoned-date-time 2018 9 1  1)
        (zoned-date-time 2018 9 1  2)
        (zoned-date-time 2018 9 1  3)
        (zoned-date-time 2018 9 1  4)]
       (range
         (zoned-date-time 2018 9 1  1 )
         (zoned-date-time 2018 9 1  5 )
         (Duration/ofHours 1)))
  )

(dotest
  (let [start-sunday   (spyx (trunc-to-sunday-midnight (zoned-date-time 2018 9 1)))
        stop-inst      (zoned-date-time 2018 9 17)
        start-instants (range start-sunday stop-inst (Duration/ofDays 7))]
    (is= start-instants
      [(zoned-date-time 2018 8 26)
       (zoned-date-time 2018 9 2)
       (zoned-date-time 2018 9 9)
       (zoned-date-time 2018 9 16)])))

