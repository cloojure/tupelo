(ns tst.tupelo.java-time
  (:refer-clojure :exclude [range])
  (:use tupelo.java-time tupelo.core tupelo.test)
  (:require
    [clj-time.core :as joda]
    [clojure.string :as str]
    [tupelo.string :as ts]
    )
  (:import [java.time Duration ZoneId ZoneId ZonedDateTime ZonedDateTime LocalDateTime Instant Period]
           [java.util Date] ))

(dotest
  (is (temporal? (ZonedDateTime/parse "2018-09-08T13:03:04.500Z")))
  (is (temporal? (ZonedDateTime/parse "2018-09-08T13:03:04Z")))
  (is (temporal? (ZonedDateTime/parse "2018-09-08T00:00Z")))

  (is (fixed-time-point? (zoned-date-time 2018 9 1)))
  (is (fixed-time-point? (->instant (zoned-date-time 2018 9 1))))
  (is (fixed-time-point? (joda/date-time 2018 9 1)))

  (is (period? (Period/ofDays 3)))
  (is (period? (Period/ofWeeks 3)))
  (is (period? (Period/ofMonths 3)))
  (is (period? (Period/ofYears 3)))

  (is= {:zdt     "2018-09-01T00:00:00Z",
        :instant "2018-09-01T00:00:00Z",
        :joda-dt "2018-09-01T00:00:00Z"}
    (stringify-times
      {:zdt     (zoned-date-time 2018 9 1)
       :instant (->instant (zoned-date-time 2018 9 1))
       :joda-dt (->instant (joda/date-time 2018 9 1))})))

(dotest
  (let [zone-ids          (vec (sort (ZoneId/getAvailableZoneIds))) ; all ZoneId String values
        zone-ids-america  (vec (keep-if #(str/starts-with? % "America/" ) zone-ids))
        zone-ids-europe   (vec (keep-if #(str/starts-with? % "Europe/" ) zone-ids))
        zone-ids-us       (vec (keep-if #(str/starts-with? % "US/" ) zone-ids)) ]
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
    (is= (string-date-iso zdt)            "2018-09-08")
    (is= (string-date-time-iso zdt)       "2018-09-08T02:03:04Z")
    (is= (string-date-time-nice zdt)      "2018-09-08 02:03:04Z")
    (is= (iso-date-str zdt)            "2018-09-08") ; deprecated
    (is= (iso-date-time-str zdt) "2018-09-08T02:03:04Z")) ; deprecated
  (let [zdt (zoned-date-time 2018 9 8,, 2 3 4,, 123456789)]
    (is= (string-date-compact zdt)        "20180908" )
    (is= (string-date-time-nice zdt)      "2018-09-08 02:03:04.123456789Z")
    (is= (string-date-time-compact zdt)   "20180908-020304" )
    (is= (string-date-time-hyphens zdt)   "2018-09-08-02-03-04")
    (is= (iso-date-str zdt)            "2018-09-08") ; deprecated
    (is= (iso-date-time-str zdt)       "2018-09-08T02:03:04.123456789Z") ; deprecated
  ))

(dotest
  (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3,}Z" (now->iso-str)))  ; at least 3 decimal seconds
  (is (re-matches #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}Z"         (now->iso-str-simple))))

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
         (Duration/ofHours 1))))

(dotest
  (let [start-sunday   (trunc-to-sunday-midnight (zoned-date-time 2018 9 1))
        stop-inst      (zoned-date-time 2018 9 17)
        start-instants (range start-sunday stop-inst (Duration/ofDays 7))]
    (is= start-instants
      [(zoned-date-time 2018 8 26)
       (zoned-date-time 2018 9 2)
       (zoned-date-time 2018 9 9)
       (zoned-date-time 2018 9 16)])))

(dotest
  (is= (zoned-date-time 2018 9 1) (->zoned-date-time (joda/date-time 2018 9 1)))
  (is= (zoned-date-time 2018 9 1) (->zoned-date-time (joda/date-time 2018 9 1 ,, 0 0 0)))
  (is= (zoned-date-time 2018 9 1 ,, 2 3 4) (->zoned-date-time (joda/date-time 2018 9 1 ,, 2 3 4)))

  (is (same-instant?
        (zoned-date-time 2018 9 1)
        (joda/date-time 2018 9 1)
        (->instant (zoned-date-time 2018 9 1))
        (->instant (joda/date-time 2018 9 1)) ))
  (is (same-instant?
        (zoned-date-time 2018 9 1)
        (joda/date-time 2018 9 1)
        (->zoned-date-time (zoned-date-time 2018 9 1))
        (->zoned-date-time (joda/date-time 2018 9 1)) ))
  (is (same-instant?
        (zoned-date-time 2018 9 1)
        (joda/date-time 2018 9 1)
        (->instant (->zoned-date-time (zoned-date-time 2018 9 1)))
        (->instant (->zoned-date-time (joda/date-time 2018 9 1))))) )

(dotest
  (let [out-low  (zoned-date-time 2018 8 30)
        lb       (zoned-date-time 2018 9 1)
        mid      (zoned-date-time 2018 9 1, 1 2 3)
        ub       (zoned-date-time 2018 9 2)
        out-high (zoned-date-time 2018 9 3)

        itvl     (->Interval lb ub)
        itvl-o   (->Interval lb ub) ; :open
        itvl-ho  (->Interval lb ub) ; :half-open
        itvl-c   (->Interval lb ub)] ; :closed

    (isnt (interval-open-contains? itvl out-low))
    (isnt (interval-open-contains? itvl lb))
    (is (interval-open-contains? itvl mid))
    (isnt (interval-open-contains? itvl ub))
    (isnt (interval-open-contains? itvl out-high))

    (isnt (interval-slice-contains? itvl out-low))
    (is (interval-slice-contains? itvl lb))
    (is (interval-slice-contains? itvl mid))
    (isnt (interval-slice-contains? itvl ub))
    (isnt (interval-slice-contains? itvl out-high))

    (isnt (interval-closed-contains? itvl out-low))
    (is (interval-closed-contains? itvl lb))
    (is (interval-closed-contains? itvl mid))
    (is (interval-closed-contains? itvl ub))
    (isnt (interval-closed-contains? itvl out-high))


    (comment
      (is (interval-contains? itvl lb))
      (is (interval-contains? itvl mid))
      (isnt (interval-contains? itvl ub))

      (isnt (interval-contains? itvl-o lb))
      (is (interval-contains? itvl-o mid))
      (isnt (interval-contains? itvl-o ub))

      (is (interval-contains? itvl-c lb))
      (is (interval-contains? itvl-c mid))
      (is (interval-contains? itvl-c ub))
      )

    )

  (is= (stringify-times [:something
                         {:ambassador-id 13590,
                          :created-at    (with-zoneid zoneid-us-pacific
                                           (zoned-date-time 2018 1 2, 7))
                          :team-id       45,}
                         #{:some :more :stuff}])
    [:something
     {:ambassador-id 13590
      :created-at    "2018-01-02T15:00:00Z"
      :team-id       45}
     #{:stuff :some :more}])
  )

(dotest
  (let [now-instant-1          (instant)
        now-zdt-1a             (zoned-date-time)
        now-zdt-1b             (now->zdt)
        >>                     (Thread/sleep 100)
        now-instant-2          (now->instant)

        instant-interval-short (->Interval now-instant-1 now-instant-2)
        instant-interval-11    (->Interval (.minusSeconds now-instant-1 1) now-instant-2)

        millis-1               (.toEpochMilli now-instant-1)
        instant-1c             (millis->instant millis-1)
        instant-1-trunc        (secs->instant (quot millis-1 1000))]
    (is (interval-closed-contains? instant-interval-short (->instant now-zdt-1a)))
    (is (interval-closed-contains? instant-interval-short (->instant now-zdt-1b)))
    (is (interval-closed-contains? instant-interval-11 instant-1c))
    (is (interval-closed-contains? instant-interval-11 instant-1-trunc))))

(dotest
  ; note that instants are in DESCENDING order
  (let [instants         ["2016-04-30T10:29:17.000-00:00"
                          "2016-03-24T12:13:12.000-00:00"
                          "2016-03-24T12:09:43.000-00:00"
                          "2016-03-23T13:19:03.000-00:00"
                          "2016-02-26T14:51:37.000-00:00"
                          "2016-01-20T16:55:24.000-00:00"]
        zoned-date-times (mapv #(ZonedDateTime/parse %) instants)
        zdt-pairs        (partition 2 1 zoned-date-times)
        durations        (vec (for [[interval-stop interval-start] zdt-pairs]
                                (.toMinutes ; *** truncates ***
                                  (Duration/between interval-start interval-stop))))]
    (is= durations [53176 3 1370 37347 53156]))

  ; Can use Clojure #inst or string as input
  (let [instant (.toInstant            #inst "2016-03-24T12:13:12.000-00:00")  ; java.util.Date
        zdt     (ZonedDateTime/parse         "2016-03-24T12:13:12.000-00:00")] ; String
    (is (zero? (.toMillis (Duration/between instant zdt))))))

(dotest
  (let [iso-str     "2019-02-03T04:05:06.789Z"
        instant     (Instant/parse iso-str)
        millis      (.toEpochMilli instant)
        jud         (Date. millis)
        zdt         (ZonedDateTime/ofInstant instant, zoneid-utc)
        instant-str (.toString instant)
        zdt-str     (.toString instant)]
    (is= 1549166706789 millis)
    (let [result (ts/quotes->single (pr-str instant))]
      (is (ts/contains-match? result #"#object\[java.time.Instant \p{Alnum}* '2019-02-03T04:05:06.789Z'\]"))
      (is (ts/contains-str? result "#object[java.time.Instant"))
      (is (ts/contains-str? result "2019-02-03T04:05:06.789Z")))
    (let [result (ts/quotes->single (pr-str zdt))]
      (is (ts/contains-str? result "#object[java.time.ZonedDateTime"))
      (is (ts/contains-str? result "2019-02-03T04:05:06.789Z[UTC]")))
    (is= instant-str "2019-02-03T04:05:06.789Z")
    (is= zdt-str "2019-02-03T04:05:06.789Z")
    (is= (.toString jud) "Sat Feb 02 20:05:06 PST 2019")

    (is= "2019-02-03T04:05:06.789Z" (string-date-time-iso zdt))
    (is= "2019-02-03T04:05:06.789Z" (string-date-time-iso instant))

    (is= millis
      (iso-str->millis iso-str)
      (iso-str->millis instant-str)
      (iso-str->millis zdt-str))

    (let [timestamp          (java.sql.Timestamp. millis)
          timestamp-from-str (iso-str->timestamp iso-str)
          timestamp-str      (.toString timestamp)
          timestamp-str-gmt  (.toGMTString timestamp)]
      (is= timestamp-str "2019-02-02 20:05:06.789") ; uses default TZ (US/Pacific in this example)
      (is= timestamp-str-gmt "3 Feb 2019 04:05:06 GMT") ; UGLY!
      (is= timestamp timestamp-from-str)
      (is= (walk-timestamp->instant [1 {:j-s-ts timestamp} 2 3]) [1 {:j-s-ts instant} 2 3])
      (is= (walk-instant->str [1 {:j-t-inst instant} 2 3]) [1 {:j-t-inst instant-str} 2 3])
      (is= (walk-instant->timestamp [1 {:j-s-ts instant} 2 3]) [1 {:j-s-ts timestamp} 2 3]))
    ))

(dotest
  (let [str-nice "2019-09-19 18:09:35Z"
        result   (parse-iso-str-nice str-nice)]
    (is (instance? Instant result))
    (is= str-nice (string-date-time-nice result))))













