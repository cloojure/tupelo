(ns tst.tupelo.java-time
  (:refer-clojure :exclude [range])
  (:use tupelo.java-time tupelo.test)
  (:require
    [clj-time.core :as joda]
    [tupelo.chars :as chars]
    [tupelo.core :as t :refer [spyx spyx-pretty]]
    [tupelo.interval :as interval]
    [tupelo.java-time :as tjt]
    [tupelo.string :as str]
    [tupelo.java-time.convert :as convert])
  (:import
    [java.sql Timestamp]
    [java.util Date UUID]
    [java.time  Clock Instant Duration Instant MonthDay YearMonth LocalDate LocalDateTime Period
               ZoneId ZoneId ZonedDateTime DayOfWeek]
    [java.util Date]
    [java.time.temporal ChronoUnit ChronoField TemporalField]))

(t/when-java-1-11-plus

  (verify
    (throws-not? (Instant/parse "2019-02-14T02:03:04.334Z"))
    (throws-not? (Instant/parse "2019-02-14T02:03:04Z"))
    (throws-not? (Instant/parse "0019-02-14T02:03:04Z")) ; can handle really old dates w/o throwing

    ; is very strict on format & values
    (throws? (Instant/parse "2019-02-14T02:03:04"))
    (throws? (Instant/parse "2019-02-14T02:03:4Z"))
    (throws? (Instant/parse "2019-02-14T02:3:04Z"))
    (throws? (Instant/parse "2019-02-14T2:03:04Z"))
    (throws? (Instant/parse "2019-02-14 02:03:04Z"))
    (throws? (Instant/parse "2019-02-4T02:03:04Z"))
    (throws? (Instant/parse "2019-2-14T02:03:04Z"))
    (throws? (Instant/parse "2019-02-31T02:03:04Z")))

  (verify
    (let [ld    (LocalDate/parse (format "2013-12-19"))
          ldstr "2013-12-19"]
      (is (LocalDate? ld))
      (isnt (LocalDate? ldstr))

      (is (LocalDate-str? ldstr))))

  (comment          ; #todo update/fix
    (verify
      (let [month->quarter [:Q1 :Q1 :Q1 :Q2 :Q2 :Q2 :Q3 :Q3 :Q3 :Q4 :Q4 :Q4]
            quarters-set   (set month->quarter)]
        (is-set= quarters-set [:Q1 :Q2 :Q3 :Q4])
        (is= (vec year-quarters) [:Q1 :Q2 :Q3 :Q4])
        (is= 12 (count month->quarter))
        (is (every? year-quarter? month->quarter))
        (is= month->quarter
          (t/forv [month-num (t/thru 1 12)] (->year-quarter (YearMonth/of 2013 month-num))))
        (is= month->quarter
          (t/forv [month-num (t/thru 1 12)] (->year-quarter (MonthDay/of month-num 13)))) ; 13'th of each month
        (is= month->quarter
          (t/forv [month-num (t/thru 1 12)] (->year-quarter (LocalDate/parse (format "2013-%02d-19" month-num)))))
        (is= month->quarter
          (t/forv [month-num (t/thru 1 12)] (->year-quarter (LocalDateTime/parse (format "2013-%02d-19T12:13:14" month-num)))))
        (is= month->quarter
          (t/forv [month-num (t/thru 1 12)] (->year-quarter (ZonedDateTime/parse (format "2013-%02d-19T12:13:14Z" month-num))))))

      (let [dnum-q1         (LocalDateStr->eday "2013-03-31")
            dnum-q2         (LocalDateStr->eday "2013-04-01")
            dnum-q1-quarter (eday->year-quarter dnum-q1)
            dnum-q2-quarter (eday->year-quarter dnum-q2)]
        (isnt= dnum-q1 dnum-q2)
        (is= (inc dnum-q1) dnum-q2)
        (is= :Q1 dnum-q1-quarter)
        (is= :Q2 dnum-q2-quarter)
        (isnt= dnum-q1-quarter dnum-q2-quarter))))

  (comment          ; #todo kill this?
    (verify
      (let [ld (LocalDate/parse "1995-01-04")]
        (is= {:LocalDate "1995-01-04"} (LocalDate->tagval ld))
        (is= (walk-LocalDate->tagval (LocalDate->trailing-interval ld 5))
          #tupelo.interval.Interval{:type  :generic
                                    :lower {:LocalDate "1994-12-30"},
                                    :upper {:LocalDate "1995-01-04"}}))))

  (verify
    (let [localdates-30 (t/forv [day (t/thru 1 30)]
                          (LocalDate/parse (format "2019-12-%02d" day)))]
      ; LocalDate-str (ISO text string) sorts correctly
      (let [dates-1 (shuffle localdates-30)
            dates-2 (sort dates-1)]
        (is= localdates-30 dates-2))

      ; LocalDate-str values work with compare
      (let [d1 (nth localdates-30 0)
            d2 (nth localdates-30 1)
            d3 (nth localdates-30 2)]
        (is (t/compare-increasing d1 d2 d3))
        (isnt (t/compare-increasing d2 d2 d3))
        (is (t/compare-increasing-or-equal d1 d2 d3))
        (is (t/compare-increasing-or-equal d2 d2 d3))
        (isnt (t/compare-increasing-or-equal d3 d2 d3))

        (is= 1 (LocalDate-interval->days (interval/new d1 d2)))
        (is= [0 1 2 3 4]
          (localdates->day-idxs (take 5 localdates-30))
          (localdates->day-idxs (take 5 (drop 9 localdates-30)))))

      (let [ld-2019-12-05  (LocalDate/parse "2019-12-05")
            ld-2019-12-09  (LocalDate/parse "2019-12-09")

            ld-itvl-open   (interval/new-open ld-2019-12-05 ld-2019-12-09)
            ld-itvl-slice  (interval/new-slice ld-2019-12-05 ld-2019-12-09)
            ld-itvl-closed (interval/new-closed ld-2019-12-05 ld-2019-12-09)

            open-5-9       (t/keep-if #(interval/contains-value? ld-itvl-open %) localdates-30)
            slice-5-9      (t/keep-if #(interval/contains-value? ld-itvl-slice %) localdates-30)
            thru-5-9       (t/keep-if #(interval/contains-value? ld-itvl-closed %) localdates-30)]

        (is= (mapv str open-5-9) ["2019-12-06" "2019-12-07" "2019-12-08"])
        (is= (mapv str slice-5-9) ["2019-12-05" "2019-12-06" "2019-12-07" "2019-12-08"])
        (is= (mapv str thru-5-9) ["2019-12-05" "2019-12-06" "2019-12-07" "2019-12-08" "2019-12-09"]))))

  (verify
    (is (Temporal? (ZonedDateTime/parse "2018-09-08T13:03:04.500Z")))
    (is (Temporal? (ZonedDateTime/parse "2018-09-08T13:03:04Z")))
    (is (Temporal? (ZonedDateTime/parse "2018-09-08T00:00Z")))

    (is (fixed-point? (zoned-date-time 2018 9 1)))
    (is (fixed-point? (->Instant (zoned-date-time 2018 9 1))))
    (is (fixed-point? (joda/date-time 2018 9 1)))

    (is (Period? (Period/ofDays 3)))
    (is (Period? (Period/ofWeeks 3)))
    (is (Period? (Period/ofMonths 3)))
    (is (Period? (Period/ofYears 3))))

  (verify
    (isnt (LocalDate-str? "12-31-1999"))
    (isnt (LocalDate-str? "12-31-99"))
    (is (LocalDate-str? "1999-12-31"))


    (doseq [ld-str ["1970-01-01"
                    "1970-01-02"
                    "1970-02-01"
                    "1971-01-01"
                    "1999-12-31"]]
      (let [ld (LocalDate/parse ld-str)
            tv (LocalDate->tagval ld)]
        (is= ld (-> ld (LocalDate->tagval) (tagval->LocalDate)))
        (is= tv (-> tv (tagval->LocalDate) (LocalDate->tagval))))))

  (verify
    (let [zone-ids         (vec (sort (ZoneId/getAvailableZoneIds))) ; all ZoneId String values
          zone-ids-america (vec (t/keep-if #(str/starts-with? % "America/") zone-ids))
          zone-ids-europe  (vec (t/keep-if #(str/starts-with? % "Europe/") zone-ids))
          zone-ids-us      (vec (t/keep-if #(str/starts-with? % "US/") zone-ids))]
      (is (< 590 (count zone-ids)))
      (is (< 160 (count zone-ids-america)))
      (is (< 60 (count zone-ids-europe)))
      (is (< 10 (count zone-ids-us))))

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

      (is (same-instant? (truncated-to ref ChronoUnit/YEARS) (zoned-date-time 2018)))
      (is (same-instant? (truncated-to ref ChronoUnit/MONTHS) (zoned-date-time 2018 2)))
      (is (same-instant? (truncated-to ref ChronoUnit/DAYS) (zoned-date-time 2018 2 3)))
      (is (same-instant? (truncated-to ref ChronoUnit/HOURS) (zoned-date-time 2018 2 3,, 4)))
      (is (same-instant? (truncated-to ref ChronoUnit/MINUTES) (zoned-date-time 2018 2 3,, 4 5)))
      (is (same-instant? (truncated-to ref ChronoUnit/SECONDS) (zoned-date-time 2018 2 3,, 4 5 6)))
      (is (same-instant? (truncated-to ref ChronoUnit/MILLIS) (ZonedDateTime/parse "2018-02-03t04:05:06.123z")))
      (is (same-instant? (truncated-to ref ChronoUnit/NANOS) (ZonedDateTime/parse "2018-02-03t04:05:06.123456789z")))

      (is (same-instant? ref (zoned-date-time 2018 2 3,, 4 5 6,, 123456789)))
      (is (same-instant? ref (zoned-date-time 2018 2 3,, 4 5 6,, 123456789 zoneid-utc)))

      (is (same-instant? ref (with-zoneid zoneid-utc
                               (zoned-date-time 2018 2 3,, 4 5 6 123456789))))
      (is (same-instant? (zoned-date-time 2018 2 3,, 12 5 6,, 123456789)
            (with-zoneid zoneid-us-eastern (zoned-date-time 2018 2 3,, 7 5 6,, 123456789))
            (with-zoneid zoneid-us-central (zoned-date-time 2018 2 3,, 6 5 6,, 123456789))
            (with-zoneid zoneid-us-mountain (zoned-date-time 2018 2 3,, 5 5 6,, 123456789))
            (with-zoneid zoneid-us-pacific (zoned-date-time 2018 2 3,, 4 5 6,, 123456789)))))

    (is (same-instant? (zoned-date-time 2018 8 26) ; a Sunday
          (previous-or-same (zoned-date-time 2018 9 1) DayOfWeek/SUNDAY)))
    (is (same-instant? (zoned-date-time 2018 9 2) ; a Sunday
          (previous-or-same (zoned-date-time 2018 9 2) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 3) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 4) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 5) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 6) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 7) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 8) DayOfWeek/SUNDAY)))

    (is (same-instant? (zoned-date-time 2018 9 9) ; a Sunday
          (previous-or-same (zoned-date-time 2018 9 9) DayOfWeek/SUNDAY)
          (previous-or-same (zoned-date-time 2018 9 10) DayOfWeek/SUNDAY))))

  (verify
    (is= 1 (between->units ChronoUnit/HOURS
             (->Instant "1987-11-22t01:30:00z")
             (->Instant "1987-11-22t03:29:00z")))

    (is= 111111111 (between->units ChronoUnit/NANOS
                     (->Instant "1987-11-22t11:22:33.444444444z")
                     (->Instant "1987-11-22t11:22:33.555555555z")))
    (is= 444 (between->units ChronoUnit/MILLIS
               (->Instant "1987-11-22t11:22:33z")
               (->Instant "1987-11-22t11:22:33.444444z")))
    (is= 1 (between->units ChronoUnit/SECONDS
             (->Instant "1987-11-22t11:22:33z")
             (->Instant "1987-11-22t11:22:34.4z")))
    (is= 11 (between->units ChronoUnit/MINUTES
              (->Instant "1987-11-22t11:22:33z")
              (->Instant "1987-11-22t11:33:44z")))
    (is= 10 (between->units ChronoUnit/HOURS
              (->Instant "1987-11-22t01:02:03z")
              (->Instant "1987-11-22t11:22:00z")))
    (is= 1 (between->units ChronoUnit/DAYS
             (->Instant "1987-11-22t01:02:03z")
             (->Instant "1987-11-23t11:22:00z")))

    (is= 4 (between->units ChronoUnit/WEEKS
             (->LocalDate "1987-01-01")
             (->LocalDate "1987-01-31")))
    (is= 10 (between->units ChronoUnit/MONTHS
              (->LocalDate "1987-01-22")
              (->LocalDate "1987-11-23")))
    (is= 12 (between->units ChronoUnit/YEARS
              (->LocalDate "1987-01-22")
              (->LocalDate "1999-11-23"))))

  (verify
    (let [i1 (str->Instant "1987-01-22")
          i2 (str->Instant "1987-01-23")
          i3 (str->Instant "1987-01-24")]
      (is (tjt/increasing? i1 i2 i3))
      (isnt (tjt/increasing? i1 i1 i3))
      (isnt (tjt/increasing? i1 i3 i3))
      (isnt (tjt/increasing? i2 i1 i3))
      (isnt (tjt/increasing? i1 i3 i2))
      (isnt (tjt/increasing? i3 i2 i1))

      (is (tjt/increasing-or-equal? i1 i2 i3))
      (is (tjt/increasing-or-equal? i1 i1 i3))
      (is (tjt/increasing-or-equal? i1 i3 i3))
      (isnt (tjt/increasing-or-equal? i2 i1 i3))
      (isnt (tjt/increasing-or-equal? i1 i3 i2))
      (isnt (tjt/increasing-or-equal? i3 i2 i1))))

  (verify
    (let [zdt  (zoned-date-time 2018 9 8,, 2 3 4)
          inst (->Instant zdt)]
      (is= "2018-09-08"
        (->str-YYYY-MM-DD zdt)
        (->str-YYYY-MM-DD inst))
      (is= "2018-09-08T02:03:04Z"
        (->str-iso zdt)
        (->str-iso inst))
      (is= "2018-09-08 02:03:04Z"
        (->str-iso-nice zdt)
        (->str-iso-nice inst))
      (is= "2018-09-08T02:03:04Z" (str (->Instant "2018-09-08T02:03:04Z")))
      (is= (str (->ZonedDateTime "2018-09-08T02:03:04Z")) "2018-09-08T02:03:04Z[UTC]"))

    (let [inst (Instant/parse "2037-07-14t01:02:03.012345678Z")
          zdt  (->ZonedDateTime inst)]
      (is= "20370714"
        (temporal->YYYYMMDD inst)
        (temporal->YYYYMMDD zdt)
        (->str-YYYYMMDD zdt)
        (->str-YYYYMMDD inst))
      (is= "010203"
        (->str-HHMMSS inst)
        (->str-HHMMSS zdt))
      (is= "20370714-010203"
        (->str-YYYYMMDD-HHMMSS zdt)
        (->str-YYYYMMDD-HHMMSS inst))

      (is= "2037-07-14T01:02:03.012345678Z"
        (->str-iso zdt)
        (->str-iso inst))
      (is= "2037-07-14 01:02:03.012345678Z"
        (->str-iso-nice zdt)
        (->str-iso-nice inst))))

  (verify
    (is= [(zoned-date-time 2018 9 1)
          (zoned-date-time 2018 9 2)
          (zoned-date-time 2018 9 3)
          (zoned-date-time 2018 9 4)]
      (slice
        (zoned-date-time 2018 9 1)
        (zoned-date-time 2018 9 5)
        (Duration/ofDays 1)))

    (is= [(zoned-date-time 2018 9 1 2 3 4)
          (zoned-date-time 2018 9 2 2 3 4)
          (zoned-date-time 2018 9 3 2 3 4)
          (zoned-date-time 2018 9 4 2 3 4)]
      (slice
        (zoned-date-time 2018 9 1 2 3 4)
        (zoned-date-time 2018 9 5 2 3 4)
        (Duration/ofDays 1)))

    (is= [(zoned-date-time 2018 9 1 1)
          (zoned-date-time 2018 9 1 2)
          (zoned-date-time 2018 9 1 3)
          (zoned-date-time 2018 9 1 4)]
      (slice
        (zoned-date-time 2018 9 1 1)
        (zoned-date-time 2018 9 1 5)
        (Duration/ofHours 1))))

  ; (previous-or-same  (zoned-date-time 2018 9 9) DayOfWeek/SUNDAY)
  (verify
    (let [start-sunday   (previous-or-same (zoned-date-time 2018 9 1) DayOfWeek/SUNDAY)
          stop-inst      (zoned-date-time 2018 9 17)
          start-instants (slice start-sunday stop-inst (Duration/ofDays 7))]
      (is= start-instants
        [(zoned-date-time 2018 8 26)
         (zoned-date-time 2018 9 2)
         (zoned-date-time 2018 9 9)
         (zoned-date-time 2018 9 16)])))

  (verify
    (is= (zoned-date-time 2018 9 1) (->ZonedDateTime (joda/date-time 2018 9 1)))
    (is= (zoned-date-time 2018 9 1) (->ZonedDateTime (joda/date-time 2018 9 1,, 0 0 0)))
    (is= (zoned-date-time 2018 9 1,, 2 3 4) (->ZonedDateTime (joda/date-time 2018 9 1,, 2 3 4)))

    (is (same-instant?
          (zoned-date-time 2018 9 1)
          (joda/date-time 2018 9 1)
          (->Instant (zoned-date-time 2018 9 1))
          (->Instant (joda/date-time 2018 9 1))))
    (is (same-instant?
          (zoned-date-time 2018 9 1)
          (joda/date-time 2018 9 1)
          (->ZonedDateTime (zoned-date-time 2018 9 1))
          (->ZonedDateTime (joda/date-time 2018 9 1))))
    (is (same-instant?
          (zoned-date-time 2018 9 1)
          (joda/date-time 2018 9 1)
          (->Instant (->ZonedDateTime (zoned-date-time 2018 9 1)))
          (->Instant (->ZonedDateTime (joda/date-time 2018 9 1))))))

  (verify
    (let [out-low     (zoned-date-time 2018 8 30)
          lb          (zoned-date-time 2018 9 1)
          mid         (zoned-date-time 2018 9 1, 1 2 3)
          ub          (zoned-date-time 2018 9 2)
          out-high    (zoned-date-time 2018 9 3)

          itvl-slice  (interval/new-slice lb ub) ; slice
          itvl-open   (interval/new-open lb ub) ; :open
          itvl-closed (interval/new-closed lb ub)] ; :closed

      (isnt (interval/contains-value? itvl-open out-low))
      (isnt (interval/contains-value? itvl-open lb))
      (is (interval/contains-value? itvl-open mid))
      (isnt (interval/contains-value? itvl-open ub))
      (isnt (interval/contains-value? itvl-open out-high))

      (isnt (interval/contains-value? itvl-slice out-low))
      (is (interval/contains-value? itvl-slice lb))
      (is (interval/contains-value? itvl-slice mid))
      (isnt (interval/contains-value? itvl-slice ub))
      (isnt (interval/contains-value? itvl-slice out-high))

      (isnt (interval/contains-value? itvl-closed out-low))
      (is (interval/contains-value? itvl-closed lb))
      (is (interval/contains-value? itvl-closed mid))
      (is (interval/contains-value? itvl-closed ub))
      (isnt (interval/contains-value? itvl-closed out-high))


      (comment
        (is (interval/contains-value? itvl lb))
        (is (interval/contains-value? itvl mid))
        (isnt (interval/contains-value? itvl ub))

        (isnt (interval/contains-value? itvl-open lb))
        (is (interval/contains-value? itvl-open mid))
        (isnt (interval/contains-value? itvl-open ub))

        (is (interval/contains-value? itvl-closed lb))
        (is (interval/contains-value? itvl-closed mid))
        (is (interval/contains-value? itvl-closed ub)))
      ))

  (verify
    (let [now-date-1             (now->Date)
          now-instant-1          (now->Instant)
          now-zdt-1a             (now->ZonedDateTime)
          now-zdt-1b             (now->ZonedDateTime)
          >>                     (Thread/sleep 100)
          now-instant-2          (now->Instant)
          now-date-2             (now->Date)

          instant-interval-date     (interval/new-closed (->Instant now-date-1) (->Instant now-date-2))
          instant-interval-short (interval/new-closed now-instant-1 now-instant-2)
          instant-interval-11    (interval/new-closed (.minusSeconds now-instant-1 1) now-instant-2)

          millis-1               (.toEpochMilli now-instant-1)
          instant-1c             (millis->Instant millis-1)]
      (is (interval/contains-value? instant-interval-date (->Instant now-zdt-1a)))
      (is (interval/contains-value? instant-interval-date (->Instant now-zdt-1b)))

      (is (interval/contains-value? instant-interval-short (->Instant now-zdt-1a)))
      (is (interval/contains-value? instant-interval-short (->Instant now-zdt-1b)))

      (is (interval/contains-value? instant-interval-11 instant-1c))))

  (verify
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
    (let [instant (.toInstant #inst "2016-03-24T12:13:12.000-00:00") ; java.util.Date
          zdt     (ZonedDateTime/parse "2016-03-24T12:13:12.000-00:00")] ; String
      (is (zero? (.toMillis (Duration/between instant zdt))))))

  (verify
    (let [iso-str     "2019-02-03T04:05:06.789Z"
          instant     (Instant/parse iso-str)
          millis      (.toEpochMilli instant)
          jud         (Date. millis)
          zdt         (ZonedDateTime/ofInstant instant, zoneid-utc)
          instant-str (.toString instant)
          zdt-str     (.toString instant)]
      (is= 1549166706789 millis)
      (is= instant (->Instant jud))
      (is= jud (convert/Instant->Date instant))

      (let [result (str/quotes->single (pr-str instant))]
        (is (str/contains-match? result #"#object\[java.time.Instant \p{Alnum}* '2019-02-03T04:05:06.789Z'\]"))
        (is (str/contains-str? result "#object[java.time.Instant"))
        (is (str/contains-str? result "2019-02-03T04:05:06.789Z")))
      (let [result (str/quotes->single (pr-str zdt))]
        (is (str/contains-str? result "#object[java.time.ZonedDateTime"))
        (is (str/contains-str? result "2019-02-03T04:05:06.789Z[UTC]")))
      (is= instant-str "2019-02-03T04:05:06.789Z")
      (is= zdt-str "2019-02-03T04:05:06.789Z")
      (is= (.toString jud) "Sat Feb 02 20:05:06 PST 2019")

      (is= "2019-02-03T04:05:06.789Z" (->str-iso zdt))
      (is= "2019-02-03T04:05:06.789Z" (->str-iso instant))

      (is= instant (parse-iso-str->Instant iso-str))
      (is= millis
        (parse-iso-str->millis iso-str)
        (parse-iso-str->millis instant-str)
        (parse-iso-str->millis zdt-str))

      (let [timestamp          (java.sql.Timestamp. millis)
            timestamp-from-str (parse-iso-str->sql-timestamp iso-str)
            timestamp-str      (.toString timestamp)
            timestamp-str-gmt  (.toGMTString timestamp)]
        (is= timestamp-str "2019-02-02 20:05:06.789") ; uses default TZ (US/Pacific in this example)
        (is= timestamp-str-gmt "3 Feb 2019 04:05:06 GMT") ; UGLY!
        (is= timestamp timestamp-from-str)
        )))

  (verify
    ; near-ISO string (includes "Z" at end)
    (let [str-sloppy "  2019-09-19   18:09:35Z  "
          str-nice   (str/whitespace-collapse str-sloppy)
          result     (parse-iso-str-nice->Instant str-sloppy)]
      (is (instance? Instant result))
      (is= (str result) "2019-09-19T18:09:35Z")
      (is= str-nice (->str-iso-nice result))

      ; also works if fractional seconds are present
      (is= "2019-09-19T18:09:35.123Z" (str (parse-iso-str-nice->Instant "  2019-09-19  18:09:35.123Z  "))))

    ; java.sql.Timestamp (no "Z" present at end)
    (is= "2019-09-19T18:09:35Z" (str (parse-sql-timestamp-str->Instant-utc "  2019-09-19  18:09:35  ")))
    (is= "2019-09-19T18:09:35.123Z" (str (parse-sql-timestamp-str->Instant-utc "  2019-09-19  18:09:35.123  "))))

  ;-----------------------------------------------------------------------------
  (verify
    (let [pat (t/grab :LocalDate time-str-patterns)]
      (is (re-matches pat "1999-12-31"))
      (isnt (re-matches pat "1999-2-31")))
    (let [pat (t/grab :Timestamp time-str-patterns)]
      (is (re-matches pat "1999-12-31 11:22:33"))
      (isnt (re-matches pat "1999-12-31 1:2:3")))
    (let [pat (t/grab :Instant time-str-patterns)]
      (is (re-matches pat "1999-12-31t11:22:33z"))
      (is (re-matches pat "1999-12-31t11:22:33Z"))
      (is (re-matches pat "1999-12-31t11:22:33.789Z"))
      (isnt (re-matches pat "1999-12-31 11:22:33Z"))
      (isnt (re-matches pat "1999-12-31t11:22:33x")))
    (let [pat (t/grab :ZonedDateTime time-str-patterns)]
      (is (re-matches pat "1999-12-31t11:22:33+00:00"))
      (is (re-matches pat "1999-12-31t11:22:33+00:00[UTC]"))))

  (verify
    (is (LocalDate-str? "1999-11-22"))
    (is (Timestamp-str? "1999-11-22 00:00:00"))
    (is (LocalDateTime-str? "1999-11-22t00:00:00"))
    (is (Instant-str? "1999-11-22t00:00:00z"))
    (is (Instant-str? "1999-11-22t00:00:00.000z"))
    (is (ZonedDateTime-str? "1999-11-22t00:00:00+00:00"))
    (is (ZonedDateTime-str? "1999-11-22t00:00:00.000+00:00"))
    (is (ZonedDateTime-str? "1999-11-22t00:00:00+00:00[UTC]"))

    (is= (str->Instant "1999-11-22") (Instant/parse "1999-11-22t00:00:00z"))
    (let [ref (-> "1999-11-22t00:00:00z" (Instant/parse))]
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00z")))
      (is (same-instant? ref (str->Instant "1999-11-22")))
      (is (same-instant? ref (str->Instant "1999-11-22 00:00:00")))
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00")))
      (is (same-instant? ref (str->Instant "1999-11-22 00:00:00z")))
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00z")))
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00.000z")))
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00+00:00")))
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00.000+00:00")))
      (is (same-instant? ref (str->Instant "1999-11-22t00:00:00+00:00[UTC]")))))

  (verify
    (is= (->field-strs (Instant/parse "2037-07-14t19:17:16.123456789Z"))
      {:day-2    "14"
       :hour-2   "19"
       :micros-6 "123456"
       :millis-3 "123"
       :min-2    "17"
       :month-2  "07"
       :nanos-9  "123456789"
       :sec-2    "16"
       :year-2   "37"
       :year-4   "2037"})
    (is= (->field-strs (Instant/parse "2037-07-14t01:02:03.012345678Z"))
      {:day-2    "14"
       :hour-2   "01"
       :micros-6 "012345"
       :millis-3 "012"
       :min-2    "02"
       :month-2  "07"
       :nanos-9  "012345678"
       :sec-2    "03"
       :year-2   "37"
       :year-4   "2037"})

    ;-----------------------------------------------------------------------------
    (is (every? chars/hex? (seq (random-hex-chars 20))))
    (is (every? chars/hex? (seq (random-hex-str 20))))

    (is (tuid-str? "2037-0714-191716-123456789-88d43adf-efc8b8ce"))
    (isnt (tuid-str? "X037-0714-191716-123456789-88d43adf-efc8b8ce"))
    (isnt (tuid-str? "20370-714-191716-123456789-88d43adf-efc8b8ce"))
    (is (every? tuid-str?
          (repeatedly 33 tuid-str)))

    ; sample output:  "2037-0714-191716-123456789-88d43adf-efc8b8ce"
    ; tens             00000000001111111111222222222233333333334444
    ; ones             01234567890123456789012345678901234567890123
    (is= 44 (count (tuid-str)))
    (let [sample-inst (Instant/parse "2037-07-14t19:17:16.123456789Z")
          clock       (Clock/fixed sample-inst tjt/zoneid-utc)]
      (with-redefs [instant-now (fn [] (Instant/now clock))]
        (let [result     (tuid-str)
              fixed-part (subs result 0 27)
              rnd1-str   (subs result 27 35)
              rnd2-str   (subs result 36)]
          (is= fixed-part "2037-0714-191716-123456789-")
          (is (every? chars/hex? rnd1-str))
          (is (every? chars/hex? rnd2-str))))))

  )
