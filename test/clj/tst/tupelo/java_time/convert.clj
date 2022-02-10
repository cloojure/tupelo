(ns ^:test-refresh/focus
  tst.tupelo.java-time.convert
  (:use tupelo.java-time.convert tupelo.core tupelo.test)
  (:require
    [tupelo.core :as t]
    [tupelo.string :as str]
    [schema.core :as s]
    [tupelo.misc :as misc]
    [tupelo.java-time :as tjt])
  (:import
    [java.time Duration Instant MonthDay YearMonth LocalDate LocalDateTime Period
               ZoneId ZoneId ZonedDateTime DayOfWeek]
    [java.util Date]
    [java.time.temporal ChronoUnit]))

(dotest
  (let [pat (grab :LocalDate patterns)]
    (is (re-matches  pat  "1999-12-31"))
    (isnt (re-matches pat "1999-2-31")))
  (let [pat (grab :Timestamp patterns)]
    (is (re-matches  pat "1999-12-31 11:22:33"))
    (isnt (re-matches  pat "1999-12-31 1:2:3")))
  (let [pat (grab :Instant patterns)]
    (is (re-matches  pat "1999-12-31t11:22:33z"))
    (is (re-matches  pat "1999-12-31t11:22:33Z"))
    (is (re-matches  pat "1999-12-31t11:22:33.789Z"))
    (isnt (re-matches  pat "1999-12-31 11:22:33Z"))
    (isnt (re-matches  pat "1999-12-31t11:22:33x")))
  (let [pat (grab :ZonedDateTime patterns)]
    (is (re-matches  pat "1999-12-31t11:22:33+00:00"))
    (is (re-matches  pat "1999-12-31t11:22:33+00:00[UTC]")))
  )

(dotest
  (is= (str->Instant "1999-11-22") (Instant/parse "1999-11-22t00:00:00z"))
  (is= (LocalDate->LocalDateTime-midnight (LocalDate/parse "1999-11-22")) (LocalDateTime/parse "1999-11-22t00:00:00"))

  ; note that equivalent ZonedDateTime values are not always equal
  (let [a  (ZonedDateTime/parse "1999-11-22t00:00:00z")
        b  (ZonedDateTime/parse "1999-11-22t00:00:00+00:00")
        c  (ZonedDateTime/parse "1999-11-22t00:00:00+00:00[UTC]")
        ib (Instant/from b)
        ic (Instant/from c)]
    (is= a b)
    (isnt= b c)     ; different zone spec => not equal
    (is= ib ic)     ; both convert to same instant, so equal here

    (is (tjt/same-instant? b c)) ; coerce to instant, then compare
    (is (tjt/same-instant? ib ic)) ; Instants are equal
    (is (tjt/same-instant? a b c ib ic))) ; can be mixed Temporal types

  (is (tjt/same-instant?
        (LocalDateTime->ZonedDateTime-utc (LocalDateTime/parse "1999-11-22t00:00:00"))
        (ZonedDateTime/parse "1999-11-22t00:00:00z")))
  (is (tjt/same-instant?
        (-> "1999-11-22t00:00:00z" (Instant/parse))
        (-> "1999-11-22t00:00:00" (LocalDateTime/parse) (LocalDateTime->ZonedDateTime-utc))
        (-> "1999-11-22t00:00:00+00:00" (ZonedDateTime/parse))
        (-> "1999-11-22T00:00:00+00:00[UTC]" (ZonedDateTime/parse))))

  (let [ref (-> "1999-11-22t00:00:00z" (Instant/parse))]
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00z")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22 00:00:00")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00z")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00.000z")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00+00:00")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00.000+00:00")))
    (is (tjt/same-instant? ref (str->Instant "1999-11-22t00:00:00+00:00[UTC]")))
    )

  )

