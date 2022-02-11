(ns ^:test-refresh/focus
  tst.tupelo.java-time.convert
  (:use tupelo.java-time.convert tupelo.core tupelo.test)
  (:require
    [tupelo.string :as str]
    [schema.core :as s]
    [tupelo.java-time :as tjt])
  (:import
    [java.time Duration Instant MonthDay YearMonth LocalDate LocalDateTime Period
               ZoneId ZoneId ZonedDateTime DayOfWeek]
    [java.util Date]
    [java.time.temporal ChronoUnit]))

(dotest
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

  )

