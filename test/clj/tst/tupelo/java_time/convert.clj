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
    (is (str/contains-match? "1999-12-31" pat))
    (isnt (str/contains-match? "1999-2-31" pat)))
  (let [pat (grab :Timestamp patterns)]
    (is (str/contains-match? "1999-12-31 11:22:33" pat))
    (isnt (str/contains-match? "1999-12-31 1:2:3" pat)))
  (let [pat (grab :Instant patterns)]
    (is (str/contains-match? "1999-12-31t11:22:33z" pat))
    (is (str/contains-match? "1999-12-31t11:22:33Z" pat))
    (isnt (str/contains-match? "1999-12-31 11:22:33Z" pat))
    (isnt (str/contains-match? "1999-12-31t11:22:33x" pat)))
  (let [pat (grab :ZonedDateTime patterns)]
    (is (str/contains-match? "1999-12-31t11:22:33[abc]" pat))
    (isnt (str/contains-match? "1999-12-31t11:22:33[]" pat))))

(dotest
  (is= (str->Instant "1999-11-22") (Instant/parse "1999-11-22t00:00:00z"))
  (is= (LocalDate->LocalDateTime (LocalDate/parse "1999-11-22")) (LocalDateTime/parse "1999-11-22t00:00:00"))
  (let [a  (ZonedDateTime/parse "1999-11-22t00:00:00z")
        b  (ZonedDateTime/parse "1999-11-22t00:00:00+00:00")
        c  (ZonedDateTime/parse "1999-11-22t00:00:00+00:00[UTC]")
        ib (Instant/from b)
        ic (Instant/from c)]
    (is= a b)
    (isnt= b c)     ; different zone spec => not equal
    (is (tjt/same-instant? b c)) ; coerce to instant, then compare
    (is= ib ic)     ; both convert to same instant, so equal here
    )

  )

