(ns ^:test-refresh/focus
  tst.tupelo.java-time.convert
  (:use tupelo.java-time.convert tupelo.core tupelo.test)
  (:require
    [clj-time.coerce :as joda.coerce]
    [clj-time.core :as joda]
    [tupelo.java-time :as tjt])
  (:import
    [java.time Instant LocalDate LocalDateTime Year
               ZonedDateTime ]
    [java.sql Timestamp]
    [java.util Date]
    ))

(when-java-1-11-plus

  (verify
    ; NOTE:
    ;  (type jdt) => org.joda.time.DateTime
    ;  (supers (type jdt))
    ;      => #{java.io.Serializable
    ;           java.lang.Comparable
    ;           java.lang.Object
    ;           org.joda.time.ReadableDateTime
    ;           org.joda.time.ReadableInstant
    ;           org.joda.time.base.AbstractDateTime
    ;           org.joda.time.base.AbstractInstant
    ;           org.joda.time.base.BaseDateTime}
    (let [jdt        (joda/date-time 2018 9 21 11 22 33 789)
          inst       (Instant/parse "2018-09-21T11:22:33.789z")
          sql-ts  (Instant->sql-Timestamp inst)
          jdt-milli  (joda.coerce/to-long jdt)
          inst-milli (.toEpochMilli inst)]
      (is= jdt-milli inst-milli)
      (is= inst (-> inst Instant->joda joda->Instant))
      (is= jdt (-> jdt joda->Instant Instant->joda))

      (is= sql-ts (-> sql-ts sql-Timestamp->Instant Instant->sql-Timestamp))))

  (verify
    (is= (LocalDate->LocalDateTime (LocalDate/parse "1999-11-22")) (LocalDateTime/parse "1999-11-22t00:00:00"))

    ; note that equivalent ZonedDateTime values are not always equal
    (let [a  (ZonedDateTime/parse "1999-11-22t00:00:00z")
          b  (ZonedDateTime/parse "1999-11-22t00:00:00+00:00")
          c  (ZonedDateTime/parse "1999-11-22t00:00:00+00:00[UTC]")
          ib (Instant/from b)
          ic (Instant/from c)]
      (is= a b)
      (isnt= b c)   ; different zone spec => not equal
      (is= ib ic)   ; both convert to same instant, so equal here

      (is (tjt/same-instant? b c)) ; coerce to instant, then compare
      (is (tjt/same-instant? ib ic)) ; Instants are equal
      (is (tjt/same-instant? a b c ib ic))) ; can be mixed Temporal types

    (is (tjt/same-instant?
          (LocalDateTime->ZonedDateTime (LocalDateTime/parse "1999-11-22t00:00:00"))
          (ZonedDateTime/parse "1999-11-22t00:00:00z")))
    (is (tjt/same-instant?
          (-> "1999-11-22t00:00:00z" (Instant/parse))
          (-> "1999-11-22t00:00:00" (LocalDateTime/parse) (LocalDateTime->ZonedDateTime))
          (-> "1999-11-22t00:00:00+00:00" (ZonedDateTime/parse))
          (-> "1999-11-22T00:00:00+00:00[UTC]" (ZonedDateTime/parse))))

    )

  (verify
    (let [date (LocalDate->Date (LocalDate/parse "1999-12-31"))]
      (is (instance? java.util.Date date))
      (is= "1999-12-31T00:00:00Z" (str (.toInstant date))))
    (let [ld-str   "1999-12-31"
          inst     (LocalDate->Instant (LocalDate/parse ld-str))
          inst-str "1999-12-31T00:00:00Z"]
      (is (instance? java.time.Instant inst))
      (is= inst-str (str inst))
      (is= ld-str (str (Instant->LocalDate inst)))
      )
    )

  (verify
    (let [inst-str      "1999-12-31T01:02:03.456Z" ; "Thu Dec 30 17:02:03 PST 1999"
          instant       (Instant/parse inst-str)

          millis        (.toEpochMilli instant)
          date          (Date. millis) ; NOTE: toString() truncates millis
          sql-timestamp (Timestamp. millis)
          sql-date      (java.sql.Date/valueOf "1999-12-30")
          zdt           (ZonedDateTime/parse "1999-11-22t11:33:44.555-08:00")
          ldt           (Instant->LocalDateTime instant)
          ld            (Instant->LocalDate instant)
          ym            (Instant->YearMonth instant)
          year          (Year/parse "1999")
          ]
      (is= (str instant) "1999-12-31T01:02:03.456Z")
      (is= instant (-> instant (str) (Instant/parse)))

      (is= (str zdt) "1999-11-22T11:33:44.555-08:00")
      (is= zdt (-> zdt (str) (ZonedDateTime/parse)))

      (is= instant (-> instant Instant->Date Date->Instant))
      (is= instant (-> instant Instant->ZonedDateTime ZonedDateTime->Instant))

      (is= (str ldt) "1999-12-31T01:02:03.456")
      (is= instant (-> instant Instant->LocalDateTime LocalDateTime->Instant))

      (is= (str ld) "1999-12-31")
      (is= ld (-> ld LocalDate->Instant Instant->LocalDate))

      (is= (Date->str date) "1999-12-31T01:02:03.456Z")
      (is= date (-> date Date->str str->Date))

      (is= (sql-Date->str sql-date) "1999-12-30")
      (is= sql-date (-> sql-date sql-Date->str str->sql-Date))

      (is= (sql-Timestamp->str sql-timestamp) "1999-12-30 17:02:03.456")
      (is= sql-timestamp (-> sql-timestamp sql-Timestamp->str str->sql-Timestamp))

      (is= (sql-Timestamp->str sql-timestamp) "1999-12-30 17:02:03.456")
      (is= sql-timestamp (-> sql-timestamp sql-Timestamp->str str->sql-Timestamp))

      (is= (str ym) "1999-12")
      (is= ym (-> ym YearMonth->Instant Instant->YearMonth))

      (is= 1999 (.getValue year))
      (is= year (-> year Year->Instant Instant->Year))
      ))

  )

