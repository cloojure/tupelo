(ns tupelo.java-time.convert
  (:use tupelo.core)
  (:require
    [clj-time.coerce :as joda.coerce]
    [schema.core :as s]
    [tupelo.schema :as tsk])
  (:import
    [java.sql Timestamp]
    [java.time LocalDate LocalDateTime Instant LocalDateTime YearMonth Year ZoneId ZonedDateTime ZoneOffset]
    [java.util Date]
    [org.joda.time ReadableInstant]
    ))

;-----------------------------------------------------------------------------
(s/defn sql-Timestamp->Instant :- Instant
  "Converts a java.sql.Timestamp to a java.time.Instant"
  [sql-Timestamp :- java.sql.Timestamp] (.toInstant sql-Timestamp))

(s/defn Instant->sql-Timestamp :- java.sql.Timestamp
  "Converts a java.time.Instant to a java.sql.Timestamp"
  [inst :- Instant] (java.sql.Timestamp. (.toEpochMilli inst)))

;-----------------------------------------------------------------------------
(s/defn joda->Instant :- Instant
  "Converts a joda DateTime or similar to a java.time.Instant"
  [joda-inst :- org.joda.time.ReadableInstant]
  (Instant/ofEpochMilli (joda.coerce/to-long joda-inst)))

(s/defn Instant->joda :- org.joda.time.ReadableInstant
  "Converts a java.time.Instant to a joda DateTime"
  [inst :- Instant] (joda.coerce/from-long (.toEpochMilli inst)))

;-----------------------------------------------------------------------------
(def ^:no-doc zoneid-utc (ZoneId/of "UTC"))

(s/defn LocalDate->LocalDateTime :- LocalDateTime
  "Converts LocalDate -> LocalDateTime at start-of-day "
  [ld :- LocalDate] (.atStartOfDay ld))

(s/defn LocalDateTime->ZonedDateTime :- ZonedDateTime
  "Converts LocalDateTime -> ZonedDateTime in UTC time zone"
  [ldt :- LocalDateTime] (.atZone ldt zoneid-utc))

(s/defn Instant->LocalDate :- LocalDate
  "Converts an Instant to a LocalDate using the UTC timezone."
  [inst :- Instant]
  (if-java-1-11-plus
    (LocalDate/ofInstant ^Instant inst, ^ZoneId zoneid-utc)
    (throw (RuntimeException. "Unimplemented prior to Java 1.9"))))

(s/defn LocalDate->Instant :- Instant
  "Converts a LocalDate to an Instant at start-of-day in the UTC timezone."
  [ld :- LocalDate]
  (-> ld
    (LocalDate->LocalDateTime)
    (.toInstant ZoneOffset/UTC)))

(s/defn Instant->LocalDateTime :- LocalDateTime
  "Converts an Instant to a LocalDateTime in the UTC timezone."
  [inst :- Instant]
  (if-java-1-11-plus
    (LocalDateTime/ofInstant inst zoneid-utc)
    (throw (RuntimeException. "Unimplemented prior to Java 1.9"))))

(s/defn LocalDateTime->Instant :- Instant
  "Converts a LocalDateTime to an Instant in the UTC timezone."
  [ldt :- LocalDateTime] (.toInstant ldt ZoneOffset/UTC))

(s/defn LocalDate->Date :- Date
  "Converts a LocalDate to a java.util.Date at start-of-day in the UTC timezone."
  [ld :- LocalDate] (Date/from (LocalDate->Instant ld)))

(s/defn Instant->Date :- Date
  "Convert an Instant to a java.util.Date"
  [inst :- Instant] (Date/from inst))
(s/defn Date->Instant :- Instant
  "Convert a java.util.Date to an Instant"
  [date :- Date] (.toInstant date))

(s/defn Date->str :- s/Str
  "Convert a java.util.Date to a string like '1999-12-31T01:02:03.456Z'"
  [date :- Date] (str (Date->Instant date)))
(s/defn str->Date :- Date
  "Parses a string like '1999-12-31T01:02:03.456Z' a Date"
  [s :- s/Str] (Instant->Date (Instant/parse s)))

(s/defn sql-Date->str :- s/Str
  "Converts a java.sql.Date into a String like '1999-12-30' "
  [date :- java.sql.Date] (str date))
(s/defn str->sql-Date :- Date
  "Parses a String like '1999-12-30' into a java.sql.Date"
  [s :- s/Str] (java.sql.Date/valueOf  s))

(s/defn sql-Timestamp->str :- s/Str
  "Converts a java.sql.Timestamp into a string like '1999-12-30 17:02:03.456'"
  [ts :- Timestamp] (str ts))
(s/defn str->sql-Timestamp :- Timestamp
  "Parses a string like '1999-12-30 17:02:03.456' into a java.sql.Timestamp"
  [s :- s/Str] (Timestamp/valueOf  s))

(s/defn ZonedDateTime->Instant :- Instant
  "Converts a ZonedDateTime to an Instant"
  [zdt :- ZonedDateTime] (.toInstant zdt))
(s/defn Instant->ZonedDateTime :- ZonedDateTime
  "Converts an Instant into a ZonedDateTime in the UTC timezone"
  [inst :- Instant] (ZonedDateTime/ofInstant inst zoneid-utc))

(s/defn Instant->YearMonth :- YearMonth
  "Convert an Instant to a YearMonth in the UTC timezone"
  [inst :- Instant ]
  (let [ld (Instant->LocalDate inst)
        ym (YearMonth/of (.getYear ld) (.getMonth ld))]
    ym))
(s/defn YearMonth->Instant :- Instant
  "Given a YearMonth, returns the first Instant in the UTC timezone"
  [ym :- YearMonth ]
  (let [ld (LocalDate/of  (.getYear ym) (.getMonth ym) 1)]
    (LocalDate->Instant ld)))

(s/defn Instant->Year :- Year
  "Convert an Instant to a Year object in the UTC timezone"
  [inst :- Instant ]
  (let [ld (Instant->LocalDate inst)]
    (Year/of (.getYear ld))))
(s/defn Year->Instant :- Instant
  "Given a Year object, returns the first Instant using the UTC timezone"
  [year :- Year ]
  (let [ld (LocalDate/of (.getValue year) 1 1)]
    (LocalDate->Instant ld)))

