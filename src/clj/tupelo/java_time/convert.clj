(ns tupelo.java-time.convert
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    )
  (:import
    [java.time LocalDate LocalDateTime DayOfWeek ZoneId ZonedDateTime Instant Period LocalDateTime]
    [java.time.temporal Temporal TemporalUnit TemporalAdjusters TemporalAccessor TemporalAmount ChronoUnit]
    [java.util Date]
    ))

(def zoneid-utc (ZoneId/of "UTC"))

(s/defn LocalDate+startOfDay->LocalDateTime :- LocalDateTime
  "Converts LocalDate -> LocalDateTime at midnight "
  [ld :- LocalDate] (.atStartOfDay ld))

(s/defn LocalDateTime+utc->ZonedDateTime :- ZonedDateTime
  "Converts LocalDateTime -> ZonedDateTime with UTC time zone"
  [ldt :- LocalDateTime] (.atZone ldt zoneid-utc))

(s/defn LocalDate->Instant :- Instant
  "Converts a LocalDate to an Instant, using midnight (start of day) and the UTC timezone."
  [ld :- LocalDate]
  (-> ld
    (LocalDate+startOfDay->LocalDateTime)
    (LocalDateTime+utc->ZonedDateTime)
    (Instant/from)))

(s/defn LocalDate->Date :- Date
  "Converts a LocalDate to a java.util.Date, using midnight (start of day) and the UTC timezone."
  [ld :- LocalDate] (Date/from (LocalDate->Instant ld)))

(s/defn Instant->Date :- Date
  [inst :- Instant] (Date/from inst))

(s/defn Date->Instant :- Instant
  [date :- Date] (.toInstant date))

(s/defn Date->str :- s/Str
  [date :- Date] (str (Date->Instant date)))

(s/defn str->Date :- Date
  "Parse an Instant string into a Date"
  [s :- s/Str] (Instant->Date (Instant/parse s)))

; #todo ZDT->Instant

; #todo Instant->LocalDate
; #todo Instant->LocalDateTime
; #todo Instant->YearMonth
; #todo Instant->Year
