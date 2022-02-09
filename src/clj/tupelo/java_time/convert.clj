(ns tupelo.java-time.convert
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.java-time :as tjt]
    [tupelo.string :as str]
    )
  (:import
    [java.time LocalDate LocalDateTime DayOfWeek ZoneId ZonedDateTime Instant Period LocalDateTime]
    [java.time.format DateTimeFormatter]
    [java.time.temporal Temporal TemporalUnit TemporalAdjusters TemporalAccessor TemporalAmount ChronoUnit ]
    [java.util Date]
    [tupelo.interval Interval]
    ))

(def patterns
  {
   :LocalDate  #"\d{4}-\d{2}-\d{2}" ; maybe allow 1999-4-2 ???
   :Timestamp  #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}"
   :Instant  #"(?i)\d{4}-\d{2}-\d{2}t\d{2}:\d{2}:\d{2}z"
   :ZonedDateTime  #"(?i)\d{4}-\d{2}-\d{2}t\d{2}:\d{2}:\d{2}\[\w+\]"
   }
  )
(s/defn LocalDate->LocalDateTime :- LocalDateTime
  "Converts LocalDate -> LocalDateTime at midnight "
  [ld :- LocalDate] (.atStartOfDay ld))

(s/defn LocalDateTime->ZonedDateTime :- ZonedDateTime
  "Converts LocalDateTime -> ZonedDateTime with UTC time zone"
  [ldt :- LocalDateTime] (.atZone ldt tjt/zoneid-utc))

(s/defn str->Instant :- Instant
  "Parse a string into a java.time.Instant"
  [arg :- s/Str]
  (cond
    (str/contains-match? arg (grab :LocalDate patterns))
    (it-> arg (LocalDate/parse it) (.atStartOfDay it tjt/zoneid-utc) (.toInstant it))
    ))