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
   :LocalDate     #"\d{4}-\d{2}-\d{2}" ; maybe allow 1999-4-2 ???
   :Timestamp     #"\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}"
   :LocalDateTime #"(?i)\d{4}-\d{2}-\d{2}t\d{2}:\d{2}:\d{2}"
   :Instant       #"(?i)\d{4}-\d{2}-\d{2}t\d{2}:\d{2}:\d{2}(\.\d+)?z"
   :ZonedDateTime #"(?i)\d{4}-\d{2}-\d{2}t\d{2}:\d{2}:\d{2}\+\d{2}:\d{2}(\[\w+\])?"
   })
(s/defn LocalDate->LocalDateTime :- LocalDateTime
  "Converts LocalDate -> LocalDateTime at midnight "
  [ld :- LocalDate] (.atStartOfDay ld))

(s/defn LocalDateTime->ZonedDateTime :- ZonedDateTime
  "Converts LocalDateTime -> ZonedDateTime with UTC time zone"
  [ldt :- LocalDateTime] (.atZone ldt tjt/zoneid-utc))

(s/defn str->Instant :- Instant
  "Parse a string into a java.time.Instant. Valid formats include:

        1999-11-22                      ; LocalDate (assumes midnight utc)
        1999-11-22 00:00:00             ; sql timestamp (assumes utc)
        1999-11-22t00:00:00             ; LocalDateTime
        1999-11-22t00:00:00z            ; Instant
        1999-11-22t00:00:00.000z        ; Instant
        1999-11-22t00:00:00+00:00       ; ZonedDateTime
        1999-11-22t00:00:00+00:00[UTC]  ; ZonedDateTime
 "
  [s :- s/Str]
  (cond
    (re-matches (grab :LocalDate patterns) s)
    (-> s (LocalDate/parse) (LocalDate->LocalDateTime) (LocalDateTime->ZonedDateTime) (Instant/from))

    (re-matches (grab :LocalDateTime patterns) s)
    (-> s (LocalDateTime/parse) (LocalDateTime->ZonedDateTime) (Instant/from))

    (re-matches (grab :Timestamp patterns) s)
    (tjt/parse-sql-timestamp-str->Instant-utc s)

    (re-matches (grab :Instant patterns) s)
    ;  (re-matches (grab :Instant+nanos patterns) s)

    (Instant/parse s)

    (re-matches (grab :ZonedDateTime patterns) s)
    (-> s (ZonedDateTime/parse) (Instant/from))

    :else (throw (ex-info "pattern not recognized" {:s s}))
    ))
