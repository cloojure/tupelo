(ns tupelo.java-time.convert
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    )
  (:import
    [java.time LocalDate LocalDateTime DayOfWeek ZoneId ZonedDateTime Instant Period LocalDateTime]
    [java.time.temporal Temporal TemporalUnit TemporalAdjusters TemporalAccessor TemporalAmount ChronoUnit ]
    ))

(def zoneid-utc (ZoneId/of "UTC"))

(s/defn LocalDate+startOfDay->LocalDateTime :- LocalDateTime
  "Converts LocalDate -> LocalDateTime at midnight "
  [ld :- LocalDate] (.atStartOfDay ld))

(s/defn LocalDateTime+utc->ZonedDateTime :- ZonedDateTime
  "Converts LocalDateTime -> ZonedDateTime with UTC time zone"
  [ldt :- LocalDateTime] (.atZone ldt zoneid-utc))

