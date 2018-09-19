(ns tupelo.java-time
  (:refer-clojure :exclude [range])
  (:use tupelo.core)
  (:require [schema.core :as s])
  (:import (java.time.temporal TemporalAdjusters Temporal TemporalAmount)
           (java.time DayOfWeek ZoneId ZonedDateTime Instant)
           [java.time.format DateTimeFormatter]))

(defn instant?
  "Returns true iff arg is an instance of java.time.Instant "
  [it]
  (instance? java.time.Instant it))

(defn temporal?
  "Returns true iff arg is an instance of java.time.temporal.Temporal "
  [it]
  (instance? java.time.temporal.Temporal it))

(defn period?
  "Returns true iff arg is an instance of org.joda.time.ReadablePeriod.
  Example:  (period (days 3)) => true "
  [it]
  (instance? java.time.Period it))

;ZonedDateTime/of​(int year, int month, int dayOfMonth,
;                 int hour, int minute, int second, int nanoOfSecond,
;                 ZoneId zone)

; #todo: create a namespace java-time.zone ???
(def zoneid-utc            (ZoneId/of "UTC"))
(def zoneid-us-alaska      (ZoneId/of "US/Alaska"))
(def zoneid-us-aleutian    (ZoneId/of "US/Aleutian"))
(def zoneid-us-central     (ZoneId/of "US/Central"))
(def zoneid-us-eastern     (ZoneId/of "US/Eastern"))
(def zoneid-us-hawaii      (ZoneId/of "US/Hawaii"))
(def zoneid-us-mountain    (ZoneId/of "US/Mountain"))
(def zoneid-us-pacific     (ZoneId/of "US/Pacific"))

(def ^:dynamic *zone-id* zoneid-utc)

(defmacro with-zoneid
  [zone-id & forms]
  `(binding [*zone-id* ~zone-id]
     ~@forms))

; "Returns a ZonedDateTime"
(defn zoned-date-time ; #todo add schema & map-version
  "Returns a java.time.ZonedDateTime with the specified parameters and truncated values (day/month=1, hour/minute/sec=0)
  for all other date/time components. Assumes time zone is UTC unless the maximum-arity constructor is used. Usage:

  ; Assumes UTC time zone
  (zoned-date-time year)
  (zoned-date-time year month)
  (zoned-date-time year month day)
  (zoned-date-time year month day hour)
  (zoned-date-time year month day hour minute)
  (zoned-date-time year month day hour minute second)
  (zoned-date-time year month day hour minute second nanos)

  ; Explicit time zone
  (zoned-date-time year month day hour minute second nanos zone-id)

  ; Explicit time zone alternate shortcut arities.
  (with-zoneid zoneid-us-eastern
    (zoned-date-time year month day ...))  ; any arity w/o zone-id

  "
  ([year]                                             (zoned-date-time year 1     1   0    0      0      0     *zone-id* ))
  ([year month]                                       (zoned-date-time year month 1   0    0      0      0     *zone-id* ))
  ([year month day]                                   (zoned-date-time year month day 0    0      0      0     *zone-id* ))
  ([year month day hour]                              (zoned-date-time year month day hour 0      0      0     *zone-id* ))
  ([year month day hour minute]                       (zoned-date-time year month day hour minute 0      0     *zone-id* ))
  ([year month day hour minute second]                (zoned-date-time year month day hour minute second 0     *zone-id* ))
  ([year month day hour minute second nanos]          (zoned-date-time year month day hour minute second nanos *zone-id* ))
  ([year month day hour minute second nanos zone-id]  (ZonedDateTime/of year month day hour minute second nanos zone-id)))

; #todo: need idempotent ->zoned-date-time-utc (using with-zoneid) & ->instant for ZonedDateTime, Instant, OffsetDateTime
; #todo: need offset-date-time & with-offset
; #todo: need (instant year month day ...) arities

;----------------------------------------------------------------------------------------
; #todo: Make all use protocol for all Temporal's (ZonedDateTime, OffsetDateTime, Instant, ...?)

(s/defn same-instant? :- s/Bool
  "Returns true if two ZonedDateTime objects represent the same instant of time, regardless of time zone.
  A thin wrapper over `ZonedDateTime/isEqual`"
  [this  :- ZonedDateTime
   & others :- [ZonedDateTime]]
  (every? truthy?
    (mapv #(.isEqual this %) others)))

(defn trunc-to-second
  "Returns a ZonedDateTime truncated to first instant of the second."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/SECONDS))

(defn trunc-to-minute
  "Returns a ZonedDateTime truncated to first instant of the minute."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/MINUTES))

(defn trunc-to-hour
  "Returns a ZonedDateTime truncated to first instant of the hour."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/HOURS))

(defn trunc-to-day
  "Returns a ZonedDateTime truncated to first instant of the day."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/DAYS))

(defn trunc-to-month
  "Returns a ZonedDateTime truncated to first instant of the month."
  [zdt]
  (-> zdt
    trunc-to-day
    (.with (TemporalAdjusters/firstDayOfMonth))))

(defn trunc-to-year
  "Returns a ZonedDateTime truncated to first instant of the year."
  [zdt]
  (-> zdt
    trunc-to-day
    (.with (TemporalAdjusters/firstDayOfYear))))

(s/defn trunc-to-sunday-midnight
  "For an instant T, truncate time to midnight and return the first Sunday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/SUNDAY))))

(s/defn trunc-to-monday-midnight
  "For an instant T, truncate time to midnight and return the first Monday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/MONDAY))))

(s/defn trunc-to-tuesday-midnight
  "For an instant T, truncate time to midnight and return the first Tuesday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/TUESDAY))))

(s/defn trunc-to-wednesday-midnight
  "For an instant T, truncate time to midnight and return the first Wednesday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/WEDNESDAY))))

(s/defn trunc-to-thursday-midnight
  "For an instant T, truncate time to midnight and return the first thursday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/THURSDAY))))

(s/defn trunc-to-friday-midnight
  "For an instant T, truncate time to midnight and return the first Friday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/FRIDAY))))

(s/defn trunc-to-saturday-midnight
  "For an instant T, truncate time to midnight and return the first Saturday at or before T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/SATURDAY))))

(defn iso-date-str
  "Returns a string like `2018-09-05`"
  [zdt]
  (.format zdt DateTimeFormatter/ISO_LOCAL_DATE))

(defn iso-date-time-str
  "Returns a ISO date-time string like `2018-09-05T23:05:19.123Z`"
  [zdt]
  (.format zdt DateTimeFormatter/ISO_INSTANT))

(defn nice-date-time-str
  "Returns an ISO date-time string like `2018-09-05 23:05:19.123Z`
  (with a space instead of `T`)"
  [zdt]
  (let [sb (StringBuffer. (.format zdt DateTimeFormatter/ISO_INSTANT))]
    (.setCharAt sb 10 \space)
    (str sb)))

(defn range
  "Returns a vector of instants in the half-open interval [start stop) (both instants)
  with increment <step> (a period). Not lazy.  Example:

       (range (zoned-date-time 2018 9 1)
              (zoned-date-time 2018 9 5)
              (Duration/ofDays 1)))  => <vector of 4 ZonedDateTime's from 2018-9-1 thru 2018-9-4>
  "
  [start-inst stop-inst step-dur]
  (validate temporal? start-inst) ; #todo use Plumatic Schema
  (validate temporal? stop-inst) ; #todo use Plumatic Schema
  (verify (instance? TemporalAmount step-dur)) ; #todo -> predicate fn?
  (loop [result    []
         curr-inst start-inst]
    (if (.isBefore curr-inst stop-inst)
      (recur (conj result curr-inst)
        (.plus curr-inst step-dur))
      result)))

