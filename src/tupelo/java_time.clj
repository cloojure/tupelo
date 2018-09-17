(ns tupelo.java-time
  (:use tupelo.core)
  (:refer-clojure :exclude [range])
  (:require [schema.core :as s])
  (:import (java.time.temporal TemporalAdjusters Temporal)
           (java.time DayOfWeek ZoneId ZonedDateTime)))

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

;----------------------------------------------------------------------------------------
; #todo: Make all use protocol for ZonedDateTime, OffsetDateTime, or Instant

(s/defn same-instant? :- s/Bool
  "Returns true if two ZonedDateTime objects represent the same instant of time, regardless of time zone.
  A thin wrapper over `ZonedDateTime/isEqual`"
  [this  :- ZonedDateTime
   & others :- [ZonedDateTime]]
  (every? truthy?
    (mapv #(.isEqual this %) others)))

(defn ->beginning-of-second
  "Returns a ZonedDateTime truncated to first instant of the second."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/SECONDS))

(defn ->beginning-of-minute
  "Returns a ZonedDateTime truncated to first instant of the minute."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/MINUTES))

(defn ->beginning-of-hour
  "Returns a ZonedDateTime truncated to first instant of the hour."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/HOURS))

(defn ->beginning-of-day
  "Returns a ZonedDateTime truncated to first instant of the day."
  [zdt]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/DAYS))

(defn ->beginning-of-month
  "Returns a ZonedDateTime truncated to first instant of the month."
  [zdt]
  (-> zdt
    ->beginning-of-day
    (.with (TemporalAdjusters/firstDayOfMonth))))

(defn ->beginning-of-year
  "Returns a ZonedDateTime truncated to first instant of the year."
  [zdt]
  (-> zdt
    ->beginning-of-day
    (.with (TemporalAdjusters/firstDayOfYear))))

(comment
(def fmt-iso-date (grab :year-month-day time-format/formatters))
(def fmt-iso-date-time (grab :date-time time-format/formatters))

(defn ->iso-date-str
  "Converts a timestamp to a string like `2018-09-05`"
  [instant]
  (time-format/unparse fmt-iso-date instant))

(defn ->iso-date-time-str
  "Converts a timestamp to a string like `2018-09-05T23:05:19.123Z`"
  [instant]
  (time-format/unparse fmt-iso-date-time instant))

(defn ->nice-date-time-str
  "Converts a timestamp to an ISO date-time string like `2018-09-05 23:05:19.123Z`
  (with a space instead of `T`) "
  [instant]
  (let [sb (StringBuffer. (->iso-date-time-str instant))]
    (.setCharAt sb 10 \space)
    (str sb)))
)

(s/defn floor-sunday
  "Given an instant T, returns the first Sunday (at midnight) less than or equal to T."
  [temporal :- Temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (-> temporal
    ->beginning-of-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/SUNDAY))))

(comment
(defn range
  "Returns a vector of instants in the half-open interval [start stop) (both instants)
  with increment <step> (a period). Not lazy.  Example:

       (range (time/date-time 2018 9 1)
              (time/date-time 2018 9 5)
              (time/days 1)))  => <vector of 4 instants from 2018-9-1 thru 2018-9-4>
  "
  [start-inst stop-inst step-dur]
  (validate instant? start-inst) ; #todo use Plumatic Schema
  (validate instant? stop-inst)
  (validate period? step-dur)
  (loop [result    []
         curr-inst start-inst]
    (if (time/before? curr-inst stop-inst)
      (recur (conj result curr-inst)
             (time/plus curr-inst step-dur))
      result)))

 )
