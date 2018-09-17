(ns tupelo.java-time
  (:use tupelo.core)
  (:refer-clojure :exclude [range])
  (:import (java.time.temporal TemporalAdjusters)
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

(def zoneid-utc (ZoneId/of "UTC"))

; "Returns a ZonedDateTime"
(defn zoned-date-time ; #todo add schema & map-version
  ([year] (zoned-date-time time year 1 1 0 0 0 0 zoneid-utc ))
  ([year month] (zoned-date-time time year month 1 0 0 0 0 zoneid-utc ))
  ([year month day] (zoned-date-time time year month day 0 0 0 0 zoneid-utc ))
  ([year month day hour] (zoned-date-time time year month day hour 0 0 0 zoneid-utc ))
  ([year month day hour minute] (zoned-date-time time year month day hour minute 0 0 zoneid-utc ))
  ([year month day hour minute second] (zoned-date-time time year month day hour minute second 0 zoneid-utc ))
  ([year month day hour minute second nanos] (zoned-date-time time year month day hour minute second nanos zoneid-utc ))
  ([year month day hour minute second nanos zone-id] (ZonedDateTime/of time year month day hour minute second nanos zone-id)))

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

(defn floor-sunday
  "Given an instant T, returns the first Sunday (at midnight) less than or equal to T."
  [temporal]
  (validate temporal? temporal) ; #todo plumatic schema
  (.with temporal (TemporalAdjusters/previousOrSame DayOfWeek/SUNDAY)))

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
