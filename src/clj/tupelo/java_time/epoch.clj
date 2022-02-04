(ns tupelo.java-time.epoch
  (:refer-clojure :exclude [range])
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.interval :as interval]
    [tupelo.java-time :as tjt]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    )
  (:import
    [java.time LocalDate DayOfWeek ZoneId ZonedDateTime Instant Period]
    [java.time.format DateTimeFormatter]
    [java.time.temporal Temporal TemporalAdjusters TemporalAccessor TemporalAmount ChronoUnit ]
    [java.util Date]
    [tupelo.interval Interval]
    ))

;---------------------------------------------------------------------------------------------------
(def ^:no-doc LocalDate-epoch (LocalDate/parse "1970-01-01"))

;-----------------------------------------------------------------------------
; NOTE: All "Epoch" units are ambiguous regarding timezone. Could be local or UTC.
; #todo add esec (eg Instant.getEpochSecond), eweek, emonth, equarter, quarter-of-year, year-quarter

(s/defn LocalDate->eday :- s/Int ; #todo generalize & test for negative eday
  "Normalizes a LocalDate as the offset from 1970-1-1"
  [arg :- LocalDate] (.between ChronoUnit/DAYS LocalDate-epoch arg))

(s/defn eday->LocalDate :- LocalDate
  "Given an eday, returns a LocalDate "
  [arg :- s/Int] (.plusDays LocalDate-epoch arg))

(s/defn eday->monthValue :- s/Int
  "Given an eday, returns a monthValue in [1..12]"
  [arg :- s/Int] (.getMonthValue (eday->LocalDate arg)))

(s/defn eday->year :- s/Int
  "Given an eday, returns a year like 2013"
  [arg :- s/Int] (.getYear (eday->LocalDate arg)))

(s/defn LocalDateStr->eday :- s/Int
  "Parses a LocalDate string like `1999-12-31` into an integer eday (rel to epoch) like 10956"
  [arg :- s/Str] (-> arg (LocalDate/parse) (LocalDate->eday)))

(s/defn eday->LocalDateStr :- s/Str
  "Converts an integer eday like 10956 (rel to epoch) into a LocalDate string like `1999-12-31` "
  [arg :- s/Int] (-> arg (eday->LocalDate) (str)))

;---------------------------------------------------------------------------------------------------
(def ENano {:enano s/Int})
(def EMilli {:emilli s/Int})
(def ESec {:esec s/Int})
(def EDay {:eday s/Int})
(def EMonth {:emonth s/Int})
(def EQtr {:eqtr s/Int})

(s/defn eday->quarter :- Quarter
  [eday :- EQtr]
  )

;#todo year-quarter => like "2013-Q1"
(s/defn eday->eqtr :- EQtr
  [arg]
  (let [month-value (.getMonthValue arg) ; 1..12
        month-idx   (dec month-value) ; 0..11
        quarter-idx (quot month-idx 3)
        result      (nth year-quarters-sorted-vec quarter-idx)]
    result))

;#todo year-quarter => like "2013-Q1"
(s/defn ->year-quarter :- tsk/Quarter ;#todo rename quarter-of-year
  "Given a date-ish value (e.g. LocalDate, et al), returns the quarter of the year
  as one of #{ :Q1 :Q2 :Q3 :Q4 } "
  [arg]
  (let [month-value (.getMonthValue arg) ; 1..12
        month-idx   (dec month-value) ; 0..11
        quarter-idx (quot month-idx 3)
        result      (nth year-quarters-sorted-vec quarter-idx)]
    result))

(s/defn eday->year-quarter :- tsk/Quarter
  "Like `->year-quarter` but works for eday values"
  [eday :- s/Int] (-> eday (eday->LocalDate) (->year-quarter)))

;-----------------------------------------------------------------------------
(s/defn LocalDate->Instant :- Instant
  "Converts a LocalDate to a java.util.Date, using midnight (start of day) and the UTC timezone."
  [ld :- LocalDate]
  (it-> ld
    (.atStartOfDay it zoneid-utc)
    (.toInstant it)))

(s/defn LocalDate->Date :- Date
  "Converts a LocalDate to a java.util.Date, using midnight (start of day) and the UTC timezone."
  [ld :- LocalDate] (Date/from (LocalDate->Instant ld)))

(s/defn LocalDate-interval->days :- s/Int
  "Returns the duration in days from the start to the end of a LocalDate Interval"
  [interval :- Interval]
  (with-map-vals interval [lower upper]
    (assert (and (LocalDate? lower) (LocalDate? upper)))
    (.between ChronoUnit/DAYS lower upper)))

(s/defn localdates->day-idxs :- [s/Int] ; #todo kill this?
  "Converts a sequence of LocalDate objects into an integer series like [0 1 2 ...], relative to the first value.
  Assumes LocalDate's are in ascending order."
  [ld-vals :- [LocalDate]]
  (let [first-date (xfirst ld-vals)]
    (mapv #(LocalDate-interval->days (interval/new first-date %)) ld-vals)))

(comment
  (s/defn LocalDateStr-interval->eday-interval :- Interval ; #todo kill this?
    [itvl :- Interval]
    (with-map-vals itvl [lower upper]
      (assert (and (LocalDateStr? lower) (LocalDateStr? upper)))
      (interval/new
        (LocalDateStr->eday lower)
        (LocalDateStr->eday upper))))

  (s/defn LocalDate->trailing-interval ; #todo kill this? at least specify type (slice, antislice, closed...?)
    "Returns a LocalDate interval of span N days ending on the date supplied"
    [localdate :- LocalDate
     N :- s/Num]
    (let [ld-start (.minusDays localdate N)]
      (interval/new ld-start localdate)))
  )

;---------------------------------------------------------------------------------------------------
(defn ZonedDateTime?
  "Returns true iff arg is an instance of java.time.ZonedDateTime"
  [it] (instance? ZonedDateTime it)) ; #todo test all

(defn Instant?
  "Returns true iff arg is an instance of java.time.Instant "
  [it] (instance? Instant it))

(defn joda-instant?
  "Returns true iff arg is an instance of org.joda.time.ReadableInstant "
  [it] (instance? org.joda.time.ReadableInstant it))

(defn fixed-time-point?
  "Returns true iff arg represents a fixed point in time. Examples:

      [java.time       ZonedDateTime  Instant]
      [org.joda.time        DateTime  Instant  ReadableInstant]
  "
  [it]
  (or (ZonedDateTime? it)
    (Instant? it)
    (joda-instant? it)))

(defn Temporal?
  "Returns true iff arg is an instance of java.time.temporal.Temporal "
  [it]
  (instance? Temporal it))

(defn Period?
  "Returns true iff arg is an instance of org.joda.time.ReadablePeriod.
  Example:  (period (days 3)) => true "
  [it]
  (instance? Period it))

(defn ->Instant
  "Coerces an Instant, ZonedDateTime, or org.joda.time.ReadableInstant => Instant "
  [arg]
  (cond
    (instance? Instant arg) arg
    (instance? ZonedDateTime arg) (.toInstant arg)
    (instance? org.joda.time.ReadableInstant arg) (-> arg .getMillis Instant/ofEpochMilli)))

(defn ->ZonedDateTime ; #todo -> protocol?
  "Coerces a org.joda.time.ReadableInstant to java.time.ZonedDateTime"
  [arg]
  (cond
    (instance? ZonedDateTime arg) arg
    (instance? Instant arg) (ZonedDateTime/ofInstant arg zoneid-utc)
    (instance? org.joda.time.ReadableInstant arg) (it-> arg
                                                    (->Instant it)
                                                    (.atZone it zoneid-utc))
    :else (throw (IllegalArgumentException. (str "Invalid type found: " (class arg) " " arg)))))

;---------------------------------------------------------------------------------------------------
(def ^:dynamic *zone-id* zoneid-utc)

(defmacro with-zoneid
  [zone-id & forms]
  `(binding [*zone-id* ~zone-id]
     ~@forms))

; #todo maybe new-ZonedDateTime?
(defn zoned-date-time ; #todo add schema & map-version
  "Returns a java.time.ZonedDateTime with the specified parameters and truncated values (day/month=1, hour/minute/sec=0)
  for all other date/time components. Assumes time zone is UTC unless the maximum-arity constructor is used. Usage:

  ; Assumes UTC time zone
  (zoned-date-time)          => current time
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
  ([] (ZonedDateTime/now *zone-id*))
  ([year] (zoned-date-time year 1 1 0 0 0 0 *zone-id*))
  ([year month] (zoned-date-time year month 1 0 0 0 0 *zone-id*))
  ([year month day] (zoned-date-time year month day 0 0 0 0 *zone-id*))
  ([year month day hour] (zoned-date-time year month day hour 0 0 0 *zone-id*))
  ([year month day hour minute] (zoned-date-time year month day hour minute 0 0 *zone-id*))
  ([year month day hour minute second] (zoned-date-time year month day hour minute second 0 *zone-id*))
  ([year month day hour minute second nanos] (zoned-date-time year month day hour minute second nanos *zone-id*))
  ([year month day hour minute second nanos zone-id] (ZonedDateTime/of year month day hour minute second nanos zone-id)))

; #todo: need idempotent ->zoned-date-time-utc (using with-zoneid) & ->instant for ZonedDateTime, Instant, OffsetDateTime
; #todo: need offset-date-time & with-offset
; #todo: need (instant year month day ...) arities

(defn instant
  "Wrapper for java.time.Instant/now "
  [] (java.time.Instant/now))

(defn millis->Instant
  "Wrapper for java.time.Instant/ofEpochMilli "
  [millis] (java.time.Instant/ofEpochMilli millis))

(defn esec->Instant
  "Wrapper for java.time.Instant/ofEpochSecs "
  [esec] (java.time.Instant/ofEpochSecond esec))

(defn now->ZonedDateTime
  "Returns the current time as a java.lang.ZonedDateTime (UTC)"
  [] (with-zoneid zoneid-utc (ZonedDateTime/now)))

(defn now->Instant
  "Returns the current time as a java.lang.Instant"
  [] (Instant/now))

(defn now->iso-str
  "Returns an ISO string representation of the current time,
  like '2019-02-19T18:44:01.123456Z' "
  []
  (-> (java.time.Instant/now)
    (.toString)))

(defn now->iso-str-simple
  "Returns a canonical string representation of the current time truncated to the current second,
  like '2019-02-19 18:44:01Z' "
  []
  (-> (java.time.Instant/now)
    (.truncatedTo ChronoUnit/SECONDS)
    (str/replace-first \T \space)
    (.toString)))

;----------------------------------------------------------------------------------------
; #todo: Make all use protocol for all Temporal's (ZonedDateTime, OffsetDateTime, Instant, ...?)

; #todo: make work for Clojure `=` ZDT, Instant, etc
(defn same-inst? ; #todo coerce to correct type
  "Returns true iff two Instant/ZonedDateTime objects (or similar) represent the same instant of time,
  regardless of time zone. A thin wrapper over `ZonedDateTime/isEqual`"
  [this & others]
  (cond
    (every? Instant? others) (every? truthy?
                               (mapv #(.equals this %) others))

    (every? ZonedDateTime? others) (every? truthy?
                                     (mapv #(.isEqual this %) others))

    ; attempt to coerce to Instant
    :else (let [instants (mapv ->Instant (prepend this others))]
            (apply same-inst? instants))))

(def DateTimeStamp (s/conditional
                     #(instance? ZonedDateTime %) ZonedDateTime
                     #(instance? Instant %) Instant))

; #todo need version of < and <= (N-arity) for both ZDT/Instant

; #todo: make a generic (truncate-to :day)
; #todo: make a generic (previous :tuesday)
; #todo: make a generic (previous-or-same :tuesday)
; #todo: make a generic (next :tuesday)
; #todo: make a generic (next-or-same :tuesday)
(s/defn trunc-to-second
  "Returns a ZonedDateTime truncated to first instant of the second."
  [zdt :- DateTimeStamp]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/SECONDS))

(s/defn trunc-to-minute
  "Returns a ZonedDateTime truncated to first instant of the minute."
  [zdt :- DateTimeStamp]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/MINUTES))

(s/defn trunc-to-hour
  "Returns a ZonedDateTime truncated to first instant of the hour."
  [zdt :- DateTimeStamp]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/HOURS))

(s/defn trunc-to-day
  "Returns a ZonedDateTime truncated to first instant of the day."
  [zdt :- DateTimeStamp]
  (.truncatedTo zdt java.time.temporal.ChronoUnit/DAYS))

(s/defn trunc-to-month
  "Returns a ZonedDateTime truncated to first instant of the month."
  [zdt :- ZonedDateTime]
  (-> zdt
    trunc-to-day
    (.with (TemporalAdjusters/firstDayOfMonth))))

(s/defn trunc-to-year
  "Returns a ZonedDateTime truncated to first instant of the year."
  [zdt :- ZonedDateTime]
  (-> zdt
    trunc-to-day
    (.with (TemporalAdjusters/firstDayOfYear))))

;-----------------------------------------------------------------------------
; #todo maybe a single fn taking `DayOfWeek/SUNDAY` or similar?

(s/defn trunc-to-midnight-sunday
  "For an instant T, truncate time to midnight and return the first Sunday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/SUNDAY))))

(s/defn trunc-to-midnight-monday
  "For an instant T, truncate time to midnight and return the first Monday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/MONDAY))))

(s/defn trunc-to-midnight-tuesday
  "For an instant T, truncate time to midnight and return the first Tuesday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/TUESDAY))))

(s/defn trunc-to-midnight-wednesday
  "For an instant T, truncate time to midnight and return the first Wednesday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/WEDNESDAY))))

(s/defn trunc-to-midnight-thursday
  "For an instant T, truncate time to midnight and return the first thursday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/THURSDAY))))

(s/defn trunc-to-midnight-friday
  "For an instant T, truncate time to midnight and return the first Friday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/FRIDAY))))

(s/defn trunc-to-midnight-saturday
  "For an instant T, truncate time to midnight and return the first Saturday at or before T."
  [temporal :- Temporal]
  (validate Temporal? temporal) ; #todo plumatic schema
  (-> temporal
    trunc-to-day
    (.with (TemporalAdjusters/previousOrSame DayOfWeek/SATURDAY))))

;-----------------------------------------------------------------------------
; #todo rethink these and simplify/rename
(s/defn format->LocalDate-iso :- s/Str ; won't work for Instant
  "Given an Instant or ZonedDateTime, returns a string like `2018-09-05`"
  [zdt :- TemporalAccessor]
  (.format (->ZonedDateTime zdt) DateTimeFormatter/ISO_LOCAL_DATE))

(s/defn format->LocalDate-compact :- s/Str ; won't work for Instant
  "Given an Instant or ZonedDateTime, returns a compact date-time string like
    `2018-09-05 23:05:19.123Z` => `20180905` "
  [inst :- TemporalAccessor]
  (let [formatter (DateTimeFormatter/ofPattern "yyyyMMdd")]
    (.format (->ZonedDateTime inst) formatter)))

(defn format->iso-str ; #todo maybe inst->iso-date-time
  "Given an Instant or ZonedDateTime, returns a ISO 8601 datetime string like `2018-09-05T23:05:19.123Z`"
  [inst]
  (str (->Instant inst))) ; uses DateTimeFormatter/ISO_INSTANT

(defn format->iso-str-nice
  "Given an Instant or ZonedDateTime, returns an ISO date-time string like
  `2018-09-05 23:05:19.123Z` (with a space instead of `T`)"
  [inst]
  (let [sb (StringBuffer. (format->iso-str inst))]
    (.setCharAt sb 10 \space)
    (str sb)))

(defn format->timestamp-compact ; won't work for Instant
  "Given an Instant or ZonedDateTime, returns a compact date-time string like
  `2018-09-05 23:05:19.123Z` => `20180905-230519` "
  [inst]
  (let [formatter (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")]
    (.format (->ZonedDateTime inst) formatter)))

(s/defn parse-iso-str->Instant :- Instant ; #todo => parse-iso-str->millis
  "Convert an ISO 8601 string to epoch milliseconds. Will collapse excess whitespace."
  [iso-datetime-str :- s/Str]
  (-> iso-datetime-str
    (str/trim)
    (Instant/parse)))

(s/defn parse-iso-str->millis :- s/Int ; #todo => parse-iso-str->millis
  "Convert an ISO 8601 string to epoch milliseconds. Will collapse excess whitespace."
  [iso-datetime-str :- s/Str]
  (-> iso-datetime-str
    (parse-iso-str->Instant)
    (.toEpochMilli)))

(s/defn parse-iso-str->sql-timestamp ; #todo => parse-iso-str->sql-timestamp
  "Convert an ISO 8601 string to a java.sql.Timestamp"
  [iso-datetime-str :- s/Str]
  (-> iso-datetime-str
    (parse-iso-str->millis)
    (java.sql.Timestamp.)))

(s/defn parse-iso-str-nice->Instant :- Instant ; #todo => parse-iso-str-nice->Instant
  "Parse a near-iso string like '2019-09-19 18:09:35Z' (it is missing the 'T' between the
  date & time fields) into an Instant. Will collapse excess whitespace."
  [iso-str :- s/Str]
  (it-> iso-str
    (str/whitespace-collapse it)
    (vec it) ; convert to vector of chars
    (assoc it 10 \T) ; set index 10 to a "T" char
    (str/join it) ; convert back to a string
    (Instant/parse it)))

(s/defn parse-sql-timestamp-str->Instant-utc :- Instant ; #todo => parse-sql-timestamp->Instant-utc
  "Parse a near-Timestamp string like '  2019-09-19 18:09:35 ' (sloppy spacing) into an Instant.
  Assumes UTC timezone. Will collapse excess whitespace."
  [sql-timestamp-str :- s/Str]
  (it-> sql-timestamp-str
    (str/whitespace-collapse it)
    (vec it) ; convert to vector of chars
    (assoc it 10 \T) ; set index 10 to a "T" char
    (append it \Z)
    (str/join it) ; convert back to a string
    (Instant/parse it)))

;-----------------------------------------------------------------------------
(defn walk-sql-Timestamp->Instant
  "Walks a tree-like data structure, converting any instances of java.sql.Timestamp => java.time.Instant"
  [tree]
  (walk/postwalk
    (fn [item]
      (if (= java.sql.Timestamp (type item))
        (.toInstant item)
        item))
    tree))

(defn walk-Instant->sql-Timestamp
  "Walks a tree-like data structure, converting any instances of java.sql.Timestamp => java.time.Instant"
  [tree]
  (walk/postwalk
    (fn [item]
      (if (= java.time.Instant (type item))
        (java.sql.Timestamp.
          (.toEpochMilli item))
        item))
    tree))

(defn walk-Instant->str
  "Walks a tree-like data structure, calling `.toString` on any instances java.time.Instant"
  [tree]
  (walk/postwalk
    (fn [item]
      (if (= java.time.Instant (type item))
        (.toString item)
        item))
    tree))

;-----------------------------------------------------------------------------

; #todo make work for relative times (LocalDate, LocalDateTime, etc)
(defn stringify-times
  "Will recursively walk any data structure, converting any `fixed-time-point?` object to a string"
  [form]
  (walk/postwalk (fn [item]
                   (cond-it-> item
                     (fixed-time-point? it) (format->iso-str it)))
    form))

(s/defn range :- [Temporal]
  "Returns a vector of instants in the half-open interval [start stop) (both instants)
  with increment <step> (a period). Not lazy.  Example:

       (range (zoned-date-time 2018 9 1)
              (zoned-date-time 2018 9 5)
              (Duration/ofDays 1)))  => <vector of 4 ZonedDateTime's from 2018-9-1 thru 2018-9-4>
  "
  [start-inst :- Temporal
   stop-inst :- Temporal
   step-dur :- TemporalAmount]
  ; (validate temporal? start-inst) ; #todo use Plumatic Schema
  ; (validate temporal? stop-inst) ; #todo use Plumatic Schema
  ; (verify (instance? TemporalAmount step-dur)) ; #todo -> predicate fn?
  (loop [result    []
         curr-inst start-inst]
    (if (.isBefore ^Temporal curr-inst stop-inst)
      (recur (conj result curr-inst)
        (.plus curr-inst step-dur))
      result)))


