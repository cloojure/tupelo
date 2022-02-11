(ns tupelo.java-time
  (:refer-clojure :exclude [range])
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.interval :as interval]
    [tupelo.java-time.convert :as convert]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    )
  (:import
    [java.time LocalDate LocalDateTime DayOfWeek ZoneId ZonedDateTime Instant Period LocalDateTime]
    [java.time.format DateTimeFormatter]
    [java.time.temporal Temporal TemporalUnit TemporalAdjusters TemporalAccessor TemporalAmount ChronoUnit ]
    [java.util Date]
    [tupelo.interval Interval]
    ))

;-----------------------------------------------------------------------------
; #todo study:  clojure.java.io/Coercions
; #todo study:  clojure.java.io/file

;---------------------------------------------------------------------------------------------------
(declare
  parse-iso-str->Instant
  parse-iso-str->millis
  parse-iso-str->sql-timestamp
  parse-iso-str-nice->Instant
  parse-sql-timestamp-str->Instant-utc)

;---------------------------------------------------------------------------------------------------
(comment
  (def months-q1 #{Month/JANUARY Month/FEBRUARY Month/MARCH})
  (def months-q2 #{Month/APRIL Month/MAY Month/JUNE})
  (def months-q3 #{Month/JULY Month/AUGUST Month/SEPTEMBER})
  (def months-q4 #{Month/OCTOBER Month/NOVEMBER Month/DECEMBER}))

;---------------------------------------------------------------------------------------------------
(def ^:no-doc LocalDate-epoch (LocalDate/parse "1970-01-01"))

; #todo: create a namespace java-time.zoneid ???
(def zoneid-utc (ZoneId/of "UTC"))
(def zoneid-us-alaska (ZoneId/of "US/Alaska"))
(def zoneid-us-aleutian (ZoneId/of "US/Aleutian"))
(def zoneid-us-central (ZoneId/of "US/Central"))
(def zoneid-us-eastern (ZoneId/of "US/Eastern"))
(def zoneid-us-hawaii (ZoneId/of "US/Hawaii"))
(def zoneid-us-mountain (ZoneId/of "US/Mountain"))
(def zoneid-us-pacific (ZoneId/of "US/Pacific"))

;-----------------------------------------------------------------------------
(def ^:no-doc iso-date-regex #"(\d\d\d\d)-(\d\d)-(\d\d)")
(def ^:no-doc iso-date-bounds-default {:year   {:min 1776 :max 2112}
                                       :month  {:min 1 :max 12}
                                       :day    {:min 1 :max 31}
                                       :hour   {:min 0 :max 23}
                                       :minute {:min 0 :max 59}
                                       :second {:min 0 :max 60}})

;---------------------------------------------------------------------------------------------------
(s/defn LocalDate? :- s/Bool
  "Returns true iff arg is of type LocalDate"
  [arg]
  (= java.time.LocalDate (type arg)))

(s/defn LocalDateStr? :- s/Bool
  "Returns true iff string is a legal ISO LocalDate like '1999-12-31' (valid for years 1900-2100)."
  [arg]
  (and (string? arg)
    (= 10 (count arg))
    (with-exception-default false
      (LocalDate/parse arg)
      true))) ; if no exception => passes

(s/defn LocalDate->tagval :- {:LocalDate s/Str}
  "Converts a java.time.LocalDate object to a tagval"
  [ld :- LocalDate] {:LocalDate (str ld)})

(s/defn tagval->LocalDate :- LocalDate
  "Parses a tagval into a java.time.LocalDate"
  [ldtv :- {:LocalDate s/Str}]
  (LocalDate/parse (grab :LocalDate ldtv)))

(defn walk-LocalDate->tagval
  [data]
  (walk/postwalk (fn [item]
                   (cond-it-> item
                     (instance? LocalDate it) (LocalDate->tagval it)))
    data))

;---------------------------------------------------------------------------------------------------
(def year-quarters (sorted-set :Q1 :Q2 :Q3 :Q4)) ; #todo => String like "Q1"
(def ^:no-doc year-quarters-sorted-vec (vec (sort year-quarters)))
(s/defn year-quarter? :- s/Bool
  "Returns true iff arg is indicates a (financial) quarter in the year."
  [arg] (contains-key? year-quarters arg))

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

;-----------------------------------------------------------------------------
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

;---------------------------------------------------------------------------------------------------
(def time-str-patterns
  {:LocalDate     #"(?x)\d{4}-\d{2}-\d{2}"    ; 1999-12-31   (maybe allow sloppy like '1999-4-2' ?)

   :Timestamp     #"(?x)\d{4}-\d{2}-\d{2}     # 1999-12-31
                   \s{1}                      # single space
                   \d{2}:\d{2}:\d{2}"         ; 11:22:33

   :LocalDateTime #"(?ix)\d{4}-\d{2}-\d{2}    # 1999-12-31
                   t                          # separator
                   \d{2}:\d{2}:\d{2}          # 11:22:33
                   (\.\d+)?"                  ; fractional seconds optional

   :iso-str-nice  #"(?ix)\d{4}-\d{2}-\d{2}    # 1999-12-31
                   \s{1}                      # space as separator
                   \d{2}:\d{2}:\d{2}          # 11:22:33
                   (\.\d+)?                   # fractional seconds optional
                   z"                         ; always utc or "zulu" time zone

   :Instant       #"(?ix)\d{4}-\d{2}-\d{2}    # 1999-12-31
                   t                          # separator
                   \d{2}:\d{2}:\d{2}          # 11:22:33
                   (\.\d+)?                   # fractional seconds optional
                   z"                         ; always utc or "zulu" time zone

   :ZonedDateTime #"(?ix)\d{4}-\d{2}-\d{2}    # 1999-12-31
                   t                          # separator
                   \d{2}:\d{2}:\d{2}          # 11:22:33
                   (\.\d+)?                   # fractional seconds optional
                   \+\d{2}:\d{2}              # +01:00
                   (\[\w+\])?"                ; timezone label like '[UTC]' optional
   })

(s/defn LocalDate-str? :- s/Bool
  "Returns true if string matches a LocalDate pattern like '1999-12-31' "
  [s :- s/Str] (truthy? (re-matches (grab :LocalDate time-str-patterns) s)))

(s/defn Timestamp-str? :- s/Bool
  "Returns true if string matches a SQL Timestamp pattern like '1999-12-31 11:22:33' "
  [s :- s/Str] (truthy? (re-matches (grab :Timestamp time-str-patterns) s)))

(s/defn LocalDateTime-str? :- s/Bool
  "Returns true if string matches a LocalDateTime pattern like '1999-12-31T11:22:33' "
  [s :- s/Str] (truthy? (re-matches (grab :LocalDateTime time-str-patterns) s)))

(s/defn Instant-str? :- s/Bool
  "Returns true if string matches a Instant (ISO 8601) pattern like '1999-12-31T11:22:33Z' "
  [s :- s/Str] (truthy? (re-matches (grab :Instant time-str-patterns) s)))

(s/defn iso-str-nice-str? :- s/Bool
  "Returns true if string matches a 'nice' ISO 8601 pattern like '1999-12-31 11:22:33Z' "
  [s :- s/Str] (truthy? (re-matches (grab :iso-str-nice time-str-patterns) s)))

(s/defn ZonedDateTime-str? :- s/Bool
  "Returns true if string matches a Instant pattern like '1999-12-31T11:22:33-08:00Z' "
  [s :- s/Str] (truthy? (re-matches (grab :ZonedDateTime time-str-patterns) s)))

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

(defn fixed-point?
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
    (instance? org.joda.time.ReadableInstant arg) (-> arg .getMillis Instant/ofEpochMilli)
    (instance? String arg)  (parse-iso-str-nice->Instant arg) ; #todo need unit test
    :else (throw (ex-info "Invalid arg type" {:type (type arg) :arg arg}))))

(defn ->ZonedDateTime ; #todo -> protocol?
  "Coerces a org.joda.time.ReadableInstant to java.time.ZonedDateTime"
  [arg]
  (cond
    (instance? ZonedDateTime arg) arg
    (instance? Instant arg) (ZonedDateTime/ofInstant arg zoneid-utc)
    (instance? org.joda.time.ReadableInstant arg) (it-> arg
                                                    (->Instant it)
                                                    (.atZone it zoneid-utc))
    (instance? String arg) (->ZonedDateTime (parse-iso-str-nice->Instant arg)) ; #todo need unit test
    :else (throw (ex-info "Invalid arg type" {:type (type arg) :arg arg}))))

(s/defn ->LocalDate :- LocalDate ; #todo need tests, => tjt
  [arg]
  (cond
    (string? arg) (LocalDate/parse arg)
    (instance? Instant arg) (LocalDate/ofInstant arg zoneid-utc)
    (instance? ZonedDateTime arg) (->LocalDate (->Instant arg))
    ; #todo LocalDateTime
    (instance? org.joda.time.ReadableInstant arg) (->LocalDate (->Instant arg)) ; #todo need test
    :else (throw (ex-info "Invalid arg type" {:type (type arg) :arg arg}))))

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
(def pseudo-Temporal (s/cond-pre Temporal org.joda.time.ReadableInstant))
(s/defn same-instant? :- s/Bool ; #todo coerce to correct type
  "Returns true iff two Instant/ZonedDateTime objects (or similar) represent the same instant of time,
  regardless of time zone. A thin wrapper over `ZonedDateTime/isEqual`"
  [& temporals :- [pseudo-Temporal]]
  (let [instants (if (every? Instant? temporals)
                   temporals
                   (mapv ->Instant temporals)) ; coerce all to Instant
        [base & others] instants]
    (every? truthy?
      (mapv #(.equals base %) others))))

; #todo need version of < and <= (N-arity) for both ZDT/Instant

(def ^:no-doc truncated-to-units-direct #{ChronoUnit/NANOS
                                          ChronoUnit/MILLIS
                                          ChronoUnit/SECONDS
                                          ChronoUnit/MINUTES
                                          ChronoUnit/HOURS
                                          ChronoUnit/DAYS})
(s/defn truncated-to
  "Returns a Temporal truncated to corresponding ChronoUnit.  Example:

        (truncated-to t ChronoUnit/HOURS)
  "
  [temporal :- Temporal,
   chrono-unit :- ChronoUnit]
  (cond
    (contains-key? truncated-to-units-direct chrono-unit) (.truncatedTo temporal chrono-unit)

    (= chrono-unit ChronoUnit/MONTHS)
    ; cannot use previous pattern or get:
    ;   UnsupportedTemporalTypeException: Unit is too large to be used for truncation
    (it-> temporal
      (truncated-to it ChronoUnit/DAYS)
      (.with it (TemporalAdjusters/firstDayOfMonth)))

    (= chrono-unit ChronoUnit/YEARS)
    ; cannot use previous pattern or get:
    ;   UnsupportedTemporalTypeException: Unit is too large to be used for truncation
    (it-> temporal
      (truncated-to it ChronoUnit/DAYS)
      (.with it (TemporalAdjusters/firstDayOfYear)))

    :else (throw (ex-info "invalid chrono-unit" (vals->map temporal chrono-unit)))))

;-----------------------------------------------------------------------------
(s/defn between :- s/Int
  "Returns the integer number of ChronoUnit values between two temporal values, truncating any fraction.
   Example:

        (let [i1      (->Instant \"1987-11-22t01:30:00z\")
              i2      (->Instant \"1987-11-22t03:29:00z\")
              delta   (between ChronoUnit/HOURS i1 i2) ]  ...)

  yields delta=1 since 1hr 59min is truncated to 1 hour. "
  [chrono-unit :- ChronoUnit
   t1 :- Temporal
   t2 :- Temporal]
  (.between chrono-unit t1 t2))

;-----------------------------------------------------------------------------
; #todo: make a generic (previous :tuesday)
; #todo: make a generic (next :tuesday)
; #todo: make a generic (next-or-same :tuesday)
(s/defn previous-or-same :- Temporal
  "Given a temporal value and a target such as DayOfWeek/SUNDAY, makes the minimal
  change to previous or same day of week. Example:

        (previous-or-same t DayOfWeek/SUNDAY)
  "
  [temporal  :- Temporal
   tgt-dow :- DayOfWeek]
  (.with temporal (TemporalAdjusters/previousOrSame tgt-dow)))

;-----------------------------------------------------------------------------
(s/defn format->LocalDate-iso :- s/Str ; won't work for Instant
  "Given an Instant or ZonedDateTime, returns a string like `2018-09-05`"
  [zdt :- Temporal]
  (.format (->ZonedDateTime zdt) DateTimeFormatter/ISO_LOCAL_DATE))

(s/defn format->LocalDate-compact :- s/Str ; won't work for Instant
  "Given an Instant or ZonedDateTime, returns a compact date-time string like
    `2018-09-05 23:05:19.123Z` => `20180905` "
  [inst :- Temporal]
  (let [formatter (DateTimeFormatter/ofPattern "yyyyMMdd")]
    (.format (->ZonedDateTime inst) formatter)))

(s/defn format->iso-str :- s/Str ; #todo maybe inst->iso-date-time
  "Given an Instant or ZonedDateTime, returns a ISO 8601 datetime string like `2018-09-05T23:05:19.123Z`"
  [inst :- Temporal]
  (str (->Instant inst))) ; uses DateTimeFormatter/ISO_INSTANT

(s/defn format->iso-str-nice :- s/Str
  "Given an Instant or ZonedDateTime, returns an ISO date-time string like
  `2018-09-05 23:05:19.123Z` (with a space instead of `T`)"
  [inst :- Temporal]
  (let [sb (StringBuffer. (format->iso-str inst))]
    (.setCharAt sb 10 \space)
    (str sb)))

(s/defn format->timestamp-compact :- s/Str ; won't work for Instant
  "Given an Instant or ZonedDateTime, returns a compact date-time string like
  `2018-09-05 23:05:19.123Z` => `20180905-230519` "
  [inst :- Temporal]
  (let [formatter (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss")]
    (.format (->ZonedDateTime inst) formatter)))

(s/defn parse-iso-str->Instant :- Instant
  "Convert an ISO 8601 string to epoch milliseconds. Will collapse excess whitespace."
  [iso-datetime-str :- s/Str]
  (-> iso-datetime-str
    (str/whitespace-collapse)
    (Instant/parse)))


(s/defn parse-iso-str->millis :- s/Int ; #todo => convert/Instant->millis
  "Convert an ISO 8601 string to epoch milliseconds. Will collapse excess whitespace."
  [iso-datetime-str :- s/Str]
  (-> iso-datetime-str
    (parse-iso-str->Instant)
    (.toEpochMilli)))

(s/defn parse-iso-str->sql-timestamp
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
(s/defn str->Instant :- Instant
  "Parse a string into a java.time.Instant. Valid formats include:

      1999-12-31                          ; LocalDate (assumes utc & start-of-day (00:00:00)
      1999-12-31 11:22:33                 ; sql timestamp (assumes utc)
      1999-12-31T11:22:33                 ; LocalDateTime (assumes utc)
      1999-12-31T11:22:33.123             ; LocalDateTime (assumes utc)
      1999-12-31T11:22:33z                ; Instant (ISO 8601)
      1999-12-31 11:22:33Z                ; ISO 8601 'nice' format
      1999-12-31T11:22:33.123z            ; Instant
      1999-12-31T11:22:33+00:00           ; ZonedDateTime
      1999-12-31T11:22:33.123+00:00[UTC]  ; ZonedDateTime
 "
  [s :- s/Str]
  (let [tgt (str/whitespace-collapse s)]
    (cond
      (LocalDate-str? tgt) (-> tgt (LocalDate/parse) (convert/LocalDate->Instant))

      (LocalDateTime-str? tgt)
      (-> tgt (LocalDateTime/parse) (convert/LocalDateTime+utc->ZonedDateTime) (Instant/from))

      (Timestamp-str? tgt) (parse-sql-timestamp-str->Instant-utc tgt)

      (iso-str-nice-str? tgt) (parse-iso-str-nice->Instant tgt)

      (Instant-str? tgt) (Instant/parse tgt)

      (ZonedDateTime-str? tgt) (-> tgt (ZonedDateTime/parse) (Instant/from))

      :else (throw (ex-info "pattern not recognized" {:s tgt})))))

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
                     (fixed-point? it) (format->iso-str it)))
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


