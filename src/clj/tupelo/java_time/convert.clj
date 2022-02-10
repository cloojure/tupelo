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
   :LocalDate     #"(?x)\d{4}-\d{2}-\d{2}"    ; 1999-12-31   (maybe allow sloppy like '1999-4-2' ?)

   :Timestamp     #"(?x)\d{4}-\d{2}-\d{2}     # 1999-12-31
                   \s{1}                      # single space
                   \d{2}:\d{2}:\d{2}"         ; 11:22:33

   :LocalDateTime #"(?ix)\d{4}-\d{2}-\d{2}    # 1999-12-31
                   t                          # separator
                   \d{2}:\d{2}:\d{2}          # 11:22:33
                   (\.\d+)?"                  ; fractional seconds optional

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
  (let [tgt (str/trim s)]
    (cond
      (re-matches (grab :LocalDate patterns) tgt)
      (-> tgt (LocalDate/parse) (LocalDate->LocalDateTime) (LocalDateTime->ZonedDateTime) (Instant/from))

      (re-matches (grab :LocalDateTime patterns) tgt)
      (-> tgt (LocalDateTime/parse) (LocalDateTime->ZonedDateTime) (Instant/from))

      (re-matches (grab :Timestamp patterns) tgt)
      (tjt/parse-sql-timestamp-str->Instant-utc tgt)

      (re-matches (grab :Instant patterns) tgt)
      (Instant/parse tgt)

      (re-matches (grab :ZonedDateTime patterns) tgt)
      (-> tgt (ZonedDateTime/parse) (Instant/from))

      :else (throw (ex-info "pattern not recognized" {:s tgt}))
      )))
