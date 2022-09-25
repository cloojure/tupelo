(ns tupelo.java-time.epoch
  (:refer-clojure :exclude [range])
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.interval :as interval]
    [tupelo.java-time :as tjt]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    [tupelo.tagval :as tv]
    )
  (:import
    [java.time LocalDate LocalDateTime ZonedDateTime Instant Year YearMonth LocalDateTime]
    [java.time.temporal ChronoUnit]
    [tupelo.interval Interval]
    ))

;-----------------------------------------------------------------------------
; All "epoch time" quantities expressed as a tupelo.tagval so that one knows what "type" the integer represents
(def ENano {:enano s/Int})
(def EMilli {:emilli s/Int})
(def ESec {:esec s/Int})
(def EDay {:eday s/Int})
(def EMonth {:emonth s/Int})
(def EQtr {:eqtr s/Int})

;-----------------------------------------------------------------------------
(s/defn enano? :- s/Bool
  [arg :- s/Any] (and (tv/tagval? arg) (= :enano (tv/tag arg)) (int? (tv/val arg))))
(s/defn emilli? :- s/Bool
  [arg :- s/Any] (and (tv/tagval? arg) (= :emilli (tv/tag arg)) (int? (tv/val arg))))
(s/defn esec? :- s/Bool
  [arg :- s/Any] (and (tv/tagval? arg) (= :esec (tv/tag arg)) (int? (tv/val arg))))
(s/defn eday? :- s/Bool
  [arg :- s/Any] (and (tv/tagval? arg) (= :eday (tv/tag arg)) (int? (tv/val arg))))
(s/defn emonth? :- s/Bool
  [arg :- s/Any] (and (tv/tagval? arg) (= :emonth (tv/tag arg)) (int? (tv/val arg))))
(s/defn eqtr? :- s/Bool
  [arg :- s/Any] (and (tv/tagval? arg) (= :eqtr (tv/tag arg)) (int? (tv/val arg))))

;-----------------------------------------------------------------------------
; Remember, in java.time there are no leap seconds!
(def SECOND->NANO (* 1000 1000 1000))
(def SECOND->MILLI 1000)
(def MINUTE->SECOND 60)
(def HOUR->MINUTE 60)
(def DAY->HOUR 24)
(def WEEK->DAY 7)
(def YEAR->MONTH 12)

(def HOUR->SECOND (* HOUR->MINUTE MINUTE->SECOND) )
(def DAY->SECOND (* DAY->HOUR HOUR->SECOND) )
(def WEEK->SECOND (* WEEK->DAY DAY->SECOND) )

(def DAY->MILLI (* DAY->SECOND SECOND->MILLI) )
(def WEEK->MILLI (* WEEK->DAY DAY->MILLI) )

; *->MINUTE
; *->HOUR

;-----------------------------------------------------------------------------
; larger to smaller units => exact calculation

(s/defn sec->milli :- s/Int
  "Converts integer seconds to milliseconds"
  [sec :- s/Int] (* sec SECOND->MILLI))
(s/defn esec->emilli :- EMilli
  "Converts tagval esec to emilli"
  [esec :- ESec] {:emilli (sec->milli (tv/val esec))})

(s/defn min->sec :- s/Int
  "Converts integer minutes to seconds"
  [min :- s/Int] (* min MINUTE->SECOND))

(s/defn hour->min :- s/Int
  "Converts integer hours to minutes"
  [hour :- s/Int] (* hour HOUR->MINUTE))

(s/defn hour->sec :- s/Int
  "Converts integer hours to seconds"
  [hour :- s/Int] (* hour HOUR->SECOND))

(s/defn day->sec :- s/Int
  "Converts integer hours to seconds"
  [day :- s/Int] (* day DAY->SECOND))
(s/defn eday->esec :- ESec
  "Converts tagval eday to esec"
  [eday :- EDay] {:esec (day->sec (tv/val eday))} )

;-----------------------------------------------------------------------------
; smaller to larger units => truncation

(s/defn milli->sec :- s/Int
  "Converts integer milliseconds to seconds, with truncation"
  [milli :- s/Int] (quot milli SECOND->MILLI))
(s/defn emilli->esec :- ESec
  "Converts tagval emilli to esec with truncation"
  [emilli :- EMilli] {:esec (quot (tv/val emilli) SECOND->MILLI)})

(s/defn sec->min :- s/Int
  "Converts integer seconds to minutes, with truncation"
  [sec :- s/Int] (quot sec MINUTE->SECOND))

(s/defn min->hour :- s/Int
  "Converts integer minutes to hours, with truncation"
  [min :- s/Int] (quot min HOUR->MINUTE))

(s/defn sec->hour :- s/Int
  "Converts integer seconds to hours, with truncation"
  [sec :- s/Int] (quot sec HOUR->SECOND))

(s/defn sec->day :- s/Int
  "Converts integer seconds to hours, with truncation"
  [sec :- s/Int] (quot sec DAY->SECOND))
(s/defn esec->eday :- EDay
  "Converts tagval esec to eday with truncation"
  [esec :- ESec] {:eday (quot (tv/val esec) DAY->SECOND)})

;-----------------------------------------------------------------------------
(def ^:no-doc epoch-Year-int 1970)
(def ^:no-doc epoch-Year (Year/parse "1970"))
(def ^:no-doc epoch-YearMonth (YearMonth/parse "1970-01"))
(def ^:no-doc epoch-LocalDate (LocalDate/parse "1970-01-01")) ; modern: LocalDate/EPOCH
(def ^:no-doc epoch-LocalDateTime (LocalDateTime/parse "1970-01-01T00:00")) ; seconds optional here
(def ^:no-doc epoch-Instant  Instant/EPOCH)

;-----------------------------------------------------------------------------
; NOTE: All "Epoch" units are ambiguous regarding timezone. Could be local or UTC.
; #todo add esec (eg Instant.getEpochSecond), eweek, emonth, equarter, quarter-of-year, year-quarter

; #todo inline?
(s/defn LocalDate->eday :- EDay ; #todo generalize & test for negative eday
  "Returns the number of days since the epoch"
  [arg :- LocalDate] {:eday (.toEpochDay arg)})

(s/defn eday->LocalDate :- LocalDate
  "Converts an eday to a LocalDate"
  [arg :- EDay] (LocalDate/ofEpochDay (tv/val arg)))

; #todo inline?
(s/defn Instant->esec :- ESec ; #todo generalize & test for negative eday
  "Converts an Instant to whole epoch seconds"
  [arg :- Instant] {:esec (.getEpochSecond arg)})

(defn esec->Instant
  "Wrapper for java.time.Instant/ofEpochSecs "
  [esec] (java.time.Instant/ofEpochSecond esec))

(comment  ; #todo conversion for floating point epoch-seconds->Instant
  (def SECOND->NANOS 1.0e9)

  (s/defn epoch-sec->Instant :- Instant
    "Accepts a floating point value of epoch second and converts to a java.time.Instant"
    [epoch-seconds :- s/Num]
    (assert (<= 0 epoch-seconds)) ; throw for negative times for simplicity
    (let [esec-dbl      (double epoch-seconds)
          esec-whole    (Math/floor esec-dbl)
          esec-fraction (- esec-dbl esec-whole)
          esec-nanos    (Math/round (* esec-fraction SECOND->NANOS))
          result        (Instant/ofEpochSecond (long esec-whole) (long esec-nanos))]
      result))

  (verify
    (throws? (epoch-sec->Instant -1))
    (is= "1970-01-01T00:00:00Z" (str (epoch-sec->Instant 0.0)))
    (is= "1970-01-01T00:00:00.100Z" (str (epoch-sec->Instant 0.1)))
    (is= "1970-01-01T00:00:00.999990Z" (str (epoch-sec->Instant 0.99999)))
    (is= "1970-01-01T00:00:00.999999900Z" (str (epoch-sec->Instant 0.9999999)))))

(s/defn eday->year :- s/Int
  "Given an eday, returns a year like 2013"
  [arg :- EDay] (.getYear (eday->LocalDate arg)))

(s/defn ->eday :- EDay
  [arg]
  (cond
    (string? arg) (LocalDate->eday (tjt/->LocalDate (str/trim arg)))
    (int? arg) {:eday arg} ; #todo add other types
    (instance? LocalDate arg) (LocalDate->eday arg)
    (instance? Instant arg) (->eday (tjt/->LocalDate arg))
    (instance? ZonedDateTime arg) (->eday (tjt/->LocalDate arg))
    ; (instance? org.joda.time.ReadableInstant arg) (->eday (tjt/->Instant arg)) ; #todo need test
    :else (throw (ex-info "Invalid arg type" {:type (type arg) :arg arg}))))

; #todo use .toEpochSecond & .ofEpochSecond
(s/defn ->esec :- ESec           ; #todo finish
  [arg]
  (cond
    (string? arg) (->esec (tjt/truncated-to (tjt/->Instant arg) ChronoUnit/SECONDS))
    (int? arg) {:esec arg} ; #todo add other types
    (instance? Instant arg) (Instant->esec arg)
    (instance? ZonedDateTime arg) (->esec (tjt/->Instant arg))
    ; (instance? org.joda.time.ReadableInstant arg) (->esec (tjt/->Instant arg)) ; #todo need test
    :else (throw (ex-info "Invalid arg type" {:type (type arg) :arg arg}))))

; #todo: & "str->XXX" as (Instant->XXX (str->Instant XXX))
; #todo: Constructor functions
; ->enano
; ->emilli ; use .toEpochMillis & .ofEpochMillis
; ->eqtr
; ->emonth
; ->year
; #todo source: Instant, ZDT

; #todo: conversions (enano, emilli, esec, eday, emonth, eqtr, year) <=> all

; #todo eXXX->Instant
; #todo maybe eXXX->eYYY (sec/day, etc)

(s/defn between :- s/Int ; #todo test
  "Returns the integer difference between two epoch-vals (- ev2 ev1)"
  [epoch-val-1 :- tsk/TagVal
   epoch-val-2 :- tsk/TagVal]
  (let [tag1 (tv/tag epoch-val-1)
        tag2 (tv/tag epoch-val-2)
        int1 (tv/val epoch-val-1)
        int2 (tv/val epoch-val-2)]
    (when (not= tag1 tag2)
      (throw (ex-info "incompatible epoch values " (vals->map epoch-val-1 epoch-val-2))))
    (- int2 int1)))

; #todo need fns for add, subtract, etc ???

(comment

  (s/defn eday->quarter :- EQtr
    [eday :- EQtr]
    )

  ;#todo year-quarter => like "2013-Q1"
  (s/defn eday->eqtr :- EQtr
    [arg :- EDay]
    (let [month-value (.getMonthValue arg) ; 1..12
          month-idx   (dec month-value) ; 0..11
          quarter-idx (quot month-idx 3)
          ]
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

  )

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

