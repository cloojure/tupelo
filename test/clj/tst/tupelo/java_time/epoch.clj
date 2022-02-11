(ns ^:test-refresh/focus
  tst.tupelo.java-time.epoch
  (:refer-clojure :exclude [range])
  (:use tupelo.java-time.epoch tupelo.core tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.java-time :as tjt]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    )
  (:import
    [java.time Duration Instant MonthDay YearMonth LocalDate LocalDateTime Period
               ZoneId ZoneId ZonedDateTime]
    [java.time.temporal Temporal TemporalAdjusters TemporalAccessor TemporalAmount ChronoUnit TemporalUnit]
    ))

(dotest
  (is= 3600 HOUR->SECONDS)
  (is= 86400 DAY->SECONDS)
  (is= (* 86400 1000) DAY->MILLIS)

  (is= (seconds->millis 1) SECOND->MILLIS)
  (is= (minutes->seconds 1) MINUTE->SECONDS)
  (is= (hours->minutes 1) HOUR->MINUTES)
  (is= (hours->seconds 1) HOUR->SECONDS)
  (is= (days->seconds 1) DAY->SECONDS)

  ; verify truncation
  (is= (millis->seconds (inc SECOND->MILLIS)) 1)
  (is= (seconds->minutes (inc MINUTE->SECONDS)) 1)
  (is= (minutes->hours (inc HOUR->MINUTES)) 1)
  (is= (seconds->hours (inc HOUR->SECONDS)) 1)
  (is= (seconds->days (inc DAY->SECONDS)) 1)

  )

(dotest
  (isnt (enano? {:eday 234}))
  (isnt (emilli? {:eday 234}))
  (isnt (esec? {:eday 234}))
  (is (eday? {:eday 234}))
  (isnt (emonth? {:eday 234}))
  (isnt (eqtr? {:eday 234})))

(dotest ; LocalDate <==> eday
  (is= {:eday 0} (LocalDate->eday (LocalDate/parse "1970-01-01")))
  (is= {:eday 1} (LocalDate->eday (LocalDate/parse "1970-01-02")))
  (is= {:eday 31} (LocalDate->eday (LocalDate/parse "1970-02-01")))
  (is= {:eday 365} (LocalDate->eday (LocalDate/parse "1971-01-01")))
  (doseq [ld-str ["1970-01-01"
                  "1970-01-02"
                  "1970-02-01"
                  "1971-01-01"
                  "1999-12-31"]]
    (let [ld (LocalDate/parse ld-str)]
      (is= ld (-> ld (LocalDate->eday) (eday->LocalDate)))))
  (doseq [daynum [0 1 9 99 999 9999]]
    (is= {:eday daynum} (-> daynum (->eday) (eday->LocalDate) (LocalDate->eday)))))

;-----------------------------------------------------------------------------
;(dotest
;  (let [ldstr->monthVal (fn [arg] (-> arg (LocalDateStr->eday) (eday->monthValue)))]
;    (is= 1 (ldstr->monthVal "2013-01-25"))
;    (is= 2 (ldstr->monthVal "2013-02-28"))
;    (is= 11 (ldstr->monthVal "2013-11-30"))
;    (is= 12 (ldstr->monthVal "2013-12-31"))))
;
;(dotest
;  (let [ldstr->year (fn [arg] (-> arg (LocalDateStr->eday) (eday->year)))]
;    (is= 2013 (ldstr->year "2013-01-25"))
;    (is= 2014 (ldstr->year "2014-02-28"))
;    (is= 2014 (ldstr->year "2014-11-30"))
;    (is= 2019 (ldstr->year "2019-12-31"))))
;
;(dotest
;  (is= {:eday 9134} (LocalDateStr->eday "1995-01-04"))
;  (is= {:eday 10956} (LocalDateStr->eday "1999-12-31"))
;  (doseq [daynum [0 9 99 999 9999]]
;    (is= {:eday daynum} (-> daynum (->eday) (eday->LocalDateStr) (LocalDateStr->eday)))))

;-----------------------------------------------------------------------------
(dotest
  (let [inst (Instant/parse "1987-11-22t11:22:33Z")
        zdt  (tjt/->ZonedDateTime inst)
        ld   (tjt/->LocalDate inst)
        ]
    (is= {:eday 0} (->eday "1970-01-01"))
    (is= {:eday 1} (->eday "1970-01-02"))
    (is= {:eday 31} (->eday "1970-02-01"))
    (is= {:eday 365} (->eday "1971-01-01"))
    (is= {:eday 6534}
      (->eday inst)
      (->eday zdt)
      (->eday ld))))

;-----------------------------------------------------------------------------
(dotest
  (is= (->esec "1970-01-01t00:00:00Z") {:esec 0})
  (is= (->esec "1970-01-01t00:00:00.123Z") {:esec 0})
  (is= (->esec "1970-01-01t01:00:00Z") {:esec 3600})
  (is= (->esec 3600) {:esec 3600})
  (is= (->esec "1970-01-02t00:00:00Z") {:esec 86400})
  (is= (->esec "1971-01-01t00:00:00Z") {:esec (* 86400 365)})
  (is= (->esec (tjt/->Instant "1971-01-01t00:00:00Z")) {:esec (* 86400 365)})
  (is= (->esec (tjt/->ZonedDateTime "1971-01-01t00:00:00Z")) {:esec (* 86400 365)})
  )










