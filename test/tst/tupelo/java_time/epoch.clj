(ns tst.tupelo.java-time.epoch
  (:refer-clojure :exclude [range])
  (:use tupelo.java-time.epoch tupelo.core tupelo.test)
  (:require
    [tupelo.java-time :as tjt]
    )
  (:import
    [java.time Instant LocalDate LocalDateTime Period]
    ))

(when-java-1-11-plus

  (verify
    (is= 3600 HOUR->SECOND)
    (is= 86400 DAY->SECOND)
    (is= (* 86400 1000) DAY->MILLI)

    (is= (sec->milli 1) SECOND->MILLI)
    (is= (min->sec 1) MINUTE->SECOND)
    (is= (hour->min 1) HOUR->MINUTE)
    (is= (hour->sec 1) HOUR->SECOND)
    (is= (day->sec 1) DAY->SECOND)

    ; verify truncation
    (is= (milli->sec (inc SECOND->MILLI)) 1)
    (is= (sec->min (inc MINUTE->SECOND)) 1)
    (is= (min->hour (inc HOUR->MINUTE)) 1)
    (is= (sec->hour (inc HOUR->SECOND)) 1)
    (is= (sec->day (inc DAY->SECOND)) 1)

    )

  (verify
    (isnt (enano? {:eday 234}))
    (isnt (emilli? {:eday 234}))
    (isnt (esec? {:eday 234}))
    (is (eday? {:eday 234}))
    (isnt (emonth? {:eday 234}))
    (isnt (eqtr? {:eday 234})))

  (verify ; LocalDate <==> eday
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
  ;(verify
  ;  (let [ldstr->monthVal (fn [arg] (-> arg (LocalDateStr->eday) (eday->monthValue)))]
  ;    (is= 1 (ldstr->monthVal "2013-01-25"))
  ;    (is= 2 (ldstr->monthVal "2013-02-28"))
  ;    (is= 11 (ldstr->monthVal "2013-11-30"))
  ;    (is= 12 (ldstr->monthVal "2013-12-31"))))
  ;
  ;(verify
  ;  (let [ldstr->year (fn [arg] (-> arg (LocalDateStr->eday) (eday->year)))]
  ;    (is= 2013 (ldstr->year "2013-01-25"))
  ;    (is= 2014 (ldstr->year "2014-02-28"))
  ;    (is= 2014 (ldstr->year "2014-11-30"))
  ;    (is= 2019 (ldstr->year "2019-12-31"))))
  ;
  ;(verify
  ;  (is= {:eday 9134} (LocalDateStr->eday "1995-01-04"))
  ;  (is= {:eday 10956} (LocalDateStr->eday "1999-12-31"))
  ;  (doseq [daynum [0 9 99 999 9999]]
  ;    (is= {:eday daynum} (-> daynum (->eday) (eday->LocalDateStr) (LocalDateStr->eday)))))

  ;-----------------------------------------------------------------------------
  (verify
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
        (->eday ld)))
    (is= (esec->emilli {:esec 1}) {:emilli 1000})
    (is= (eday->esec {:eday 1}) {:esec 86400})

    (is= (emilli->esec {:emilli 1000}) {:esec 1})
    (is= (emilli->esec {:emilli 1001}) {:esec 1})
    (is= (esec->eday {:esec 86400}) {:eday 1})
    (is= (esec->eday {:esec 86401}) {:eday 1}))

  ;-----------------------------------------------------------------------------
  (verify
    ; always rounds toward negative infinity
    (is= +1.0 (Math/floor 1.5))
    (is= -2.0 (Math/floor -1.5))

    ; integer values
    (is= "1969-12-31T23:59:59Z" (str (esec->Instant -1)))
    (is= "1970-01-01T00:00:00Z" (str (esec->Instant 0)))
    (is= "1970-01-01T00:00:01Z" (str (esec->Instant 1)))

    ; floating-point values
    (is= "1969-12-31T23:59:59Z" (str (esec->Instant -1.0)))
    (is= "1970-01-01T00:00:00Z" (str (esec->Instant 0.0)))
    (is= "1970-01-01T00:00:00.100Z" (str (esec->Instant 0.1)))
    (is= "1970-01-01T00:00:00.999990Z" (str (esec->Instant 0.99999)))
    (is= "1970-01-01T00:00:00.999999900Z" (str (esec->Instant 0.9999999)))

    (let [epoch-instant (Instant/parse "1970-01-01t00:00:00Z")
          epoch-millis  (.toEpochMilli epoch-instant)
          epoch-esec    (quot epoch-millis 1000)]
      (is= 0 epoch-millis)
      (is= 0 epoch-esec)
      (is= epoch-instant (esec->Instant epoch-esec)))

    (is= (->esec "1970-01-01t00:00:00Z") {:esec 0})
    (is= (->esec "1970-01-01t00:00:00.123Z") {:esec 0})
    (is= (->esec "1970-01-01t01:00:00Z") {:esec 3600})
    (is= (->esec 3600) {:esec 3600})
    (is= (->esec "1970-01-02t00:00:00Z") {:esec 86400})
    (is= (->esec "1971-01-01t00:00:00Z") {:esec (* 86400 365)})
    (is= (->esec (tjt/->Instant "1971-01-01t00:00:00Z")) {:esec (* 86400 365)})
    (is= (->esec (tjt/->Instant "1971-01-01t00:00:00.123Z")) {:esec (* 86400 365)})
    (is= (->esec (tjt/->ZonedDateTime "1971-01-01t00:00:00Z")) {:esec (* 86400 365)}))
  )
