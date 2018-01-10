;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.time.instant
  (:use tupelo.time.instant tupelo.core tupelo.test )
  (:require
    [tupelo.impl :as i] )
  (:import [java.time Instant LocalDate Month DayOfWeek YearMonth MonthDay Year LocalTime]
           [java.time.temporal TemporalAdjuster TemporalAdjusters]
           [java.util Locale]
           [java.time.format TextStyle])
)

(dotest
  (let [new-years-almost (Instant/parse "2017-12-31T13:14:15.678z")
        today            (LocalDate/now)

        payday-1         (-> (LocalDate/of 2018 Month/JANUARY 1)
                           (.with (TemporalAdjusters/lastDayOfMonth))
                           (.minusDays 2))

        payday-2         (-> (LocalDate/of 2018 Month/JANUARY 9)
                           (with (TemporalAdjusters/lastDayOfMonth))
                           (minusDays 2))]

    (is (-> (Instant/now) (.isAfter new-years-almost)))
    (is= payday-1 payday-2 (LocalDate/of 2018 Month/JANUARY 29)))

  (let [dateOfBirth   (LocalDate/of 2012 Month/MAY 14)
        firstBirthday (-> dateOfBirth
                        (.plusYears 1))]
    (is= firstBirthday (LocalDate/of 2013 Month/MAY 14)) )

  (let [monday DayOfWeek/MONDAY
        locale (Locale/getDefault)]
    (is= DayOfWeek/THURSDAY (.plus monday 3))
    (is= (.getDisplayName monday TextStyle/FULL locale) "Monday")
    (is= (.getDisplayName monday TextStyle/NARROW locale) "M")
    (is= (.getDisplayName monday TextStyle/SHORT locale) "Mon") )

  (let [feb-max-len (.maxLength Month/FEBRUARY)
        august Month/AUGUST
        locale (Locale/getDefault)]
    (is= 29 feb-max-len)
    (is= (.getDisplayName august TextStyle/FULL locale) "August")
    (is= (.getDisplayName august TextStyle/NARROW locale) "A")
    (is= (.getDisplayName august TextStyle/SHORT locale) "Aug") )

  (let [date    (LocalDate/of 2000 Month/NOVEMBER 20) ; a Monday
        nextWed (-> date (.with (TemporalAdjusters/next DayOfWeek/WEDNESDAY)))]
    (is= nextWed (LocalDate/of 2000 Month/NOVEMBER 22))
    (is= DayOfWeek/MONDAY (-> (LocalDate/of 2012, Month/JULY, 9) (.getDayOfWeek))) )

  (let [feb-2010 (YearMonth/of 2010 Month/FEBRUARY)
        feb-2012 (YearMonth/of 2012 Month/FEBRUARY) ]
    (is= 28 (.lengthOfMonth feb-2010))
    (is= 29 (.lengthOfMonth feb-2012))
    (is= "2010-02"  (str feb-2010))
    (is= "2012-02"  (str feb-2012)))

  (let [month-day (MonthDay/of Month/FEBRUARY 29)
        valid-leap-year (.isValidYear month-day 2010)]
    (isnt valid-leap-year)
    (is (-> 2012 (Year/of) (.isLeap))))

  (let [mid-lunch (LocalTime/of 12 13 14)]
    (is= 12 (.getHour mid-lunch))
    (is= 13 (.getMinute mid-lunch))
    (is= 14 (.getSecond mid-lunch))
    )

  )
