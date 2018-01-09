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
  (:import [java.time Instant LocalDate]
           [java.time.temporal TemporalAdjuster TemporalAdjusters])
)

(defprotocol ILocalDate
  (with [localDate temporalAdjuster])
  (minusDays [localDate daysToSubtract])
  )

(extend-type LocalDate
  ILocalDate
  (with      [localDate temporalAdjuster] (.with localDate temporalAdjuster))
  (minusDays [localDate daysToSubtract]   (.minusDays localDate daysToSubtract))
  )

(dotest
  (is (.isAfter (Instant/now) ; create clojure.java.time ns
        (Instant/parse "2017-12-31T13:14:15z")))
  (let-spy [today (LocalDate/now)
             payday (-> today
                      (.with (TemporalAdjusters/lastDayOfMonth))
                      (.minusDays 2))
            payday2 (-> today
                      (with (TemporalAdjusters/lastDayOfMonth))
                      (minusDays 2))

        ])

  )
