;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.parse

  #?@(:clj
      [ (:use tupelo.core tupelo.test)
       (:require
         [tupelo.parse :as tpar]
         [tupelo.core :as t]
         [schema.core :as s])
       (:import
         [java.lang.Math])]))

#?(:clj (do

(dotest
  (is= (tpar/edn-parsible {:a 1 :b "two" :c [1 2 3]}) ; function object is not edn-parsible
    {:a 1 :b "two" :c [1 2 3]})
  (let [some-fn (fn [x] (+ x 1))]
    (is= (tpar/edn-parsible {:a 1 :x some-fn}) ; function object is not edn-parsible
      {:a 1 :x :tupelo.parse/edn-non-parsible})))

;-----------------------------------------------------------------------------
(dotest
  (is= 15                               (tpar/parse-byte "15"))
  (is= -5                               (tpar/parse-byte "-5"))
  (throws? NumberFormatException        (tpar/parse-byte "999"))
  (throws? NumberFormatException        (tpar/parse-byte " "))

  (is= 15                               (tpar/parse-byte "15"             :default nil ))
  (is= -5                               (tpar/parse-byte "-5"             :default nil ))
  (is= nil                              (tpar/parse-byte "999"            :default nil ))
  (is= nil                              (tpar/parse-byte ""               :default nil ))
  (is= 0                                (tpar/parse-byte "xyz"            :default 0   )))

(dotest
  (is= 15                               (tpar/parse-short "15"))
  (is= -5                               (tpar/parse-short "-5"))
  (is= 999                              (tpar/parse-short "999"))
  (throws? NumberFormatException        (tpar/parse-short "99999"))
  (throws? NumberFormatException        (tpar/parse-short" "))

  (is= 15                               (tpar/parse-short "15"            :default nil ))
  (is= -5                               (tpar/parse-short "-5"            :default nil ))
  (is= 999                              (tpar/parse-short "999"           :default nil ))
  (is= nil                              (tpar/parse-short "99999"         :default nil ))
  (is= nil                              (tpar/parse-short ""              :default nil ))
  (is= 0                                (tpar/parse-short "xyz"           :default 0   )))

(dotest
  (is= 15                               (tpar/parse-int "15"))
  (is= -5                               (tpar/parse-int "-5"))
  (is= 99999                            (tpar/parse-int "99999"))
  (throws? NumberFormatException        (tpar/parse-int "9876543210"))
  (throws? NumberFormatException        (tpar/parse-int ""))

  (is= 15                               (tpar/parse-int "15"              :default nil ))
  (is= -5                               (tpar/parse-int "-5"              :default nil ))
  (is= 99999                            (tpar/parse-int "99999"           :default nil ))
  (is= nil                              (tpar/parse-int "9876543210"      :default nil ))
  (is= nil                              (tpar/parse-int ""                :default nil ))
  (is= 0                                (tpar/parse-int "xyz"             :default 0   )))

(dotest
  (is= 15                               (tpar/parse-long "15"))
  (is= -5                               (tpar/parse-long "-5"))
  (is= 99999                            (tpar/parse-long "99999"))
  (is= 9876543210                       (tpar/parse-long "9876543210"))
  (throws? NumberFormatException        (tpar/parse-long "98765432109876543210"))
  (throws? NumberFormatException        (tpar/parse-long ""))

  (is= 15                               (tpar/parse-long "15"                     :default nil ))
  (is= -5                               (tpar/parse-long "-5"                     :default nil ))
  (is= 99999                            (tpar/parse-long "99999"                  :default nil ))
  (is= 9876543210                       (tpar/parse-long "9876543210"             :default nil ))
  (is= nil                              (tpar/parse-long "98765432109876543210"   :default nil ))
  (is= nil                              (tpar/parse-long ""                       :default nil ))
  (is= 0                                (tpar/parse-long "xyz"                    :default 0   )))

(dotest
  (is= 15.0                             (tpar/parse-float "15"))
  (is= -5.0                             (tpar/parse-float "-5"))
  (is= 0.5                              (tpar/parse-float "0.5"))
  (is (t/rel=  0.1                      (tpar/parse-float "0.1")            :digits 7))
  (is (t/rel=  3.141592654              (tpar/parse-float "3.141592654")    :digits 7))
  (throws? NumberFormatException        (tpar/parse-float ""))
  (throws? NumberFormatException        (tpar/parse-float "xyz")

  (is= 15.0                             (tpar/parse-float "15"               :default nil ))
  (is= -5.0                             (tpar/parse-float "-5"               :default nil ))
  (is= nil                              (tpar/parse-float ""                 :default nil ))
  (is= 0                                (tpar/parse-float "xyz"              :default 0   ))
  (is= 0.5                              (tpar/parse-float "0.5"              :default nil ))
  (is (t/rel=  (/ 1 10)                 (tpar/parse-float "0.1"              :default 0) :digits 7))
  (is (t/rel=  3.141592654              (tpar/parse-float "3.141592654"      :default 0) :digits 7))) )

(dotest
  (is= 15.0                             (tpar/parse-double "15"))
  (is= -5.0                             (tpar/parse-double "-5"))
  (throws? NumberFormatException        (tpar/parse-double ""))
  (throws? NumberFormatException        (tpar/parse-double "xyz"))
  (is= 0.5                              (tpar/parse-double "0.5"))
  (is (t/rel=  (double (/ 1 10) )       (tpar/parse-double "0.1")           :digits 9))
  (is (t/rel=  Math/PI                  (tpar/parse-double "3.141592654")   :digits 9))

  (is= 15.0                             (tpar/parse-double "15"          :default nil ))
  (is= -5.0                             (tpar/parse-double "-5"          :default nil ))
  (is= nil                              (tpar/parse-double ""            :default nil ))
  (is= 0                                (tpar/parse-double "xyz"         :default 0   ))
  (is= 0.5                              (tpar/parse-double "0.5"         :default nil ))
  (is (t/rel= (/ 1 10)                  (tpar/parse-double "0.1"         :default 0)     :digits 9))
  (is (t/rel= Math/PI                   (tpar/parse-double "3.141592654" :default 0)     :digits 9)) )

))
