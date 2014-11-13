;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.parse-test
  (:require [cooljure.parse             :as cool]
            [clojure.test               :refer :all] )
  (:import   [java.lang.Math] ))

(defn error-ratio
  "Compute the error ratio for two floating-point values."
  [ val1 val2 ]
  (let [  dbl1         (double val1)
          dbl2         (double val2)
          abs-delta    (Math/abs (- dbl1 dbl2))
          max-abs-val  (Math/max (double (Math/abs dbl1))
                                 (double (Math/abs dbl2)) )
          error-ratio  (/ abs-delta max-abs-val) ]
    error-ratio 
  ))
; AWTAWT TODO: handle [0  0] case

(deftest parse-byte
  (testing "basic"
    (is (= 15                              (cool/parse-byte "15")))
    (is (= -5                              (cool/parse-byte "-5")))
    (is (thrown? NumberFormatException     (cool/parse-byte "999")))
    (is (thrown? NumberFormatException     (cool/parse-byte " "))) )
  (testing "with :or"
    (is (= 15      (cool/parse-byte "15"                           :or nil )))
    (is (= -5      (cool/parse-byte "-5"                           :or nil )))
    (is (= nil     (cool/parse-byte "999"                          :or nil )))
    (is (= nil     (cool/parse-byte ""                             :or nil )))
    (is (= 0       (cool/parse-byte "xyz"                          :or 0   ))) ))

(deftest parse-short
  (testing "basic"
    (is (= 15                              (cool/parse-short "15")))
    (is (= -5                              (cool/parse-short "-5")))
    (is (= 999                             (cool/parse-short "999")))
    (is (thrown? NumberFormatException     (cool/parse-short "99999")))
    (is (thrown? NumberFormatException     (cool/parse-short" "))) )
  (testing "with :or"
    (is (= 15      (cool/parse-short "15"                          :or nil )))
    (is (= -5      (cool/parse-short "-5"                          :or nil )))
    (is (= 999     (cool/parse-short "999"                         :or nil )))
    (is (= nil     (cool/parse-short "99999"                       :or nil )))
    (is (= nil     (cool/parse-short ""                            :or nil )))
    (is (= 0       (cool/parse-short "xyz"                         :or 0   ))) ))

(deftest parse-int
  (testing "basic"
    (is (= 15                              (cool/parse-int "15")))
    (is (= -5                              (cool/parse-int "-5")))
    (is (= 99999                           (cool/parse-int "99999")))
    (is (thrown? NumberFormatException     (cool/parse-int "9876543210")))
    (is (thrown? NumberFormatException     (cool/parse-int ""))) )
  (testing "with :or"
    (is (= 15      (cool/parse-int "15"                            :or nil )))
    (is (= -5      (cool/parse-int "-5"                            :or nil )))
    (is (= 99999   (cool/parse-int "99999"                         :or nil )))
    (is (= nil     (cool/parse-int "9876543210"                    :or nil )))
    (is (= nil     (cool/parse-int ""                              :or nil )))
    (is (= 0       (cool/parse-int "xyz"                           :or 0   ))) ))

(deftest parse-long
  (testing "basic"
    (is (= 15                              (cool/parse-long "15")))
    (is (= -5                              (cool/parse-long "-5")))
    (is (= 99999                           (cool/parse-long "99999")))
    (is (= 9876543210                      (cool/parse-long "9876543210")))
    (is (thrown? NumberFormatException     (cool/parse-long "98765432109876543210")))
    (is (thrown? NumberFormatException     (cool/parse-long ""))) )
  (testing "with :or"
    (is (= 15           (cool/parse-long "15"                            :or nil )))
    (is (= -5           (cool/parse-long "-5"                            :or nil )))
    (is (= 99999        (cool/parse-long "99999"                         :or nil )))
    (is (= 9876543210   (cool/parse-long "9876543210"                    :or nil )))
    (is (= nil          (cool/parse-long "98765432109876543210"          :or nil )))
    (is (= nil          (cool/parse-long ""                              :or nil ))) 
    (is (= 0            (cool/parse-long "xyz"                           :or 0   ))) ))

(deftest parse-float
  (testing "basic"
    (is (= 15.0                            (cool/parse-float "15")))
    (is (= -5.0                            (cool/parse-float "-5")))
    (is (thrown? NumberFormatException     (cool/parse-float "")))
    (is (thrown? NumberFormatException     (cool/parse-float "xyz")))
    (is (= 0.5                             (cool/parse-float "0.5")))
    (is (> 1e-7 (error-ratio               (cool/parse-float "0.1") 0.1 )))
    (is (> 1e-7 (error-ratio               (cool/parse-float "3.141592654") 3.141592654 ))) )

  (testing "with :or"
    (is (= 15.0                     (cool/parse-float "15"               :or nil )))
    (is (= -5.0                     (cool/parse-float "-5"               :or nil )))
    (is (= nil                      (cool/parse-float ""                 :or nil )))
    (is (= 0                        (cool/parse-float "xyz"              :or 0   )))
    (is (= 0.5                      (cool/parse-float "0.5"              :or nil )))
    (is (> 1e-7 (error-ratio        (cool/parse-float "0.1"              :or 0) 
                                                      0.1 )))
    (is (> 1e-7 (error-ratio        (cool/parse-float "3.141592654"      :or 0) 
                                                      3.141592654 ))) 
  ))

(deftest parse-double
  (testing "basic"
    (is (= 15.0                            (cool/parse-double "15")))
    (is (= -5.0                            (cool/parse-double "-5")))
    (is (thrown? NumberFormatException     (cool/parse-double "")))
    (is (thrown? NumberFormatException     (cool/parse-double "xyz")))
    (is (= 0.5                             (cool/parse-double "0.5")))
    (is (> 1e-9 (error-ratio               (cool/parse-double "0.1") (double (/ 1 10) ))))
    (is (> 1e-9 (error-ratio               (cool/parse-double "3.141592654") Math/PI   ))) )

  (testing "with :or"
    (is (= 15.0                     (cool/parse-double "15"              :or nil )))
    (is (= -5.0                     (cool/parse-double "-5"              :or nil )))
    (is (= nil                      (cool/parse-double ""                :or nil )))
    (is (= 0                        (cool/parse-double "xyz"             :or 0   )))
    (is (= 0.5                      (cool/parse-double "0.5"             :or nil )))
    (is (> 1e-9 (error-ratio        (cool/parse-double "0.1" :or 0) (double (/ 1 10) ))))
    (is (> 1e-9 (error-ratio        (cool/parse-double "3.141592654" :or 0) Math/PI   ))) 
  ))

