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

(deftest parseByte
  (testing "basic"
    (is (= 15                              (cool/parseByte "15")))
    (is (= -5                              (cool/parseByte "-5")))
    (is (thrown? NumberFormatException     (cool/parseByte "999")))
    (is (thrown? NumberFormatException     (cool/parseByte " "))) )
  (testing "with :or"
    (is (= 15      (cool/parseByte "15"                           :or nil )))
    (is (= -5      (cool/parseByte "-5"                           :or nil )))
    (is (= nil     (cool/parseByte "999"                          :or nil )))
    (is (= nil     (cool/parseByte ""                             :or nil )))
    (is (= 0       (cool/parseByte "xyz"                          :or 0   ))) ))

(deftest parseShort
  (testing "basic"
    (is (= 15                              (cool/parseShort "15")))
    (is (= -5                              (cool/parseShort "-5")))
    (is (= 999                             (cool/parseShort "999")))
    (is (thrown? NumberFormatException     (cool/parseShort "99999")))
    (is (thrown? NumberFormatException     (cool/parseShort" "))) )
  (testing "with :or"
    (is (= 15      (cool/parseShort "15"                          :or nil )))
    (is (= -5      (cool/parseShort "-5"                          :or nil )))
    (is (= 999     (cool/parseShort "999"                         :or nil )))
    (is (= nil     (cool/parseShort "99999"                       :or nil )))
    (is (= nil     (cool/parseShort ""                            :or nil )))
    (is (= 0       (cool/parseShort "xyz"                         :or 0   ))) ))

(deftest parseInt
  (testing "basic"
    (is (= 15                              (cool/parseInt "15")))
    (is (= -5                              (cool/parseInt "-5")))
    (is (= 99999                           (cool/parseInt "99999")))
    (is (thrown? NumberFormatException     (cool/parseInt "9876543210")))
    (is (thrown? NumberFormatException     (cool/parseInt ""))) )
  (testing "with :or"
    (is (= 15      (cool/parseInt "15"                            :or nil )))
    (is (= -5      (cool/parseInt "-5"                            :or nil )))
    (is (= 99999   (cool/parseInt "99999"                         :or nil )))
    (is (= nil     (cool/parseInt "9876543210"                    :or nil )))
    (is (= nil     (cool/parseInt ""                              :or nil )))
    (is (= 0       (cool/parseInt "xyz"                           :or 0   ))) ))

(deftest parseLong
  (testing "basic"
    (is (= 15                              (cool/parseLong "15")))
    (is (= -5                              (cool/parseLong "-5")))
    (is (= 99999                           (cool/parseLong "99999")))
    (is (= 9876543210                      (cool/parseLong "9876543210")))
    (is (thrown? NumberFormatException     (cool/parseLong "98765432109876543210")))
    (is (thrown? NumberFormatException     (cool/parseLong ""))) )
  (testing "with :or"
    (is (= 15           (cool/parseLong "15"                            :or nil )))
    (is (= -5           (cool/parseLong "-5"                            :or nil )))
    (is (= 99999        (cool/parseLong "99999"                         :or nil )))
    (is (= 9876543210   (cool/parseLong "9876543210"                    :or nil )))
    (is (= nil          (cool/parseLong "98765432109876543210"          :or nil )))
    (is (= nil          (cool/parseLong ""                              :or nil ))) 
    (is (= 0            (cool/parseLong "xyz"                           :or 0   ))) ))

(deftest parseFloat
  (testing "basic"
    (is (= 15.0                            (cool/parseFloat "15")))
    (is (= -5.0                            (cool/parseFloat "-5")))
    (is (thrown? NumberFormatException     (cool/parseFloat "")))
    (is (thrown? NumberFormatException     (cool/parseFloat "xyz")))
    (is (= 0.5                             (cool/parseFloat "0.5")))
    (is (> 1e-7 (error-ratio               (cool/parseFloat "0.1") 0.1 )))
    (is (> 1e-7 (error-ratio               (cool/parseFloat "3.141592654") 3.141592654 ))) )

  (testing "with :or"
    (is (= 15.0                     (cool/parseFloat "15"               :or nil )))
    (is (= -5.0                     (cool/parseFloat "-5"               :or nil )))
    (is (= nil                      (cool/parseFloat ""                 :or nil )))
    (is (= 0                        (cool/parseFloat "xyz"              :or 0   )))
    (is (= 0.5                      (cool/parseFloat "0.5"              :or nil )))
    (is (> 1e-7 (error-ratio        (cool/parseFloat "0.1"              :or 0) 
                                                      0.1 )))
    (is (> 1e-7 (error-ratio        (cool/parseFloat "3.141592654"      :or 0) 
                                                      3.141592654 ))) 
  ))

(deftest parseDouble
  (testing "basic"
    (is (= 15.0                            (cool/parseDouble "15")))
    (is (= -5.0                            (cool/parseDouble "-5")))
    (is (thrown? NumberFormatException     (cool/parseDouble "")))
    (is (thrown? NumberFormatException     (cool/parseDouble "xyz")))
    (is (= 0.5                             (cool/parseDouble "0.5")))
    (is (> 1e-9 (error-ratio               (cool/parseDouble "0.1") (double (/ 1 10) ))))
    (is (> 1e-9 (error-ratio               (cool/parseDouble "3.141592654") Math/PI   ))) )

  (testing "with :or"
    (is (= 15.0                     (cool/parseDouble "15"              :or nil )))
    (is (= -5.0                     (cool/parseDouble "-5"              :or nil )))
    (is (= nil                      (cool/parseDouble ""                :or nil )))
    (is (= 0                        (cool/parseDouble "xyz"             :or 0   )))
    (is (= 0.5                      (cool/parseDouble "0.5"             :or nil )))
    (is (> 1e-9 (error-ratio        (cool/parseDouble "0.1" :or 0) (double (/ 1 10) ))))
    (is (> 1e-9 (error-ratio        (cool/parseDouble "3.141592654" :or 0) Math/PI   ))) 
  ))

