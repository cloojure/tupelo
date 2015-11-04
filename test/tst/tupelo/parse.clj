;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tst.tupelo.parse
  (:require [tupelo.parse             :as coolp]
            [clojure.test               :refer :all] 
            [tupelo.core              :refer [rel=]] )
  (:import   [java.lang.Math] ))

(deftest parse-byte
  (testing "basic"
    (is (= 15                               (coolp/parse-byte "15")))
    (is (= -5                               (coolp/parse-byte "-5")))
    (is (thrown? NumberFormatException      (coolp/parse-byte "999")))
    (is (thrown? NumberFormatException      (coolp/parse-byte " "))) )
  (testing "with :default"
    (is (= 15                               (coolp/parse-byte "15"             :default nil )))
    (is (= -5                               (coolp/parse-byte "-5"             :default nil )))
    (is (= nil                              (coolp/parse-byte "999"            :default nil )))
    (is (= nil                              (coolp/parse-byte ""               :default nil )))
    (is (= 0                                (coolp/parse-byte "xyz"            :default 0   ))) ))

(deftest parse-short
  (testing "basic"
    (is (= 15                               (coolp/parse-short "15")))
    (is (= -5                               (coolp/parse-short "-5")))
    (is (= 999                              (coolp/parse-short "999")))
    (is (thrown? NumberFormatException      (coolp/parse-short "99999")))
    (is (thrown? NumberFormatException      (coolp/parse-short" "))) )
  (testing "with :default"
    (is (= 15                               (coolp/parse-short "15"            :default nil )))
    (is (= -5                               (coolp/parse-short "-5"            :default nil )))
    (is (= 999                              (coolp/parse-short "999"           :default nil )))
    (is (= nil                              (coolp/parse-short "99999"         :default nil )))
    (is (= nil                              (coolp/parse-short ""              :default nil )))
    (is (= 0                                (coolp/parse-short "xyz"           :default 0   ))) ))

(deftest parse-int
  (testing "basic"
    (is (= 15                               (coolp/parse-int "15")))
    (is (= -5                               (coolp/parse-int "-5")))
    (is (= 99999                            (coolp/parse-int "99999")))
    (is (thrown? NumberFormatException      (coolp/parse-int "9876543210")))
    (is (thrown? NumberFormatException      (coolp/parse-int ""))) )
  (testing "with :default"
    (is (= 15                               (coolp/parse-int "15"              :default nil )))
    (is (= -5                               (coolp/parse-int "-5"              :default nil )))
    (is (= 99999                            (coolp/parse-int "99999"           :default nil )))
    (is (= nil                              (coolp/parse-int "9876543210"      :default nil )))
    (is (= nil                              (coolp/parse-int ""                :default nil )))
    (is (= 0                                (coolp/parse-int "xyz"             :default 0   ))) ))

(deftest parse-long
  (testing "basic"
    (is (= 15                               (coolp/parse-long "15")))
    (is (= -5                               (coolp/parse-long "-5")))
    (is (= 99999                            (coolp/parse-long "99999")))
    (is (= 9876543210                       (coolp/parse-long "9876543210")))
    (is (thrown? NumberFormatException      (coolp/parse-long "98765432109876543210")))
    (is (thrown? NumberFormatException      (coolp/parse-long ""))) )
  (testing "with :default"
    (is (= 15                               (coolp/parse-long "15"                     :default nil )))
    (is (= -5                               (coolp/parse-long "-5"                     :default nil )))
    (is (= 99999                            (coolp/parse-long "99999"                  :default nil )))
    (is (= 9876543210                       (coolp/parse-long "9876543210"             :default nil )))
    (is (= nil                              (coolp/parse-long "98765432109876543210"   :default nil )))
    (is (= nil                              (coolp/parse-long ""                       :default nil ))) 
    (is (= 0                                (coolp/parse-long "xyz"                    :default 0   ))) ))

(deftest parse-float
  (testing "basic"
    (is (= 15.0                             (coolp/parse-float "15")))
    (is (= -5.0                             (coolp/parse-float "-5")))
    (is (= 0.5                              (coolp/parse-float "0.5")))
    (is (rel=  0.1                          (coolp/parse-float "0.1")            :digits 7))
    (is (rel=  3.141592654                  (coolp/parse-float "3.141592654")    :digits 7))
    (is (thrown? NumberFormatException      (coolp/parse-float "")))
    (is (thrown? NumberFormatException      (coolp/parse-float "xyz")))

  (testing "with :default"
    (is (= 15.0                             (coolp/parse-float "15"               :default nil )))
    (is (= -5.0                             (coolp/parse-float "-5"               :default nil )))
    (is (= nil                              (coolp/parse-float ""                 :default nil )))
    (is (= 0                                (coolp/parse-float "xyz"              :default 0   )))
    (is (= 0.5                              (coolp/parse-float "0.5"              :default nil )))
    (is (rel=  (/ 1 10)                     (coolp/parse-float "0.1"              :default 0) :digits 7))
    (is (rel=  3.141592654                  (coolp/parse-float "3.141592654"      :default 0) :digits 7)))
  ))

(deftest parse-double
  (testing "basic"
    (is (= 15.0                             (coolp/parse-double "15")))
    (is (= -5.0                             (coolp/parse-double "-5")))
    (is (thrown? NumberFormatException      (coolp/parse-double "")))
    (is (thrown? NumberFormatException      (coolp/parse-double "xyz")))
    (is (= 0.5                              (coolp/parse-double "0.5")))
    (is (rel=  (double (/ 1 10) )           (coolp/parse-double "0.1")           :digits 9))
    (is (rel=  Math/PI                      (coolp/parse-double "3.141592654")   :digits 9))) 

  (testing "with :default"
    (is (= 15.0                             (coolp/parse-double "15"          :default nil )))
    (is (= -5.0                             (coolp/parse-double "-5"          :default nil )))
    (is (= nil                              (coolp/parse-double ""            :default nil )))
    (is (= 0                                (coolp/parse-double "xyz"         :default 0   )))
    (is (= 0.5                              (coolp/parse-double "0.5"         :default nil )))
    (is (rel= (/ 1 10)                      (coolp/parse-double "0.1"         :default 0)     :digits 9))
    (is (rel= Math/PI                       (coolp/parse-double "3.141592654" :default 0)     :digits 9))
  ))

