;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cooljure.misc-test
  (:require [clojure.contrib.seq    :as seq]
            [cooljure.misc          :refer :all]
            [cooljure.core          :refer :all]
            [clojure.test           :refer :all] ))

(deftest str->kw-tst
  (testing "basic usage"
    (is (= :abc-def-gh-qrs (str->kw "abc def*gh_qrs"))) ))

