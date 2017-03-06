;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.x
  "Experimental new code"
  (:use clojure.test tupelo.test tupelo.x)
  (:require
    [clojure.core.async :as ca :refer [go go-loop chan thread]]
    [clojure.pprint :refer [pprint]]
    [tupelo.core :as t :refer [lazy-cons]]
    [tupelo.async :as ta]
    ))
(t/refer-tupelo)

