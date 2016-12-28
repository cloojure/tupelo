;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns ^:no-doc ^:deprecated tupelo.explicit
  "Functions that avoid ambiguity"
  (:refer-clojure :exclude [get get-in] )
  (:require [clojure.string   :as str]
            [clojure.core     :as clj]
            [schema.core      :as s] 
            [tupelo.core      :as tc] ))

; If necessary, can copy the following syntax to override clojure/core vars
;       (:refer-clojure :exclude [* - + == /])

(defn ^:no-doc ^:deprecated get 
  "A fail-fast version of clojure.core/get. For map m & key k, returns
  the value v associated with k in m.  Throws an exception if k is not
  present in m."
  [m k]
  (tc/fetch-in m [k]))

(defn ^:no-doc ^:deprecated get-in
  "A fail-fast version of clojure.core/get-in. For map m & keys ks,
  returns the value v associated with ks in m, as for (get-in m ks).
  Throws an exception if the path ks is not present in m."
  [m  ks]
  (tc/fetch-in m ks))

