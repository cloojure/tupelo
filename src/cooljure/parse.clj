;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure.  
            Utils for reading CSV (comma-separated-value) formatted files."
      :author "Alan Thompson"}
  cooljure.parse
  (:require [cooljure.core :refer :all] ))

(defn parseLong
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.Long/parseLong.  Parses the string str-val into a long.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (Long/parseLong str-val)
      (with-exception-default or-val (Long/parseLong str-val)) )))

