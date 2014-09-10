;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure.  
            Utils for parsing string values."
      :author "Alan Thompson"}
  cooljure.parse
  (:require [cooljure.core :refer :all] ))

; #awt TODO:  convert args from [str-val & opts] -> [str-val & {:as opts} ]

(defn parseByte
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.Byte/parseByte.  Parses the string str-val into a byte.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (Byte/parseByte str-val)
      (with-exception-default or-val (Byte/parseByte str-val)) )))

(defn parseShort
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.Short/parseShort.  Parses the string str-val into a short.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (Short/parseShort str-val)
      (with-exception-default or-val (Short/parseShort str-val)) )))

(defn parseInt
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.Integer/parseInt  Parses the string str-val into a integer.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (Integer/parseInt str-val)
      (with-exception-default or-val (Integer/parseInt str-val)) )))

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

(defn parseFloat
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.Float/parseFloat.  Parses the string str-val into a float.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (Float/parseFloat str-val)
      (with-exception-default or-val (Float/parseFloat str-val)) )))

(defn parseDouble
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.Double/parseDouble.  Parses the string str-val into a double.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (Double/parseDouble str-val)
      (with-exception-default or-val (Double/parseDouble str-val)) )))

#_(defn parseXXXX
 "( [str-val]
    [str-val :or default-val] )
  A thin wrapper around java.lang.XXXX/parseXXXX.  Parses the string str-val into a xxxx.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        or-val      (get opts-map :or ::none) ]
    (if (= or-val ::none)
      (XXXX/parseXXXX str-val)
      (with-exception-default or-val (XXXX/parseXXXX str-val)) )))

; #awt TODO:  finish other parse* functions

