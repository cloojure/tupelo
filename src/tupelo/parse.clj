;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.parse
 "Utils for parsing string values. Provides a thin Clojure wrapper around java native
  parsing functions such as java.lang.Float/parseFloat.  Unlike the original java
  functions, these native-Clojure functions can be used as higher-order functions in maps,
  function arguments, etc.  Each function also provides an optional default-value which
  will be returned if there is an exception during parsing."
  (:require [tupelo.core :refer :all] ))

; #todo:  write doc page
; #todo:  convert args from [str-val & opts] -> [str-val & {:as opts} ]

(defn parse-byte
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.Byte/parseByte.  Parses the string str-val into a byte.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (Byte/parseByte str-val)
      (with-exception-default default-val (Byte/parseByte str-val)) )))

(defn parse-short
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.Short/parseShort.  Parses the string str-val into a short.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (Short/parseShort str-val)
      (with-exception-default default-val (Short/parseShort str-val)) )))

(defn parse-int
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.Integer/parseInt  Parses the string str-val into a integer.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (Integer/parseInt str-val)
      (with-exception-default default-val (Integer/parseInt str-val)) )))

(defn parse-long
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.Long/parseLong.  Parses the string str-val into a long.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (Long/parseLong str-val)
      (with-exception-default default-val (Long/parseLong str-val)) )))

(defn parse-float
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.Float/parseFloat.  Parses the string str-val into a float.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (Float/parseFloat str-val)
      (with-exception-default default-val (Float/parseFloat str-val)) )))

(defn parse-double
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.Double/parseDouble.  Parses the string str-val into a double.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (Double/parseDouble str-val)
      (with-exception-default default-val (Double/parseDouble str-val)) )))

#_(defn parse-xxxx
 "( [str-val]
    [str-val :default default-val] )
  A thin wrapper around java.lang.XXXX/parseXXXX.  Parses the string str-val into a xxxx.
  If the optional default-val is specified, it will be returned in the event of an
  exception."
  [str-val & opts]
  {:pre [ (string? str-val) ] }
  (let [opts-map    (apply hash-map opts)
        default-val (get opts-map :default ::none) ]
    (if (= default-val ::none)
      (XXXX/parseXXXX str-val)
      (with-exception-default default-val (XXXX/parseXXXX str-val)) )))

; #awt TODO:  finish other parse* functions

