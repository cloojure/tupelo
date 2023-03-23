;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.types.coerce
  (:refer-clojure :exclude [parse-long parse-double])
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    ))

#?(:clj
   (do
     (s/defn ->byte :- Byte
       "Coerce a value to Byte "
       [arg :- s/Any]
       (cond
         (string? arg) (Byte/parseByte (str/trim arg))
         (or (number? arg)) (byte arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (s/defn ->short :- Short
       "Coerce a value to Short "
       [arg :- s/Any]
       (cond
         (string? arg) (Short/parseShort (str/trim arg))
         (or (number? arg)) (short arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (s/defn ->int :- Integer
       "Coerce a value to Integer "
       [arg :- s/Any]
       (cond
         (string? arg) (Integer/parseInt (str/trim arg))
         (or (number? arg)) (int arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (s/defn ->long :- Long
       "Coerce a value to long "
       [arg :- s/Any]
       (cond
         (string? arg) (Long/parseLong (str/trim arg))
         (or (number? arg)) (long arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (s/defn ->float :- Float
       "Coerce a value to Float "
       [arg :- s/Any]
       (cond
         (string? arg) (Float/parseFloat (str/trim arg))
         (or (number? arg)) (float arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (s/defn ->double :- Double
       "Coerce a value to double "
       [arg :- s/Any]
       (cond
         (string? arg) (Double/parseDouble (str/trim arg))
         (or (number? arg)) (double arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     )) ; clj

;#?(:cljs
;   (do
;       ; These regexes are from the internet and aren't guarenteed to be perfect
;       (def regex-int
;         "Nominal regex for signed/unsigned integers"
;         #"^([+-]?[1-9]\d*|0)$")
;       (def regex-float
;         "Nominal regex for signed/unsigned floating-point numbers (possibly in scientific notation)"
;         #"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?")
;
;       (s/defn parse-int :- s/Int ; #todo => tupelo.cljs.parse
;         "
;               (parse-int str-val)
;               (parse-int str-val :default default-val )
;
;          A thin wrapper around js/parseInt  Parses the string str-val into a integer.
;          If the optional default-val is specified, it will be returned in the event of an
;          Nan."
;         ([str-val :- s/Str]
;          (t/cond-it-> (str/trim str-val)
;            (not (re-matches regex-int it)) (throw (ex-info "parse-int: could not parse input value #1"
;                                                     (t/vals->map str-val)))
;            true (js/parseInt it)
;            (js/isNaN it) (throw (ex-info "parse-int: could not parse input value #2"
;                                   (t/vals->map str-val)))))
;         ([str-val default-val]
;          (t/with-exception-default default-val
;            (parse-int str-val))))
;
;       (s/defn parse-float :- s/Num
;         "
;               (parse-float str-val)
;               (parse-float str-val :default default-val )
;
;          A thin wrapper around js/parseFloat.  Parses the string str-val into a float.
;          If the optional default-val is specified, it will be returned in the event of an
;          NaN."
;         ([str-val :- s/Str]
;          (t/cond-it-> (str/trim str-val)
;            (not (re-matches regex-float it)) (throw (ex-info "parse-float: could not parse input value #1"
;                                                       (t/vals->map str-val)))
;            true (js/parseFloat it)
;            (js/isNaN it) (throw (ex-info "parse-float: could not parse input value #2"
;                                   (t/vals->map str-val)))))
;         ([str-val default-val]
;          (t/with-exception-default default-val
;            (parse-float str-val))))
;
;       ))

