;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.parse.coerce
  (:refer-clojure :exclude [parse-long parse-double])
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    ))

#?(:clj
   (do

     (s/defn ->long :- Long
       "Coerce a value to long "
       [arg :- s/Any]
       (cond
         (string? arg) (Long/parseLong (str/trim arg))
         (or  (number? arg)) (long arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (s/defn ->double :- Double
       "Coerce a value to double "
       [arg :- s/Any]
       (cond
         (string? arg) (Double/parseDouble (str/trim arg))
         (or  (number? arg)) (double arg)
         :else (throw (ex-info "invalid arg" {:arg arg}))))

     (comment
       (defn parse-byte
         "
               (parse-byte str-val )
               (parse-byte str-val :default default-val )

          A thin wrapper around java.lang.Byte/parseByte.  Parses the string str-val into a byte.
          If the optional default-val is specified, it will be returned in the event of an
          exception."
         [str-val & opts]
         {:pre [(string? str-val)]}
         (let [opts-map    (apply hash-map opts)
               default-val (get opts-map :default ::none)]
           (if (= default-val ::none)
             (Byte/parseByte str-val)
             (t/with-exception-default default-val (Byte/parseByte str-val)))))

       (defn parse-short
         "
               (parse-short str-val)
               (parse-short str-val :default default-val )

          A thin wrapper around java.lang.Short/parseShort.  Parses the string str-val into a short.
          If the optional default-val is specified, it will be returned in the event of an
          exception."
         [str-val & opts]
         {:pre [(string? str-val)]}
         (let [opts-map    (apply hash-map opts)
               default-val (get opts-map :default ::none)]
           (if (= default-val ::none)
             (Short/parseShort str-val)
             (t/with-exception-default default-val (Short/parseShort str-val)))))

       (defn parse-int
         "
               (parse-int str-val)
               (parse-int str-val :default default-val )

          A thin wrapper around java.lang.Integer/parseInt  Parses the string str-val into a integer.
          If the optional default-val is specified, it will be returned in the event of an
          exception."
         [str-val & opts]
         {:pre [(string? str-val)]}
         (let [opts-map    (apply hash-map opts)
               default-val (get opts-map :default ::none)]
           (if (= default-val ::none)
             (Integer/parseInt str-val)
             (t/with-exception-default default-val (Integer/parseInt str-val))))))

     (comment
       (defn parse-float
         "
               (parse-float str-val
               (parse-float str-val :default default-val )

          A thin wrapper around java.lang.Float/parseFloat.  Parses the string str-val into a float.
          If the optional default-val is specified, it will be returned in the event of an
          exception."
         [str-val & opts]
         {:pre [(string? str-val)]}
         (let [opts-map    (apply hash-map opts)
               default-val (get opts-map :default ::none)]
           (if (= default-val ::none)
             (Float/parseFloat str-val)
             (t/with-exception-default default-val (Float/parseFloat str-val)))))

       (def ^:no-doc unspecified ::unspecified) ; #todo make all use Var instead of ::none ???
       (s/defn parse-decimal
         "A thin wrapper around java.math/BigDecimal(String value).  Parses the string str-val into a BigDecimal.
          An optional default value may be specified in the opts map; is present, it will be returned in the event of an exception. "
         ([str-val :- s/Str] (parse-decimal {} str-val))
         ([opts :- tsk/KeyMap
           str-val :- s/Str]
          (let [default-val (get opts :default unspecified)]
            (if (= default-val unspecified)
              (BigDecimal. str-val)
              (t/with-exception-default default-val
                (BigDecimal. str-val)))))))

     #_(defn parse-xxxx
         "( [str-val]
            [str-val :default default-val] )
          A thin wrapper around java.lang.XXXX/parseXXXX.  Parses the string str-val into a xxxx.
          If the optional default-val is specified, it will be returned in the event of an
          exception."
         [str-val & opts]
         {:pre [(string? str-val)]}
         (let [opts-map    (apply hash-map opts)
               default-val (get opts-map :default ::none)]
           (if (= default-val ::none)
             (XXXX/parseXXXX str-val)
             (t/with-exception-default default-val (XXXX/parseXXXX str-val)))))

     ; #awt TODO:  finish other parse* functions

     ))     ; clj

#?(:cljs
   #_(do
     ; These regexes are from the internet and aren't guarenteed to be perfect
     (def regex-int
       "Nominal regex for signed/unsigned integers"
       #"^([+-]?[1-9]\d*|0)$")
     (def regex-float
       "Nominal regex for signed/unsigned floating-point numbers (possibly in scientific notation)"
       #"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?")

     (s/defn parse-int :- s/Int ; #todo => tupelo.cljs.parse
       "
             (parse-int str-val)
             (parse-int str-val :default default-val )

        A thin wrapper around js/parseInt  Parses the string str-val into a integer.
        If the optional default-val is specified, it will be returned in the event of an
        Nan."
       ([str-val :- s/Str]
        (t/cond-it-> (str/trim str-val)
          (not (re-matches regex-int it)) (throw (ex-info "parse-int: could not parse input value #1"
                                                   (t/vals->map str-val)))
          true (js/parseInt it)
          (js/isNaN it) (throw (ex-info "parse-int: could not parse input value #2"
                                 (t/vals->map str-val)))))
       ([str-val default-val]
        (t/with-exception-default default-val
          (parse-int str-val))))

     (s/defn parse-float :- s/Num
       "
             (parse-float str-val)
             (parse-float str-val :default default-val )

        A thin wrapper around js/parseFloat.  Parses the string str-val into a float.
        If the optional default-val is specified, it will be returned in the event of an
        NaN."
       ([str-val :- s/Str]
        (t/cond-it-> (str/trim str-val)
          (not (re-matches regex-float it)) (throw (ex-info "parse-float: could not parse input value #1"
                                                     (t/vals->map str-val)))
          true (js/parseFloat it)
          (js/isNaN it) (throw (ex-info "parse-float: could not parse input value #2"
                                 (t/vals->map str-val)))))
       ([str-val default-val]
        (t/with-exception-default default-val
          (parse-float str-val))))

     ))

