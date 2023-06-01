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
  (:refer-clojure :exclude [parse-long parse-double])
  (:require
    [clojure.string :as str]
    [clojure.tools.reader.edn :as edn]
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    ))

; #todo:  write doc page
; #todo:  convert args from [str-val & opts] -> [str-val & {:as opts} ]

(defn edn-parsible
  "Traverses a data structure to ensure it can be serialized with `pr-str` and read via
  clojure.tools.reader.edn/read-string. All non-parsible content is replaced
  with `::non-parsible-object`. "
  [data]
  (let [edn-parse-roundtrip (fn [item]
                              (t/with-exception-default ::edn-non-parsible
                                (let [item-str (pr-str item)
                                      item-edn (edn/read-string item-str)]
                                  (t/assert-info  (= item item-edn) "edn-non-parsible" {:data data})
                                  item)))
        data-unlazy         (t/unlazy data) ; coerce to map/vector/string wherever possible
        data-parsible       (walk/postwalk edn-parse-roundtrip data-unlazy)]
    data-parsible))

#?(:clj
   (do

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
           (t/with-exception-default default-val (Integer/parseInt str-val)))))

     (defn parse-long
       "
             (parse-long str-val)
             (parse-long str-val :default default-val )

        A thin wrapper around java.lang.Long/parseLong.  Parses the string str-val into a long.
        If the optional default-val is specified, it will be returned in the event of an
        exception."
       [str-val & opts]
       {:pre [(string? str-val)]}
       (let [opts-map    (apply hash-map opts)
             default-val (get opts-map :default ::none)]
         (if (= default-val ::none)
           (Long/parseLong str-val)
           (t/with-exception-default default-val (Long/parseLong str-val)))))

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

     (defn parse-double
       "
             (parse-double str-val
             (parse-double str-val :default default-val )

        A thin wrapper around java.lang.Double/parseDouble.  Parses the string str-val into a double.
        If the optional default-val is specified, it will be returned in the event of an
        exception."
       [str-val & opts]
       {:pre [(string? str-val)]}
       (let [opts-map    (apply hash-map opts)
             default-val (get opts-map :default ::none)]
         (if (= default-val ::none)
           (Double/parseDouble str-val)
           (t/with-exception-default default-val (Double/parseDouble str-val)))))

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
              (BigDecimal. str-val))))))

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
   (do
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



