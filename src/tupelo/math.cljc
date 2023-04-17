;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.math
  "Miscellaneous functions."
  ; We use the self-require trick to force separate compilation stages for macros
  ; See "ClojureScript Macro Tower & Loop" by Mike Fikes (2015-12-18)
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs
     (:require-macros
       [tupelo.misc :refer [with-dots]]))
  (:require
    [schema.core :as s]
    [tupelo.core :as t :refer [glue grab thru kw->str validate it-> spyx spyxx vals->map]]
    )
  #?(:clj
     (:import
       [java.math RoundingMode])))

;-----------------------------------------------------------------------------
(s/defn factorial :- s/Int
  "Computes the factorial of N"
  [n :- s/Int]
  (when (or (neg? n)
          (not (int? n)))
    (throw (ex-info "factorial: N must be a non-negative integer=" (vals->map n))))
  (if (zero? n)
    1
    (apply * (thru 1 n))))

(s/defn signum :- s/Int
  "Returns either -1, 0, or +1, to indicate the sign of the input. "
  [x :- s/Num]
  (cond
    (pos? x) +1
    (neg? x) -1
    :else 0))

(s/defn same-sign :- s/Bool
  "Returns `true` iff x and y have the same sign, or are both zero."
  [x :- s/Num
   y :- s/Num]
  (t/truthy?
    (or
      (and (pos? x) (pos? y))
      (and (neg? x) (neg? y))
      (and (zero? x) (zero? y)))))

#?(:clj
   (do
     ;-----------------------------------------------------------------------------
     ; #todo need BigInt version?
     (defn ceil->Long [x] (long (Math/ceil (double x))))
     (defn floor->Long [x] (long (Math/floor (double x))))
     (defn round->Long [x] (long (Math/round (double x))))
     (defn trunc->Long [x] (long (.longValue (double x))))

     ;---------------------------------------------------------------------------------------------------
     (s/defn pow->BigInteger :- BigInteger
       "An BigInteger version of java.Math/pow( base, exp )"
       [base :- s/Int
        exp :- s/Int]
       (when-not (int? base) (throw (ex-info "base must be an integer" (vals->map base))))
       (when-not (int? exp) (throw (ex-info "exp must be an integer" (vals->map exp))))
       (when-not (t/nonneg? base) (throw (ex-info "base must be nonneg" (vals->map base))))
       (when-not (t/nonneg? exp) (throw (ex-info "exp must be nonneg" (vals->map exp))))
       (.pow ^BigInteger (biginteger base) exp))

     (s/defn pow->Long :- s/Int
       "An Long (integer) version of java.Math/pow( base, exp )"
       [base :- s/Int
        exp :- s/Int]
       (let [result-bi   (pow->BigInteger base exp)
             >>          (when-not (<= Long/MIN_VALUE result-bi Long/MAX_VALUE)
                           (throw (ex-info "Long overflow" (vals->map base exp result-bi))))
             result-long (.longValueExact result-bi)]
         result-long))

     ;-----------------------------------------------------------------------------
     (s/defn ->bigdec-N :- BigDecimal
       "Coerces a numeric value to a BigDecimal with N decimal digits. Also accepts
       a numeric value encoded as a String."
       [val :- (s/cond-pre s/Num s/Str)
        N :- s/Int]
       (it-> val
         (bigdec it)
         (.setScale
           ^BigDecimal it ; need type hint to avoid IntelliJ/IDEA deprecation warning
           N
           RoundingMode/HALF_UP ; must include RoundingMode arg!!!
           )))

     (s/defn ->bigdec-2 :- BigDecimal
       "Coerces a numeric value to a BigDecimal with 2 decimal digits. Also accepts
       a numeric value encoded as a String."
       [val :- (s/cond-pre s/Num s/Str)]
       (->bigdec-N val 2))

     (s/defn round-N :- Double
       "Round a floating point number to N decimal places, returning a double.

             (round-decimals 3.14156  2) => 3.14
             (round-decimals 1234567 -2) => 1234500
       "
       [val :- s/Num
        N :- s/Int]
       (it-> val
         (->bigdec-N it N)
         (double it)))

     ;---------------------------------------------------------------------------------------------------
     (def ^:no-doc ln-2 (Math/log 2.0))
     (s/defn log2 :- Double ; #todo need test
       "Returns the log base-2 of x"
       [x :- s/Num]
       (it-> (Math/log (double x))
         (/ it ln-2)))

     ))
