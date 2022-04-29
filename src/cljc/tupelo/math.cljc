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
            [tupelo.schema :as tsk]
            [tupelo.string :as str]
            )
  #?(:clj
     (:import
       [java.math RoundingMode]))
  )

(s/defn factorial :- s/Int
  "Computes the factorial of N"
  [n :- s/Int]
  (when (or (neg? n)
          (not (int? n)))
    (throw (ex-info "factorial: N must be a non-negative integer=" (vals->map n))))
  (if (zero? n)
    1
    (apply * (thru 1 n))))

(defn round-N
  "Round a floating point number to N decimal places, returning a double.

        (round-decimals 3.14156  2) => 3.14
        (round-decimals 1234567 -2) => 1234500
  "
  [val N]
  (let [factor (Math/pow 10.0 (double N))]
    (it-> (double val)
      (* it factor)
      (Math/round it)
      (/ it factor))))

#?(:clj
   (do
     (s/defn ->bigdec-N :- BigDecimal
       "Coerces a numeric value to a BigDecimal with N decimal digits. Also accepts
       a numeric value encoded as a String."
       [val :- (s/cond-pre s/Num s/Str)
        N :- s/Int]
       (it-> val
         (bigdec it)
         (.setScale
           ^BigDecimal it ; need type hint to avoid IDEA deprecation warning
           N
           RoundingMode/HALF_UP))) ; must include RoundingMode arg!!!

     (s/defn ->bigdec-2 :- BigDecimal
       "Coerces a numeric value to a BigDecimal with 2 decimal digits. Also accepts
       a numeric value encoded as a String."
       [val :- (s/cond-pre s/Num s/Str)]
       (->bigdec-N val 2))

     ;---------------------------------------------------------------------------------------------------
     (def ^:no-doc ln-2 (Math/log 2.0))
     (defn log2 ; #todo need test
       "Returns the log base-2 of x"
       [x]
       (it-> (Math/log (double x))
         (/ it ln-2)))

     ; #todo OBE via clojure.core/biginteger ???
     (s/defn ->BigInteger :- BigInteger
       "Converts a numeric value into a (positive) BigInteger (with truncation for Double and BigDecimal)"
       [arg :- s/Num]
       (assert (t/nonneg? arg))
       (cond
         (t/biginteger? arg) arg ; noop
         (int? arg) (BigInteger/valueOf (long arg)) ; any integer type, coerce to long
         (float? arg) (recur (bigdec arg)) ; any floating-point (Float & Double)
         (t/bigint? arg) (recur (bigdec arg)) ; clojure.core/BigInt <> java.math.BigInteger
         (t/bigdecimal? arg) (.toBigInteger ^BigDecimal arg) ; *** truncation ***
         :else (let [arg-type (type arg)]
                 (throw (ex-info "invalid arg type" (vals->map arg arg-type))))))

     (s/defn pow-BigInteger :- BigInteger
       "An BigInteger version of java.Math/pow( base, exp )"
       [base :- s/Int
        exp :- s/Int]
       (when-not (int? base) (throw (ex-info "base must be an integer" (vals->map base))))
       (when-not (int? exp) (throw (ex-info "exp must be an integer" (vals->map exp))))
       (when-not (t/nonneg? base) (throw (ex-info "base must be nonneg" (vals->map base))))
       (when-not (t/nonneg? exp) (throw (ex-info "exp must be nonneg" (vals->map exp))))
       (.pow ^BigInteger (->BigInteger base) exp))

     (s/defn pow-long :- s/Int
       "An Long (integer) version of java.Math/pow( base, exp )"
       [base :- s/Int
        exp :- s/Int]
       (let [result-bi (pow-BigInteger base exp)
             >>        (when (< Long/MAX_VALUE result-bi)
                         (throw (ex-info "Long overflow" (vals->map result-bi))))
             result-long    (.longValueExact result-bi)]
         result-long))

     (s/defn BigInteger->binary-str :- s/Str
       "Converts a (positive) BigInteger into a binary String"
       [bi :- BigInteger]
       (assert (t/nonneg? bi))
       (.toString ^BigInteger bi 2))

     (s/defn binary-str->BigInteger :- BigInteger
       "Converts a binary char sequence into a (positive) BigInteger"
       [bin-str :- s/Str]
       (let [result (BigInteger. ^String bin-str 2)]
         (assert (t/nonneg? result))
         result))

     (s/defn BigInteger->binary-chars :- [Character]
       "Converts a (positive) BigInteger into a binary char sequence"
       [bi :- BigInteger] (vec (BigInteger->binary-str bi)))

     (s/defn binary-chars->BigInteger :- BigInteger
       "Converts a binary char sequence into a (positive) BigInteger"
       [bin-chars :- [Character]] (binary-str->BigInteger (str/join bin-chars)))

     (s/defn BigInteger->hex-str :- s/Str
       "Converts a (positive) BigInteger into a hex string of `min-width` chars"
       [bi :- BigInteger
        min-width :- s/Int] ; #todo test min-width & all
       (assert (t/biginteger? bi))
       (assert (t/nonneg? bi))
       (let [hex-chars       (vec (.toString ^BigInteger bi 16))
             hex-chars-num   (count hex-chars)
             chars-needed    (max 0 (- min-width hex-chars-num)) ; soft overflow
             hex-chars-final (glue (repeat chars-needed \0) hex-chars)
             result          (str/join hex-chars-final)]
         (assert (<= min-width (count hex-chars-final)))
         result))


     ))

;---------------------------------------------------------------------------------------------------
(comment            ; #todo #awt complete/fix this stuff

  (def ^:dynamic UNIT
    "The unit magnitued & type used for round, floor, ceil, etc"
    (double 1))

  (defmacro with-units
    "Re-define the default UNIT to the specified magnitude & type for the enclosed operations"
    [u & forms]
    `(binding [UNIT u]
       ~@forms))

  #_(s/defn unit-type []
      (cond =)
      )

  (s/defn floor :- s/Num
    [x :- s/Num]
    )

  )
