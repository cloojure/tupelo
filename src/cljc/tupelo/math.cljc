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
