(ns tupelo.math
  (:require
    [tupelo.core :as t]
    [schema.core :as s]))

#?(:clj
   (do

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

     ))
