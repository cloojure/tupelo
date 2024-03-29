(ns tupelo.uuid
  (:refer-clojure :exclude [rand])
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    [tupelo.core.impl :as impl])
  #?@(:clj
     [(:require
        [clj-uuid :as uuid])
      (:import
        [java.util UUID])]))

; #TODO add code & tests for cljs

(def const-null-str
  "A null UUID string '00000000-0000-0000-0000-000000000000' "
  "00000000-0000-0000-0000-000000000000")

(def const-dummy-str
  "A dummy UUID string '00000000-0000-0000-0000-000000000000' "
  "cafebabe-1953-0510-0970-0123456789ff")

#?(:clj
   (do
     (def const-null-obj
       "A null UUID object '00000000-0000-0000-0000-000000000000' "
       (UUID/fromString const-null-str))

     (def const-dummy-obj
       "A dummy UUID object '00000000-0000-0000-0000-000000000000' "
       (UUID/fromString const-dummy-str))
     ))

;-----------------------------------------------------------------------------
(def uuid-str? ; #todo add tests for cljc
  "Returns true iff the string shows a valid UUID-like pattern of hex digits. Does not
  distinguish between UUID subtypes."
  impl/uuid-str?)

(def null-str
  "Returns a null UUID string '00000000-0000-0000-0000-000000000000' "
  (t/const->fn const-null-str))

(def dummy-str
  "Returns a dummy UUID string 'cafebabe-1953-0510-0970-0123456789ff' "
  (t/const->fn const-dummy-str))

#?(:clj
   (do
     (def null
       "Returns a null UUID object '00000000-0000-0000-0000-000000000000' "
       (t/const->fn const-null-obj))

     (def dummy
       "Returns a dummy UUID object 'cafebabe-1953-0510-0970-0123456789ff'"
       (t/const->fn const-dummy-obj))

     (s/defn rand :- UUID
       "Returns a random uuid as a UUID object"
       [] (uuid/v4))

     (s/defn rand-str :- s/Str
       "Returns a random uuid as a String"
       [] (str (tupelo.uuid/rand)))

     ;-----------------------------------------------------------------------------
     (def ^:no-doc uuid-counter (atom nil)) ; uninitialized
     (defn counted-reset!
       "Resets the index for `counted-str` to zero."
       [] (reset! uuid-counter 0))
     (counted-reset!) ; initialize upon load

     (s/defn counted-str :- s/Str
       "Each call returns the next UUID string in a sequence like:

              00000000-aaaa-bbbb-cccc-ddddeeeeffff
              00000001-aaaa-bbbb-cccc-ddddeeeeffff
              00000002-aaaa-bbbb-cccc-ddddeeeeffff
              "
       []
       (let [cnt      (t/swap-out! uuid-counter inc)
             uuid-str (format "%08x-aaaa-bbbb-cccc-ddddeeeeffff" cnt)]
         (t/validate uuid-str? uuid-str)))

     #?(:clj
        (s/defn counted :- UUID
          "Returns a UUID object as the result of the next call to `counted-str`"
          []
          (UUID/fromString (counted-str))))

     ;-----------------------------------------------------------------------------
     (defmacro with-null
       "For testing, replace all calls to uuid/rand with uuid/null"
       [& forms]
       `(with-redefs [rand null]
          ~@forms))

     (defmacro with-counted
       "For testing, replace all calls to uuid/rand with uuid/counted"
       [& forms]
       `(with-redefs [rand counted]
          (counted-reset!)
          ~@forms))

     ))

