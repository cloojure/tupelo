(ns tupelo.uuid
  (:use tupelo.core)
  (:refer-clojure :exclude [rand])
  (:require
    [clj-uuid :as uuid]
    [clojure.core :exclude [rand]]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
  (:import
    [java.util UUID]))

(def null-str "00000000-0000-0000-0000-000000000000")
(def null (constantly null-str))

(def dummy-str "cafebabe-0867-5309-0666-0123456789ff")
(def dummy (constantly dummy-str))

(def ^:no-doc uuid-regex-pattern
  #"(?x)            # expanded mode
  \p{XDigit}{8}     # 8 hex digits
  -                 # hyphen
  \p{XDigit}{4}     # 4 hex digits
  -                 # hyphen
  \p{XDigit}{4}     # 4 hex digits
  -                 # hyphen
  \p{XDigit}{4}     # 4 hex digits
  -                 # hyphen
  \p{XDigit}{12}    # 12 hex digits
  ")
(s/defn uuid-str? :- s/Bool
  "Returns true iff the string shows a valid UUID-like pattern of hex digits. Does not
  distinguish between UUID subtypes."
  [arg]
  (truthy?
    (when (string? arg)
      (re-matches uuid-regex-pattern arg))))

;-----------------------------------------------------------------------------
(s/defn rand :- UUID
  "Returns a random uuid as a String"
  [] (uuid/v4))

(s/defn rand-str :- s/Str
  "Returns a random uuid object"
  [] (str (tupelo.uuid/rand)))

;-----------------------------------------------------------------------------
(def ^:no-doc uuid-counter (atom nil)); uninitialized
(defn counted-reset! [] (reset! uuid-counter 0))
(counted-reset!); initialize

(defn counted-str []
  (let [cnt (swap-out! uuid-counter inc)
        uuid-str  (format "%08x-aaaa-bbbb-cccc-ddddeeeeffff" cnt)]
    uuid-str ))

(defn counted []
  (UUID/fromString (counted-str)))

;-----------------------------------------------------------------------------
(defmacro with-null
  "For testing, replace all calls to uuid/rand with uuid/null"
  [& forms]
  `(with-redefs [rand-str null]
     ~@forms))

(defmacro with-counted
  "For testing, replace all calls to uuid/rand with uuid/counted"
  [& forms]
  `(with-redefs [rand counted]
     (counted-reset!)
     ~@forms))

