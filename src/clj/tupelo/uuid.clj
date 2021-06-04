(ns tupelo.uuid
  (:use tupelo.core)
  (:refer-clojure :exclude [rand])
  (:require
    [clojure.core :exclude [rand]]
    [clj-uuid :as uuid]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as str]
    ))

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

(s/defn rand :- s/Str
  "Returns a random uuid"
  [] (str (uuid/v4)))

(def ^:no-doc uuid-counter (atom nil)); uninitialized
(defn counted-reset!
  []
  (reset! uuid-counter (Long/parseLong "abcd0000" 16))); count in hex
(defn counted
  []
  (let [cnt (swap-out! uuid-counter inc)]
    (format "%08x-aaaa-bbbb-cccc-0123456789ff" cnt)))
(counted-reset!); initialize

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

