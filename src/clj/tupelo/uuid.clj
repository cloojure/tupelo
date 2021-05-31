(ns tupelo.uuid
  (:use tupelo.core)
  (:require
    [clojure.core :exclude [rand]]
    [clj-uuid :as uuid]
    [schema.core :as s]
    [tupelo.schema :as tsk]))

(def null-str "00000000-0000-0000-0000-000000000000")
(def null (constantly null-str))

(def dummy-str "cafebabe-0867-5309-0666-0123456789ff")
(def dummy (constantly dummy-str))

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

