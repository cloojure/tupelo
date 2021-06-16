(ns tupelo.dev.csv2
  "Utils for reading CSV (comma-separated-value) formatted files."
  (:use tupelo.core)
  (:require
    [clojure.data.csv :as csv]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
  (:import
    [java.io Reader StringReader StringWriter]))

(s/defn ^:no-doc verified-keys :- [s/Any]
  "Verifies that each entity has an identical keyset. Returns a sorted vector of keys."
  [entities :- [tsk/Map]]
  (let [keyset (into #{} (keys (xfirst entities)))]
    (doseq [entity entities]
      (assert
        (= keyset (set (keys entity)))
        "All entities must have identical keys"))
    (vec keyset)))

(s/defn entities->csv-force-quote :- s/Str
  "Writes a sequence of EDN maps to a multi-line CSV string.  Keys are output in
   sorted order.  Optionally accepts a map-key conversion function"
  [entities :- [tsk/Map]]
  (let [keys-sorted     (vec (sort (verified-keys entities)))
        hdr-vec         (forv [curr-key keys-sorted]
                          curr-key)
        data-vecs       (forv [entity entities]
                          (forv [curr-key keys-sorted]
                            (str (grab curr-key entity)))) ; coerce all to string for output to CSV
        string-table-2d (prepend hdr-vec data-vecs)
        string-writer   (StringWriter.)
        >>              (apply csv/write-csv string-writer string-table-2d [:quote? (constantly true)])
        result          (.toString string-writer)
        ]
    result))

