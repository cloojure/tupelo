(ns tupelo.parse.yaml
  (:use tupelo.core )
  (:require
    [clojure.walk :as walk] )
  (:import
    [org.snakeyaml.engine.v1.api LoadSettingsBuilder LoadSettings Load]
    [org.snakeyaml.engine.v1.api DumpSettingsBuilder DumpSettings Dump ]
    ))


; #todo merge => tupelo.core
(defn unlazy-2 ; #todo need tests & docs. Use for datomic Entity?
  "Converts a lazy collection to a concrete (eager) collection of the same type."
  [coll]
  (let [unlazy-item (fn [item]
                      (cond
                        (sequential? item) (vec item)
                        (map? item) (do
                                      (println :found-map item)
                                      (into {} item))
                        (set? item) (into #{} item)

                        (instance? java.io.InputStream item) (slurp item)  ; #todo need test
                        (instance? java.util.List item) (vec item)  ; #todo need test
                        (instance? java.util.Map item) (into {} item)  ; #todo need test
                        (instance? java.lang.Iterable item) (into [] item)  ; #todo need test

                        :else item))
        result    (walk/postwalk unlazy-item coll) ]
    result ))

;-----------------------------------------------------------------------------
(def load-settings (-> (LoadSettingsBuilder.)
                     ;(.setLabel "Custom user configuration")
                     (.build)))

(def yaml-load (Load. load-settings))

(defn load-all-yaml-from-string-raw [str-in]
  (unlazy-2 (.loadAllFromString yaml-load str-in)))

(defn load-all-yaml-from-string [str-in]
  (walk/keywordize-keys
    (load-all-yaml-from-string-raw  str-in)))

(defn load-yaml-from-string-raw [str-in]
  (unlazy-2 (.loadFromString yaml-load str-in)))

(defn load-yaml-from-string [str-in]
  (walk/keywordize-keys
    (load-yaml-from-string-raw  str-in)))

;-----------------------------------------------------------------------------
(def dump-settings (it-> (DumpSettingsBuilder.)
                     ;(.setDefaultScalarStyle it ScalarStyle.DOUBLE_QUOTED)
                     (.build it)))

(def yaml-dump (Dump. dump-settings))

(defn yaml-dump-to-string [it]
  (.dumpToString yaml-dump
    (walk/stringify-keys it)))

