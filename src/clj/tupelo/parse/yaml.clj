(ns tupelo.parse.yaml
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    )
  (:import ; #todo maybe switch to jackson for yaml ???
    [org.snakeyaml.engine.v2.api LoadSettings DumpSettings Load Dump ]))

;-----------------------------------------------------------------------------

(def ^:private load-settings (-> (LoadSettings/builder)
                               (.build)))

(def ^:private yaml-load (Load. load-settings))

(s/defn parse-all-raw
  "Parses a String containing multiple YAML objects, returning a vector of normalized Clojure data structure."
  [str-in :- s/Str]
  (unlazy (.loadAllFromString yaml-load (s/validate s/Str str-in))))

(s/defn parse-all
  "Parses a String containing multiple YAML objects, returning a vector of normalized Clojure data structure (with keywordized map keys)."
  [str-in :- s/Str]
  (walk/keywordize-keys
    (parse-all-raw (s/validate s/Str str-in))))

(s/defn parse-raw
  "Parses a String containing a single YAML object, returning a normalized Clojure data structure."
  [str-in :- s/Str]
  (unlazy (.loadFromString yaml-load (s/validate s/Str str-in))))

(s/defn parse
  "Parses a String containing a single YAML object, returning a normalized Clojure data structure (with keywordized map keys)."
  [str-in :- s/Str]
  (walk/keywordize-keys
    (parse-raw (s/validate s/Str str-in))))

;-----------------------------------------------------------------------------
(def ^:private dump-settings
  (-> (DumpSettings/builder)
    (.build)))

(def ^:private yaml-dump
  (Dump. dump-settings))

(s/defn edn->yaml :- s/Str
  "Serializes a Clojure EDN data structure into a YAML string."
  [it]
  (.dumpToString yaml-dump
    (walk/stringify-keys it)))

