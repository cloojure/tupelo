(ns tupelo.parse.yaml ; #todo maybe switch to jackson for yaml ???
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk]
    [schema.core :as s]
    )
  (:import
    ; from  https://bitbucket.org/asomov/snakeyaml-engine/src/default/
    [org.snakeyaml.engine.v2.api LoadSettings DumpSettings Load Dump ]
    [org.snakeyaml.engine.v2.common FlowStyle ScalarStyle] ))

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
  [yaml-str :- s/Str]
  (unlazy (.loadFromString yaml-load (s/validate s/Str yaml-str))))

(s/defn parse
  "Parses a String containing a single YAML object, returning a normalized Clojure data structure (with keywordized map keys)."
  [yaml-str :- s/Str]
  (walk/keywordize-keys
    (parse-raw (s/validate s/Str yaml-str))))

;-----------------------------------------------------------------------------
(def ^:private dump-settings
  (it-> (DumpSettings/builder)
   ;(.setDefaultScalarStyle it ScalarStyle/DOUBLE_QUOTED)
    (.setDefaultFlowStyle it FlowStyle/BLOCK)
    (.build it)))

(def ^:private yaml-dump
  (Dump. dump-settings))

(s/defn edn->yaml :- s/Str ; #todo:  rename =>  `data->yaml`  ???
  "Serializes a Clojure EDN data structure into a YAML string."
  [it]
  (.dumpToString yaml-dump
    (walk/stringify-keys it)))

