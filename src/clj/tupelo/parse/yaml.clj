(ns tupelo.parse.yaml
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk])
  (:import
    [org.snakeyaml.engine.v1.api LoadSettingsBuilder Load]
    [org.snakeyaml.engine.v1.api DumpSettingsBuilder Dump]
    ))

;-----------------------------------------------------------------------------
(def load-settings (-> (LoadSettingsBuilder.)
                     ;(.setLabel "Custom user configuration")
                     (.build)))

(def yaml-load (Load. load-settings))

(defn load-all-yaml-from-string-raw [str-in]
  (unlazy (.loadAllFromString yaml-load str-in)))

(defn load-all-yaml-from-string [str-in]
  (walk/keywordize-keys
    (load-all-yaml-from-string-raw  str-in)))

(defn load-yaml-from-string-raw [str-in]
  (unlazy (.loadFromString yaml-load str-in)))

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

