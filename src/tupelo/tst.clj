(ns tupelo.tst
  (:refer-clojure :exclude [update partition])
  (:require [datomic.api      :as d]
            [tupelo.core      :refer [truthy? safe-> it-> spy spyx spyxx grab]]
            [tupelo.schema    :as ts]
            [schema.core      :as s] )
  (:use   clojure.pprint)
  (:import [java.util HashSet] )
  (:gen-class))



(defn -main []
  (println "-main"))


