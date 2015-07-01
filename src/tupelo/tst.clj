(ns tupelo.tst
  (:refer-clojure :exclude [update partition])
  (:require [datomic.api      :as d]
            [tupelo.core      :refer [truthy? safe-> it-> spy spyx spyxx grab]]
            [tupelo.schema    :as ts]
            [tupelo.datomic   :as td]
            [schema.core      :as s] )
  (:use   clojure.pprint)
  (:import [java.util HashSet] )
  (:gen-class))


(spyx (td/contains-pull? :find '[xx (pull [*]) ?y ] ))

(defn -main []
  (println "-main"))


