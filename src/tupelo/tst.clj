(ns tupelo.tst
  (:refer-clojure :exclude [update partition])
  (:require [datomic.api      :as d]
            [tupelo.core      :refer [truthy? safe-> it-> spy spyx spyxx grab]]
            [tupelo.schema    :as ts]
            [schema.core      :as s] )
  (:use   clojure.pprint)
  (:import [java.util HashSet] )
  (:gen-class))

(defmacro add [& args]
  `(apply + [ ~@args ] ))
  
(println (macroexpand-1 '(add 1 2 3) ))
(println                 (add 1 2 3) )

(defmacro go [& args]
  `(add ~@args))
(println "go" (go 1 2))

(defn -main []
  (println "-main"))


