(ns tst.tupelo.x.data
  (:use tupelo.x.data
        tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.impl :as i]))
(t/refer-tupelo :dev)

;-----------------------------------------------------------------------------
(defprotocol Finder
  (find-it [data ctx pattern]))

(extend-type clojure.lang.IPersistentMap
  Finder (find-it [data ctx pattern]
           (println "----- map -----")
           (spyx-pretty [ctx data pattern])
           (with-spy-indent
             (map-let [[dk dv] data
                       [pk pv] pattern]
               (spy [:key dk pd])
               (find-it v
                 (update-in ctx [:path] append k)
                 v)))))

(extend-type java.lang.Object
  Finder (find-it [data ctx pattern]
           (println "----- obj -----")
           (spyx-pretty [ctx data pattern])
         ))

(dotest
  (spy \newline "-----------------------------------------------------------------------------")
  (let [ctx       {:path []  :vals {}}
        data-1    {:a 1 :b {:x 11}}
        pattern-1 '{:a ?v :b {:x 11}}
        ]
    (find-it data-1 ctx pattern-1)))

