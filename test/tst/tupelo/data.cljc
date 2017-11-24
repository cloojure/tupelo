(ns tst.tupelo.data
  (:use tupelo.data
        tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]))
(t/refer-tupelo :dev)

(spyx (supers (type []))) ; => clojure.lang.IPersistentVector
(spyx (supers (type {}))) ; => clojure.lang.IPersistentMap
(spyx (supers (type #{}))) ; => clojure.lang.IPersistentSet


(defprotocol Walker
  (walk [this]))

(extend-type clojure.lang.IPersistentMap
  Walker (walk [it]
           (spy [:map it])
           (with-spy-indent
             (doseq [[k v] it]
               (spy [:key k])
               (walk v)))))

(extend-type clojure.lang.IPersistentVector
  Walker (walk [it]
           (spy [:vec it])
           (with-spy-indent
             (doseq [[i v] (indexed it)]
               (spy [:i i])
               (walk v)))))

(extend-type clojure.lang.IPersistentSet
  Walker (walk [it]
           (spy [:set it])
           (with-spy-indent
             (doseq [k it]
               (spy [:key k])
               (walk k)))))

(extend-type java.lang.Object
  Walker (walk [it]
           (with-spy-indent
             (spyx it))))

(dotest
  (let [d1 {:a 1 :b {:x 11} :c [31 32] :d #{41 42}}
        ]
    (walk d1)

    ))
