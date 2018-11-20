(ns tst.tupelo.x.data-1
  (:use tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as i]))

;(spyx (supers (type []))) ; => clojure.lang.IPersistentVector
;(spyx (supers (type {}))) ; => clojure.lang.IPersistentMap
;(spyx (supers (type #{}))) ; => clojure.lang.IPersistentSet

(defprotocol Walker
  (walk [this]))

(extend-type clojure.lang.IPersistentMap
  Walker (walk [it]
           (i/spy [:map it])
           (i/with-spy-indent
             (doseq [[k v] it]
               (i/spy [:key k])
               (walk v)))))

(extend-type clojure.lang.IPersistentVector
  Walker (walk [it]
           (i/spy [:vec it])
           (i/with-spy-indent
             (doseq [[i v] (i/indexed it)]
               (i/spy [:i i])
               (walk v)))))

(extend-type clojure.lang.IPersistentSet
  Walker (walk [it]
           (i/spy [:set it])
           (i/with-spy-indent
             (doseq [k it]
               (i/spy [:key k])
               (walk k)))))

(extend-type java.lang.Object
  Walker (walk [it]
           (i/with-spy-indent
             (i/spyx it)
           )))

(dotest
  (binding [i/*spy-enabled* false]
    (println \newline "-----------------------------------------------------------------------------")
    (let [d1 {:a 1 :b {:x 11} :c [31 32] :d #{41 42}}]
      (walk d1))))

