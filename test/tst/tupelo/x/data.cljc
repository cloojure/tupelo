(ns tst.tupelo.x.data
  (:use tupelo.x.data
        tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.impl :as i]))
(t/refer-tupelo :dev)

; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(def ^:dynamic *destruct* nil)

(defn validate-destruct []
  (when-not (map? *destruct*)
    (throw (IllegalArgumentException. (str "validate-destruct: failed destruct=" *destruct*)))))

(defmacro with-destruct ; #todo swap names?
  [destruct-arg & forms]
  `(binding [*destruct* ~destruct-arg]
     (validate-destruct)
     ~@forms ))

; HID & :hid are shorthand for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Node }
(defn new-destruct
  "Returns a new, empty destruct."
  []
  {})

;-----------------------------------------------------------------------------
(defrecord MapEntity [hid])
(defrecord VecEntity [hid])
(defprotocol Finder
  (find-it [data ctx pattern]))

(extend-type clojure.lang.IPersistentMap
  Finder (find-it [data ctx pattern]
           (spyx-pretty [:map ctx data pattern])
           (with-spy-indent
             (doseq [[dk dv] data]
               (spy [:key dk])
               (find-it dv
                 (update-in ctx [:path] append dk)
                 dv)))))

(extend-type java.lang.Object
  Finder (find-it [data ctx pattern]
           (spyx-pretty [:obj ctx data pattern])
         ))

(dotest
  (spy \newline "-----------------------------------------------------------------------------")
  (let [ctx       {:path []  :vals {}}
        data-1    {:a 1 :b {:x 11} :c [31 32]}
        pattern-1 '{:a ?v :b {:x 11}}
        ]
    (find-it data-1 ctx pattern-1)))

