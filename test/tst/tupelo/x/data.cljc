(ns tst.tupelo.x.data
  (:use tupelo.x.data
        tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [tupelo.misc :as tm]
    [tupelo.schema :as tsk]))
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

; #todo avoid self-cycles
; #todo avoid descendant-cycles

(s/defn add-entity
  [hid :- tm/HID
   entity           ; :- tsk/Map
   ]
  (spyx [:adding hid entity])
  (set! *destruct* (glue *destruct* {hid entity})))

;-----------------------------------------------------------------------------
(defrecord MapRef [hid])
(defrecord VecRef [hid])

(defrecord MapEntry   [k v])
(defrecord VecElement [i v])
(defrecord MapEntity [entry-hids])
(defrecord VecEntity [element-hids])
(defrecord Value [value])

(defprotocol Loader
  (load-it [data]))

(extend-type clojure.lang.IPersistentMap
  Loader (load-it [data]
           (spyx-pretty [:map data])
           (with-spy-indent
             (let [entry-hids (forv [[k v] data]
                             (let [v2        (load-it v)
                                   map-entry (->MapEntry k v2)
                                   hid       (tm/new-hid)]
                               (add-entity hid (spyx map-entry))
                               hid))
                   map-entity (->MapEntity entry-hids)
                   hid (tm/new-hid) ]
               (add-entity hid map-entity)
               hid))))

(extend-type java.lang.Object
  Loader (load-it [data]
           (spyx-pretty [:obj  data ])
           (let [retval (->Value data)]
             retval)))

(dotest
  (with-destruct (new-destruct)
    (spy \newline "-----------------------------------------------------------------------------")
    (let [ctx       {:path [] :vals {}}
          data-1    {:a 1 :b {:x 11} :c [31 32]}
          pattern-1 '{:a ?v :b {:x 11}}
          ]
      (load-it data-1))
    (nl)
    (spyx-pretty *destruct*)
  ))

