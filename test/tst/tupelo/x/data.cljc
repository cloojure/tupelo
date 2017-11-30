(ns tst.tupelo.x.data
  (:use tupelo.x.data
        tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [tupelo.misc :as tm :refer [HID]]
    [tupelo.string :as tstr]
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
     ~@forms))

; HID & :hid are shorthand for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Node }
(defn new-destruct
  "Returns a new, empty destruct."
  []
  {})

(defn print-destruct [destruct]
  (println "-----------------------------------------------------------------------------")
  (doseq [[hid val] (glue (sorted-map) destruct)]
    (println "  " hid (tstr/indent 4 (pr-str val))))
  (println "-----------------------------------------------------------------------------"))

; #todo avoid self-cycles
; #todo avoid descendant-cycles

; #todo maybe create a record type HID to wrap hid keyword values for type dispatch

(s/defn new-hid :- HID ; #todo ***** temp for testing only! *****
  []
  (->> (tm/sha-uuid)
    (clip-str 8)
    (keyword)))

(s/defn add-entity
  [entity :- tsk/Map]
  ;(spyx [:adding hid entity]) (flush)
  (let [hid (new-hid)]
    (set! *destruct* (glue *destruct* {hid entity}))
    hid))

(s/defn get-entity :- tsk/Map
  [hid :- HID]
  (grab hid *destruct*))

;-----------------------------------------------------------------------------
(defrecord MapRef [hid]) ; todo needed?
(defrecord VecRef [hid]) ; todo needed?

(defrecord MapEntry     [key val-hid])
(defrecord VecElement   [idx val-hid])
(defrecord MapEntity    [entry-hids])
(defrecord VecEntity    [element-hids])
(defrecord Value        [value])

;-----------------------------------------------------------------------------
(defprotocol Edn->Destruct
  (edn->destruct [data]))

(extend-type clojure.lang.IPersistentMap
  Edn->Destruct (edn->destruct [data]
                  ;(spyx-pretty [:map data])
                  (with-spy-indent
                    (let [entry-hids (forv [[k v] data]
                                       (let [; >>        (spyx [k v])
                                             v2        (edn->destruct v)
                                             map-entry (->MapEntry k v2)
                                             hid       (add-entity map-entry)]
                                         hid))
                          map-entity (->MapEntity entry-hids)
                          hid        (add-entity map-entity)]
                      hid))))

(extend-type clojure.lang.IPersistentVector ; #todo add for Set
  Edn->Destruct (edn->destruct [data]
                  ; (spyx-pretty [:vec data])
                  (with-spy-indent
                    (let [entry-hids (forv [[idx v] (indexed data)]
                                       (let [; >>       (spyx [idx v])
                                             v2       (edn->destruct v)
                                             vec-elem (->VecElement idx v2)
                                             hid      (add-entity vec-elem)]
                                         hid))
                          vec-entity (->VecEntity entry-hids)
                          hid        (add-entity vec-entity)]
                      hid))))

(extend-type java.lang.Object
  Edn->Destruct (edn->destruct [data]
                  ; (spyx-pretty [:obj data])
                  (let [value (->Value data)
                        hid   (add-entity value)]
                    hid)))

;-----------------------------------------------------------------------------
(defprotocol Destruct->Edn
  (destruct->edn [hid]))

(extend-type clojure.lang.Keyword
  Destruct->Edn (destruct->edn [it]
                  (destruct->edn (get-entity it))))
(extend-type MapEntry
  Destruct->Edn (destruct->edn [map-entry]
                  ;(spyx-pretty map-entry)
                  {(grab :key map-entry)
                   (grab :val-hid map-entry)}))
(extend-type VecElement
  Destruct->Edn (destruct->edn [vec-elem]
                  ;(spyx-pretty vec-elem)
                  [(grab :idx vec-elem)
                   (grab :val-hid vec-elem)]))
(extend-type Value
  Destruct->Edn (destruct->edn [value]
                  ;(spyx-pretty value)
                  (grab :value value)))
(extend-type MapEntity
  Destruct->Edn (destruct->edn [map-entity]
                  ;(spyx-pretty [map-entity])
                  (with-spy-indent
                    (let [map-entry-tuples (forv [hid (grab :entry-hids map-entity)]
                                             (let [map-entry-destruct (get-entity hid)
                                                   map-entry-tuple    [(grab :key map-entry-destruct)
                                                                       (destruct->edn
                                                                         (grab :val-hid map-entry-destruct))]]
                                               map-entry-tuple))
                          map-result       (into {} map-entry-tuples)]
                      map-result))))
(extend-type VecEntity
  Destruct->Edn (destruct->edn [vec-entity]
                  ;(spyx-pretty [vec-entity])
                  (with-spy-indent
                    (let [vec-elem-tuples        (forv [hid (grab :element-hids vec-entity)]
                                                   (let [vec-elem-destruct (get-entity hid)
                                                         vec-elem-tuple    [(grab :idx vec-elem-destruct)
                                                                            (destruct->edn
                                                                              (grab :val-hid vec-elem-destruct))]]
                                                     vec-elem-tuple))
                          vec-elem-tuples-sorted (sort-by first vec-elem-tuples)
                          vec-result             (mapv second vec-elem-tuples-sorted) ]
                          vec-result))))

(dotest
  (with-destruct (new-destruct)
    (let [ctx       {:path [] :vals {}}
          data-1    {:a 1 :b {:x 11} :c [31 32]}
          pattern-1 '{:a ?v :b {:x 11}}
          root-hid (edn->destruct data-1)
          ]
      (nl) (print-destruct *destruct*)
      (nl) (spyx (destruct->edn root-hid)))

  ))

