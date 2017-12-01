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
;(defrecord MapRef [hid]) ; todo needed?
;(defrecord VecRef [hid]) ; todo needed?
;(defrecord MapEntry     [key val-hid])
;(defrecord VecElement   [idx val-hid])

(defrecord MapEntity    [map-entity])
(defrecord VecEntity    [vec-entity-sorted])
(defrecord Value        [value])

;-----------------------------------------------------------------------------
(defprotocol Destruct->Edn
  (destruct->edn [hid]))

(extend-type clojure.lang.Keyword
  Destruct->Edn (destruct->edn [it]
                  (destruct->edn (get-entity it))))
(extend-type Value
  Destruct->Edn (destruct->edn [value]
                  (grab :value value)))
(extend-type MapEntity
  Destruct->Edn (destruct->edn [map-entity]
                  (with-spy-indent
                    (let [map-entry-tuples (forv [[key val-hid] (grab :map-entity map-entity)]
                                             (let [map-entry-tuple [key (destruct->edn val-hid)]]
                                               map-entry-tuple))
                          map-result       (into {} map-entry-tuples)]
                      map-result))))
(extend-type VecEntity
  Destruct->Edn (destruct->edn [vec-entity-sorted]
                  (with-spy-indent
                    (let [vec-elems (forv [[idx val-hid] (grab :vec-entity-sorted vec-entity-sorted)]
                                      (let [vec-elem (destruct->edn val-hid)]
                                        vec-elem))]
                      vec-elems))))

;-----------------------------------------------------------------------------
(defprotocol Edn->Destruct
  (edn->destruct [data]))

(extend-type clojure.lang.IPersistentMap
  Edn->Destruct (edn->destruct [data]
                  (with-spy-indent
                    (let [map-entries (forv [[k v] data]
                                        (let [value-hid   (edn->destruct v)
                                              map-entry   {k value-hid}]
                                          map-entry))
                          map-entity (->MapEntity (apply glue map-entries))
                          hid        (add-entity map-entity)]
                      hid))))

(extend-type clojure.lang.IPersistentVector ; #todo add for Set
  Edn->Destruct (edn->destruct [data]
                  (with-spy-indent
                    (let [vec-entries (forv [[idx v] (indexed data)]
                                       (let [value-hid  (edn->destruct v)
                                             vec-entry  {idx value-hid} ]
                                         vec-entry))
                          vec-entity-sorted (->VecEntity (apply glue (sorted-map) vec-entries))
                          hid        (add-entity vec-entity-sorted)]
                      hid))))

(extend-type java.lang.Object
  Edn->Destruct (edn->destruct [data]
                  (let [value (->Value data)
                        hid   (add-entity value)]
                    hid)))


;(defprotocol Match
;  (match [ctx hid query]))
;(extend-type MapEntity
;  Match (match [ctx hid query]
;          (with-spy-indent
;            (let [entry-hids (forv [[k v] query]
;                               (let [>>        (spyx [k v])
;
;                                     v2        (edn->destruct v)
;                                     map-entry (->MapEntry k v2)
;                                     hid       (add-entity map-entry)]
;                                 hid))
;                  map-entity (->MapEntity entry-hids)
;                  hid        (add-entity map-entity)]
;              hid))))
;          ))

(dotest
  (with-destruct (new-destruct)
    (let [ctx       {:path [] :vals {}}
         ;data-1    {:a 1 :b {:x 11} }
          data-1    {:a 1 :b {:x 11} :c [31 32]}
          pattern-1 '{:a ?v :b {:x 11}}
          root-hid (edn->destruct data-1)
          ]
      (nl) (print-destruct *destruct*)
      (nl) (spyx (destruct->edn root-hid))

  )))

