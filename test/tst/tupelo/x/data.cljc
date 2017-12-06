(ns tst.tupelo.x.data
  (:use tupelo.x.data
        tupelo.test)
  (:require
    [clojure.string :as str]
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

(defrecord MapEntity    [content])
(defrecord VecEntity    [content])
(defrecord Value        [content])

;-----------------------------------------------------------------------------
(defprotocol Edn->Fracture ; shatter, shard, sharder, destruct, destructure
  (edn->destruct [data]))

(extend-type clojure.lang.IPersistentMap
  Edn->Fracture (edn->destruct [data]
                  (with-spy-indent
                    (let [map-entries (forv [[k v] data]
                                        (let [value-hid   (edn->destruct v)
                                              map-entry   {k value-hid}]
                                          map-entry))
                          map-entity (->MapEntity (apply glue map-entries))
                          hid        (add-entity map-entity)]
                      hid))))

(extend-type clojure.lang.IPersistentVector ; #todo add for Set
  Edn->Fracture (edn->destruct [data]
                  (with-spy-indent
                    (let [vec-entry-maps (forv [[idx v] (indexed data)]
                                           (let [value-hid (edn->destruct v)]
                                             {idx value-hid}))
                          vec-entity     (->VecEntity (apply glue (sorted-map) vec-entry-maps))
                          hid            (add-entity vec-entity)]
                      hid))))

(extend-type java.lang.Object
  Edn->Fracture (edn->destruct [data]
                  (let [value (->Value data)
                        hid   (add-entity value)]
                    hid)))

;-----------------------------------------------------------------------------
(defprotocol Fracture->Edn
  (destruct->edn [hid]))

(extend-type clojure.lang.Keyword
  Fracture->Edn (destruct->edn [hid]
                  (destruct->edn (get-entity hid))))
(extend-type Value
  Fracture->Edn (destruct->edn [value]
                  (grab :content value)))
(extend-type MapEntity
  Fracture->Edn (destruct->edn [map-entity]
                  (with-spy-indent
                    (let [entry-maps (forv [[key val-hid] (grab :content map-entity)]
                                       {key (destruct->edn val-hid)})
                          map-result (apply glue entry-maps)]
                      map-result))))
(extend-type VecEntity
  Fracture->Edn (destruct->edn [vec-entity]
                  (with-spy-indent
                    (let [vec-elems (forv [[idx val-hid] (grab :content (glue (sorted-map) vec-entity))]
                                      [idx (destruct->edn val-hid)])]
                      vec-elems))))

;-----------------------------------------------------------------------------
(s/defn query-variable?
  "Returns true for symbols like '?name' "
  [arg]
  (and (symbol? arg)
    (str/starts-with? (name arg) "?")))

(defprotocol Match
  (match [query hid ctx]))
(extend-type clojure.lang.IPersistentMap
  Match (match [query hid ctx]
          (assert (map? query)) (assert (keyword? hid)) (assert (map? ctx))
          (spyx [query hid ctx])
          (with-spy-indent
            (let-spy [
                      [query-key query-val] (xfirst (seq query))
                      data-map (grab :content (get-entity hid))
                      ]
              (if (not (spyx (contains? data-map query-key)))
                ctx
                (let [ctx (if (spyx (query-variable? query-val))
                            (glue ctx {query-val (destruct->edn (grab query-key data-map))})
                            ctx)
                      query-rem (dissoc query query-key)]
                  (spyx :inner [query-rem hid ctx])
                  (if-not (spyx (empty? query-rem))
                    (match query-rem hid ctx)
                    ctx)))))))

(dotest
  (with-destruct (new-destruct)
    (let [ctx       {:path [] :vals {}}
          data-1    {:a 1 :b {:x 11} }
         ;data-1    {:a 1 :b {:x 11} :c [31 32]}
          query-1 '{:a ?v :b {:x 11}}
          root-hid (edn->destruct data-1)
          ]
      (nl) (print-destruct *destruct*)
      (nl) (spyx (destruct->edn root-hid))
      (nl) (spyx (match query-1 root-hid {}))

  )))

