(ns tst.tupelo.x.data
  (:use tupelo.core tupelo.test )
  (:require
    [clojure.math.combinatorics :as combo]
    [schema.core :as s]
    [tupelo.misc :as tm :refer [HID]]
    [tupelo.string :as str]
    [tupelo.schema :as tsk]))


(defn combo-all [& args]
  (let [wrapped-args (forv [arg args]
                       (if (sequential? arg)
                         arg
                         [arg] ))]
    (unlazy (apply combo/cartesian-product wrapped-args))))

(verify
  (is= [[:a 1 :b] [:a 2 :b] [:a 3 :b]]
    (unlazy (combo/cartesian-product [:a] [1 2 3] [:b]))
    (combo-all :a [1 2 3] :b)))


; WARNING: Don't abuse dynamic scope. See: https://stuartsierra.com/2013/03/29/perils-of-dynamic-scope
(def ^:dynamic *fracture* nil)

(defn validate-fracture []
  (when-not (map? *fracture*)
    (throw (IllegalArgumentException. (str "validate-fracture: failed fracture=" *fracture*)))))

(defmacro with-fracture ; #todo swap names?
  [fracture-arg & forms]
  `(binding [*fracture* ~fracture-arg]
     (validate-fracture)
     ~@forms))

; HID & :hid are shorthand for Hash ID, the SHA-1 hash of a v1/UUID expressed as a hexadecimal keyword
; format { :hid Node }
(defn new-fracture
  "Returns a new, empty fracture."
  []
  {})

(defn print-fracture [fracture]
  (println "-----------------------------------------------------------------------------")
  (doseq [[hid val] (glue (sorted-map) fracture)]
    (println "  " hid (str/indent 4 (pr-str val))))
  (println "-----------------------------------------------------------------------------"))

; #todo avoid self-cycles
; #todo avoid descendant-cycles

; #todo maybe create a record type HID to wrap hid keyword values for type dispatch

(s/defn new-hid :- HID ; #todo ***** temp for testing only! *****
  []
  (->> (tm/sha-uuid)
    (str/clip 8)
    (keyword)))

(s/defn add-entity
  [entity :- tsk/Map]
  ;(spyx [:adding hid entity]) (flush)
  (let [hid (new-hid)]
    (set! *fracture* (glue *fracture* {hid entity}))
    hid))

(s/defn get-entity :- tsk/Map
  [hid :- HID]
  (grab hid *fracture*))

;-----------------------------------------------------------------------------
;(defrecord MapRef [hid]) ; todo needed?
;(defrecord VecRef [hid]) ; todo needed?
;(defrecord MapEntry     [key val-hid])
;(defrecord VecElement   [idx val-hid])

(defrecord MapEntity    [content])
(defrecord VecEntity    [content])
(defrecord Value        [content])

(comment
;-----------------------------------------------------------------------------
(defprotocol Edn->Fracture ; shatter, shard, sharder, destruct, destructure
  (edn->fracture [data]))

(extend-type clojure.lang.IPersistentMap
  Edn->Fracture (edn->fracture [data]
                  (with-spy-indent
                    (let [map-entries (forv [[k v] data]
                                        (let [value-hid   (edn->fracture v)
                                              map-entry   {k value-hid}]
                                          map-entry))
                          map-entity (->MapEntity (apply glue map-entries))
                          hid        (add-entity map-entity)]
                      hid))))

(extend-type clojure.lang.IPersistentVector ; #todo add for Set
  Edn->Fracture (edn->fracture [data]
                  (with-spy-indent
                    (let [vec-entry-maps (forv [[idx v] (indexed data)]
                                           (let [value-hid (edn->fracture v)]
                                             {idx value-hid}))
                          vec-entity     (->VecEntity (apply glue (sorted-map) vec-entry-maps))
                          hid            (add-entity vec-entity)]
                      hid))))

(extend-type java.lang.Object
  Edn->Fracture (edn->fracture [data]
                  (let [value (->Value data)
                        hid   (add-entity value)]
                    hid)))

;-----------------------------------------------------------------------------
(defprotocol Fracture->Edn
  (fracture->edn [hid]))

(extend-type clojure.lang.Keyword
  Fracture->Edn (fracture->edn [hid]
                  (fracture->edn (get-entity hid))))
(extend-type Value
  Fracture->Edn (fracture->edn [value]
                  (grab :content value)))
(extend-type MapEntity
  Fracture->Edn (fracture->edn [map-entity]
                  (with-spy-indent
                    (let [entry-maps (forv [[key val-hid] (grab :content map-entity)]
                                       {key (fracture->edn val-hid)})
                          map-result (apply glue entry-maps)]
                      map-result))))
(extend-type VecEntity
  Fracture->Edn (fracture->edn [vec-entity]
                  (with-spy-indent
                    (let [vec-elems (forv [[idx val-hid] (glue (sorted-map) (grab :content vec-entity))]
                                      (fracture->edn val-hid))]
                      vec-elems))))

;-----------------------------------------------------------------------------
(def FAILURE :failure)
(defn failure? [arg] (= arg FAILURE))

(s/defn query-variable?
  "Returns true for symbols like '?name' "
  [arg]
  (and (symbol? arg)
    (str/starts-with? (name arg) "?")))

(defprotocol Match
  (match [query hid ctx]))

(extend-type clojure.lang.Symbol
  Match (match [query hid ctx ]
          (assert (query-variable? query)) ; found match
          (assert (keyword? hid)) (assert (map? ctx))
          (with-spy-indent
            (spyx [query hid ctx ])
            (let-spy [edn-val (fracture->edn hid)
                      result-ctx (glue ctx {query edn-val})]
              (spy :found result-ctx)))))

(extend-type clojure.lang.IPersistentMap
  Match (match [query hid ctx]
          ; #todo how use Plumatic Schema for protocols?
          (assert (map? query)) (assert (keyword? hid)) (assert (map? ctx))
          (with-spy-indent
            (spyx [query hid ctx])
            (let-spy [shard (grab :content (get-entity hid))
                      >> (assert (map? shard))
                      sub-results (forv [[query-key query-val] (vec query)]
                                    (do
                                      (spyx [query-key query-val])
                                      (cond
                                        (spyx (query-variable? query-key))
                                        (let-spy [sub-results (forv [[shard-key shard-hid] (vec shard)]
                                                                (let [ctx-inner (glue ctx {query-key shard-key})]
                                                                  (spyx :inner2 [query-key query-val shard-key shard-hid ctx-inner])
                                                                  (match query-val shard-hid ctx-inner)))]
                                          sub-results)

                                        (spyx (contains? shard query-key))
                                        (let [shard-hid (grab query-key shard)]
                                          (spyx :inner [query-val shard-hid ctx])
                                          (match query-val shard-hid ctx))

                                        :else FAILURE)))
                      sub-result-combos (apply combo-all sub-results)
                      sub-result-combos-ok (drop-if #(has-some? failure? %) sub-result-combos)
                      result-ctx-list (if (empty? sub-result-combos-ok)
                                        FAILURE
                                        (forv [sub-result sub-result-combos-ok]
                                          (apply glue sub-result)))]
              result-ctx-list))))

(extend-type java.lang.Object
  Match (match [query hid ctx] ; found match
          (assert (keyword? hid)) (assert (map? ctx))
          (with-spy-indent
            (spyx [query hid ctx])
            (let-spy [edn-val (fracture->edn hid)]
              (if (= query edn-val)
                (spy :object ctx)
                FAILURE)))))

(verify
  (spy-indent-reset)
  (with-fracture (new-fracture)
    (let [ctx      {:path [] :vals {}}
         ;data-1   {:a 1 :b {:x 11}}
          data-1    {:a 1 :b {:x 11} :c [31 32]}
          query-1  '{:a ?v :b {:x 11}}
          query-1  '{?k 1 :b {:x 11}}
          query-1  '{:a ?v :b {:x 11} :c ?c}
          query-1  '{:a ?a :b {:x ?x}}
          root-hid (edn->fracture data-1) ]

      ; #todo continue here! (2018)
      ;(nl) (print-fracture *fracture*)
      ;(nl) (spyx (fracture->edn root-hid))
      ;(nl) (spy :result (match query-1 root-hid {}))


  )))

  )
