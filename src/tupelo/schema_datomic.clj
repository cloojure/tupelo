(ns tupelo.schema-datomic
  (:require [schema.core      :as s]
            [tupelo.schema    :as ts] )
  (:import [java.util HashSet] )
  (:gen-class))

;---------------------------------------------------------------------------------------------------
; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(def TxResult
  "A map returned by a successful transaction. Contains the keys 
   :db-before, :db-after, :tx-data, and :tempids"
  { :db-before    datomic.db.Db
    :db-after     datomic.db.Db
    :tx-data      [s/Any]  ; #todo (seq of datom)
    :tempids      ts/Map } )  ; #todo

