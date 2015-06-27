(ns tst.tupelo.datomic
  (:use tupelo.core
        clojure.test )
  (:require [datomic.api      :as d]
            [tupelo.datomic   :as t]
            [schema.core      :as s]))

(set! *warn-on-reflection* false)
(set! *print-length* 5)
(set! *print-length* nil)

;---------------------------------------------------------------------------------------------------
; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(def ^:dynamic *conn*)

(use-fixtures :each
  (fn [tst-fn]
    ; Create the database & a connection to it
    (let [uri           "datomic:mem://tupelo"
          _ (d/create-database uri)
          conn          (d/connect uri)
    ]
      (binding [*conn* conn]
        (tst-fn))
      (d/delete-database uri)
    )))


(deftest t-new-attribute
  (testing "basic"
    (let [result  (t/new-attribute :weapon/type :db.type/keyword 
                      :db.unique/value       :db.unique/identity 
                      :db.cardinality/one    :db.cardinality/many 
                      :db/index :db/fulltext :db/isComponent :db/noHistory ) ]
      (is (s/validate datomic.db.DbId (:db/id result)))
      (is (matches? result
              {:db/id           _       :db/ident               :weapon/type
               :db/index        true    :db/unique              :db.unique/identity  
               :db/noHistory    true    :db/cardinality         :db.cardinality/many
               :db/isComponent  true    :db.install/_attribute  :db.part/db 
               :db/fulltext     true    :db/valueType           :db.type/keyword } )))
               
    (let [result  (t/new-attribute :weapon/type :db.type/keyword 
                      :db.unique/identity    :db.unique/value
                      :db.cardinality/many   :db.cardinality/one
                      :db/index :db/fulltext :db/isComponent :db/noHistory ) ]
      (is (matches? result
              {:db/id           _       :db/ident               :weapon/type
               :db/index        true    :db/unique              :db.unique/value  
               :db/noHistory    true    :db/cardinality         :db.cardinality/one  
               :db/isComponent  true    :db.install/_attribute  :db.part/db 
               :db/fulltext     true    :db/valueType           :db.type/keyword } ))))

  (testing "types"
    (let [result  (t/new-attribute :location :db.type/string) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/string       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/keyword) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/keyword       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/boolean) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/boolean       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/long) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/long       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/bigint) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/bigint       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/float) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/float       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/double) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/double       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/bigdec) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/bigdec       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/bytes) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/bytes       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/instant) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/instant       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/uuid) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/uuid       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/uri) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/uri       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/ref) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :xb.type/ref       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } ))))

  (testing "cardinality & unique"
    (let [result  (t/new-attribute :location :db.type/string) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :db.type/string       :db/cardinality :db.cardinality/one 
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/string :db.cardinality/many) ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :db.type/string       :db/cardinality :db.cardinality/many
               :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/string :db.unique/value)  ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :db.type/string       :db/cardinality :db.cardinality/one 
               :db/unique       :db.unique/value      :db.install/_attribute :db.part/db } )))
    (let [result  (t/new-attribute :location :db.type/string :db.unique/identity)  ]
      (is (matches? result
              {:db/id           _                     :db/ident       :location 
               :db/valueType    :db.type/string       :db/cardinality :db.cardinality/one 
               :db/unique       :db.unique/identity   :db.install/_attribute :db.part/db } ))))
)


(deftest t-new-partition
  (let [result   (t/new-partition :people ) ]
    (is (matches? result
           {:db/id                    #db/id[:db.part/db _] 
            :db.install/_partition    :db.part/db 
            :db/ident                 :people} )))
  (let [result   (t/new-partition :part.with.ns ) ]
    (is (matches? result
           {:db/id                    #db/id[:db.part/db _] 
            :db.install/_partition    :db.part/db 
            :db/ident                 :part.with.ns} )))
)

