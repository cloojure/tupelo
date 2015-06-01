;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.cooljure.datomic
  (:use cooljure.datomic 
        clojure.test
        cooljure.core)
)

(deftest entity-spec-t
  (is (= (entity-spec :my-ident :db.type/string)
          {:db/id                                     "#db/id[:db.part/db]"
           :db/ident                                  :my-ident
           :db/valueType                              :db.type/string, 
           :db/cardinality                            :db.cardinality/one
           :db/doc                                    "???"
           :db.install/_attribute                     :db.part/db} ))

  (is (= (entity-spec :my-ident :db.type/string
                        :unique? :identity )
          {:db/id                                     "#db/id[:db.part/db]"
           :db/ident                                  :my-ident
           :db/valueType                              :db.type/string, 
           :db/cardinality                            :db.cardinality/one
           :db/unique                                 :db.unique/identity
           :db/doc                                    "???"
           :db.install/_attribute                     :db.part/db} ))
)

