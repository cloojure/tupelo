;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.datomic
  "Helper functions for Datomic."
  (:require [tupelo.types   :refer [boolean?] ]
            [schema.core      :as s]
            [schema.coerce    :as coerce] )
  (:gen-class))

(s/def EntitySpec
  { :db/id                    s/Any
    :db/ident                 s/Keyword
    :db/valueType             s/Keyword
    :db/cardinality           s/Keyword
    (s/optional-key 
      :db/unique)             s/Keyword
    :db/doc                   s/Str
    :db.install/_attribute    s/Keyword } )

(defmacro throw-with-msg
  [sym-name]
  `(throw (IllegalArgumentException. 
            (str ~(str "Bad value for arg '" sym-name "': ") ~sym-name))))
; (macroexpand-1 '(throw-with-msg scalar?))


(defn entity-spec 
; "Create and return a Datomic entity specification"
  [ ident valueType 
    & {:keys [scalar? unique? fulltext? doc]
        :or { scalar?    true 
              unique?    false
              fulltext?  false
              doc        "???" }} 
  ]
  ; contract for return value
  {:post [ (s/validate EntitySpec %) ] }

  ; contract for input args
  (when-not (keyword?     ident)                      (throw-with-msg ident))
  (when-not (keyword?     valueType)                  (throw-with-msg valueType))
  (when-not (= (namespace valueType) "db.type")       (throw-with-msg valueType))
  (when-not (boolean?     scalar?)                    (throw-with-msg scalar?))
  (when-not (or (= false      unique?)
                (= :identity  unique?)
                (= :value     unique?))               (throw-with-msg unique?))
  (when-not (boolean?     fulltext?)                  (throw-with-msg fulltext?))
  (when-not (string?      doc)                        (throw-with-msg doc))

  (conj {}
    { :db/id                        "#db/id[:db.part/db]"  ; #awt #todo fix string -> reader conditional
      :db/ident                     ident
      :db/valueType                 valueType
      :db/cardinality               (if scalar?   :db.cardinality/one 
                                                  :db.cardinality/many)
      :db/doc                       doc
      :db.install/_attribute        :db.part/db }
    (condp = unique?            
      :identity   { :db/unique  :db.unique/identity }
      :value      { :db/unique  :db.unique/value } 
      nil )
  ))

