(ns tupelo.schema
  "Prismatic Schema type definitions"
  (:refer-clojure :exclude [Fn List Single])
  (:require [schema.core :as s])
  #?(:clj
     (:import [java.util HashSet])))

(def Map      {s/Any      s/Any} )
(def KeyMap   {s/Keyword  s/Any} )

(def Set
  "Either a Clojure hash-set or a java.util.HashSet"
  #?(:clj  (s/if set? #{s/Any} java.util.HashSet)
     :cljs #{s/Any}))

(def List
  "An ordered sequence of items of indeterminate length."
  [s/Any] )

#?(:clj             ; #todo cljs?
   (def CharVec
     "An ordered sequence of characters of indeterminate length."
     [Character]))

(def Single [(s/one s/Any "x1")]) ; length-1 vector
(def Pair [(s/one s/Any "x1") (s/one s/Any "x2")]) ; length-2 vector
(def Triple [(s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3")]) ; length-3 vector
(def Quad [(s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3") (s/one s/Any "x4")]) ; length-4 vector
(def Vec [s/Any]) ; arbitrary length vector (or any sequence)

(def EnliveNode
  "An Enlive tree node"
  {:tag s/Any :attrs KeyMap :content [s/Any]})

(def Tuple
  "A specific type of sequential collection, typically a vector of constant length where each
   element has a pre-defined interpretation."
  [s/Any])

(def TupleList
  "A sequence of tuples (typically a vector of vectors)"
  [Tuple])

(def TupleSet
  "The result of any Datomic using the Entity API is logically a hash-set of tuples (vectors).
   The contents and order of each tuple is determined by the find clause:

        ----- query -----                         ----- tuples -----
      (d/q '{:find [?e ?name ?age] ...)     ->    [?e ?name ?age]
   "
  #{Tuple})

(def MapList [Map]) ; a list of Maps
(def TupleMap [Map]) ; a single  result  returned by Datomic pull api  ; #todo needs (s/one ...) ??? or MapList?
(def TupleMaps [TupleMap]) ; a list of results returned by Datomic pull api

(def Fn (s/make-fn-schema s/Any s/Any))

#?(:clj
   (do
     ;-----------------------------------------------------------------------------
     ; Datomic-related stuff

     (def Eid
       "Each entity in the DB is uniquely specified its Entity ID (EID).  Indeed, allocation of a unique
        EID is what 'creates' an entity in the DB."
       Long)

     ; #todo - clarify in all doc-strings that entity-spec = [EID or lookup-ref]
     (def LookupRef
       "If an entity has an attribute with either :db.unique/value or :db.unique/identity, that entity
        can be uniquely specified using a lookup-ref (LookupRef). A lookup-ref is an attribute-value pair
        expressed as a tuple:  [ <attribute> <value> ]"
       [(s/one s/Keyword "attr")
        (s/one s/Any "val")])

     (def EntitySpec
       "An EntitySpec is used to uniquely specify an entity in the DB. It consists of
        either an EID or a LookupRef."
       (s/if int? Eid LookupRef))

     (def DatomMap
       "The Clojure map representation of a Datom."
       {:e Eid :a Eid :v s/Any :tx Eid :added s/Bool})

     ;-----------------------------------------------------------------------------
     ; HTTP related stuff

     (def HttpRequest
       {s/Any                             s/Any
        :body                             s/Any
        :content-length                   s/Any
        :headers                          {s/Str s/Str}
        :params                           s/Any
        :protocol                         s/Str
        :remote-addr                      s/Str
        :scheme                           s/Keyword
        :server-name                      s/Str
        :server-port                      s/Int
        :uri                              s/Str

        (s/optional-key :content-type)    nil
        (s/optional-key :cookies)         s/Any
        (s/optional-key :flash)           nil
        (s/optional-key :form-params)     s/Any
        (s/optional-key :query-params)    {}
        (s/optional-key :query-string)    s/Any
        (s/optional-key :request-method)  s/Keyword
        (s/optional-key :route-handler)   s/Any
        (s/optional-key :route-params)    s/Any
        (s/optional-key :session)         s/Any
        (s/optional-key :ssl-client-cert) s/Any})

     ))












