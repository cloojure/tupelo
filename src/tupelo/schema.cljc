(ns tupelo.schema
  (:require [schema.core :as s] )
  (:import [java.util HashSet] )
  (:gen-class))

;---------------------------------------------------------------------------------------------------
; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(def Set     #{s/Any} )
(def Map      {s/Any      s/Any} )
(def KeyMap   {s/Keyword  s/Any} )

(def Eid
  "Each entity in the DB is uniquely specified its Entity ID (EID).  Indeed, allocation of a unique
   EID is what 'creates' an entity in the DB."
  Long)

; #todo - clarify in all doc-strings that entity-spec = [EID or lookup-ref]
(def LookupRef  
  "If an entity has an attribute with either :db.unique/value or :db.unique/identity, that entity
   can be uniquely specified using a lookup-ref (LookupRef). A lookup-ref is an attribute-value pair
   expressed as a tuple:  [ <attribute> <value> ]"
  [ (s/one s/Keyword  "attr")  
    (s/one s/Any      "val" ) ] )

(def EntitySpec 
  "An EntitySpec is used to uniquely specify an entity in the DB. It consists of 
   either an EID or a LookupRef."
  (s/either Eid 
            LookupRef))

(def DatomMap
  "The Clojure map representation of a Datom."
  { :e Eid  :a Eid  :v s/Any  :tx Eid  :added s/Bool } )

(def Set
  "Either a Clojure hash-set or a java.util.HashSet"
  (s/either #{s/Any} 
            java.util.HashSet ))

(def List
  "An ordered sequence of items of indeterminate length."
  [s/Any] )

(def StrVec
  "An ordered sequence of characters of indeterminate length."
  [Character] )

(def Tuple
  "A specific type of sequential collection, typically a vector of constant length where each
   element has a pre-defined interpretation."
  [s/Any] )

(def TupleList
  "A sequence of tuples (typically a vector of vectors)"
  [Tuple] )

(def TupleSet 
  "The result of any Datomic using the Entity API is logically a hash-set of tuples (vectors).  
   The contents and order of each tuple is determined by the find clause:

        ----- query -----                         ----- tuples -----
      (d/q '{:find [?e ?name ?age] ...)     ->    [?e ?name ?age] 
   
   "
  #{Tuple} )

(def MapList      [Map] )         ; a list of Maps
(def TupleMap     [Map] )         ; a single  result  returned by Datomic pull api  ; #todo needs (s/one ...) ??? or MapList?
(def TupleMaps    [TupleMap] )    ; a list of results returned by Datomic pull api

(def Vec  [ s/Any ] )  ; arbitrary length vector (or any sequence)
(def Vec1 [ (s/one s/Any "x1") ] )  ; length-1 vector
(def Vec2 [ (s/one s/Any "x1") (s/one s/Any "x2") ] ) ; length-2 vector
(def Vec3 [ (s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3") ] )  ; length-3 vector
(def Vec4 [ (s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3") (s/one s/Any "x4") ] )
(def Vec5 [ (s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3") (s/one s/Any "x4") (s/one s/Any "x5") ] )

