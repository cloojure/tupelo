;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.schema
  "Prismatic Schema type definitions"
  (:refer-clojure :exclude [Fn List Single MapEntry])
  (:require [schema.core :as s])
  #?(:clj
     (:import [java.util HashSet])))

(def Map      {s/Any      s/Any} )
(def KeyMap   {s/Keyword  s/Any} )
(def MapEntry #?(:clj  clojure.lang.MapEntry
                 :cljs cljs.core.MapEntry))

(def Set
  "Either a Clojure hash-set or a java.util.HashSet"
  #?(:clj  (s/if set? #{s/Any} java.util.HashSet)
     :cljs #{s/Any}))

(def Vec
  "An ordered sequence of items of indeterminate length (synonymous for List)."
  [s/Any])
(def List
  "An ordered sequence of items of indeterminate length (synonymous for Vec)."
  [s/Any])
(def Array
  "An ordered sequence of vectors."
  [Vec])

#?(:clj             ; #todo cljs?
   (def CharVec
     "An ordered sequence of characters of indeterminate length."
     [Character]))

(def Single [(s/one s/Any "x1")]) ; length-1 vector
(def Pair [(s/one s/Any "x1") (s/one s/Any "x2")]) ; length-2 vector
(def Triple [(s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3")]) ; length-3 vector
(def Quad [(s/one s/Any "x1") (s/one s/Any "x2") (s/one s/Any "x3") (s/one s/Any "x4")]) ; length-4 vector

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

(def Collection
  "Any collection type of Vec (& List), Map, or Set"
  (s/cond-pre Vec Map Set))

(def Fn (s/make-fn-schema s/Any s/Any))

(def Interceptor ; #todo add to tupelo.forest
  "Plumatic Schema type name for interceptor type used by `walk-entity`."
  {(s/required-key :enter) s/Any
   (s/required-key :leave) s/Any
   (s/optional-key :id)    s/Keyword})

(defmacro with-validation-enabled
  "Run forms with Plumatic Schema enabled"
  [& forms]
  `(let [orig-validation-state# (s/fn-validation?)]
     (s/set-fn-validation! true)
     (let [result# (do ~@forms)]
       (s/set-fn-validation! orig-validation-state#)
       result#)))

(defmacro with-validation-disabled
  "Run forms with Plumatic Schema disabled"
  [& forms]
  `(let [orig-validation-state# (s/fn-validation?)]
     (s/set-fn-validation! false)
     (let [result# (do ~@forms)]
       (s/set-fn-validation! orig-validation-state#)
       result#)))

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











