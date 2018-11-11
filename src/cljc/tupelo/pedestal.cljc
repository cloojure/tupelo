;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.pedestal
  "Utils for Pedestal"
  #?@(:clj
      [(:use tupelo.core)
       (:require
         [clojure.set :as set]
         [clojure.string :as str]
         [schema.core :as s]
         [tupelo.schema :as tsk]
      )]))

;-----------------------------------------------------------------------------
; http string constants
(def accept                                       "Accept")
(def application-edn                              "application/edn")
(def application-json                             "application/json")
(def application-xml                              "application/xml")
(def content-security-policy                      "Content-Security-Policy")
(def content-type                                 "Content-Type")
(def location                                     "Location")
(def strict-transport-security                    "Strict-Transport-Security")
(def text-html                                    "text/html")
(def text-plain                                   "text/plain")
(def x-content-type-options                       "X-Content-Type-Options")
(def x-download-options                           "X-Download-Options")
(def x-frame-options                              "X-Frame-Options")
(def x-permitted-cross-domain-policies            "X-Permitted-Cross-Domain-Policies")
(def x-xss-protection                             "X-XSS-Protection")

#?(:clj
   (do

;-----------------------------------------------------------------------------
; Plumatic Schema type definitions

 (def context-keys-base #{:bindings
                          :io.pedestal.interceptor.chain/execution-id
                          ; :io.pedestal.interceptor.chain/queue
                          :io.pedestal.interceptor.chain/stack
                          :io.pedestal.interceptor.chain/terminators
                          :request
                          :servlet
                          :servlet-config
                          :servlet-request
                          :servlet-response})

 (def request-keys-base #{:async-supported?
                          :body
                          :headers
                          :path-info
                          :protocol
                          :query-string
                          :remote-addr
                          :request-method
                          :scheme
                          :server-name
                          :server-port
                          :uri})

 (def Request
   {:async-supported? s/Bool
    :body             s/Any
    :headers          {s/Str s/Str}
    :path-info        (s/maybe s/Str)
    :protocol         s/Str
    :query-string     (s/maybe s/Str)
    :remote-addr      s/Str
    :request-method   s/Keyword
    :scheme           (s/maybe s/Keyword)
    :server-name      (s/maybe s/Str)
    :server-port      s/Int
    :uri              (s/maybe s/Str)
    s/Keyword         s/Any})

 (def Context
   ; :io.pedestal.interceptor.chain/queue        [s/Any]  ; NOT ALWAYS PRESENT!
   {:bindings                                   tsk/Map
    :io.pedestal.interceptor.chain/execution-id s/Int
    :io.pedestal.interceptor.chain/stack        [tsk/KeyMap]
    :io.pedestal.interceptor.chain/terminators  [s/Any]
    :request                                    Request
    :servlet                                    s/Any
    :servlet-config                             s/Any
    :servlet-request                            s/Any
    :servlet-response                           s/Any
    s/Keyword                                   s/Any})

 (def TableRouteInfo
   {:verb                          s/Keyword
    :path                          s/Str
    :route-name                    s/Keyword
    (s/optional-key :interceptors) s/Any
    (s/optional-key :constraints)  s/Any})

 (s/defn table-route :- tsk/Tuple
   "Creates a Pedestal table-route entry from a context map."
   [route-map :- TableRouteInfo]
   (prepend
     (grab :path route-map)
     (grab :verb route-map)
     (grab :interceptors route-map)
     (keyvals-seq {:missing-ok true
                   :the-map    route-map :the-keys [:route-name :constraints]})))

 (s/defn context? :- s/Bool
   [map-in :- tsk/KeyMap]
   (let [keys-found (set (keys map-in))]
     (set/subset? context-keys-base keys-found)))

 (s/defn request? :- s/Bool
   [map-in :- tsk/KeyMap]
   (let [keys-found (set (keys map-in))]
     (set/subset? request-keys-base keys-found)))

 (s/defn interceptor? :- s/Bool
   [map-in :- tsk/KeyMap]
   (let [enter-fn   (get map-in :enter)
         leave-fn   (get map-in :leave)
         error-fn   (get map-in :error)
         keys-found (keys map-in)]
     (and
       (or (not-nil? enter-fn) (not-nil? leave-fn) (not-nil? error-fn))
       (set/subset? keys-found #{:name :enter :leave :error}))))

 (defn definterceptor-impl
   [name ctx]
   (assert symbol? name)
   (assert map? ctx)
   (let [keys-found (set (keys ctx))
         >>         (when-not (set/subset? keys-found #{:enter :leave :error})
                      (throw (IllegalArgumentException. (str "invalid keys-found:  " keys-found))))
         enter-fn   (get ctx :enter)
         leave-fn   (get ctx :leave)
         error-fn   (get ctx :error)
         >>         (when-not (or enter-fn leave-fn )
                      (throw (IllegalArgumentException. "Must have 1 or more of [enter-fn leave-fn error-fn]")))
         intc-map   (glue {:name (keyword name)}
                      (if (not-nil? enter-fn)
                        {:enter (grab :enter ctx)}
                        {})
                      (if (not-nil? leave-fn)
                        {:leave (grab :leave ctx)}
                        {} ) )]
     `(def ~name
        ~intc-map)))

 (defmacro definterceptor
   "Creates a Pedestal interceptor given a name and a map like
   (definterceptor my-intc
     {:enter  <enter-fn>
      :leave  <leave-fn>}
      :error  <error-fn>} ) "
   [name ctx]
   (definterceptor-impl name ctx))

 ))
