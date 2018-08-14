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
         [clojure.string :as str]
         [schema.core :as s]
         [tupelo.core :as t]
         [tupelo.impl :as i]
         [tupelo.schema :as tsch]
         [tupelo.string :as ts] )]))

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

     (def TableRouteInfo
       {(s/required-key :verb)         s/Keyword
        (s/required-key :path)         s/Str
        (s/optional-key :interceptors) s/Any
        (s/optional-key :route-name)   s/Keyword
        (s/optional-key :constraints)  s/Any})

     (s/defn table-route :- tsch/Tuple
       "Creates a Pedestal table-route entry from a context map."
       [ctx :- TableRouteInfo]
       (prepend
         (grab :path ctx)
         (grab :verb ctx)
         (grab :interceptors ctx)
         (i/keyvals-seq* {:missing-ok true} ctx [:route-name :constraints])))

     (defn edn-parsible-request
       "Pulls out 'safe' keys from the request map that can be parsed using `edn/read-string`."
       [request]
       (into (sorted-map)
         (submap-by-keys request
           [:protocol :async-supported? :remote-addr :headers
            :server-port :content-length :content-type :path-info
            :character-encoding :uri :server-name :query-string
            :path-params :scheme :request-method :context-path])))

     ))
