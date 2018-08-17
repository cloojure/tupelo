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
       [route-map :- TableRouteInfo]
       (prepend
         (grab :path route-map)
         (grab :verb route-map)
         (grab :interceptors route-map)
         (keyvals-seq {:missing-ok true
                       :the-map    route-map :the-keys [:route-name :constraints]})))

     (defn edn-parsible-request
       "Pulls out 'safe' keys from the request map that can be parsed using `edn/read-string`."
       [request]
       (into (sorted-map)
         (submap-by-keys (unlazy request)
           [:protocol :async-supported? :body :remote-addr :headers
            :server-port :content-length :content-type :path-info
            :character-encoding :uri :server-name :query-string :query-params
            :path-params :scheme :request-method :context-path]
           :missing-ok)))

     (defn edn-parsible-interceptor
       "Pulls out 'safe' keys from the interceptor map that can be parsed using `edn/read-string`."
       [interceptor]
       (submap-by-keys interceptor [:name]))

     (defn edn-parsible-route
       "Pulls out 'safe' keys from the request map that can be parsed using `edn/read-string`."
       [route]
       (let [result (it-> (sorted-map)
                      (into it
                        (submap-by-keys (unlazy route)
                          [:path :method :path-constraints :path-parts :route-name :path-params]
                          :missing-ok))
                      (glue it {:interceptors (mapv edn-parsible-interceptor (grab :interceptors route))}))]
         result))

     (defn edn-parsible-context
       "Pulls out 'safe' keys from the context map that can be parsed using `edn/read-string`."
       [context]
       (nl) (println "-----------------------------------------------------------------------------")
       (spyx-pretty :edn-parsible-context context)
       (let [req    {:request (edn-parsible-request (grab :request context))}
             route  {:route (edn-parsible-route (grab :route context))}
             stack  {:io.pedestal.interceptor.chain/stack
                     (mapv edn-parsible-interceptor (grab :io.pedestal.interceptor.chain/stack context))}
             result (glue (sorted-map) req route stack)]
         result))

     ))
