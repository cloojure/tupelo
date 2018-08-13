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


(def header-strs
  #{"Strict-Transport-Security"
    "X-Frame-Options"
    "X-Content-Type-Options"
    "X-XSS-Protection"
    "X-Download-Options"
    "X-Permitted-Cross-Domain-Policies"
    "Content-Security-Policy"
    "Content-Type"
    "Accept"
    "Location"
    "text/html"
    "text/plain"
    "application/edn"
    "application/json" })


(def header-kw->str
  (into (sorted-map)
    (for [header-str header-strs]
      {(ts/str->kw-normalized (str/lower-case header-str)) header-str})))

(comment
  ; example header-kw->str =>
  {:accept                    "Accept",
   :application-edn           "application/edn",
   :application-json          "application/json",
   :content-security-policy   "Content-Security-Policy",
   :content-type              "Content-Type",
   :location                  "Location",
   :strict-transport-security "Strict-Transport-Security",
   :text-html                 "text/html",
   :text-plain                "text/plain",
   :x-content-type-options    "X-Content-Type-Options",
   :x-download-options        "X-Download-Options",
   :x-frame-options           "X-Frame-Options",
   :x-permitted-cross-domain-policies
                              "X-Permitted-Cross-Domain-Policies",
   :x-xss-protection          "X-XSS-Protection"}
  )

(s/defn header :- s/Str
  "Returns an HTTP Header string given its normalized keyword representation
   (e.g. :text-html => 'text/html',  :content-type => 'Content-Type') "
  [hdr-kw :- s/Keyword]
  (grab hdr-kw header-kw->str))

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
