;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.pedestal-data
  (:use tupelo.core
        tupelo.pedestal))

#?(:clj
   (do

     (def sample-context
       {:async?                                     :tupelo.parse/edn-non-parsible,
        :bindings
                                                    {:tupelo.parse/edn-non-parsible :tupelo.parse/edn-non-parsible},
        :enter-async                                [:tupelo.parse/edn-non-parsible],
        :request
                                                    {:protocol         "HTTP/1.1",
                                                     :async-supported? true,
                                                     :remote-addr      "127.0.0.1",
                                                     :servlet-response :tupelo.parse/edn-non-parsible,
                                                     :servlet          :tupelo.parse/edn-non-parsible,
                                                     :headers
                                                                       {"accept"
                                                                                                    "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
                                                                        "upgrade-insecure-requests" "1",
                                                                        "connection"                "keep-alive",
                                                                        "user-agent"
                                                                                                    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.140 Safari/537.36",
                                                                        "host"                      "localhost:8890",
                                                                        "accept-encoding"           "gzip, deflate, br",
                                                                        "accept-language"           "en-US,en;q=0.9"},
                                                     :server-port      8890,
                                                     :servlet-request  :tupelo.parse/edn-non-parsible,
                                                     :path-info        "/echo",
                                                     :url-for          :tupelo.parse/edn-non-parsible,
                                                     :uri              "/echo",
                                                     :server-name      "localhost",
                                                     :query-string     nil,
                                                     :path-params      {},
                                                     :body             "",
                                                     :scheme           :http,
                                                     :request-method   :get,
                                                     :context-path     ""},
        :route
                                                    {:path        "/echo",
                                                     :method      :get,
                                                     :path-re     :tupelo.parse/edn-non-parsible,
                                                     :path-parts  ["echo"],
                                                     :interceptors
                                                                  [{:name  :echo,
                                                                    :enter :tupelo.parse/edn-non-parsible,
                                                                    :leave nil,
                                                                    :error nil}],
                                                     :route-name  :echo-base,
                                                     :path-params {},
                                                     :io.pedestal.http.route.prefix-tree/satisfies-constraints?
                                                                  :tupelo.parse/edn-non-parsible},
        :servlet                                    :tupelo.parse/edn-non-parsible,
        :servlet-config                             :tupelo.parse/edn-non-parsible,
        :servlet-request                            :tupelo.parse/edn-non-parsible,
        :servlet-response                           :tupelo.parse/edn-non-parsible,
        :url-for                                    :tupelo.parse/edn-non-parsible,
        :io.pedestal.interceptor.chain/execution-id 1,
        :io.pedestal.interceptor.chain/queue        [],
        :io.pedestal.interceptor.chain/stack
                                                    [{:name  :echo,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.route/router,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.secure-headers/secure-headers,
                                                      :enter nil,
                                                      :leave :tupelo.parse/edn-non-parsible,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.route/method-param,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.route/path-params-decoder,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.route/query-params,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.ring-middlewares/content-type-interceptor,
                                                      :enter nil,
                                                      :leave :tupelo.parse/edn-non-parsible,
                                                      :error nil}
                                                     {:name  :io.pedestal.http/not-found,
                                                      :enter nil,
                                                      :leave :tupelo.parse/edn-non-parsible,
                                                      :error nil}
                                                     {:name  :io.pedestal.http/log-request,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}
                                                     {:name  :io.pedestal.http.impl.servlet-interceptor/ring-response,
                                                      :enter nil,
                                                      :leave :tupelo.parse/edn-non-parsible,
                                                      :error :tupelo.parse/edn-non-parsible}
                                                     {:name  :io.pedestal.http.impl.servlet-interceptor/stylobate,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave :tupelo.parse/edn-non-parsible,
                                                      :error :tupelo.parse/edn-non-parsible}
                                                     {:name
                                                             :io.pedestal.http.impl.servlet-interceptor/terminator-injector,
                                                      :enter :tupelo.parse/edn-non-parsible,
                                                      :leave nil,
                                                      :error nil}],
        :io.pedestal.interceptor.chain/terminators
                                                    [:tupelo.parse/edn-non-parsible]}

       )

     (def sample-request (grab :request sample-context))

   ))
