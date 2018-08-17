;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.pedestal
  (:use tupelo.core tupelo.pedestal tupelo.test)
  (:require
    [clojure.data :as data]
    [schema.core :as s]
  ))

#?(:clj
   (do
     (dotest
       (is= content-type "Content-Type")
       (is= text-html "text/html"))

     (dotest
       (let [dummy-handler identity
             dummy-contraints (constantly true)]
         (is= (table-route {:path         "/todo/:list-id/:item"
                             :verb         :delete
                             :interceptors dummy-handler})
           ["/todo/:list-id/:item" :delete dummy-handler])

         (is= (table-route {:path         "/todo/:list-id/:item"
                             :verb         :delete
                             :interceptors dummy-handler
                             :route-name   :list-item-delete})
           ["/todo/:list-id/:item" :delete dummy-handler :route-name :list-item-delete])

         (is= (table-route {:path         "/todo/:list-id/:item"
                             :verb         :delete
                             :interceptors dummy-handler
                             :constraints  dummy-contraints})
           ["/todo/:list-id/:item" :delete dummy-handler :constraints dummy-contraints])

         (is= (table-route {:path         "/todo/:list-id/:item"
                            :verb         :delete
                            :interceptors [dummy-handler]
                            :route-name   :list-item-delete
                            :constraints  dummy-contraints})
           ["/todo/:list-id/:item" :delete [dummy-handler] :route-name :list-item-delete :constraints dummy-contraints]))

       )


     (dotest
       (let [intc {:name  :echo-url-for,
                   :enter ::non-parsible-object,
                   :leave nil,
                   :error nil}]
         (set= [:name] (keys (edn-parsible-interceptor intc))))

       (let [context {:io.pedestal.interceptor.chain/stack
                                                                  [{:name  :echo-url-for,
                                                                    :enter ::non-parsible-object,
                                                                    :leave nil,
                                                                    :error nil}
                                                                   {:name  :io.pedestal.http.route/router,
                                                                    :enter
                                                                           ::non-parsible-object,
                                                                    :leave nil,
                                                                    :error nil}
                                                                   {:name  :io.pedestal.http.secure-headers/secure-headers,
                                                                    :enter nil,
                                                                    :leave ::non-parsible-object

                                                                    :error nil}
                                                                   {:name  :io.pedestal.http.route/method-param,
                                                                    :enter ::non-parsible-object

                                                                    :leave nil,
                                                                    :error nil}],
                      :request
                                                                  {:protocol           "HTTP/1.1",
                                                                   :async-supported?   true,
                                                                   :remote-addr        "127.0.0.1",
                                                                   :servlet-response
                                                                                       ::non-parsible-object
                                                                   :servlet
                                                                                       ::non-parsible-object
                                                                   :headers            {"content-length" "0", "content-type" ""},
                                                                   :server-port        -1,
                                                                   :servlet-request
                                                                                       ::non-parsible-object
                                                                   :content-length     0,
                                                                   :content-type       "",
                                                                   :path-info          "/echo-url-for/abcdef/12345",
                                                                   :character-encoding "UTF-8",
                                                                   :url-for            ::non-parsible-object
                                                                   :uri                "/echo-url-for/abcdef/12345",
                                                                   :server-name        nil,
                                                                   :query-string       nil,
                                                                   :path-params        {:list-id "abcdef", :item-id "12345"},
                                                                   :body
                                                                                       ::input-stream
                                                                   :scheme             nil,
                                                                   :request-method     :get,
                                                                   :context-path       ""}
                      :enter-async                                [::non-parsible-object],
                      :io.pedestal.interceptor.chain/terminators
                                                                  [::non-parsible-object]
                      :servlet-response                           ::non-parsible-object
                      :route                                      {:path                                                      "/echo-url-for/:list-id/:item-id",
                                                                   :method                                                    :get,
                                                                   :path-constraints                                          {:list-id "([^/]+)", :item-id "([^/]+)"},
                                                                   :io.pedestal.http.route.prefix-tree/satisfies-constraints? ::non-parsible-object
                                                                   :path-re                                                   #"/\Qecho-url-for\E/([^/]+)/([^/]+)",
                                                                   :path-parts                                                ["echo-url-for" :list-id :item-id],
                                                                   :interceptors                                              [{:name  :echo-url-for,
                                                                                                                                :enter ::non-parsible-object
                                                                                                                                :leave ::non-parsible-object
                                                                                                                                :error ::non-parsible-object}],
                                                                   :route-name                                                :echo-url-for,
                                                                   :path-params                                               {:list-id "abcdef", :item-id "12345"}
                                                                   },
                      :servlet                                    ::non-parsible-object
                      :servlet-request                            ::non-parsible-object
                      :io.pedestal.interceptor.chain/queue        ::non-parsible-object
                      :url-for                                    ::non-parsible-object
                      :io.pedestal.interceptor.chain/execution-id 190,
                      :servlet-config                             nil,
                      :async?                                     ::non-parsible-object
                      }]
         (is= (edn-parsible-request (:request context))
           {:async-supported?   true,
            :body               :tst.tupelo.pedestal/input-stream,
            :character-encoding "UTF-8",
            :content-length     0,
            :content-type       "",
            :context-path       "",
            :headers            {"content-length" "0", "content-type" ""},
            :path-info          "/echo-url-for/abcdef/12345",
            :path-params        {:list-id "abcdef", :item-id "12345"},
            :protocol           "HTTP/1.1",
            :query-string       nil,
            :remote-addr        "127.0.0.1",
            :request-method     :get,
            :scheme             nil,
            :server-name        nil,
            :server-port        -1,
            :uri                "/echo-url-for/abcdef/12345"})

         (let [expected {:interceptors     [{:name :echo-url-for}]
                         :method           :get
                         :path             "/echo-url-for/:list-id/:item-id"
                         :path-constraints {:list-id "([^/]+)" :item-id "([^/]+)"}
                         :path-params      {:list-id "abcdef" :item-id "12345"}
                         :path-parts       ["echo-url-for" :list-id :item-id]
                         :route-name       :echo-url-for}
               actual   (edn-parsible-route (:route context))]
           (is= expected actual))

         (is= (mapv edn-parsible-interceptor (:io.pedestal.interceptor.chain/stack context))
           [{:name :echo-url-for}
            {:name :io.pedestal.http.route/router}
            {:name :io.pedestal.http.secure-headers/secure-headers}
            {:name :io.pedestal.http.route/method-param}])


         (let [expected {:request
                         {:async-supported?   true,
                          :body               :tst.tupelo.pedestal/input-stream,
                          :character-encoding "UTF-8",
                          :content-length     0,
                          :content-type       "",
                          :context-path       "",
                          :headers            {"content-length" "0", "content-type" ""},
                          :path-info          "/echo-url-for/abcdef/12345",
                          :path-params        {:list-id "abcdef", :item-id "12345"},
                          :protocol           "HTTP/1.1",
                          :query-string       nil,
                          :remote-addr        "127.0.0.1",
                          :request-method     :get,
                          :scheme             nil,
                          :server-name        nil,
                          :server-port        -1,
                          :uri                "/echo-url-for/abcdef/12345"},
                         :route
                         {:interceptors     [{:name :echo-url-for}],
                          :method           :get,
                          :path             "/echo-url-for/:list-id/:item-id",
                          :path-constraints {:list-id "([^/]+)", :item-id "([^/]+)"},
                          :path-params      {:list-id "abcdef", :item-id "12345"},
                          :path-parts       ["echo-url-for" :list-id :item-id],
                          :route-name       :echo-url-for},
                         :io.pedestal.interceptor.chain/stack
                         [{:name :echo-url-for}
                          {:name :io.pedestal.http.route/router}
                          {:name :io.pedestal.http.secure-headers/secure-headers}
                          {:name :io.pedestal.http.route/method-param}]}
               actual   (edn-parsible-context context)]
           (is= expected actual)) ))


     ))
