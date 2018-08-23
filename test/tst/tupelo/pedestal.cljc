;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.pedestal
  (:use tupelo.core tupelo.pedestal tupelo.test)
  (:require
    [schema.core :as s]
    [tst.tupelo.pedestal-data :as tst-data]
  ))

#?(:clj
   (do

 (dotest
   (is= content-type "Content-Type")
   (is= text-html "text/html"))

 (dotest
   (let [dummy-handler    identity
         dummy-contraints (constantly true)]
     (is (wild-match?
           ["/todo/:list-id/:item" :delete :* :route-name :todo-list-item-create]
           (table-route {:verb         :delete :path "/todo/:list-id/:item" :route-name :todo-list-item-create
                         :interceptors dummy-handler})))

     (is (wild-match? ["/todo/:list-id/:item" :delete :* :route-name :todo-list-item-create :constraints :*]
           (table-route {:verb         :delete :path "/todo/:list-id/:item" :route-name :todo-list-item-create
                         :interceptors dummy-handler
                         :constraints  dummy-contraints})))

     (is (wild-match? ["/todo/:list-id/:item" :delete :* :route-name :todo-list-item-create :constraints :*]
           (table-route {:verb         :delete :path "/todo/:list-id/:item" :route-name :todo-list-item-create
                         :interceptors [dummy-handler]
                         :constraints  dummy-contraints})))))

 (dotest
   ; these all work
   (is= (definterceptor-impl 'alpha '{:enter (fn alpha-enter-fn [ctx] ctx)
                                      :leave (fn alpha-leave-fn [ctx] ctx)})
     '(def alpha {:name  :alpha
                  :enter (fn alpha-enter-fn [ctx] ctx)
                  :leave (fn alpha-leave-fn [ctx] ctx)}))

   (is= '[def aaa {:name :aaa, :enter identity}]
     (seq (definterceptor-impl 'aaa '{:enter identity})))
   (is= '[def bbb {:name :bbb, :leave truthy}]
     (seq (definterceptor-impl 'bbb '{:leave truthy})))

   (throws? (definterceptor-impl 'z1 '{:enter falsey? :zzz "zzz"}))
   (throws? (definterceptor-impl 'z2 '{:zzz "zzz"}))

   ; normal usage
   (definterceptor sample-intc
     {:enter (fn alpha-enter-fn [ctx] ctx)
      :leave (fn alpha-leave-fn [ctx] ctx)})

   ; interceptors are just maps with certain keys
   (is (interceptor? {:name :aaa :enter identity}))
   (is (interceptor? {:name :aaa :leave identity}))
   (is (interceptor? {:name :aaa :error identity}))
   (isnt (interceptor? {:name :aaa :zzz identity})))

 (dotest
   (is (context? tst-data/sample-context))
   (is (request? tst-data/sample-request))

   (is (s/validate Context tst-data/sample-context))
   (is (s/validate Request tst-data/sample-request))

   (isnt (request? tst-data/sample-context))
   (isnt (context? tst-data/sample-request))

   (throws? (s/validate Request tst-data/sample-context))
   (throws? (s/validate Context tst-data/sample-request)) )

 ))
