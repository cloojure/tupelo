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



))
