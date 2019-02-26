;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.math
  (:require
    [tupelo.math :as math]
    #?@(:clj [[schema.core :as s]
              [tupelo.test :refer [define-fixture dotest dotest-focus is isnt is= isnt= is-set= is-nonblank= testing throws?]]
              [tupelo.core :as t :refer [spy spyx spyxx it-> rel=]]
              ])
    #?@(:cljs [[schema.core :as s]
               [tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= is-set= is-nonblank= testing throws?]]
               [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
               ]))
  #?(:clj (:import [java.lang Byte Integer]))
)

#?(:cljs (enable-console-print!))

#?(:clj
   (do

     (defn bigdec? [x] (decimal? x) )
     (defn bigint? [x] (= (type x)) )

     (dotest

       (spyx (bigdec 5))
       (spyx (type (bigdec 5)))
       (spyx (type (double 5)))
       (spyx (type (int 5)))
       (spyx (type  5N))
       (spyx (type  5M))
       (spyx (type  (java.math.BigInteger. "5")))
       (spyx (type  (java.math.BigDecimal. "5")))


       )

))
