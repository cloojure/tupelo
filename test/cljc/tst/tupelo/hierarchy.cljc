;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.hierarchy
;---------------------------------------------------------------------------------------------------
;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
#?(:cljs (:require-macros
           [tupelo.testy]
           ))
(:require
  [clojure.test] ; sometimes this is required - not sure why
  [tupelo.hierarchy :as th]
  [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty forv vals->map glue truthy? falsey?
                             ]]
  [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                        throws? throws-not? define-fixture]]))

(dotest
  (throws? (th/validate-item-types [:a :b 'c]))
  (throws-not? (th/validate-item-types [:a :b :c]))
  (throws-not? (th/validate-item-types ['a 'b 'c]))

  (let [h (t/it-> (make-hierarchy)
            (derive it ::rect ::shape)
            (derive it ::square ::rect)
            (derive it ::ellipse ::shape)
            (derive it ::circle ::ellipse))
        ]
    (is= (th/lineage-to-item h ::rect)
      #{::rect ::shape})
    (is= (th/lineage-to-item h ::square)
      #{::square ::rect ::shape})
    (is= (th/common-lineage h ::rect ::square)
      #{::shape ::rect})

    (is= (th/greatest-common-derivation h ::square ::shape) ::shape)
    (is= (th/greatest-common-derivation h ::rect ::shape) ::shape)
    (is= (th/greatest-common-derivation h ::rect ::square) ::rect)
    (is= (th/greatest-common-derivation h ::square ::rect) ::rect)
    (is= (th/greatest-common-derivation h ::circle ::square) ::shape)
    (is= (th/greatest-common-derivation h ::circle ::rect) ::shape)
    (is= (th/greatest-common-derivation h ::circle ::shape) ::shape))
  )
