;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tst.tupelo.hierarchy
  (:use tupelo.hierarchy tupelo.core tupelo.test))

(dotest
  (throws? (validate-item-types [:a :b 'c]))
  (throws-not? (validate-item-types [:a :b :c]))
  (throws-not? (validate-item-types ['a 'b 'c]))

  (let [h (it-> (make-hierarchy)
            (derive it ::rect ::shape)
            (derive it ::square ::rect)
            (derive it ::ellipse ::shape)
            (derive it ::circle ::ellipse))
        ]
    (is= (lineage-to-item h ::rect)
      #{::rect ::shape})
    (is= (lineage-to-item h ::square)
      #{::square ::rect ::shape})
    (is= (common-lineage h ::rect ::square)
      #{::shape ::rect})

    (is= (greatest-common-derivation h ::square ::shape) ::shape)
    (is= (greatest-common-derivation h ::rect ::shape) ::shape)
    (is= (greatest-common-derivation h ::rect ::square) ::rect)
    (is= (greatest-common-derivation h ::square ::rect) ::rect)
    (is= (greatest-common-derivation h ::circle ::square) ::shape)
    (is= (greatest-common-derivation h ::circle ::rect) ::shape)
    (is= (greatest-common-derivation h ::circle ::shape) ::shape))
  )
