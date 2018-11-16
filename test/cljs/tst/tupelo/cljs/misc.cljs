;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.cljs.misc
  "Tupelo - Making Clojure even sweeter"
  (:require
    [tupelo.cljs.misc :as misc]
    [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
    [tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= set= nonblank= testing throws?]] ))

(dotest
  (is= (misc/grouper #"[a-z0-9][A-Z]" "aTaTa")
    [{:groups ["aT"] :match "aT" :index 0 :last-index 2 :input "aTaTa"}
     {:groups ["aT"] :match "aT" :index 2 :last-index 4 :input "aTaTa"}])

  (is= (misc/grouper #"((\d+)-(\d+))" "672-345-456-3212")
    [{:groups ["672-345"  "672-345"  "672"  "345"] :match "672-345"  :index 0 :last-index  7 :input "672-345-456-3212"}
     {:groups ["456-3212" "456-3212" "456" "3212"] :match "456-3212" :index 8 :last-index 16 :input "672-345-456-3212"}]))
