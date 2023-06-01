;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns  tst.tupelo.string.safe
  #?(:cljs (:require-macros
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty forv]]
    [tupelo.test :refer [deftest testing is verify verify-focus isnt is= isnt= is-set= is-nonblank= is-nonblank-lines=
                          throws? throws-not?  ]]
    [tupelo.string.safe :as safe]
    ))

(verify
  (is= nil (safe/walk-normalize nil))
  (is= (safe/walk-normalize "  Hello THERE!  ")
    "hello there!")

  (let [m {:a 1
           :b "  Hello There "
           :c nil}]
    (is= (safe/walk-whitespace-collapse m)
      {:a 1
       :b "Hello There"
       :c nil})))
