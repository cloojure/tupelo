(ns tst.tupelo.core
  (:require
    #?@(:clj [[tupelo.test   :refer [dotest is isnt is= isnt= testing define-fixture]]
              [tupelo.core :as t]])
    #?@(:cljs [[tupelo.test-cljs :refer [dotest is isnt is= isnt= testing define-fixture]]
               [tupelo.core :as t :include-macros true]])
  ))

(define-fixture :once
     {:enter (fn [ctx] (println "*** TEST ONCE *** - enter "))
      :leave (fn [ctx] (println "*** TEST ONCE *** - leave "))})
;--------------------------------------------------------------------------------------------------

(dotest
  (println "tst.tupelo.core test 1")
  (is= 2 (+ 1 1))

  (is (t/truthy? true))
  (is (t/truthy? 5))
  (is (t/falsey? false))
  (is (t/falsey? nil))
  )

(dotest
  (let [inf-rng-1 (map inc (range))]
    (is= 42 (t/only [42]))
    (is= :x (t/only [:x]))
    (is= "hello" (t/only ["hello"]))

    ; #todo #wip
    ;(throws? IllegalArgumentException (t/only []))
    ;(throws? IllegalArgumentException (t/only [:x :y]))
    ;(throws? IllegalArgumentException (t/only inf-rng-1))

    (is= [1 2 3] (t/onlies [[1] [2] [3]]))
    ;(throws? (t/onlies [[1] [2] [3 4]]))
    ;(throws? (t/onlies [[1] [] [3]]))

    (is= 5 (t/only2 [[5]]))
    ;(throws? (t/only2 [[1 2]]))
    ;(throws? (t/only2 [[1] [2]]))

    (is (t/single? [42]))
    (is (t/single? [:x]))
    (is (t/single? ["hello"]))
    (isnt (t/single? []))
    (isnt (t/single? [:x :y]))
    (isnt (t/single? inf-rng-1))

    (is (t/pair? [42 43]))
    (is (t/pair? [:x :y]))
    (is (t/pair? ["hello" "there"]))
    (isnt (t/pair? []))
    (isnt (t/pair? [:y]))
    (isnt (t/pair? inf-rng-1))

    (is (t/triple? [42 43 44]))
    (is (t/triple? [:x :y :z]))
    (is (t/triple? ["hello" "there" "you"]))
    (isnt (t/triple? []))
    (isnt (t/triple? [:y]))
    (isnt (t/triple? [:x :y]))
    (isnt (t/triple? inf-rng-1))

    (is (t/quad? [42 43 44 45]))
    (is (t/quad? [:x :y :z :99]))
    (is (t/quad? ["hello" "there" "again" "you"]))
    (isnt (t/quad? []))
    (isnt (t/quad? [:x]))
    (isnt (t/quad? [:x :y]))
    (isnt (t/quad? [:x :y :z]))
    (isnt (t/quad? inf-rng-1))))

