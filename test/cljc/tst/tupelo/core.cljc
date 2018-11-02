(ns tst.tupelo.core
  (:require
    #?@(:clj [[tupelo.test   :refer [define-fixture dotest is isnt is= isnt= testing throws?]]
              [tupelo.core :as t]])
    #?@(:cljs [[tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= testing throws?]]
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
    (throws? (t/only []))
    (throws? (t/only [:x :y]))
    (throws? (t/only inf-rng-1))

    (is= [1 2 3] (t/onlies [[1] [2] [3]]))
    (throws? (t/onlies [[1] [2] [3 4]]))
    (throws? (t/onlies [[1] [] [3]]))

    (is= 5 (t/only2 [[5]]))
    (throws? (t/only2 [[1 2]]))
    (throws? (t/only2 [[1] [2]]))

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

(dotest
  (let [inf-rng-1 (map inc (range))
        tst-map   (t/glue (sorted-map) {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6})]

    (throws? (t/xtake 1 []))

    (is= [1] (t/xtake 1 [1]))
    (is= [1] (t/xtake 1 [1 2]))
    (is= [1] (t/xtake 1 inf-rng-1))
    (is= [1 2] (t/xtake 2 [1 2]))
    (is= [1 2] (t/xtake 2 inf-rng-1))
    (is= {:a 1} (t/xtake 1 tst-map))
    (is= {:a 1 :b 2} (t/xtake 2 tst-map))

    (throws? (t/xfirst []))
    (is= 1 (t/xfirst [1]))
    (is= 1 (t/xfirst [1 2]))
    (is= 1 (t/xfirst inf-rng-1))
    ;(is= {:a 1} (t/xfirst tst-map))

    (throws? (t/xsecond []))
    (throws? (t/xsecond [1]))
    (is= 2 (t/xsecond [1 2]))
    (is= 2 (t/xsecond [1 2 3]))
    (is= 2 (t/xsecond [1 2 3 4]))
    (is= 2 (t/xsecond inf-rng-1))
    ;(is= {:b 2} (t/xsecond tst-map))

    (throws? (t/xthird []))
    (throws? (t/xthird [1]))
    (throws? (t/xthird [1 2]))
    (is= 3 (t/xthird [1 2 3]))
    (is= 3 (t/xthird [1 2 3 4]))
    (is= 3 (t/xthird inf-rng-1))
    ;(is= {:b 92} (t/xthird tst-map))

    (throws? (t/xfourth []))
    (throws? (t/xfourth [1]))
    (throws? (t/xfourth [1 2]))
    (throws? (t/xfourth [1 2 3]))
    (is= 4 (t/xfourth [1 2 3 4]))
    (is= 4 (t/xfourth [1 2 3 4 5]))
    (is= 4 (t/xfourth inf-rng-1))
    ;(is= {:b 92} (t/xfourth tst-map))

    (throws? (t/xlast nil))
    (throws? (t/xlast []))
    (is= 5 (t/xlast [1 2 3 4 5]))
    ;(is= {:b 92} (t/xlast tst-map))

    (is= [1 2 3 4] (t/xbutlast [1 2 3 4 5]))
    (is= [] (t/xbutlast [1]))
    (throws? (t/xbutlast []))
    (throws? (t/xbutlast nil))
    ;(is= {:b 92} (t/xbutlast tst-map))

    (throws? (t/xrest []))
    (is= [] (t/xrest [1]))
    (is= [2] (t/xrest [1 2]))
    (is= [2 3] (t/xrest [1 2 3]))
    (is= [2 3 4] (t/xrest [1 2 3 4]))
    (is= 2 (first (t/xrest inf-rng-1)))

    (throws? (t/xvec nil))
    (is= [] (t/xvec []))
    (is= [1] (t/xvec '(1)))
    (is= [1 2] (t/xvec [1 2]))
    ))

