  ;   Copyright (c) Alan Thompson. All rights reserved.
  ;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
  ;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
  ;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
  ;   bound by the terms of this license.  You must not remove this notice, or any other, from this
  ;   software.
  (ns tst.tupelo.interval
    ;---------------------------------------------------------------------------------------------------
    ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
    ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
    #?(:cljs (:require-macros
               [tupelo.core]
               [tupelo.misc]
               [tupelo.test]
               ))
    (:require
      [clojure.test] ; sometimes this is required - not sure why
      [tupelo.core :as t :refer [spy spyx spyxx spy-pretty spyx-pretty nl
                                 vals->map xmap? forv glue keep-if]]
      [tupelo.interval :as interval]
      [tupelo.test :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                            throws? throws-not? ]])
    )

#?(:cljs (enable-console-print!))

#?(:clj
   (dotest

     (is= (< 0 1.0) true)
     (is= (= 0 1.0) false)
     (is= (> 0 1.0) false)

     (is= (< 1 1.0) false)
     (is= (= 1 1.0) false) ; *****  single-equals `=` doesn't work for integer and floating point values (never!) *****
     (is= (== 1 1.0) true) ; *****  double-equals `==` works for numbers (only) in different categories *****
     (is= (> 1 1.0) false)

     (is= (< 2 1.0) false)
     (is= (= 2 1.0) false)
     (is= (> 2 1.0) true)
     ))

(dotest
  ; `compare` works correctly for numbers in different categories (eg int vs float)
  (is= (compare 0 1.0) -1)
  (is= (compare 1 1.0) 0)
  (is= (compare 2 1.0) 1)

  (isnt (interval/interval? [7 8]))

  (throws? (interval/new-slice 5 1))
  (throws? (interval/new-closed 5 1))
  (throws? (interval/new-open 5 1))
  (throws? (interval/new-anti-slice 5 1))

  (throws-not? (interval/new-slice 5 5))
  (throws-not? (interval/new-closed 5 5))
  (throws-not? (interval/new-open 5 5))
  (throws-not? (interval/new-anti-slice 5 5))

  ; generic interval cannot be queried for membership
  (let [itvl    (interval/new 1 9)]
    (is (interval/interval? itvl))
    (throws? (interval/contains-value? itvl 5)))
  (let [itvl    (interval/new "aa" "xxx")] ; can accept string or char boundaries
    (is (interval/interval? itvl))
    (throws? (interval/contains-value? itvl "bb")))

  (let [itvl    (interval/new-slice 1.0 5.0) ; float interval bounds vs integer values
        members (keep-if #(interval/contains-value? itvl %) (range 10))]
    (is (interval/interval? itvl))
    (is= members [1 2 3 4]))

  (let [itvl    (interval/new-closed 1.0 5.0) ; float interval bounds vs integer values
        members (keep-if #(interval/contains-value? itvl %) (range 10))]
    (is (interval/interval? itvl))
    (is= members [1 2 3 4 5]))

  (let [itvl    (interval/new-open 1.0 5.0) ; float interval bounds vs integer values
        members (keep-if #(interval/contains-value? itvl %) (range 10))]
    (is (interval/interval? itvl))
    (is= members [2 3 4]))

  (let [itvl    (interval/new-anti-slice 1.0 5.0) ; float interval bounds vs integer values
        members (keep-if #(interval/contains-value? itvl %) (range 10))]
    (is (interval/interval? itvl))
    (is= members [2 3 4 5]))

  (let [itvl    (interval/new-closed \d \f)
        members (keep-if #(interval/contains-value? itvl %) (t/chars-thru \a \z))]
    (is (interval/interval? itvl))
    (is (interval/contains-value? itvl \e)) ; characters are comparable
    (is= members [\d \e \f])
    (isnt (interval/contains-value? itvl \b)))

  (let [itvl    (interval/new-closed "d" "f")
        members (keep-if #(interval/contains-value? itvl %) (mapv str (t/chars-thru \a \z)))]
    (is (interval/interval? itvl))
    (is (interval/contains-value? itvl "e")) ; characters are comparable
    (is= members ["d" "e" "f"])
    (isnt (interval/contains-value? itvl "b")))
  )

(dotest
  (is= [6 7] (interval/->integers (interval/new-open 5 8)))
  (is= [5 6 7] (interval/->integers (interval/new-slice 5 8)))
  (is= [6 7 8] (interval/->integers (interval/new-anti-slice 5 8)))
  (is= [5 6 7 8] (interval/->integers (interval/new-closed 5 8)))

  (is= [3 5 7] (interval/->integers (interval/new-open 1 8) 2))
  (is= [1 3 5 7] (interval/->integers (interval/new-slice 1 8) 2))
  (is= [3 5 7] (interval/->integers (interval/new-anti-slice 1 8) 2))
  (is= [1 3 5 7] (interval/->integers (interval/new-closed 1 8) 2)))

(dotest
  (is= [2.0 3.0] (interval/->doubles (interval/new-open 1 4)))
  (is= [1.0 2.0 3.0] (interval/->doubles (interval/new-slice 1 4)))
  (is= [2.0 3.0 4.0] (interval/->doubles (interval/new-anti-slice 1 4)))
  (is= [1.0 2.0 3.0 4.0] (interval/->doubles (interval/new-closed 1 4)))

  (is= [1.5 2.0 2.5 3.0 3.5] (interval/->doubles (interval/new-open 1 4) 0.5))
  (is= [1.0 1.5 2.0 2.5 3.0 3.5] (interval/->doubles (interval/new-slice 1 4) 0.5))
  (is= [1.5 2.0 2.5 3.0 3.5 4.0] (interval/->doubles (interval/new-anti-slice 1 4) 0.5))
  (is= [1.0 1.5 2.0 2.5 3.0 3.5 4.0] (interval/->doubles (interval/new-closed 1 4) 0.5)))

