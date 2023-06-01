;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.tagval
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.test]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.string :as str]
    [tupelo.tagval :as tv ]
    [tupelo.core :as t :refer [spyx spyx-pretty forv unlazy
                               ]]

    [tupelo.test :refer [deftest testing is dotest dotest-focus isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? ]]
    ))

; (defn tv [t v] (td/->TagVal t v))

(defrecord DummyRec [dum])

(dotest
  (let [tv     (tv/new :a 1)
        tv-str (with-out-str (println tv))]
    (is= {:a 1} tv)
    (is= :a (tv/tag tv))
    (is= 1 (tv/val tv))
    (is= "{:a 1}" (str/trim tv-str))
    (is (tv/tagval? tv))
    (isnt (tv/tagval? 1))
    (is= 1 (tv/untagged tv))
    (is= 1 (tv/untagged 1)))

  (let [dum5 (->DummyRec 5)]
    (is (map? {:a 1})) ; expected
    (isnt (record? {:a 1})) ; expected
    (is (record? dum5)) ; expected
    (is (map? dum5)) ; *** problem ***

    (is (t/xmap? (sorted-map))) ; expected
    (is (t/xmap? {:a 1})) ; solution
    (isnt (t/xmap? dum5))) ;solution

  (let [vv [1 2 3]]
    (isnt (map? vv))
    (isnt (t/xmap? vv)))

  (let [sa  (t/->sym :a)
        tv-sym-a (tv/new :sym sa)]
    (is= tv-sym-a {:sym (quote a)})))

(dotest
  (is= {:x 1} (tv/map-val inc {:x 0}))
  (let [tvs     [{:x 0}
                 {:x 1}
                 {:x 2}]
        tvs-new (tv/mapv inc tvs)]
    (is= tvs-new [{:x 1} {:x 2} {:x 3}])))

;---------------------------------------------------------------------------------------------------
(comment ; old record version
  (defrecord DummyRec [dum]
    IVal (<val [this] dum))

  (dotest
    (let [tv     (tt/->TagVal :a 1)
          tv-str (with-out-str (println tv))]
      (is= {:tag :a :val 1} (unlazy tv))
      (is= :a (tt/<tag tv))
      (is= 1 (tt/<val tv))
      (is= "<:a 1>" (str/trim tv-str))
      (is (tt/tagval? tv))
      (isnt (tt/tagval? 1))
      (is= 1 (tt/untagged tv))
      (is= 1 (tt/untagged 1)))

    (let [dum5 (->DummyRec 5)]
      (is (map? {:a 1})) ; expected
      (isnt (record? {:a 1})) ; expected
      (is (record? dum5)) ; expected
      (is (map? dum5)) ; *** problem ***

      (is (t/xmap? (sorted-map))) ; expected
      (is (t/xmap? {:a 1})) ; solution
      (isnt (t/xmap? dum5))) ;solution

    (let [vv [1 2 3]]
      (isnt (map? vv))
      (isnt (t/xmap? vv)))

    (let [sa  (t/->sym :a)
          tva (tt/->TagVal :sym sa)]
      (is= (t/unlazy tva) {:tag :sym, :val (quote a)})))
  )

