;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tst.tupelo.tag
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros
             [tupelo.misc]
             [tupelo.testy]
             ))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [clojure.string :as str]
    [tupelo.tag :as tt :refer [IVal ITag ITagMap]]
    [tupelo.core :as t :refer [spyx spyx-pretty forv unlazy
                               ]]

    [tupelo.testy :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture ]]
    ))

; (defn tv [t v] (td/->TagVal t v))

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

    (is (t/map-plain? (sorted-map))) ; expected
    (is (t/map-plain? {:a 1})) ; solution
    (isnt (t/map-plain? dum5))) ;solution

  (let [vv [1 2 3]]
    (isnt (map? vv))
    (isnt (t/map-plain? vv)))

  (let [sa  (t/->sym :a)
        tva (tt/->TagVal :sym sa)]
    (is= (t/unlazy tva) {:tag :sym, :val (quote a)})))


