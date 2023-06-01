(ns tst.tupelo.schema
  ;---------------------------------------------------------------------------------------------------
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros [tupelo.test]))
  (:require
    [clojure.test] ; sometimes this is required - not sure why
    [schema.core :as s]
    [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty]]
    [tupelo.schema :as tsk]
    [tupelo.test :refer [deftest testing is dotest isnt is= isnt= is-set= is-nonblank=
                          throws? throws-not? define-fixture]]
    ))

; #todo add more tests (& test.check for Eid, etc)
(dotest
  (is= [1] (s/validate tsk/Single [1]))
  (is= [1 2] (s/validate tsk/Pair [1 2]))
  (is= [1 2 3] (s/validate tsk/Triple [1 2 3]))
  (is= [1 2 3 4] (s/validate tsk/Quad [1 2 3 4]))

  (is= [1 2 3] (s/validate tsk/List [1 2 3]))

  (throws? (s/validate tsk/Single [1 2]))
  (throws? (s/validate tsk/Pair [1]))
  (throws? (s/validate tsk/List {:a 1 :b 2}))
  (throws? (s/validate tsk/List #{:a 1 :b 2})))

(dotest
  (is= {} (s/validate tsk/Map {}))
  (is= {:a 1} (s/validate tsk/Map {:a 1}))
  (is= {:a 1 :b 2} (s/validate tsk/Map {:a 1 :b 2}))
  (is= {:a 1 :b 2} (s/validate tsk/Map {:a 1 :b 2}))

  (throws? (s/validate tsk/Map nil))
  (throws? (s/validate tsk/Map [1 2 3]))
  (throws? (s/validate tsk/Map #{1 2 3})))

(dotest
  (is= #{} (s/validate tsk/Set #{}))
  (is= #{:a 1} (s/validate tsk/Set #{:a 1}))
  (is= #{:a 1 \b 2} (s/validate tsk/Set #{:a 1 \b 2}))
  (is= #{:a 1 \b 2} (s/validate tsk/Set #{:a 1 \b 2}))

  (throws? (s/validate tsk/Set nil))
  (throws? (s/validate tsk/Set [1 2 3]))
  (throws? (s/validate tsk/Set {:a 1 :b 2})))

