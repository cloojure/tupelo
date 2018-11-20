(ns tst.tupelo.schema
  (:require
    [schema.core :as s]
    [tupelo.schema :as tsk]
    #?@(:clj [[tupelo.test :refer [define-fixture dotest dotest-focus is isnt is= isnt= set= nonblank= testing throws?]]
              [tupelo.core :as t :refer [spy spyx spyxx]]
              ])
    #?@(:cljs [[tupelo.test-cljs :refer [define-fixture dotest is isnt is= isnt= set= nonblank= testing throws?]]
               [tupelo.core :as t :refer [spy spyx spyxx] :include-macros true]
               [tupelo.string :as ts :include-macros true]
               [goog.crypt :as crypt]
               [goog.crypt.Sha1]
               [reagent.format :as rf]
              ])))

; #todo add more tests (& test.check for Eid, etc)
(dotest
  (is= [1]            (s/validate tsk/Single  [1] ))
  (is= [1 2]          (s/validate tsk/Pair    [1 2] ))
  (is= [1 2 3]        (s/validate tsk/Triple  [1 2 3] ))
  (is= [1 2 3 4]      (s/validate tsk/Quad    [1 2 3 4] ))

  (is= [1 2 3]        (s/validate tsk/List [1 2 3] ))

  (throws? (s/validate tsk/Single [1 2] ))
  (throws? (s/validate tsk/Pair   [1] ))
  (throws? (s/validate tsk/List  {:a 1 :b 2} ))
  (throws? (s/validate tsk/List #{:a 1 :b 2} )))

(dotest
  (is= {}           (s/validate tsk/Map  {} ))
  (is= {:a 1}       (s/validate tsk/Map  {:a 1} ))
  (is= {:a 1 :b 2}  (s/validate tsk/Map  {:a 1 :b 2} ))
  (is= {:a 1 :b 2}  (s/validate tsk/Map  {:a 1 :b 2} ))

  (throws? (s/validate tsk/Map  nil ))
  (throws? (s/validate tsk/Map  [1 2 3] ))
  (throws? (s/validate tsk/Map #{1 2 3} )))

(dotest
  (is= #{}           (s/validate tsk/Set  #{} ))
  (is= #{:a 1}       (s/validate tsk/Set  #{:a 1} ))
  (is= #{:a 1 \b 2}  (s/validate tsk/Set  #{:a 1 \b 2} ))
  (is= #{:a 1 \b 2}  (s/validate tsk/Set  #{:a 1 \b 2} ))

  (throws? (s/validate tsk/Set  nil ))
  (throws? (s/validate tsk/Set  [1 2 3] ))
  (throws? (s/validate tsk/Set  {:a 1 :b 2} )))

