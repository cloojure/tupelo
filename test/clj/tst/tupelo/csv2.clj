;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns ^:test-refresh/focus tst.tupelo.csv2
  (:use tupelo.csv2 tupelo.core tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.string :as str]
    ))

(dotest
  (let [sample-edn [{:aa-key "aaa" :bb-key "b,b"} ; 2nd val needs to be quoted
                    {:aa-key "aa2" :bb-key "bb2"}]]
    ; is verified-keys working?
    (is= (verified-keys sample-edn) [:aa-key :bb-key])
    (throws?
      (verified-keys
        [{:aa-key "aaa" :bb-X "b,b"} ; "bb" keys don't match
         {:aa-key "aa2" :bb-key "bb2"}]))

    ; 'b,b' value quoted correctly
    (do
      (let [edn-str-keys (walk/postwalk
                           (fn [item]
                             (cond-it-> item
                               (keyword? it) (kw->str it)))
                           sample-edn)]
        (is (str/nonblank-lines=
              (entities->csv-force-quote edn-str-keys)
              (str/quotes->double
                "aa-key,bb-key
                 aaa,'b,b'
                 aa2,bb2 ")))

        (let [result (str/quotes->single (entities->csv-force-quote sample-edn))]
          (nl)
          (println :result-----------------------------------------------------------------------------)
          (println result)
          (is (str/nonblank-lines=  result
             "':aa-key',':bb-key'
              'aaa','b,b'
              'aa2','bb2' ")))
        (nl)

        ))))


