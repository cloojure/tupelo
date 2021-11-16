(ns  ^:test-refresh/focus
  tst.tupelo.x.walk-1
  (:use tupelo.x.walk-1 tupelo.core tupelo.test)
  (:require
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.string :as str]))


(dotest
  (let [m  {:a 1}
        mes (vec m) ; convert map => seq of MapEntry objs
        me1 (first mes)]
    (is= :a (key me1)) ; access key from ME
    (is= 1 (val me1)) ; access val from ME
    ))

(dotest
  (prn :-----------------------------------------------------------------------------)
  (let [intc {:enter (fn [ctx]
                       ; (spy-pretty :enter-in ctx)
                       ; (spy-pretty :enter-out)
                       (cond-it-> ctx
                         (and (= :map-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(* % 10))
                         ))
              :leave (fn [ctx]
                       ; (spy-pretty :leave-in ctx)
                       ; (spy-pretty :leave-out)
                       (cond-it-> ctx
                         (map? (grab :data it)) (update-in it [:data] #(glue % {:zz 99}))

                         (and (= :map-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] inc)
                         ))}]
    (walk-with-context {:a 1 :b 2} intc)
    )
  (nl)
  )
