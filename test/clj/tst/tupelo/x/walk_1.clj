(ns ;     ^:test-refresh/focus
  tst.tupelo.x.walk-1
  (:use tupelo.x.walk-1 tupelo.core tupelo.test)
  (:require
    [tupelo.core :as t]
    ))

(dotest   ; -focus
  (let [m   {:a 1}
        mes (vec m) ; convert map => seq of MapEntry objs
        me1 (first mes)]
    (is= :a (key me1)) ; access key from ME
    (is= 1 (val me1)) ; access val from ME

    (is= {:a 1 :b 2} (apply-glue-not-nil [{:a 1} nil {:b 2}]))

    (throws? (grab :a nil)) ; error for `nil` map value
    (is= nil (:a nil)) ; safe for `nil` value
    ))

(dotest   ; -focus
  (let [intc {:enter (fn [ctx]
                       ; (spyx-pretty ctx)
                       (cond-it-> ctx
                         (and (= :set-entry/elem (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(* % 10))))
              :leave (fn [ctx]
                       (cond-it-> ctx
                         (and (= :set-entry/elem (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(inc %))))}]
    (is= (walk-with-context #{2 3} intc)
      #{21 31}))
  (nl))

(dotest-focus
  (let [intc {:enter (fn [ctx]
                       ; (spyx-pretty ctx)
                       (cond-it-> ctx
                         (and (= :list-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(* % 10))))
              :leave (fn [ctx]
                       (cond-it-> ctx
                         (xsequential? (grab :data it)) (update-in it [:data] #(glue % [:zz 99]))

                         (and (= :list-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(inc %))))}]
    (is= (walk-with-context [2 3] intc)
      [21 31 :zz 99]))

  (let [intc {:enter (fn [ctx]
                       ; (spyx-pretty ctx)
                       (cond-it-> ctx
                         (and (= :list-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(* % 10))))
              :leave (fn [ctx]
                       (cond-it-> ctx
                         (= (grab :branch it) :list-entry/idx)
                         (update-in it [:data] #(- %))))}]
    (is= (walk-with-context [2 3] intc)
      [30 20]))

  (nl))

(dotest   ; -focus
  (let [intc {:enter (fn [ctx]
                       ; (spyx-pretty ctx)
                       ; (spy-pretty :enter-in ctx)
                       ; (spy-pretty :enter-out)
                       (cond-it-> ctx
                         (and (= :map-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] #(* % 10))
                         ))
              :leave (fn [ctx]
                       ; (spyx :intc-leave (ctx-depth ctx))
                       ; (spy-pretty :leave-in ctx)
                       ; (spy-pretty :leave-out)
                       (cond-it-> ctx
                         (map? (grab :data it)) (update-in it [:data] #(glue % {:zz 99}))

                         (and (= :map-entry/val (grab :branch it))
                           (int? (grab :data it)))
                         (update-in it [:data] inc)
                         ))}]
    (is= (walk-with-context {:a 1 :b 2} intc)
      {:a 11, :b 21, :zz 99}))

  (nl))

