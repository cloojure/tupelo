;   Copyrigh (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.interval
  "Tupelo - Making Clojure even sweeter"
  ; We use the self-require trick to force separate compilation stages for macros
  ; See "ClojureScript Macro Tower & Loop" by Mike Fikes (2015-12-18)
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs ; http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
     (:require-macros
       [tupelo.core.impl]
       [tupelo.core :refer [it-> cond-it-> some-it->
                            vals->map with-map-vals forv
                            with-spy-indent spyx spyxx spy-pretty spyx-pretty
                            let-spy let-spy-pretty let-some map-let* map-let lazy-cons
                            try-catchall with-exception-default verify
                            destruct lazy-gen yield yield-all matches?]]))
  (:require
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.core :as t])
  (:refer-clojure :exclude [contains?]))

; #todo maybe use shorthand names: slice [), block [], span (), anti-slice (]
; #todo maybe add in Allen's Interval Algebra predicates

;-----------------------------------------------------------------------------
; Represents the boundaries of an interval, without specifying open, half-open, or closed.
(defrecord Interval [type lower upper])

(defn interval?
  "Returns true iff the arg represents an interval"
  [it] (instance? Interval it))

(s/defn new
  "Creates a new generic Interval record using the `->Interval` constructor function."
  [lower upper]
  (assert (t/compare-less-equal lower upper))
  (->Interval :generic lower upper))

(s/defn new-closed
  "Creates a new closed Interval record using the `->Interval` constructor function."
  [lower upper]
  (assert (t/compare-less-equal lower upper))
  (->Interval :closed lower upper))

(s/defn new-slice
  "Creates a new slice Interval record using the `->Interval` constructor function."
  [lower upper]
  (assert (t/compare-less-equal lower upper))
  (->Interval :slice lower upper))

(s/defn new-open
  "Creates a new open Interval record using the `->Interval` constructor function."
  [lower upper]
  (assert (t/compare-less-equal lower upper))
  (->Interval :open lower upper))

(s/defn new-anti-slice
  "Creates a new anti-slice Interval record using the `->Interval` constructor function."
  [lower upper]
  (assert (t/compare-less-equal lower upper))
  (->Interval :anti-slice lower upper))

; #todo maybe add Interval coercion functions ->closed ->slice ->open ->anti-slice
; #todo maybe add Interval predicate functions closed? slice? open? anti-slice?

(s/defn contains? :- s/Bool
  "Returns true iff an interval contains a value such that (lower < L < upper)."
  [interval :- Interval
   val :- s/Any]
  (t/with-map-vals interval [type lower upper]
    (cond
      (= type :closed) (t/compare-less-equal lower val upper)
      (= type :slice) (and
                        (t/compare-less-equal lower val)
                        (t/compare-less val upper))
      (= type :open) (t/compare-less lower val upper)
      (= type :anti-slice) (and
                             (t/compare-less lower val)
                             (t/compare-less-equal val upper))
      :else (throw (ex-info "Invalid Interval type" {:interval interval})))))

(s/defn ->integers :- [s/Int] ; #todo => tupelo.interval
  "For an Interval with integer bounds, returns a vector of all integers within the Interval"
  ([itvl :- Interval] (->integers itvl 1))
  ([itvl :- Interval
    step :- s/Int]
   (t/with-map-vals itvl [lower upper]
     (assert (every? int? [lower upper]))
     (t/keep-if #(contains? itvl %)
       (range lower (inc upper) step)))))

(s/defn ->doubles :- [java.lang.Double] ; #todo => tupelo.interval
  "For an Interval with integer bounds, returns a vector of all integers within the Interval"
  ([itvl :- Interval] (->doubles itvl 1))
  ([itvl :- Interval
    step :- s/Num]
   (t/with-map-vals itvl [lower upper]
     (mapv double
       (t/keep-if #(contains? itvl %)
         (range lower (inc upper) step))))))


