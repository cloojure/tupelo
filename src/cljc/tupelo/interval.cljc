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
                            if-java-1-7-plus if-java-1-8-plus
                            when-clojure-1-8-plus when-clojure-1-9-plus when-not-clojure-1-9-plus
                            destruct lazy-gen yield yield-all matches?]]))
  (:require
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.core :as t]))

;-----------------------------------------------------------------------------
; Represents the boundaries of an interval, without specifying open, half-open, or closed.
(defrecord Interval [lower upper])

(defn interval?
  "Returns true iff the arg represents an interval"
  [it] (instance? Interval it))

(s/defn new
  "Creates a new Interval record using the `->Interval` constructor function."
  [lower upper]
  (assert (t/compare-less-equal lower upper))
  (->Interval lower upper))

(s/defn open-contains? :- s/Bool
  "Returns true iff an open interval contains a value such that (lower < L < upper)."
  [interval :- Interval
   val :- s/Any]
  (t/with-map-vals interval [lower upper]
    (t/compare-less lower val upper)))

(s/defn slice-contains? :- s/Bool
  "Returns true iff a 'slice' interval contains a value such that (lower <= L < upper)."
  [interval :- Interval
   val :- s/Any]
  (t/with-map-vals interval [lower upper]
    (and
      (t/compare-less-equal lower val)
      (t/compare-less val upper))))

(s/defn closed-contains? :- s/Bool
  "Returns true iff a closed interval contains a value such that (lower <= L <= upper)."
  [range :- tsk/KeyMap
   val :- s/Any]
  (t/with-map-vals range [lower upper]
    (t/compare-less-equal lower val upper)))

