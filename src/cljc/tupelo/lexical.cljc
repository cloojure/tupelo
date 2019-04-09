;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.lexical
  "Utils for lexical sorting and searching"
  (:refer-clojure :exclude [compare])
#?(:clj (:require
          [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab]]
          [tupelo.schema :as tsk]
          [clojure.data.avl :as avl]
          [schema.core :as s]
          ))
#?(:cljs (:require
           [tupelo.core :include-macros true :as t :refer  [spy spyx spyxx spyx-pretty grab]]
           [tupelo.schema :as tsk]
           [clojure.core :as core]
           [clojure.data.avl :as avl]
           [schema.core :as s]
         )))

(def Val tsk/Vec)
(def Set (class (avl/sorted-set 1 2 3)))
(def Map (class (avl/sorted-map :a 1 :b 2 :c 3)))

; #todo generalize to allow `nil` as an ultimate lower bound?
(s/defn compare :- s/Int
  "Performs a lexical comparison of 2 sequences, sorting as follows:
      [1]
      [1 :a]
      [1 :b]
      [1 :b 3]
      [2]
      [3]
      [3 :y] "
  [a :- tsk/Vec
   b :- tsk/Vec]
  (cond
    (= a b) 0
    (empty? a) -1
    (empty? b) 1
    :else (let [a0 (t/xfirst a)
                b0 (t/xfirst b)]
            (if (= a0 b0)
              (compare (t/xrest a) (t/xrest b))
              (clojure.core/compare a0 b0)))))

(s/defn ->sorted-set :- tsk/Set
  "Converts a set into a lexically-sorted set"
  [some-set :- (s/cond-pre tsk/Set tsk/Vec)]
  (into (avl/sorted-set-by compare) some-set))
; #todo add (->sorted-map <map>)        => (into (sorted-map) <map>)
; #todo add (->sorted-vec <sequential>) => (vec (sort <vec>))

(s/defn bound-lower :- tsk/Vec
  "Given a lexical value as a vector such as [1 :a], returns a lower bound like [1]"
  [val :- tsk/Vec]
  (when (zero? (count val))
    (throw (ex-info "Cannot find lower bound for empty vec" {:val val})))
  (t/xbutlast val))

(s/defn prefix-match? :- s/Bool
  "Returns true if the sample value equals the pattern when truncated to the same length"
  [pattern :- Val
   sample :- Val]
  (= pattern (t/xtake (count pattern) sample)))

(s/defn split-key-prefix :- {s/Keyword Set}
  "Like clojure.data.avl/split-key, but allows prefix matches. Given a lexically sorted set like:
    #{[:a 1]
      [:a 2]
      [:a 3]
      [:b 1]
      [:b 2]
      [:b 3]
      [:c 1]
      [:c 2]}
   splits data by prefix match for patterns like [:b], returning a map of 3 sorted sets like:
  {:smaller #{[:a 1]
              [:a 2]
              [:a 3]}
   :matches #{[:b 1]
              [:b 2]
              [:b 3]}
   :larger  #{[:c 1]
              [:c 2]} ]
      "
  [match-val :- Val
   lex-set :- Set]
  (let [[smaller-set found-val larger-set] (avl/split-key match-val lex-set)
        result (if (nil? found-val)
                 (let [[matches-seq larger-seq] (split-with #(prefix-match? match-val %) larger-set)]
                   {:smaller smaller-set
                    :matches (->sorted-set matches-seq)
                    :larger  (->sorted-set larger-seq)})
                 {:smaller smaller-set
                  :matches (avl/sorted-set found-val)
                  :larger  larger-set})]
   ;(s/validate Set (grab :smaller result))
   ;(s/validate Set (grab :matches result))
   ;(s/validate Set (grab :larger result))
    result))















