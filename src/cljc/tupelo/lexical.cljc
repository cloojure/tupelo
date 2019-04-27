;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.lexical
  "Utils for lexical sorting and searching"
  (:refer-clojure :exclude [compare])  ; #todo
  #?(:clj (:require
            [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab]]
            [tupelo.schema :as tsk]
            [clojure.data.avl :as avl]
            [schema.core :as s]
            ))
  #?(:cljs (:require
             [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab] ] ; #todo :include-macros true
             [tupelo.schema :as tsk]
             [clojure.data.avl :as avl]
             [schema.core :as s]
             ))
  )

#?(:cljs (enable-console-print!))

(def Val tsk/Vec)
(def Set (class (avl/sorted-set 1 2 3)))
(def Map (class (avl/sorted-map :a 1 :b 2 :c 3)))

;---------------------------------------------------------------------------------------------------
;; comparison-class throws exceptions for some types that might be
;; useful to include.

(defn comparison-class [x]
  (cond (nil? x) ""
        ;; Lump all numbers together since Clojure's compare can
        ;; compare them all to each other sensibly.
        (number? x) "java.lang.Number"

        ;; sequential? includes lists, conses, vectors, and seqs of
        ;; just about any collection, although it is recommended not
        ;; to use this to compare seqs of unordered collections like
        ;; sets or maps (vectors should be OK).  This should be
        ;; everything we would want to compare using cmp-seq-lexi
        ;; below.  TBD: Does it leave anything out?  Include anything
        ;; it should not?
        (sequential? x) "clojure.lang.Sequential"

        (set? x) "clojure.lang.IPersistentSet"
        (map? x) "clojure.lang.IPersistentMap"
        (.isArray (class x)) "java.util.Arrays"

        ;; Comparable includes Boolean, Character, String, Clojure
        ;; refs, and many others.
        (instance? Comparable x) (.getName (class x))
        :else (throw
               (ex-info (format "cc-cmp does not implement comparison of values with class %s"
                                (.getName (class x)))
                        {:value x}))))

(defn cmp-seq-lexi
  [cmpf x y]
  (loop [x x
         y y]
    (if (seq x)
      (if (seq y)
        (let [c (cmpf (first x) (first y))]
          (if (zero? c)
            (recur (rest x) (rest y))
            c))
        ;; else we reached end of y first, so x > y
        1)
      (if (seq y)
        ;; we reached end of x first, so x < y
        -1
        ;; Sequences contain same elements.  x = y
        0))))

;; The same result can be obtained by calling cmp-seq-lexi on two
;; vectors, but cmp-vec-lexi should allocate less memory comparing
;; vectors.
(defn cmp-vec-lexi
  [cmpf x y]
  (let [x-len (count x)
        y-len (count y)
        len (min x-len y-len)]
    (loop [i 0]
      (if (== i len)
        ;; If all elements 0..(len-1) are same, shorter vector comes
        ;; first.
        (clojure.core/compare x-len y-len)
        (let [c (cmpf (x i) (y i))]
          (if (zero? c)
            (recur (inc i))
            c))))))

(defn cmp-array-lexi
  [cmpf x y]
  (let [x-len (alength x)
        y-len (alength y)
        len (min x-len y-len)]
    (loop [i 0]
      (if (== i len)
        ;; If all elements 0..(len-1) are same, shorter array comes
        ;; first.
        (clojure.core/compare x-len y-len)
        (let [c (cmpf (aget x i) (aget y i))]
          (if (zero? c)
            (recur (inc i))
            c))))))


(defn cc-cmp
  [x y]
  (let [x-cls (comparison-class x)
        y-cls (comparison-class y)
        c (clojure.core/compare x-cls y-cls)]
    (cond (not= c 0) c  ; different classes

          ;; Compare sets to each other as sequences, with elements in
          ;; sorted order.
          (= x-cls "clojure.lang.IPersistentSet")
          (cmp-seq-lexi cc-cmp (sort cc-cmp x) (sort cc-cmp y))

          ;; Compare maps to each other as sequences of [key val]
          ;; pairs, with pairs in order sorted by key.
          (= x-cls "clojure.lang.IPersistentMap")
          (cmp-seq-lexi cc-cmp
                        (sort-by key cc-cmp (seq x))
                        (sort-by key cc-cmp (seq y)))

          (= x-cls "java.util.Arrays")
          (cmp-array-lexi cc-cmp x y)

          ;; Make a special check for two vectors, since cmp-vec-lexi
          ;; should allocate less memory comparing them than
          ;; cmp-seq-lexi.  Both here and for comparing sequences, we
          ;; must use cc-cmp recursively on the elements, because if
          ;; we used compare we would lose the ability to compare
          ;; elements with different types.
          (and (vector? x) (vector? y)) (cmp-vec-lexi cc-cmp x y)

          ;; This will compare any two sequences, if they are not both
          ;; vectors, e.g. a vector and a list will be compared here.
          (= x-cls "clojure.lang.Sequential")
          (cmp-seq-lexi cc-cmp x y)

          :else (clojure.core/compare x y))))


(s/defn compare-lex :- s/Int
  [a :- tsk/Vec
   b :- tsk/Vec ]
  (cc-cmp a b))
;---------------------------------------------------------------------------------------------------


(s/defn ->sorted-set :- Set
  "Converts a set into a lexically-sorted set"
  ([] (->sorted-set #{}))
  ([some-set :- (s/cond-pre tsk/Set tsk/Vec)]
    (into (avl/sorted-set-by compare-lex) some-set)))
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









