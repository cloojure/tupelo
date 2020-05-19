;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.lexical
  "Utils for lexical sorting and searching"
  (:require
    [tupelo.core.impl :as impl]
    [tupelo.schema :as tsk]
    [schema.core :as s]
    ))

#?(:cljs (enable-console-print!))

;---------------------------------------------------------------------------------------------------
; comparison-class throws exceptions for some types that might be
; useful to include.

(s/defn comparison-class :- s/Str
  "Returns a string specifying the comparison class to be used for sorting a piece of data."
  [x :- s/Any]
  (cond
    (nil? x) "" ; empty string sorts first of all strings

    ; Lump all numbers together since Clojure's compare can compare them all to each other sensibly.
    (number? x) "Type/Clojure-Number"

    (keyword? x) "Type/Clojure-Keyword"
    (string? x) "Type/Clojure-String"
    (char? x) "Type/Clojure-Character"
    (boolean? x) "Type/Clojure-Boolean"
    (symbol? x) "Type/Clojure-Symbol"

    ; sequential? includes lists, conses, vectors, and seqs of just about any collection, although it is recommended not
    ; to use this to compare seqs of unordered collections like sets or maps (vectors should be OK).  This should be
    ; everything we would want to compare using cmp-seq-lexi below. A clojure.lang.MapEntry also implements clojure.lang.Sequential,
    ; so we can sort {:a 1 :b 2} as if it were [[:a 1] [:b 2]].
    ; TBD: Does it leave anything out?  Include anything it should not?
    (sequential? x) "Type/Clojure-Sequential"

    ; NOTE: record case must preempt `(map? ...)` case below, since all records can be viewed as maps
    (record? x) (tupelo.core.impl/type-name-str x)

    (set? x) "Type/Clojure-IPersistentSet"
    (map? x) "Type/Clojure-IPersistentMap"

    ; #todo what about cljs?  THIS IS UGLY!!!
    ; #?(:clj (.isArray (class x))) #?(:clj "java.util.Arrays")
    (impl/native-array? x) "Type/Platform-Native-Array"

    ; Comparable includes Boolean, Character, String, Clojure refs, and many others.
    #?(:clj  (instance? java.lang.Comparable x)
       :cljs (satisfies? cljs.core/IComparable x))
    (do
      (println :comparable-block x)
      (tupelo.core.impl/type-name-str x))

    :else (throw
            (ex-info
              (str "cc-cmp does not implement comparison of values with class name=" (tupelo.core.impl/type-name-str x))
              {:value x}))))

(defn ^:no-doc compare-seq-lexi
  [cmpr-fn x y]
  (loop [x x
         y y]
    (if (seq x)
      (if (seq y)
        (let [c (cmpr-fn (first x) (first y))]
          (if (zero? c)
            (recur (rest x) (rest y))
            c))
        ; else we reached end of y first, so x > y
        1)
      (if (seq y)
        ; we reached end of x first, so x < y
        -1
        ; Sequences contain same elements.  x = y
        0))))

; The same result can be obtained by calling cmp-seq-lexi on two
; vectors, but cmp-vec-lexi should allocate less memory comparing
; vectors.
(defn ^:no-doc compare-vec-lexi
  [cmpr-fn x y]
  (let [x-len (count x)
        y-len (count y)
        len   (min x-len y-len)]
    (loop [i 0]
      (if (== i len)
        ; If all elements 0..(len-1) are same, shorter vector comes
        ; first.
        (clojure.core/compare x-len y-len)
        (let [c (cmpr-fn (x i) (y i))]
          (if (zero? c)
            (recur (inc i))
            c))))))

(defn ^:no-doc compare-array-lexi
  [cmpr-fn x y]
  (let [x-len (alength x)
        y-len (alength y)
        len   (min x-len y-len)]
    (loop [i 0]
      (if (== i len)
        ; If all elements 0..(len-1) are same, shorter array comes
        ; first.
        (clojure.core/compare x-len y-len)
        (let [c (cmpr-fn (aget x i) (aget y i))]
          (if (zero? c)
            (recur (inc i))
            c))))))

(defn compare-generic
  [x y]
  (let [x-class-str    (comparison-class x)
        y-class-str    (comparison-class y)
        ;>> (println [x-class-str x])
        ;>> (println [y-class-str y])
        compare-result (clojure.core/compare x-class-str y-class-str)]
    (cond
      (not= compare-result 0) compare-result ; different classes

      ; Compare sets to each other as sequences, with elements in
      ; sorted order.
      (= x-class-str "Type/Clojure-IPersistentSet")
      (compare-seq-lexi compare-generic (sort compare-generic x) (sort compare-generic y))

      ; Compare records to each other like maps below.
      ; NOTE: record case must preempt `(map? ...)` case below, since all records can be viewed as maps
      (record? x) (compare-seq-lexi compare-generic
                    (sort-by key compare-generic (seq x))
                    (sort-by key compare-generic (seq y)))

      ; Compare maps to each other as sequences of [key val] pairs, with pairs in order sorted by key.
      ; Since keys are unique
      (= x-class-str "Type/Clojure-IPersistentMap")
      (compare-seq-lexi compare-generic
        (sort-by key compare-generic (seq x)) ; sorted [[xk1 xv1] [xk2 xv2] ...]
        (sort-by key compare-generic (seq y))) ; sorted [[yk1 yv1] [yk2 yv2] ...]

      (= x-class-str "Type/Platform-Native-Array")
      (compare-array-lexi compare-generic x y)

      ; Make a special check for two vectors, since cmp-vec-lexi
      ; should allocate less memory comparing them than
      ; cmp-seq-lexi.  Both here and for comparing sequences, we
      ; must use cc-cmp recursively on the elements, because if
      ; we used compare we would lose the ability to compare
      ; elements with different types.
      (and (vector? x) (vector? y)) (compare-vec-lexi compare-generic x y)

      ; This will compare any two sequences, if they are not both
      ; vectors, e.g. a vector and a list will be compared here.
      (= x-class-str "Type/Clojure-Sequential")
      (compare-seq-lexi compare-generic x y)

      :else (clojure.core/compare x y))))

(s/defn compare-lex :- s/Int
  [a :- tsk/Vec
   b :- tsk/Vec]
  (compare-generic a b))

