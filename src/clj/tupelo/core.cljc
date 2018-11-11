;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.core
  "Tupelo - Making Clojure even sweeter"
  (:refer-clojure :exclude [map seqable?])
  (:require
    [tupelo.impl :as i]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    #?@(:clj [ [cheshire.core :as cheshire]] ))
  #?(:clj (:import [java.io PrintStream ByteArrayOutputStream]))
)

; #todo unify terminolgy (atom/ref/agent)
;   -> reset!/ref-set => set
;   -> swap!/alter => update

;(defmacro xxx [& forms]
;  `(i/xxx ~@forms))

; #todo need (defkw :fred) and (kw :fred) to catch errors like
; (when (= person :frid)  ; (kw :frid) -> throws
;    (println "Hi Barney!"))

; WARNING:  cannot use Plumatic schema for functions that may receive an infinite lazy sequence
; as input.  See:  https://groups.google.com/forum/#!topic/clojure/oM1PH4sXzoA

; #todo need (dbg :awt122 (some-fn 1 2 3)) -> (spy :msg :awt122 (some-fn 1 2 3))






; #todo replace clojure.core/map => tupelo.lazy/map if (t/refer-tupelo :strict)
; #todo replace clojure.core/map : not lazy; can add one of :trunc or :lazy modifiers
; (map + (range 5))
; (map + 0 (range 5))
; (map + (range 5) :lazy)
; (map vector [:a :b :c] (range 9) :trunc)  ; error w/o :trunc
;(defn mapper [& args]   ; alts:  mapr  onto  morph  vmap
;  "An eager version of clojure.core/map
;   Use (zip ... :trunc) if you want to truncate all inputs to the lenght of the shortest.
;   Use (zip ... :lazy)  if you want it to be lazy.  "
;  (apply clojure.core/map args))
; #todo (map-indexed ... :lazy)   vmap-indexed
; #todo (mapcat ... :lazy)    vmapcat
; #todo (for ... :lazy)       vfor
; #todo (concat ... :lazy)    vconcat


















; #todo ***** toptop ***********************************************************************************
#?(:clj (do


; #todo ----- gogo ----------------------------------------------------------------------------------







; #todo:  add in clear-nil-entries to recursively delete all k-v pairs where val is nil or empty?

; #todo:  create safe-map ns with non-nil/non-dup versions of assoc-in, update-in, dissoc-in (&
; singles). Basically like compiler-like guarentees against misspellings, duplicate entries, missing
; entries.

;(defmacro ^:private safe-> ; #todo: remove this
;  [expr & forms]
;  (throw (RuntimeException. "Obsolete: replace with:  (validate not-nil? (-> <expr> <forms> ))" )))







(defmacro lazy-gen
  "Creates a 'generator function' that returns a lazy seq of results
  via `yield` (a la Python)."
  [& forms]
  `(i/lazy-gen ~@forms))

(defmacro yield
  "Within a 'generator function' created by `lazy-gen`, populates the
  result lazy seq with the supplied value (a la Python). Returns the value."
  [value]
  `(i/yield ~value))

(defmacro yield-all
  "Within a 'generator function' created by `lazy-gen`, populates the
  result lazy seq with each item from the supplied collection. Returns the collection."
  [values]
  `(i/yield-all ~values))

; (defn round [dblVal :incr   (/ 1 3)]            ; #todo add
;   (let [factor (Math/pow 10 *digits*)]
;     (it-> dblVal
;           (* it factor)
;           (Math/round it)
;           (/ it factor))))
; (defn round [dblVal :exp -2]
;   (round dblVal :incr (Math/pow 10 *digits*)))
; (defn round [dblVal :digits 2]
;   (round dblVal :exp (- *digits*)))

;; #todo delete old definition
;(defn set=
;  "Returns true if two collections are equal when converted into sets."
;  [& colls]
;  (assert (< 1 (count colls))) ; #todo add msg
;  (apply = (mapv set colls)))

(defmacro with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defmacro with-system-err-str
  "Evaluates exprs in a context in which JVM System/err is bound to a fresh
  PrintStream.  Returns the string created by any nested printing calls."
  [& body]
  `(let [baos# (ByteArrayOutputStream.)
         ps# (PrintStream. baos#) ]
     (System/setErr ps#)
     ~@body
     (System/setErr System/err)
     (.close ps#)
     (.toString baos#)))

(defmacro with-system-out-str
  "Evaluates exprs in a context in which JVM System/out is bound to a fresh
  PrintStream.  Returns the string created by any nested printing calls."
  [& body]
  `(let [baos# (ByteArrayOutputStream.)
         ps# (PrintStream. baos#) ]
     (System/setOut ps#)
     ~@body
     (System/setOut System/out)
     (.close ps#)
     (.toString baos#)))

(defn ex-msg
  "Returns the message from an exception => (.getMessage exception)"
  [exception]
  (.getMessage exception))

(defn ex-stacktrace
  "Returns the stacktrace from an exception "
  [exception]
  (with-system-err-str
    (.printStackTrace exception)))



(defn chars-thru
  "Given two characters (or numerical equivalents), returns a seq of characters
  (inclusive) from the first to the second.  Characters must be in ascending order."
  [start-char stop-char] (i/chars-thru start-char stop-char))

(defn pretty-str
  "Returns a string that is the result of clojure.pprint/pprint"
  [arg] (i/pretty-str arg))

(defn pretty
  "Shortcut to clojure.pprint/pprint. Returns it (1st) argument."
  [& args] (apply i/pretty args))

;                                               "1234.4567.89ab.cdef"  also valid for read
; #todo need conversion from Long -> hex string="1234-4567-89ab-cdef" (& inverse)
; #todo need rand-id/randid/rid/rid-str (rand id) -> 64 bit hex string="1234-4567-89ab-cdef"
; i[12] = Random.nextInt(); bytes += i[12].toHexString()

 ; #todo fix usage docs

(defn submap?
  "Returns true if the map entries (key-value pairs) of one map are a subset of the entries of
   another map.  Similar to clojure.set/subset?"
  [inner-map outer-map] (i/submap? inner-map outer-map))

(defn validate-map-keys ; #todo docstring, README
  [tst-map valid-keys ] (i/validate-map-keys tst-map valid-keys))

(defn map-keys ; #todo docstring, README
  "Transforms each key in a map using the supplied `tx-fn`:

    (t/map-keys {1 :a 2 :b 3 :c} inc)                  =>  {  2 :a   3 :b 4   :c}
    (t/map-keys {1 :a 2 :b 3 :c} {1 101 2 202 3 303})  =>  {101 :a 202 :b 303 :c}"
  [map-in tx-fn & tx-args ]
  (apply i/map-keys map-in tx-fn tx-args))

(defn map-vals ; #todo docstring, README
  "Transforms each value in a map using the supplied `tx-fn`:

      (t/map-vals {:a 1 :b 2 :c 3} inc)                  =>  {:a 2,   :b 3,   :c 4}
      (t/map-vals {:a 1 :b 2 :c 3} {1 101 2 202 3 303})  =>  {:a 101, :b 202, :c 303} "
  [map-in tx-fn & tx-args]
  (apply i/map-vals map-in tx-fn tx-args))

(defn wild-item?
  "Returns true if any element in a nested collection is the wildcard :*"
  [item] (i/wild-item? item))

(defn val=
  "Compares values for equality using clojure.core/=, treating records as plain map values:

      (defrecord SampleRec [a b])
      (assert (val= (->SampleRec 1 2) {:a 1 :b 2}))   ; fails for clojure.core/= "
  [& vals]
  (apply i/val= vals))

(defmacro matches?
  "A shortcut to clojure.core.match/match to aid in testing.  Returns true if the data value
   matches the pattern value.  Underscores serve as wildcard values. Usage:

     (matches? pattern & values)

   sample:

     (matches?  [1 _ 3] [1 2 3] )         ;=> true
     (matches?  {:a _ :b _       :c 3}
                {:a 1 :b [1 2 3] :c 3}
                {:a 2 :b 99      :c 3}
                {:a 3 :b nil     :c 3} )  ;=> true

   Note that a wildcald can match either a primitive or a composite value."
  [pattern & values]
  `(i/matches? ~pattern ~@values))

; #todo: add (throwed? ...) for testing exceptions

; #todo readme
(s/defn starts-with? :- s/Bool
  "Returns true when the initial elements of coll match those of tgt"
  [coll tgt-in]     ; #todo schema
  (let [tgt-vec (vec tgt-in)
        tgt-len (count tgt-vec) ]
    (if (< (count coll) tgt-len)
      false
      (let [coll-vals (take tgt-len coll)]
        (= coll-vals tgt-vec)))))

; #todo readme
(defn index-using
  "Finds the first index N where (< N (count coll)) such that (pred (drop N coll)) is truthy.
  Returns `nil` if no match found."
  [pred coll]
  (let [all-vals (vec coll)
        num-vals (count all-vals)]
    (loop [i 0]
      (if (<= num-vals i)
        nil         ; did not find match
        (let [curr-vals (subvec all-vals i)]
          (if (pred curr-vals)
            i
            (recur (inc i))))))))

; #todo readme
(defn split-using    ; #todo schema
  "Splits a collection based on a predicate with a collection argument.
  Finds the first index N such that (pred (drop N coll)) is true. Returns a length-2 vector
  of [ (take N coll) (drop N coll) ]. If pred is never satisified, [ coll [] ] is returned."
  [pred coll]
  (let [N (index-using pred (vec coll))]
    (if (nil? N)
      [coll []]
      [(take N coll) (drop N coll)])))

; #todo readme
(defn split-match    ; #todo schema
  "Splits a collection src by matching with a sub-sequence tgt of length L.
  Finds the first index N such that (= tgt (->> coll (drop N) (take L))) is true.
  Returns a length-2 vector of [ (take N coll) (drop N coll) ].
  If no match is found, [ coll [] ] is returned."
  [coll tgt]
  (split-using
    (fn [partial-coll] (starts-with? partial-coll (vec tgt)))
    (vec coll)))

; #todo readme
(s/defn partition-using
  "Partitions a collection into vector of segments based on a predicate with a collection argument.
  The first segment is initialized by removing the first element from `values`, with subsequent
  elements similarly transferred as long as `(pred remaining-values)` is falsey. When
  `(pred remaining-values)` becomes truthy, the algorithm begins building the next segment.
  Thus, the first partition finds the smallest N (< 0 N) such that (pred (drop N values))
  is true, and constructs the segment as (take N values). If pred is never satisified,
  [values] is returned."
  [pred   :- s/Any    ; a predicate function  taking a list arg
   values :- tsk/List ]
  (loop [vals   (vec values)
         result []]
    (if (empty? vals)
      result
      (let [
        out-first  (take 1 vals)
        [out-rest unprocessed] (split-using pred (rest vals))
        out-vals   (i/glue out-first out-rest)
        new-result (i/append result out-vals)
      ]
        (recur unprocessed new-result)))))

;(defn refer-tupelo  ; #todo delete?
;  "Refer a number of commonly used tupelo.core functions into the current namespace so they can
;   be used without namespace qualification."
;  [& args]
;  (refer 'tupelo.core :only
;   '[spy spyx spyx-pretty spyxx
;     let-spy let-spy-pretty
;     with-spy-indent with-spy-enabled check-spy-enabled
;     not-nil? not-empty? has-some? has-none?
;
;     forv map-let* map-let
;     when-clojure-1-8-plus when-clojure-1-9-plus
;     glue glue-rows
;     macro? chars-thru
;     append prepend grab dissoc-in fetch fetch-in
;     submap? submap-by-keys submap-by-vals keyvals keyvals-seq validate-map-keys map-keys map-vals
;     validate only it-> keep-if drop-if zip zip* zip-lazy indexed
;     strcat nl pretty pretty-str json->edn edn->json clip-str range-vec thru rel= all-rel=
;     drop-at insert-at replace-at idx
;     starts-with? int->kw kw->int
;     xfirst xsecond xthird xfourth xlast xbutlast xrest xreverse
;     kw->sym kw->str str->sym str->kw str->chars sym->kw sym->str
;     split-using split-match partition-using
;     wild-item? submatch? val=
;     increasing? increasing-or-equal? ->vector unwrap xvec
;     fibonacci-seq fibo-thru fibo-nth unnest
;     with-exception-default lazy-cons lazy-gen yield yield-all
;    ] )
;  (let [flags (set args)]
;    (when (contains? flags :dev)
;      ; (refer 'tupelo.impl :only '[   ])
;    )
;    (when (contains? flags :strict)
;      ; #todo unlink/relink troublesome clojure.core stuff
;    )))

;---------------------------------------------------------------------------------------------------
; DEPRECATED functions

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(i/when-not-clojure-1-9-plus
  (defn ^{:deprecated "1.9.0-alpha5"} seqable?  ; from clojure.contrib.core/seqable
    "Returns true if (seq x) will succeed, false otherwise."
    [x]
    (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x))))

;---------------------------------------------------------------------------------------------------
; Another benefit of test-all:  don't need "-test" suffix like in lein test:
; ~/tupelo > lein test :only tupelo.core
; lein test user
; Ran 0 tests containing 0 assertions.     ***** Nearly silent failure *****
; 0 failures, 0 errors.
;
; ~/tupelo > lein test :only tst.tupelo.core
; lein test tst.tupelo.core
; Ran 8 tests containing 44 assertions.     ***** Runs correctly *****
; 0 failures, 0 errors.
;
; ~/tupelo > lein test :only tst.tupelo.core/convj-test
; lein test tst.tupelo.core
; Ran 1 tests containing 3 assertions.
; 0 failures, 0 errors.
;
; #todo:  add run-tests with syntax like lein test :only
;   (run-tests 'tst.tupelo.core)              ; whole namespace
;   (run-tests 'tst.tupelo.core/convj-test)   ; one function only
;
; #todo make it handle either tst.orig.namespace or orig.namespace-test
; #todo make it a macro to accept unquoted namespace values
; #todo delete this
;(defn ^:deprecated ^:no-doc test-all
;  "Convenience fn to reload a namespace & the corresponding test namespace from disk and
;  execute tests in the REPL.  Assumes canonical project test file organization with
;  parallel src/... & test/tst/... directories, where a 'tst.' prefix is added to all src
;  namespaces to generate the cooresponding test namespace.  Example:
;
;    (test-all 'tupelo.core 'tupelo.csv)
;
;  This will reload tupelo.core, tst.tupelo.core, tupelo.csv, tst.tupelo.csv and
;  then execute clojure.test/run-tests on both of the test namespaces."
;  [& ns-list]
;  (let [test-ns-list (for [curr-ns ns-list]
;                       (let [curr-ns-test (symbol (str "tst." curr-ns))]
;                         (println (str "testing " curr-ns " & " curr-ns-test))
;                         (require curr-ns curr-ns-test :reload)
;                         curr-ns-test))
;        ]
;    (println "-----------------------------------------------------------------------------")
;    (apply clojure.test/run-tests test-ns-list)
;    (println "-----------------------------------------------------------------------------")
;    (newline) ))

))
