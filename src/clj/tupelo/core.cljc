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

(defn truthy?
  "Returns true if arg is logical true (neither nil nor false); otherwise returns false."
  [arg] (i/truthy? arg))

(defn falsey?
  "Returns true if arg is logical false (either nil or false); otherwise returns false. Equivalent
   to (not (truthy? arg))."
  [arg] (i/falsey? arg))

(defn nl
  "Abbreviated name for `newline` "
  [] (i/nl))

(defn unlazy ; #todo need tests & docs. Use for datomic Entity?
  "Converts a lazy collection to a concrete (eager) collection of the same type."
  [coll] (i/unlazy coll))

(defn has-length?
  "Returns true if the collection has the indicated length. Does not hang for infinite sequences."
  [coll n] (i/has-length? coll n))

(defn only
  "Ensures that a sequence is of length=1, and returns the only value present.
  Throws an exception if the length of the sequence is not one.
  Note that, for a length-1 sequence S, (first S), (last S) and (only S) are equivalent."
  [coll] (i/only coll))

(defn onlies
  "Given an outer collection of length-1 collections, returns a sequence of the unwrapped values.
    (onlies  [ [1] [2] [3] ])  =>  [1 2 3]
    (onlies #{ [1] [2] [3] })  => #{1 2 3} "
  [coll] (i/onlies coll))

(defn only2
  "Given a collection like `[[5]]`, returns `5`.  Equivalent to `(only (only coll))`."
  [coll] (i/only2 coll))

(defn single?
  "Returns true if the collection contains a single item.`"
  [coll] (i/single? coll))

(defn pair?
  "Returns true if the collection contains exactly 2 items."
  [coll] (i/pair? coll))

(defn triple?
  "Returns true if the collection contains exactly 3 items."
  [coll] (i/triple? coll))

(defn quad?
  "Returns true if the collection contains exactly 4 items."
  [coll] (i/quad? coll))

(defn xtake
  "Returns the first n values from a collection.  Returns map for map colls.
  Throws if empty."
  [n coll] (i/xtake n coll))

; WARNING:  cannot use Plumatic schema for functions that may receive an infinite lazy sequence
; as input.  See:  https://groups.google.com/forum/#!topic/clojure/oM1PH4sXzoA
(defn xfirst
  "Returns the first value in a list or vector. Throws if empty."
  [coll] (i/xfirst coll))
(defn xsecond
  "Returns the second value in a list or vector. Throws if (< len 2)."
  [coll] (i/xsecond coll))
(defn xthird
  "Returns the third value in a list or vector. Throws if (< len 3)."
  [coll] (i/xthird coll))
(defn xfourth
  "Returns the fourth value in a list or vector. Throws if (< len 4)."
  [coll] (i/xfourth coll))
(defn xlast
  "Returns the last value in a list or vector. Throws if empty."
  [coll] (i/xlast coll))
(defn xbutlast
  "Returns a vector of all but the last value in a list or vector. Throws if empty."
  [coll] (i/xbutlast coll))
(defn xrest
  "Returns the last value in a list or vector. Throws if empty."
  [coll] (i/xrest coll))
(defn xreverse
  "Returns all but the last value in a list or vector. Throws if empty."
  [coll] (i/xreverse coll))

(defn xvec
  "Converts a collection into a vector. Throws if given nil."
  [coll] (i/xvec coll))

(defn glue
  "Glues together like collections:

     (glue [1 2] [3 4] [5 6])                -> [1 2 3 4 5 6]
     (glue {:a 1} {:b 2} {:c 3})             -> {:a 1 :c 3 :b 2}
     (glue #{1 2} #{3 4} #{6 5})             -> #{1 2 6 5 3 4}
     (glue \"I\" \" like \" \\a \" nap!\" )  -> \"I like a nap!\"

  If you want to convert to a sorted set or map, just put an empty one first:

     (glue (sorted-map) {:a 1} {:b 2} {:c 3})      -> {:a 1 :b 2 :c 3}
     (glue (sorted-set) #{1 2} #{3 4} #{6 5})      -> #{1 2 3 4 5 6}

   If there are duplicate keys when using glue for maps or sets, then \"the last one wins\":

     (glue {:band :VanHalen :singer :Dave}  {:singer :Sammy}) "
  [& colls]
  (apply i/glue colls))

(defn glue-rows
  " Convert a vector of vectors (2-dimensional) into a single vector (1-dimensional).
  Equivalent to `(apply glue ...)`"
  [coll-2d] (i/glue-rows coll-2d))

(defn increasing?
  "Returns true iff the vectors are in (strictly) lexicographically increasing order
    [1 2]  [1]        -> false
    [1 2]  [1 1]      -> false
    [1 2]  [1 2]      -> false
    [1 2]  [1 2 nil]  -> true
    [1 2]  [1 2 3]    -> true
    [1 2]  [1 3]      -> true
    [1 2]  [2 1]      -> true
    [1 2]  [2]        -> true "
  [a b] (i/increasing? a b))

(defn increasing-or-equal?
  "Returns true iff the vectors are in (strictly) lexicographically increasing-or-equal order
    [1 2]  [1]        -> false
    [1 2]  [1 1]      -> false
    [1 2]  [1 2]      -> true
    [1 2]  [1 2 nil]  -> true
    [1 2]  [1 2 3]    -> true
    [1 2]  [1 3]      -> true
    [1 2]  [2 1]      -> true
    [1 2]  [2]        -> true "
  [a b] (i/increasing-or-equal? a b))

(defn not-nil?
  "Returns true if arg is not nil; false otherwise. Equivalent to (not (nil? arg)),
   or the poorly-named clojure.core/some? "
  [arg] (i/not-nil? arg))

(defn not-empty?
  "For any collection coll, returns true if coll contains any items; otherwise returns false.
   Equivalent to (not (empty? coll))."
  [coll] (i/not-empty? coll))

(defn kw->sym       ; #todo ->sym  ->str  -> chars ->kw
  "Converts a keyword to a symbol"
  [arg] (i/kw->sym arg))
(defn kw->str
  "Converts a keyword to a string"
  [arg] (i/kw->str arg))

(defn str->kw
  "Converts a string to a keyword"
  [arg] (i/str->kw arg))
(defn str->sym
  "Converts a string to a symbol"
  [arg] (i/str->sym arg))
(defn str->chars
  "Converts a string to a vector of chars"
  [arg] (i/str->chars arg))

(defn sym->kw
  "Converts a symbol to a keyword"
  [arg] (i/sym->kw arg))
(defn sym->str
  "Converts a symbol to a string"
  [arg] (i/sym->str arg))

(defn int->kw [arg]
  (keyword (str arg)))
#?(:clj
   (defn kw->int [arg]
     (Integer/parseInt (kw->str arg))))
#?(:cljs
   (defn kw->int [arg]
     (js/parseInt (kw->str arg) 10)))

(defn prettify
  "Recursively walks a data structure and returns a prettified version.
  Converts all lists to vectors. Converts all maps & sets to sorted collections."
  [coll] (i/prettify coll))

(defmacro with-spy-indent
  "Increments indentation level of all spy, spyx, or spyxx expressions within the body."
  [& forms] `(i/with-spy-indent ~@forms))

(defmacro with-spy-enabled ; #todo README & test
  [tag & forms]
  `(i/with-spy-enabled ~tag ~@forms))

(defmacro check-spy-enabled ; #todo README & test
  [tag & forms]
  `(i/check-spy-enabled ~tag ~@forms))

; #todo need (dbg :awt122 (some-fn 1 2 3)) -> (spy :msg :awt122 (some-fn 1 2 3))

(defn spy
  "A form of (println ...) to ease debugging display of either intermediate values in threading
   forms or function return values. There are three variants.  Usage:

    (spy :msg <msg-string>)
        This variant is intended for use in either thread-first (->) or thread-last (->>)
        forms.  The keyword :msg is used to identify the message string and works equally
        well for both the -> and ->> operators. Spy prints both <msg-string>  and the
        threading value to stdout, then returns the value for further propogation in the
        threading form. For example, both of the following:
            (->   2
                  (+ 3)
                  (spy :msg \"sum\" )
                  (* 4))
            (->>  2
                  (+ 3)
                  (spy :msg \"sum\" )
                  (* 4))
        will print 'sum => 5' to stdout.

    (spy <msg-string> <value>)
        This variant is intended for simpler use cases such as function return values.
        Function return value expressions often invoke other functions and cannot be
        easily displayed since (println ...) swallows the return value and returns nil
        itself.  Spy will output both <msg-string> and the value, then return the value
        for use by further processing.  For example, the following:
            (println (* 2
                       (spy \"sum\" (+ 3 4))))
      will print:
            sum => 7
            14
      to stdout.

    (spy <value>)
        This variant is intended for use in very simple situations and is the same as the
        2-argument arity where <msg-string> defaults to 'spy'.  For example (spy (+ 2 3))
        prints 'spy => 5' to stdout.  "
  [& args] (apply i/spy args))

#?(:clj
   (do
     ; #todo add test & README
     (defn json->edn
       "Shortcut to cheshire.core/parse-string"
       [arg]
       (cheshire/parse-string arg true)) ; true => keywordize-keys

     ; #todo add test & README
     (defn edn->json
       "Shortcut to cheshire.core/generate-string"
       [arg]
       (cheshire/generate-string arg))
     ))
#?(:cljs
   (do
     ; #todo add test & README
     (defn json->edn
       "Convert from json -> edn"
       [arg]
       (js->clj (.parse js/JSON arg) :keywordize-keys true)) ; true => keywordize-keys

     ; #todo add test & README
     (defn edn->json
       "Convert from edn -> json "
       [arg]
       (.stringify js/JSON (clj->js arg)))
     ))

(defmacro forv
  "Like clojure.core/for but returns results in a vector.   Not lazy."
  [& forms] `(i/forv ~@forms))

(defn keep-if
  "Returns a vector of items in coll for which (pred item) is true (alias for clojure.core/filter)"
  [pred coll] (i/keep-if pred coll))

(defn drop-if
  "Returns a vector of items in coll for which (pred item) is false (alias for clojure.core/remove)"
  [pred coll] (i/drop-if pred coll))

(defn has-some?
  "For any predicate pred & collection coll, returns true if (pred x) is logical true for at least one x in
   coll; otherwise returns false.  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (i/has-some? pred coll))

(defn has-none?
  "For any predicate pred & collection coll, returns false if (pred x) is logical true for at least one x in
   coll; otherwise returns true.  Equivalent to clojure.core/not-any?, but inverse of has-some?."
  [pred coll]
  (i/has-none? pred coll))

(defmacro spyx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& forms] `(i/spyx ~@forms))

(defmacro spyxx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expression, printing both the expression, its type, and its value to stdout, then returns the value."
  [expr] `(i/spyxx ~expr))

(defmacro spyx-pretty
  "Like `spyx` but with pretty printing (clojure.pprint/pprint)"
  [& forms] `(i/spyx-pretty ~@forms))

(defmacro spy-pretty
  "Like `spyx-pretty` but without printing the original form"
  [& forms] `(i/spy-pretty ~@forms))

(defmacro let-spy
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& forms] `(i/let-spy ~@forms))

(defmacro let-spy-pretty
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& forms] `(i/let-spy-pretty ~@forms))

(defmacro let-some
  "Threads forms as with `when-some`, but allow more than 1 pair of binding forms."
  [bindings & forms]
  `(i/let-some ~bindings ~@forms))

(defn contains-elem?
  "For any collection coll & element tgt, returns true if coll contains at least one
  instance of tgt; otherwise returns false. Note that, for maps, each element is a
  vector (i.e MapEntry) of the form [key value]."
  [coll elem] (i/contains-elem? coll elem))
(defn contains-key?
  "For any map or set, returns true if elem is a map key or set element, respectively"
  [map-or-set elem ] (i/contains-key? map-or-set elem))
(defn contains-val?
  "For any map, returns true if elem is present in the map for at least one key."
  [map elem] (i/contains-val? map elem))

(defn strcat
  "Recursively concatenate all arguments into a single string result."
  [& args] (apply i/strcat args))

(defn print-versions [] ; #todo need CLJS version
  #?(:clj
     (let [version-str (format "Clojure %s    Java %s"
                         (clojure-version) (System/getProperty "java.version"))
           num-hyphen  (+ 6 (count version-str))
           hyphens     (strcat (repeat num-hyphen \-))
           version-str (strcat "   " version-str)]
       (nl)
       (println hyphens)
       (println version-str)
       (println hyphens)))
  #?(:cljs
     (let [version-str (str "ClojureScript " *clojurescript-version* )
           num-hyphen  (+ 6 (count version-str))
           hyphens     (strcat (repeat num-hyphen \-))
           version-str (strcat "   " version-str)]
       (nl)
       (println hyphens)
       (println version-str)
       (println hyphens))) )

; #todo ***** toptop *****
#?(:clj (do

(defmacro when-clojure-1-8-plus [& forms]
  `(i/when-clojure-1-8-plus ~@forms))

(defmacro when-clojure-1-9-plus [& forms]
  `(i/when-clojure-1-9-plus ~@forms))

; #todo ----- gogo -----

(defn rand-elem
  "Returns a random element from a collection"
  [coll] (i/rand-elem coll))

(defn lexical-compare
  "Performs a lexical comparison of 2 sequences, sorting as follows:
      [1]
      [1 :a]
      [1 :b]
      [1 :b 3]
      [2]
      [3]
      [3 :y] "
  [a b] (i/lexical-compare a b))

(defn zip*
  "Usage:  (zip* context & colls)
  where context is a map with default values:  {:strict true}
  Not lazy. "
  [context & colls]
  (apply i/zip* context colls))

(defn zip
  "Zips together vectors producing a vector of tuples (like Python zip). Not lazy.
  Example:

     (zip [:a :b :c] [1 2 3]) ->  [ [:a 1] [:b 2] [:c 3] ]

   ***** WARNING - will hang for infinite length inputs ***** "
  [& args]
  (apply i/zip args))

(defn zip-lazy
  "Usage:  (zip-lazy coll1 coll2 ...)
      (zip-lazy xs ys zs) -> [ [x0 y0 z0]
                               [x1 y1 z1]
                               [x2 y2 z2]
                               ... ]

  Returns a lazy result. Will truncate to the length of the shortest collection.
  A convenience wrapper for `(map vector coll1 coll2 ...)`.  "
  [& colls]
  (apply i/zip-lazy colls))

(defn indexed
  "Given one or more collections, returns a sequence of indexed tuples from the collections:
      (indexed xs ys zs) -> [ [0 x0 y0 z0]
                              [1 x1 y1 z1]
                              [2 x2 y2 z2]
                              ... ] "
  [& colls]
  (apply i/indexed colls))

(defn chan->lazy-seq
  "Accepts a core.async channel and returns the contents as a lazy list."
  [chan]
  (i/chan->lazy-seq chan))

(defmacro map-let*
  "Usage:  (map-let* ctx bindings & forms)

  where ctx is a map with default values:
    {:strict true
     :lazy   false}"
  [context bindings & forms] `(i/map-let* ~context ~bindings ~@forms) )

(defmacro map-let
  "Usage:
    (map-let bindings & forms)

  Given bindings and forms like `(map-let [x xs, y ys, ...] (+ x y))`, will iterate over the
  collections [xs ys ...] assigning successive values of each collection to [x y ...], respectively.
  The local symbols [x y ...] can then be used in `forms` to generate the output mapping.
  Will throw if collections are not all of the same length. Not lazy."
  [bindings & forms] `(i/map-let ~bindings ~@forms))

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

(defn fetch-in
  "A fail-fast version of clojure.core/get-in. When invoked as (fetch-in the-map keys-vec),
   returns the value associated with keys-vec as for (clojure.core/get-in the-map keys-vec).
   Throws an Exception if the path keys-vec is not present in the-map."
  [the-map keys-vec] (i/fetch-in the-map keys-vec))

(defn fetch
  "A fail-fast version of keyword/map lookup.  When invoked as (fetch the-map :the-key),
   returns the value associated with :the-key as for (clojure.core/get the-map :the-key).
   Throws an Exception if :the-key is not present in the-map."
  [the-map the-key] (i/fetch the-map the-key))

(defn grab
  "A fail-fast version of keyword/map lookup.  When invoked as (grab :the-key the-map),
   returns the value associated with :the-key as for (clojure.core/get the-map :the-key).
   Throws an Exception if :the-key is not present in the-map."
  [the-key the-map] (i/grab the-key the-map))

(defn submap-by-keys
  "Returns a new map containing entries with the specified keys. Throws for missing keys,
  unless `:missing-ok` is specified. Usage:

      (submap-by-keys {:a 1 :b 2} #{:a   }             )  =>  {:a 1}
      (submap-by-keys {:a 1 :b 2} #{:a :z} :missing-ok )  =>  {:a 1}
  "
  [map-arg keep-keys & opts] (apply i/submap-by-keys map-arg keep-keys opts))

(defn submap-by-vals
  "Returns a new map containing entries with the specified vals. Throws for missing vals,
  unless `:missing-ok` is specified. Usage:

      (submap-by-vals {:a 1 :b 2 :A 1} #{1  }             )  =>  {:a 1 :A 1}
      (submap-by-vals {:a 1 :b 2 :A 1} #{1 9} :missing-ok )  =>  {:a 1 :A 1} "
  [map-arg keep-vals & opts] (apply i/submap-by-vals map-arg keep-vals opts))

;-----------------------------------------------------------------------------
; Java version stuff

(s/defn java-version :- s/Str
  []
  (System/getProperty "java.version"))

(s/defn java-version-matches? :- s/Bool
  "Returns true if Java version exactly matches supplied string."
  [version-str :- s/Str]
  (str/starts-with? (java-version) version-str))

(s/defn java-version-min? :- s/Bool
  "Returns true if Java version is at least as great as supplied string.
  Sort is by lexicographic (alphabetic) order."
  [version-str :- s/Str]
  (ts/increasing-or-equal? version-str (java-version)))

(defn is-java-1-7? [] (java-version-matches? "1.7"))
(defn is-java-1-8? [] (java-version-matches? "1.8"))

(defn is-java-1-7-plus? [] (java-version-min? "1.7"))
(defn is-java-1-8-plus? [] (java-version-min? "1.8"))

(defmacro if-java-1-7-plus
  "If JVM is Java 1.7 or higher, evaluates if-form into code. Otherwise, evaluates else-form."
  [if-form else-form]
  (if (is-java-1-7-plus?)
    `(do ~if-form)
    `(do ~else-form)))

(defmacro if-java-1-8-plus
  "If JVM is Java 1.8 or higher, evaluates if-form into code. Otherwise, evaluates else-form."
  [if-form else-form]
  (if (is-java-1-8-plus?)
      `(do ~if-form)
      `(do ~else-form)))

; #todo need min-java-1-8  ???


(defn validate
  "(validate tst-fn tst-val)
  Used to validate intermediate results. Returns tst-val if the result of
  (tst-fn tst-val) is truthy.  Otherwise, throws IllegalArgumentException."
  [tst-fn tst-val] (i/validate tst-fn tst-val))

(defmacro verify
  "(verify <some-expr>)
  Used to verify intermediate results. Returns value of <some-expr> if the result
  is truthy.  Otherwise, throws IllegalArgumentException."
  [form]
  `(i/verify ~form))

(defmacro vals->map ; #todo -> README
  "Called with a list of symbols like `(vals->map a b c)` returns a map
   like {:a a :b b :c c}.

       (let [a 1
             b 2
             c 3]
         (vals->map a b c))  ;=>  {:a 1 :b 2 :c 3} }

   See `with-map-vals` for simple destructuring of such maps."
  [& symbols]
  `(i/vals->map ~@symbols))

(defmacro with-map-vals ; #todo -> README
  "Given a map like {:a 1 :b 2 :c 3} (such as generated by `(vals->map a b c)`),
  performs safe `let` destructuring using `grab` like:

     (let [some-map  {:a 1 :b 2 :c 3} } ]
       (with-map-vals some-map [a b c]
          (+ a b c)))  ;=>  6

  `with-map-vals` is safe for typos since `grab` will throw is the requrested map key is not present.
  See `vals->map` for simple creation of labelled data maps."
  [the-map items-vec & forms]
  `(i/with-map-vals ~the-map ~items-vec ~@forms))

(defmacro destruct  ; #todo flesh out
  "Natural destructuring:
     (let [data {:a 1
                 :b {:c 3
                     :d 4}}]
       ...
       (destruct [data {:a ?
                        :b {:c ?}}]
       ...
   then can use local values  a=1, c=3.  With vector data:
     (let [data [1 2 3 4 5]]
       ...
       (destruct [data [a b c]]
        ...
     then can use local values a=1 b=2 c=3.  Can use `(restruct)`, `(restruct data)`, or `(restruct-all)`
     to re-structure & return original data shape using current values."
  [bindings & forms]
  `(i/destruct ~bindings ~@forms))

(defn restruct
  "within a `(destruct [<data> <shape>] ...) form, `(restruct)` or `(restruct <data>)` causes re-structuring
   & return of original data shape using current values."
  [& args] (throw (ex-info "restruct: illegal usage - should never get here." args)))

(defn restruct-all
  "within a `(destruct [data-1 <shape-1>
                        data-2 <shape-2] ...) form, causes re-structuring & return of original data shapes using
  current values as with (vals->map data-1 data-2 ...)"
  [& args] (throw (ex-info "restruct-all: illegal usage - should never get here." args)))

(defn keyvals
  "For any map m, returns the (alternating) keys & values of m as a vector, suitable for reconstructing m via
   (apply hash-map (keyvals m)). (keyvals {:a 1 :b 2} => [:a 1 :b 2] "
  [m] (i/keyvals m))

(s/defn keyvals-seq :- [s/Any]
  "For any map m, returns the (alternating) keys & values of m as a vector, suitable for reconstructing m via
   (apply hash-map (keyvals m)). (keyvals {:a 1 :b 2} => [:a 1 :b 2]

     Usage:  (keyvals-seq ctx) ctx-default: {:missing-ok false}
             (keyvals-seq the-map the-keys) "
  ([ctx :- tsk/KeyMap ]
    (let [defaults {:missing-ok false}]
      (i/keyvals-seq (into defaults ctx))))
  ([the-map :- tsk/KeyMap
    the-keys :- [s/Any]]
    (keyvals-seq (vals->map the-map the-keys))) )

(defn unwrap
  "Works with the `->vector` function to unwrap vectors/lists to insert
  their elements as with the unquote-spicing operator (~@). Examples:

      (->vector 1 2 3 4 5 6 7 8 9)              =>  [1 2 3 4 5 6 7 8 9]
      (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9)   =>  [1 2 3 4 5 6 7 8 9] "
  [data] (i/unwrap data))

(defn ->vector
  "Wraps all args in a vector, as with `clojure.core/vector`. Will (recursively) recognize
  any embedded calls to (unwrap <vec-or-list>) and insert their elements as with the
  unquote-spicing operator (~@). Examples:

      (->vector 1 2 3 4 5 6 7 8 9)              =>  [1 2 3 4 5 6 7 8 9]
      (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9)   =>  [1 2 3 4 5 6 7 8 9] "
  [& args] (apply i/->vector args))

(defn unnest
  "Given any set of arguments including vectors, maps, sets, & scalars, performs a depth-first
  recursive walk returning scalar args (int, string, keyword, etc) in a single 1-D vector."
  [& values] (apply i/unnest values))

(defn macro?
  "Returns true if a quoted symbol resolves to a macro. Usage:

    (println (macro? 'and))  ;=> true "
  [s] (i/macro? s))

(defn append
  "Given a sequential object (vector or list), add one or more elements to the end."
  [listy & elems] (apply i/append listy elems))
(defn prepend
  "Given a sequential object (vector or list), add one or more elements to the beginning"
  [& args] (apply i/prepend args))

(defn drop-at
  "Removes an element from a collection at the specified index."
  [coll index] (i/drop-at coll index))
(defn insert-at
  "Inserts an element into a collection at the specified index."
  [coll index elem] (i/insert-at coll index elem))
(defn replace-at
  "Replaces an element in a collection at the specified index."
  [coll index elem] (i/replace-at coll index elem))

(defn idx
  "Indexes into a vector, allowing negative index values"
  [coll index-val] (i/idx coll index-val) )

(s/defn dissoc-in :- s/Any
  "A sane version of dissoc-in that will not delete intermediate keys.
   When invoked as (dissoc-in the-map [:k1 :k2 :k3... :kZ]), acts like
   (clojure.core/update-in the-map [:k1 :k2 :k3...] dissoc :kZ). That is, only
   the map entry containing the last key :kZ is removed, and all map entries
   higher than kZ in the hierarchy are unaffected."
  [the-map :- tsk/KeyMap
   keys-vec :- [s/Keyword]]
  (let [num-keys     (count keys-vec)
        key-to-clear (last keys-vec)
        parent-keys  (butlast keys-vec)]
    (cond
      (zero? num-keys) the-map
      (= 1 num-keys) (dissoc the-map key-to-clear)
      :else (update-in the-map parent-keys dissoc key-to-clear))))

; #todo:  add in clear-nil-entries to recursively delete all k-v pairs where val is nil or empty?

; #todo:  create safe-map ns with non-nil/non-dup versions of assoc-in, update-in, dissoc-in (&
; singles). Basically like compiler-like guarentees against misspellings, duplicate entries, missing
; entries.

;(defmacro ^:private safe-> ; #todo: remove this
;  [expr & forms]
;  (throw (RuntimeException. "Obsolete: replace with:  (validate not-nil? (-> <expr> <forms> ))" )))

(defmacro it->
  "A threading macro like as-> that always uses the symbol 'it' as the placeholder for the next threaded value:
      (it-> 1
            (inc it)
            (+ it 3)
            (/ 10 it))
      ;=> 2 "
  [expr & forms]
  `(i/it-> ~expr ~@forms))

(defmacro cond-it->
  "A threading macro like as-> that always uses the symbol 'it' as the placeholder for the next threaded value:

    (let [params {:a 1 :b 1 :c nil :d nil}]
      (cond-it-> params
        (:a it)        (update it :b inc)
        (= (:b it) 2)  (assoc it :c \"here\")
        (:c it)        (assoc it :d \"again\")))

    ;=> {:a 1, :b 2, :c \"here\", :d \"again\"}"
  [expr & forms]
  `(i/cond-it-> ~expr ~@forms))

(defmacro some-it->
  "Threads forms as with `it->`, terminates & returns `nil` if any expression is nil."
  [expr & forms]
  `(i/some-it-> ~expr ~@forms))

(defmacro with-exception-default
  "Evaluates body & returns its result.  In the event of an exception, default-val is returned
   instead of the exception."
  [default-val & forms]
  `(i/with-exception-default ~default-val ~@forms))

(defn validate-or-default
  "Returns `sample-val` if `(is-valid? sample-val)` is truthy; else returns `default-val`"
  [is-valid? sample-val default-val]
  (i/validate-or-default is-valid? sample-val default-val))

(defn with-nil-default
  "Returns `sample-val` if not nil; else returns `default-val`"
  [default-val sample-val]
  (i/with-nil-default default-val sample-val))

(defmacro lazy-cons
  "The simple way to create a lazy sequence:
      (defn lazy-next-int [n]
        (t/lazy-cons n (lazy-next-int (inc n))))
      (def all-ints (lazy-next-int 0)) "
  [curr-val recursive-call-form]
  `(i/lazy-cons ~curr-val ~recursive-call-form))

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

(defn rel=
  "Returns true if 2 double-precision numbers are relatively equal, else false.  Relative equality
   is specified as either (1) the N most significant digits are equal, or (2) the absolute
   difference is less than a tolerance value.  Input values are coerced to double before comparison.
   Example:

     (rel= 123450000 123456789   :digits 4   )  ; true
     (rel= 1         1.001       :tol    0.01)  ; true
   "
  [val1 val2 & {:as opts}]
  {:pre  [(number? val1) (number? val2)]
   :post [(contains? #{true false} %)]}
  (let [{:keys [digits tol]} opts]
    (when-not (or digits tol)
      (throw (IllegalArgumentException.
               (str "Must specify either :digits or :tol" \newline
                 "opts: " opts))))
    (when tol
      (when-not (number? tol)
        (throw (IllegalArgumentException.
                 (str ":tol must be a number" \newline
                   "opts: " opts))))
      (when-not (pos? tol)
        (throw (IllegalArgumentException.
                 (str ":tol must be positive" \newline
                   "opts: " opts)))))
    (when digits
      (when-not (integer? digits)
        (throw (IllegalArgumentException.
                 (str ":digits must be an integer" \newline
                   "opts: " opts))))
      (when-not (pos? digits)
        (throw (IllegalArgumentException.
                 (str ":digits must positive" \newline
                   "opts: " opts)))))
    ; At this point, there were no invalid args and at least one of
    ; either :tol and/or :digits was specified.  So, return the answer.
    (let [val1      (double val1)
          val2      (double val2)
          delta-abs (Math/abs (- val1 val2))
          or-result (truthy?
                      (or (zero? delta-abs)
                        (and tol
                          (let [tol-result (< delta-abs tol)]
                            tol-result))
                        (and digits
                          (let [abs1          (Math/abs val1)
                                abs2          (Math/abs val2)
                                max-abs       (Math/max abs1 abs2)
                                delta-rel-abs (/ delta-abs max-abs)
                                rel-tol       (Math/pow 10 (- digits))
                                dig-result    (< delta-rel-abs rel-tol)]
                            dig-result))))
          ]
      or-result)))

(defn all-rel=
  "Applies"
  [x-vals y-vals & opts]
  (let [num-x (count x-vals)
        num-y (count y-vals)]
    (when-not (= num-x num-y)
      (throw (IllegalArgumentException.
               (str ": x-vals & y-vals must be same length" \newline
                 "  #x: " num-x "  #y: " num-y)))))
  (every? truthy?
    (clojure.core/map #(apply rel= %1 %2 opts)
      x-vals y-vals)))

;; #todo delete old definition
;(defn set=
;  "Returns true if two collections are equal when converted into sets."
;  [& colls]
;  (assert (< 1 (count colls))) ; #todo add msg
;  (apply = (mapv set colls)))

(defn range-vec     ; #todo README;  maybe xrange?  maybe kill this?
  "An eager version clojure.core/range that always returns its result in a vector."
  [& args] (apply i/range-vec args))

(defn thru
  "Returns a sequence of integers. Like clojure.core/rng, but is inclusive of the right boundary value. Not lazy. "
  [& args] (apply i/thru args))

(defn fibonacci-seq
  "A lazy seq of Fibonacci numbers (memoized)."
  []
  (let [fibo-step (fn fibo-step [[val1 val2]]
                    (let [next-val (+ val1 val2)]
                      (lazy-cons next-val (fibo-step [val2 next-val] )))) ]
    (cons 0 (cons 1 (fibo-step [0N 1N])))))

(defn fibo-thru
  "Returns a vector of Fibonacci numbers up to limit (inclusive). Note that a
  2^62  corresponds to 91'st Fibonacci number."
  [limit]
  (vec (take-while #(<= % limit) (fibonacci-seq))))

(defn fibo-nth
  "Returns the N'th Fibonacci number (zero-based). Note that
  N=91 corresponds to approx 2^62"
  [N]
  (first (drop N (fibonacci-seq))))

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


(defn seq->str
  "Convert a seq into a string (using pr) with a space preceding each value"
  [seq-in]
  (with-out-str
    (doseq [it (seq seq-in)]
      (print \space)
      (pr it))))

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

(defn clip-str
  "Converts all args to single string and clips any characters beyond nchars."
  [nchars & args] (apply i/clip-str nchars args))

 ; #todo fix usage docs
(defmulti wild-match?
  "Returns true if a pattern is matched by one or more values.  The special keyword :* (colon-star)
   in the pattern serves as a wildcard value.  Note that a wildcald can match either a primitive or a
   composite value: Classic usage:

     (wild-match? pattern & values)

   examples:

     (wild-match? {:a :* :b 2}
                  {:a 1  :b 2})         ;=> true

     (wild-match? [1 :* 3]
                  [1 2  3]
                  [1 9  3] ))           ;=> true

     (wild-match? {:a :*       :b 2}
                  {:a [1 2 3]  :b 2})   ;=> true

   wild-match? also accepts a context map; usage:

     (wild-match? ctx)

   example (default values shown):

     (wild-match?  { :submap-ok   false
                     :subset-ok   false
                     :subvec-ok   false
                     :wildcard-ok true
                     :pattern     <required param>
                     :values    [ <patttern-spec>+ ]   ; vector of 1 or more required
                   } )
"
  (fn wild-match-dispatch-fn [& args]
    (if (and (= 1 (count args))
          (map? (only args)))
      :ctx
      :classic)))

(defmethod wild-match? :ctx
  [ctx]
  (i/wild-match? ctx))

(defmethod wild-match? :classic
  [pattern & values]
  (verify #(pos? (count values)))
  (i/wild-match?
    (vals->map pattern values)))

(defn submatch? ; #todo readme & test
  "Returns true if the first arg is (recursively) a subset/submap/subvec of the 2nd arg"
  [smaller larger]
  (let [ctx {:submap-ok   true
             :subset-ok   true
             :subvec-ok   true
             :wildcard-ok false
             :pattern     smaller
             :values      [larger]}]
    (wild-match? ctx)))

(defn wild-submatch? ; #todo readme & test
  "Simple wrapper for wild-match? where all types of sub-matching are enabled."
  [pattern & values]
  (let [ctx {:submap-ok   true
             :subset-ok   true
             :subvec-ok   true
             :wildcard-ok true
             :pattern     pattern
             :values      values}]
    (wild-match? ctx)))

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
        out-vals   (glue out-first out-rest)
        new-result (append result out-vals)
      ]
        (recur unprocessed new-result)))))

(defn refer-tupelo  ; #todo document in readme
  "Refer a number of commonly used tupelo.core functions into the current namespace so they can
   be used without namespace qualification."
  [& args]
  (refer 'tupelo.core :only
   '[spy spyx spyx-pretty spyxx
     let-spy let-spy-pretty
     with-spy-indent with-spy-enabled check-spy-enabled
     truthy? falsey? not-nil? not-empty? has-some? has-none?
     contains-key? contains-val? contains-elem?
     forv map-let* map-let
     when-clojure-1-8-plus when-clojure-1-9-plus
     conjv glue glue-rows
     macro? chars-thru
     append prepend grab dissoc-in fetch fetch-in
     submap? submap-by-keys submap-by-vals keyvals keyvals-seq validate-map-keys map-keys map-vals
     validate only it-> keep-if drop-if zip zip* zip-lazy indexed
     strcat nl pretty pretty-str json->edn edn->json clip-str range-vec thru rel= all-rel=
     drop-at insert-at replace-at idx
     starts-with? int->kw kw->int
     xfirst xsecond xthird xfourth xlast xbutlast xrest xreverse
     kw->sym kw->str str->sym str->kw str->chars sym->kw sym->str
     split-using split-match partition-using
     wild-item? submatch? val=
     increasing? increasing-or-equal? ->vector unwrap xvec
     fibonacci-seq fibo-thru fibo-nth unnest
     with-exception-default lazy-cons lazy-gen yield yield-all
    ] )
  (let [flags (set args)]
    (when (contains? flags :dev)
      ; (refer 'tupelo.impl :only '[   ])
    )
    (when (contains? flags :strict)
      ; #todo unlink/relink troublesome clojure.core stuff
    )))

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

(s/defn ^:deprecated conjv :- [s/Any] ; #todo remove
  "***** DEPRECATED:  replaced by tupelo.core/append *****

   Given base-coll and and one or more values, converts base-coll to a vector and then appends the values.
   The result is always returned as a vector. Note that `(conjv nil 5)` -> `[5]`"
  ; From Stuart Sierra post 2014-2-10
  ([base-coll :- [s/Any]
    value :- s/Any]
    (conj (vec base-coll) value))
  ([base-coll :- [s/Any]
    value :- s/Any
    & values :- [s/Any]]
    (apply conj (vec base-coll) value values)))

))
