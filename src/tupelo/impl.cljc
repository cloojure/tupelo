;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns ^:no-doc tupelo.impl
  "Tupelo - Making Clojure even sweeter"
  (:refer-clojure :exclude [first rest])
  (:require 
    [clojure.core.async :as ca]
    [clojure.core.match :as ccm]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.test]
    [clojure.walk :as walk]
    [cheshire.core :as cc]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.types :as types]
  )
)

; #todo need option for (take 3 coll :exact) & drop; xtake xdrop

(defn nl
  "Abbreviated name for `newline` "
  [] (newline))

(s/defn xfirst :- s/Any ; #todo -> tests
  "Returns the first value in a list or vector. Throws if empty."
  [arg :- [s/Any]]
  (when (or (nil? arg) (empty? arg))
    (throw (IllegalArgumentException. (str "first: invalid arg:" arg))))
  (clojure.core/first arg))

(s/defn xsecond :- s/Any ; #todo -> tests
  "Returns the second value in a list or vector. Throws if (< len 2)."
  [arg :- [s/Any]]
  (when (or (nil? arg) (< (count arg) 2))
    (throw (IllegalArgumentException. (str "second: invalid arg:" arg))))
  (clojure.core/second arg))

(s/defn xthird :- s/Any ; #todo -> tests
  "Returns the third value in a list or vector. Throws if (< len 3)."
  [arg :- [s/Any]]
  (when (or (nil? arg) (< (count arg) 3))
    (throw (IllegalArgumentException. (str "third: invalid arg:" arg))))
  (clojure.core/nth arg 3))

; #todo -> tests
(s/defn xrest :- s/Any
  "Returns a vector containing all but the first value in a list or vector. Throws if (zero? (count arg))."
  [arg :- [s/Any]]
  (when (or (nil? arg) (zero? (count arg)))
    (throw (IllegalArgumentException. (str "first: invalid arg:" arg))))
  (vec (clojure.core/rest arg)))

(s/defn kw->sym :- s/Symbol
  [arg :- s/Keyword]
  (symbol (name arg)))

(s/defn kw->str :- s/Str
  [arg :- s/Keyword]
  (name arg))

(s/defn sym->str :- s/Str
  [arg :- s/Symbol]
  (name arg))

(s/defn sym->kw :- s/Keyword
  [arg :- s/Symbol]
  (keyword arg))

(s/defn str->sym :- s/Symbol
  [arg :- s/Str]
  (symbol arg))

(s/defn str->kw :- s/Keyword
  [arg :- s/Str]
  (keyword arg))

(s/defn vec->list :- [s/Any]
  [arg :- [s/Any]]
  (apply list arg))

; #todo add test & README
(defn pretty-str
  "Returns a string that is the result of clojure.pprint/pprint"
  [arg]
  (with-out-str (pprint/pprint arg)))

; #todo rename to pp or pprint ?
; #todo add test & README
(defn pretty                                                ; #todo experimental
  "Shortcut to clojure.pprint/pprint. Returns it argument."
  ([arg]
   (pprint/pprint arg)
   arg)
  ([arg writer]
   (pprint/pprint arg writer)
   arg))

(defmacro with-exception-default
  "Evaluates body & returns its result.  In the event of an exception, default-val is returned
   instead of the exception."
  [default-val & body]
  `(try
     ~@body
     (catch Exception e# ~default-val)))

;-----------------------------------------------------------------------------
; spy stuff

(def ^:dynamic *spy-enabled* true)
(def ^:dynamic *spy-enabled-map* {})

(defmacro with-spy-enabled ; #todo README & test
  [tag ; :- s/Keyword #todo schema for macros?
   & forms ]
  `(binding [*spy-enabled-map* (assoc *spy-enabled-map* ~tag true)]
     ~@forms))

(defmacro check-spy-enabled ; #todo README & test
  [tag ; :- s/Keyword #todo schema for macros?
   & forms]
  `(binding [*spy-enabled* (get *spy-enabled-map* ~tag false)]
     ~@forms))

(def ^:no-doc spy-indent-level (atom 0))

(defn ^:no-doc spy-indent-spaces []
  (str/join (repeat (* 2 @spy-indent-level) \space)))

(defn ^:no-doc spy-indent-reset
  "Reset the spy indent level to zero."
  []
  (reset! spy-indent-level 0))

(defn ^:no-doc spy-indent-inc
  "Increase the spy indent level by one."
  []
  (swap! spy-indent-level inc))

(defn ^:no-doc spy-indent-dec
  "Decrease the spy indent level by one."
  []
  (swap! spy-indent-level dec))

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
  ([arg1 arg2 arg3]
   (cond
     (= :msg arg1) (do
                     (when *spy-enabled*
                       (println (str (spy-indent-spaces) arg2 " => " (pr-str arg3))))
                     arg3)

     (= :msg arg2) (do
                     (when *spy-enabled*
                       (println (str (spy-indent-spaces) arg3 " => " (pr-str arg1))))
                     arg1)

     :else (throw (IllegalArgumentException. (str "spy: either first or 2nd arg must be :msg \n   args:"
                                               (pr-str [arg1 arg2 arg3]))))))
  ; #todo change 2-arg arity to assume keyword arg is message. If both are kw's, assume 1st is msg.
  ([msg value]  ; 2-arg arity assumes value is last arg
   (spy :msg msg value))
  ([value] ; 1-arg arity uses a generic "spy" message
   (spy :msg "spy" value)))

; #todo stop (spyx :hello) result:   :hello => :hello
(defn- spyx-proc
  [exprs]
  (let [r1         (for [expr (butlast exprs)]
                     (when *spy-enabled*
                       (if (keyword? expr)
                         `(println (str (spy-indent-spaces) ~expr))
                         `(println (str (spy-indent-spaces) '~expr " => " ~expr)))))
        r2         (let [expr (last exprs)]
                     `(let [spy-val# ~expr]
                        (when *spy-enabled*
                          (println (str (spy-indent-spaces) '~expr " => " (pr-str spy-val#))))
                        spy-val#))
        final-code `(do ~@r1 ~r2) ]
    final-code))

; #todo allow spyx to have labels like (spyx :dbg-120 (+ 1 2)):  ":dbg-120 (+ 1 2) => 3"
(defmacro spyx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& exprs]
  (spyx-proc exprs))

(defmacro spyxx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expression, printing both the expression, its type, and its value to stdout, then returns the value."
  [expr]
  `(let [spy-val#    ~expr
         class-name# (-> spy-val# class .getName)]
     (when *spy-enabled*
       (println (str (spy-indent-spaces) '~expr " => " class-name# "->" (pr-str spy-val#))))
     spy-val#))

(defn- spyx-pretty-proc
  [exprs]
  (let [r1         (for [expr (butlast exprs)]
                     (when *spy-enabled*
                       (if (keyword? expr)
                         `(println (str ~expr))
                         `(println (str '~expr " => " ~expr)))))
        r2         (let [expr (last exprs)]
                     `(let [spy-val# ~expr]
                        (when *spy-enabled*
                          (println (str '~expr " => "))
                          (pprint/pprint spy-val#))
                        spy-val#))
        final-code `(do
                      (newline)
                      ~@r1 ~r2)]
    final-code))

; #todo On all spy* make print file & line number
; #todo allow spyx-pretty to have labels like (spyx-pretty :dbg-120 (+ 1 2)):  ":dbg-120 (+ 1 2) => 3"
(defmacro spyx-pretty
  "Like `spyx` but with pretty printing (clojure.pprint/pprint)"
  [& exprs]
  (spyx-pretty-proc exprs))

(defmacro with-spy-indent
  "Increments indentation level of all spy, spyx, or spyxx expressions within the body."
  [& body]
  `(do
     (spy-indent-inc)
     (let [result# (do ~@body)]
       (spy-indent-dec)
       result#)))

(defn- let-spy-impl
  [exprs]
  (let [decls (xfirst exprs)
        _     (when (not (even? (count decls)))
                (throw (IllegalArgumentException. (str "spy-let-proc: uneven number of decls:" decls))))
        forms (xrest exprs)
        fmt-pair (fn [[dest src]]
                   [ dest src
                     '_ (list 'spyx dest)] ) ; #todo gensym instead of underscore?
        pairs (vec (partition 2 decls))
        r1    (vec (mapcat fmt-pair pairs ))
        final-code  `(let ~r1 ~@forms ) ]
    final-code ))

; #todo spy-let should also print the return value
(defmacro spy-let   ; #todo -> deprecated
  [& exprs]
  (let-spy-impl exprs))

(defmacro let-spy
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& exprs]
  (let-spy-impl exprs))

;-----------------------------------------------------------------------------
(defn- let-spy-pretty-impl
  [exprs]
  (let [decls (xfirst exprs)
        _     (when (not (even? (count decls)))
                (throw (IllegalArgumentException. (str "spy-let-pretty-impl: uneven number of decls:" decls))))
        forms (xrest exprs)
        fmt-pair (fn [[dest src]]
                   [dest src
                    '_ (list 'spyx-pretty dest)] ) ; #todo gensym instead of underscore?
        pairs (vec (partition 2 decls))
        r1    (vec (mapcat  fmt-pair pairs ))
        final-code  `(let ~r1 ~@forms ) ]
    final-code ))

(defmacro spy-let-pretty   ; #todo -> deprecated
  [& exprs]
  (let-spy-pretty-impl exprs))

(defmacro let-spy-pretty
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& exprs]
  (let-spy-pretty-impl exprs))

;-----------------------------------------------------------------------------

(defn truthy?
  "Returns true if arg is logical true (neither nil nor false); otherwise returns false."
  [arg]
  (if arg true false))

; #todo how to test the :ret part?
; (sp/fdef truthy?
;   :args (sp/cat :arg any?)
;   :ret  boolean?)

(s/defn falsey? :- s/Bool
  "Returns true if arg is logical false (either nil or false); otherwise returns false. Equivalent
   to (not (truthy? arg))."
  [arg :- s/Any]
  (if arg false true))

(defn validate
  "(validate tst-fn tst-val)
  Used to validate intermediate results. Returns tst-val if the result of
  (tst-fn tst-val) is truthy.  Otherwise, throws IllegalStateException."
  [tst-fn tst-val]
  (let [tst-result (tst-fn tst-val)]
    (when-not (truthy? tst-result)
      (throw (IllegalStateException. (format "validate: tst-val=%s, tst-result=%s" tst-val tst-result))))
    tst-val))

(defn only
  "(only coll-in)
  Ensures that a sequence is of length=1, and returns the only value present.
  Throws an exception if the length of the sequence is not one.
  Note that, for a length-1 sequence S, (first S), (last S) and (only S) are equivalent."
  [coll]
  (let [coll-seq  (seq coll)
        num-items (count coll-seq)]
    (when-not (= 1 num-items)
      (throw (IllegalArgumentException. (str "only: num-items must=1; num-items=" num-items))))
    (clojure.core/first coll-seq)))

;-----------------------------------------------------------------------------
(defmacro it->
  "A threading macro like as-> that always uses the symbol 'it' as the placeholder for the next threaded value:
      (it-> 1
            (inc it)
            (+ it 3)
            (/ 10 it))
      ;=> 2
   "
  [expr & forms]
  `(let [~'it ~expr
         ~@(interleave (repeat 'it) forms)
         ]
     ~'it))

(defn clip-str      ; #todo -> tupelo.string?
  "Converts all args to single string and clips any characters beyond nchars."
  [nchars & args]
  (it-> (apply str args)
    (take nchars it)
    (apply str it)))

; #todo -> README
(s/defn has-some? :- s/Bool ; #todo rename to has-any?   Add warning re new clj/any?
  "For any predicate pred & collection coll, returns true if (pred x) is logical true for at least one x in
   coll; otherwise returns false.  Like clojure.core/some, but returns only true or false."
  [pred :-  s/Any
   coll :- [s/Any] ]
  (truthy? (some pred coll)))
; NOTE: was `any?` prior to new `clojure.core/any?` added in clojure 1.9.0-alpha10

; #todo -> README
(s/defn has-none? :- s/Bool
  "For any predicate pred & collection coll, returns false if (pred x) is logical true for at least one x in
   coll; otherwise returns true.  Equivalent to clojure.core/not-any?, but inverse of has-some?."
  [pred :-  s/Any
   coll :- [s/Any] ]
  (falsey? (some pred coll))) ; #todo -> (not (has-some? pred coll))

(s/defn contains-elem? :- s/Bool
  "For any collection coll & element tgt, returns true if coll contains at least one
  instance of tgt; otherwise returns false. Note that, for maps, each element is a
  vector (i.e MapEntry) of the form [key value]."
  [coll :- s/Any
   elem :- s/Any ]
  (has-some? truthy?
    (mapv #(= elem %) (seq coll))))

(s/defn contains-key? :- s/Bool
  "For any map or set, returns true if elem is a map key or set element, respectively"
  [map-or-set :- (s/pred #(or (map? %) (set? %)))
   elem :- s/Any ]
  (contains? map-or-set elem))

(s/defn contains-val? :- s/Bool
  "For any map, returns true if elem is present in the map for at least one key."
  [map :- tsk/Map
   elem :- s/Any ]
  (has-some? truthy?
    (mapv #(= elem %) (vals map))))

(s/defn keyvals :- [s/Any]
  "For any map m, returns the (alternating) keys & values of m as a vector, suitable for reconstructing m via
   (apply hash-map (keyvals m)). (keyvals {:a 1 :b 2} => [:a 1 :b 2] "
  [m :- tsk/Map ]
  (reduce into [] (seq m)))

(defn range-vec     ; #todo README;  maybe xrange?  maybe kill this?
  "An eager version clojure.core/range that always returns its result in a vector."
  [& args]
  (vec (apply range args)))

; #todo need docs & tests
; #todo:  add (thru a b)     -> [a..b] (inclusive)
;             (thru 1 3)     -> [ 1  2  3]
;             (thru \a \c)   -> [\a \b \c]
;             (thru :a :c)   -> [:a :b :c]
;             (thru 'a 'c)   -> ['a 'b 'c]
;             (thru 1   2   0.1)     -> [1.0  1.1  1.2 ... 2.0]
;             (thru 0.1 0.3 0.1)     -> [0.1  0.2  0.3]
;                  (thru start stop step) uses integer steps and
;                  (rel= curr stop :tol step) as ending criteria
;       (thru :cc 1 5)   -> [1 2 3 4 5]
;       (thru :oc 1 5)   -> [  2 3 4 5]
;       (thru :co 1 5)   -> [1 2 3 4  ]  ; like (range ...)
;       (thru :oo 1 5)   -> [  2 3 4  ]

(defn thru          ; #todo make lazy: thruz or (thru 1 3 :lazy)
  "Returns a sequence of integers. Like clojure.core/rng, but is inclusive of the right boundary value. Not lazy. "
  ([end]
   (range-vec (inc end)))
  ([start end]
   (range-vec start (inc end)))
  ([start end step]
   (let [delta          (- (double end) (double start))
         nsteps-dbl     (/ (double delta) (double step))
         nsteps-int     (Math/round nsteps-dbl)
         rounding-error (Math/abs (- nsteps-dbl nsteps-int)) ]
     (when (< 0.00001 rounding-error)
       (throw (IllegalArgumentException. (str
                                           "thru: non-integer number of steps \n   args:"
                                           (pr-str [start end step])))))
     (it-> (inc nsteps-int)
       (range-vec it)
       (clojure.core/map #(* step %) it)
       (clojure.core/map #(+ start %) it)
       (vec it))))
  )

; #todo need test, readme
(defn char-seq
  "Given two characters (or numerical equivalents), returns a seq of characters
  (inclusive) from the first to the second.  Characters must be in ascending order."
  [start-char stop-char]
  {:pre [ (char start-char) (char stop-char) ] }
  ; These "dummy" casts are to ensure that any input integer values are within the valid
  ; range for Unicode characters
  (let [start-val   (int start-char)
        stop-val    (int stop-char)]
    (when-not (<= start-val stop-val)
      (throw (IllegalArgumentException.
               (str "char-seq: start-char must come before stop-char."
                 "  start-val=" start-val "  stop-val=" stop-val))))
    (mapv char (thru start-val stop-val))))

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
  (let [string-or-char? #(or (string? %) (char? %))]
    (cond
      (every? sequential? colls)        (reduce into [] colls) ; coerce to vector result
      (every? map? colls)               (reduce into    colls) ; first item determines type of result
      (every? set? colls)               (reduce into    colls) ; first item determines type of result
      (every? string-or-char? colls)    (apply str colls)
      :else (throw (IllegalArgumentException.
                     (str "colls must be all same type; found types=" (mapv type colls)))))))
; #todo look at using (ex-info ...)

(s/defn append :- tsk/List
  "Given a sequential object (vector or list), add one or more elements to the end."
  [listy :- tsk/List
   & elems :- [s/Any]]
  (when-not (sequential? listy)
    (throw (IllegalArgumentException. (str "Sequential collection required, found=" listy))))
  (when (empty? elems)
    (throw (IllegalArgumentException. (str "Nothing to append! elems=" elems))))
  (vec (concat listy elems)))

(s/defn prepend :- tsk/List
  "Given a sequential object (vector or list), add one or more elements to the beginning"
  [& args]
  (let [elems (butlast args)
        listy (last args)]
    (when-not (sequential? listy)
      (throw (IllegalArgumentException. (str "Sequential collection required, found=" listy))))
    (when (empty? elems)
      (throw (IllegalArgumentException. (str "Nothing to prepend! elems=" elems))))
    (vec (concat elems listy))))

(s/defn not-nil? :- s/Bool
  "Returns true if arg is not nil; false otherwise. Equivalent to (not (nil? arg)),
   or the poorly-named clojure.core/some? "
  [arg :- s/Any]
  (not (nil? arg)))

(s/defn not-empty? :- s/Bool
  "For any collection coll, returns true if coll contains any items; otherwise returns false.
   Equivalent to (not (empty? coll))."
  ; [coll :- [s/Any]]  ; #todo extend Prismatic Schema to accept this for strings
  [coll]
  (not (empty? coll)))

; #todo:  mapz, forz, filterz, ...?
(defn keep-if
  "Returns a vector of items in coll for which (pred item) is true (alias for clojure.core/filter)"
  [pred coll]
  (cond
    (sequential? coll) (vec (clojure.core/filter pred coll))
    (map? coll) (reduce-kv (fn [cum-map k v]
                             (if (pred k v)
                               (assoc cum-map k v)
                               cum-map))
                  {}
                  coll)
    (set? coll) (reduce (fn [cum-set elem]
                          (if (pred elem)
                            (conj cum-set elem)
                            cum-set))
                  #{}
                  (seq coll))
    :else (throw (IllegalArgumentException.
                   (str "keep-if: coll must be sequential, map, or set, class=" (class coll))))))

(defn drop-if
  "Returns a vector of items in coll for which (pred item) is false (alias for clojure.core/remove)"
  [pred coll]
  (keep-if (complement pred) coll))

(defn strcat
  "Recursively concatenate all arguments into a single string result."
  [& args]
  (let [
        ; We need to use flatten twice since the inner one doesn't change a string into a
        ; sequence of chars, nor does it affect byte-array, et al.  We eventually get
        ; seq-of-scalars which can look like [ \a \b 77 78 \66 \z ]
        seq-of-scalars (flatten
                         (for [it (keep-if not-nil? (flatten [args])) ]
                           ; Note that "sequential?" returns false for sets, strings, and the various
                           ; array types.
                           (if (or
                                 (sequential? it)
                                 (set? it)
                                 (string? it)
                                 (types/byte-array? it)
                                 (types/char-array? it)
                                 (types/int-array? it)
                                 (types/long-array? it)
                                 (types/object-array? it)
                                 (types/short-array? it))
                             (seq it)
                             it)))
        ; Coerce any integer values into character equivalents (e.g. 65 -> \A), then combine
        ; into a single string.
        result         (apply str
                         (clojure.core/map char
                           (keep-if not-nil? seq-of-scalars)))
        ]
    result))

; #todo rename to "get-in-safe" ???
; #todo make throw if not Associative arg (i.e. (get-in '(1 2 3) [0]) -> throw)
; #todo make throw if any index invalid
; #todo need parallel safe (assoc-in m [ks] v) (assoc-in m [ks] v :missing-ok)
(s/defn fetch-in :- s/Any
  "A fail-fast version of clojure.core/get-in. When invoked as (fetch-in the-map keys-vec),
   returns the value associated with keys-vec as for (clojure.core/get-in the-map keys-vec).
   Throws an Exception if the path keys-vec is not present in the-map."
  [the-map   :- tsk/Map
   keys-vec  :- tsk/Vec ]
  (let [result (get-in the-map keys-vec ::not-found)]
    (if (= result ::not-found)
      (throw (IllegalArgumentException.
               (str "Key seq not present in map:" \newline
                 "  map : " the-map \newline
                 "  keys: " keys-vec \newline)))
      result)))

(s/defn fetch :- s/Any
  "A fail-fast version of keyword/map lookup.  When invoked as (grab the-map :the-key),
   returns the value associated with :the-key as for (clojure.core/get the-map :the-key).
   Throws an Exception if :the-key is not present in the-map."
  [the-map :- tsk/Map
   the-key :- s/Any]
  (fetch-in the-map [the-key]))

(s/defn grab :- s/Any
  "A fail-fast version of keyword/map lookup.  When invoked as (grab :the-key the-map),
   returns the value associated with :the-key as for (clojure.core/get the-map :the-key).
   Throws an Exception if :the-key is not present in the-map."
  [the-key :- s/Any
   the-map :- tsk/Map]
  (fetch-in the-map [the-key]))


(defmacro forv    ; #todo: (for-vec ...)  or  (vfor ...)
  "Like clojure.core/for but returns results in a vector.   Not lazy."
  [& body]
  `(vec (for ~@body)))

(defmacro map-with  ; #todo maybe rename -> map-using  ???
  "Usage: (map-with [bindings & forms])

  Given bindings and forms like `(map-with [x xs, y ys, ...] (+ x y))`, will iterate over the
  collections [xs ys ...] assigning successive values of each collection to [x y ...], respectively.
  The local symbols [x y ...] can then be used in `forms` to generate the output mapping. Not lazy."
  [bindings & forms]
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. (str "map-with: bindings must be even in number=" bindings))))
  (let [pairs (partition 2 bindings)
        syms  (mapv xfirst pairs)
        colls (mapv xsecond pairs)]
    `(let [lengths# (mapv count ~colls)
           map-fn#  (fn ~syms ~@forms)]
       (when-not (apply = lengths#)
         (throw (IllegalArgumentException.
                  (str "map-with: coll lengths must all be same length; lengths=" lengths#))))
       (mapv map-fn# ~@colls))))

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
  `(and ~@(forv [value values]
            `(ccm/match ~value
               ~pattern true
               :else false))))

; #todo -> README
(s/defn submap-by-keys :- tsk/Map
  "Returns a new map containing entries with the specified keys. Throws for missing keys,
  unless `:missing-ok` is specified. Usage:

      (submap-by-keys {:a 1 :b 2} #{:a   }             )  =>  {:a 1}
      (submap-by-keys {:a 1 :b 2} #{:a :z} :missing-ok )  =>  {:a 1}
  "
  [map-arg :- tsk/Map
   keep-keys :- tsk/Set
   & opts]
  (assert (set? keep-keys))
  (if (= opts [:missing-ok])
    (apply glue {}
      (for [key keep-keys]
        (with-exception-default {}
          {key (grab key map-arg)})))
    (apply glue {}
      (for [key keep-keys]
        {key (grab key map-arg)}))))

; #todo -> README
(s/defn submap-by-vals :- tsk/Map
  "Returns a new map containing entries with the specified vals. Throws for missing vals,
  unless `:missing-ok` is specified. Usage:

      (submap-by-vals {:a 1 :b 2 :A 1} #{1  }             )  =>  {:a 1 :A 1}
      (submap-by-vals {:a 1 :b 2 :A 1} #{1 9} :missing-ok )  =>  {:a 1 :A 1}
  "
  [map-arg :- tsk/Map
   keep-vals :- tsk/Set
   & opts]
  (assert (set? keep-vals))
  (let [found-map    (into {}
                       (for [entry map-arg
                             :let [entry-val (val entry)]
                             :when (contains? keep-vals entry-val)]
                         entry))
        found-vals   (into #{} (vals found-map))
        missing-vals (set/difference keep-vals found-vals)]
    (if (or (empty? missing-vals) (= opts [:missing-ok]))
      found-map
      (throw (IllegalArgumentException.
               (format "submap-by-vals: missing values= %s  map-arg= %s  " missing-vals (pretty-str map-arg)))))))

; #todo need test & README
(s/defn submap? :- Boolean
  "Returns true if the map entries (key-value pairs) of one map are a subset of the entries of
   another map.  Similar to clojure.set/subset?"
  [inner-map :- {s/Any s/Any}                           ; #todo
   outer-map :- {s/Any s/Any}]                          ; #todo
  (let [inner-set (set inner-map)
        outer-set (set outer-map)]
    (set/subset? inner-set outer-set)))

; #todo -> README
; #todo variant: allow single or vec of default values
(s/defn map-keys->vals :- tsk/List
  "Returns a vector of values for each key, in the order specified. Throws for missing keys."
  [map   :- tsk/KeyMap
   keys  :- [s/Keyword]]
  (forv [key keys]
    (grab key map)))

(defmacro vals->map ; #todo -> README
  "Called with a list of symbols like `(label-value-map a b c)` returns a labeled map
   like {:a a :b b :c c} with the values for a, b, & c."
  [& symbols]
  (let [map-vec (forv [symbol symbols]
                  {(keyword symbol) symbol})]
    `(glue ~@map-vec) ) )

(defmacro with-map-vals ; #todo -> README
  "Given a map like {:a 1 :b 2 :c 3} (such as generated by `(label-value-map a b c)`),
  performs safe `let` destructuring using `grab` like:

     (def the-map {:a 1 :b 2 :c 3} }
     (with-map-fields the-map [a b c]  ...)

        =>  (let [a (grab :a the-map)
                  b (grab :b the-map)
                  c (grab :c the-map) ]
               ... )

  `with-map-fields` is safe for typos since `grab` will throw is the requrested map key is not present."
  [ the-map syms-vec & forms]
  `(do
     ; (assert (map? ~the-map))
     ; (assert (sequential? ~syms-vec))
     (let  ; generate the binding vector dynamically
       ~(apply glue
          (for [sym syms-vec
                :let [kw (keyword sym)]]
            [sym (list 'grab kw the-map)]))
       ~@forms)))

(defn macro?
  "Returns true if a quoted symbol resolves to a macro. Usage:

    (println (macro? 'and))  ;=> true
  "
  [s]
  (-> s resolve meta :macro boolean))
    ; from Alex Miller StackOverflow answer 2017-5-6

(s/defn map=
  "Recursively converts all records into maps, then tests for equality using clojure.core/="
  [& vals]
  (let [mapify   (fn [arg]
                   (if (map? arg)
                     (into {} arg)
                     arg))
        mapified (mapv #(walk/postwalk mapify %) vals)
        result   (apply = mapified)]
    result))

(s/defn ^:private ^:no-doc wild-match-impl
  [ctx :- tsk/KeyMap ; #todo more precise schema needed { :submap-ok s/Bool ... }
   pattern :- s/Any
   value :- s/Any ]
  (with-map-vals ctx [submap-ok subset-ok subvec-ok wildcard-ok]
    (let [result (truthy?
                   (cond
                     (= pattern value)   true

                     (and wildcard-ok
                       (= pattern :*))   true

                     (and (map? pattern) (map? value))
                         (let [keyset-pat (set (keys pattern))
                               keyset-val (set (keys value))]
                           (and
                             (or (= keyset-pat keyset-val)
                               (and submap-ok ; #todo need test
                                 (set/subset? keyset-pat keyset-val)))
                             (every? truthy?
                               (forv [key keyset-pat]
                                 (wild-match-impl ctx
                                   (grab key pattern)
                                   (grab key value))))))

                     (and (set? pattern) (set? value)) ; #todo need test
                         (or (= pattern value)
                           (and subset-ok
                             (set/subset? pattern value)))

                     (and (coll? pattern) (coll? value))
                         (let [num-pat     (count pattern)
                               num-val     (count value)
                               lengths-ok? (or (= num-pat num-val) ; #todo need test
                                             (and subvec-ok
                                               (<= num-pat num-val)))]
                           (and lengths-ok?
                             (every? truthy?
                               (mapv #(wild-match-impl ctx %1 %2) pattern value)))) ; truncates shortest

                     :default false)) ]
      result)))

(defn wild-match-ctx? ; #todo readme
  "Returns true if a pattern is matched by one or more values.  The special keyword :* (colon-star)
   in the pattern serves as a wildcard value.  Note that a wildcald can match either a primitive or a
   composite value: Usage:

     (wild-match-ctx? ctx pattern & values)

   samples:

     (wild-match-ctx? ctx {:a :* :b 2}
                          {:a 1  :b 2})         ;=> true

     (wild-match-ctx? ctx [1 :* 3]
                          [1 2  3]
                          [1 9  3] ))           ;=> true

     (wild-match-ctx? ctx {:a :*       :b 2}
                          {:a [1 2 3]  :b 2})   ;=> true

   wild-match? accepts a context map as an optional first argument which defaults to:

     (let [ctx {:submap-ok false
                :subset-ok false
                :subvec-ok false}]
       (wild-match-ctx? ctx pattern values))) "
  [ctx-in pattern & values]
  (let [ctx (glue {:submap-ok   false
                   :subset-ok   false
                   :subvec-ok   false
                   :wildcard-ok true} ctx-in)]
    (every? truthy?
      (for [value values]
        (wild-match-impl ctx pattern value)))))

(defn wild-match? ; #todo readme
  "Simple wrapper for wild-match-ctx? using the default context"
  ([pattern & values]
   (apply wild-match-ctx? {} pattern values)))

(defn wild-submatch? ; #todo readme & test
  "Simple wrapper for wild-match-ctx? where all types of sub-matching are enabled."
  ([pattern & values]
   (let [ctx {:submap-ok   true
              :subset-ok   true
              :subvec-ok   true
              :wildcard-ok true}]
     (apply wild-match-ctx? ctx pattern values))))

; #todo re-impl w/o wildcard stuff
(defn submatch? ; #todo readme & test
  "Returns true if the first arg is (recursively) a subset/submap/subvec of the 2nd arg"
  ([smaller larger]
   (let [ctx {:submap-ok   true
              :subset-ok   true
              :subvec-ok   true
              :wildcard-ok false}]
     (wild-match-ctx? ctx smaller larger))))


(s/defn unnest :- [s/Any] ; #todo readme
  "Given any set of arguments including vectors, maps, sets, & scalars, performs a depth-first
  recursive walk returning scalar args (int, string, keyword, etc) in a single 1-D vector."
  [& values]
  (let [unnest-coll (fn fn-unnest-coll [coll]
                      (apply glue
                        (for [item coll]
                          (if (coll? item)
                            (fn-unnest-coll item)
                            [item]))))
        result      (apply glue
                      (for [item values]
                        (if (coll? item)
                          (unnest-coll item)
                          [item])))]
    result))

(s/defn wild-item? :- s/Bool
  "Returns true if any element in a nested collection is the wildcard :*"
  [item :- s/Any]
  (has-some? #(= :* %) (unnest [item])))

(defn set-match-impl
  [ctx pattern data]
  (or
    (= pattern :*)
    (= pattern data)
    (if (empty? pattern)
      (empty? data) ; #todo or :subset-ok
      (let [sub-pat     (xfirst (seq pattern))
            pattern-new (set/difference pattern #{sub-pat})]
        (if (wild-item? sub-pat)
          ; wildcard pattern
          (loop [items (seq data)]
            (if (empty? items)
              false
              (let [item     (xfirst items)
                    data-new (set/difference data #{item})]
                (if (and
                      (set-match-impl ctx sub-pat item)
                      (set-match-impl ctx pattern-new data-new)
                      )
                  true
                  (recur (xrest items))))))
          ; non-wildcard pattern
          (and (contains? data sub-pat)
            (let [data-new (set/difference data #{sub-pat})]
              (set-match-impl ctx pattern-new data-new))))))))

(defn set-match-ctx? [ctx-in pattern & values]
  (let [ctx (glue {:subset-ok false} ctx-in)]
  (every? truthy?
    (for [value values]
      (set-match-impl ctx pattern value)))))

(defn set-match? [pattern & values]
  (every? truthy?
      (for [value values]
        (set-match-impl {} pattern value))))

; #todo maybe ns-assoc, ns-dissoc, ns-get for intern/ns-unmap

; #todo maybe add explicit arg checking
; #todo   map->entries, entries->map
; #todo   str->chars, chars->str
; #todo   set->vec, vec->set
