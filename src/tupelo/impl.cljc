;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns ^:no-doc tupelo.impl
  "Tupelo - Making Clojure even sweeter"
  (:require 
    [clojure.core.async :as ca]
    [clojure.core.match :as ccm]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.test]
    [cheshire.core :as cc]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.types :as types]
  )
)

; #todo
; need option for (take 3 coll :exact)
; need option for (drop 3 coll :exact)

;-----------------------------------------------------------------------------
; spy stuff

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
   (cond (= :msg arg1) (do (println (str (spy-indent-spaces) arg2 " => " (pr-str arg3))) arg3) ; ->>  case
         (= :msg arg2) (do (println (str (spy-indent-spaces) arg3 " => " (pr-str arg1))) arg1) ; ->   case
         :else (throw (IllegalArgumentException. (str
                                                   "spy: either first or 2nd arg must be :msg \n   args:"
                                                   (pr-str [arg1 arg2 arg3]))))))

  ; #todo change 2-arg arity to assume keyword arg is message. If both are kw's, assume 1st is msg.
  ([msg value]  ; 2-arg arity assumes value is last arg
   (spy :msg msg value))

  ([value]                                                  ; 1-arg arity uses a generic "spy" message
   (spy :msg "spy" value)))

; #todo stop (spyx :hello) result:   :hello => :hello
(defn- spyx-proc
  [exprs]
; (println :spyx-proc :exprs exprs)
; (println :spyx-proc :r1 )
  (let [r1  (vec
              (for [expr (butlast exprs) ]
                (if (keyword? expr)
                  `(println (str (spy-indent-spaces) ~expr))
                  `(println (str (spy-indent-spaces) '~expr " => " ~expr)))))
        r2  (let [expr (last exprs) ]
              `(let [spy-val# ~expr]
                 (println (str (spy-indent-spaces) '~expr " => " (pr-str spy-val# )))
                 spy-val#)
              )
        final-code `(do ~@r1 ~r2)
        ]
;   (newline) (newline)
;   (println :spyx-proc :r1 )
;   (pprint/pprint r1)

;   (newline) (newline)
;   (println :spyx-proc :r2 )
;   (pprint/pprint r2)

;   (newline) (newline)
;   (println :spyx-proc :final-code )
;   (pprint/pprint final-code)
;   (newline) (newline)

    final-code
  ))

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
     (println (str (spy-indent-spaces) '~expr " => " class-name# "->" (pr-str spy-val#)))
     spy-val#))

(defn- spyx-pretty-proc
  [exprs]
  (let [r1  (vec
              (for [expr (butlast exprs) ]
                (if (keyword? expr)
                  `(println (str  ~expr))
                  `(println (str  '~expr " => " ~expr)))))
        r2  (let [expr (last exprs) ]
              `(let [spy-val# ~expr]
                 (println (str  '~expr " => " ))
                 (pprint/pprint spy-val# )
                 spy-val#)
              )
        final-code `(do ~@r1 ~r2) ]
    final-code
  ))

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
  [coll-in]
  (when-not (or (sequential? coll-in) (set? coll-in) )
    (throw (IllegalArgumentException. (str "only: arg must be vector/list or set; arg=" coll-in))))
  (let [num-items (count coll-in)]
    (when-not (= 1 num-items)
      (throw (IllegalArgumentException. (str "only: num-items must=1; num-items=" num-items)))))
  (first coll-in))

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

(defn range-vec     ; #todo README
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

; #todo make inverse named "get-safe" ???

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

(defn- wild-match-1
  [pattern value]
  (with-spy-indent
    ; (spy :msg "pattern" pattern) (spy :msg "value  " value) (flush)       ; for debug
    (let [result (truthy?
                   (cond
                     (= pattern :*) true
                     (= pattern value) true
                     (map? pattern) (wild-match-1 (seq (glue (sorted-map) pattern))
                                      (seq (glue (sorted-map) value)))
                     (coll? value) (and (= (count pattern) (count value))
                                     (every? truthy? (mapv wild-match-1 pattern value)))
                     :default false))
          ]
      ; (spy :msg "result " result) (flush)      ; for debug
      result)))

(defn wild-match?
  "Returns true if a pattern is matched by one or more values.  The special keyword :* (colon-star)
   in the pattern serves as a wildcard value.  Note that a wildcald can match either a primitive or a
   composite value: Usage:

     (wild-match? pattern & values)

   samples:

     (wild-match?  {:a :* :b 2}
                   {:a 1  :b 2})         ;=> true

     (wild-match?  [1 :* 3]
                   [1 2  3]
                   [1 9  3] ))           ;=> true

     (wild-match?  {:a :*       :b 2}
                   {:a [1 2 3]  :b 2})   ;=> true "
  [pattern & values]
  (every? truthy?
    (forv [value values]
      (if (map? pattern)
        (wild-match-1 (glue (sorted-map) pattern)
          (glue (sorted-map) value))
        (wild-match-1 pattern value)))))

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

; #todo could do variant `submap-by-keys` & throw on missing
(s/defn grab-keys :- tsk/Map
  "Returns a new map containing entries for specified keys. Throws for missing keys."
  [map   :- tsk/Map
   keep-keys  :- tsk/Set]
  (apply glue {}
    (for [key keep-keys]
      {key (grab key map)})))

; #todo -> README
; #todo variant: allow single or vec of default values
(s/defn map-keys->vals :- tsk/List
  "Returns a vector of values for each key, in the order specified. Throws for missing keys."
  [map   :- tsk/KeyMap
   keys  :- [s/Keyword]]
  (forv [key keys]
    (grab key map)))

; #todo could do variant `submap-by-keys` & throw on missing
(s/defn keep-map-keys :- tsk/Map
  "Given a source map, returns a subset map containing only target keys."
  [map        :- tsk/Map
   tgt-keys   :- tsk/Set]
  (into {}
    (for [entry map
          :let [entry-key (key entry)]
          :when (contains? tgt-keys entry-key) ]
      entry )))

; #todo could do variant `submap-by-vals` & throw on missing
(s/defn keep-map-vals :- tsk/Map
  "Given a source map, returns a subset map containing only target values."
  [map        :- tsk/Map
   tgt-vals   :- tsk/Set]
  (into {}
    (for [entry map
          :let [entry-val (val entry)]
          :when (contains? tgt-vals entry-val) ]
      entry )))

(defn hiccup->enlive
  "Converts a tree of data from Hiccup -> Enlive format"
  [tree-node]
  (if-not (sequential? tree-node)
    tree-node       ; leaf - just return it
    (let [tag    (first tree-node)
          less-1 (rest tree-node)]
      (if (empty? less-1)
        {:tag     tag
         :attrs   {}
         :content []}
        (let [v2 (first less-1)]
          (if (map? v2)
            {:tag     tag
             :attrs   v2
             :content (forv [child (rest less-1)]
                        (hiccup->enlive child))}
            {:tag     tag
             :attrs   {}
             :content (forv [child less-1]
                        (hiccup->enlive child))}))))))

(defn enlive->hiccup
  [tree-node]
  (if-not (map? tree-node)
    tree-node       ; leaf - just return it
    ; #todo maybe a let-keys macro for this?
    (let [tag        (grab :tag tree-node)
          attrs      (grab :attrs tree-node)
          content    (grab :content tree-node)
          tag-attrs  (if (empty? attrs)
                       [tag]
                       [tag attrs])
          content-tx (forv [child content]
                       (enlive->hiccup child))
          result      (glue tag-attrs content-tx) ]
      result)))

;#todo write fn enlive-remove-attrs

;---------------------------------------------------------------------------------------------------
(defn- ^:no-doc find-tree-impl
  [result-atom parents tree tgt-path]
  ;(newline)
  ;(println :result-atom) (pretty @result-atom)
  ;(spyx parents)
  ;(spyx tree)
  ;(spyx tgt-path)
  (when (map? tree) ; avoid trying to process value `1` on leaf like [:a 1]
    (when (and (not-empty? tree) (not-empty? tgt-path))
      (let [tgt           (first tgt-path)
            tgt-path-rest (rest tgt-path)
            tag           (grab :tag tree)
            content       (grab :content tree)]
        (when (or (= tag :*) (= tag :**))
          (throw (IllegalArgumentException. (str "fing-tag*: found reserved tag " tag " in tree"))))
        ;(spyx tgt)
        ;(spyx tgt-path-rest)
        (if (or (= tgt tag) (= tgt :*))
          (do
            ;(println :200 "match tag:" tag)
            (if (empty? tgt-path-rest)
              (let [soln {:parent-path parents
                          :subtree     tree}]
                ;(println :210 "empty soln:" soln)
                (swap! result-atom glue #{soln}))
              (let [parents-new (append parents tag)]
                ;(println :220 "not-empty parents-new:" parents-new)
                (doseq [child-tree content]
                  ;(println :230 "child-tree:" child-tree)
                  (find-tree-impl result-atom parents-new child-tree tgt-path-rest)))))
          (when (= tgt :**)
            ;(println :300 "tgt = :**")
            (when (not-empty? tgt-path-rest) ; :** wildcard cannot terminate the tgt-path
              (let [parents-new (append parents tag)]
                ;(println :320 ":** parents-new:" parents-new)
                ;(println (str :331 "  recurse  parents:" parents "   tree:" tree "  tgt-path-rest:" tgt-path-rest))
                (find-tree-impl result-atom parents tree tgt-path-rest)
                (doseq [child-tree content]
                  ;(println :330 ":** child-tree:" child-tree)
                  ;(println (str :332 "    recurse  parents-new:" parents-new "  tgt-path:" tgt-path))
                  (find-tree-impl result-atom parents-new child-tree tgt-path))))))))))

; #todo need update-tree & update-leaf fn's
(defn find-tree
  "Searches an Enlive-format tree for the specified tgt-path"
  [tree tgt-path]
  ;(println "=============================================================================")
  (when (empty? tree)
    (throw (IllegalStateException. "find-tree: tree is empty")))
  (when (empty? tgt-path)
    (throw (IllegalStateException. "find-tree: tgt-path is empty")))
  (when (= :** (last tgt-path))
    (throw (IllegalArgumentException. "find-tree: recursive-wildcard `:**` cannot terminate tgt-path")))

  (let [result-atom (atom #{}) ]
    (try
      (find-tree-impl result-atom [] tree tgt-path)
      (catch Exception e
        (throw (RuntimeException. (str "find-tree: failed for tree=" tree \newline
                                    "  tgt-path=" tgt-path \newline
                                    "  caused by=" (.getMessage e))))))
    @result-atom))

(defn find-leaf [tree tgt-path leaf-val]
  "Searches an Enlive-format tree for the specified tgt-path & leaf value."
  (let [subtree-solns     (find-tree tree tgt-path)
        tgt-path-terminal (last tgt-path)
        tgt-leaf-node     {:tag tgt-path-terminal :attrs {} :content [leaf-val]}
        results           (into #{}
                            (for [soln subtree-solns
                                  :when (or (= tgt-leaf-node (grab :subtree soln))
                                          (= leaf-val :*)) ]
                              soln)) ]
    results ))

(defn- find-tree-result->hiccup
  [soln-set]
  (into #{}
    (for [soln soln-set]
      (update-in soln [:subtree] enlive->hiccup))))

(defn find-tree-hiccup [tree tgt-path]
  "Searches an Hiccup-format tree for the specified tgt-path,
  returning a Hiccup-format result."
  (find-tree-result->hiccup
    (find-tree (hiccup->enlive tree) tgt-path)))

(defn find-leaf-hiccup [tree tgt-path leaf-val]
  "Searches an Hiccup-format tree for the specified tgt-path & leaf value,
  returning a Hiccup-format result."
  (find-tree-result->hiccup
    (find-leaf (hiccup->enlive tree) tgt-path leaf-val)))

; Enlive format   #todo: need *-hiccup versin?
(defn get-tree      ; #todo need test & README
  [tree tgt-path]
  (-> (find-tree tree tgt-path)
    only
    :subtree ))

; Enlive format   #todo: need *-hiccup versin?
(defn get-leaf      ; #todo need test & README
  [tree tgt-path]
  (-> (get-tree tree tgt-path)
    :content
    only))

;---------------------------------------------------------------------------------------------------
(defn select-entries
  "Returns a new map containing key-value entries selected from the supplied map"
  [map-arg ]
  )

