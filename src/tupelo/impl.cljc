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
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.test]
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.schema :as tsk]
   ;[tupelo.spec :as tsp]
    [tupelo.types :as types]
    [tupelo.schema :as ts]
  ))

;-----------------------------------------------------------------------------
; Clojure version stuff

(s/defn increasing? :- s/Bool
  "Returns true iff the vectors are in (strictly) lexicographically increasing order
    [1 2]  [1]        -> false
    [1 2]  [1 1]      -> false
    [1 2]  [1 2]      -> false
    [1 2]  [1 2 nil]  -> true
    [1 2]  [1 2 3]    -> true
    [1 2]  [1 3]      -> true
    [1 2]  [2 1]      -> true
    [1 2]  [2]        -> true
  "
  [a :- ts/List
   b :- ts/List]
  (let [len-a        (count a)
        len-b        (count b)
        cmpr         (fn [x y] (cond
                                 (= x y) :eq
                                 (< x y) :incr
                                 (> x y) :decr
                                 :else (throw (IllegalStateException. "should never get here"))))
        cmpr-res     (mapv cmpr a b)
        first-change (first (drop-while #{:eq} cmpr-res)) ; nil if all :eq
        ]
    (cond
      (= a b)                       false
      (= first-change :decr)        false
      (= first-change :incr)        true
      (nil? first-change)           (< len-a len-b))))

(s/defn increasing-or-equal? :- s/Bool
  "Returns true iff the vectors are in (strictly) lexicographically increasing order
    [1 2]  [1]        -> false
    [1 2]  [1 1]      -> false
    [1 2]  [1 2]      -> true
    [1 2]  [1 2 nil]  -> true
    [1 2]  [1 2 3]    -> true
    [1 2]  [1 3]      -> true
    [1 2]  [2 1]      -> true
    [1 2]  [2]        -> true
  "
  [a :- ts/List
   b :- ts/List]
  (or (= a b)
    (increasing? a b)))

(defn is-clojure-1-7-plus? []
  (let [{:keys [major minor]} *clojure-version*]
    (increasing-or-equal? [1 7] [major minor])))

(defn is-clojure-1-8-plus? []
  (let [{:keys [major minor]} *clojure-version*]
    (increasing-or-equal? [1 8] [major minor])))

(defn is-clojure-1-9-plus? []
  (let [{:keys [major minor]} *clojure-version*]
    (increasing-or-equal? [1 9] [major minor])))

(defn is-pre-clojure-1-8? [] (not (is-clojure-1-8-plus?)))
(defn is-pre-clojure-1-9? [] (not (is-clojure-1-9-plus?)))

; #todo add is-clojure-1-8-max?
; #todo need clojure-1-8-plus-or-throw  ??

(defmacro when-clojure-1-8-plus
  "Wraps code that should only be included for Clojure 1.8 or higher.  Otherwise, code is supressed."
  [& forms]
  (if (is-clojure-1-8-plus?)
    `(do ~@forms)))

(defmacro when-clojure-1-9-plus
  "Wraps code that should only be included for Clojure 1.9 or higher.  Otherwise, code is supressed."
  [& forms]
  (if (is-clojure-1-9-plus?)
    `(do ~@forms)))

(defmacro when-not-clojure-1-9-plus
  "Wraps code that should only be included for Clojure versions prior to 1.9.  Otherwise, code is supressed."
  [& forms]
  (if (is-pre-clojure-1-9?)
    `(do ~@forms)))

;----------------------------------------------------------------------------
(when-clojure-1-9-plus
  (require
    '[clojure.spec.alpha :as sp]
    '[clojure.spec.gen.alpha :as gen]
    '[clojure.spec.test.alpha :as stest] ))

(ns-unmap *ns* 'first) ; #todo -> (set-tupelo-strict! true/false)
(ns-unmap *ns* 'second)
(ns-unmap *ns* 'rest)
(ns-unmap *ns* 'next)
(ns-unmap *ns* 'last)

;-----------------------------------------------------------------------------
(declare clip-str)

;-----------------------------------------------------------------------------
; #todo need option for (take 3 coll :exact) & drop; xtake xdrop

(defn nl
  "Abbreviated name for `newline` "
  [] (newline))

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

(defn only
  [coll]
  (let [coll-seq  (seq coll)
        rest-items (clojure.core/rest coll-seq) ]
    (when (nil? coll) (throw (IllegalArgumentException. (str "only: coll must not be nil: " coll))))
    (when (nil? coll-seq) (throw (IllegalArgumentException. (str "only: coll must not be empty: " coll))))
    (when-not (empty? rest-items)
      (throw (IllegalArgumentException. (str "only: num-items must=1; coll="
                                          (clip-str 99 (clojure.core/take 99 coll))))))
    (clojure.core/first coll-seq)))

(defn xfirst      ; #todo -> tests
  [coll]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xfirst: invalid coll: " coll))))
  (nth coll 0))

(defn xsecond  ; #todo -> tests
  [coll]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xsecond: invalid coll: " coll))))
  (nth coll 1))

(defn xthird  ; #todo -> tests
  [coll ]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xthird: invalid coll: " coll))))
  (nth coll 2))

(defn xfourth  ; #todo -> tests
  [coll]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xfourth: invalid coll: " coll))))
  (nth coll 3))

(s/defn xlast :- s/Any ; #todo -> tests
  [coll :- [s/Any]]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xlast: invalid coll: " coll))))
  (clojure.core/last coll))

(s/defn xbutlast :- s/Any ; #todo -> tests
  [coll :- [s/Any]]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xbutlast: invalid coll: " coll))))
  (vec (clojure.core/butlast coll)))

(defn xrest ; #todo -> tests
  [coll]
  (when (or (nil? coll) (empty? coll)) (throw (IllegalArgumentException. (str "xrest: invalid coll: " coll))))
  (clojure.core/rest coll))

(defn xreverse ; #todo -> tests & doc
  "Returns a vector containing a sequence in reversed order. Throws if nil."
  [coll]
  (when (nil? coll) (throw (IllegalArgumentException. (str "xreverse: invalid coll: " coll))))
  (vec (clojure.core/reverse coll)))

; #todo Need safe versions of:
; #todo    + - * /  (others?)  (& :strict :safe reassignments)
; #todo    and, or    (& :strict :safe reassignments)
; #todo    = not=   (others?)  (& :strict :safe reassignments)
; #todo    (drop-last N coll)  (take-last N coll)
; #todo    subvec
; #todo    others???

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

(s/defn str->chars :- s/Keyword
  [arg :- s/Str]
  (vec arg))

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

(s/defn indent-lines-with :- s/Str  ; #todo add readme ;  need test
  "Splits out each line of txt using clojure.string/split-lines, then
  indents each line by prepending it with the supplied string. Joins lines together into
  a single string result, with each line terminated by a single \newline."
  [indent-str :- s/Str
   txt  :- s/Str]
  (str/join
    (interpose \newline
      (for [line (str/split-lines txt)]
        (str indent-str line)))))

(defmacro with-exception-default
  "Evaluates body & returns its result.  In the event of an exception, default-val is returned
   instead of the exception."
  [default-val & forms]
  `(try
     ~@forms
     (catch Exception e# ~default-val)))

; #todo rename to "get-in-safe" ???
; #todo make throw if not Associative arg (i.e. (get-in '(1 2 3) [0]) -> throw)
; #todo make throw if any index invalid
; #todo need safe (assoc-in m [ks] v) (assoc-in m [ks] v :missing-ok)
; #todo need safe (update-in m [ks] f & args)
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

;-----------------------------------------------------------------------------
(defn truthy?
  "Returns true if arg is logical true (neither nil nor false); otherwise returns false."
  [arg]
  (if arg true false))

(defn falsey?
  "Returns true if arg is logical false (either nil or false); otherwise returns false. Equivalent
   to (not (truthy? arg))."
  [arg]
  (if arg false true))

;-----------------------------------------------------------------------------
; clojure.spec stuff
(when-clojure-1-9-plus
  (sp/def ::anything (sp/spec (constantly true) :gen gen/any-printable))
  (sp/def ::nothing  (sp/spec (constantly false)))

  ; #todo how to test the :ret part?
  (sp/fdef truthy?
    :args (sp/cat :arg ::anything)
    :ret boolean?)

  (sp/fdef falsey?
    :args (sp/cat :arg ::anything)
    :ret boolean?
    :fn #(= (:ret %) (not (truthy? (-> % :args :arg))))))

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
                         `(when *spy-enabled* (println (str (spy-indent-spaces) ~expr)))
                         `(when *spy-enabled* (println (str (spy-indent-spaces) '~expr " => " ~expr))))))
        r2         (let [expr (xlast exprs)]
                     `(let [spy-val# ~expr]
                        (when *spy-enabled*
                          (println (str (spy-indent-spaces) '~expr " => " (pr-str spy-val#))))
                        spy-val#))
        final-code `(do ~@r1 ~r2) ]
    final-code))

; #todo allow spyx to have labels like (spyx :dbg-120 (+ 1 2)):  ":dbg-120 (+ 1 2) => 3"
(defmacro spyx
  [& exprs]
  (spyx-proc exprs))

(defmacro spyxx
  [expr]
  `(let [spy-val#    ~expr
         class-name# (-> spy-val# class .getName)]
     (when *spy-enabled*
       (println (str (spy-indent-spaces) '~expr " => <#" class-name# " " (pr-str spy-val#) ">")))
     spy-val#))

(defn- spyx-pretty-proc
  [exprs]
  (let [r1         (for [expr (butlast exprs)]
                       (if (keyword? expr)
                         `(when *spy-enabled* (println (spy-indent-spaces) (str ~expr)))
                         `(when *spy-enabled* (println (spy-indent-spaces) (str '~expr " => " ~expr)))))
        r2         (let [expr (xlast exprs)]
                     `(let [spy-val# ~expr]
                        (when *spy-enabled*
                          (println (str (spy-indent-spaces) '~expr " => "))
                          (println (indent-lines-with (spy-indent-spaces)
                                     (pretty-str spy-val#))))
                        spy-val#))
        final-code `(do
                      ~@r1
                      ~r2)]
    final-code))

; #todo On all spy* make print file & line number
; #todo allow spyx-pretty to have labels like (spyx-pretty :dbg-120 (+ 1 2)):  ":dbg-120 (+ 1 2) => 3"
(defmacro spyx-pretty
  [& exprs]
  (spyx-pretty-proc exprs))

(defmacro with-spy-indent
  "Increments indentation level of all spy, spyx, or spyxx expressions within the body."
  [& forms]
  `(do
     (spy-indent-inc)
     (let [result# (do ~@forms)]
       (spy-indent-dec)
       result#)))

(defmacro let-spy
  [& exprs]
  (let [decls      (xfirst exprs)
        _          (when (not (even? (count decls)))
                     (throw (IllegalArgumentException. (str "spy-let-proc: uneven number of decls:" decls))))
        forms      (xrest exprs)
        fmt-pair   (fn [[dest src]]
                     [dest src
                      '_ (list 'spyx dest)]) ; #todo gensym instead of underscore?
        pairs      (vec (partition 2 decls))
        r1         (vec (mapcat fmt-pair pairs))
        final-code `(let ~r1 ~@forms)]
    final-code))

;-----------------------------------------------------------------------------

(defmacro let-spy-pretty   ; #todo -> deprecated
  [& exprs]
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

(defn validate
  "(validate tst-fn tst-val)
  Used to validate intermediate results. Returns tst-val if the result of
  (tst-fn tst-val) is truthy.  Otherwise, throws IllegalStateException."
  [tst-fn tst-val]
  (let [tst-result (tst-fn tst-val)]
    (when-not (truthy? tst-result)
      (throw (IllegalStateException. (format "validate: tst-val=%s, tst-result=%s" tst-val tst-result))))
    tst-val))

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
                     (str "glue: colls must be all same type; found types=" (mapv type colls)))))))
; #todo look at using (ex-info ...)

; #todo: rename labeled-map
(defmacro vals->context ; #todo -> README
  "Called with a list of symbols like `(vals->context a b c)` returns a map
   like {:a a :b b :c c}.

       (let [a 1
             b 2
             c 3]
         (vals->context a b c))  ;=>  {:a 1 :b 2 :c 3} }

   See `with-context` for simple destructuring of such maps."
  [& symbols]
  (let [maps-list (for [symbol symbols]
                    {(keyword symbol) symbol})]
    `(glue ~@maps-list)) )

; #todo: rename with-labeled-map
(defmacro with-context ; #todo -> README
  "Given a map like {:a 1 :b 2 :c 3} (such as generated by `(vals->context a b c)`),
  performs safe `let` destructuring using `grab` like:

     (let [some-map  {:a 1 :b 2 :c 3} } ]
       (with-context some-map [a b c]
          (+ a b c)))  ;=>  6

  `with-context` is safe for typos since `grab` will throw is the requrested map key is not present.
  See `vals->context` for simple creation of labelled data maps."
  [ the-map items-vec & forms]
  `(do
     ; (assert (map? ~the-map))
     ; (assert (sequential? ~items-vec))
     (let  ; generate the binding vector dynamically
       ~(apply glue
          (for [item items-vec
                :let [sym (symbol (name item))
                      kw  (keyword item)]]
            [sym (list 'grab kw the-map)]))
       ~@forms)))

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

; #todo -> README
(s/defn submap-by-keys :- tsk/Map
  "Returns a new map containing entries with the specified keys. Throws for missing keys,
  unless `:missing-ok` is specified. Usage:

      (submap-by-keys {:a 1 :b 2} #{:a   }             )  =>  {:a 1}
      (submap-by-keys {:a 1 :b 2} #{:a :z} :missing-ok )  =>  {:a 1}
  "
  [map-arg :- tsk/Map
   keep-keys :- (s/either tsk/Set tsk/List)
   & opts]
  (let [keep-keys (set keep-keys)]
    (if (= opts [:missing-ok])
      (apply glue {}
        (for [key keep-keys]
          (with-exception-default {}
            {key (grab key map-arg)})))
      (apply glue {}
        (for [key keep-keys]
          {key (grab key map-arg)})))))

; #todo -> README
(s/defn submap-by-vals :- tsk/Map
  "Returns a new map containing entries with the specified vals. Throws for missing vals,
  unless `:missing-ok` is specified. Usage:

      (submap-by-vals {:a 1 :b 2 :A 1} #{1  }             )  =>  {:a 1 :A 1}
      (submap-by-vals {:a 1 :b 2 :A 1} #{1 9} :missing-ok )  =>  {:a 1 :A 1}
  "
  [map-arg :- tsk/Map
   keep-vals :- (s/either tsk/Set tsk/List)
   & opts]
  (let [keep-vals    (set keep-vals)
        found-map    (into {}
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

; #todo need README
(s/defn submap? :- Boolean
  "Returns true if the map entries (key-value pairs) of one map are a subset of the entries of
   another map.  Similar to clojure.set/subset?"
  [inner-map :- {s/Any s/Any}                           ; #todo
   outer-map :- {s/Any s/Any}]                          ; #todo
  (let [inner-set (set inner-map)
        outer-set (set outer-map)]
    (set/subset? inner-set outer-set)))


(s/defn keyvals :- [s/Any]
  "For any map m, returns the (alternating) keys & values of m as a vector, suitable for reconstructing m via
   (apply hash-map (keyvals m)). (keyvals {:a 1 :b 2} => [:a 1 :b 2] "
  [m :- tsk/Map ]
  (reduce into [] (seq m)))

(s/defn keyvals-seq* :- [s/Any]
  [ctx :- tsk/KeyMap
   m :- tsk/Map
   keys-seq :- [s/Any]]
  (with-context ctx [missing-ok]
    (apply glue
      (for [key keys-seq]
        (let [val (get m key ::missing)]
          (if-not (= val ::missing)
            [key val]
            (if missing-ok
              []
              (throw (IllegalArgumentException.
                       (str "Key not present in map:" \newline
                         "  map: " m \newline
                         "  key: " key \newline))))))))))

(s/defn keyvals-seq :- [s/Any]
  "Like `keyvals`, but only outputs selected keys in the order specified."
  [m :- tsk/Map
   keys-seq :- [s/Any]]
  (keyvals-seq* {:missing-ok false} m keys-seq))

(defn range-vec [& args]
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
;  #todo range version => (butlast (thru ...))
(defn thru          ; #todo make lazy: (thruz ...) -> (thru* {:lazy true} ...)
  ([end]       (thru 0 end))
  ([start end] (thru start end 1))
  ([start end step]
   (let [delta          (- (double end)   (double start))
         nsteps-dbl     (/ (double delta) (double step))
         nsteps-int     (Math/round nsteps-dbl)
         rounding-error (Math/abs (- nsteps-dbl nsteps-int)) ]
     (when (< 0.00001 rounding-error)
       (throw (IllegalArgumentException. (str
                                           "thru: non-integer number of steps \n   args:"
                                           (pr-str [start end step])))))
     (vec (clojure.core/map #(-> %
                               (* step)
                               (+ start))
            (range (inc nsteps-int)))))))

; #todo need test, readme
; #todo merge into `thru` using a protocol for int, double, char, string, keyword, symbol, other?
(defn chars-thru
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

(s/defn join-2d->1d :- ts/List ; #todo think about better name
  "Convert a vector of vectors (2-dimensional) into a single vector (1-dimensional).
  Equivalent to `(apply glue ...)`"
  [listy :- ts/List]
  (when-not (sequential? listy)
    (throw (IllegalArgumentException. (str "Sequential collection required, found=" listy))))
  (when-not (every? sequential? listy)
    (throw (IllegalArgumentException. (str "Nested sequential collections required, found=" listy))))
  (reduce into [] listy))

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
        listy (xlast args)]
    (when-not (sequential? listy)
      (throw (IllegalArgumentException. (str "Sequential collection required, found=" listy))))
    (when (empty? elems)
      (throw (IllegalArgumentException. (str "Nothing to prepend! elems=" elems))))
    (vec (concat elems listy))))

(defrecord Unwrapped [data])
(s/defn unwrap :- Unwrapped
  "Works with the `->vector` function to unwrap vectors/lists to insert
  their elements as with the unquote-spicing operator (~@). Examples:

      (->vector 1 2 3 4 5 6 7 8 9)              =>  [1 2 3 4 5 6 7 8 9]
      (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9)   =>  [1 2 3 4 5 6 7 8 9]
  "
  [data :- [s/Any]]
  (assert (sequential? data))
  (->Unwrapped data))

(s/defn ->vector :- [s/Any]
  "Wraps all args in a vector, as with `clojure.core/vector`. Will (recursively) recognize
  any embedded calls to (unwrap <vec-or-list>) and insert their elements as with the
  unquote-spicing operator (~@). Examples:

      (->vector 1 2 3 4 5 6 7 8 9)              =>  [1 2 3 4 5 6 7 8 9]
      (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9)   =>  [1 2 3 4 5 6 7 8 9]
  "
  [& args :- [s/Any]]
  ;(nl)
  ;(println "->vector args = " args)
  (let [result (reduce (fn [accum it]
                         (let [it-use (cond
                                           (sequential? it) [ (apply ->vector it) ]
                                           (instance? Unwrapped it) (apply ->vector (fetch it :data))
                                           :else [it])
                               accum-out (glue accum it-use ) ]
                           ;(println it "->" it-use "  =>  " accum-out)
                           accum-out ))
                 [] args)]
       result))

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

(defn flat-vec ; #todo remove this?
  "Accepts any number of nested args and returns the flattened result as a vector."
  [& args]
  (vec (flatten args)))

; #todo:  make (map-ctx {:trunc false :eager true} <fn> <coll1> <coll2> ...) <- default ctx
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
                           (cond
                             (or (sequential? it)
                                 (set? it)
                                 (string? it)
                                 (types/byte-array? it)
                                 (types/char-array? it)
                                 (types/int-array? it)
                                 (types/long-array? it)
                                 (types/object-array? it)
                                 (types/short-array? it))        (seq it)
                             (instance? java.io.InputStream it) (seq (slurp it))
                             :else it )))
        ; Coerce any integer values into character equivalents (e.g. 65 -> \A), then combine
        ; into a single string.
        result         (apply str
                         (clojure.core/map char
                           (keep-if not-nil? seq-of-scalars)))
        ]
    result))

; #todo max-key -> t/max-by

(defmacro forv ; #todo wrap body in implicit do
  [& forms]
  `(vec (for ~@forms)))

(defmacro lazy-cons
  "The simple way to create a lazy sequence:
      (defn lazy-next-int [n]
        (t/lazy-cons n (lazy-next-int (inc n))))
      (def all-ints (lazy-next-int 0)) "
  [curr-val next-form]
  `(lazy-seq (cons ~curr-val ~next-form)))

; #todo document use via binding
(def ^:dynamic *lazy-gen-buffer-size*
  "Specifies the output channel default buffer size for `lazy-gen` forms"
  32)

; #todo add to README
; #todo fix SO posting:  defgen -> lazy-gen
; #todo make null case return [] instead of nil
; #todo make eager version?  gen-vec, gen-seq, ...
(defmacro lazy-gen [& forms]
  "Creates a 'generator function' that returns a lazy seq of results
  via `yield` (a la Python)."
  `(let [~'lazy-gen-output-buffer    (ca/chan *lazy-gen-buffer-size*)
         lazy-reader-fn#             (fn lazy-reader-fn# []
                                       (let [curr-item# (ca/<!! ~'lazy-gen-output-buffer)] ; #todo ta/take-now!
                                            (when (not-nil? curr-item#)
                                              (lazy-cons curr-item# (lazy-reader-fn#))))) ]
        (ca/go
          ~@forms
          (ca/close! ~'lazy-gen-output-buffer))
     (lazy-reader-fn#)))

(defmacro yield ; #todo put-now/put-later & dynamic
  "Within a 'generator function' created by `lazy-gen`, populates the
  result lazy seq with the supplied value (a la Python). Returns the value."
  [value]
  `(do
     (ca/>! ~'lazy-gen-output-buffer ~value)
     ~value))

(defmacro yield-all
  "Within a 'generator function' created by `lazy-gen`, populates the
  result lazy seq with each item from the supplied collection. Returns the collection."
  [values]
  `(do
     (doseq [value# ~values]
       (yield value#))
     (vec ~values)))

; #todo fix so doesn't hang if give infinite lazy seq
; #todo rename :strict -> :trunc
(defmacro map-let*
  [context bindings & forms]
  (when (empty? bindings)
    (throw (IllegalArgumentException. (str "map-let*: bindings cannot be empty=" bindings))))
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. (str "map-let*: (count bindings) must be even=" bindings))))
  (when-not (pos? (count forms))
    (throw (IllegalArgumentException. (str "map-let*: forms cannot be empty=" forms))))
  (let [binding-pairs (partition 2 bindings)
        syms          (mapv xfirst binding-pairs)
        colls         (mapv xsecond binding-pairs) ]
       `(do
          (when-not (map? ~context)
            (throw (IllegalArgumentException. (str "map-let*: context must be a map=" ~context))))
          (let [lazy#          (get ~context :lazy false)
                strict#        (get ~context :strict true)
                lengths#       (mapv count ~colls)
                lengths-equal# (apply = lengths#)
                map-fn#        (fn ~syms ~@forms)
                output-fn#     (if lazy# identity vec)]
               (when (and strict#
                       (not lengths-equal#))
                 (throw (IllegalArgumentException.
                          (str "map-let*: colls must all be same length; lengths=" lengths#))))
            (output-fn# (map map-fn# ~@colls))))))

(defmacro map-let
  [bindings & forms]
  `(map-let* {:strict true
              :lazy   false}
     ~bindings ~@forms))

; #todo rename :strict -> :trunc
(defn zip-1*
  "Usage:  (zip* context & colls)
  where context is a map with default values:  {:strict true}
  Not lazy. "
  [context & colls] ; #todo how use Schema with "rest" args?
  (assert (map? context))
  (assert #(every? sequential? colls))
  (let [strict        (get context :strict true)
        lengths       (mapv count colls)
        lengths-equal (apply = lengths) ]
    (when (and strict
            (not lengths-equal))
      (throw (IllegalArgumentException.
               (str "zip*: colls must all be same length; lengths=" lengths))))
    (vec (apply map vector colls))))
; #todo fix so doesn't hang if give infinite lazy seq. Technique:
;  (def x [1 2 3])
;  (seq (drop 2 x))          =>  (3)
;  (seq (drop 3 x))          =>  nil
;  (nil? (seq (drop 3 x)))   =>  true
;  (nil? (drop 3 (range)))   =>  false

; #todo rename :strict -> :trunc
(defn zip*
  [context & colls] ; #todo how use Schema with "rest" args?
  (assert (map? context))
  (assert #(every? sequential? colls))
  (let [num-colls  (count colls)
        strict-flg (get context :strict true)]
    (loop [result []
           colls  colls]
      (let [empty-flgs  (mapv empty? colls)
            num-empties (count (keep-if truthy? empty-flgs)) ]
        (if (zero? num-empties)
          (do
            (let [new-row (mapv xfirst colls)
                  new-results (append result new-row) ]
              (recur
                new-results
                (mapv xrest colls))))
          (do
            (when (and strict-flg
                    (not= num-empties num-colls))
              (throw (RuntimeException. (str "zip*: collections are not all same length; empty-flgs=" empty-flgs))))
            result))))))

; #todo add schema; result = tsk/List[ tsk/Pair ]
; #todo add :trunc & assert;
(defn zip
  ; #todo ***** WARNING - will hang for infinite length inputs *****
  ; #todo fix so doesn't hang if give infinite lazy seq. Technique:
  ; #todo Use (zip ... {:trunc true}) if you want to truncate all inputs to the length of the shortest.
  [& args]
  (assert #(every? sequential? args))
  (apply zip* {:strict true} args))

(defn zip-lazy
  [& colls]  ; #todo how use Schema with "rest" args?
  (assert #(every? sequential? colls))
  (apply map vector colls))

(defn indexed
  [& colls]
  (apply zip-lazy (range) colls))

; #todo rename -> drop-idx
; #todo force to vector result
(s/defn drop-at :- tsk/List
  "Removes an element from a collection at the specified index."
  [coll :- tsk/List
   index :- s/Int]
  (when (neg? index)
    (throw (IllegalArgumentException. (str "Index cannot be negative: " index))))
  (when (<= (count coll) index)
    (throw (IllegalArgumentException. (str "Index cannot exceed collection length: "
                                        " (count coll)=" (count coll) " index=" index))))
  (glue (take index coll)
    (drop (inc index) coll)))

; #todo rename -> insert-idx
; #todo force to vector result
(s/defn insert-at :- tsk/List
  "Inserts an element into a collection at the specified index."
  [coll :- tsk/List
   index :- s/Int
   elem :- s/Any]
  (when (neg? index)
    (throw (IllegalArgumentException. (str "Index cannot be negative: " index))))
  (when (< (count coll) index)
    (throw (IllegalArgumentException. (str "Index cannot exceed collection length: "
                                        " (count coll)=" (count coll) " index=" index))))
  (glue (take index coll) [elem]
    (drop index coll)))

; #todo rename -> replace-idx
; #todo force to vector result
; #todo if was vector, could just use (assoc the-vec idx new-val)
(s/defn replace-at :- tsk/List
  "Replaces an element in a collection at the specified index."
  [coll :- tsk/List
   index :- s/Int
   elem :- s/Any]
  (when (neg? index)
    (throw (IllegalArgumentException. (str "Index cannot be negative: " index))))
  (when (<= (count coll) index)
    (throw (IllegalArgumentException. (str "Index cannot exceed collection length: "
                                        " (count coll)=" (count coll) " index=" index))))
  (glue (take index coll)
    [elem]
    (drop (inc index) coll)))

; #todo use (idx    coll int-or-kw) as `get` replacement?
; #todo use (idx-in coll [kw's]) as `fetch-in` replacement?
; #todo allow (idx coll [low high]) like python xx( low:high )
; #todo multiple dimensions
(defn idx
  "Indexes into a vector, allowing negative index values"
  [coll-in idx-in]
  (let [data-vec (vec coll-in)
        N (count data-vec)
        >> (assert (pos? N))
        idx (mod idx-in N)
        result (clojure.core/get data-vec idx)]
    result ))

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

(def MapKeySpec (s/either [s/Any] #{s/Any}))
(s/defn validate-map-keys :- s/Any
  [tst-map :- ts/Map
   valid-keys :- MapKeySpec]
  (let [valid-keys (set valid-keys)
        map-keys   (keys tst-map)]
    (when-not (every? truthy?
                (forv [curr-key map-keys]
                  (contains-key? valid-keys curr-key)))
      (throw (IllegalArgumentException. (format "validate-map-keys: invalid key found tst-map=%s, valid-keys=%s" tst-map valid-keys))))
    tst-map))

(s/defn map-keys :- tsk/Map ; #todo README
  [map-in :- tsk/Map
   tx-fn  :- tsk/Fn
   & tx-args
   ]
  (let [tuple-seq-orig (vec map-in)
        tuple-seq-out  (for [[tuple-key tuple-val] tuple-seq-orig]
                         [ (apply tx-fn tuple-key tx-args) tuple-val])
        map-out        (into {} tuple-seq-out) ]
    map-out))

(s/defn map-vals :- tsk/Map ; #todo README
  [map-in :- tsk/Map
   tx-fn  :- tsk/Fn
   & tx-args
  ]
  (let [tuple-seq-orig (vec map-in)
        tuple-seq-out  (for [[tuple-key tuple-val] tuple-seq-orig]
                         [tuple-key (apply tx-fn tuple-val tx-args) ])
        map-out        (into {} tuple-seq-out) ]
    map-out))

(defn macro?
  "Returns true if a quoted symbol resolves to a macro. Usage:

    (println (macro? 'and))  ;=> true "
  [s]
  (-> s resolve meta :macro boolean))
    ; from Alex Miller StackOverflow answer 2017-5-6

(s/defn val= :- s/Bool
  "Compares values for equality using clojure.core/=, treating records as plain map values:

      (defrecord SampleRec [a b])
      (assert (val= (->SampleRec 1 2) {:a 1 :b 2}))   ; fails for clojure.core/= "
  [& vals]
  (let [mapify   (fn [arg]
                   (if (map? arg)
                     (into {} arg)
                     arg))
        mapified (mapv #(walk/postwalk mapify %) vals)
        result   (apply = mapified)]
    result))

; #todo allow pred fn to replace entire node in search path:
; #todo    (fn [node] (and (contains? #{:horse :dog} (grab :animal/species node))
; #todo                 (<= 1 (grab :age node) 3 )))   ; an "adolescent" animal
(s/defn ^:private ^:no-doc wild-match-impl
  [ctx :- tsk/KeyMap ; #todo more precise schema needed { :submap-ok s/Bool ... }
   pattern :- s/Any
   value :- s/Any ]
  (with-context ctx [submap-ok subset-ok subvec-ok wildcard-ok]
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
