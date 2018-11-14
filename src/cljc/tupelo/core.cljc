;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns ^:no-doc tupelo.core
  "Tupelo - Making Clojure even sweeter"
  ; We use the self-require trick to force separate compilation stages for macros
  ; See "ClojureScript Macro Tower & Loop" by Mike Fikes (2015-12-18)
  #?(:cljs          ; http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
     (:require-macros
       [tupelo.core :refer
        [it-> cond-it-> some-it->
         vals->map with-map-vals forv
         with-spy-indent spyx spyxx spy-pretty spyx-pretty
         let-spy let-spy-pretty let-some map-let* map-let lazy-cons
         try-catchall with-exception-default verify
         if-java-1-7-plus if-java-1-8-plus
         when-clojure-1-8-plus when-clojure-1-9-plus when-not-clojure-1-9-plus
         destruct lazy-gen yield yield-all matches?
        ]]))
  (:require
    [clojure.core :as cc]
    [clojure.core.async :as ca]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [clojure.test]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.schema :as tsk]

    #?@(:clj [[cheshire.core :as cheshire]
              [clojure.core.match :as ccm]
              [tupelo.types :as types]
             ]))
  #?(:clj (:import [java.io PrintStream ByteArrayOutputStream]))
)

;---------------------------------------------------------------------------------------------------
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


; #todo:  add in clear-nil-entries to recursively delete all k-v pairs where val is nil or empty?

; #todo:  create safe-map ns with non-nil/non-dup versions of assoc-in, update-in, dissoc-in (&
; singles). Basically like compiler-like guarentees against misspellings, duplicate entries, missing
; entries.

;(defmacro ^:private safe-> ; #todo: remove this
;  [expr & forms]
;  (throw (RuntimeException. "Obsolete: replace with:  (validate not-nil? (-> <expr> <forms> ))" )))


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


;                                               "1234.4567.89ab.cdef"  also valid for read
; #todo need conversion from Long -> hex string="1234-4567-89ab-cdef" (& inverse)
; #todo need rand-id/randid/rid/rid-str (rand id) -> 64 bit hex string="1234-4567-89ab-cdef"
; i[12] = Random.nextInt(); bytes += i[12].toHexString()

; #todo fix usage docs

; #todo: add (throwed? ...) for testing exceptions

;---------------------------------------------------------------------------------------------------
; #todo make sure works with cljdoc

; #todo wrap = < <= et al to throw ArityException if only 1 arg
; #todo or if not number?
; #todo wrap contains? get etc to enforce "normal" input types: map/set vs vec/list
; #todo contains-key? for map/set, contains-val? for map/set/vec/list (disable contains? for strict) (use .contains for -val)
; #todo (fnil inc 0) => (with-default-args [0 "hello" :cc]
; #todo                   some-fn-of-3-or-more-args)
; #todo    like (some-fn* (glue {0 0   1 "hello"   2 :cc} {<user args here>} ))

(defn cljs-env?     ; from plumatic schema/macros.clj
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

(defmacro if-cljs     ; from plumatic schema/macros.clj
  "Return then if we are generating cljs code and else for Clojure code.
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (cljs-env? &env) then else))

(defmacro try-catchall     ; from plumatic schema/macros.clj
  "A cross-platform variant of try-catch that catches all exceptions.
   Does not (yet) support finally, and does not need or want an exception class."
  [& body]
  (let [try-body (butlast body)
        [catch sym & catch-body :as catch-form] (last body)]
    (assert (= catch 'catch))
    (assert (symbol? sym))
    `(if-cljs
       (try ~@try-body (~'catch js/Object ~sym ~@catch-body))
       (try ~@try-body (~'catch Throwable ~sym ~@catch-body)))))

;-----------------------------------------------------------------------------
; for tupelo.string

(s/defn string-increasing? :- s/Bool ; #todo merge with general in tupelo.core
  "Returns true if a pair of strings are in increasing lexicographic order."
  [a :- s/Str
   b :- s/Str ]
  (neg? (compare a b)))

(s/defn string-increasing-or-equal? :- s/Bool ; #todo merge with general in tupelo.core
  "Returns true if a pair of strings are in increasing lexicographic order, or equal."
  [a :- s/Str
   b :- s/Str ]
  (or (= a b)
    (string-increasing? a b)))
;-----------------------------------------------------------------------------

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

(defn nl
  "Abbreviated name for `newline` "
  [] (newline))

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


(defn unlazy ; #todo need tests & docs. Use for datomic Entity?
  "Converts a lazy collection to a concrete (eager) collection of the same type."
  [coll]
  (let [unlazy-item (fn [item]
                      (cond
                        (sequential? item) (vec item)
                        (map? item) (into {} item)
                        (set? item) (into #{} item)
             #?@(:clj [ (instance? java.io.InputStream item) (slurp item) ])  ; #todo need test
                        :else item))
        result    (walk/postwalk unlazy-item coll) ]
    result ))

; #todo impl-merge *****************************************************************************

(defn has-length?
  "Returns true if the collection has the indicated length. Does not hang for infinite sequences."
  [coll n]
  (when (nil? coll) (throw (ex-info "has-length?: coll must not be nil" coll)))
  (let [take-items (cc/take n coll)
        rest-items (cc/drop n coll)]
    (and (= n (count take-items))
      (empty? rest-items))))

(defn only
  "Ensures that a sequence is of length=1, and returns the only value present.
  Throws an exception if the length of the sequence is not one.
  Note that, for a length-1 sequence S, (first S), (last S) and (only S) are equivalent."
  [coll]
  (when-not (has-length? coll 1)
    (throw (ex-info "only: num-items must=1" coll)))
  (clojure.core/first coll))

(defn onlies
  "Given an outer collection of length-1 collections, returns a sequence of the unwrapped values.
    (onlies  [ [1] [2] [3] ])  =>  [1 2 3]
    (onlies #{ [1] [2] [3] })  => #{1 2 3} "
  [coll] (into (unlazy (empty coll)) (mapv only coll)))

(defn only2
  "Given a collection like `[[5]]`, returns `5`.  Equivalent to `(only (only coll))`."
  [coll] (only (only coll)))

(defn single?
  "Returns true if the collection contains a single item.`"
  [coll] (has-length? coll 1))

(defn pair?
  "Returns true if the collection contains exactly 2 items."
  [coll] (has-length? coll 2))

(defn triple?
  "Returns true if the collection contains exactly 3 items."
  [coll] (has-length? coll 3))

(defn quad?
  "Returns true if the collection contains exactly 4 items."
  [coll] (has-length? coll 4))

; #todo make xdrop ?
(defn xtake
  "Returns the first n values from a collection.  Returns map for map colls.
  Throws if empty."
  [n coll]
  (when (or (nil? coll) (empty? coll))
    (throw (ex-info "xtake: invalid coll: " coll)))
  (let [items (cc/take n coll)
        actual (count items)]
    (when (<  actual n)
      (throw (ex-info "xtake: insufficient items" {:n n :actual actual} )))
    (if (map? coll)
      (into {} items)
      (vec items))))

(defn xfirst        ; #todo -> tests
  "Returns the first value in a list or vector. Throws if empty."
  [coll]
  (when (or (nil? coll) (empty? coll))
    (throw (ex-info "xfirst: invalid coll: " coll)))
  (nth coll 0))

; #todo fix up for maps
; #todo (it-> coll (take 2 it), (validate (= 2 (count it))), (last it))
(defn xsecond  ; #todo -> tests
  "Returns the second value in a list or vector. Throws if (< len 2)."
  [coll]
  (when (or (nil? coll) (empty? coll))
    (throw (ex-info "xsecond: invalid coll: " coll)))
  (nth coll 1))

; #todo fix up for maps
(defn xthird  ; #todo -> tests
  "Returns the third value in a list or vector. Throws if (< len 3)."
  [coll ]
  (when (or (nil? coll) (empty? coll)) (throw (ex-info "xthird: invalid coll: " coll)))
  (nth coll 2))

; #todo fix up for maps
(defn xfourth  ; #todo -> tests
  "Returns the fourth value in a list or vector. Throws if (< len 4)."
  [coll]
  (when (or (nil? coll) (empty? coll)) (throw (ex-info "xfourth: invalid coll: " coll)))
  (nth coll 3))

; #todo fix up for maps
(s/defn xlast :- s/Any ; #todo -> tests
  "Returns the last value in a list or vector. Throws if empty."
  [coll :- [s/Any]]
  (when (or (nil? coll) (empty? coll)) (throw (ex-info "xlast: invalid coll: " coll)))
  (clojure.core/last coll))

; #todo fix up for maps
(s/defn xbutlast :- s/Any ; #todo -> tests
  "Returns a vector of all but the last value in a list or vector. Throws if empty."
  [coll :- [s/Any]]
  (when (or (nil? coll) (empty? coll)) (throw (ex-info "xbutlast: invalid coll: " coll)))
  (vec (clojure.core/butlast coll)))

; #todo fix up for maps
(defn xrest ; #todo -> tests
  "Returns the last value in a list or vector. Throws if empty."
  [coll]
  (when (or (nil? coll) (empty? coll)) (throw (ex-info "xrest: invalid coll: " coll)))
  (clojure.core/rest coll))

(defn xreverse ; #todo -> tests & doc
  "Returns a vector containing a sequence in reversed order. Throws if nil."
  [coll]
  (when (nil? coll) (throw (ex-info "xreverse: invalid coll: " coll)))
  (vec (clojure.core/reverse coll)))

(s/defn xvec :- [s/Any]
  "Converts a collection into a vector. Throws if given nil."
  [coll :- [s/Any]]
  (when (nil? coll) (throw (ex-info "xvec: invalid coll: " coll)))
  (clojure.core/vec coll))

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
      :else (throw (ex-info "glue: colls must be all same type; found types=" (mapv type colls))))))

(defn glue-rows   ; #todo :- tsk/List ; #todo necessary?
  " Convert a vector of vectors (2-dimensional) into a single vector (1-dimensional).
  Equivalent to `(apply glue ...)`"
  [coll-2d          ; :- tsk/List
   ]
  (when-not (sequential? coll-2d)
    (throw (ex-info "Sequential collection required, found=" coll-2d)))
  (when-not (every? sequential? coll-2d)
    (throw (ex-info "Nested sequential collections required, found=" coll-2d)))
  (reduce into [] coll-2d))

; #todo rename to (map-of a b c ...)  ??? (re. potpuri)
(defmacro vals->map ; #todo -> README
  "Called with a list of symbols like `(vals->map a b c)` returns a map
   like {:a a :b b :c c}.

       (let [a 1
             b 2
             c 3]
         (vals->map a b c))  ;=>  {:a 1 :b 2 :c 3} }

   See `with-map-vals` for simple destructuring of such maps."
  [& symbols]
  (let [maps-list (for [symbol symbols]
                    {(keyword symbol) symbol})]
    `(glue ~@maps-list)) )

(defmacro with-map-vals ; #todo -> README
  "Given a map like {:a 1 :b 2 :c 3} (such as generated by `(vals->map a b c)`),
  performs safe `let` destructuring using `grab` like:

     (let [some-map  {:a 1 :b 2 :c 3} } ]
       (with-map-vals some-map [a b c]
          (+ a b c)))  ;=>  6

  `with-map-vals` is safe for typos since `grab` will throw is the requrested map key is not present.
  See `vals->map` for simple creation of labelled data maps."
  [the-map items-vec & forms]
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
    [1 2]  [2]        -> true "
  [a :- tsk/List
   b :- tsk/List]
  (let [len-a        (count a)
        len-b        (count b)
        cmpr         (fn [x y] (cond
                                 (= x y) :eq
                                 (< x y) :incr
                                 (> x y) :decr
                                 :else (throw (ex-info "should never get here" nil))))
        cmpr-res     (mapv cmpr a b)
        first-change (first (drop-while #{:eq} cmpr-res)) ; nil if all :eq
        ]
    (cond
      (= a b)                       false
      (= first-change :decr)        false
      (= first-change :incr)        true
      (nil? first-change)           (< len-a len-b))))

(s/defn increasing-or-equal? :- s/Bool
  "Returns true iff the vectors are in (strictly) lexicographically increasing-or-equal order
    [1 2]  [1]        -> false
    [1 2]  [1 1]      -> false
    [1 2]  [1 2]      -> true
    [1 2]  [1 2 nil]  -> true
    [1 2]  [1 2 3]    -> true
    [1 2]  [1 3]      -> true
    [1 2]  [2 1]      -> true
    [1 2]  [2]        -> true "
  [a :- tsk/List
   b :- tsk/List]
  (or (= a b)
    (increasing? a b)))

;-----------------------------------------------------------------------------
(declare clip-str
  )

;-----------------------------------------------------------------------------
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

; #todo add not-neg? not-pos? not-zero?

; #todo make coercing versions of these ->sym ->str ->kw ->int  for args of (kw, str, sym, int)

(s/defn kw->sym :- s/Symbol
  "Converts a keyword to a symbol"
  [arg :- s/Keyword]
  (symbol (name arg)))

(s/defn kw->str :- s/Str
  "Converts a keyword to a string"
  [arg :- s/Keyword]
  (name arg))

(s/defn sym->str :- s/Str
  "Converts a symbol to a string"
  [arg :- s/Symbol]
  (name arg))

(s/defn sym->kw :- s/Keyword
  "Converts a symbol to a keyword"
  [arg :- s/Symbol]
  (keyword arg))

(s/defn str->sym :- s/Symbol
  "Converts a string to a symbol"
  [arg :- s/Str]
  (symbol arg))

; #todo throw if bad string
(s/defn str->kw :- s/Keyword
  "Converts a string to a keyword"
  [arg :- s/Str]
  (keyword arg))

(s/defn str->chars  :- [s/Any]  ;  #todo  make tighter
  "Converts a string to a vector of chars"
  [arg :- s/Str]
  (vec arg))

(defn int->kw [arg]
  (keyword (str arg)))

(s/defn int->char :- s/Any    ; #todo need clj/cljs char? test
  "Convert a unicode int to a char"
  [arg :- s/Int]
  #?(:clj (char arg))
  #?(:cljs
     (do
       (assert (int? arg))
       (.fromCharCode js/String arg) ; #todo just use cljs.core/char  ???
       )))

(s/defn char->int :- s/Int
  "Convert a char to an unicode int"
  [arg :- s/Any ]   ; #todo need clj/cljs char? test
  #?(:clj (int arg))
  #?(:cljs
     (do
       (assert (= 1 (count arg)))
       (.charCodeAt arg 0))))

#?(:clj
   (defn kw->int [arg]
     (Integer/parseInt (kw->str arg))))
#?(:cljs
   (defn kw->int [arg]
     (js/parseInt (kw->str arg) 10)))

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
    :else (throw (ex-info "keep-if: coll must be sequential, map, or set." coll))))

(defn drop-if
  "Returns a vector of items in coll for which (pred item) is false (alias for clojure.core/remove)"
  [pred coll]
  (keep-if (complement pred) coll))

;-----------------------------------------------------------------------------
(defn prettify
  "Recursively walks a data structure and returns a prettified version.
  Converts all lists to vectors. Converts all maps & sets to sorted collections."
  [coll]
  (let [prettify-item (fn prettify-item [item]
                        (cond
                          (sequential? item) (vec item)
                          (map? item) (into (sorted-map) item)
                          (set? item) (into (sorted-set) item)

                          #?@(:clj [ (instance? java.io.InputStream item) (prettify-item (slurp item)) ]) ; #todo need test

                          :else item))
        result        (walk/postwalk prettify-item coll)]
    result))

(defn strcat
  "Recursively concatenate all arguments into a single string result."
  [& args]
  (let [
        ; We need to use flatten twice since the inner one doesn't change a string into a
        ; sequence of chars, nor does it affect byte-array, et al.  We eventually get
        ; seq-of-scalars which can look like [ \a \b 77 78 \66 \z ]
        seq-of-scalars (flatten
                         (for [it (keep-if not-nil? (flatten [args]))]
                           ; Note that "sequential?" returns false for sets, strings, and the various
                           ; array types.
                           (cond
                             (or
                               (sequential? it)
                               (set? it)
                               (string? it)
                               #?@(:clj [
                                         (types/byte-array? it)
                                         (types/char-array? it)
                                         (types/int-array? it)
                                         (types/long-array? it)
                                         (types/object-array? it)
                                         (types/short-array? it)
                                         ])
                               ) (seq it)

                             #?@(:clj [(instance? java.io.InputStream it) (seq (slurp it))])

                             :else it)))
        ; Coerce any integer values into character equivalents (e.g. 65 -> \A), then combine
        ; into a single string.
        result         (apply str
                         (clojure.core/map char
                           (keep-if not-nil? seq-of-scalars)))]
    result))

(defn print-versions [] ; #todo need CLJS version
  #?(:clj
     (let [version-str (format "Clojure %s    Java %s"
                         (clojure-version) (System/getProperty "java.version"))
           num-hyphen  (+ 6 (count version-str))
           hyphens     (strcat (repeat num-hyphen \-))
           version-str (strcat "   " version-str)]
       (newline)
       (println hyphens)
       (println version-str)
       (println hyphens)))
  #?(:cljs
     (let [version-str (str "ClojureScript " *clojurescript-version* )
           num-hyphen  (+ 6 (count version-str))
           hyphens     (strcat (repeat num-hyphen \-))
           version-str (strcat "   " version-str)]
       (newline)
       (println hyphens)
       (println version-str)
       (println hyphens))) )

(defn seq->str
  "Convert a seq into a string (using pr) with a space preceding each value"
  [seq-in]
  (with-out-str
    (doseq [it (seq seq-in)]
      (print \space)
      (pr it))))

;-----------------------------------------------------------------------------
; spy stuff

; #todo defn-spy  saves fn name to locals for spy printout
; #todo spyxl  adds line # to spy printout

; (def ^:dynamic *spy-enabled* false)
(def ^:dynamic *spy-enabled* true) ; #TODO fix before commit!!!

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

;-----------------------------------------------------------------------------
; #todo  Need it?-> like some-> that short-circuits on nil
(defmacro it->
  "A threading macro like as-> that always uses the symbol 'it' as the placeholder for the next threaded value:
      (it-> 1
            (inc it)
            (+ it 3)
            (/ 10 it))
      ;=> 2 "
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

(defmacro forv ; #todo wrap body in implicit do
  "Like clojure.core/for but returns results in a vector.   Not lazy."
  [& forms]
  `(vec (for ~@forms)))

;-----------------------------------------------------------------------------
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
  ([arg1 arg2]
   (let [[tag value] (cond
                       (keyword? arg1) [arg1 arg2]
                       (keyword? arg2) [arg2 arg1]
                       :else (throw (ex-info "spy: either first or 2nd arg must be a keyword tag \n   args:" [arg1 arg2])))]
     (when *spy-enabled*
       (println (str (spy-indent-spaces) tag " => " (pr-str value))))
     value ))
  ([value] ; 1-arg arity uses a generic "spy" message
   (spy :spy value)))

(defn spyx-proc
  [exprs]
  (let [r1         (for [expr (butlast exprs)]
                     (when *spy-enabled*
                       (if (keyword? expr)
                         `(when *spy-enabled* (print (str (spy-indent-spaces) ~expr \space)))
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
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& exprs]
  (spyx-proc exprs))

(defn ^:no-doc spy-pretty-proc ; #todo => core
  [exprs]
  (let [r1         (for [expr (butlast exprs)]
                     `(when *spy-enabled* (println (spy-indent-spaces) (str ~expr))))
        r2         (let [expr (xlast exprs)]
                     `(let [spy-val# ~expr]
                        (when *spy-enabled*
                          (println (indent-lines-with (spy-indent-spaces)
                                     (pretty-str spy-val#))))
                        spy-val#))
        final-code `(do
                      ~@r1
                      ~r2)]
    final-code))

; #todo only allow 1 arg + optional kw-label
(defmacro spy-pretty ; #todo => core
  "Like `spyx-pretty` but without printing the original form"
  [& exprs]
  (spy-pretty-proc exprs)) ; #todo add in use of `prettify` for each value

(defn ^:no-doc spyx-pretty-proc
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
; #todo only allow 1 arg + optional kw-label
; #todo On all spy* make print file & line number
; #todo allow spyx-pretty to have labels like (spyx-pretty :dbg-120 (+ 1 2)):  ":dbg-120 (+ 1 2) => 3"
(defmacro spyx-pretty
  "Like `spyx` but with pretty printing (clojure.pprint/pprint)"
  [& exprs]
  (spyx-pretty-proc exprs)) ; #todo add in use of `prettify` for each value

(defmacro with-spy-indent
  "Increments indentation level of all spy, spyx, or spyxx expressions within the body."
  [& forms]
  `(do
     (spy-indent-inc)
     (let [result# (do ~@forms)]
       (spy-indent-dec)
       result#)))

(defmacro let-spy
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& exprs]
  (let [decls      (xfirst exprs)
        _          (when (not (even? (count decls)))
                     (throw (ex-info "spy-let-proc: uneven number of decls:" decls)))
        forms      (xrest exprs)
        fmt-pair   (fn [[dest src]]
                     [dest src
                      '_ (list `spyx dest)]) ; #todo gensym instead of underscore?
        pairs      (vec (partition 2 decls))
        r1         (vec (mapcat fmt-pair pairs))
        final-code `(let ~r1 ~@forms)]
    final-code))

;-----------------------------------------------------------------------------

(defmacro let-spy-pretty   ; #todo -> deprecated
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expressions, printing both the expression and its value to stdout. Returns the value of the
   last expression."
  [& exprs]
  (let [decls (xfirst exprs)
        _     (when (not (even? (count decls)))
                (throw (ex-info "spy-let-pretty-impl: uneven number of decls:" decls)))
        forms (xrest exprs)
        fmt-pair (fn [[dest src]]
                   [dest src
                    '_ (list `spyx-pretty dest)] ) ; #todo gensym instead of underscore?
        pairs (vec (partition 2 decls))
        r1    (vec (mapcat  fmt-pair pairs ))
        final-code  `(let ~r1 ~@forms ) ]
    final-code ))

(defmacro let-some
  "Threads forms as with `when-some`, but allow more than 1 pair of binding forms."
  [bindings & forms]
  (let [num-bindings (count bindings)]
    (when-not (even? num-bindings)
      (throw (ex-info (str "num-bindings must be even; value=" num-bindings) bindings)))
    (if (pos? num-bindings)
      `(let [result# ~(cc/second bindings)]
         (when (not-nil? result#)
           (let [~(cc/first bindings) result#]
             (let-some ~(cc/drop 2 bindings) ~@forms))))
      `(do ~@forms))))


; #todo fix so doesn't hang if give infinite lazy seq
; #todo rename :strict -> :trunc
(defmacro map-let*
  "Usage:  (map-let* ctx bindings & forms)

  where ctx is a map with default values:
    {:strict true
     :lazy   false}"
  [context bindings & forms]
  (when (empty? bindings)
    (throw (ex-info "map-let*: bindings cannot be empty=" bindings)))
  (when-not (even? (count bindings))
    (throw (ex-info "map-let*: (count bindings) must be even=" bindings)))
  (when-not (pos? (count forms))
    (throw (ex-info  "map-let*: forms cannot be empty=" forms)))
  (let [binding-pairs (partition 2 bindings)
        syms          (mapv xfirst binding-pairs)
        colls         (mapv xsecond binding-pairs) ]
    `(do
       (when-not (map? ~context)
         (throw (ex-info  "map-let*: context must be a map=" ~context)))
       (let [lazy#          (get ~context :lazy false)
             strict#        (get ~context :strict true)
             lengths#       (mapv count ~colls)
             lengths-equal# (apply = lengths#)
             map-fn#        (fn ~syms ~@forms)
             output-fn#     (if lazy# identity vec)]
         (when (and strict#
                 (not lengths-equal#))
           (throw (ex-info  "map-let*: colls must all be same length; lengths=" lengths#)))
         (output-fn# (map map-fn# ~@colls))))))

(defmacro map-let
  "Usage:
    (map-let bindings & forms)

  Given bindings and forms like `(map-let [x xs, y ys, ...] (+ x y))`, will iterate over the
  collections [xs ys ...] assigning successive values of each collection to [x y ...], respectively.
  The local symbols [x y ...] can then be used in `forms` to generate the output mapping.
  Will throw if collections are not all of the same length. Not lazy."
  [bindings & forms]
  `(map-let* {:strict true
              :lazy   false}
     ~bindings ~@forms))

(s/defn append :- tsk/List
  "Given a sequential object (vector or list), add one or more elements to the end."
  [listy       :- tsk/List
   & elems     :- [s/Any] ]
  (when-not (sequential? listy)
    (throw (ex-info  "Sequential collection required, found=" listy)))
  (when (empty? elems)
    (throw (ex-info "Nothing to append! elems=" elems)))
  (vec (concat listy elems)))

(s/defn prepend :- tsk/List
  "Given a sequential object (vector or list), add one or more elements to the beginning"
  [& args]
  (let [elems (butlast args)
        listy (xlast args)]
    (when-not (sequential? listy)
      (throw (ex-info  "Sequential collection required, found=" listy)))
    (when (empty? elems)
      (throw (ex-info "Nothing to prepend! elems=" elems)))
    (vec (concat elems listy))))

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
      (throw (ex-info  "zip*: colls must all be same length; lengths=" lengths)))
    (vec (apply map vector colls))))
; #todo fix so doesn't hang if give infinite lazy seq. Technique:
;  (def x [1 2 3])
;  (seq (drop 2 x))          =>  (3)
;  (seq (drop 3 x))          =>  nil
;  (nil? (seq (drop 3 x)))   =>  true
;  (nil? (drop 3 (range)))   =>  false

; #todo rename :strict -> :trunc
(defn zip*
  "Usage:  (zip* context & colls)
  where context is a map with default values:  {:strict true}
  Not lazy. "
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
              (throw (ex-info "zip*: collections are not all same length; empty-flgs=" empty-flgs)))
            result))))))

; #todo add schema; result = tsk/List[ tsk/Pair ]
; #todo add :trunc & assert;
(defn zip
  "Zips together vectors producing a vector of tuples (like Python zip). Not lazy.
  Example:

     (zip [:a :b :c] [1 2 3]) ->  [ [:a 1] [:b 2] [:c 3] ]

   ***** WARNING - will hang for infinite length inputs ***** "
  ; #todo ***** WARNING - will hang for infinite length inputs *****
  ; #todo fix so doesn't hang if give infinite lazy seq. Technique:
  ; #todo Use (zip ... {:trunc true}) if you want to truncate all inputs to the length of the shortest.
  [& args]
  (assert #(every? sequential? args))
  (apply zip* {:strict true} args))

(defn zip-lazy
  "Usage:  (zip-lazy coll1 coll2 ...)
      (zip-lazy xs ys zs) -> [ [x0 y0 z0]
                               [x1 y1 z1]
                               [x2 y2 z2]
                               ... ]

  Returns a lazy result. Will truncate to the length of the shortest collection.
  A convenience wrapper for `(map vector coll1 coll2 ...)`.  "
  [& colls]  ; #todo how use Schema with "rest" args?
  (assert #(every? sequential? colls))
  (apply map vector colls))

(defn indexed
  "Given one or more collections, returns a sequence of indexed tuples from the collections:
      (indexed xs ys zs) -> [ [0 x0 y0 z0]
                              [1 x1 y1 z1]
                              [2 x2 y2 z2]
                              ... ] "
  [& colls]
  (apply zip-lazy (range) colls))

(defmacro lazy-cons
  "The simple way to create a lazy sequence:
      (defn lazy-next-int [n]
        (t/lazy-cons n (lazy-next-int (inc n))))
      (def all-ints (lazy-next-int 0)) "
  [curr-val recursive-call-form]
  `(lazy-seq (cons ~curr-val ~recursive-call-form)))

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
      (throw (ex-info "Must specify either :digits or :tol" opts)))
    (when tol
      (when-not (number? tol)
        (throw (ex-info ":tol must be a number" opts)))
      (when-not (pos? tol)
        (throw (ex-info ":tol must be positive" opts))))
    (when digits
      (when-not (integer? digits)
        (throw (ex-info ":digits must be an integer" opts)))
      (when-not (pos? digits)
        (throw (ex-info ":digits must positive" opts))))
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
      (throw (ex-info ": x-vals & y-vals must be same length" (vals->map num-x num-y)))))
  (every? truthy?
    (clojure.core/map #(apply rel= %1 %2 opts)
      x-vals y-vals)))

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
;  #todo range version => (butlast (thru ...))
(defn thru          ; #todo make lazy: (thruz ...) -> (thru* {:lazy true} ...)
  "Returns a sequence of integers. Like clojure.core/rng, but is inclusive of the right boundary value. Not lazy. "
  ([end]       (thru 0 end))
  ([start end] (thru start end 1))
  ([start end step]
   (let [delta          (- (double end)   (double start))
         nsteps-dbl     (/ (double delta) (double step))
         nsteps-int     (Math/round nsteps-dbl)
         rounding-error (Math/abs (- nsteps-dbl nsteps-int)) ]
     (when (< 0.00001 rounding-error)
       (throw (ex-info "thru: non-integer number of steps \n   args:" (vals->map start end step))))
     (vec (clojure.core/map #(-> %
                               (* step)
                               (+ start))
            (range (inc nsteps-int)))))))

; #todo need CLJ/CLJS coercion ->kw (char/str, sym)
; #todo need CLJ/CLJS coercion ->sym (char/str, kw, sym)
; #todo need CLJ/CLJS coercion ->str (char/str, kw, sym, )

; #todo need CLJS coercion ->char (char/str or int)
; #todo need CLJS coercion ->int (char/str or int)
; #todo need CLJS version of char? => len-1 string
; #todo need test, readme
; #todo merge into `thru` using a protocol for int, double, char, string, keyword, symbol, other?
(defn chars-thru    ; #todo add schema.
  "Given two characters (or numerical equivalents), returns a seq of characters
  (inclusive) from the first to the second.  Characters must be in ascending order."
  [start-char stop-char]
  ; #todo throw if not char or int
  (let [start-int   (if (integer? start-char) start-char (char->int start-char))
        stop-int    (if (integer? stop-char) stop-char (char->int stop-char))
        thru-vals   (thru start-int stop-int)
        char-vals   (mapv int->char thru-vals) ]
    (when (< 65535 stop-int) ; #todo cleanup limit
      (throw (ex-info "chars-thru: stop-int too large" (vals->map start-int stop-int))))
    (when-not (<= start-int stop-int)
      (throw (ex-info "chars-thru: start-char must come before stop-char." (vals->map start-int stop-int))))
    char-vals))

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
      (throw (ex-info "Key seq not present in map:" (vals->map the-map keys-vec)))
      result)))

(s/defn fetch :- s/Any
  "A fail-fast version of keyword/map lookup.  When invoked as (fetch the-map :the-key),
   returns the value associated with :the-key as for (clojure.core/get the-map :the-key).
   Throws an Exception if :the-key is not present in the-map."
  [the-map :- tsk/Map
   the-key :- s/Any]
  (fetch-in the-map [the-key]))

; #todo:  (grab [:person :address :zip] the-map)  => fetch-in
; #todo:  (grab-all :name :phone [:address :zip] the-map)
; #todo:      => (mapv #(grab % the-map) keys)
(s/defn grab :- s/Any
  "A fail-fast version of keyword/map lookup.  When invoked as (grab :the-key the-map),
   returns the value associated with :the-key as for (clojure.core/get the-map :the-key).
   Throws an Exception if :the-key is not present in the-map."
  [the-key :- s/Any
   the-map :- tsk/Map]
  (fetch-in the-map [the-key]))

(defrecord Unwrapped [data])
(s/defn unwrap :- Unwrapped
  "Works with the `->vector` function to unwrap vectors/lists to insert
  their elements as with the unquote-spicing operator (~@). Examples:

      (->vector 1 2 3 4 5 6 7 8 9)              =>  [1 2 3 4 5 6 7 8 9]
      (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9)   =>  [1 2 3 4 5 6 7 8 9] "
  [data :- [s/Any]]
  (assert (sequential? data))
  (->Unwrapped data))

(s/defn ->vector :- [s/Any]
  "Wraps all args in a vector, as with `clojure.core/vector`. Will (recursively) recognize
  any embedded calls to (unwrap <vec-or-list>) and insert their elements as with the
  unquote-spicing operator (~@). Examples:

      (->vector 1 2 3 4 5 6 7 8 9)              =>  [1 2 3 4 5 6 7 8 9]
      (->vector 1 2 3 (unwrap [4 5 6]) 7 8 9)   =>  [1 2 3 4 5 6 7 8 9] "
  [& args :- [s/Any]]
  (let [result (reduce (fn [accum item]
                         (let [it-use (cond
                                        (sequential? item) [ (apply ->vector item) ]
                                        (instance? Unwrapped item) (apply ->vector (fetch item :data))
                                        :else [item])
                               accum-out (glue accum it-use ) ]
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

(defn cond-it-impl
  [expr & forms]
  (let [num-forms (count forms)]
    (when-not (even? num-forms)
      (throw (ex-info "num-forms must be even; value=" (vals->map num-forms forms)))))
  (let [cond-action-pairs (partition 2 forms)
        cond-action-forms (for [[cond-form action-form] cond-action-pairs]
                            `(if ~cond-form
                               ~action-form
                               ~'it)) ]
    `(it-> ~expr ~@cond-action-forms)))

(defmacro cond-it->
  "A threading macro like as-> that always uses the symbol 'it' as the placeholder for the next threaded value:

    (let [params {:a 1 :b 1 :c nil :d nil}]
      (cond-it-> params
        (:a it)        (update it :b inc)
        (= (:b it) 2)  (assoc it :c \"here\")
        (:c it)        (assoc it :d \"again\")))

    ;=> {:a 1, :b 2, :c \"here\", :d \"again\"}"
  [& forms]
  (apply cond-it-impl forms))

; #todo #wip
(defmacro some-it->
  "Threads forms as with `it->`, terminates & returns `nil` if any expression is nil."
  [expr & forms]
  (let [binding-pairs (interleave (repeat 'it) forms) ]
    `(let-some [~'it ~expr
                            ~@binding-pairs]
       ~'it)))

(defmacro with-exception-default
  "Evaluates body & returns its result.  In the event of an exception, default-val is returned
   instead of the exception."
  [default-val & forms] ; :default
  `(try-catchall ~@forms (catch e# ~default-val)))

(defn validate
  "(validate tst-fn tst-val)
  Used to validate intermediate results. Returns tst-val if the result of
  (tst-fn tst-val) is truthy.  Otherwise, throws ex-info with ex-data
  {:sample-val sample-val :tst-result tst-result}."
  [tst-fn tst-val]
  (let [tst-result (tst-fn tst-val)]
    (when-not (truthy? tst-result)
      (throw (ex-info  "validate: " (vals->map tst-val tst-result))))
    tst-val))

(defn validate-or-default
  "Returns `sample-val` if `(is-valid? sample-val)` is truthy; else returns `default-val`"
  [is-valid? sample-val default-val]
  (if (is-valid? sample-val)
    sample-val
    default-val))

(defn with-nil-default
  "Returns `sample-val` if not nil; else returns `default-val`"
  [default-val sample-val]
  (validate-or-default not-nil? sample-val default-val))

(defmacro verify
  "(verify <some-expr>)
  Used to verify intermediate results. Returns value of <some-expr> if the result
  is truthy.  Otherwise, throws IllegalArgumentException."
  [form]
  `(let [value# ~form]
     (if (truthy? value#)
       value#
       (throw (ex-info "verification failed  " '~form)))))

;-----------------------------------------------------------------------------
; #todo need option for (take 3 coll :exact) & drop; xtake xdrop

; #todo fix up for maps
(defn rand-elem
  "Returns a random element from a collection"
  [coll]
  (verify (not-nil? coll))
  (rand-nth (vec coll)))

; #todo add (->sorted-map <map>)        => (into (sorted-map) <map>)
; #todo add (->sorted-set <set>)        => (into (sorted-set) <set>)
; #todo add (->sorted-vec <sequential>) => (vec (sort <vec>))

(s/defn lexical-compare :- s/Int
  "Performs a lexical comparison of 2 sequences, sorting as follows:
      [1]
      [1 :a]
      [1 :b]
      [1 :b 3]
      [2]
      [3]
      [3 :y] "
  [a :- tsk/List
   b :- tsk/List]
  (cond
    (= a b) 0
    (empty? a) -1
    (empty? b) 1
    :else (let [a0 (xfirst a)
                b0 (xfirst b)]
            (if (= a0 b0)
              (lexical-compare (xrest a) (xrest b))
              (compare a0 b0)))))

; #todo maybe submap-without-keys, submap-without-vals ?
; #todo filter by pred in addition to set/list?
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
 ;(println :awt00 map-arg)
  (let [keep-keys (set keep-keys)]
   ;(println :awt01 keep-keys)
   ;(println :awt02 opts)
    (if (= opts [:missing-ok])
      (do
       ;(println :awt10 )
        (apply glue {}
          (for [key keep-keys]
            (with-exception-default {}
              {key (grab key map-arg)}))))
      (do
       ;(println :awt20 )
        (apply glue {}
          (for [key keep-keys]
            {key (grab key map-arg)}))))))

; #todo -> README
(s/defn submap-by-vals :- tsk/Map
  "Returns a new map containing entries with the specified vals. Throws for missing vals,
  unless `:missing-ok` is specified. Usage:

      (submap-by-vals {:a 1 :b 2 :A 1} #{1  }             )  =>  {:a 1 :A 1}
      (submap-by-vals {:a 1 :b 2 :A 1} #{1 9} :missing-ok )  =>  {:a 1 :A 1} "
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
      (throw (ex-info "submap-by-vals: " (vals->map missing-vals map-arg))))))

; #todo need README
(s/defn submap? :- s/Bool
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

(s/defn keyvals-seq-impl :- [s/Any]
  [ctx :- tsk/KeyMap]
  (with-map-vals ctx [missing-ok the-map the-keys]
    (apply glue
      (for [key the-keys]
        (let [val (get the-map key ::missing)]
          (if-not (= val ::missing)
            [key val]
            (if missing-ok
              []
              (throw (ex-info "Key not present in map:" (vals->map the-map key))))))))))

(s/defn keyvals-seq :- [s/Any]
  "For any map m, returns the (alternating) keys & values of m as a vector, suitable for reconstructing m via
   (apply hash-map (keyvals m)). (keyvals {:a 1 :b 2} => [:a 1 :b 2]

     Usage:  (keyvals-seq ctx) ctx-default: {:missing-ok false}
             (keyvals-seq the-map the-keys) "
  ([ctx :- tsk/KeyMap ]
    (let [defaults {:missing-ok false}]
      (keyvals-seq-impl (into defaults ctx))))
  ([the-map :- tsk/KeyMap
    the-keys :- [s/Any]]
    (keyvals-seq (vals->map the-map the-keys))) )

; #todo rename -> drop-idx
; #todo force to vector result
; #todo allow range to drop
(s/defn drop-at :- tsk/List
  "Removes an element from a collection at the specified index."
  [coll :- tsk/List
   index :- s/Int]
  (when (neg? index)
    (throw (ex-info "Index cannot be negative " index)))
  (when (<= (count coll) index)
    (throw (ex-info "Index cannot exceed collection length: " {:length (count coll) :index index})))
  (glue (take index coll)
    (drop (inc index) coll)))

; #todo rename -> insert-idx
; #todo force to vector result
; #todo allow vector to insert
(s/defn insert-at :- tsk/List
  "Inserts an element into a collection at the specified index."
  [coll :- tsk/List
   index :- s/Int
   elem :- s/Any]
  (when (neg? index)
    (throw (ex-info "Index cannot be negative " index)))
  (when (< (count coll) index)
    (throw (ex-info "Index cannot exceed collection length: "  {:length (count coll) :index index} )))
  (glue (take index coll) [elem]
    (drop index coll)))

; #todo rename -> elem-set
; #todo force to vector result
; #todo allow idx range to replace with vector (maybe not equal # of elems)
(s/defn replace-at :- tsk/List
  "Replaces an element in a collection at the specified index."
  [coll :- tsk/List
   index :- s/Int
   elem :- s/Any]
  (when (neg? index)
    (throw (ex-info "Index cannot be negative " index)))
  (when (<= (count coll) index)
    (throw (ex-info "Index cannot exceed collection length: " {:length (count coll) :index index} )))
  (glue
    (take index coll)
    [elem]
    (drop (inc index) coll)))

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

; #todo use (idx    coll int-or-kw) as `get` replacement?
; #todo use (idx-in coll [kw's]) as `fetch-in` replacement?
; #todo allow (idx coll [low high]) like python xx( low:high )
; #todo multiple dimensions
(s/defn idx
  "Indexes into a vector, allowing negative index values"
  [coll       :- tsk/List
   index-val  :- s/Int]
  (when (nil? coll)
    (throw (ex-info "idx: coll cannot be nil: " coll)))
  (let [data-vec (vec coll)
        N        (count data-vec)
        >>       (assert (pos? N))
        ii       (mod index-val N)
        >>       (when (<= (count coll) ii)
                   (throw (ex-info "Index cannot exceed collection length: " {:len (count coll) :index ii})))
        result   (clojure.core/get data-vec ii)]
    result))

(s/defn map-keys :- tsk/Map ; #todo README
  "Transforms each key in a map using the supplied `tx-fn`:

    (t/map-keys {1 :a 2 :b 3 :c} inc)                  =>  {  2 :a   3 :b 4   :c}
    (t/map-keys {1 :a 2 :b 3 :c} {1 101 2 202 3 303})  =>  {101 :a 202 :b 303 :c}"
  [map-in :- tsk/Map
   tx-fn  :- tsk/Fn
   & tx-args ]
  (let [tuple-seq-orig (vec map-in)
        tuple-seq-out  (for [[tuple-key tuple-val] tuple-seq-orig]
                         [ (apply tx-fn tuple-key tx-args) tuple-val])
        map-out        (into {} tuple-seq-out) ]
    map-out))

(s/defn map-vals :- tsk/Map ; #todo README ; #todo docstring, README
  "Transforms each value in a map using the supplied `tx-fn`:

      (t/map-vals {:a 1 :b 2 :c 3} inc)                  =>  {:a 2,   :b 3,   :c 4}
      (t/map-vals {:a 1 :b 2 :c 3} {1 101 2 202 3 303})  =>  {:a 101, :b 202, :c 303} "
  [map-in :- tsk/Map
   tx-fn  :- tsk/Fn
   & tx-args ]
  (let [tuple-seq-orig (vec map-in)
        tuple-seq-out  (for [[tuple-key tuple-val] tuple-seq-orig]
                         [tuple-key (apply tx-fn tuple-val tx-args) ])
        map-out        (into {} tuple-seq-out) ]
    map-out))

(def MapKeySpec (s/either [s/Any] #{s/Any}))
(s/defn validate-map-keys :- s/Any ; #todo docstring, README
  [tst-map :- tsk/Map
   valid-keys :- MapKeySpec]
  (let [valid-keys (set valid-keys)
        map-keys   (keys tst-map)]
    (when-not (every? truthy?
                (forv [curr-key map-keys]
                  (contains-key? valid-keys curr-key)))
      (throw (ex-info "validate-map-keys: invalid key found " (vals->map tst-map valid-keys))))
    tst-map))





; #todo ***** toptop **********************************************************************************
#?(:clj (do

(ns-unmap *ns* 'first) ; #todo -> (set-tupelo-strict! true/false)
(ns-unmap *ns* 'second)
(ns-unmap *ns* 'rest)
(ns-unmap *ns* 'next)
(ns-unmap *ns* 'last)

;-----------------------------------------------------------------------------
; Clojure version stuff
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
  (string-increasing-or-equal? version-str (java-version)))

; #todo need min-java-1-8  ???

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

(defmacro if-java-1-8-plus ; #todo need for java 9, 10, 11, ...
  "If JVM is Java 1.8 or higher, evaluates if-form into code. Otherwise, evaluates else-form."
  [if-form else-form]
  (if (is-java-1-8-plus?)
    `(do ~if-form)
    `(do ~else-form)))

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

(defmacro spyxx
  "An expression (println ...) for use in threading forms (& elsewhere). Evaluates the supplied
   expression, printing both the expression, its type, and its value to stdout, then returns the value."
  [expr]
  `(let [spy-val#    ~expr
         class-name# (if-cljs
                       (-> spy-val# type)
                       (-> spy-val# class .getName) )]
     (when *spy-enabled*
       (println (str (spy-indent-spaces) '~expr " => <#" class-name# " " (pr-str spy-val#) ">")))
     spy-val#))


; #todo gogo ---------------------------------------------------------------------------------------------------


; #todo Need safe versions of:
; #todo    + - * /  (others?)  (& :strict :safe reassignments)
; #todo    and, or    (& :strict :safe reassignments)
; #todo    = not=   (others?)  (& :strict :safe reassignments)
; #todo    (drop-last N coll)  (take-last N coll)
; #todo    subvec
; #todo    others???

; #todo add postwalk and change to all sorted-map, sorted-set
; #todo rename to pp or pprint ?
; #todo add test & README
(defn pretty                                                ; #todo experimental
  "Shortcut to clojure.pprint/pprint. Returns it (1st) argument."
  ([arg]
   (pprint/pprint arg)
   arg)
  ([arg writer]
   (pprint/pprint arg writer)
   arg))

; #todo add test & README
; #todo defer to tupelo.core/pretty
(defn pretty-str
  "Returns a string that is the result of clojure.pprint/pprint"
  [arg]
  (with-out-str (pprint/pprint arg)))

(comment
  (is= (merge-deep  ; #todo need a merge-deep where
         {:a {:b 2}}
         {:a {:c 3}})
    {:a {:b 2
         :c 3}}))

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

(s/defn sequential->idx-map :- {s/Any s/Any} ; #todo move
  [data :- [s/Any]]
  (into (sorted-map)
    (map-indexed (fn [idx val] [idx val])
      data)))

(defn char->sym [ch] (symbol (str ch))) ; #todo move

(defn get-in-strict [data path] ; #todo move
  (let [result (get-in data path ::not-found)]
    (when (= result ::not-found)
      (throw (ex-info "destruct(get-in-strict): value not found" {:data data :path path})))
    result))

(defn ^:no-doc destruct-tmpl-analyze
  [ctx]
  (with-map-vals ctx [parsed path tmpl]
    ;(spyx path)
    (cond
      (map? tmpl)
      (doseq [entry tmpl]
        ;(spyx entry)
        (let [[curr-key curr-val] entry]
          ;(spyx [curr-key curr-val])
          (let [path-new (append path curr-key)]
            ;(spyx path-new)
            (if (symbol? curr-val)
              (let [var-sym (if (= curr-val (char->sym \?))
                              (kw->sym curr-key)
                              curr-val)]
                (swap! parsed append {:path path-new :name var-sym}))
              (destruct-tmpl-analyze {:parsed parsed :path path-new :tmpl curr-val})))))

      (sequential? tmpl)
      (do
        ;(spy :tmpl-51 tmpl)
        (destruct-tmpl-analyze {:parsed parsed :path path :tmpl (sequential->idx-map tmpl)}))

      :else (println :oops-44))))

(defn ^:no-doc is-restruct-one?
  "Return true if receive a form like either `(restruct)` or `(restruct info)` (i.e. either zero or one symbol args)."
  [form]
  (and (list? form)
    (= 2 (count form))
    (= 'restruct (cc/first form))))

(defn ^:no-doc destruct-impl
  [bindings forms]
  (when (not (even? (count bindings)))
    (throw (ex-info "destruct: uneven number of bindings:" bindings)))
  (when (empty? bindings)
    (throw (ex-info "destruct: bindings empty:" bindings)))
  (let [binding-pairs (partition 2 bindings)
        datas         (mapv cc/first binding-pairs)
        tmpls         (mapv cc/second binding-pairs)
        tmpls-parsed  (vec (for [tmpl tmpls]
                             (let [parsed (atom [])]
                               (destruct-tmpl-analyze {:parsed parsed :path [] :tmpl tmpl})
                               @parsed)))]
    ; (spyx tmpls-parsed)
    ; look for duplicate variable names
    (let [var-names (vec (for [tmpl-parsed   tmpls-parsed
                               path-name-map tmpl-parsed]
                           (grab :name path-name-map)))]
      ; (spyx var-names)
      (when (not= var-names (distinct var-names))
        (println "destruct: var-names not unique" var-names)
        (throw (ex-info "destruct: var-names not unique" var-names))))

    (let [data-parsed-pairs (zip datas tmpls-parsed)]
      ; (spyx data-parsed-pairs)
      (let [extraction-pairs    (apply glue
                                  (for [[data parsed] data-parsed-pairs
                                        {:keys [name path]} parsed]
                                    [name `(get-in-strict ~data ~path)]))
            ; >>   (do (nl) (spyx extraction-pairs))

            construct-one-pairs (apply glue
                                  (for [[data parsed] data-parsed-pairs]
                                    {data (apply glue
                                            (for [{:keys [name path]} parsed]
                                              `[~data (assoc-in ~data ~path ~name)]))}))
            ; >>   (do (nl) (spyx-pretty construct-one-pairs))

            construct-all-pairs (apply glue (vals construct-one-pairs))
            ; >>   (spyx-pretty construct-all-pairs)

            restruct-one-defs   (apply glue
                                  (for [[data construction-pairs] construct-one-pairs]
                                    {data (apply list `[fn []
                                                        (let [~@construction-pairs]
                                                          ~data)])}))
            ; >>   (do (nl) (spyx-pretty restruct-one-defs))

            restruct-only-def   (when (= 1 (count datas))
                                  (let [[data construction-pairs] (only construct-one-pairs)] ; #todo test single data case
                                    (apply list `[fn []
                                                  (let [~@construction-pairs]
                                                    ~data)])))
            ; >>   (do (nl) (spyx-pretty restruct-only-def))

            restruct-all-def    (apply list `[fn []
                                              (let [~@construct-all-pairs]
                                                (vals->map ~@datas))])
            ; >>   (do (nl) (spyx-pretty restruct-all-def))

            res-raw             `(let [~@extraction-pairs]
                                   ~@forms)
            ; >>   (do (nl) (spyx-pretty res-raw))

            res-all             (walk/postwalk
                                  (fn [form]
                                    (if (not= form '(restruct-all))
                                      form
                                      (list 'let ['restruct-fn restruct-all-def
                                                  'result (list 'restruct-fn)]
                                        'result)))
                                  res-raw)
            ; >>   (do (nl) (spyx-pretty res-all))

            res-one             (walk/postwalk
                                  (fn [form]
                                    (if-not (is-restruct-one? form)
                                      form
                                      (let [restr-one-data (cc/second form)
                                            restr-one-def  (get restruct-one-defs restr-one-data ::not-found) ]
                                        (list 'let ['restruct-fn restr-one-def
                                                    'result (list 'restruct-fn)]
                                          'result))))
                                  res-all)
            ; >>   (do (nl) (spyx-pretty res-one))

            res-only            (walk/postwalk
                                  (fn [form]
                                    (if (not= form '(restruct))
                                      form
                                      (if (not= 1 (count datas)) ; #todo test exception works
                                        (do
                                          (println "(restruct) error: more than 1 data src present" datas)
                                          (throw (IllegalArgumentException. "restruct:  aborting...")))
                                        (list 'let ['restruct-fn restruct-only-def
                                                    'result (list 'restruct-fn)]
                                          'result))))
                                    res-one) ]
        ; (do (nl) (spyx-pretty res-only))
        res-only ))))

(defmacro destruct
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
  (destruct-impl bindings forms))

(defn restruct
  "within a `(destruct [<data> <shape>] ...) form, `(restruct)` or `(restruct <data>)` causes re-structuring
   & return of original data shape using current values."
  [& args] (throw (ex-info "restruct: illegal usage - should never get here." args)))

(defn restruct-all
  "within a `(destruct [data-1 <shape-1>
                        data-2 <shape-2] ...) form, causes re-structuring & return of original data shapes using
  current values as with (vals->map data-1 data-2 ...)"
  [& args] (throw (ex-info "restruct-all: illegal usage - should never get here." args)))

; #todo max-key -> t/max-by

(defn chan->lazy-seq ; #todo add schema, add tests, readme
  "Accepts a core.async channel and returns the contents as a lazy list."
  [chan]
  (let [curr-item (ca/<!! chan)] ; #todo ta/take-now!
    (when (not-nil? curr-item)
      (lazy-cons curr-item (chan->lazy-seq chan)))))

; #todo document use via binding
(def ^:dynamic *lazy-gen-buffer-size*
  "Default output buffer size for `lazy-gen`."
  32)

; #todo add to README
; #todo fix SO posting:  defgen -> lazy-gen
; #todo make null case return [] instead of nil
; #todo make eager version?  gen-vec, gen-seq, ...
(defmacro lazy-gen
  "Creates a 'generator function' that returns a lazy seq of results
  via `yield` (a la Python)."
  [& forms]
  `(let [~'lazy-gen-output-buffer (ca/chan *lazy-gen-buffer-size*) ]
        (ca/go
          ~@forms
          (ca/close! ~'lazy-gen-output-buffer))
        (chan->lazy-seq ~'lazy-gen-output-buffer)))

(defmacro yield ; #todo put-now/put-later & dynamic
  "Within a 'generator function' created by `lazy-gen`, populates the
  result lazy seq with the supplied value (a la Python). Returns the value."
  [value]
  `(do
     (ca/>! ~'lazy-gen-output-buffer ~value)
     ~value))

(defmacro yield-all ; #todo maybe pattern after `restruct` and make function + dynamic value???
  "Within a 'generator function' created by `lazy-gen`, populates the
  result lazy seq with each item from the supplied collection. Returns the collection."
  [values]
  `(do
     (doseq [value# ~values]
       (yield value#))
     (vec ~values)))

; #todo make replace-in that is like assoc-in but verifies path first !!! (merge with replace-at)

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
            [out-rest unprocessed] (split-using pred (cc/rest vals))
            out-vals   (glue out-first out-rest)
            new-result (append result out-vals)
            ]
        (recur unprocessed new-result)))))

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

(defn macro?
  "Returns true if a quoted symbol resolves to a macro. Usage:

    (println (macro? 'and))  ;=> true "
  [s]
  (-> s resolve meta :macro boolean))
    ; from Alex Miller StackOverflow answer 2017-5-6

(s/defn val= :- s/Bool ; maybe value=  or   map=  (like set=)
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

(defn wild-match-root? ; #todo readme
  [ctx-in]
  (let [ctx (glue {:submap-ok   false
                   :subset-ok   false
                   :subvec-ok   false
                   :wildcard-ok true }
              ctx-in)]
    (with-map-vals ctx [pattern values]
      (every? truthy?
        (for [value values]
          (wild-match-impl ctx pattern value))))))


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
  (wild-match-root? ctx))

(defmethod wild-match? :classic
  [pattern & values]
  (verify #(pos? (count values)))
  (wild-match-root?
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
; #todo   line-seq et al not lazy (+ tupelo.lazy orig)

;---------------------------------------------------------------------------------------------------
; DEPRECATED functions

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(when-not-clojure-1-9-plus
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


))

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
;     macro?
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
;      ; (refer 'tupelo.core :only '[   ])
;    )
;    (when (contains? flags :strict)
;      ; #todo unlink/relink troublesome clojure.core stuff
;    )))

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

