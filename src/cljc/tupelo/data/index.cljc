;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.data.index
  (:refer-clojure :exclude [load ->VecNode])
  (:use tupelo.core) ; #todo remove for cljs
  #?(:clj (:require
            [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab glue map-entry indexed
                                       forv vals->map fetch-in
                                       ]]
            [tupelo.lexical :as lex]
            [tupelo.schema :as tsk]
            [clojure.data.avl :as avl]
            [clojure.set :as set]
            [schema.core :as s]
            ))
  #?(:cljs (:require
             [tupelo.core :as t :refer [spy spyx spyxx spyx-pretty grab]] ; #todo :include-macros true
             [tupelo.lexical :as lex]
             [tupelo.schema :as tsk]
             [clojure.data.avl :as avl]
             [clojure.set :as set]
             [schema.core :as s]
             ))

  )

; #todo add indexes
; #todo add sets (primative only or HID) => map with same key/value
; #todo copy destruct syntax for search

#?(:cljs (enable-console-print!))

; #todo Tupelo Data Language (TDL)

;---------------------------------------------------------------------------------------------------
; index stuff
(def LexicalValType tsk/Vec)
(def SortedSetType (class (avl/sorted-set 1 2 3)))
(def SortedMapType (class (avl/sorted-map :a 1 :b 2 :c 3)))

(def IndexType  SortedSetType )
(def IndexEntryType  tsk/Vec )

(s/defn ->sorted-set-avl :- SortedSetType
  "Converts a set into a lexically-sorted set"
  ([] (->sorted-set-avl #{}))
  ([some-set :- (s/cond-pre tsk/Set tsk/Vec)]
   (into (avl/sorted-set-by lex/compare-lex) some-set)))
; #todo add (->sorted-map <map>)        => (into (sorted-map) <map>)
; #todo add (->sorted-vec <sequential>) => (vec (sort <vec>))

(s/defn empty-index
  "Returns a new, empty index"
  [] (->sorted-set-avl))

(s/defn bound-lower :- tsk/Vec
  "Given a lexical value as a vector such as [1 :a], returns a lower bound like [1]"
  [val :- tsk/Vec]
  (when (zero? (count val))
    (throw (ex-info "Cannot find lower bound for empty vec" {:val val})))
  (t/xbutlast val))

(s/defn prefix-match? :- s/Bool
  "Returns true if the sample value equals the pattern when truncated to the same length"
  [pattern :- LexicalValType
   sample :- LexicalValType]
  (= pattern (t/xtake (count pattern) sample)))

(s/defn split-key-prefix :- {s/Keyword SortedSetType}
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
  [match-val :- LexicalValType
   lex-set :- SortedSetType]
  (let [[smaller-set found-val larger-set] (avl/split-key match-val lex-set)
        result (if (nil? found-val)
                 (let [[matches-seq larger-seq] (split-with #(prefix-match? match-val %) larger-set)]
                   {:smaller smaller-set
                    :matches (->sorted-set-avl matches-seq)
                    :larger  (->sorted-set-avl larger-seq)})
                 {:smaller smaller-set
                  :matches (avl/sorted-set found-val)
                  :larger  larger-set})]
    ;(s/validate SortedSetType (grab :smaller result))
    ;(s/validate SortedSetType (grab :matches result))
    ;(s/validate SortedSetType (grab :larger result))
    result))

; #todo add-entry & remove-entry instead of conj/disj  ???
(s/defn add-entry
  "Add an entry to the index, returning the modified index"
  [index :- SortedSetType
   entry :- tsk/Vec ]
  (conj index entry))

(s/defn remove-entry
  "Remove an entry to the index, returning the modified index. Throws if entry not found in index."
  [index :- SortedSetType
   entry :- tsk/Vec ]
  (when-not (contains? index entry)
    (throw (ex-info "entry not found in index" (vals->map entry))) )
  (disj index entry))






