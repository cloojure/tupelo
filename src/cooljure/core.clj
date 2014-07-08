;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure"
      :author "Alan Thompson"}
  cooljure.core)

(defn truthy?
  "Returns true if arg is logical true (neither nil nor false);
  otherwise returns false."
  [arg]
  (if arg true false) )

(defn falsey?
  "Returns true if arg is logical false (either nil or false);
  otherwise returns false. Equivalent to (not (truthy? arg))."
  [arg]
  (if arg false true) )

(defn any?
  "Returns true if (pred x) is logical true for any x in collection; otherwise returns
  false.  Like clojure.core/some, but returns only true or false."
  [pred coll]
  (truthy? (some pred coll)) )

(defn not-empty?
  "Returns true if collection contains any items; otherwise returns false
  Equivalent to (not (empty? coll))."
  [coll]
  (truthy? (seq coll)) )

(defn conjv 
  "Appends items to collection, always returning the result as a vector."
  ; From Stuart Sierra post 2014-2-10
  ( [coll x]
      (conj (vec coll) x) )
  ( [coll x & xs]
      (apply conj (vec coll) x xs) ))

(defn keyvals 
  "Returns a map's keys & values as a vector, suitable for reconstructing the map via
  (apply hashmap (keyvals m))."
  [m]
  {:pre [ (map? m) ] }
  (vec (flatten (seq m))) )

