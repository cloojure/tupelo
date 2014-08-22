cooljure
========

## Cool stuff you wish was in Clojure

Leiningen coordinates:

[![Clojars Project]
(http://clojars.org/cooljure/latest-version.svg)]
(http://clojars.org/cooljure)

## Overview

Have you ever wanted to do something simple but clojure.core doesn't support it? Or, maybe you are wishing for an enhanced version of a standard function. The goal of Cooljure is to add support for these convenience features, so that you have a simple way of using either the enhanced version or the original version.

The goal in using `cooljure.core` is that you can just plop it in any namespace without having to worry about any conflicts with core functionality. So, both the core functions and the added/enhanced functions are both available for use at all times. As such, we use normally use `:refer :all` for cooljure.core in our namespaces:

```clojure
(ns my.proj
  (:require ...
            [cooljure.core    :refer :all]
            [cooljure.csv     :as cool-csv]
            [cooljure.parse   :as cool-parse]
            ... ))
````

## cooljure.core - Basic functions

### The truth is not ambiguous

Clojure marries the worlds of Java and Lisp. Unfortunately, these two worlds have different ideas of truth, so Clojure accepts both `false` and `nil` as _false_. Sometimes you want to coerce logical values into literal _true_ or _false_ values, so we provide a simple way to do that:

```
(truthy? arg)
  Returns true if arg is logical true (neither nil nor false);
  otherwise returns false.

(falsey? arg)
  Returns true if arg is logical false (either nil or false);
  otherwise returns false. Equivalent to (not (truthy? arg)).
```

### Convenience testing seq's

These functions aren't in clojure.core, but people keep writing into the mailing list wondering where they are. Well, now they are available:

```
(any? pred coll)
  For any predicate & collection, returns true if (pred x) is 
  logical true for any x in coll; otherwise returns false. Like
  clojure.core/some, but returns only true or false.

(not-empty? coll)
  For any collection, returns true if coll contains any items; 
  otherwise returns false. Equivalent to (not (empty? coll)).
```
### Focus on vectors

Clojure's seq abstraction (and lazy seq's) is very useful, but sometimes you just want everything to stay in a nice, eager, random-access vector. Here is an easy way to build up a vector result:

```
(conjv coll x)
(conjv coll x & xs)
  For any collection coll and list of values x, appends the x's to 
  collection, always returning the result as a vector.
```
```clojure
=> (conjv '(1 2) 3)
[1 2 3]
```

### Map manipulation

Sometimes you want to extract the keys & values from a map for manipulation or extension before building up another map (especially useful manipulating default function args). Here is very handy function for that:

```clojure
(keyvals m)
  For any map m, returns the keys & values of m as a vector, 
  suitable for reconstructing via (apply hashmap (keyvals m)).
```
```clojure
=> (keyvals {:a 1 :b 2})
[:b 2 :a 1]
=> (apply hash-map (keyvals {:a 1 :b 2}))
{:b 2, :a 1}
```

### Expression debugging

Ever been debugging some new code and had trouble inserting printing out intermediate values?  For example:

```clojure
(-> 1
    (inc)
    (* 2))
4
```
Suppose you want to disply the value after the (inc) function. You can't just insert a (println) because the return value of `nil` will break the pipeline structure. Instead, just use `spy-first`:

```clojure
(-> 1
    (inc)
    (spy-first "after inc")
    (* 2))
after inc => 2
4
```
This tool is named `spy-first` since it is intended to be placed into a "thread-first" pipeline (e.g. using `->` or similar). A companion named `spy-last` is available for pipelines using "thread-last" (e.g. `->>`):
```clojure
(->> 1
    (inc)
    (spy-last "after inc")
    (* 2))
after inc => 2
4
```

Sometimes you may prefer to print out the expression itself, or nothing at all. Then, just use `spy-expr` or `spy-val`:
```clojure
(as-> 1 x
      (spy-expr (inc x))
      (* 2 x))
(inc x) => 2
4

(->> 1
     (inc)
     (spy-val)
     (* 2))
2
4
```
To be precise, the function signatures are:
```clojure
(spy-first expr msg )
(spy-last  msg  expr)
(spy-expr  expr)
(spy-val   expr)
```

## cooljure.csv - Functions for using CSV (Comma Separate Value) files

TEMP TODO:  see source code [cooljure.csv](http://github.com/cloojure/cooljure/blob/master/src/cooljure/csv.clj)

## coojure.parse - Functions to ease parsing

TEMP TODO:  see source code [cooljure.parse](http://github.com/cloojure/cooljure/blob/master/src/cooljure/parse.clj)
