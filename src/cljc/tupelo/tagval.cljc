;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.tagval
  "Tupelo - Making Clojure even sweeter"
  (:refer-clojure :exclude [key val])
  (:require
    [clojure.core :as clj]
    [schema.core :as s]
    [tupelo.core :as t :refer [spy spyx spyx-pretty spyxx  ]]
    [tupelo.schema :as tsk]
    ))

(s/defn tagval? :- s/Bool
  "Returns true if arg is a TagVal "
  [arg :- s/Any]
  (and (t/xmap? arg)
    (let [entries (seq arg)]
      (and (= 1 (count entries))
        (keyword? (clj/key (t/xfirst entries)))))))

(s/defn new :- tsk/TagVal
  "Constructs a new TagVal as the literal `{tag value}` "
  [tag :- s/Keyword
   value :- s/Any]
  {tag value})

(s/defn tag :- s/Keyword ; #todo => <tag ???
  "Returns the keyword tag of a TagVal"
  [arg :- tsk/TagVal]
  (assert (tagval? arg))
  (clj/key (first arg)))

(s/defn val :- s/Any ; #todo => <val ???
  "Returns value of a TagVal"
  [arg :- tsk/TagVal]
  (assert (tagval? arg))
  (clj/val (first arg)))

(s/defn untagged :- s/Any
  "If given a TagVal, returns the value; else noop."
  [arg :- s/Any]
  (t/cond-it-> arg
    (tagval? it) (val it)))

(comment ; old Record version
  (defprotocol IVal (<val [this]))
  (defprotocol ITag (<tag [this]))
  (defprotocol ITagMap (->tagmap [this]))

  (defrecord TagVal
    [tag val]
    ITag (tag [this] tag)
    IVal (val [this] val))

  ; ***** fails strangely under lein-test-refresh after 1st file save if define this using Plumatic schema *****
  (defn tagval? ; :- s/Bool
    [arg] (= TagVal (type arg)))

  #?(:clj
     (defmethod print-method TagVal
       [tv ^java.io.Writer writer]
       (.write writer
         (format "<%s %s>" (tag tv) (val tv)))))
  #?(:cljs
     ; https://stackoverflow.com/questions/42916447/adding-a-custom-print-method-for-clojurescript
     (extend-protocol IPrintWithWriter TagVal
       (-pr-writer [arg writer -opts-]
         (write-all writer "<TagVal" (val arg) ">"))))

  (s/defn tagval-map? :- s/Bool
    "Returns true iff arg is a map that looks like a TagVal record:  {:tag :something :val 42}"
    [item]
    (and (t/xmap? item)
      (= #{:tag :val} (set (keys item)))))

  (s/defn tagval-with?
    [tag :- s/Keyword
     arg :- TagVal]
    (= tag (tag arg)))

  (defn tagged-param?
    [arg]
    (and (= TagVal (type arg))
      (tagval-with? :param arg)))

  (s/defn untagged :- s/Any
    [arg :- s/Any]
    (t/cond-it-> arg
      (satisfies? IVal it) (val it)))

  )
