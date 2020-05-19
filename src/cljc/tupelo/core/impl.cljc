;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.core.impl
  "Tupelo - Making Clojure even sweeter"
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [schema.core :as s]))

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
        [catch-op ex-symbol & catch-body :as catch-form] (last body)]
    (assert (= catch-op 'catch))
    (assert (symbol? ex-symbol))
    `(if-cljs
       (try ~@try-body (catch js/Object ~ex-symbol ~@catch-body))
       (try ~@try-body (catch Throwable ~ex-symbol ~@catch-body)))))

(defn type-name-str
  "Returns the type/class name of a value as a string.  Works for both CLJ and CLJS."
  [arg]
  #?(:clj  (.getName (clojure.core/class arg))
     :cljs (cljs.core/type->str (cljs.core/type arg))))

(defn native-array?
  "Returns true iff arg is a native Java or JavaScript array."
  [arg]
  (boolean
    #?(:clj  (.isArray (class arg))
       :cljs (cljs.core/array? arg))))

