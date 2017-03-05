;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.x
  "Experimental new code"
  (:require
    [clojure.core.async     :as ca :refer [go go-loop chan thread]]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.async :as ta]
  ))
(t/refer-tupelo)

(s/set-fn-validation! true) ; enforce fn schemas

(def defgen-buffer-size
  "Specifies the output channel default buffer size for `defgen` forms"
  32)

(defmacro yield
  "Special form used to return lazy result values within generator functions (see `defgen`)."
  [value]
  `(ta/put-go! ~'lazy-buffer ~value))

(defmacro defgen [& forms]
  "Creates a 'generator function' that returns a lazy seq of results via the `(yield ...)`
  special form, similar to Python."
  (let [
        [head-forms tail-forms] (split-with #(not (vector? %1)) forms)
        arglist    (first tail-forms)
        body-forms (rest tail-forms)
        ]
    `(defn
       ~@head-forms
       ~arglist
       (let [~'lazy-buffer      (chan defgen-buffer-size)
             lazy-reader-fn#    (fn lazy-reader-fn# []
                                  (let [curr-item# (ta/take-now! ~'lazy-buffer)]
                                    (when (t/not-nil? curr-item#)
                                      (t/lazy-cons curr-item# (lazy-reader-fn#)))))]
         (go
           ~@body-forms
           (ca/close! ~'lazy-buffer))
         (lazy-reader-fn#)))))

