;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.quote
  ; We use the self-require trick to force separate compilation stages for macros
  ; See "ClojureScript Macro Tower & Loop" by Mike Fikes (2015-12-18)
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs (:require-macros [tupelo.quote]))
  (:require
    [clojure.walk :as walk]
    [tupelo.core :as t :refer [glue grab thru kw->str validate it-> spyx spyxx vals->map
                               xfirst xsecond forv ]]
    ))

(defn insert-form? ; #todo => `run` or `live` or `unq` or `ins` or `insert`???
  [arg]
  (and (list? arg)
    (= (quote insert) (first arg))))

(defn splice-form? ; #todo => `splat` or `splice` or `unq*` ???
  [arg]
  (and (list? arg)
    (= (quote splice) (first arg))))

(defn tmpl-impl
  [form]
  (walk/prewalk
    (fn [item]
      (cond
        (insert-form? item) (eval (xsecond item))
        (sequential? item) (let [unquoted-vec (apply glue
                                                (forv [it item]
                                                  (if (splice-form? it)
                                                    (eval (xsecond it))
                                                    [it])))
                                 final-result (if (list? item)
                                                (t/->list unquoted-vec)
                                                unquoted-vec)]
                             final-result)
        :else item))
    form))

(defmacro tmpl ; #todo maybe => `qtmpl` or `quot` or `qt` or `quoted` or `template`
  [form]
  (tmpl-impl form))


