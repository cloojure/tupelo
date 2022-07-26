;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.string.safe
  (:require
    [clojure.walk :as walk]
    [tupelo.core :as t]
    [tupelo.string :as str]
    ))

(defn walk-normalize ; => tupelo.string.safe/walk-normalize
  "Recursively walks a data structure. For all string values, perform
  `(str/lower-case (str/whitespace-collapse arg))`, else noop."
  [data]
  (walk/postwalk
    (fn [arg]
      (t/cond-it-> arg
        (string? it) (str/lower-case (str/whitespace-collapse it))))
    data))

(defn walk-whitespace-collapse
  "Recursively walks a data structure. For all string values, perform
  `(str/whitespace-collapse ...)`, else noop."
  [data]
  (walk/postwalk
    (fn [arg] (t/cond-it-> arg
                (string? it) (str/whitespace-collapse it)))
    data))

