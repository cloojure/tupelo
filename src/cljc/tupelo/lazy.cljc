;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.lazy
  "Tupelo - Making Clojure even sweeter"
  (:require [clojure.core :as cc]
            [schema.core :as s]
            [tupelo.schema :as tsk]))

(defn join
  "Lazily concatenates a sequence-of-sequences into a flat sequence."
  [sequences]
  (lazy-seq
    (when-let [seq-of-seqs (seq sequences)]
      (concat (cc/first seq-of-seqs) (join (cc/rest seq-of-seqs))))))
