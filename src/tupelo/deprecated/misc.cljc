;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.deprecated.misc
  "Miscellaneous functions."
  #?@(:clj [
  (:require
    [clojure.walk :refer [postwalk]]
    [tupelo.core :as t]
    [tupelo.impl :as i]
    [tupelo.char :as char]
    [tupelo.string :as ts]
  (:import
    [java.nio ByteBuffer]
    [java.security MessageDigest]
    [java.util UUID ] )
            ]) )

#?(:clj (do
(defn ^{:deprecated "0.9.15"} collapse-whitespace [& args] (apply ts/collapse-whitespace args))
(defn ^{:deprecated "0.9.15"} equals-ignore-spacing [& args] (apply ts/equals-ignore-spacing? args))
(defn ^{:deprecated "0.9.15"} double-quotes->single-quotes [& args] (apply ts/double-quotes->single-quotes args))
(defn ^{:deprecated "0.9.15"} single-quotes->double-quotes [& args] (apply ts/single-quotes->double-quotes args))
(defn ^{:deprecated "0.9.15"} normalize-str [& args] (apply ts/normalize-str args))
(defn ^{:deprecated "0.9.15"} str->kw [& args] (apply ts/str->kw-normalized args))
(defn ^{:deprecated "0.9.15"} char-seq [& args] (apply i/chars-thru args))
(defn ^{:deprecated "0.9.15"} seq->str [& args] (apply t/seq->str args))
(def  ^{:deprecated "0.9.15"} printable-chars  char/text )

))
