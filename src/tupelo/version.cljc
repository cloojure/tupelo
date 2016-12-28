;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.version
  "Java version testing functions & macros"
  (:require
    [clojure.string :as str]
    [schema.core :as sk]
    [tupelo.core :as t]
    [tupelo.misc :as tm]
    [schema.core :as s]))

(defn java-version-matches?
  "Returns true if Java version exactly matches supplied string."
  [version-str]
  {:pre [ (string? version-str) ] }
  (let [system-ver-chars  (vec (System/getProperty "java.version"))
        tgt-ver-chars     (vec version-str) ]
    (t/starts-with? system-ver-chars tgt-ver-chars)))

(defn java-version-min?
  "Returns true if Java version is at least as great as supplied string.
  Sort is by lexicographic (alphabetic) order."
  [version-str]
  {:pre [ (string? version-str) ] }
  (neg? (compare version-str (System/getProperty "java.version"))))

(defn is-java-1-7? []  (java-version-matches? "1.7"))
(defn is-java-1-8? []  (java-version-matches? "1.8"))

(defn is-java-1-7-plus? [] (java-version-min? "1.7"))
(defn is-java-1-8-plus? [] (java-version-min? "1.8"))

(defmacro java-1-7-plus-or-throw
  [& forms]
  (if (is-java-1-7-plus?)
    `(do ~@forms)
    `(do (throw (RuntimeException. "Unimplemented prior to Java 1.7: ")))))

(defmacro java-1-8-plus-or-throw
  [& forms]
  (if (is-java-1-8-plus?)
    `(do ~@forms)
    `(do (throw (RuntimeException. "Unimplemented prior to Java 1.8: ")))))

; #todo need min-java-1-8  ???

;-----------------------------------------------------------------------------

(defn is-clojure-1-7-plus? []
  (tm/increasing-or-equal? [1 7] (t/select-values *clojure-version* [:major :minor ])))

(defn is-clojure-1-8-plus? []
  (tm/increasing-or-equal? [1 8] (t/select-values *clojure-version* [:major :minor ])))

(defn is-clojure-1-9-plus? []
  (tm/increasing-or-equal? [1 9] (t/select-values *clojure-version* [:major :minor ])))

; #todo add is-clojure-1-8-max?
; #todo need clojure-1-8-plus-or-throw  ??

  (defmacro min-clojure-1-8
  [& forms]
  (if (is-clojure-1-8-plus?)
    `(do ~@forms)))
