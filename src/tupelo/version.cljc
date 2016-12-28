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
  ))

(defn java-version-matches?
  [version-str]
  {:pre [ (string? version-str) ] }
  (let [idx-val (str/index-of (System/getProperty "java.version") version-str) ]
    (and (t/not-nil? idx-val) (zero? idx-val))))

(defn java-version-min?
  [version-str]
  {:pre [ (string? version-str) ] }
  (neg? (compare version-str (System/getProperty "java.version"))))

(defn is-java-1-7? []  (java-version-matches? "1.7"))
(defn is-java-1-8? []  (java-version-matches? "1.8"))

(defn is-java-1-7-plus? [] (java-version-min? "1.7"))
(defn is-java-1-8-plus? [] (java-version-min? "1.8"))

(defmacro min-java-1-7
  [& forms]
  (if (is-java-1-7-plus?)
    `(do ~@forms)
    `(do (throw (RuntimeException. "Unimplemented prior to Java 1.7: ")))))

(defmacro min-java-1-8
  [& forms]
  (if (is-java-1-8-plus?)
    `(do ~@forms)
    `(do (throw (RuntimeException. "Unimplemented prior to Java 1.8: ")))))

