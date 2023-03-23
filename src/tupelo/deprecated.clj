(ns tupelo.deprecated
  (:use tupelo.core)
  (:import [java.io BufferedReader StringReader]))

; duplicate of str/split-lines
(defn ^:deprecated str->lines ; duplicate of str/split-lines *****
  "Returns a lazy seq of lines from a string"
  [string-arg]
  (line-seq (BufferedReader. (StringReader. string-arg))))

; As of Clojure 1.9.0-alpha5, seqable? is native to clojure
(when-not-clojure-1-9-plus

  (defn ^{:deprecated "1.9.0-alpha5"} seqable? ; from clojure.contrib.core/seqable
    "Returns true if (seq x) will succeed, false otherwise."
    [x]
    (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x .getClass .isArray)
      (string? x)
      (instance? java.util.Map x)))

)
