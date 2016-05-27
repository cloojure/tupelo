;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.misc
  "Miscellaneous functions."
  (:use tupelo.core)
  (:require [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.set :as set]
            [schema.core :as s]
            [tupelo.schema :as ts]
            [tupelo.async :as tas]
            [clojure.core.async :refer [ go go-loop chan buffer close! thread alts! alts!! timeout ]]
  ))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs


(def ^:dynamic *os-shell* "/bin/bash")  ; could also use /bin/zsh, etc

(defn collapse-whitespace
 "Replaces all consecutive runs of whitespace characters (including newlines) with a single space.
  Also removes any leading or trailing whitespace."
  [it]
  (-> it
      str/trim
      (str/replace #"\s+" " ")))

(defn normalize-str
 "Returns a 'normalized' version of str-in, stripped of leading/trailing
  blanks, and with all non-alphanumeric chars converted to hyphens."
  [str-in]
  (-> str-in
      str/trim
      (str/replace #"[^a-zA-Z0-9]" "-")))
  ; #todo replace with other lib

(defn str->kw
 "Returns a keyword constructed from the normalized str-in"
  [str-in]
  (keyword (normalize-str str-in)))
  ; #todo replace with other lib

(defn take-dist
 "Returns a sequence of n items from a collection, distributed
  evenly between first & last elements, which are always included."
  ; #todo write tests, incl degenerate cases of N=0,1,2, etc
  [n coll]
  {:pre [(pos? n)] }
  (if (= n 1)
    (first coll)
    (let [interval    (Math/round (double (/ (count coll) (- n 1))))
          result      (flatten [ (take (- n 1) (take-nth interval coll))
                                 (last coll) ] ) ] 
      result )))

(defn char-seq
  "Given two characters (or numerical equivalents), returns a seq of characters
  (inclusive) from the first to the second.  Characters must be in ascending order."
  [start-char stop-char]
  {:pre [ (char start-char) (char stop-char) ] }
    ; These "dummy" casts are to ensure that any input integer values are within the valid
    ; range for Unicode characters
  (let [start-val   (int start-char)
        stop-val    (int stop-char)]
    (when-not (<= start-val stop-val)
      (throw (IllegalArgumentException. 
        (str "char-seq: start-char must come before stop-char."
        "  start-val=" start-val "  stop-val=" stop-val))))
    (mapv char (range start-val (inc stop-val)))))

(defn seq->str
  "Convert a seq into a string (using pr) with a space preceding each value"
  [seq-in]
  (with-out-str
    (doseq [it (seq seq-in)]
      (print \space)
      (pr it))))

(defn shell-cmd 
  "Run a command represented as a string in an OS shell (default=/bin/bash).
  Example: 'ls -ldF *'  "
  [cmd-str]
  (let [result (shell/sh *os-shell* "-c" cmd-str)]
    (if (= 0 (safe-> :exit result))
      result
      (throw (RuntimeException. 
               (str "shell-cmd: clojure.java.shell/sh failed. \n" 
                    "cmd-str:"     cmd-str        "\n"
                    "exit status:" (:exit result) "\n"
                    "stderr:"      (:err  result) "\n"
                    "result:"      (:out  result) "\n" 
              ))))))

(def printable-chars
  "A seq of 1-char strings of all printable characters from space (32) to tilde (126)"
  (mapv str (char-seq \space \~)))

; #todo document in README
(def ^:no-doc dot-counter   (atom 0))
(def ^:no-doc dots-ctx      (atom { :dots-per-row   100
                                    :decimation       1 } ))
(defn dots-config!  ; #todo need docstring
  [ctx]  ; #todo check pos integers
  (swap! dots-ctx conj ctx))

(defn dot 
 "Prints a single dot (flushed) to the console, keeping a running count of dots printed.  Wraps to a
  newline when 100 dots have been printed. Displays the running dot count at the beginning of each line.
  Usage:

    (tm/dots-config! {:decimation 10} )
    (tm/with-dots
      (doseq [ii (range 200)]
        (tm/dot)
        (Thread/sleep 10))))
  "
  [] 
  (let [
        decimation        (grab :decimation @dots-ctx)
        counts-per-row    (* decimation (grab :dots-per-row @dots-ctx))
        old-count         @dot-counter
        new-count         (swap! dot-counter inc) ]
    (when (zero? (rem old-count counts-per-row))
      (print (format "%10d " old-count)))
    (when (zero? (rem old-count decimation))
      (print \.))
    (flush)
    (when (zero? (rem new-count counts-per-row))
      (newline))
    (flush)))

(defmacro with-dots
  "Increments indentation level of all spy, spyx, or spyxx expressions within the body."
  [& body]
  `(do
     (reset! dot-counter 0)
     (let [result#  (do ~@body) ]
      (newline)
      (println (format "%10d total" @dot-counter))
      result#)))

(s/defn factorial :- s/Int
  "Computes the factorial of N"
  [n :- s/Int]
  (when (neg? n)
    (throw (IllegalArgumentException.
             (str "factorial: N must be a non-negative integer=" n))))
  (if (zero? n)
    1
    (apply * (thru 1 n))))

