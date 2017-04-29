;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.misc
  "Miscellaneous functions."
  (:require 
    [clojure.core.async :refer [go go-loop chan buffer close! thread alts! alts!! timeout]]
    [clojure.data.xml :as xml]
    [clojure.java.shell :as shell]
    [clojure.string :as str]
    [clojure.walk :refer [postwalk]]
    [clj-uuid :as uuid]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.schema :as tsk]
    [tupelo.string :as ts]
    [tupelo.types :as tt]
  )
  (:import
    [java.nio ByteBuffer]
    [java.security MessageDigest]
    [java.util UUID ]
  ))
(t/refer-tupelo)

;  #todo Make clojure versions of all pcapng stuff
;
; def split_float( fval ):
; """Splits a float into integer and fractional parts."""
; frac, whole = math.modf( fval )
; micros = int( round( frac * 1000000 ))
; return int(whole), micros
;
; def curr_utc_timetuple():
; """Returns the current UTC time as a (secs, usecs) tuple."""
; global test_ctx
; if test_ctx['enable']:
; utc_secs = test_ctx['utc_time']
; else:
; utc_secs = time.time()
; secs, usecs = split_float( utc_secs )
; return secs, usecs
;
; def curr_utc_secs():
; """Returns the current UTC time in integer seconds."""
; secs, usecs = curr_utc_timetuple()
; return secs
;
; def curr_utc_secs_hexstr()
; """Returns the current UTC time in integer seconds."""
; return int32_to_hexstr(curr_utc_secs())


(def ^:dynamic *os-shell* "/bin/bash")  ; could also use /bin/zsh, etc

(defn ^{:deprecated "0.9.15"} collapse-whitespace [& args] (apply ts/collapse-whitespace args))
(defn ^{:deprecated "0.9.15"} equals-ignore-spacing [& args] (apply ts/equals-ignore-spacing args))
(defn ^{:deprecated "0.9.15"} double-quotes->single-quotes [& args] (apply ts/double-quotes->single-quotes args))
(defn ^{:deprecated "0.9.15"} single-quotes->double-quotes [& args] (apply ts/single-quotes->double-quotes args))
(defn ^{:deprecated "0.9.15"} normalize-str [& args] (apply ts/normalize-str args))
(defn ^{:deprecated "0.9.15"} str->kw [& args] (apply ts/str->kw-normalized args))
(defn ^{:deprecated "0.9.15"} char-seq [& args] (apply t/char-seq args))
(defn ^{:deprecated "0.9.15"} seq->str [& args] (apply t/seq->str args))
(def  ^{:deprecated "0.9.15"} printable-chars  ts/chars-text )

; #todo *warn-on-lazy* -> print warning on first usage of each lazy function:
; #todo     for, map/indexed, flatten, line-seq, concat, distinct, drop/last/while, filter/remove/keep,
; #todo     partition*, re-seq, take/nth/while


; #todo add functions:
;   fibonacci-list(n)                 - 1ist n  fibo's
;   fibonacci-list-bounded(maxVal)    - list of fibo's <= maxVal
;   prime-list(n)                     - 1ist n  fibo's
;   prime-list-bounded(maxVal)        - list of fibo's <= maxVal

; #todo
; (defn instaparse-failure? [result] (= (class result) instaparse.gll.Failure))

(defn take-dist
  "Returns a sequence of n items from a collection, distributed
   evenly between first & last elements, which are always included."
  ; #todo write tests, incl degenerate cases of N=0,1,2, etc
  [n coll]
  {:pre [(pos? n)] }
  (let [coll (vec coll)]
    (if (= n 1)
      [ (first coll) ]
      (let [interval (Math/round (double (/ (count coll) (- n 1))))
            result   (flatten [(take (- n 1) (take-nth interval coll))
                               (last coll)])]
        result))))

(defn shell-cmd
  "Run a command represented as a string in an OS shell (default=/bin/bash).
  Example: 'ls -ldF *'  "
  [cmd-str]
  (let [result (shell/sh *os-shell* "-c" cmd-str)]
    (if (= 0 (t/safe-> :exit result))
      result
      (throw (RuntimeException. 
               (str "shell-cmd: clojure.java.shell/sh failed. \n" 
                    "cmd-str:"     cmd-str        "\n"
                    "exit status:" (:exit result) "\n"
                    "stderr:"      (:err  result) "\n"
                    "result:"      (:out  result) "\n" 
              ))))))

(defn get-os []
  (let [os-name (System/getProperty "os.name") ]
    (condp re-find (str/lower-case os-name) ; required to match os.name="Windows 8.1"
      #"windows"  :windows
      #"linux"    :linux
      #"mac"      :mac
      (throw (RuntimeException. (str "get-os: Unknown operating system found: " os-name ))))))

(comment  "stuff to make a generic run-shell-cmd"

  (defn format-shell-cmd-vec [cmd-str]
    (when-not (string? cmd-str)
      (throw (IllegalArgumentException. (str "format-shell-cmd: cmd-str must be a string; received=" cmd-str))))
    (if (is-windows?)
        ["cmd.exe" "/c" cmd-str :dir "c:\\users"] ; windows
        ["bash"    "-c" cmd-str                 ] ; linux
    ))

  (defn run-shell-cmd [cmd-str]
    (when-not (string? cmd-str)
      (throw (IllegalArgumentException. (str "format-shell-cmd: cmd-str must be a string; received=" cmd-str))))
    (apply shell/sh (util/format-shell-cmd-vec cmd-str)))

)


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

    (ns xxx.core 
      (:require [tupelo.misc :as tm]))
    (tm/dots-config! {:decimation 10} )
    (tm/with-dots
      (doseq [ii (range 2345)]
        (tm/dot)
        (Thread/sleep 5)))
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

; #todo need tests & docs. Use for datomic Entity?
(defn unlazy
  [coll]
  (let [unlazy-item (fn [item]
                      (cond
                        (sequential? item) (vec item)
                        (map? item) (into {} item)
                        :else item))
        result    (postwalk unlazy-item coll)
  ]
    result ))

; -----------------------------------------------------------------------------
; #todo maybe move to tupelo.bytes ns

(defn bytes->hex-str
  "Converts a byte array to a hex string, where each byte becomes 2 hex digits."
  [bytes]
  (assert (tt/byte-array? bytes))
  (str/join (map #(format "%02x" %) bytes)))

(s/defn long->bytes
  "Converts a Long into an array of bytes (big-endian)."
  [arg]
  (assert (tt/long? arg))
  (it-> (ByteBuffer/allocate Long/BYTES)
    (.putLong it arg)
    (.array it)))

(def uuid->str
  "Returns the SHA-1 hex string for a UUID"
  (let [sha-1-instance (MessageDigest/getInstance "SHA")]
    (s/fn uuid->str :- s/Str
      [uuid :- java.util.UUID]
      (let [bytes-big    (long->bytes (.getMostSignificantBits  ^UUID uuid))
            bytes-little (long->bytes (.getLeastSignificantBits ^UUID uuid))]
        (.reset sha-1-instance)
        (.update sha-1-instance bytes-big)
        (.update sha-1-instance bytes-little)
        (let [bytes (.digest sha-1-instance)]
          (bytes->hex-str bytes))))))

(def str->sha
  "Returns the SHA-1 hex string for a string"
  (let [sha-1-instance (MessageDigest/getInstance "SHA")]
    (s/fn str->sha :- s/Str
      [str-arg :- s/Str]
      (.reset sha-1-instance)
      (doseq [ch str-arg]
        (.update sha-1-instance (byte ch)))
      (bytes->hex-str (.digest sha-1-instance)))))

(s/defn sha-uuid :- s/Str
  "Returns a string that is the SHA-1 hash of the `uuid/v1`."
  []
  (uuid->str (uuid/v1)))


;-----------------------------------------------------------------------------
; #todo -> tupelo.vector
; #todo README & more tests

(s/defn find-pattern :- [s/Int]
  "Searches for pattern-vec within data-vec, returning a lazy seq of indexes into data-vec."
  [pattern-vec :- tsk/List
   data-vec :- tsk/List]
  (let [parts         (partition (count pattern-vec) 1 data-vec)
        idxs          (keep-indexed
                        (fn [idx candidate]
                          (when (= candidate pattern-vec)
                            idx))
                        parts)]
      idxs))

;; Assuming require [clojure.tools.logging :as log]
(defn log-uncaught-exceptions []
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException [_ thread ex]
        (println ex "Uncaught exception on" (.getName thread)))))) ; or (log/error ...)


