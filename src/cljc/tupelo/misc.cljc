;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.misc
  "Miscellaneous functions."
  ; We use the self-require trick to force separate compilation stages for macros
  ; See "ClojureScript Macro Tower & Loop" by Mike Fikes (2015-12-18)
  ;   https://code.thheller.com/blog/shadow-cljs/2019/10/12/clojurescript-macros.html
  ;   http://blog.fikesfarm.com/posts/2015-12-18-clojurescript-macro-tower-and-loop.html
  #?(:cljs
     (:require-macros
       [tupelo.misc :refer [with-dots]]))
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [schema.core :as s]
            [tupelo.core :as t :refer [glue grab thru kw->str validate it-> spyx spyxx vals->map]]
            [tupelo.schema :as tsk]
            [tupelo.string :as ts]
            #?(:cljs [goog.crypt :as crypt])
            #?(:cljs [goog.crypt.Sha1])
            )
  #?(:clj (:require
            [clj-uuid :as clj-uuid]
            [clojure.java.shell :as shell]))
  #?(:clj (:import
            [java.lang Byte Integer]
            [java.nio ByteBuffer]
            [java.nio.file Paths]
            [java.security MessageDigest]
            [java.util UUID]))
  )

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

(s/defn boolean->binary :- s/Int ; #todo => misc
  "Convert true => 1, false => 0"
  [arg :- s/Bool] (if arg 1 0))

;---------------------------------------------------------------------------------------------------
(defn walk-map->sorted
  "Recursively walks a data structure, converting maps (but not records!) into (plain) sorted maps."
  [data]
  (walk/postwalk (fn [item]
                   (t/cond-it-> item
                     (t/xmap? it) (glue (sorted-map) it)))
    data))

(defn walk-map->sorted-generic
  "Recursively walks a data structure, converting maps (but not records!) into generic sorted maps.
  A generic sort allows keys of different categories such as keyword, string, int, nil, etc."
  [data]
  (walk/postwalk (fn [item]
                   (t/cond-it-> item
                     (t/xmap? it) (glue (t/sorted-map-generic) it)))
    data))

(defn walk-rec->map
  [data]
  (walk/postwalk (fn [item]
                   (t/cond-it-> item
                     (record? it) (into {} it)))
    data))

; -----------------------------------------------------------------------------
; #todo maybe move to tupelo.bytes ns
(s/defn byte-unsigned->signed
  "Converts an unsigned int value [0..255] into a signed byte [-128..127]."
  [unsigned-int :- s/Int]
  (t/when-clojure-1-9-plus
    (when-not (int? unsigned-int)
      (throw (ex-info "byte-unsigned->signed: value must be an int" (t/vals->map unsigned-int)))))
  (when-not (<= 0 unsigned-int 255)
    (throw (ex-info "byte-unsigned->signed: value out of range" (t/vals->map unsigned-int))))
  (if (< unsigned-int 128)
    unsigned-int
    (- unsigned-int 256)))

(s/defn byte-signed->unsigned
  "Converts a signed byte [-128..127] into an unsigned byte [0..255]."
  [signed-byte :- s/Int]
  (t/when-clojure-1-9-plus
    (when-not (int? signed-byte)
      (throw (ex-info "byte-signed->unsigned: value must be an int" (t/vals->map signed-byte)))))
  (when-not (<= -128 signed-byte 127)
    (throw (ex-info "byte-signed->unsigned: value out of range" (t/vals->map signed-byte))))
  (if (neg? signed-byte)
    (+ signed-byte 256)
    signed-byte))

(s/defn bytes-unsigned->signed
  "Converts a vector of unsigned byte values [0..255] into one of signed byte values [-128..127]"
  [byte-vals]
  (mapv byte-unsigned->signed byte-vals ))

(s/defn bytes-signed->unsigned
  "Converts a vector of signed byte values [-128..127] into one of unsigned byte values [0..255] "
  [byte-vals]
  (mapv byte-signed->unsigned byte-vals ))

(def hex-chars [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f])
(def int->hex (zipmap (range 16) hex-chars))
(def hex->int (zipmap hex-chars (range 16)))
(s/defn byte-unsigned->hex :- s/Str
  "Converts a sequence of unsigned bytes [0..255] to a hex string, where each byte becomes 2 hex digits."
  [unsigned-byte  :- s/Int]
  (t/when-clojure-1-9-plus
    (when-not (int? unsigned-byte)
      (throw (ex-info "unsigned-byte->hex value must be an int" (t/vals->map unsigned-byte)))))
  (when-not (<= 0 unsigned-byte 255)
    (throw (ex-info "unsigned-byte->hex value out of range" (t/vals->map unsigned-byte))))
  (let [high-bits-val (quot unsigned-byte 16)
        low-bits-val  (mod unsigned-byte 16)
        high-char     (get hex-chars high-bits-val)
        low-char      (get hex-chars low-bits-val)]
    (str high-char low-char))) ; (crypt/byteArrayToHex byte-arr) ; NOTE: requires unsigned vals [0..255]

(s/defn byte-signed->hex :- s/Str
  "Converts a sequence of unsigned bytes [0..255] to a hex string, where each byte becomes 2 hex digits."
  [signed-byte]
  (-> signed-byte byte-signed->unsigned byte-unsigned->hex))

(s/defn hex-str->unsigned-bytes
  "Converts a hex string to a vector of unsigned bytes"
  [hex-str :- s/Str]
  (when-not (even? (count hex-str))
    (throw (ex-info "hex-str must have a whole number of bytes (even length)" {:hex-str hex-str :len (count hex-str)})))
  (let [hex-pairs      (partition 2 (t/str->chars hex-str))
        bytes-unsigned (t/forv [hex-pair hex-pairs]
                         (let [char-high (first hex-pair)
                               char-low  (second hex-pair)
                               val-high  (grab char-high hex->int)
                               val-low   (grab char-low hex->int)
                               byte-val  (+ val-low (* 16 val-high)) ]
                           byte-val)) ]
    bytes-unsigned) )

(s/defn hex-str->signed-bytes
  "Converts a hex string to a vector of unsigned bytes"
  [hex-str :- s/Str]
  (bytes-unsigned->signed
    (hex-str->unsigned-bytes hex-str)))

(s/defn bytes-unsigned->hex-str :- s/Str
  "Converts a sequence of unsigned bytes [0..255] to a hex string, where each byte becomes 2 hex digits."
  [unsigned-bytes :- [s/Int]]
  (str/join (map byte-unsigned->hex unsigned-bytes)))

(s/defn bytes-signed->hex-str :- s/Str
  "Converts a sequence of signed bytes [-128..127] to a hex string, where each byte becomes 2 hex digits."
  [signed-bytes :- [s/Int]]
  (str/join (map byte-signed->hex signed-bytes)))


; for ref:  (crypt/stringToUtf8ByteArray s)
(s/defn str->byte-array ; #todo: move to tupelo.string and avoid duplicate!
  [str-val :- s/Str]
  (let [unsigned-bytes (mapv t/char->codepoint (t/str->chars str-val))
        byte-arr       (do
                         #?(:clj (byte-array (bytes-unsigned->signed unsigned-bytes)))
                         #?(:cljs (into-array unsigned-bytes)))]
    byte-arr))

#?(:clj
   (def str->sha ; #todo: move to tupelo.string
     "Returns the SHA-1 hex string for a string"
     (let [sha-1-instance (MessageDigest/getInstance "SHA")]
       (s/fn str->sha :- s/Str
         [str-val :- s/Str]
         (let [byte-arr (str->byte-array str-val)]
           (.reset sha-1-instance)
           (.update sha-1-instance byte-arr)
           (let [bytes      (vec (.digest sha-1-instance))
                 hex-result (bytes-signed->hex-str bytes)]
             hex-result))))))
#?(:cljs ; #todo: move to tupelo.string
   (s/defn str->sha ; modeled after reagent-utils reagent.crypt
     [str-val :- s/Str]
     (let [sha-1-instance (goog.crypt.Sha1.)]
       (let [byte-arr (str->byte-array str-val)]
         (.update sha-1-instance byte-arr))
       (let [bytes      (vec (.digest sha-1-instance))
             hex-result (bytes-unsigned->hex-str bytes)]
         hex-result))))

(s/defn hash->hex :- s/Str
  "Given arbitrary arguments, uses clojure.lang/hash to generate a 32-bit hex hashcode."
  [& args] (format "%08x" (hash args)))

;---------------------------------------------------------------------------------------------------
;#?(:clj  ; #todo old way; delete?
;   (do
;     ;(s/defn long->byte-array
;     ;  "Converts a Long into an array of bytes (big-endian)."
;     ;  [arg]
;     ;  (validate tt/long? arg)
;     ;  (it-> (ByteBuffer/allocate Long/BYTES)
;     ;    (.putLong it arg)
;     ;    (.array it)))
;
;     (def uuid->str
;       "Returns the SHA-1 hex string for a UUID string"
;       (let [sha-1-instance (MessageDigest/getInstance "SHA")]
;         (s/fn uuid->str :- s/Str
;           [uuid :- java.util.UUID]
;           (let [bytes-big    (long->byte-array (.getMostSignificantBits ^UUID uuid))
;                 bytes-little (long->byte-array (.getLeastSignificantBits ^UUID uuid))
;                 bytes-all-vec (into (vec bytes-big) (vec bytes-little))
;                 bytes-all (byte-array bytes-all-vec)
;                 ]
;             (spyx (vec bytes-all-vec))
;             (.reset sha-1-instance)
;             (.update sha-1-instance bytes-big)
;             (.update sha-1-instance bytes-little)
;             (let [bytes (.digest sha-1-instance)]
;               (signed-bytes->hex-str (vec bytes)))))))))

   (defn uuid->sha
     "Returns the SHA-1 hex string for a UUID's string representation"
     [uuid]
     (t/when-clojure-1-9-plus
       (when-not (uuid? uuid)
         (throw (ex-info "arg must be a uuid" (t/vals->map uuid)))))
     (let [uuid-str (do
                      #?(:clj (str uuid))
                      #?(:cljs (.-uuid uuid)))
           usha     (str->sha uuid-str)]
       usha))

(s/defn sha-uuid :- s/Str
  "Returns a string that is the SHA-1 hash of the

        Clojure:         (clj-uuid/v1)
        ClojureScript:   (cljs.core/random-uuid)
        "
  []
  (uuid->sha
    #?(:clj (clj-uuid/v1))
    #?(:cljs (random-uuid)) ))

(def HID s/Keyword) ; #todo find way to validate
(s/defn new-hid :- HID
  "Returns a new HexID"
  []
  (keyword (sha-uuid)))

(s/defn hid? :- s/Bool
  "Returns true if the arg is a legal HexID"
  [arg]
  (and (keyword? arg)
    (let [name-str (kw->str arg)]
      (and (ts/hex? name-str)
        ; (= 40 (count name-str)) ; #todo make more robust re. with-debug-hid
        ))))

(defn normalized-sorted ; #todo need tests & docs. Use for datomic Entity?
  "Walks EDN data, converting all collections to vector, sorted-map-generic, or sorted-set-generic"
  [edn-data]
  (let [unlazy-item (fn [item]
                      (cond
                        (sequential? item) (vec item)
                        (t/xmap? item) (t/->sorted-map-generic item)
                        (set? item) (t/->sorted-set-generic item)
                        :else item))
        result      (walk/postwalk unlazy-item edn-data)]
    result))

(s/defn edn->sha :- s/Str
  "Converts EDN data into a normalized SHA-1 string"
  [edn-data]
  (str->sha (pr-str (normalized-sorted edn-data))))


;----- toptop -----------------------------------------------------------------------------

#?(:clj
   (do
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

     (def ^:dynamic *os-shell* "/bin/bash") ; could also use /bin/zsh, etc

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

     (defn shell-cmd
       "Runs a command string in the default OS shell (/bin/bash); returns result in a Clojure map.
        Example:

             (shell-cmd \"ls -ldF *\")

               ;=>   {:exit    0     ; unix exit status (0 -> normal)
                      :err    ''     ; text from any errors
                      :out    '...'  ; text output as would printed to console
                     }
       "
       [cmd-str]
       (let [result (shell/sh *os-shell* "-c" cmd-str)]
         (if (= 0 (grab :exit result))
           result
           (throw (ex-info "shell-cmd: clojure.java.shell/sh failed, cmd-str:" (vals->map cmd-str result))))))

     (defn get-os []
       (let [os-name (System/getProperty "os.name")]
         (condp re-find (str/lower-case os-name) ; required to match os.name=" Windows 8.1"
           #"windows" :windows
           #"linux" :linux
           #"mac" :mac
           (throw (ex-info "get-os: Unknown operating system found: " (vals->map os-name))))))

     (comment "stuff to make a generic run-shell-cmd"

       (defn format-shell-cmd-vec [cmd-str]
         (when-not (string? cmd-str)
           (throw (ex-info "format-shell-cmd: cmd-str must be a string; received=" (vals->map cmd-str))))
         (if (is-windows?)
           ["cmd.exe" "/c" cmd-str :dir "c:\\users"] ; windows
           ["bash" "-c" cmd-str] ; linux
           ))

       (defn run-shell-cmd [cmd-str]
         (when-not (string? cmd-str)
           (throw (ex-info "format-shell-cmd: cmd-str must be a string; received=" (vals->map cmd-str))))
         (apply shell/sh (util/format-shell-cmd-vec cmd-str)))

       )


     ; #todo document in README
     (def ^:no-doc dot-counter (atom 0))
     (def ^:no-doc dots-ctx (atom {:dots-per-row 100
                                   :decimation   1}))
     (defn dots-config! ; #todo need docstring
       [ctx]        ; #todo check pos integers
       (swap! dots-ctx conj ctx))
     (defn dot-counter-watch-fn
       [key dot-counter-ref old-count new-count]
       (let [decimation     (grab :decimation @dots-ctx)
             counts-per-row (* decimation (grab :dots-per-row @dots-ctx))]
         (when (not= old-count new-count)
           (locking dot-counter
             (when (zero? (rem old-count counts-per-row))
               (it-> old-count
                 (str it)
                 (ts/pad-left it 10)
                 (glue it \space)
                 (print it))
               (flush))
             (when (zero? (rem old-count decimation))
               (print \.)
               (flush))
             (when (zero? (rem new-count counts-per-row))
               (newline))))))

     (defn dot
       "Prints a single dot (flushed) to the console, keeping a running count of dots printed.  Wraps to a
        newline when 100 dots have been printed. Displays the running dot count at the beginning of each line.
        Usage:

              (ns demo.core
                (:require [tupelo.misc :as tm]))
              (tm/dots-config! {:decimation 10} )
              (tm/with-dots
                (doseq [ii (range 2345)]
                  (tm/dot)
                  (Thread/sleep 5)))
        "
       []
       (swap! dot-counter inc))

     (defmacro with-dots
       "Increments indentation level of all spy, spyx, or spyxx expressions within the body."
       [& body]
       `(do
          (remove-watch dot-counter :dot-counter)
          (reset! dot-counter 0)
          (add-watch dot-counter :dot-counter dot-counter-watch-fn)
          (let [result# (do ~@body)]
            (newline) (println (
                                 #?(:clj format
                                    :cljs rfmt/format)
                                 "%10d total" @dot-counter))
            result#)))

     (s/defn hid->wid :- s/Keyword
       "Uses an HID to look up a human-friendly Word-ID (WID) from an English dictionary.
       Useful for debugging purposes."
       [hid :- HID]
       nil)         ; #todo


     ;-----------------------------------------------------------------------------
     ; -> tupelo.files ?
     (defn get-path
       "Wrapper for Java Paths/get to work around the varargs strangeness"
       [path-str]
       (Paths/get "." (into-array String [path-str])))

     ;-----------------------------------------------------------------------------
     ; #todo -> tupelo.vector
     ; #todo README & more tests

     ;; Assuming require [clojure.tools.logging :as log]
     (defn log-uncaught-exceptions []
       (Thread/setDefaultUncaughtExceptionHandler
         (reify Thread$UncaughtExceptionHandler
           (uncaughtException [_ thread ex]
             (println ex "Uncaught exception on" (.getName thread)))))) ; or (log/error ...)

     (s/defn stacktrace-info :- [tsk/KeyMap] ; #todo make cljs version
       "Returns a map with the caller's namespace and function names as strings, like:
           {:ns-name 'tst.demo.core' :fn-name 'funky'} "
       [throwable :- Throwable]
       (let [stacktrace      (.getStackTrace throwable)
             stacktrace-info (t/distinct-using #(grab :class-name %)
                               (for [st-elem stacktrace]
                                 (let [class-name  (.getClassName st-elem)
                                       file-name   (.getFileName st-elem)
                                       method-name (.getMethodName st-elem)
                                       line-num    (.getLineNumber st-elem)

                                       ; class-name-caller is (usually) like "tst.demo.core$funky".
                                       ; if no `$` is present, don't crash!
                                       idx        (str/index-of class-name \$)
                                       ns-name     (t/cond-it-> class-name
                                                     (t/not-nil? idx) (subs class-name 0 idx))
                                       fn-name     (if (t/not-nil? idx)
                                                     (subs class-name (inc idx))
                                                     "")]
                                   (vals->map class-name file-name method-name line-num ns-name fn-name))))]
         stacktrace-info))

     (s/defn fn-info :- tsk/KeyMap ; #todo make cljs version
       "Returns a map of info about the current function, like:

             {:ns-name     'demo.core'
              :fn-name     'add2'
              :class-name  'demo.core$add2'
              :file-name   'core.clj'
              :line-num    57
              :method-name 'invokeStatic' }
            "
       []
       (let [stacktrace-info (stacktrace-info (RuntimeException. "dummy"))
             my-ns           (grab :ns-name (t/xfirst stacktrace-info))
             info-keep       (t/drop-if (fn [ste]
                                          (let [unwanted-flg (and (= my-ns (grab :ns-name ste))
                                                               (ts/contains-match? (grab :fn-name ste) #"fn.info"))]
                                            ; (spyx (vals->map ste unwanted-flg))
                                            unwanted-flg))
                               stacktrace-info)]
         (t/xfirst info-keep)))

     (s/defn fn-info-caller :- tsk/KeyMap ; #todo make cljs version
       "Returns a map of info about the caller of the current function, like:

             {:ns-name     'demo.core'
              :fn-name     'add2'
              :class-name  'demo.core$add2'
              :file-name   'core.clj'
              :line-num    57
              :method-name 'invokeStatic' }
            "
       []
       (let [stacktrace-info (stacktrace-info (RuntimeException. "dummy"))
             my-ns           (grab :ns-name (t/xfirst stacktrace-info))
             info-keep       (t/drop-if (fn [ste]
                                          (let [unwanted-flg (and (= my-ns (grab :ns-name ste))
                                                               (ts/contains-match? (grab :fn-name ste) #"fn.info"))]
                                            ; (spyx (vals->map ste unwanted-flg))
                                            unwanted-flg))
                               stacktrace-info)]
         (t/xsecond info-keep)))


     ))

#?(:cljs
   (do

     (defn grouper
       "Uses js/RegExp to find matching groups.  Sample output:

             (grouper #\"[a-z0-9][A-Z]\"  \"aTaTa\")  =>
               [ {:groups [\"aT\"]  :match \"aT\"  :index 0  :last-index 2  :input \"aTaTa\" }
                 {:groups [\"aT\"]  :match \"aT\"  :index 2  :last-index 4  :input \"aTaTa\" } ]

             (grouper  #\"((\\d+)-(\\d+))\" \"672-345-456-3212\")  =>
               [ {:groups [\"672-345\"  \"672-345\"  \"672\" \"345\" ]  :match \"672-345\"   :index 0  :last-index  7  :input \"672-345-456-3212\" }
                 {:groups [\"456-3212\" \"456-3212\" \"456\" \"3212\"]  :match \"456-3212\"  :index 8  :last-index 16  :input \"672-345-456-3212\" } ]

       Note that the JS value returned by `:last-index` is the index of the first char in the input string *after* the current match.
       "
       [re input-str]
       (let [re-src (.-source re)] ; the source string from the regexp arg
         (loop [groups []
                regexp (js/RegExp. re-src "g")] ; 'g' => global search
           (let [res-js  (.exec regexp input-str)
                 res-clj (js->clj res-js)]
             (if (nil? res-js)
               groups
               (recur
                 (conj groups {:groups     res-clj :match (get res-clj 0)
                               :index      (.-index res-js)
                               :input      (.-input res-js)
                               :last-index (.-lastIndex regexp)})
                 regexp))))))

     ))


; #todo move to tupelo
;
;(def xml-file-name
;   "data.xml"
;  ;"data-1k.xml"
;  ;"data-10k.xml"
;  ;"data-full.xml"
;  )
;(def bush-file-name "data-bush.edn")
;
;(defn load-xml-data  []
;  (-> xml-file-name
;    (io/resource)
;    (io/input-stream)
;    (slurp)))
;
;(defn load-bush-data  []
;  (let [bush-data (with-timer :load-bush-data
;                    (-> bush-file-name
;                      (io/resource)
;                      (io/input-stream)
;                      (slurp)
;                      (edn/read-string)))]
;    (spyx-pretty (clip-str 999 bush-data))
;  ))
;
;
;(defn xml->bush []
;  (with-forest (new-forest)
;    (let [xml-data      (with-timer :xml-data
;                          (load-xml-data))
;         ;>>            (println :xml-data \newline (clip-str 999 xml-data))
;          enlive-data   (with-timer :xml->enlive
;                          (xml->enlive xml-data))
;         ;>>            (println :enlive-data \newline (clip-str 999 enlive-data))
;          tree-data     (with-timer :enlive->tree
;                          (enlive->tree enlive-data))
;          root-hid      (with-timer :add-tree
;                          (add-tree tree-data))
;          >>            (with-timer :count-all-hid
;                          (spyx (count (all-hids))))
;          bush-data     (with-timer :hid->bush
;                          (hid->bush root-hid))
;         ;>>            (spyx-pretty (clip-str 999 bush-data))
;          bush-data-str (with-timer :pretty-str (pretty-str bush-data))
;          ]
;     ;(println (clip-str 999 bush-data-str))
;      (print "writing data...")
;      (spit (str "resources/" bush-file-name) bush-data-str)
;      (println "  done.")))
;  )
;
;
;(defn -main [& args]
;  (println "main - enter")
;  ;(println "main - hit <enter> to start:" ) (read-line)
; ;(xml->bush)
;  (load-bush-data)
;
;  ;(println "main - hit <enter> to exit:" ) (read-line)
;  (println "main - leave")
;  )
;
