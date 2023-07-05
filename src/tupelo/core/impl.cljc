;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns ^:no-doc tupelo.core.impl
  (:require
    [clojure.string :as str]
    [schema.core :as s]
    ))

(defn ^:no-doc type-name-str
  [arg]
  #?(:clj  (.getName (clojure.core/class arg))

     ; #todo file cljs bug report!
     ; This way doesn't work for records in cljs
     ;     (cljs.core/type->str (cljs.core/type arg))
     ; Result is:
     ;   "function (raw,__meta,__extmap,__hash){\nthis.raw = raw;\nthis.__meta = __meta;\nthis.__extmap = __extmap;\nthis.__hash = __hash;\nthis.cljs$lang$protocol_mask$partition0$ = 2230716170;\nthis.cljs$lang$protocol_mask$partition1$ = 139264;\n}"
     :cljs (with-out-str (pr (type arg))))) ; This kludge works => "tst.tupelo.lexical/DummyEid"

(defn ^:no-doc native-array?
  "Returns true iff arg is a native Java or JavaScript array."
  [arg]
  (boolean
    #?(:clj  (.isArray (class arg))
       :cljs (cljs.core/array? arg))))

(def ^:no-doc uuid-regex-pattern
  #?(:clj  #"(?x)             # expanded mode
            \p{XDigit}{8}     # 8 hex digits
            -                 # hyphen
            \p{XDigit}{4}     # 4 hex digits
            -                 # hyphen
            \p{XDigit}{4}     # 4 hex digits
            -                 # hyphen
            \p{XDigit}{4}     # 4 hex digits
            -                 # hyphen
            \p{XDigit}{12}    # 12 hex digits
            "
     :cljs #"[0-9a-fA-f]{8}-[0-9a-fA-f]{4}-[0-9a-fA-f]{4}-[0-9a-fA-f]{4}-[0-9a-fA-f]{12}"))

(s/defn ^:no-doc uuid-str? :- s/Bool
  "Returns true iff the string shows a valid UUID-like pattern of hex digits. Does not
  distinguish between UUID subtypes."
  [arg]
  (boolean
    (when (string? arg)
      (re-matches uuid-regex-pattern arg))))

(s/defn ^:no-doc indent-lines-with :- s/Str ; #todo add readme ;  need test
  "Splits out each line of txt using clojure.string/split-lines, then
  indents each line by prepending it with the supplied string. Joins lines together into
  a single string result, with each line terminated by a single \newline."
  [indent-str :- s/Str
   txt :- s/Str]
  (str/join
    (interpose \newline
      (for [line (str/split-lines txt)]
        (str indent-str line)))))

