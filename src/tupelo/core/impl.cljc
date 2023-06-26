;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.core.impl
  "Tupelo - Making Clojure even sweeter"
  (:require
    [clojure.core :as cc]
    [clojure.string :as str]
    [schema.core :as s]))

(defn type-name->str
  "Returns the type/class name of a value as a string.  Works for both CLJ and CLJS."
  [arg]
  #?(:clj  (.getName (clojure.core/class arg))
     :cljs (let
             ; #todo file cljs bug report!
             ; This way doesn't work for records in cljs
             ;     (cljs.core/type->str (cljs.core/type arg))
             ; Result is:
             ;   "function (raw,__meta,__extmap,__hash){\nthis.raw = raw;\nthis.__meta = __meta;\nthis.__extmap = __extmap;\nthis.__hash = __hash;\nthis.cljs$lang$protocol_mask$partition0$ = 2230716170;\nthis.cljs$lang$protocol_mask$partition1$ = 139264;\n}"
             ;
             ; This kludge works => "tst.tupelo.lexical/DummyEid"
             [captured (with-out-str (pr (type arg)))]
             captured)))

(defn native-array?
  "Returns true iff arg is a native Java or JavaScript array."
  [arg]
  (boolean
    #?(:clj  (.isArray (class arg))
       :cljs (cljs.core/array? arg))))


(def ^:no-doc uuid-regex-pattern
  #?(:clj  #"(?x)            # expanded mode
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
     :cljs #"[0-9a-fA-f]{8}-[0-9a-fA-f]{4}-[0-9a-fA-f]{4}-[0-9a-fA-f]{4}-[0-9a-fA-f]{12}"
     ))

(s/defn uuid-str? :- s/Bool
  "Returns true iff the string shows a valid UUID-like pattern of hex digits. Does not
  distinguish between UUID subtypes."
  [arg]
  (boolean
    (when (string? arg)
      (re-matches uuid-regex-pattern arg))))



