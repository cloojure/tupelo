;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns tupelo.tagstr
  (:use tupelo.core)
  (:require
    [clojure.walk :as walk]
    [schema.core :as s]
    [tupelo.java-time.convert :as convert]
    [tupelo.schema :as tsk]
    )
  (:import
    [java.sql Date Timestamp]
    [java.time Instant ZonedDateTime]
    [java.util UUID]
    ))

(def tag-str-capture-regex
  #"(?x)          # expanded form
    \<            # opening sequence
    (\#[\.\w]+)   # tag-str hash, then alnum+dot (1 or more), in a capture group
    \s            # single space
    .+            # data str
    >             # closing seq ")
(def data-str-capture-regex
  #"(?x)      # expanded form
    \<\#        # opening sequence
    [\.\w]+   # alnum/dot, 1 or more
    \s        # single space
    (.+)      # data-str, any char (1 or more), in a capture group
    >         # closing seq ")

(s/defn ^:no-doc extract-tag-str :- s/Str
  [s :- s/Str]
  (xsecond (re-matches tag-str-capture-regex s)))

(s/defn ^:no-doc extract-data-str :- s/Str
  [s :- s/Str]
  (xsecond (re-matches data-str-capture-regex s)))

(s/defn UUID-encode :- s/Str
  [uuid :- UUID] (str "<#uuid " uuid ">"))
(s/defn UUID-parse :- UUID
  [s :- s/Str] (UUID/fromString (extract-data-str s)))

(s/defn Instant-encode :- s/Str
  [inst :- Instant] (str "<#inst " inst ">"))
(s/defn Instant-parse :- Instant
  [s :- s/Str] (Instant/parse (extract-data-str s)))

(s/defn ZonedDateTime-encode :- s/Str
  [inst :- ZonedDateTime] (str "<#ZonedDateTime " inst ">"))
(s/defn ZonedDateTime-parse :- ZonedDateTime
  [s :- s/Str] (ZonedDateTime/parse (extract-data-str s)))

(s/defn Date-encode :- s/Str
  [date :- java.util.Date] (str "<#java.util.Date " (convert/Date->str date) ">"))
(s/defn Date-parse :- java.util.Date
  [s :- s/Str] (convert/str->Date (extract-data-str s)))

(s/defn sql-Date-encode :- s/Str
  [date :- java.sql.Date] (str "<#java.sql.Date " (convert/sql-Date->str date) ">"))
(s/defn sql-Date-parse :- java.sql.Date
  [s :- s/Str] (convert/str->sql-Date (extract-data-str s)))

(s/defn sql-Timestamp-encode :- s/Str
  [ts :- Timestamp] (str "<#java.sql.Timestamp " (convert/sql-Timestamp->str ts) ">"))
(s/defn sql-Timestamp-parse :- Timestamp
  [s :- s/Str] (convert/str->sql-Timestamp (extract-data-str s)))

(def type->encode-fn
  {UUID               UUID-encode
   Instant            Instant-encode
   ZonedDateTime      ZonedDateTime-encode
   java.util.Date     Date-encode
   java.sql.Date      sql-Date-encode
   java.sql.Timestamp sql-Timestamp-encode})
(def tag->parse-fn
  {"#uuid"               UUID-parse
   "#inst"               Instant-parse
   "#ZonedDateTime"      ZonedDateTime-parse
   "#java.util.Date"     Date-parse
   "#java.sql.Date"      sql-Date-parse
   "#java.sql.Timestamp" sql-Timestamp-parse})

(s/defn walk-data->tagstr :- s/Any ; #todo => tupelo.tagstr
  "Convert objects to tagged strings like:

        <#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>
        <#inst 1999-12-31T01:02:03.456Z>
        <#java.util.Date Thu Dec 30 17:02:03 PST 1999>
        <#java.sql.Date 1999-12-30>
        <#java.sql.Timestamp 1999-12-30 17:02:03.456>
  "
  ([data :- s/Any] (walk-data->tagstr type->encode-fn data))
  ([encode-map :- tsk/Map
    data :- s/Any]
   (walk/postwalk
     (fn [item]
       (let [tgt-type (type item)]
         (cond-it-> item
           (contains-key? encode-map tgt-type) (let [encode-fn (fetch encode-map tgt-type)]
                                                 (encode-fn item)))))
     data)))
; #todo add tagval {:esec 23} => "#{:esec 23}" + un/serialize fns + tagval-str?
; #todo add tagstr? "<#\w+\s\w+>"

