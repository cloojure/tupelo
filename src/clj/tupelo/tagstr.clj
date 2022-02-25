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

(def ^:no-doc tag-str-regex
  #"(?x)          # expanded form
    \<\#          # opening sequence
    .+            # tag & data
    >             # closing seq ")
(def ^:no-doc tag-str-capture-regex
  #"(?x)          # expanded form
    \<            # opening sequence
    (\#[\.\w]+)   # tag-str hash, then alnum+dot (1 or more), in a capture group
    \s            # single space
    .+            # data str
    >             # closing seq ")
(def ^:no-doc data-str-capture-regex
  #"(?x)      # expanded form
    \<\#        # opening sequence
    [\.\w]+   # alnum/dot, 1 or more
    \s        # single space
    (.+)      # data-str, any char (1 or more), in a capture group
    >         # closing seq ")

(s/defn tagstr? :- s/Bool
  "Returns true iff arg is a Tagged String"
  [item :- s/Any] (truthy? (and (string? item)
                             (re-matches tag-str-regex item))))

(s/defn extract-tag-str :- s/Str
  "Given a tagstr like `<#java.util.Date 1999-12-31T01:02:03.456Z>`,
  returns the tag str `#java.util.Date>` "
  [s :- s/Str]
  (xsecond (re-matches tag-str-capture-regex s)))

(s/defn extract-data-str :- s/Str
  "Given a tagstr like `<#java.util.Date 1999-12-31T01:02:03.456Z>`,
  returns the data str `1999-12-31T01:02:03.456Z` "
  [s :- s/Str]
  (xsecond (re-matches data-str-capture-regex s)))

(s/defn UUID-encode :- s/Str
  "Encodes a UUID object into a tagstr like `<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>`"
  [uuid :- UUID] (str "<#uuid " uuid ">"))
(s/defn UUID-parse :- UUID
  "Decodes a tagstr like `<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>` into a UUID object"
  [s :- s/Str] (UUID/fromString (extract-data-str s)))

(s/defn Instant-encode :- s/Str
  "Encodes an Instant object into a tagstr like `<#inst 1999-12-31T01:02:03.456Z>`"
  [inst :- Instant] (str "<#inst " inst ">"))
(s/defn Instant-parse :- Instant
  "Decodes a tagstr like `<#inst 1999-12-31T01:02:03.456Z>` into an Instant object"
  [s :- s/Str] (Instant/parse (extract-data-str s)))

(s/defn ZonedDateTime-encode :- s/Str
  "Encodes a ZonedDateTime object into a tagstr like `<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>`"
  [inst :- ZonedDateTime] (str "<#ZonedDateTime " inst ">"))
(s/defn ZonedDateTime-parse :- ZonedDateTime
  "Decodes a tagstr like `<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>` into a ZonedDateTime object"
  [s :- s/Str] (ZonedDateTime/parse (extract-data-str s)))

(s/defn Date-encode :- s/Str
  "Encodes a java.util.Date object into a tagstr like `<#java.util.Date 1999-12-31T01:02:03.456Z>`"
  [date :- java.util.Date] (str "<#java.util.Date " (convert/Date->str date) ">"))
(s/defn Date-parse :- java.util.Date
  "Decodes a tagstr like `<#java.util.Date 1999-12-31T01:02:03.456Z>` into a java.util.Date object"
  [s :- s/Str] (convert/str->Date (extract-data-str s)))

(s/defn sql-Date-encode :- s/Str
  "Encodes a java.sql.Date object into a tagstr like `<#java.sql.Date 1999-11-22>`"
  [date :- java.sql.Date] (str "<#java.sql.Date " (convert/sql-Date->str date) ">"))
(s/defn sql-Date-parse :- java.sql.Date
  "Decodes a tagstr like `<#java.sql.Date 1999-11-22>` into a java.sql.Date object"
  [s :- s/Str] (convert/str->sql-Date (extract-data-str s)))

(s/defn sql-Timestamp-encode :- s/Str
  "Encodes a java.sql.Timestamp object into a tagstr like `<#java.sql.Timestamp 1999-12-31 01:02:03.456>`"
  [ts :- Timestamp] (str "<#java.sql.Timestamp " (convert/sql-Timestamp->str ts) ">"))
(s/defn sql-Timestamp-parse :- Timestamp
  "Decodes a tagstr like `<#java.sql.Timestamp 1999-12-31 01:02:03.456>` into a java.sql.Timestamp object"
  [s :- s/Str] (convert/str->sql-Timestamp (extract-data-str s)))

(def type->encode-fn
  "A map from object type to tagstr encode fn"
  {UUID               UUID-encode
   Instant            Instant-encode
   ZonedDateTime      ZonedDateTime-encode
   java.util.Date     Date-encode
   java.sql.Date      sql-Date-encode
   java.sql.Timestamp sql-Timestamp-encode})

(def tag->parse-fn
  "A map from tag to object parse fn"
  {"#uuid"               UUID-parse ; copied from EDN tagged literal
   "#inst"               Instant-parse ; copied from EDN tagged literal
   "#ZonedDateTime"      ZonedDateTime-parse
   "#java.util.Date"     Date-parse
   "#java.sql.Date"      sql-Date-parse
   "#java.sql.Timestamp" sql-Timestamp-parse})

(s/defn walk-encode :- s/Any
  "Walk a data structure and convert objects to tagged strings like:

      {:date          \"<#java.util.Date 1999-12-31T01:02:03.456Z>\"
       :five          5
       :hello         \"Hello!\"
       :instant       \"<#inst 1999-12-31T01:02:03.456Z>\"
       :millis        946602123456
       :nil           nil
       :sql-date      \"<#java.sql.Date 1999-11-22>\"
       :sql-timestamp \"<#java.sql.Timestamp 1999-12-31 01:02:03.456>\"
       :uuid          \"<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>\"
       :zdt           \"<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>\"}
  "
  ([data :- s/Any] (walk-encode type->encode-fn data))
  ([encode-map :- tsk/Map
    data :- s/Any]
   (walk/postwalk
     (fn [item]
       (let [tgt-type (type item)]
         (cond-it-> item
           (contains-key? encode-map tgt-type) (let [encode-fn (fetch encode-map tgt-type)]
                                                 (encode-fn item)))))
     data)))

(s/defn walk-parse :- s/Any
  "Walk a data structure like the following and parse tagged strings into objects:

      {:date          \"<#java.util.Date 1999-12-31T01:02:03.456Z>\"
       :five          5
       :hello         \"Hello!\"
       :instant       \"<#inst 1999-12-31T01:02:03.456Z>\"
       :millis        946602123456
       :nil           nil
       :sql-date      \"<#java.sql.Date 1999-11-22>\"
       :sql-timestamp \"<#java.sql.Timestamp 1999-12-31 01:02:03.456>\"
       :uuid          \"<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>\"
       :zdt           \"<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>\"}
  "
  ([data :- s/Any] (walk-parse tag->parse-fn data))
  ([parse-map :- tsk/Map
    data :- s/Any]
   (walk/postwalk
     (fn [item]
       (cond-it-> item
         (tagstr? item) (let [tag (extract-tag-str item)]
                          (cond-it-> item
                            (contains-key? parse-map tag) (let [parse-fn (fetch parse-map tag)]
                                                            (parse-fn item))))))
     data)))

; #todo add tagval {:esec 23} => "#{:esec 23}" + un/serialize fns + tagval-str?

