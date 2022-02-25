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
    [java.time Clock Instant ZonedDateTime]
    [java.sql Timestamp]
    [java.util Date UUID]
    ))

(def alnum+dot-plus #"[\.\w]+" ) ; [dot or word chars], 1 or more

(def alnum+dot+hyphen-colon-plus
  #"(?x)      # expanded form
    \<\#        # opening sequence
    [\.\w]+   # alnum/dot, 1 or more
    \s        # single space
    ([\-\:\.\w]+)  # alnum/dot/hypen/colon, 1 or more. In a capture group
    >         # closing seq ")

(s/defn ^:no-doc extract-data-str :- s/Str
  [s :- s/Str]
  (xsecond (re-matches alnum+dot+hyphen-colon-plus s)))

(s/defn Date-encode :- s/Str
  [date :- java.util.Date] (str "<#java.util.Date " (convert/Date->str date) ">"))
(s/defn Date-parse :- Date
  [s :- s/Str] (convert/str->Date (extract-data-str s)))

(s/defn Instant-encode :- s/Str
  [inst :- Instant] (str "<#inst " inst ">"))
(s/defn Instant-parse :- Instant
  [s :- s/Str] (Instant/parse (extract-data-str s)))

(def tag->parse-fn
  {
   "java.util.Date" Date-parse
   })
(s/defn walk-data->tagstr :- s/Any ; #todo => tupelo.tagstr
  "Convert objects to tagged strings like:

        <#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>
        <#inst 1999-12-31T01:02:03.456Z>
        <#java.util.Date Thu Dec 30 17:02:03 PST 1999>
        <#java.sql.Date 1999-12-30>
        <#java.sql.Timestamp 1999-12-30 17:02:03.456>
  "
  [data :- s/Any]
  (walk/postwalk
    (fn [item]
      (cond ; #todo => make individual fns & delegate ; plus inverse constructor fns
        (= (type item) java.util.Date) (Date-encode item)
        (= (type item) java.sql.Date) (str "<#java.sql.Date " item ">") ; or j.s.Date
        (= (type item) java.sql.Timestamp) (str "<#java.sql.Timestamp " item ">") ; or j.s.TimeStamp
        (= (type item) java.time.ZonedDateTime) (str "<#java.time.ZonedDateTime " item ">") ; or j.t.*
        ; must go after the above items due to inheritance!
        (inst? item) (str "<#inst " item ">") ; or j.t.Instant etc

        (uuid? item) (str "<#uuid " item ">") ; or j.u.UUID etc
        :else item))
    data))
; #todo add tagval {:esec 23} => "#{:esec 23}" + un/serialize fns + tagval-str?
; #todo add tagstr? "<#\w+\s\w+>"

