;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public License 1.0
;   (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at
;   the root of this distribution.  By using this software in any fashion, you are agreeing to be
;   bound by the terms of this license.  You must not remove this notice, or any other, from this
;   software.
(ns ^:test-refresh/focus
  tst.tupelo.tagstr
  (:use tupelo.tagstr tupelo.core tupelo.test)
  (:require
    [clojure.string :as str])
  (:import
    [java.sql Timestamp]
    [java.time Instant ZonedDateTime]
    [java.util Date UUID]
    ))

(dotest
  ; demo how to augment an existing character class like `\w`
  (let [alnum+dot-plus #"[\.\w]+"] ; [dot or word chars], 1 or more
    (is (nil? (re-matches #"\w+" "java.util.Date"))) ; word chars, 1 or more
    (is (nil? (re-matches #"[\w]+" "java.util.Date"))) ; word chars, 1 or more
    (is (not-nil? (re-matches alnum+dot-plus "java.util.Date"))) ; [dot or word chars], 1 or more
    (is (nil? (re-matches alnum+dot-plus "java=util=Date")))) ; [dot or word chars], 1 or more

  (isnt (tagstr? 5) )
  (isnt (tagstr? "abc") )
  (isnt (tagstr? "<j.l.String abc>") )
  (isnt (tagstr? "#String abc") )
  (isnt (tagstr? "#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8") )

  (is (tagstr? "<#j.l.String abc>") )
  (is (tagstr? "<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>") )
  (is (tagstr? "<#java.util.Date 1999-12-31T01:02:03.456Z>") )

  (is= (re-matches tag-str-capture-regex "<#j.l.String abc>") ["<#j.l.String abc>" "#j.l.String"])
  (is= (extract-tag-str  "<#j.l.String abc>") "#j.l.String" )
  (is= (extract-tag-str  "<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>") "#uuid")
  (is= (extract-tag-str  "<#java.util.Date 1999-12-31T01:02:03.456Z>") "#java.util.Date")

  (is= (re-matches data-str-capture-regex "<#j.l.String abc>") ["<#j.l.String abc>" "abc"])
  (is= (extract-data-str  "<#j.l.String abc>") "abc" )
  (is= (extract-data-str  "<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>") "605ca9b3-219b-44b3-9c91-238dba64a3f8")
  (is= (extract-data-str  "<#java.util.Date 1999-12-31T01:02:03.456Z>") "1999-12-31T01:02:03.456Z"))

(dotest
  (let [uuid          (UUID/fromString "605ca9b3-219b-44b3-9c91-238dba64a3f8")
        instant       (Instant/parse "1999-12-31t01:02:03.456Z")
        millis        (.toEpochMilli instant)
        date          (Date. millis) ; NOTE: toString() truncates millis
        sql-timestamp (Timestamp/valueOf "1999-12-31 01:02:03.456")
        sql-date      (java.sql.Date/valueOf "1999-11-22")
        zdt           (ZonedDateTime/parse "1999-11-22t11:33:44.555-08:00")

        sample-data   {:date          date
                       :five          5
                       :hello         "Hello!"
                       :instant       instant
                       :millis        millis
                       :nil           nil
                       :sql-date      sql-date
                       :sql-timestamp sql-timestamp
                       :uuid          uuid
                       :zdt           zdt}]
    (is= (UUID-encode uuid) "<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>")
    (is= uuid (-> uuid UUID-encode UUID-parse))

    (is= (Instant-encode instant) "<#inst 1999-12-31T01:02:03.456Z>")
    (is= instant (-> instant Instant-encode Instant-parse))

    (is= (ZonedDateTime-encode zdt) "<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>")
    (is= zdt (-> zdt ZonedDateTime-encode ZonedDateTime-parse))

    (is= (Date-encode date) "<#java.util.Date 1999-12-31T01:02:03.456Z>")
    (is= date (-> date Date-encode Date-parse))

    (is= (sql-Date-encode sql-date) "<#java.sql.Date 1999-11-22>")
    (is= sql-date (-> sql-date sql-Date-encode sql-Date-parse))

    (is= (sql-Timestamp-encode sql-timestamp) "<#java.sql.Timestamp 1999-12-31 01:02:03.456>")
    (is= sql-timestamp (-> sql-timestamp sql-Timestamp-encode sql-Timestamp-parse))

    (is= (str instant) "1999-12-31T01:02:03.456Z")
    (is= (walk-encode uuid) "<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>")
    (is= (walk-encode instant) "<#inst 1999-12-31T01:02:03.456Z>")
    (is= (walk-encode date) "<#java.util.Date 1999-12-31T01:02:03.456Z>")
    (is= (walk-encode sql-date) "<#java.sql.Date 1999-11-22>")
    (is= (walk-encode sql-timestamp) "<#java.sql.Timestamp 1999-12-31 01:02:03.456>")
    (is= (walk-encode zdt) "<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>")

    (let [sample-data-tagstr (walk-encode sample-data)
          tagstr-expected    {:date          "<#java.util.Date 1999-12-31T01:02:03.456Z>"
                              :five          5
                              :hello         "Hello!"
                              :instant       "<#inst 1999-12-31T01:02:03.456Z>"
                              :millis        946602123456
                              :nil           nil
                              :sql-date      "<#java.sql.Date 1999-11-22>"
                              :sql-timestamp "<#java.sql.Timestamp 1999-12-31 01:02:03.456>"
                              :uuid          "<#uuid 605ca9b3-219b-44b3-9c91-238dba64a3f8>"
                              :zdt           "<#ZonedDateTime 1999-11-22T11:33:44.555-08:00>"}
          data-parsed        (walk-parse sample-data-tagstr)]
      (is= sample-data-tagstr tagstr-expected)
      (is= sample-data data-parsed)
      (is= sample-data (-> sample-data walk-encode walk-parse)))))
