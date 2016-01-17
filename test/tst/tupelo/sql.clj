(ns tst.tupelo.sql
  (:use tupelo.sql
        tupelo.core
        clojure.test)
  (:require [clojure.string         :as str]
            [clojure.java.jdbc      :as jdbc]
            [java-jdbc.ddl          :as ddl]
            [java-jdbc.sql          :as sql]
            [schema.core            :as s]
            [tupelo.misc            :as tm] )
  (:import  com.mchange.v2.c3p0.ComboPooledDataSource))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(set! *warn-on-reflection* false)

; #todo git basic pg_hba.conf into git
; #todo blog about scm of pg *.conf files
(def db-spec
  { :classname    "org.postgresql.Driver"
    :subprotocol  "postgresql"
    :subname      "//localhost:5432/alan"    ; database="alan"  #todo -> pgsql
    ; Not needed for a non-secure local database...
    ;   :user      "bilbo"
    ;   :password  "secret"
    } )

(deftest t-kw->db
  (testing "basic usage"
    (is (= "abc_def_gh" (kw->db :abc-def-gh)))))

(deftest t-db->kw
  (testing "basic usage"
    (is (= :abc-def-gh (db->kw "ABC_DEF_GH")))))

; int integer int4 int8 
; numeric
(deftest t-create-table
  (let [cmd   (create-table :tmp {:aa (not-null :int)  :bb :text} ) ]
    (is (= cmd "create table tmp (\n  aa int not null,\n  bb text) ;"))
    (try
      (jdbc/db-do-commands db-spec cmd )
      (jdbc/db-do-commands db-spec "insert into tmp (aa,bb) values (1,'one'); ")
      (jdbc/db-do-commands db-spec "insert into tmp (aa,bb) values (2,'two'); ")
      (jdbc/query db-spec (sql/select "*" :tmp))
      (jdbc/db-do-commands db-spec (drop-table :tmp))
    (catch Exception ex
      (do (spyx ex)
          (spyx (.getNextException ex))
          (System/exit 1))))))

(deftest t-out
  (is (= "user_name, phone, id"     (tm/collapse-whitespace (out :user-name :phone :id))))
  (is (= "*"                        (tm/collapse-whitespace (out :*))))
  (is (= "count(*)"                 (tm/collapse-whitespace (out "count(*)")))))

(deftest t-using
  (is (= "using (user_name, phone, id)"     (tm/collapse-whitespace (using :user-name :phone :id))))
  (is (= "using (aa)"                       (tm/collapse-whitespace (using :aa)))))

(deftest t-on
  (is (= "on (t1.aa = t2.aa)"     (tm/collapse-whitespace (on "t1.aa = t2.aa")))))

(deftest t-select
  (is (= "select user_name, phone, id from user_info"
         (tm/collapse-whitespace (select :user-name  :phone  :id  :from :user-info))
         (tm/collapse-whitespace (select "user-name,  phone" "id" :from "user-info"))))
  (is (= "select * from log_data"
         (tm/collapse-whitespace (select :* :from :log-data))))
  (is (= "select count(*) from big_table"
         (tm/collapse-whitespace (select "count(*)" :from :big-table)))))

(deftest t-natural-join
  (try
    (jdbc/db-do-commands db-spec (drop-table-if-exists :tmp1))
    (jdbc/db-do-commands db-spec (drop-table-if-exists :tmp2))

    (jdbc/db-do-commands db-spec 
      (create-table :tmp1 {:aa (not-null :int)  :bb :text} ))
    (jdbc/db-do-commands db-spec "insert into tmp1 (aa,bb) values (1,'one'); ")
    (jdbc/db-do-commands db-spec "insert into tmp1 (aa,bb) values (2,'two'); ")
  ; (spyx (jdbc/query db-spec (sql/select "*" :tmp1)))

    (jdbc/db-do-commands db-spec 
      (create-table :tmp2 {:aa (not-null :int)  :cc :text} ))
    (jdbc/db-do-commands db-spec "insert into tmp2 (aa,cc) values (1,'cc-one'); ")
    (jdbc/db-do-commands db-spec "insert into tmp2 (aa,cc) values (2,'cc-two'); ")
  ; (spyx (jdbc/query db-spec (sql/select "*" :tmp2)))

    (newline)
    (println "live query results:")
    (prn (jdbc/query db-spec 
      (spyx (join { :ll         (select :*  :from :tmp1)
                    :rr         (select "*" :from "tmp2")  ; mixed is ok
                    :using      [:aa]
                    :select     [:*]
                  } ))))
    (newline)
    (prn (jdbc/query db-spec 
      (spyx (join { :ll         (select :* :from :tmp1)
                    :rr         (select :* :from :tmp2) 
                    :on         "ll.aa = rr.aa"
                    :select     [:*]
                  } ))))

  (is (= "select count(*) from big_table"
         (tm/collapse-whitespace (select "count(*)" :from :big-table))))

  (catch Exception ex
    (do (spyx ex)
        (spyx (.getNextException ex))
        (System/exit 1)))))

(deftest t-drop-table-if-exists
  (let [cmd   (drop-table-if-exists :tmp) ]
    (is (= "drop table if exists tmp ;\n" cmd))
    (jdbc/db-do-commands db-spec cmd ))
)
