(ns tst.tupelo.sql
  (:use 
        tupelo.core
        clojure.test)
  (:require [clojure.string         :as str]
            [clojure.java.jdbc      :as jdbc]
          ; [java-jdbc.ddl          :as ddl]
          ; [java-jdbc.sql          :as sql]
            [schema.core            :as s]
            [tupelo.misc            :as tm] 
            [honeysql.core          :as sql] 
            [honeysql.helpers       :refer :all]
            [tupelo.sql             :as tsql]
  )
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
  (is (= "abc_def_gh" (tsql/kw->db :abc-def-gh))))

(deftest t-db->kw
  (is (= :abc-def-gh (tsql/db->kw "ABC_DEF_GH"))))

(deftest t-keyval
  (is (= [:a 1]           (tsql/keyval {:a 1})))
  (is (thrown? Exception  (tsql/keyval {:a 1 :b 2}))))

(deftest t-db-args
  (is (= "aa"                       (tsql/db-args :aa)))
  (is (= "aa bb cc"                 (tsql/db-args :aa "bb" :cc)))
  (is (= "user_name phone id"       (tsql/db-args :user-name :phone :id)))
  (is (= "*"                        (tsql/db-args :*)))
  (is (= "count(*)"                 (tsql/db-args "count(*)"))))

(deftest t-db-list
  (is (= "aa"                     (tsql/db-list :aa)))
  (is (= "aa, bb, cc"             (tsql/db-list :aa "bb" :cc)))
  (is (= "user_name, phone, id"   (tsql/db-list :user-name :phone :id))))

; (deftest t-left
;   (is (= { :left  "a b c" } (left  :a "b" :c))))
; (deftest t-right
;   (is (= { :right "a b c" } (right :a "b" :c))))

(deftest t-drop-table
  (let [cmd   (tsql/drop-table :tmp) ]
    (is (= "drop table tmp ;\n" cmd))))

(deftest t-drop-table-if-exists
  (let [cmd   (tsql/drop-table-if-exists :tmp) ]
    (is (= "drop table if exists tmp ;\n" cmd))
    (jdbc/db-do-commands db-spec cmd )))

; int integer int4 int8 
; numeric
(deftest t-create-table
  (let [cmd   (tsql/create-table :tmp {:aa (tsql/not-null :int)  :bb :text} ) ]
    (is (= cmd "create table tmp (\n  aa int not null,\n  bb text) ;"))
    (try
      (jdbc/db-do-commands db-spec cmd )
      (jdbc/insert! db-spec :tmp {:aa 1 :bb "one"}
                                 {:aa 2 :bb "two"} )
      (jdbc/query db-spec (tsql/select :* :tmp))
      (jdbc/db-do-commands db-spec (tsql/drop-table :tmp))
    (catch Exception ex
      (do (spyx ex)
          (spyx (.getNextException ex))
          (System/exit 1))))))

(deftest t-arrays
  (try
    (spyx (jdbc/db-do-commands db-spec "create table ta (name text, vals integer[]) ;"))
    (spyx (jdbc/db-do-commands db-spec "insert into  ta values ('aa', '{1,2,3}' ) ;"))
    (spyx (jdbc/query db-spec (tsql/select :* :ta)))
    (jdbc/db-do-commands db-spec (tsql/drop-table :ta))
  (catch Exception ex
    (do (spyx ex)
        (spyx (.getNextException ex))
        (System/exit 1)))))

(deftest t-where
  (is (= "where (t1.aa = t2.aa)"  (tm/collapse-whitespace (spyx (tsql/where "t1.aa = t2.aa"))))))

(deftest t-select
  (is (= "select user_name, phone, id from user_info"
         (tm/collapse-whitespace (tsql/select :user-name  :phone  :id  :user-info))
         (tm/collapse-whitespace (tsql/select "user-name,  phone" "id" "user-info"))))
  (is (= "select * from log_data"
         (tm/collapse-whitespace (tsql/select :* :log-data))))
  (is (= "select count(*) from big_table"
         (tm/collapse-whitespace (tsql/select "count(*)" :big-table)))))

(deftest t-with
  (is (= {:with [:a :b] } (tsql/with :a :b))))

(deftest t-cte
  (is (= {:cte :x} (tsql/cte :x))))

; (deftest t-using
;   (is (= { :using [:user-name :phone :id] }   (spyx (tsql/using :user-name :phone :id))))
;   (is (= { :using [:aa] }                     (tsql/using :aa))))
; 
; (deftest t-on
;   (is (= "on (t1.aa = t2.aa)"     (tm/collapse-whitespace (spyx (on "t1.aa = t2.aa"))))))

(deftest t-join
  (try
    (jdbc/db-do-commands db-spec (tsql/drop-table-if-exists :tmp1))
    (jdbc/db-do-commands db-spec (tsql/drop-table-if-exists :tmp2))
    (jdbc/db-do-commands db-spec (tsql/drop-table-if-exists :tmp3))

    (jdbc/db-do-commands db-spec 
      (tsql/create-table :tmp1 {:aa (tsql/not-null :int)  :bb :text} ))
    (jdbc/insert! db-spec :tmp1 {:aa 1 :bb "one"}
                                {:aa 2 :bb "two"} )
    (jdbc/db-do-commands db-spec 
      (tsql/create-table :tmp2 {:aa (tsql/not-null :int)  :cc :text} ))
    (jdbc/insert! db-spec :tmp2 {:aa 1 :cc "cc-one"}
                                {:aa 2 :cc "cc-two"} )
    (jdbc/db-do-commands db-spec 
      (tsql/create-table :tmp3 {:aa (tsql/not-null :int)  :dd :text} ))

    (spy :msg "jdbc/execute!"
      (jdbc/execute! db-spec 
        (spyx (-> (insert-into :tmp3) 
                  (values [ {:aa 1 :dd "dd-1"}
                            {:aa 2 :dd "dd-2"} ] )
                  (sql/format)))))


    (newline)
    (spyx (select :*))
    (spyx (from :tmp1))

    (newline)
    (let [cmd (spyx (-> (select :*) (from :tmp1) sql/format)) ]
      (prn (jdbc/query db-spec cmd)))
    (let [cmd (spyx (-> (select :*) (from :tmp2) sql/format)) ]
      (prn (jdbc/query db-spec cmd)))
    (let [cmd (spyx (-> (select :*) (from :tmp3) sql/format)) ]
      (prn (jdbc/query db-spec cmd)))

    (let [cmd (spyx (-> (select :*) 
                        (from [:tmp1 :t1])
                        (join [:tmp2 :t2] [:= :t1.aa :t2.aa] )
                        (sql/format)))
    ]
      (prn (jdbc/query db-spec cmd)))

  (newline)
  (let [cmd (tsql/with 
              (spyx (tsql/cte [:t1 (-> (select :*) (from :tmp1)) ] ))
              (spyx (tsql/cte [:t2 (-> (select :*) (from :tmp2)) ] ))) ]
    (spyx cmd)
    (spyx (tsql/format-with cmd))
  )

  ; (newline)
  ; (prn (jdbc/query db-spec 
  ; (spyx (join { :ll         (select :* :tmp1)
  ;               :rr         (select :* :tmp2) 
  ;               :on         "ll.aa = rr.aa"
  ;               :select     [:*]
  ;             } ))

  ; (is (= "select count(*) from big_table"
  ;        (tm/collapse-whitespace (tsql/select "count(*)" :big-table))))

  (catch Exception ex
    (do (spyx ex)
        (spyx (.getNextException ex))
        (System/exit 1)))))
