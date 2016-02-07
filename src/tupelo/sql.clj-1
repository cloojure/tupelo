(ns tupelo.sql
  (:use tupelo.core)
  (:require [clojure.string :as str]
            [schema.core    :as s]
            [tupelo.schema  :as ts] 
            [tupelo.misc    :as tm] 
            [honeysql.core          :as sql] 
            [honeysql.helpers       :refer :all] )
  (:gen-class))

; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs

(defn- dasherize 
  "Replace underscores with dashes"
  [s]
  (str/replace s "_" "-"))

(defn- undasherize
  "Replace dashes with underscores"
  [s]
  (str/replace s "-" "_"))

(defn version
  "Query the PostgreSQL version number"
  []
  "select version();")

(s/defn kw->db :- s/Str
  "Converts a keyword to a database compatible string (e.g. all hyphens converted to underscores)"
  [kw :- s/Any]  ; #todo (either s/Keyword s/Str)
  (undasherize (name kw)))

(s/defn db->kw :- s/Keyword
  "Converts a database compatible string to a keyword (e.g. all underscores converted to hyphens,
   and all characters lowercase), "
  [arg :- s/Str]
  (keyword (str/lower-case (dasherize arg))))

(s/defn db-args :- s/Str
  "Format scalar args to db strings. (db-args :aa \"bb\" :cc) -> \"aa bb cc\" "
  [& args] ; #todo kw or string
  (str/join " " (map kw->db args)))

(s/defn db-list :- s/Str
  "Format scalar args to db strings. (db-list :aa \"bb\" :cc) -> \"aa, bb, cc\" "
  [& args] ; #todo kw or str
  (str/join ", " (map kw->db args)))

#_(s/defn on :- s/Str
  "Format a ON clause to specify a join condition"
  [arg :- s/Str]
  (format "on (%s)" arg))

(s/defn where :- s/Str
  "Format where clause"
  [arg :- s/Str]
  (format "where (%s)" arg))

(s/defn keyval 
  "Accept a map with exactly 1 entry, returning the single key & value as a length-2 vector"
  [arg :- ts/KeyMap]
  (s/validate ts/Vec2 (keyvals arg)))

(s/defn not-null :- s/Str
  "Adds the suffix 'not null' to argument."
  [arg]
  (format "%s not null" (kw->db arg)))

(s/defn drop-table :- s/Str
  "Drop (delete) a table from the database. It is an error if the table does not exist."
  [name :- s/Keyword]
  (format "drop table %s ;\n" (kw->db name)))

(s/defn drop-table-if-exists :- s/Str
  "Drop (delete) a table from the database, w/o error if no table exists."
  [name :- s/Keyword]
  (format "drop table if exists %s ;\n" (kw->db name)))

(s/def ColSpec ts/KeyMap )    ; #todo -> { :kw  [:kw | colType] } ; #todo -> pig-squeal.types
(s/defn create-table  :- s/Str
  [name       :- s/Keyword
   colspecs   :- ColSpec]
  (let [cols-str  (str/join ","
                    (for [[col-name col-type] colspecs]
                      (format "\n  %s %s"   (kw->db col-name) 
                                            (kw->db col-type))))
        result    (format "create table %s (%s) ;" (kw->db name) cols-str) ]
    result ))

; (s/defn select ...)
;   alan=> select * from tmp1; select * from tmp2;
;    id | aa | bb  
;   ----+----+-----
;     1 |  1 | one
;     2 |  2 | two
;   (2 rows)
;
;    id | aa |   cc   
;   ----+----+--------
;     1 |  1 | cc-one
;     2 |  2 | cc-two
;   (2 rows)
;
;   alan=> select xx.aa a,xx.bb b,yy.cc  from tmp1 as xx natural join (select * from tmp2) as yy;
;    a |  b  |   cc   
;   ---+-----+--------
;    1 | one | cc-one
;    2 | two | cc-two
;   (2 rows)

; (def KeywordStrList  [ (s/maybe s/Keyword)  ; #todo make this work
;                        (s/maybe s/Str) ] )

; #todo maybe do:  (select :user-info [:user-name :phone :id] )
;                            ^table       ^col       ^col  ^col
; #todo maybe do:  (select :user-info [:user-name :phone :id] 
;                          :where    ... 
;                          :group-by ... 
;                          :order-by ...  } )
;       e.g.       (select [tbl-name col-lst & options-map] )

(s/defn select :- s/Str
  "Format SQL select statement; eg: 
     (select [:user-name :phone :id] :user-info) 
     (select [:*                   ] :log-data) 
     (select [:count-*             ] :big-table)
   "  ; #todo add examples to all docstrings
  [& args :- [s/Any] ] ; #todo ts/Tuple ???
  (let [num-cols    (- (count args) 1)
        col-names   (take num-cols args)
        tails       (drop num-cols args)
        from-kw     (first tails)
        table       (last tails) 
        cols-str    (apply db-list col-names)
        table-str   (kw->db table)
        result      (format "select %s from %s" cols-str table-str)
  ]
    result))

(defn with
  [& args]
  { :with (vec args) } )

(defn cte
  [arg]
  { :cte arg } )

(defn using
  [arg]
  { :using arg } )

(defn format-cte
  [spec]
  (assert (= 1 (count spec)))
  (let [[alias expr]  (grab :cte spec) 
        expr-str      (spyx (sql/format expr)) ]
    (spyx (format "%s AS %s" (name alias) (str expr-str)))))

(defn format-with 
  [spec]
  (assert (= 1 (count spec)))
  (let [ctes  (grab :with spec) ]
    (assert (vector? ctes))
    (str join " " 
      "with "
      (forv [cte ctes]
            (format-cte cte)))))

;(defn join [&args] nil)
;(defn join
; "Performs a join between two sub-expressions."
; [ lhs     
;   rhs     
;   exp-map ]
; (let [
;   [left-name  left-exp ]    (keyval lhs)
;   [right-name right-exp]    (keyval rhs)
;   select-exp  (grab :select exp-map)
;
;   join-exp    ""
;   join-exp    (cond
;                 (contains? exp-map :using)  (apply using  (grab :using exp-map))
;                 (contains? exp-map :on)     (on           (grab :on    exp-map))
;                 :else (throw (IllegalArgumentException. "join: missing join-exp")))
;
;   result      (tm/collapse-whitespace ; #todo need utils for shifting lines (right)
;                 (format "with
;                            ll as (%s),
;                            rr as (%s)
;                          select %s from 
;                            (ll join rr %s) ;" 
;                   left-exp right-exp 
;                   (apply db-args select-exp)
;                   join-exp))
; ]
;   (println result)
;   result
; ))  

        ; select
        ;   dashboards.name,
        ;   log_counts.ct
        ; from dashboards 
        ; join (
        ;   select 
        ;     distinct_logs.dashboard_id, 
        ;     count(1) as ct
        ;   from (
        ;     select distinct 
        ;       dashboard_id, 
        ;       user_id
        ;     from time_on_site_logs
        ;   ) as distinct_logs
        ;   group by distinct_logs.dashboard_id
        ; ) as log_counts 
        ; on log_counts.dashboard_id = dashboards.id
        ; order by log_counts.ct desc

        ; WITH 
        ;   regional_sales AS (
        ;     SELECT region, SUM(amount) AS total_sales
        ;     FROM orders
        ;     GROUP BY region
        ;   ), 
        ;   top_regions AS (
        ;     SELECT region
        ;     FROM regional_sales
        ;     WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
        ;   )
        ; SELECT 
        ;   region,
        ;   product,
        ;   SUM(quantity) AS product_units,
        ;   SUM(amount) AS product_sales
        ; FROM orders
        ; WHERE region IN (SELECT region FROM top_regions)
        ; GROUP BY region, product;

(defn -main []
  (println "main - enter")
)
