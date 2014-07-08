;   Copyright (c) Alan Thompson. All rights reserved.  ;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Cooljure - Cool stuff you wish was in Clojure.  
            Utils for reading CSV (comma-separated-value) formatted files."
      :author "Alan Thompson"}
  cooljure.csv
  (:require [clojure.string             :as str]
            [clojure.java.io            :as io]
            [clojure-csv.core           :as csv]
            [cooljure.misc              :as cool-misc] 
            [cooljure.core              :refer :all] ))

(defn csv->row-maps
  "Returns a sequence of maps constructed from the lines of the input file.  The first line
  is assumed to be column header strings, which are (safely) converted into keywords.
  String data from each subsequent line is paired with the corresponding column keyword to
  construct a map for that line.  Default delimiter is the comma character (i.e. \\,) but 
  may be changed using the syntax such as: 
  
    (cvs->row-maps my-file.psv :delimiter \\| )

  to select the pipe character (i.e. \\|) as the delimiter.  "
  [csv-file & {:as opts} ] 
  { :pre  [ (string? csv-file) ]
    :post [ (map? (first %)) ] }
  (let [opts-def      (merge {:delimiter \,} opts)
        data-lines    (apply csv/parse-csv (slurp csv-file) (keyvals opts-def))
        hdrs-kw       (mapv cool-misc/str->kw (first data-lines))
        data-maps     (for [row (rest data-lines)]
                        (zipmap hdrs-kw row) )
  ] data-maps ))

