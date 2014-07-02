;   Copyright (c) Alan Thompson. All rights reserved. 
;   The use and distribution terms for this software are covered by the Eclipse Public
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
            [cooljure.misc              :as cool-misc] ))

(set! *warn-on-reflection* false)

(defn parse-csv->maps
  "Reads string data from the specified CSV file. The first row is assumed to be column
  header strings, which are (safely) converted into keywords. Each row is converted to a
  map using the column-hdr-keywords map and the corresponding string data from that row.
  If supplied, each entry of parse-fns-map associates a col-hdr-keyword & a parsing
  function, which is applied to the corresponding data entry for each row. Data for
  col-hdr-keywords not present in parse-fns-map is returned as a string.  Return value is
  a sequence of row maps.  "
  ( [csv-file] (parse-csv->maps csv-file {}) )
  ( [csv-file parse-fns-map]
    { :pre  [ (string? csv-file)
              (map? parse-fns-map) ]
      :post [ (map? (first %)) ] }
    (let [data-lines    (csv/parse-csv (slurp csv-file))
          hdrs-all      (mapv cool-misc/str->kw (first data-lines))
          data-rows     (rest data-lines)
          data-maps     (for [row data-rows]
                          (let [raw-map   (zipmap hdrs-all row)
                                data-map  (reduce 
                                            (fn [cum-map [parse-kw parse-fn]]
                                              (update-in cum-map [parse-kw] parse-fn) )
                                            raw-map parse-fns-map )
                        ] data-map ))
    ] data-maps )))

