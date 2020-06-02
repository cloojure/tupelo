;   Copyright (c) Alan Thompson. All rights reserved.
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.csv
  "Utils for reading CSV (comma-separated-value) formatted files."
  (:use tupelo.core)
  (:require
    [clojure.string :as str]
    [clojure-csv.core :as csv]
    [schema.core :as s]
    [tupelo.string :as ts]
    [tupelo.schema :as tsk])
  (:import [java.io Reader StringReader]))

(s/defn entities->attrs :- tsk/Map
  "Converts a sequence of entity-maps into a map of column-vectors. Not lazy."
  [row-maps :- [tsk/Map]]
  (if (empty? row-maps)
    {}
    (let [all-keys (set (keys (xfirst row-maps)))
          >>       (doseq [row-map row-maps] ; #todo test this
                     (let [curr-keys (set (keys row-map))]
                       (when-not (= all-keys curr-keys)
                         (throw (ex-info "all rows must have same keys"
                                  (vals->map all-keys curr-keys))))))
          col-vecs (apply glue
                     (for [col-key all-keys]
                       {col-key (mapv #(grab col-key %) row-maps)}))]
      col-vecs)))

(s/defn attrs->entities :- [tsk/Map]
  "Converts a map of attribute-vectors into a vector of entity-maps. Not lazy."
  [cols-map :- tsk/Map]
  (if (empty? cols-map)
    []
    (let [col-keys (keys cols-map)
          col-vals (vals cols-map)
          >>       (let [col-lens (mapv count col-vals)] ; #todo test this
                     (when-not (apply = col-lens)
                       (throw (ex-info "column lengths must be equal"
                                (vals->map col-keys col-lens)))))
          row-vals (apply map vector col-vals)
          row-maps (map #(zipmap col-keys %) row-vals)]
      row-maps)))

(defn ^:no-doc get-labels-and-data-lines ; #todo use schema
  [opts parsed-lines]
  (if (:labels opts)
    ; if user supplied col label keywords
    {:labels-kw  (grab :labels opts) ; use them
     :data-lines parsed-lines} ; all lines are data

    ;else, convert first row of strings -> col label keywords
    {:labels-kw  (mapv ts/str->kw-normalized (first parsed-lines))
     :data-lines (rest parsed-lines)})) ; rest of lines are data

; #todo: add default label-fn (comp trim safe-char )
; #todo: change to allow line-seq, FILE, etc? (document!)
; #todo: change to ignore blank lines
; #todo: throw if mismatched missing/excess fields found? (test at least!)
; #todo: modify to accept an opts map (and support parent opts:  https://github.com/davidsantiago/clojure-csv)
; #todo: add option for no header line in input (default keys => :column-001 :column-002 :column-003...
; #todo: document user-supplied column keys
; #todo: default everything to keyword keys
; #todo: throw if missing or extra fields found
; #todo: return empty map if no data rows found (with or without header row)
(s/defn parse->entities :- [tsk/Map]
  "[csv-input & {:as opts} ]
   Returns a lazy sequence of maps constructed from csv-input.  The first line
   is assumed to be column label strings, which are (safely) converted into keywords.
   String data from each subsequent line is paired with the corresponding column keyword to
   construct a map for that line.  Default delimiter is the comma character (i.e. \\,) but
   may be changed using the syntax such as:

   ```
      (parse-csv->row-maps <csv-data-src> :delimiter \\|)
   ```

   to select the pipe character (i.e. \\|) as the delimiter.  "
  ; AWTAWT TODO: update docs re. col-labels (keywords)
  [csv-input & opts]
  (assert (or (string? csv-input)
            (instance? Reader csv-input)))
  (let [opts-default {:data-fn str/trim}
        opts         (apply hash-map opts) ; opts could be nil => {}
        opts         (glue opts-default opts)
        data-fn      (grab :data-fn opts)
        csv-reader   (cond-it-> csv-input
                       (string? it) (StringReader. it))
        parsed-lines (apply csv/parse-csv csv-reader (keyvals opts))
        ; #todo maybe break in half here =>  parse-lines->entities
        {:keys [labels-kw data-lines]} (get-labels-and-data-lines opts parsed-lines)
        num-labels   (count labels-kw)
        row-maps     (for [data-line data-lines]
                       (let [data-fields (map data-fn data-line)
                             num-fields  (count data-fields)]
                         (when (not= num-labels num-fields)
                           (throw (ex-info "Incorrect number of fields"
                                    (vals->map num-labels num-fields labels-kw data-fields))))
                         (zipmap labels-kw data-fields)))]
    row-maps))

(defn parse->attrs
  "[csv-input & {:as opts} ]
   Returns a map of attributes constructed from the columns of csv-input.  The first line is
   assumed to be column label strings, which are (safely) converted into keywords. The
   returned map has one entry for each column label keyword. The corresponding value for
   each keyword is a vector of string data taken from each subsequent line in the file.
   See tupelo.csv/parse->entities for options.  Not lazy."
  [csv-input & {:as opts}]
  (let [opts (or opts {})]
    (entities->attrs
      (apply parse->entities csv-input (keyvals opts)))))




