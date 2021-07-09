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
    [clojure.data.csv :as csv]
    [schema.core :as s]
    [tupelo.schema :as tsk]
    [tupelo.string :as str])
  (:import
    [java.io Reader StringReader StringWriter]))

(def ^:no-doc CsvInput  (s/cond-pre Reader s/Str))

(s/defn ^:no-doc verified-keys :- [s/Any]
  "Verifies that each entity has an identical keyset. Returns a vector of keys."
  [entities :- [tsk/Map]]
  (let [keyset (into #{} (keys (xfirst entities)))]
    (doseq [entity entities]
      (assert
        (= keyset (set (keys entity)))
        "All entities must have identical keys"))
    (vec keyset)))

(s/defn entities->attrs :- tsk/Map
  "Converts a sequence of entity-maps into a map of column-vectors. Not lazy."
  [entities :- [tsk/Map]]
  (if (empty? entities)
    {}
    (let [all-keys (verified-keys entities)
          attrs    (apply glue
                     (forv [col-key all-keys]
                       {col-key (mapv #(grab col-key %) entities)}))]
      attrs)))

(s/defn attrs->entities :- [tsk/Map]
  "Converts a map of attribute-vectors into a vector of entity-maps. Not lazy."
  [attrs :- tsk/Map]
  (if (empty? attrs)
    []
    (let [attr-keys (keys attrs)
          attr-vecs (vals attrs)
          >>        (let [col-lens (mapv count attr-vecs)] ; #todo test this
                      (when-not (apply = col-lens)
                        (throw (ex-info "column lengths must be equal"
                                 (vals->map attr-keys col-lens)))))
          row-vals  (apply mapv vector attr-vecs)
          row-maps  (mapv #(zipmap attr-keys %) row-vals)]
      row-maps)))


(defn ^:no-doc get-labels-and-data-lines ; #todo use schema
  [opts parsed-lines]
  (if (:labels opts)
    ; if user supplied col label keywords
    {:labels-kw  (grab :labels opts) ; use them
     :data-lines parsed-lines} ; all lines are data

)) ; rest of lines are data

(s/defn csv->table :- [[s/Str]]
  "Load `csv-input` (Reader or String) into a string table (vector of vectors). Default options:

       {:separator \\,
        :quote     \\\"}
 "
  ([csv-input :- CsvInput] (csv->table csv-input {}))
  ([csv-input :- CsvInput
    opts :- tsk/KeyMap]
   ; (nl) (spyx :table opts)
   (let [opts-default {:separator \,
                       :quote     \"}
         opts         (glue opts-default opts)
         csv-lib-opts (keyvals (submap-by-keys opts [:separator :quote]))
         csv-reader   (cond-it-> csv-input
                        (string? it) (StringReader. it))
         str-table    (vec (apply csv/read-csv csv-reader csv-lib-opts))]
     str-table)))

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
(s/defn csv->entities :- [tsk/Map]
  "
   [csv-input]
   [csv-input opts-map]

   Load `csv-input` (Reader or String), returning a vector of maps.  The first line
   is assumed to be column label strings, which are (safely) converted into keywords.
   String data from each subsequent line is paired with the corresponding column keyword to
   construct a map for that line.  Default delimiter is the comma character (i.e. \\,) but
   may be changed using the syntax such as:

       (parse-csv->row-maps <csv-data-src> {:delimiter \\}|)

   to select the pipe character (i.e. \\|) as the delimiter.

   <csv-data-source> is either a multi-line-string, or a java.io.Reader.  Default options map:

        {:key-fn           str/trim
         :val-fn           str/trim
         :headers?         true       ; is header row present?
         :headers-to-use   nil        ; vec of string/keyword to use as headers
         :keywordize-keys? true
         :separator        \\,
         :quote            \\\"
        }
  "
  ; #todo: update docs re. col-labels (keywords)
  ; #todo: add option for :ignore-blank-lines
  ; #todo: change options to (parse* ctx), using
  ;   {:ignore-blank-lines true/false
  ;    :delimiter <char> default to \,
  ;   make existing functions defer to (parse* ctx) function

  ([csv-input :- CsvInput] (csv->entities csv-input {}))
  ([csv-input :- CsvInput
    opts :- tsk/KeyMap]
   ; (nl) (spyx :csv->entities opts)
   (let [opts-default
                   {:key-fn           str/trim
                    :val-fn           str/trim
                    :headers?         true
                    :headers-to-use   nil
                    :keywordize-keys? true
                    :separator        \,
                    :quote            \"}
      options         (glue opts-default opts)
      parsed-lines (csv->table csv-input options)]
     ; (spyx-pretty options)
     (with-map-vals options [key-fn val-fn headers? headers-to-use keywordize-keys?]
       (let [parsed-first-row (first parsed-lines)
             keys-vec         (if (not-nil? headers-to-use)
                                headers-to-use
                                (if headers?
                                  (cond-it-> (mapv key-fn parsed-first-row)
                                    keywordize-keys? (forv [k it]
                                                       (-> k
                                                         (->str)
                                                         (str/lower-case)
                                                         (str/str->kw-normalized))))

                                  (range (count parsed-first-row))))


             num-keys         (count keys-vec)
             data-lines       (cond-it-> parsed-lines
                                headers? (rest parsed-lines))
             entities         (forv [line data-lines]
                                (let [data-fields (mapv val-fn line)
                                      num-fields  (count data-fields)]
                                  (when (not= num-keys num-fields)
                                    (throw (ex-info "Incorrect number of fields"
                                             (vals->map num-keys num-fields keys-vec line))))
                                  (zipmap keys-vec data-fields)))]
         entities)))))

(s/defn csv->attrs :- tsk/KeyMap
  "[csv-input & {:as opts} ]
   Returns a map of attributes constructed from the columns of csv-input.  The first line is
   assumed to be column label strings, which are (safely) converted into keywords. The
   returned map has one entry for each column label keyword. The corresponding value for
   each keyword is a vector of string data taken from each subsequent line in the file.
   See tupelo.csv/parse->entities for options.  Not lazy."
  ([csv-input :- CsvInput] (csv->attrs csv-input {}))
  ([csv-input :- CsvInput
    opts :- tsk/KeyMap]
   (entities->attrs
     (csv->entities csv-input opts))))

(s/defn entities->csv :- s/Str
  "Writes a sequence of EDN maps to a multi-line CSV string.  Keys are output in
   sorted order.  Optionally accepts a map-key conversion function.  Default options:

        {:separator     \\,
         :quote         \\\"
         :key-fn        kw->str
         :val-fn        ->str
         :newline       :lf     ; or :cr+lf
         :force-quote?   false
        }
  "
  ([entities :- [tsk/Map]] (entities->csv entities {}))
  ([entities :- [tsk/Map]
    opts :- tsk/KeyMap]
   (let [opts-default
                      {:separator \,
                       :quote     \"
                       :key-fn    kw->str
                       :val-fn    ->str
                       :newline   :lf     ; or :cr+lf
                       :force-quote? false
                       ; #todo :quote?  *** add this? ***
                       }
         options      (glue opts-default opts)
         lib-opts     (cond-it-> (submap-by-keys options [:separator :quote :newline])
                        (grab :force-quote? options) (glue it {:quote? (constantly true)}))
         ]
     (with-map-vals options [key-fn val-fn]
       (let [keys-sorted     (vec (sort (verified-keys entities)))
             hdr-vec         (mapv  key-fn keys-sorted)
             data-vecs       (forv [entity entities]
                               (forv [curr-key keys-sorted]
                                 (val-fn (grab curr-key entity)))) ; coerce all to string for output to CSV
             string-table-2d (prepend hdr-vec data-vecs)
             string-writer   (StringWriter.)
             >>              (apply csv/write-csv string-writer string-table-2d
                               (spyx (keyvals lib-opts)))
             result          (.toString string-writer)

             ]
         result)))))
