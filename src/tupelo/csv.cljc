;   Copyright (c) Alan Thompson. All rights reserved.  
;   The use and distribution terms for this software are covered by the Eclipse Public
;   License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
;   file epl-v10.html at the root of this distribution.  By using this software in any
;   fashion, you are agreeing to be bound by the terms of this license.
;   You must not remove this notice, or any other, from this software.
(ns tupelo.csv
  "Utils for reading CSV (comma-separated-value) formatted files."
  (:require [clojure.string             :as str]
            [clojure.java.io            :as io]
            [clojure-csv.core           :as csv]
            [schema.core                :as s]
            [tupelo.core                :as t]
            [tupelo.misc                :as cool-misc] )
  (:import  [java.io Reader StringReader] ))

(t/refer-tupelo)
; Prismatic Schema type definitions
(s/set-fn-validation! true)   ; #todo add to Schema docs


(defn- get-labels-and-data-lines
  [opts parsed-lines]
  (if (:labels opts)  ; if user supplied col label keywords
    { :labels-kw    (:labels opts)    ; use them
      :data-lines   parsed-lines }  ; all lines are data
  ;else, convert first row of strings -> col label keywords
    { :labels-kw    (mapv cool-misc/str->kw (first parsed-lines))
      :data-lines   (rest parsed-lines) } ))  ; rest of lines are data
; AWTAWT TODO: add default label-fn (comp trim safe-char )

; AWTAWT TODO: change to allow line-seq, FILE, etc? (document!)
; AWTAWT TODO: change to ignore blank lines
; AWTAWT TODO: throw if mismatched missing/excess fields found? (test at least!)
(defn parse-csv->row-maps
 "[csv-input & {:as opts} ] 
  Returns a lazy sequence of maps constructed from csv-input.  The first line
  is assumed to be column label strings, which are (safely) converted into keywords.
  String data from each subsequent line is paired with the corresponding column keyword to
  construct a map for that line.  Default delimiter is the comma character (i.e. \\,) but 
  may be changed using the syntax such as: 
  
    (parse-csv->row-maps <csv-data-src> :delimiter \\| )

  to select the pipe character (i.e. \\|) as the delimiter.  "
  ; AWTAWT TODO: update docs re. col-labels (keywords)
  [csv-input & {:as opts} ] 
  { :pre  [ (or (string? csv-input) (instance? Reader csv-input)) ]
    :post [ (map? (first %)) ] }
  (let [opts-default    {:data-fn str/trim}
        opts            (merge opts-default opts)
        csv-reader      (if (string? csv-input) 
                            (StringReader. csv-input)
                            csv-input )
        parsed-lines    (apply csv/parse-csv csv-reader (t/keyvals opts))
        {:keys [labels-kw data-lines]}  
                        (get-labels-and-data-lines opts parsed-lines)
        data-fn         (:data-fn opts) 
        row-maps        (for [data-line data-lines]
                          (zipmap labels-kw 
                                  (map data-fn data-line) ))
  ] row-maps ))

; AWTAWT TODO: clean up, enforce identical columns each row
(defn row-maps->col-vecs    ; move to tupelo.data ?
  "<TEMP> Converts a sequence of row-maps into a map of column-vectors"
  [row-maps]
  { :pre  [ (map? (first row-maps)) ]
    :post [ (map? %) ] }
  (let [labels-kw   (keys (first row-maps))
        col-vecs    (into {}  (for [label-kw labels-kw]
                                { label-kw (mapv label-kw row-maps) } ))
  ] col-vecs ))

; AWTAWT TODO: clean up, enforce identical columns length
(defn col-vecs->row-maps    ; move to tupelo.data ?
  "<TEMP> Converts a map of column-vectors into a sequence of row-maps"
  [col-vecs]
  { :pre  [ (map? col-vecs) ]
    :post [ (map? (first %)) ] }
  (let [col-kws     (keys col-vecs)
        col-vals    (vals col-vecs)
        row-vals    (apply map vector col-vals)
        row-maps    (map #(zipmap col-kws %) row-vals)
  ] row-maps ))

(defn parse-csv->col-vecs
 "[csv-input & {:as opts} ] 
  Returns a map constructed from the columns of csv-input.  The first line is
  assumed to be column label strings, which are (safely) converted into keywords. The
  returned map has one entry for each column label keyword. The corresponding value for
  each keyword is a vector of string data taken from each subsequent line in the file.
  See tupelo.csv/parse-csv->row-maps for options."
  [csv-input & {:as opts} ] 
  (let [opts (or opts {} ) ] 
    (row-maps->col-vecs 
      (apply parse-csv->row-maps csv-input (t/keyvals opts)))))
