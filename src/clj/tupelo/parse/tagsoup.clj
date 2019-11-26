; adapted from:  net.cgrand.tagsoup
;   Copyright (c) Christophe Grand, 2009-2013. All rights reserved.
;
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tupelo.parse.tagsoup
  (:use tupelo.core)
  (:require
    [schema.core :as s]
    [tupelo.parse.xml :as xml]
    [tupelo.string :as ts]
    [tupelo.schema :as tsk]))

(s/defn ^:private tagsoup-parse-fn
  [input-source :- org.xml.sax.InputSource
   content-handler]
  (doto (org.ccil.cowan.tagsoup.Parser.)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/default-attributes" false)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/cdata-elements" true)
    (.setFeature "http://www.ccil.org/~cowan/tagsoup/features/ignorable-whitespace" true)
    (.setContentHandler content-handler)
    (.setProperty "http://www.ccil.org/~cowan/tagsoup/properties/auto-detector"
      (proxy [org.ccil.cowan.tagsoup.AutoDetector] []
        (autoDetectingReader [^java.io.InputStream is]
          (java.io.InputStreamReader. is "UTF-8"))))
    (.setProperty "http://xml.org/sax/properties/lexical-handler" content-handler)
    (.parse input-source)))

; #todo make use string input:  (ts/string->stream html-str)
(s/defn parse-raw :- tsk/KeyMap
  "Loads and parse an HTML resource and closes the input-stream."
  [html-str :- s/Str]
  (xml/parse-raw-streaming
    (org.xml.sax.InputSource.
      (ts/string->stream html-str))
    tagsoup-parse-fn))

; #todo make use string input:  (ts/string->stream html-str)
(s/defn parse :- tsk/KeyMap
  "Loads and parse an HTML resource and closes the input-stream."
  [html-str :- s/Str]
  (xml/enlive-remove-whitespace
    (xml/enlive-normalize
      (parse-raw
        html-str))))


