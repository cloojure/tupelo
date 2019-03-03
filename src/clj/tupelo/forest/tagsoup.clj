; adapted from:  net.cgrand.tagsoup
;   Copyright (c) Christophe Grand, 2009-2013. All rights reserved.
(ns tupelo.forest.tagsoup
  (:require
    [tupelo.forest.xml :as xml] ) )

(defn- wrapped-tagsoup-parser [input-source content-handler]
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

(defn parser
  "Loads and parse an HTML resource and closes the stream."
  [stream]
  (when-not stream
    (throw (NullPointerException. "HTML resource not found.")))
  (filter map?
    (with-open [^java.io.Closeable stream stream]
      (xml/parse (org.xml.sax.InputSource. stream) wrapped-tagsoup-parser))))
