; adapted from:  net.cgrand.xml
;   Copyright (c) Christophe Grand, 2009-2013. All rights reserved.
;
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns tupelo.forest.xml
  (:use tupelo.core)
  (:require [clojure.zip :as z])
  (:import (org.xml.sax Attributes)
           (org.xml.sax.ext DefaultHandler2)
           (javax.xml.parsers SAXParserFactory)))

(defstruct element :tag :attrs :content)

(def tag (accessor element :tag))
(def attrs (accessor element :attrs))
(def content (accessor element :content))

(def tag? :tag)
(defn- document?
  "Document nodes are a parsing impelentation details and should never leak
   outside of it."
  [x] (= :document (:type x)))

(defn comment? [x] (= :comment (:type x)))
(defn dtd? [x] (= :dtd (:type x)))

(defn xml-zip
  "Returns a zipper for xml elements (as from xml/parse),
  given a root element"
  [root]
  (z/zipper #(or (tag? %) (document? %))
    (comp seq :content) #(assoc %1 :content %2) root))

(defn- insert-element [loc e]
  (-> loc (z/append-child e) z/down z/rightmost))

(defn- merge-text-left [loc s]
  (or
    (when-let [l (-> loc z/down z/rightmost)]
      (when (-> l z/node string?)
        (-> l (z/edit str s) z/up)))
    (-> loc (z/append-child s))))

(defn- handler [loc ]
  (proxy [DefaultHandler2] []
    (startElement [uri local-name q-name ^Attributes atts]
      (let [e (struct element
                (keyword q-name)
                (when (pos? (. atts (getLength)))
                  (reduce #(assoc %1 (keyword (.getQName atts %2)) (.getValue atts (int %2)))
                    {} (range (.getLength atts)))))]
        (swap! loc insert-element e)))

    (endElement [uri local-name q-name]
      (swap! loc z/up))

    (characters [ch start length]
      (swap! loc merge-text-left (String. ^chars ch (int start) (int length))))

    (ignorableWhitespace [ch start length]
      (swap! loc merge-text-left (String. ^chars ch (int start) (int length))))

    (comment [ch start length]
      (swap! loc z/append-child {:type :comment :data (String. ^chars ch (int start) (int length))}))

    (startDTD [name publicId systemId]
      (swap! loc z/append-child {:type :dtd :data [name publicId systemId]}))

    (resolveEntity
      ([name publicId baseURI systemId]
       (doto (org.xml.sax.InputSource.)
         (.setSystemId systemId)
         (.setPublicId publicId)
         (.setCharacterStream (java.io.StringReader. ""))))
      ([publicId systemId]
       (let [^DefaultHandler2 this this]
         (proxy-super resolveEntity publicId systemId))))))

(defn sax-parser-invoker
  [input-source content-handler]
  (-> (SAXParserFactory/newInstance)
    (doto
      (.setValidating false)
      (.setFeature "http://xml.org/sax/features/external-general-entities" false)
      (.setFeature "http://xml.org/sax/features/external-parameter-entities" false))
    .newSAXParser
    (doto
      (.setProperty "http://xml.org/sax/properties/lexical-handler" content-handler))
    (.parse
      ^java.io.InputStream                input-source     ; actual type => java.io.BufferedInputStream
      ^org.xml.sax.helpers.DefaultHandler content-handler  ; actual type => net.cgrand.xml.proxy$org.xml.sax.ext.DefaultHandler2
    )))

(defn parse
  "Parses and loads the source input-source, which can be a File, InputStream or
  String naming a URI. Returns a seq of tree of the xml/element struct-map,
  which has the keys :tag, :attrs, and :content. and accessor fns tag,
  attrs, and content. Other parsers can be supplied by passing
  parser, a fn taking a source and a ContentHandler and returning
  a parser"
  ([input-source]
   (parse input-source sax-parser-invoker))
  ([input-source parser]
   (let [loc             (atom (-> {:type :document :content nil} xml-zip))
         content-handler (handler loc)]
     (parser input-source content-handler)
     (when false
       (println "*****************************************************************************")
       (println :xml/parse-1)
       (println (clip-str 999 (pretty-str @loc))))
     (let [parsed-data (it-> @loc
                         (first it)
                         (:content it)
                         (drop-if #(= :dtd (:type %)) it)
                         (drop-if #(string? %) it)
                         (only it)) ]
       (with-result parsed-data
         (when false
           (println "*****************************************************************************")
           (println :xml/parse-2)
           (println (clip-str 999 (pretty-str parsed-data)))))))))








