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
  (:require
    [clojure.walk :as walk]
    [clojure.zip :as zip]
    [schema.core :as s]
    [tupelo.string :as ts])
  (:import
    [java.io Reader InputStream]
    [javax.xml.parsers SAXParserFactory]
    [org.xml.sax Attributes]
    [org.xml.sax.ext DefaultHandler2]
  ))

(defstruct Element :tag :attrs :content)

(def ^:private tag? :tag)
(defn- document?
  "Document nodes are a parsing impelentation details and should never leak
   outside of it."
  [x] (= :document (:type x)))

(defn- comment? [x] (= (:type x) :comment))
(defn- dtd?     [x] (= (:type x) :dtd))

(defn- xml-zip
  "Returns a zipper for xml elements (as from xml/parse), given a root element"
  [root]
  (zip/zipper
    #(or (tag? %) (document? %))
    (comp seq :content)
    #(assoc %1 :content %2)
    root))

(defn- insert-element [result-zipper elem]
  (-> result-zipper (zip/append-child elem) zip/down zip/rightmost))

(defn- merge-text-left [result-zipper str-val]
  (or
    (when-let [item (-> result-zipper zip/down zip/rightmost)]
      (when (-> item zip/node string?)
        (-> item (zip/edit str str-val) zip/up)))
    (-> result-zipper (zip/append-child str-val))))

(defn- handler [result-atom]
  (proxy [DefaultHandler2] []
    (startElement [uri local-name q-name ^Attributes atts]
      (let [elem (struct Element
                   (keyword q-name)
                   (when (pos? (. atts (getLength)))
                     (reduce #(assoc %1 (keyword (.getQName atts %2)) (.getValue atts (int %2)))
                       {} (range (.getLength atts)))))]
        (swap! result-atom insert-element elem)))

    (endElement [uri local-name q-name]
      (swap! result-atom zip/up))

    (characters [ch start length]
      (swap! result-atom merge-text-left (String. ^chars ch (int start) (int length))))

    (ignorableWhitespace [ch start length]
      (swap! result-atom merge-text-left (String. ^chars ch (int start) (int length))))

    (comment [ch start length]
      (swap! result-atom zip/append-child {:type :comment :data (String. ^chars ch (int start) (int length))}))

    (startDTD [name publicId systemId]
      (swap! result-atom zip/append-child {:type :dtd :data [name publicId systemId]}))

    (resolveEntity
      ([name publicId baseURI systemId]
       (doto (org.xml.sax.InputSource.)
         (.setSystemId systemId)
         (.setPublicId publicId)
         (.setCharacterStream (java.io.StringReader. ""))))
      ([publicId systemId]
       (let [^DefaultHandler2 this this]
         (proxy-super resolveEntity publicId systemId))))))

(defn enlive-normalize
  "Normalize Enlive data replacing `nil` values for :attrs or :content with empty maps & vectors, respectively.
  Also coerces all :content to vector. "
  [item]
  (if (and (map? item) ; Enlive data parsed from XML may has raw strings (esp. whitespace) embedded in it
        (contains-key? item :tag)) ; when parsing html, may get non-enlive nodes like {:type :comment, :data "..."}
    (it-> item
      (update it :attrs (fn [attrs]
                          (into {} attrs))) ; works for nil
      (update it :content (fn [content]
                            (if (or (nil? content) (empty? content))
                              []
                              (mapv enlive-normalize content)))))
    item))

(defn enlive-remove-whitespace
  "Removes whilespace strings from Enlive data :content vectors."
  [item]
  (if (and (map? item) ; Enlive data parsed from XML may has raw strings (esp. whitespace) embedded in it
        (contains-key? item :tag)) ; when parsing html, may get non-enlive nodes like {:type :comment, :data "..."}
    (let [content-new (cond-it-> (:content item)
                        (or (nil? it) (empty? it)) []
                        :then (drop-if (fn [arg]
                                         (and (string? arg)
                                           (ts/whitespace? arg))) it)
                        :then (mapv enlive-remove-whitespace it))]
      (glue item {:content content-new}))
    item))

(defn ^:private sax-parse-fn
  [xml-input content-handler]
  (let [input-source (cond
                       (or (instance? InputStream xml-input)
                           (instance? Reader xml-input))              (org.xml.sax.InputSource. xml-input)
                       (instance? org.xml.sax.InputSource xml-input)  xml-input
                       :else (throw (ex-info "sax-parse-fn: xml-input must be one of InputStream, Reader, or org.xml.sax.InputSource"
                                      {:type  (type xml-input)
                                       :class (class xml-input)})))]
    (it-> (SAXParserFactory/newInstance)
      (doto it
        (.setValidating false)
        (.setFeature "http://xml.org/sax/features/external-general-entities" false)
        (.setFeature "http://xml.org/sax/features/external-parameter-entities" false))
      (.newSAXParser it)
      (doto it
        (.setProperty "http://xml.org/sax/properties/lexical-handler" content-handler))
      (.parse it
        ^org.xml.sax.InputSource             input-source
        ^org.xml.sax.helpers.DefaultHandler  content-handler))))

(s/defn parse-raw       ; #todo fix docstring
  ([xml-input] (parse-raw xml-input sax-parse-fn))
  ([xml-input parse-fn]
    (let [result-atom     (atom (xml-zip {:type :document :content nil}))
          content-handler (handler result-atom)]
      (parse-fn xml-input content-handler)
      ; #todo document logic vvv using xkcd & plain xml example
      (let [parsed-data (it-> @result-atom
                          (first it)
                          (:content it)
                          (drop-if #(= :dtd (:type %)) it)
                          (drop-if #(string? %) it)
                          (only it) )]
        parsed-data))))

(s/defn parse       ; #todo fix docstring
  ([xml-input] (parse xml-input sax-parse-fn))
  ([xml-input parse-fn]
    (enlive-remove-whitespace
      (enlive-normalize
        (parse-raw xml-input parse-fn)))))

; "Parses and loads the source input-source, which can be a File, InputStream or String
;  naming a URI. Returns a seq of tree of the xml/element struct-map, which has the keys
;  :tag, :attrs, and :content. and accessor fns tag, attrs, and content. Other parsers
;  can be supplied by passing parse-fn, a fn taking a source and a
;  ContentHandler and returning a parse-fn"





