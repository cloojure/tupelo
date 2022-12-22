(ns tst.tupelo.parse.xml
  (:use tupelo.core
        tupelo.test)
  (:require
    [clojure.data.xml :as clj-xml]
    [tupelo.parse.tagsoup :as parse-tagsoup]
    [tupelo.parse.xml :as parse-xml]
    [tupelo.string :as ts])
  (:import [java.io StringReader]))

(def xml-str "<foo>
                <name>John</name>
                <address>1 hacker way</address>
                <phone></phone>
                <school>
                    <name>Hard Knocks</name>
                    <state>CA</state>
                    <type>FOOBAR</type>
                </school>
                <college>
                    <name>mit</name>
                    <address></address>
                    <state>Denial</state>
                </college>
              </foo> ")


(def enlive-tree-nils {:tag     :foo,
                       :attrs   nil,
                       :content ["\n                "
                                 {:tag :name, :attrs nil, :content ["John"]}
                                 "\n                "
                                 {:tag :address, :attrs nil, :content ["1 hacker way"]}
                                 "\n                "
                                 {:tag :phone, :attrs nil, :content nil}
                                 "\n                "
                                 {:tag     :school,
                                  :attrs   nil,
                                  :content ["\n                    "
                                            {:tag :name, :attrs nil, :content ["Hard Knocks"]}
                                            "\n                    "
                                            {:tag :state, :attrs nil, :content ["CA"]}
                                            "\n                    "
                                            {:tag :type, :attrs nil, :content ["FOOBAR"]}
                                            "\n                "]}
                                 "\n                "
                                 {:tag     :college,
                                  :attrs   nil,
                                  :content ["\n                    "
                                            {:tag :name, :attrs nil, :content ["mit"]}
                                            "\n                    "
                                            {:tag :address, :attrs nil, :content nil}
                                            "\n                    "
                                            {:tag :state, :attrs nil, :content ["Denial"]}
                                            "\n                "]}
                                 "\n              "
                                 ]})

(def enlive-tree-normalized {:tag     :foo,
                             :attrs   {},
                             :content ["\n                "
                                       {:tag :name, :attrs {}, :content ["John"]}
                                       "\n                "
                                       {:tag :address, :attrs {}, :content ["1 hacker way"]}
                                       "\n                "
                                       {:tag :phone, :attrs {}, :content []}
                                       "\n                "
                                       {:tag     :school,
                                        :attrs   {},
                                        :content ["\n                    "
                                                  {:tag :name, :attrs {}, :content ["Hard Knocks"]}
                                                  "\n                    "
                                                  {:tag :state, :attrs {}, :content ["CA"]}
                                                  "\n                    "
                                                  {:tag :type, :attrs {}, :content ["FOOBAR"]}
                                                  "\n                "]}
                                       "\n                "
                                       {:tag     :college,
                                        :attrs   {},
                                        :content ["\n                    "
                                                  {:tag :name, :attrs {}, :content ["mit"]}
                                                  "\n                    "
                                                  {:tag :address, :attrs {}, :content []}
                                                  "\n                    "
                                                  {:tag :state, :attrs {}, :content ["Denial"]}
                                                  "\n                "]}
                                       "\n              "]})

(def enlive-tree-normalized-nonblank
  {:tag     :foo,
   :attrs   {},
   :content [{:tag :name, :attrs {}, :content ["John"]}
             {:tag :address, :attrs {}, :content ["1 hacker way"]}
             {:tag :phone, :attrs {}, :content []}
             {:tag     :school,
              :attrs   {},
              :content [{:tag :name, :attrs {}, :content ["Hard Knocks"]}
                        {:tag :state, :attrs {}, :content ["CA"]}
                        {:tag :type, :attrs {}, :content ["FOOBAR"]}]}
             {:tag     :college,
              :attrs   {},
              :content [{:tag :name, :attrs {}, :content ["mit"]}
                        {:tag :address, :attrs {}, :content []}
                        {:tag :state, :attrs {}, :content ["Denial"]}]}]})


(verify
  ; verify auto conversion does what we want
  (is= {} (into {} nil))
  (is= {:a 1 :b 2} (into {} {:a 1 :b 2}))
  (is= [] (vec nil))
  (is= [1 2 3] (vec [1 2 3]))
  (is= [1 2 3] (vec (list 1 2 3)))

  (is= enlive-tree-normalized (parse-xml/enlive-normalize enlive-tree-nils))

  ; verify parsing
  (let [clj-xml-data             (clj-xml/parse (ts/string->stream xml-str))
        tf-xml-data              (parse-xml/parse xml-str)
        tf-xml-data-input-source (parse-xml/parse-streaming (org.xml.sax.InputSource. (ts/string->stream xml-str)))
        tf-xml-data-reader       (parse-xml/parse-streaming (StringReader. xml-str))
        tf-tagsoup-data          (parse-tagsoup/parse xml-str)]
    (is= enlive-tree-normalized clj-xml-data)
    (is= enlive-tree-normalized-nonblank (parse-xml/enlive-remove-whitespace clj-xml-data))
    (is= enlive-tree-normalized-nonblank tf-xml-data)
    (is= enlive-tree-normalized-nonblank tf-xml-data-input-source)
    (is= enlive-tree-normalized-nonblank tf-xml-data-reader)
    (is= enlive-tree-normalized-nonblank tf-tagsoup-data)))

(verify
  (let [xml-str-out (clj-xml/indent-str enlive-tree-normalized-nonblank)]
    (is-nonblank= xml-str-out
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
      <foo>
        <name>John</name>
        <address>1 hacker way</address>
        <phone/>
        <school>
          <name>Hard Knocks</name>
          <state>CA</state>
          <type>FOOBAR</type>
        </school>
        <college>
          <name>mit</name>
          <address/>
          <state>Denial</state>
        </college>
      </foo> "
      )))

(verify
  ; NOTE: if present, the `<?xml ... ?>` element must have NO LEADING SPACES! (else get org.xml.sax.SAXParseException)
  (let [xml-str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                  <note>
                    <to>Tove</to>
                    <from>Jani</from>
                    <heading>Reminder</heading>
                    <body>Don't forget me this weekend!</body>
                  </note>"]
    (is= (parse-xml/parse xml-str)
      {:attrs   {},
       :content [{:attrs {}, :content ["Tove"], :tag :to}
                 {:attrs {}, :content ["Jani"], :tag :from}
                 {:attrs {}, :content ["Reminder"], :tag :heading}
                 {:attrs {}, :content ["Don't forget me this weekend!"], :tag :body}],
       :tag     :note}))
  (let [xml-str "<?xml version=\"1.0\"?><!-- a comment --><xyz/>"]
    (is= (parse-xml/parse xml-str)
      {:attrs {}, :content [], :tag :xyz})))