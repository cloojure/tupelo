(ns tst.tupelo.forest.xml
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.data.xml :as clj-xml]
    [tupelo.forest.xml :as tf-xml]
    [tupelo.string :as ts]
    )
  (:import [java.io StringReader]))

(def xml-str "<foo>
                <name>John</name>
                <address>1 hacker way</address>
                <phone></phone>
                <school>
                    <name>Joe</name>
                    <state>CA</state>
                    <type>FOOBAR</type>
                </school>
                <college>
                    <name>mit</name>
                    <address></address>
                    <state>Denial</state>
                </college>
              </foo> ")
(dotest
  (let [
        ; contains {} or [], never nil
        clj-enlive-tree          {:tag     :foo,
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
                                                       {:tag :name, :attrs {}, :content ["Joe"]}
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
                                            "\n              "]}

        ; contains `nil` for missing :attrs or :content
        tf-enlive-tree           {:tag     :foo,
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
                                                       {:tag :name, :attrs nil, :content ["Joe"]}
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
                                            "\n              "]}

        clj-xml-data             (clj-xml/parse (ts/string->stream xml-str))
        tf-xml-data              (tf-xml/parse (ts/string->stream xml-str))
        tf-xml-data-input-source (tf-xml/parse (org.xml.sax.InputSource.
                                                 (ts/string->stream xml-str)))
        tf-xml-data-reader       (tf-xml/parse (StringReader. xml-str))
        ]
    (is= clj-enlive-tree  clj-xml-data)
    (is= tf-enlive-tree
      tf-xml-data
      tf-xml-data-input-source
      tf-xml-data-reader)

    (let [result-empty (tf-xml/walk-nil->empty tf-xml-data)]
      (is= result-empty clj-xml-data))

    ))



