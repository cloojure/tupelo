(ns tst.tupelo.parse.tagsoup
  (:use tupelo.core tupelo.test)
  (:require
    [tupelo.parse.tagsoup :as parse-tagsoup]
    [tupelo.string :as ts]))

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

(def xml-data-expected
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

(def html-str "<div class=“group”>
                 <h2>title1</h2>
                 <div class=“subgroup”>
                   <p>unused</p>
                   <h3>subheading1</h3>
                   <a href=“path1” />
                 </div>
                 <div class=“subgroup”>
                   <p>unused</p>
                   <h3>subheading2</h3>
                   <a href=“path2” />
                 </div>
               </div>
               <div class=“group”>
                 <h2>title2</h2>
                 <div class=“subgroup”>
                   <p>unused</p>
                   <h3>subheading3</h3>
                   <a href=“path3” />
                 </div>
               </div>")

(def html-data-expected
  {:tag     :html,
   :attrs   {},
   :content [{:tag     :body,
              :attrs   {},
              :content [{:tag     :div,
                         :attrs   {:class "“group”"},
                         :content [{:tag :h2, :attrs {}, :content ["title1"]}
                                   {:tag     :div,
                                    :attrs   {:class "“subgroup”"},
                                    :content [{:tag :p, :attrs {}, :content ["unused"]}
                                              {:tag :h3, :attrs {}, :content ["subheading1"]}
                                              {:tag :a, :attrs {:href "“path1”"}, :content []}]}
                                   {:tag     :div,
                                    :attrs   {:class "“subgroup”"},
                                    :content [{:tag :p, :attrs {}, :content ["unused"]}
                                              {:tag :h3, :attrs {}, :content ["subheading2"]}
                                              {:tag :a, :attrs {:href "“path2”"}, :content []}]}]}
                        {:tag     :div,
                         :attrs   {:class "“group”"},
                         :content [{:tag :h2, :attrs {}, :content ["title2"]}
                                   {:tag     :div,
                                    :attrs   {:class "“subgroup”"},
                                    :content [{:tag :p, :attrs {}, :content ["unused"]}
                                              {:tag :h3, :attrs {}, :content ["subheading3"]}
                                              {:tag :a, :attrs {:href "“path3”"}, :content []}]}]}]}]}

  )

(verify
  (let [xml-data  (parse-tagsoup/parse xml-str)
        html-data (parse-tagsoup/parse html-str) ]
    (is= xml-data xml-data-expected)
    (is= html-data html-data-expected) ))








