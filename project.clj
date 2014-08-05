(defproject cooljure "0.1.0-SNAPSHOT"
  :description "Cooljure:  Cool stuff you wish was in Clojure"
  :url "http://github.com/cloojure/cooljure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure                     "1.6.0"]
                  [org.clojure/clojure-contrib             "1.2.0"]
                  [clojure-csv/clojure-csv                 "2.0.1"]
                  [clj-time                                "0.7.0"]
                  [criterium                               "0.4.3"]
                  [com.taoensso/timbre                     "3.2.1"]
                  [commons-collections                     "3.2.1"]
                  [commons-io                              "2.4"]
                  [org.clojars.runa/kits                   "1.17.14-SNAPSHOT"] ]
  :update :daily  ; :always
  :main ^:skip-aot cooljure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
