(defproject cooljure "0.1.0-SNAPSHOT"
  :description "Cooljure:  Cool stuff you wish was in Clojure"
  :url "http://github.com/cloojure/cooljure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [   [org.clojure/clojure            "1.6.0"]
                    [org.clojure/clojure-contrib    "1.2.0"] ]
  :main ^:skip-aot cooljure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
