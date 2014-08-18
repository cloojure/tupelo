(defproject cooljure "0.1.9"
  :description "Cooljure:  Cool stuff you wish was in Clojure"
  :url "http://github.com/cloojure/cooljure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure                     "1.7.0-alpha1"]
                  [org.clojure/clojure-contrib             "1.2.0"]
                  [clojure-csv/clojure-csv                 "2.0.1"]
                  [clj-time                                "0.7.0"]
                  [criterium                               "0.4.3"] ]
  :deploy-repositories {  "snapshots" :clojars
                          "releases"  :clojars }
  :update :always; :daily  
  :main ^:skip-aot cooljure.core
  :target-path "target/%s"
  :clean-targets [ "target" ]
  :profiles { ; :dev      { :certificates ["clojars.pom"] }
              :uberjar  { :aot :all }
            }
  :jvm-opts ["-Xms2g" "-Xmx12g" ]
; :jvm-opts ["-Xms4g" "-Xmx8g" "-server"]
)
