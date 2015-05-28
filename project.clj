(defproject cooljure "0.1.27"
  :description "Cooljure:  Cool stuff you wish was in Clojure"
  :url "http://github.com/cloojure/cooljure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure                      "1.7.0-RC1"]
                  [org.clojure/core.incubator               "0.1.3"]
                  [org.clojure/data.codec                   "0.1.0"]
                  [org.clojure/test.check                   "0.5.9"]
                  [clojure-csv/clojure-csv                  "2.0.1"]
                  [clj-time                                 "0.7.0"]
                  [criterium                                "0.4.3"]
                  [prismatic/schema                         "0.3.2"] ]
  :plugins  [ [codox "0.8.10"] ]
  :codox {:src-dir-uri "http://github.com/cloojure/cooljure/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :deploy-repositories {  "snapshots" :clojars
                          "releases"  :clojars }
  :update :always; :daily  
  :main ^:skip-aot cooljure.core
  :target-path "target/%s"
  :clean-targets [ "target" ]
  :profiles { ; :dev      { :certificates ["clojars.pom"] }
              :uberjar  { :aot :all }
            }

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors { :default    (complement :slow)
                    :slow       :slow }

  :jvm-opts ["-Xms2g" "-Xmx12g" ]
; :jvm-opts ["-Xms4g" "-Xmx8g" "-server"]
)
