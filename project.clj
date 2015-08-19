(defproject tupelo "0.1.43"
  :description "Tupelo:  Making Clojure Even Sweeter"
  :url "http://github.com/cloojure/tupelo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure              "1.6.0"]
                  [org.clojure/core.incubator       "0.1.3"]
                  [org.clojure/data.codec           "0.1.0"]
                  [org.clojure/test.check           "0.5.9"]
                  [org.clojure/core.match           "0.3.0-alpha4"]
                  [clojure-csv/clojure-csv          "2.0.1"]
                  [clj-time                         "0.7.0"]
                  [criterium                        "0.4.3"]
                  [prismatic/schema                 "0.4.3"]
                  [com.datomic/datomic-pro          "0.9.5173" :exclusions [joda-time]]
                ]
  :plugins  [ [codox "0.8.10"] ]
  :codox {:src-dir-uri "http://github.com/cloojure/tupelo/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :deploy-repositories {  "snapshots" :clojars
                          "releases"  :clojars }
  :update :always; :daily  
  :main ^:skip-aot tupelo.core
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
