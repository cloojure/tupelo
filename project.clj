(defproject tupelo "0.9.40"
  :description "Tupelo:  Clojure With A Spoonful of Honey"
  :url "http://github.com/cloojure/tupelo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ 
   ;[org.clojure/clojure              "1.9.0-alpha16"]
    [cheshire                         "5.7.0"]
    [clj-time                         "0.13.0"]
    [clojure-csv/clojure-csv          "2.0.2"]
    [danlentz/clj-uuid                "0.1.7"]
    [org.clojure/clojure              "1.8.0"]
    [org.clojure/core.async           "0.2.395"]
    [org.clojure/core.match           "0.3.0-alpha4"]
    [org.clojure/data.xml             "0.0.8"]
    [org.clojure/math.combinatorics   "0.1.4"]
    [org.clojure/tools.analyzer       "0.6.9"]
    [potemkin                         "0.4.3"]
    [prismatic/schema                 "1.1.3"]
  ]
  :profiles { :dev      {:dependencies [[org.clojure/test.check "0.9.0"]] }
              :uberjar  {:aot :all}}
  :global-vars { *warn-on-reflection* false }

  :plugins  [ [lein-codox "0.9.4"] ]
  :codox {:src-dir-uri "http://github.com/cloojure/tupelo/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :deploy-repositories {  "snapshots" :clojars
                          "releases"  :clojars 
                          :sign-releases false }
  :update :daily  ; :always  
  :main ^:skip-aot tupelo.core
  :target-path      "target/%s"
  :clean-targets  [ "target" ]

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors { :default    (complement :slow)
                    :slow       :slow }

  :jvm-opts ^:replace ["-Xms1g" "-Xmx4g" ] ; "-server"
)
