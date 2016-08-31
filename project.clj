(defproject tupelo "0.9.4"
  :description "Tupelo:  Making Clojure Even Sweeter"
  :url "http://github.com/cloojure/tupelo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [ [org.clojure/clojure              "1.9.0-alpha11"]
                  [org.clojure/core.async           "0.2.382"]
                  [org.clojure/core.match           "0.3.0-alpha4"]
                  [org.clojure/math.combinatorics   "0.1.3"]
                  [org.clojure/tools.analyzer       "0.6.9"]
                  [clojure-csv/clojure-csv          "2.0.2"]
                  [clj-time                         "0.12.0"]
                  [cheshire                         "5.6.1"]
                  [prismatic/schema                 "1.1.2"]
                ]
  :profiles { :dev      {:dependencies [[org.clojure/test.check "0.9.0"]] }
              :uberjar  {:aot :all}}
  :global-vars { *warn-on-reflection* false }

  :plugins  [ [lein-codox "0.9.4"] ]
  :codox {:src-dir-uri "http://github.com/cloojure/tupelo/blob/master/"
          :src-linenum-anchor-prefix "L"}
  :deploy-repositories {  "snapshots" :clojars
                          "releases"  :clojars }
  :update :daily  ; :always  
  :main ^:skip-aot tupelo.core
  :target-path      "target/%s"
  :clean-targets  [ "target" ]

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors { :default    (complement :slow)
                    :slow       :slow }

  :jvm-opts ^:replace ["-Xms1g" "-Xmx4g" ]
; :jvm-opts ^:replace ["-Xms1g" "-Xmx8g" "-server"]
)
