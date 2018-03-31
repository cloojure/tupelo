(defproject tupelo "0.9.73"
  :description "Tupelo:  Clojure With A Spoonful of Honey"
  :url "http://github.com/cloojure/tupelo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [
    [cheshire                         "5.8.0"]
   ;[clj-time                         "0.14.2"]
    [clojure-csv/clojure-csv          "2.0.2"]
    [criterium                        "0.4.4"]
    [danlentz/clj-uuid                "0.1.7"]
    [enlive                           "1.1.6"]
   ;[org.clojure/clojure              "1.8.0"]
    [org.clojure/clojure              "1.9.0"]
    [org.clojure/core.async           "0.4.474"]
    [org.clojure/core.match           "0.3.0-alpha4"]
    [org.clojure/data.xml             "0.2.0-alpha5"]
    [org.clojure/math.combinatorics   "0.1.4"]
    [org.clojure/spec.alpha           "0.1.143"]
    [org.clojure/test.check           "0.9.0"]
    [org.clojure/tools.analyzer       "0.6.9"]
    [prismatic/schema                 "1.1.7"]
  ]
  :profiles { :dev      {:dependencies [] 
                         :plugins [] }
              :uberjar  {:aot :all}}

  :global-vars { *warn-on-reflection* false }

  :plugins  [ [lein-codox "0.10.3"] ]
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
  :test-selectors {:default (complement :slow)
                   :slow    :slow}
      ; #todo broken for tupelo.test/dospec - why?


  :jvm-opts ["-Xms500m" "-Xmx2g"
            ;"--add-modules" "java.xml.bind"
            ;"-server"
            ]
)
