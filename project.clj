(defproject tupelo "0.9.171"
  :description "Tupelo:  Clojure With A Spoonful of Honey"
  :url "https://github.com/cloojure/tupelo"
  :scm "https://github.com/cloojure/tupelo"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.9.1"

  :global-vars {*warn-on-reflection*      false }

  :excludes [org.clojure/clojure
             org.clojure/clojurescript]

  :dependencies
  [
   [cheshire "5.9.0"] ; #todo switch to muuntaja/jsonista
   [clj-time "0.15.2"]
   [clojure-csv/clojure-csv "2.0.2"]
   [danlentz/clj-uuid "0.1.9"]
   [io.pedestal/pedestal.jetty "0.5.7"]
   [io.pedestal/pedestal.route "0.5.7"]
   [io.pedestal/pedestal.service "0.5.7"]
   [joda-time/joda-time "2.10.5"]
   [org.ccil.cowan.tagsoup/tagsoup "1.2.1"]
   [org.clojure/core.async "0.5.527"]
   [org.clojure/core.match "0.3.0"]
   [org.clojure/data.avl "0.1.0"]
   [org.clojure/data.xml "0.2.0-alpha5"]
   [org.clojure/math.combinatorics "0.1.6"]
   [org.clojure/spec.alpha "0.2.176"]
   [org.clojure/test.check "0.10.0"]
   [org.clojure/tools.reader "1.3.2"]
   [org.snakeyaml/snakeyaml-engine "2.0"] ; #todo upgrade to 2.0, edit Java imports
   [prismatic/schema "1.1.12"]
   [reagent-utils "0.3.3"] ; used by cljs
   ]
  ; Using `lein-ancient check :all` checks plugins
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-codox "0.10.7"]
           ;[lein-nomis-ns-graph "0.14.2"]
            [com.jakemccrary/lein-test-refresh "0.24.1"]]

  :test-refresh {:quiet true ; true => suppress printing namespaces when testing
                 }

  :codox {:src-dir-uri               "http://github.com/cloojure/tupelo/blob/master/"
          :src-linenum-anchor-prefix "L"
          :source-paths               ["src/clj" "src/cljc"]
          :language                   :clojure
          :namespaces                 [ #"^tupelo\." ] }

  :profiles {:provided {:dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                                       [org.clojure/clojurescript "1.10.520" :scope "provided"]
                                       ]}
             :dev      {:dependencies [
                                      ;[binaryage/devtools "0.9.10"]
                                      ;[binaryage/dirac "1.4.3"]
                                      ;[com.cemerick/piggieback "0.2.2"]
                                       [criterium "0.4.5"]
                                       [org.clojure/clojure "1.10.1"]
                                       [org.clojure/clojurescript "1.10.520"]
                                       ]
                        }
             :test     {:dependencies [[overtone/at-at "1.2.0"]]}
             :1.8      {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9      {:dependencies [[org.clojure/clojure "1.9.0"]]}
             }
  :source-paths [  "src/clj"   "src/cljc" ]
  :test-paths   [ "test/clj"  "test/cljc" ]
  :target-path  "target/%s"

  ; need to add the compliled assets to the :clean-targets
  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "out"
                                    :target-path]

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors {:default (complement :slow)
                   :slow    :slow
                   :fast    :fast}
      ; #todo broken for tupelo.test/dospec - why?

  ; :main ^:skip-aot tupelo.core
  ; :uberjar      {:aot :all}

  :deploy-repositories {"snapshots"    :clojars
                        "releases"     :clojars
                        :sign-releases false}

  ; :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  ; automatically handle `--add-modules` stuff req'd for Java 9 & Java 10
  :jvm-opts ["-Xms500m" "-Xmx2g"
           ; "--illegal-access=permit"  ; may need for Java10+  [ permit, warn, debug, deny ]
            ]
)
















