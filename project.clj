(defproject tupelo "0.9.95"
  :description "Tupelo:  Clojure With A Spoonful of Honey"
  :url "http://github.com/cloojure/tupelo"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[cheshire "5.8.0"]
   [clojure-csv/clojure-csv "2.0.2"]
   [criterium "0.4.4"]
   [danlentz/clj-uuid "0.1.7"]
   [enlive "1.1.6"]
   [org.clojure/clojure "1.9.0"]
   [org.clojure/clojure "1.10.0-alpha8"]
   [org.clojure/core.async "0.4.474"]
   [org.clojure/core.match "0.3.0-alpha4"]
   [org.clojure/data.xml "0.2.0-alpha5"]
   [org.clojure/math.combinatorics "0.1.4"]
   [org.clojure/spec.alpha "0.2.168"]
   [org.clojure/test.check "0.9.0"]
   [org.clojure/tools.reader "1.3.0"]
   [org.clojure/tools.analyzer "0.6.9"]
   [prismatic/schema "1.1.9"]
   ]
  :update :daily    ; :always

  :plugins [[lein-codox "0.10.3"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-figwheel "0.5.15"]
            [lein-doo "0.1.10"]]

  ;:hooks [leiningen.cljsbuild]

  :codox {:src-dir-uri               "http://github.com/cloojure/tupelo/blob/master/"
          :src-linenum-anchor-prefix "L"}

  :deploy-repositories {"snapshots"    :clojars
                        "releases"     :clojars
                        :sign-releases false}
  :doo {:build "test"
        :paths {:karma   "node_modules/karma/bin/karma"
                :phantom "node_modules/phantomjs/bin/phantomjs"}}

  :source-paths ["src"]

  :global-vars {*warn-on-reflection*      false }

 ;:cljsbuild {:builds
  ;            [{:id           "dev"
  ;              :source-paths ["src"]
  ;              ;; The presence of a :figwheel configuration here will cause figwheel to inject the
  ;              ;; figwheel client into your build
  ;              :figwheel     {:on-jsload "tupelo.core/on-js-reload"
  ;                             ;; :open-urls will pop open your application in the default browser once
  ;                             ;; Figwheel has started and compiled your application.  Comment this out
  ;                             ;; once it no longer serves you.
  ;                             :open-urls ["http://localhost:3449/index.html"]}
  ;              :compiler     {:main                 tupelo.core
  ;                             :optimizations        :none
  ;                             :libs                 ["resources/public/libs"] ; recursive includes all children
  ;
  ;                             :foreign-libs         [{:file     "dino.js"
  ;                                                     :provides ["dinoPhony"]}]
  ;                             :externs              ["dino-externs.js"]
  ;
  ;                             :output-to            "resources/public/js/compiled/tupelo.js"
  ;                             :output-dir           "resources/public/js/compiled/tupelo-dev"
  ;                             :asset-path           "js/compiled/tupelo-dev" ; rel to figwheel default of `resources/public`
  ;                             ; ^^^ must match :output-dir
  ;                             :source-map-timestamp true
  ;                             ;; To console.log CLJS data-structures make sure you enable devtools in Chrome
  ;                             ;; https://github.com/binaryage/cljs-devtools
  ;                             :preloads             [devtools.preload]}}
  ;
  ;             {:id           "test"
  ;              :source-paths ["src" "test"]
  ;              :compiler     {:main                 tst.tupelo.doorunner
  ;                             :optimizations        :none ; :advanced
  ;                             :libs                 ["resources/public/libs"] ; recursive includes all children
  ;
  ;                             :foreign-libs         [{:file     "dino.js"
  ;                                                     :provides ["dinoPhony"]}]
  ;                             :externs              ["dino-externs.js"]
  ;
  ;                             :output-to            "resources/public/js/compiled/honey.js"
  ;                             :output-dir           "resources/public/js/compiled/honey-tst"
  ;                             ;:asset-path           "js/compiled/honey-tst" ; rel to figwheel default of `resources/public`
  ;
  ;                             :source-map-timestamp true
  ;                             }}
  ;             ]}

  ;-----------------------------------------------------------------------------
  :target-path      "target/%s"
  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "out"
                                    :target-path] ; [ "target" ]

  ; "lein test"         will not  run tests marked with the ":slow" metadata
  ; "lein test :slow"   will only run tests marked with the ":slow" metadata
  ; "lein test :all"    will run all  tests (built-in)
  :test-selectors {:default (complement :slow)
                   :slow    :slow
                   :fast    :fast}
      ; #todo broken for tupelo.test/dospec - why?

  ; :main ^:skip-aot tupelo.core
  :uberjar      {:aot :all}
  :jvm-opts ["-Xms500m" "-Xmx2g"
           ; "--illegal-access=permit"  ; may need for Java10+
            ] ; permit, warn, debug, deny

  :profiles {:dev {:dependencies [[binaryage/devtools "0.9.10"]
                                  [figwheel-sidecar "0.5.16"]
                                  [com.cemerick/piggieback "0.2.2"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   }}
)
