(ns build
  (:require
    [clojure.tools.build.api :as b]
    [deps-deploy.deps-deploy :as dd]
    [tupelo.core :as t]
    [tupelo.misc :as misc]
    [tupelo.string :as str]
    ))

(def version-str "23.03.14a") ; snapshot versions MUST look like `23.03.03-SNAPSHOT` (i.e. no letters like `-03a`)
(def tag-str (str "v" version-str)) ; ***** ASSUMES YOU CREATE A GIT TAG LIKE `v23.01.31` *****
(def lib-name 'tupelo/tupelo) ; must be a namespaced-qualified symbol, interpreted as `group-id/artifact-id`
(def scm-root "github.com/cloojure/tupelo")

(def build-folder "target")
(def jar-content (str build-folder "/classes")) ; folder where we collect files to pack in a jar
(def basis (b/create-basis {:project "deps.edn"})) ; basis structure (read details in the article)
(def jar-file-name (format "%s/%s-%s.jar" build-folder (name lib-name) version-str)) ; path for result jar file

(defn clean-files
  "Delete all compiler output files (i.e. `.target/**/*`)"
  [& args] ; ignore `nil` arg
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn tag-release
  "Tag release by prepending a `v` char to the version string and calling `git tag`
    (eg version `23.03.15` => tag `v23.03.15`)."
  [& args] ; ignore `nil` arg
  ; (prn :args args)
  (let [cmd-str-1 (str/quotes->double
                  (format "git tag --force '%s' -m'%s'" tag-str tag-str))
        >> (println :cmd-str cmd-str-1)
        r1 (misc/shell-cmd cmd-str-1)
        >> (prn r1)
        >> (when (not= 0 (t/grab :exit r1))
             (throw (ex-info "git tag failed " r1)))
        cmd-str-2 "git pull ; git push ; git push --tags --force"
        r2 (misc/shell-cmd  cmd-str-2)
        >> (prn r2)
        >> (when (not= 0 (t/grab :exit r2))
             (throw (ex-info "git push failed " r2)))
        ]
    )

  )

(defn build-jar
  "Build a new, clean JAR file from source-code."
  [& args] ; ignore `nil` arg
  (clean-files) ; clean leftovers

  (b/copy-dir {:src-dirs   ["src/clj"
                            "src/cljc"
                            "resources"] ; prepare jar content
               :target-dir jar-content})

  (b/write-pom {:class-dir jar-content ; create pom.xml
                :lib       lib-name
                :version   version-str
                :basis     basis
                :src-dirs  ["src"]
                :scm       {:tag                 tag-str
                            :url                 (str "https://" scm-root)
                            :connection          (str "scm:git:git://" scm-root ".git")
                            :developerConnection (str "scm:git:ssh://git@" scm-root ".git")}})

  (b/jar {:class-dir jar-content ; create jar
          :jar-file  jar-file-name})
  (println (format "Jar file created: \"%s\"" jar-file-name)))

(defn deploy-clojars
  "Build & deploy a source-code JAR file to clojars.org"
  [& args] ; ignore `nil` arg
  (build-jar)
  (dd/deploy {:installer :remote
              :artifact  jar-file-name
              :pom-file  (b/pom-path {:lib       lib-name
                                      :class-dir jar-content})}))


