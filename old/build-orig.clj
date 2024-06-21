(ns build
  (:require
    [clojure.tools.build.api :as b]
    [deps-deploy.deps-deploy :as dd]
    [tupelo.core :as t]
    [tupelo.misc :as misc]
    [tupelo.string :as str]
    ))

;---------------------------------------------------------------------------------------------------
; User-supplied values

(def version-str "23.05.04") ; snapshot versions MUST look like `23.03.03-SNAPSHOT` (i.e. no letters like `-03a`)
(def lib-name 'enterprises.wizard/tupelo) ; must be a namespaced-qualified symbol, interpreted as `group-id/artifact-id`
(def scm-root "github.com/wizard-enterprises/tupelo")
(def src-dirs ["src"])
(def resource-dirs ["resources"])
(def build-folder "target")


;===================================================================================================
; Derived values
(def git-tag-str (str "v" version-str)) ; a git tag like `v23.01.31` will be added
(def jar-content (str build-folder "/classes")) ; folder where we collect files to pack in a jar
(def basis (b/create-basis {:project "deps.edn"})) ; basis structure (read details in the article)
(def jar-file-name (format "%s/%s-%s.jar" build-folder (name lib-name) version-str)) ; eg `target/tupelo-23.05.04.jar`

;---------------------------------------------------------------------------------------------------
; code

(defn clean-files
  "Delete all compiler output files (i.e. `.target/**/*`)"
  [& args] ; ignore `nil` arg
  (b/delete {:path build-folder})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn check-all-committed-or-throw
  "Use git to verify there are no uncommitted files present"
  [& args] ; ignore `nil` arg
  (let [cmd-str-1     "git status --short --branch"
        shell-result  (misc/shell-cmd cmd-str-1)
        out-lines     (str/split-lines (t/grab :out shell-result))
        out-lines-num (count out-lines)]
    ; git always returns the branch as the first line like "## master...origin/master"
    ; So, there are modified uncommitted files if count is larger than 1
    (when (< 1 out-lines-num)
      (throw (ex-info "Error: Uncommitted files detected" shell-result)))))

(defn tag-release
  "Tag release by prepending a `v` char to the version string and calling `git tag`
    (eg version `23.03.15` => tag `v23.03.15`)."
  [& args] ; ignore `nil` arg
  (check-all-committed-or-throw)
  (println (str/quotes->double
             (format "Tagging release: '%s'" git-tag-str)))
  (let [cmd-str-1 (str/quotes->double
                    (format "git tag --force '%s' -m'%s'" git-tag-str git-tag-str))
        r1        (misc/shell-cmd cmd-str-1)]
    (when (not= 0 (t/grab :exit r1))
      (throw (ex-info "git tag failed " r1))))
  (println "Pushing release & tags...")
  (let [cmd-str-2 "git pull ; git push ; git push --tags --force"
        r2        (misc/shell-cmd cmd-str-2)]
    (when (not= 0 (t/grab :exit r2))
      (throw (ex-info "git push failed " r2)))))

(defn build-jar
  "Build a new, clean JAR file from source-code."
  [& args] ; ignore `nil` arg
  (newline)
  (clean-files)
  (tag-release)

  ; prepare jar content
  (b/copy-dir {:src-dirs   src-dirs
               :target-dir jar-content})

  (t/spy :write-pom (b/write-pom {:class-dir     jar-content ; create pom.xml
                :lib           lib-name
                :version       version-str
                :basis         basis
                :resource-dirs resource-dirs
                :scm           {:tag                 git-tag-str
                                :url                 (str "https://" scm-root)
                                :connection          (str "scm:git:git://" scm-root ".git")
                                :developerConnection (str "scm:git:ssh://git@" scm-root ".git")}

                ; #todo throw if "src" is not root of project source code
                ; #todo is this really needed?
                :src-dirs      ["src"] ; ***** all but first dir will be discarded *****
                :pom-data [[:licenses
                            [:license
                             [:name "EPL-1.0 license"]
                             [:url "https://github.com/cloojure/tupelo/blob/master/LICENSE.txt"]
                             [:distribution "repo"]]]]
                }))

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
