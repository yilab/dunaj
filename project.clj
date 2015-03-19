(defproject org.dunaj/dunaj "0.3.8-SNAPSHOT"
  :description "Dunaj - An alternative core API for Clojure."
  :url "http://dunaj.org"
  :dependencies [[org.dunaj/clojure "1.7.0-dunaj_pre3" :exclusions [org.clojure/clojure org.clojure/clojurescript]]
                 [org.dunaj/core.async "0.1.0-dunaj_pre3" :exclusions [org.clojure/clojure org.clojure/clojurescript]]
                 [org.dunaj/core.rrb-vector "0.0.12-dunaj_pre3" :exclusions [org.clojure/clojure org.clojure/clojurescript]]
                 [org.dunaj/synthread "1.1.0-dunaj_pre3" :exclusions [org.clojure/clojure org.clojure/clojurescript]]]
  :scm {:name "git" :url "https://github.com/dunaj-project/dunaj"}
  :signing {:gpg-key "6A72CBE2"}
  :deploy-repositories [["clojars" {:creds :gpg}]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/jvm"]
  :main ^:skip-aot dunaj.main
  :aot [bare.core dunaj.core dunaj.main] ;; see CLJ-1650
;  :aot :all
;  :clean-non-project-classes true
  :auto-clean false
  :jar-exclusions [#"project\.clj"]
  :uberjar-exclusions [#"project\.clj"]
  :global-vars {*warn-on-reflection* true}
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :profiles {:uberjar {:aot [bare.core dunaj.core dunaj.main]
                       :omit-source true}}
;;  :jvm-opts ^:replace ["-Xms3G" "-Xmx3G" "-XX:-UseConcMarkSweepGC" "-server"]
)
