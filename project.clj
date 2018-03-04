(defproject farg/v21 "0.21.0-SNAPSHOT"
  :description "v21 of FARGish/Numbo"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[better-cond "2.0.1-SNAPSHOT"]
                 [com.rpl/specter "1.1.0"]
                 [farg/pgraph "0.1.0-SNAPSHOT"]
                 [farg/util "0.1.0-SNAPSHOT"]
                 [farg/with-state "0.0.1-SNAPSHOT"]
                 [org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot v21.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
