(defproject farg/ish "0.21.4-SNAPSHOT"
  :description "v21 of FARGish/Numbo"
  :url "https://github.com/bkovitz/FARGish"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[better-cond "2.0.1-SNAPSHOT"]
                 [com.bhauman/rebel-readline "0.1.2"]
                 [com.rpl/specter "1.1.0"]
                 [farg/pgraph "0.1.0-SNAPSHOT"]
                 [farg/util "0.1.0-SNAPSHOT"]
                 [farg/with-state "0.0.1-SNAPSHOT"]
                 [org.clojure/clojure "1.9.0"]
                 [potemkin "0.4.4"]
                 [seesaw/seesaw "1.4.5"]]
  ;:main ^:skip-aot v21.core
  :aliases {"rebl" ["trampoline" "run" "-m" "rebel-readline.main"]}
  :repl-options {:init (do (use 'farg.model1)
                           (require '[farg.graphs4 :as g :refer [graph]]
                                    '[farg.logging :as log]
                                    '[farg.with-state :refer [with-state]]
                                    '[farg.pgraph2 :as pg :refer [pgraph]]
                                    '[farg.util :as u]
                                    '[com.rpl.specter :as S]))}
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
