(defproject belib "0.2.0-SNAPSHOT"
            :description "BELs utils lib"
            :url "https://github.com/bennoloeffler/belib.git"
            :license {:name "WTFPL – Do What the Fuck You Want to Public License"
                      :url  "http://www.wtfpl.net/"}
            :dependencies [[org.clojure/clojure "1.10.3"]
                           [org.clojure/test.check "1.1.1"]
                           [com.draines/postal "2.0.4"]
                           [org.clojure/core.async "1.3.610"]
                           [tick "0.6.2"]
                           [clojure.java-time "1.2.0"]
                           [com.hyperfiddle/rcf "20220405"]
                           [net.cgrand/macrovich "0.2.1"]
                           [funcool/cuerdas "2022.06.16-403"]
                           [metosin/malli "0.11.0"]
                           [io.github.borkdude/deflet "0.1.0"]]
            :source-paths ["src/cljc"]
            :aliases {"kaocha"   ["with-profile" "+kaocha" "run" "-m" "kaocha.runner" #_"--plugin" #_"notifier" "--watch"]
                      "coverage" ["with-profile" "+kaocha" "run" "-m" "kaocha.runner" "--plugin" "cloverage"]}
            :profiles {:kaocha
                       {:dependencies [[lambdaisland/kaocha "1.67.1055"]
                                       [lambdaisland/kaocha-cloverage "1.0.75"]]}}

            :repl-options {:init-ns belib.core})
