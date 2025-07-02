(defproject fizz-buzz-clj "0.1.0-SNAPSHOT"
  :description "テスト駆動開発から始めるClojure入門 ~ソフトウェア開発の三種の神器を準備する~"
  :url "https://github.com/k2works/ai-programing-exercise"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot fizz-buzz-clj.core
  :target-path "target/%s"
  :plugins [[lein-cloverage "1.2.4"]
            [jonase/eastwood "1.4.2"]  
            [lein-kibit "0.1.8"]
            [lein-auto "0.1.3"]]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[cloverage "1.2.4"]]}}
  :aliases {"check" ["do" ["test"] ["eastwood"] ["kibit"]]
            "lint" ["do" ["eastwood"] ["kibit"]]
            "coverage" ["cloverage"]
            "guard" ["auto" "test"]})
