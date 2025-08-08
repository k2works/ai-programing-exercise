(defproject algorithm-clj "0.1.0-SNAPSHOT"
  :description "アルゴリズムから始めるClojure入門プロジェクト"
  :url "https://github.com/k2works/ai-programing-exercise"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :main ^:skip-aot algorithm-clj.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
