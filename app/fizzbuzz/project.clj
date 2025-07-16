(defproject fizzbuzz "0.1.0-SNAPSHOT"
  :description "FizzBuzz implementation with TDD in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :plugins [[lein-cljfmt "0.9.2"]
            [lein-kibit "0.1.8"]
            [lein-cloverage "1.2.4"]]
  :profiles {:dev {:dependencies [[clj-kondo "RELEASE"]]}}
  :aliases {"lint" ["do" ["cljfmt" "check"] ["kibit"]]
            "format" ["cljfmt" "fix"]
            "coverage" ["cloverage"]
            "check" ["do" ["lint"] ["test"] ["coverage"]]}
  :repl-options {:init-ns fizzbuzz.core})
