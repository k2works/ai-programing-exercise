(defproject algorithm-clj "0.1.0-SNAPSHOT"
  :description "Algorithm implementations in Clojure with TDD approach"
  :url "https://github.com/k2works/ai-programing-exercise"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]

  ;; 開発環境での依存関係
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]
                                  [criterium "0.4.6"]]}}

  ;; プラグイン設定（cljfmtを一時的に除外）
  :plugins [[lein-cloverage "1.2.4"]      ; コードカバレッジ
            [lein-kibit "0.1.8"]          ; 静的コード解析 - Clojureのイディオム検査
            [jonase/eastwood "1.4.3"]     ; 静的コード解析 - リンター
            [lein-ancient "0.7.0"]        ; 依存関係の更新チェック
            [com.jakemccrary/lein-test-refresh "0.25.0"] ; テスト自動実行
            [lein-exec "0.3.7"]]          ; スクリプト実行

  ;; テスト設定
  :test-paths ["test"]

  ;; eastwood（リンター）の設定
  :eastwood {:continue-on-exception true
             :exclude-namespaces []}

  ;; cloverage（カバレッジ）の設定
  :cloverage {:output "target/coverage"
              :html? true
              :text? true
              :summary? true
              :fail-threshold 80}

  ;; エイリアス設定（cljfmtを除外したタスクランナー機能）
  :aliases {"check"     ["do" ["eastwood"] ["kibit"]]
            "test-all"  ["do" ["test"] ["cloverage"]]
            "lint"      ["do" ["eastwood"] ["kibit"]]
            "outdated"  ["ancient"]
            "coverage"  ["cloverage"]
            "autotest"  ["test-refresh"]}

  ;; JVMオプション
  :jvm-opts ["-Xmx1g"]

  ;; メインクラス
  :main ^:skip-aot algorithm-clj.core
  :target-path "target/%s"

  ;; AOTコンパイル設定
  :aot :all

  ;; リソースディレクトリ
  :resource-paths ["resources"])
