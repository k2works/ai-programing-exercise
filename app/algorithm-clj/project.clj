(defproject algorithm-clj "0.1.0-SNAPSHOT"
  :description "Algorithm implementations in Clojure with TDD approach"
  :url "https://github.com/k2works/ai-programing-exercise"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]]

  ;; 開発環境での依存関係
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]
                                  [criterium "0.4.6"]
                                  [lein-checkall "0.1.1"]]}}

  ;; プラグイン設定（循環複雑度測定ツールを追加）
  :plugins [[lein-cloverage "1.2.4"]           ; コードカバレッジ
            [lein-kibit "0.1.8"]               ; 静的コード解析 - Clojureのイディオム検査
            [jonase/eastwood "1.4.3"]          ; 静的コード解析 - リンター
            [lein-ancient "0.7.0"]             ; 依存関係の更新チェック
            [com.jakemccrary/lein-test-refresh "0.25.0"] ; テスト自動実行
            [lein-exec "0.3.7"]                ; スクリプト実行
            [lein-nvd "2.0.0"]                 ; 依存関係の脆弱性検査
            [lein-bikeshed "0.5.2"]            ; コード品質メトリクス（循環複雑度含む）
            [venantius/yagni "0.1.7"]]         ; 未使用関数検出

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

  ;; bikeshed（コード品質メトリクス）の設定
  :bikeshed {:verbose true
             :max-line-length 100
             :long-lines false
             :trailing-whitespace false
             :trailing-blank-lines false
             :var-redefs false
             :docstrings false}

  ;; yagni（未使用関数検出）の設定
  :yagni {:entry-points ["algorithm-clj.core/-main"]}

  ;; nvd（脆弱性検査）の設定
  :nvd {:output-dir "reports/nvd"
        :config-filename ".nvd/config.json"}

  ;; エイリアス設定（循環複雑度測定を含むタスクランナー機能）
  :aliases {"check"        ["do" ["eastwood"] ["kibit"]]
            "test-all"     ["do" ["test"] ["cloverage"]]
            "lint"         ["do" ["eastwood"] ["kibit"]]
            "metrics"      ["do" ["bikeshed"] ["yagni"]]
            "complexity"   ["bikeshed"]
            "security"     ["nvd" "check"]
            "quality"      ["do" ["check"] ["metrics"] ["test-all"]]
            "outdated"     ["ancient"]
            "coverage"     ["cloverage"]
            "autotest"     ["test-refresh"]
            "full-check"   ["do" ["clean"] ["check"] ["metrics"] ["security"] ["test-all"]]}

  ;; JVMオプション
  :jvm-opts ["-Xmx1g"]

  ;; メインクラス
  :main ^:skip-aot algorithm-clj.core
  :target-path "target/%s"

  ;; AOTコンパイル設定
  :aot :all

  ;; リソースディレクトリ
  :resource-paths ["resources"])
