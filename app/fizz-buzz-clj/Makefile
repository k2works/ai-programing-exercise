# テスト駆動開発から始めるClojure入門 タスクランナー

.PHONY: help test lint format format-fix run clean setup

help: ## ヘルプを表示
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

setup: ## 依存関係のセットアップ
	@echo "依存関係を確認中..."
	@lein deps

test: ## テストを実行
	@echo "テストを実行中..."
	@lein test

lint: ## 静的コード解析を実行
	@echo "静的コード解析を実行中..."
	@lein lint

format: ## コードフォーマットをチェック
	@echo "コードフォーマットをチェック中..."
	@clojure -M:format

format-fix: ## コードフォーマットを自動修正
	@echo "コードフォーマットを自動修正中..."
	@clojure -M:format-fix

run: ## アプリケーションを実行
	@echo "FizzBuzzアプリケーションを実行中..."
	@lein run

check: ## 全ての品質チェックを実行
	@echo "全ての品質チェックを実行中..."
	@lein lint
	@clojure -M:format
	@lein test

coverage: ## コードカバレッジを実行
	@echo "コードカバレッジを実行中..."
	@lein coverage

auto: ## ファイル監視と自動テスト実行
	@echo "ファイル監視を開始（Ctrl+Cで終了）..."
	@lein guard

clean: ## 生成されたファイルをクリーンアップ
	@echo "クリーンアップ中..."
	@rm -rf target/
	@rm -rf .cpcache/
