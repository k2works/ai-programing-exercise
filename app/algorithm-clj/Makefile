# Algorithm Clojure - Makefile
# テスト駆動開発アプローチでのClojureプロジェクト管理

.PHONY: help clean deps test lint format coverage check build run install dev-setup

# デフォルトターゲット
help: ## ヘルプを表示
	@echo "Available targets:"
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

# セットアップとクリーンアップ
clean: ## ビルド成果物をクリーンアップ
	lein clean
	rm -rf target/

deps: ## 依存関係をインストール
	lein deps

dev-setup: deps ## 開発環境をセットアップ
	@echo "Development environment setup complete!"

# テスト関連
test: ## テストを実行
	lein test

test-watch: ## テストを自動実行（ファイル変更監視）
	lein autotest

coverage: ## コードカバレッジを測定
	lein coverage

test-all: ## テスト実行とカバレッジ測定
	lein test-all

# 品質チェック
format-check: ## コードフォーマットをチェック（修正なし）
	lein cljfmt check

format-fix: ## コードフォーマットを自動修正
	lein fix

lint: ## 静的コード解析を実行
	lein lint

check: ## 全体的な品質チェック（フォーマット+リント）
	lein check

# アプリケーション実行
run: ## アプリケーションを実行
	lein run

repl: ## REPLを開始
	lein repl

# ビルドとデプロイ
build: ## プロジェクトをビルド
	lein uberjar

install: ## プロジェクトをローカルリポジトリにインストール
	lein install

# メンテナンス
outdated: ## 古い依存関係をチェック
	lein outdated

# CI/CD向けターゲット
ci-setup: deps ## CI環境でのセットアップ
	@echo "CI setup complete"

ci-test: ## CI環境でのテスト実行
	lein test-all

ci-quality: ## CI環境での品質チェック
	lein check

ci-build: clean ci-setup ci-quality ci-test build ## CI環境での完全ビルド
	@echo "CI build complete!"

# 開発フロー
dev-flow: format-fix test-all lint ## 開発時の推奨フロー
	@echo "Development flow complete!"

pre-commit: check test ## コミット前のチェック
	@echo "Pre-commit checks passed!"
