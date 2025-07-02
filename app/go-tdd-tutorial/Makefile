.PHONY: test test-watch test-coverage lint fmt vet build run clean check fix watch help

# バイナリ名
BINARY_NAME=go-tdd-tutorial
BUILD_DIR=bin

# Go関連のコマンド
GO_CMD=go
GO_BUILD=$(GO_CMD) build
GO_CLEAN=$(GO_CMD) clean
GO_TEST=$(GO_CMD) test
GO_GET=$(GO_CMD) get
GO_MOD=$(GO_CMD) mod

# テスト実行
test:
	@echo "テストを実行中..."
	$(GO_TEST) -v ./...

# テストのwatch実行
test-watch:
	@echo "テストのwatch実行を開始..."
	@command -v fswatch >/dev/null 2>&1 || { echo "fswatch がインストールされていません。brew install fswatch または apt-get install fswatch でインストールしてください"; exit 1; }
	@fswatch -o . | xargs -n1 -I{} make test

# テストカバレッジ
test-coverage:
	@echo "テストカバレッジを実行中..."
	$(GO_TEST) -coverprofile=coverage.out ./...
	$(GO_CMD) tool cover -html=coverage.out -o coverage.html
	@echo "カバレッジレポートが coverage.html に作成されました"

# 静的解析（lint）
lint:
	@echo "静的解析を実行中..."
	@command -v golangci-lint >/dev/null 2>&1 || { echo "golangci-lint がインストールされていません。https://golangci-lint.run/usage/install/ を参照してインストールしてください"; exit 1; }
	golangci-lint run

# コードフォーマット
fmt:
	@echo "コードフォーマットを実行中..."
	$(GO_CMD) fmt ./...

# go vet実行
vet:
	@echo "go vetを実行中..."
	$(GO_CMD) vet ./...

# ビルド
build:
	@echo "アプリケーションをビルド中..."
	@mkdir -p $(BUILD_DIR)
	$(GO_BUILD) -o $(BUILD_DIR)/$(BINARY_NAME) .

# アプリケーション実行
run:
	@echo "アプリケーションを実行中..."
	$(GO_CMD) run .

# クリーンアップ
clean:
	@echo "クリーンアップ中..."
	$(GO_CLEAN)
	@rm -rf $(BUILD_DIR)
	@rm -f coverage.out coverage.html

# 品質チェック（フォーマット、vet、lint、テスト）
check: fmt vet lint test

# 自動修正（フォーマット）
fix: fmt

# ファイル監視による自動実行
watch:
	@echo "ファイル監視による自動チェックを開始..."
	@command -v fswatch >/dev/null 2>&1 || { echo "fswatch がインストールされていません。brew install fswatch または apt-get install fswatch でインストールしてください"; exit 1; }
	@fswatch -o . | xargs -n1 -I{} make check

# 依存関係のダウンロード
deps:
	@echo "依存関係をダウンロード中..."
	$(GO_MOD) download
	$(GO_MOD) tidy

# ヘルプ
help:
	@echo "利用可能なコマンド:"
	@echo "  test           - テストを実行"
	@echo "  test-watch     - テストのwatch実行"
	@echo "  test-coverage  - テストカバレッジを実行"
	@echo "  lint           - 静的解析を実行"
	@echo "  fmt            - コードフォーマットを実行"
	@echo "  vet            - go vetを実行"
	@echo "  build          - アプリケーションをビルド"
	@echo "  run            - アプリケーションを実行"
	@echo "  clean          - クリーンアップ"
	@echo "  check          - 品質チェック（fmt + vet + lint + test）"
	@echo "  fix            - 自動修正（fmt）"
	@echo "  watch          - ファイル監視による自動チェック"
	@echo "  deps           - 依存関係のダウンロード"
	@echo "  help           - このヘルプを表示"
