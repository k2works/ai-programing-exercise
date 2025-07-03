# FizzBuzz TDD Makefile

.PHONY: build test format lint coverage watch clean doc

# デフォルトはビルド
default: build

# ビルド
build:
	@echo "🔨 Building..."
	@cargo build

# テストを実行
test:
	@echo "🧪 Running tests..."
	@cargo test

# フォーマット
format:
	@echo "📝 Formatting code..."
	@cargo fmt --all

# 静的解析
lint:
	@echo "🔍 Linting..."
	@cargo clippy -- -D warnings

# カバレッジ計測
coverage:
	@echo "📊 Generating coverage report..."
	@cargo tarpaulin --config tarpaulin.toml

# 監視モード（ファイル変更時に自動テスト）
watch:
	@echo "👀 Watching for changes..."
	@./scripts/watch.sh

# ドキュメント生成
doc:
	@echo "📚 Generating documentation..."
	@cargo doc --no-deps --open

# クリーンアップ
clean:
	@echo "🧹 Cleaning..."
	@cargo clean
	@rm -rf coverage
