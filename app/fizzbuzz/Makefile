# Haskell FizzBuzz Makefile

.PHONY: help build test lint format coverage clean all

# デフォルトターゲット
all: lint format test coverage

# ヘルプメッセージ
help:
	@echo "利用可能なタスク:"
	@echo "  build     - プロジェクトをビルド"
	@echo "  test      - テストを実行"
	@echo "  lint      - HLintによる静的解析"
	@echo "  format    - stylish-haskellによるコードフォーマット"
	@echo "  coverage  - カバレッジ付きテスト実行"
	@echo "  clean     - ビルド成果物を削除"
	@echo "  all       - lint, format, test, coverageを順次実行"

# プロジェクトビルド
build:
	stack build

# テスト実行
test:
	stack test

# 静的解析
lint:
	/root/.local/bin/hlint .

# コードフォーマット
format:
	/root/.local/bin/stylish-haskell --inplace src/**/*.hs test/**/*.hs app/**/*.hs

# カバレッジ付きテスト
coverage:
	stack test --coverage

# クリーンアップ
clean:
	stack clean
