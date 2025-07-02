# Haskell TDD Makefile

.PHONY: help build test clean lint format watch setup

# デフォルトターゲット
.DEFAULT_GOAL := help

## ヘルプを表示
help:
	@echo "Available commands:"
	@echo "  setup   - プロジェクトの初期セットアップ"
	@echo "  build   - プロジェクトのビルド"
	@echo "  test    - テストの実行"
	@echo "  lint    - 静的コード解析"
	@echo "  format  - コードフォーマット"
	@echo "  watch   - ファイル変更を監視してテスト自動実行"
	@echo "  clean   - ビルド成果物の削除"

## プロジェクトの初期セットアップ
setup:
	stack setup
	stack build --dependencies-only

## プロジェクトのビルド
build:
	stack build

## テストの実行
test:
	stack test

## 静的コード解析
lint:
	stack exec hlint -- src test app

## コードフォーマット（hlintの提案を適用）
format:
	stack exec hlint -- src test app --refactor --refactor-options="--inplace"

## ファイル変更を監視してテスト自動実行
watch:
	stack test --file-watch

## ビルド成果物の削除
clean:
	stack clean

## カバレッジ付きテスト実行
coverage:
	stack test --coverage

## プロファイル付きビルド
profile:
	stack build --profile
