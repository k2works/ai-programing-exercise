.PHONY: help setup build test lint format coverage complexity complexity-report clean watch

# ヘルプ表示
help:
	@echo "利用可能なコマンド:"
	@echo "  setup             - プロジェクトのセットアップ"
	@echo "  build             - プロジェクトのビルド"
	@echo "  test              - テストの実行"
	@echo "  lint              - 静的コード解析"
	@echo "  format            - コードフォーマット"
	@echo "  coverage          - カバレッジ付きテスト実行"
	@echo "  complexity        - 循環複雑度解析"
	@echo "  complexity-report - 詳細な循環複雑度レポート"
	@echo "  clean             - ビルドファイルの削除"
	@echo "  watch             - ファイル変更の監視とテスト実行"

# プロジェクトセットアップ
setup:
	stack setup
	stack build --dependencies-only

# ビルド
build:
	stack build

# テスト実行
test:
	stack test

# 静的コード解析
lint:
	/home/developer/.local/bin/hlint src/ test/

# コードフォーマット
format:
	find src/ test/ -name "*.hs" -exec ormolu --mode inplace {} \;

# カバレッジ付きテスト
coverage:
	stack test --coverage

# 循環複雑度解析
complexity:
	stack exec -- runhaskell complexity-analyzer.hs

# 詳細な循環複雑度レポート
complexity-report:
	stack exec -- runhaskell complexity-report.hs

# 全体的な品質チェック
check: lint test coverage complexity
	@echo "✅ 全ての品質チェックが完了しました"

# クリーンアップ
clean:
	stack clean

# ファイル監視（entrでファイル変更を監視）
watch:
	@if command -v entr > /dev/null; then \
		find src/ test/ -name "*.hs" | entr -c make test; \
	else \
		echo "entr コマンドが見つかりません。インストールしてください: apt-get install entr"; \
	fi
