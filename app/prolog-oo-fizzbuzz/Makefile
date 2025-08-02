# Prolog OO FizzBuzz プロジェクト Makefile
# テスト駆動開発から始めるProlog入門3

.PHONY: help test run clean setup

# デフォルトターゲット
help:
	@echo "Prolog OO FizzBuzz プロジェクト"
	@echo ""
	@echo "利用可能なコマンド:"
	@echo "  make setup    - プロジェクト初期設定"
	@echo "  make test     - テスト実行"
	@echo "  make run      - FizzBuzz実行"
	@echo "  make clean    - 一時ファイルの削除"
	@echo "  make help     - このヘルプの表示"

# プロジェクト初期設定
setup:
	@echo "プロジェクト初期設定中..."
	@mkdir -p src tests scripts
	@chmod +x scripts/*.sh
	@echo "初期設定完了!"

# テスト実行
test:
	@echo "テスト実行中..."
	@./scripts/run_tests.sh

# FizzBuzz実行
run:
	@echo "FizzBuzzを起動します..."
	@./scripts/run_fizzbuzz.sh

# クリーンアップ
clean:
	@echo "一時ファイルを削除中..."
	@find . -name "*.qlf" -delete
	@find . -name "*.qly" -delete
	@find . -name "*~" -delete
	@echo "クリーンアップ完了!"

# Git操作のショートカット
git-status:
	@git status

git-log:
	@git log --oneline -10
