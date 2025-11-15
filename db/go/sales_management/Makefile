.PHONY: help build test clean seed db-up db-down db-reset migrate-up migrate-down

# デフォルトのヘルプメッセージ
help:
	@echo "利用可能なコマンド:"
	@echo "  make build       - アプリケーションをビルド"
	@echo "  make test        - テストを実行"
	@echo "  make clean       - ビルド成果物を削除"
	@echo "  make seed        - シードデータを投入"
	@echo "  make db-up       - データベースを起動"
	@echo "  make db-down     - データベースを停止"
	@echo "  make db-reset    - データベースをリセットしてシードデータを投入"
	@echo "  make migrate-up  - マイグレーションを実行"
	@echo "  make migrate-down - マイグレーションをロールバック"

# アプリケーションのビルド
build:
	@echo "🔨 アプリケーションをビルドしています..."
	go build -o bin/server cmd/server/main.go
	@echo "✅ ビルド完了"

# テストの実行
test:
	@echo "🧪 テストを実行しています..."
	go test -v ./...
	@echo "✅ テスト完了"

# ビルド成果物の削除
clean:
	@echo "🧹 ビルド成果物を削除しています..."
	rm -rf bin/
	@echo "✅ クリーンアップ完了"

# シードデータの投入
seed:
	@echo "🌱 シードデータを投入しています..."
	@if [ -z "$(DATABASE_URL)" ]; then \
		echo "DATABASE_URL が設定されていません。デフォルト設定を使用します。"; \
		go run cmd/seed/*.go; \
	else \
		echo "DATABASE_URL: $(DATABASE_URL) を使用します。"; \
		DATABASE_URL=$(DATABASE_URL) go run cmd/seed/*.go; \
	fi

# データベースの起動
db-up:
	@echo "🚀 データベースを起動しています..."
	docker-compose up -d postgres
	@echo "⏳ データベースの起動を待機中..."
	@sleep 5
	@echo "✅ データベースが起動しました"

# データベースの停止
db-down:
	@echo "🛑 データベースを停止しています..."
	docker-compose down
	@echo "✅ データベースが停止しました"

# データベースのリセット（停止→起動→マイグレーション→シードデータ投入）
db-reset: db-down db-up
	@echo "🔄 データベースをリセットしています..."
	@sleep 5
	@echo "📊 シードデータを投入します..."
	@$(MAKE) seed
	@echo "✅ データベースのリセットが完了しました"

# マイグレーションの実行（migrate CLIが必要）
migrate-up:
	@echo "⬆️  マイグレーションを実行しています..."
	@if command -v migrate >/dev/null 2>&1; then \
		migrate -path migrations -database "postgres://postgres:password@localhost:5432/sales_management?sslmode=disable" up; \
		echo "✅ マイグレーション完了"; \
	else \
		echo "❌ migrate コマンドが見つかりません"; \
		echo "インストール方法: https://github.com/golang-migrate/migrate"; \
	fi

# マイグレーションのロールバック（migrate CLIが必要）
migrate-down:
	@echo "⬇️  マイグレーションをロールバックしています..."
	@if command -v migrate >/dev/null 2>&1; then \
		migrate -path migrations -database "postgres://postgres:password@localhost:5432/sales_management?sslmode=disable" down; \
		echo "✅ ロールバック完了"; \
	else \
		echo "❌ migrate コマンドが見つかりません"; \
		echo "インストール方法: https://github.com/golang-migrate/migrate"; \
	fi
