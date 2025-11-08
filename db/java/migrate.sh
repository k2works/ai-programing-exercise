#!/bin/bash

# Flyway マイグレーション実行スクリプト
# Docker コンテナ内から SQL ファイルを順番に実行します

set -e

CONTAINER_NAME="sales-management-postgres"
DB_NAME="sales_management"
DB_USER="postgres"
MIGRATION_DIR="app/src/main/resources/db/migration"

echo "=================================="
echo "Flyway Migration Script"
echo "=================================="
echo ""

# Docker コンテナが起動しているか確認
if ! docker ps | grep -q "$CONTAINER_NAME"; then
    echo "Error: PostgreSQL container '$CONTAINER_NAME' is not running."
    echo "Please start the container with: docker-compose up -d postgres"
    exit 1
fi

echo "Database: $DB_NAME"
echo "Container: $CONTAINER_NAME"
echo ""

# マイグレーションファイルを順番に実行
count=0
for file in "$MIGRATION_DIR"/V*.sql; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        echo "Executing: $filename"
        docker exec -i "$CONTAINER_NAME" psql -U "$DB_USER" -d "$DB_NAME" < "$file"
        ((count++))
        echo "✓ $filename completed"
        echo ""
    fi
done

echo "=================================="
echo "Migration completed successfully!"
echo "Total migrations executed: $count"
echo "=================================="

# テーブル一覧を表示
echo ""
echo "Tables in database:"
docker exec "$CONTAINER_NAME" psql -U "$DB_USER" -d "$DB_NAME" -c "\dt"
