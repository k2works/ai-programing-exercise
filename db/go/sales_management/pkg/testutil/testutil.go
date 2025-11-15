package testutil

import (
	"testing"

	"github.com/jmoiron/sqlx"
	"github.com/k2works/sales-management-db/test"
)

// TestDB はテスト用のデータベース接続を管理します
type TestDB struct {
	DB        *sqlx.DB
	container *test.PostgreSQLContainer
}

// SetupTestDB はテスト用のデータベースをセットアップします
func SetupTestDB(t *testing.T) *TestDB {
	t.Helper()

	// testcontainersでPostgreSQLコンテナを起動
	container := test.SetupPostgreSQLContainer(t)
	db := container.DB

	// テーブルをクリーンアップ
	t.Cleanup(func() {
		cleanupTables(t, db)
	})

	return &TestDB{
		DB:        db,
		container: container,
	}
}

// cleanupTables はテスト後にすべてのテーブルをクリーンアップします
func cleanupTables(t *testing.T, db *sqlx.DB) {
	t.Helper()

	// 依存関係の順に削除
	tables := []string{
		"在庫",
		"売上明細",
		"売上",
		"受注明細",
		"受注",
		"顧客別販売単価",
		"代替商品",
		"商品マスタ",
		"商品分類マスタ",
		"倉庫マスタ",
		"顧客マスタ",
		"仕入先マスタ",
		"取引先マスタ",
		"取引先グループマスタ",
		"社員マスタ",
		"部門マスタ",
	}

	test.TruncateTables(t, db, tables...)
}
