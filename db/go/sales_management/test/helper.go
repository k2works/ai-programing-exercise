package test

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq"
	"github.com/testcontainers/testcontainers-go"
	"github.com/testcontainers/testcontainers-go/modules/postgres"
	"github.com/testcontainers/testcontainers-go/wait"
)

// PostgreSQLContainer はテスト用PostgreSQLコンテナを管理します
type PostgreSQLContainer struct {
	container *postgres.PostgresContainer
	DB        *sqlx.DB
}

// SetupPostgreSQLContainer はテスト用PostgreSQLコンテナをセットアップします
func SetupPostgreSQLContainer(t *testing.T) *PostgreSQLContainer {
	t.Helper()

	ctx := context.Background()

	// PostgreSQLコンテナの起動
	postgresContainer, err := postgres.RunContainer(ctx,
		testcontainers.WithImage("postgres:16-alpine"),
		postgres.WithDatabase("testdb"),
		postgres.WithUsername("postgres"),
		postgres.WithPassword("password"),
		testcontainers.WithWaitStrategy(
			wait.ForLog("database system is ready to accept connections").
				WithOccurrence(2).
				WithStartupTimeout(30*time.Second)),
	)
	if err != nil {
		t.Fatalf("PostgreSQLコンテナの起動失敗: %v", err)
	}

	// 接続文字列を取得
	connStr, err := postgresContainer.ConnectionString(ctx, "sslmode=disable")
	if err != nil {
		t.Fatalf("接続文字列の取得失敗: %v", err)
	}

	// データベース接続
	db, err := sqlx.Connect("postgres", connStr)
	if err != nil {
		t.Fatalf("データベース接続失敗: %v", err)
	}

	// テスト終了時のクリーンアップ
	t.Cleanup(func() {
		db.Close()
		if err := postgresContainer.Terminate(ctx); err != nil {
			t.Logf("コンテナの終了失敗: %v", err)
		}
	})

	return &PostgreSQLContainer{
		container: postgresContainer,
		DB:        db,
	}
}

// TruncateTables はテスト後にテーブルをクリーンアップします
func TruncateTables(t *testing.T, db *sqlx.DB, tables ...string) {
	t.Helper()

	for _, table := range tables {
		query := fmt.Sprintf("TRUNCATE TABLE %s CASCADE", table)
		_, err := db.Exec(query)
		if err != nil {
			t.Logf("警告: テーブル %s のクリーンアップ失敗: %v", table, err)
		}
	}
}

// RunMigrations はマイグレーションを実行します
func RunMigrations(t *testing.T, db *sqlx.DB, migrations []string) {
	t.Helper()

	for _, migration := range migrations {
		_, err := db.Exec(migration)
		if err != nil {
			t.Fatalf("マイグレーション実行失敗: %v", err)
		}
	}
}
