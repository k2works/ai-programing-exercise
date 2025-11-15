package test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
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

	// マイグレーションを実行
	if err := runMigrationsFromFiles(t, db); err != nil {
		t.Fatalf("マイグレーション実行失敗: %v", err)
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

// runMigrationsFromFiles はマイグレーションディレクトリからファイルを読み込んで実行します
func runMigrationsFromFiles(t *testing.T, db *sqlx.DB) error {
	t.Helper()

	// プロジェクトルートを見つける
	projectRoot, err := findProjectRoot()
	if err != nil {
		return fmt.Errorf("プロジェクトルートの検索失敗: %w", err)
	}

	// マイグレーションディレクトリのパスを取得
	migrationsDir := filepath.Join(projectRoot, "migrations")

	// .up.sql ファイルを取得
	files, err := filepath.Glob(filepath.Join(migrationsDir, "*_*.up.sql"))
	if err != nil {
		return fmt.Errorf("マイグレーションファイルの検索失敗: %w", err)
	}

	// ファイルが見つからない場合はエラー
	if len(files) == 0 {
		// カレントディレクトリを確認
		wd, _ := os.Getwd()
		return fmt.Errorf("マイグレーションファイルが見つかりません: %s (cwd: %s)", migrationsDir, wd)
	}

	// ファイル名でソート
	sort.Strings(files)

	// 各マイグレーションファイルを実行
	for _, file := range files {
		content, err := os.ReadFile(file)
		if err != nil {
			return fmt.Errorf("マイグレーションファイルの読み込み失敗 %s: %w", file, err)
		}

		// 空のファイルはスキップ
		sqlContent := strings.TrimSpace(string(content))
		if sqlContent == "" {
			t.Logf("スキップ: 空のマイグレーションファイル %s", file)
			continue
		}

		// SQL実行
		if _, err := db.Exec(sqlContent); err != nil {
			return fmt.Errorf("マイグレーション実行失敗 %s: %w", file, err)
		}

		t.Logf("マイグレーション実行成功: %s", file)
	}

	return nil
}

// findProjectRoot はgo.modファイルがあるディレクトリを探してプロジェクトルートを返します
func findProjectRoot() (string, error) {
	// カレントディレクトリから開始
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}

	// 上位ディレクトリを探索
	for {
		// go.modファイルが存在するかチェック
		goModPath := filepath.Join(dir, "go.mod")
		if _, err := os.Stat(goModPath); err == nil {
			return dir, nil
		}

		// 親ディレクトリに移動
		parent := filepath.Dir(dir)
		if parent == dir {
			// ルートディレクトリに到達
			return "", fmt.Errorf("go.mod not found")
		}
		dir = parent
	}
}
